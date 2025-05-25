class SocketHandler {
    constructor(io) {
        this.io = io;
        this.rooms = new Map(); // Хранилище комнат
        this.playerRooms = new Map(); // Связь игрок -> комната
        this.setupEventHandlers();
        console.log('✅ SocketHandler инициализирован');
    }

    setupEventHandlers() {
        this.io.on('connection', (socket) => {
            console.log('🎮 Игрок подключился:', socket.id);

            // Создание комнаты
            socket.on('create-room', () => {
                console.log('📝 Создание комнаты от игрока:', socket.id);
                
                try {
                    const roomId = this.generateRoomId();
                    const room = {
                        id: roomId,
                        players: {
                            princess: { 
                                id: socket.id, 
                                name: this.generatePlayerName() 
                            },
                            helper: null
                        },
                        gameState: 'lobby',
                        createdAt: new Date()
                    };

                    this.rooms.set(roomId, room);
                    this.playerRooms.set(socket.id, roomId);
                    socket.join(roomId);

                    console.log(`✅ Комната ${roomId} создана игроком ${socket.id}`);
                    
                    const roomData = this.getRoomData(roomId);
                    console.log('📤 Отправляем данные комнаты:', roomData);
                    
                    socket.emit('room-created', roomData);
                } catch (error) {
                    console.error('❌ Ошибка создания комнаты:', error);
                    socket.emit('error', 'Не удалось создать комнату');
                }
            });

            // Присоединение к комнате
            socket.on('join-room', (roomId) => {
                console.log(`🚪 Попытка присоединения к комнате ${roomId} от игрока ${socket.id}`);
                
                try {
                    const room = this.rooms.get(roomId);
                    if (!room) {
                        console.log(`❌ Комната ${roomId} не найдена`);
                        socket.emit('error', 'Комната не найдена');
                        return;
                    }

                    if (room.gameState !== 'lobby') {
                        console.log(`❌ Игра в комнате ${roomId} уже началась`);
                        socket.emit('error', 'Игра уже началась');
                        return;
                    }

                    if (room.players.helper) {
                        console.log(`❌ Комната ${roomId} полна`);
                        socket.emit('error', 'Комната полна');
                        return;
                    }

                    // Присоединяем как помощницу
                    room.players.helper = { 
                        id: socket.id, 
                        name: this.generatePlayerName() 
                    };
                    this.playerRooms.set(socket.id, roomId);
                    socket.join(roomId);

                    console.log(`✅ Игрок ${socket.id} присоединился к комнате ${roomId}`);
                    
                    const roomData = this.getRoomData(roomId);
                    
                    // Отправляем данные присоединившемуся игроку
                    socket.emit('room-joined', {
                        ...roomData,
                        yourRole: 'helper'
                    });

                    // Обновляем лобби для всех в комнате
                    this.io.to(roomId).emit('lobby-update', roomData);
                } catch (error) {
                    console.error('❌ Ошибка присоединения к комнате:', error);
                    socket.emit('error', 'Не удалось присоединиться к комнате');
                }
            });

            // Запуск кооперативной игры
            socket.on('start-coop-game', (data) => {
                console.log(`🎮 Запуск игры в комнате ${data.roomId}`);
                
                try {
                    const room = this.rooms.get(data.roomId);
                    if (!room || !room.players.princess || !room.players.helper) {
                        socket.emit('error', 'Недостаточно игроков для начала игры');
                        return;
                    }

                    // Проверяем, что запрос от создателя комнаты (княжны)
                    if (room.players.princess.id !== socket.id) {
                        socket.emit('error', 'Только создатель комнаты может начать игру');
                        return;
                    }

                    room.gameState = 'playing';
                    
                    // Создаем базовые данные игры
                    const gameData = {
                        roomId: data.roomId,
                        players: room.players,
                        scene: {
                            title: "Утреннее пробуждение",
                            text: `Утренний свет пробивается сквозь тяжелые шторы княжеской спальни. 
                            
                            Княжна просыпается и видит рядом незнакомую девушку, очень похожую на неё. 
                            Девушка представляется сестрой и предлагает завтрак.
                            
                            <em>Теперь каждый игрок может делать выборы за своего персонажа!</em>`
                        },
                        choices: {
                            princess: [
                                {
                                    id: "princess_greet",
                                    text: "Поприветствовать",
                                    description: "Тепло поздороваться с 'сестрой'"
                                },
                                {
                                    id: "princess_suspicious",
                                    text: "Подозрительно осмотреться",
                                    description: "Что-то кажется странным..."
                                }
                            ],
                            helper: [
                                {
                                    id: "helper_explain",
                                    text: "Объяснить ситуацию",
                                    description: "Рассказать про родителей и войну"
                                },
                                {
                                    id: "helper_magic",
                                    text: "Использовать магию",
                                    description: "Активировать магические серьги"
                                }
                            ]
                        },
                        stats: {
                            princess: {
                                awareness: 0,
                                outfit: 'Ночная рубашка',
                                loyalty: 50
                            },
                            helper: {
                                influence: 50,
                                secretsKnown: ['parents_dead', 'magic_items'],
                                trustLevel: 75
                            }
                        },
                        currentTurn: 'princess',
                        chapter: 1
                    };

                    console.log(`✅ Кооперативная игра началась в комнате ${data.roomId}`);
                    this.io.to(data.roomId).emit('game-started', gameData);
                } catch (error) {
                    console.error('❌ Ошибка запуска игры:', error);
                    socket.emit('error', 'Не удалось запустить игру');
                }
            });

            // Выбор в кооперативной игре
            socket.on('make-choice', (data) => {
                console.log(`🎯 Выбор от игрока ${socket.id}:`, data);
                
                try {
                    const roomId = this.playerRooms.get(socket.id);
                    if (!roomId) {
                        socket.emit('error', 'Вы не находитесь в игре');
                        return;
                    }

                    const room = this.rooms.get(roomId);
                    if (!room || room.gameState !== 'playing') {
                        socket.emit('error', 'Игра не найдена или не началась');
                        return;
                    }

                    // Простая обработка выбора (для демонстрации)
                    const responseText = this.getChoiceResponse(data.choiceId);
                    
                    const gameData = {
                        roomId: roomId,
                        players: room.players,
                        scene: {
                            title: "Результат выбора",
                            text: responseText
                        },
                        choices: {
                            princess: [
                                {
                                    id: "continue",
                                    text: "Продолжить",
                                    description: "Идти дальше по сюжету"
                                }
                            ],
                            helper: [
                                {
                                    id: "continue_helper",
                                    text: "Продолжить",
                                    description: "Идти дальше по сюжету"
                                }
                            ]
                        },
                        stats: {
                            princess: { awareness: 10, outfit: 'Ночная рубашка', loyalty: 50 },
                            helper: { influence: 60, secretsKnown: ['parents_dead'], trustLevel: 75 }
                        },
                        currentTurn: data.character === 'princess' ? 'helper' : 'princess',
                        chapter: 1
                    };

                    this.io.to(roomId).emit('game-update', gameData);
                } catch (error) {
                    console.error('❌ Ошибка обработки выбора:', error);
                    socket.emit('error', 'Не удалось обработать выбор');
                }
            });

            // Чат
            socket.on('chat-message', (data) => {
                try {
                    const roomId = this.playerRooms.get(socket.id);
                    if (!roomId) return;

                    const room = this.rooms.get(roomId);
                    if (!room) return;

                    const playerName = this.getPlayerName(socket.id, room);
                    const chatData = {
                        playerName: playerName,
                        message: data.message,
                        timestamp: new Date()
                    };

                    this.io.to(roomId).emit('chat-message', chatData);
                } catch (error) {
                    console.error('❌ Ошибка чата:', error);
                }
            });

            // Покидание комнаты
            socket.on('leave-room', (roomId) => {
                this.handlePlayerLeave(socket.id, roomId);
            });

            // Отключение
            socket.on('disconnect', () => {
                console.log('👋 Игрок отключился:', socket.id);
                const roomId = this.playerRooms.get(socket.id);
                if (roomId) {
                    this.handlePlayerLeave(socket.id, roomId);
                }
            });

            // Тест соединения
            socket.emit('connection-test', { 
                message: 'Соединение установлено!', 
                socketId: socket.id 
            });
        });
    }

    generateRoomId() {
        const chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
        let result = '';
        for (let i = 0; i < 4; i++) {
            result += chars.charAt(Math.floor(Math.random() * chars.length));
        }
        
        // Проверяем, что такой ID еще не существует
        if (this.rooms.has(result)) {
            return this.generateRoomId();
        }
        return result;
    }

    generatePlayerName() {
        const adjectives = ['Мудрый', 'Смелый', 'Хитрый', 'Добрый', 'Быстрый'];
        const nouns = ['Странник', 'Рыцарь', 'Мудрец', 'Бард', 'Лучник'];
        const adj = adjectives[Math.floor(Math.random() * adjectives.length)];
        const noun = nouns[Math.floor(Math.random() * nouns.length)];
        return `${adj} ${noun}`;
    }

    getChoiceResponse(choiceId) {
        const responses = {
            princess_greet: "Княжна тепло поприветствовала 'сестру'. Девушка улыбнулась в ответ.",
            princess_suspicious: "Княжна внимательно осмотрелась. Что-то действительно кажется странным...",
            helper_explain: "Помощница спокойно объяснила ситуацию с родителями и войной.",
            helper_magic: "Помощница активировала магию серег. Её слова стали звучать убедительнее.",
            continue: "История продолжается...",
            continue_helper: "История продолжается..."
        };
        
        return responses[choiceId] || "Что-то произошло...";
    }

    getRoomData(roomId) {
        const room = this.rooms.get(roomId);
        if (!room) return null;

        return {
            roomId: roomId,
            players: room.players,
            gameState: room.gameState
        };
    }

    getPlayerName(socketId, room) {
        if (room.players.princess?.id === socketId) {
            return room.players.princess.name + ' (Княжна)';
        }
        if (room.players.helper?.id === socketId) {
            return room.players.helper.name + ' (Помощница)';
        }
        return 'Неизвестный игрок';
    }

    handlePlayerLeave(socketId, roomId) {
        try {
            const room = this.rooms.get(roomId);
            if (!room) return;

            const playerName = this.getPlayerName(socketId, room);
            
            // Удаляем игрока из комнаты
            if (room.players.princess?.id === socketId) {
                room.players.princess = null;
            }
            if (room.players.helper?.id === socketId) {
                room.players.helper = null;
            }

            this.playerRooms.delete(socketId);

            // Если комната пустая, удаляем ее
            if (!room.players.princess && !room.players.helper) {
                this.rooms.delete(roomId);
                console.log(`🗑️ Комната ${roomId} удалена - нет игроков`);
                return;
            }

            // Уведомляем оставшихся игроков
            this.io.to(roomId).emit('player-left', {
                playerName: playerName,
                ...this.getRoomData(roomId)
            });

            console.log(`👋 Игрок ${socketId} покинул комнату ${roomId}`);
        } catch (error) {
            console.error('❌ Ошибка при покидании комнаты:', error);
        }
    }
}

module.exports = SocketHandler;
