const CoopGameLogic = require('../game/coopGameLogic');

class SocketHandler {
    constructor(io) {
        this.io = io;
        this.rooms = new Map();
        this.playerRooms = new Map(); // socketId -> roomId
        this.gameLogic = new CoopGameLogic();
        this.setupEventHandlers();
    }

    setupEventHandlers() {
        this.io.on('connection', (socket) => {
            console.log(`🔌 Игрок подключился: ${socket.id}`);

            // Создание комнаты
            socket.on('create-room', () => {
                const roomId = this.generateRoomId();
                const roomData = {
                    id: roomId,
                    players: {
                        princess: null,
                        helper: null
                    },
                    gameState: 'waiting',
                    host: socket.id
                };

                this.rooms.set(roomId, roomData);
                this.playerRooms.set(socket.id, roomId);
                socket.join(roomId);

                console.log(`🏠 Комната создана: ${roomId} хостом ${socket.id}`);
                
                socket.emit('room-created', {
                    roomId: roomId,
                    players: roomData.players,
                    isHost: true
                });
            });

            // Присоединение к комнате
            socket.on('join-room', (roomId) => {
                const room = this.rooms.get(roomId);
                if (!room) {
                    socket.emit('error', 'Комната не найдена');
                    return;
                }

                if (room.gameState !== 'waiting') {
                    socket.emit('error', 'Игра уже началась');
                    return;
                }

                // Назначаем роль
                if (!room.players.princess) {
                    room.players.princess = { id: socket.id, name: `Игрок ${socket.id.substring(0, 6)}` };
                } else if (!room.players.helper) {
                    room.players.helper = { id: socket.id, name: `Игрок ${socket.id.substring(0, 6)}` };
                } else {
                    socket.emit('error', 'Комната полна');
                    return;
                }

                this.playerRooms.set(socket.id, roomId);
                socket.join(roomId);

                console.log(`🚪 Игрок ${socket.id} присоединился к комнате ${roomId}`);

                // Обновляем всех в лобби
                const lobbyData = {
                    roomId: roomId,
                    players: room.players,
                    isHost: socket.id === room.host
                };

                socket.emit('room-joined', lobbyData);
                socket.to(roomId).emit('lobby-update', lobbyData);
            });

            // Запуск кооперативной игры
            socket.on('start-coop-game', (data) => {
                try {
                    const room = this.rooms.get(data.roomId);
                    if (!room || !room.players.princess || !room.players.helper) {
                        socket.emit('error', 'Недостаточно игроков для начала игры');
                        return;
                    }

                    if (socket.id !== room.host) {
                        socket.emit('error', 'Только создатель комнаты может начать игру');
                        return;
                    }

                    room.gameState = 'playing';
                    
                    const gameData = this.gameLogic.startGame(data.roomId, room.players);
                    
                    // Добавляем отладочную информацию
                    console.log('📡 Запуск игры, данные:', {
                        roomId: data.roomId,
                        stats: gameData.stats,
                        helperOutfit: gameData.stats?.helper?.outfit,
                        princessOutfit: gameData.stats?.princess?.outfit
                    });
                    
                    this.io.to(data.roomId).emit('game-started', gameData);
                } catch (error) {
                    console.error('❌ Ошибка запуска игры:', error);
                    socket.emit('error', 'Не удалось запустить игру');
                }
            });

            // Обработка выборов
            socket.on('make-choice', (data) => {
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

                    console.log(`🎯 Получен выбор: ${data.choiceId} от ${socket.id} для ${data.character}`);

                    const result = this.gameLogic.makeChoice(roomId, socket.id, data.choiceId, data.character);
                    
                    if (result.success) {
                        console.log('✅ Выбор обработан, отправляем обновление:', {
                            helperOutfit: result.gameData.stats?.helper?.outfit,
                            princessOutfit: result.gameData.stats?.princess?.outfit
                        });
                        
                        this.io.to(roomId).emit('game-update', result.gameData);
                    } else {
                        socket.emit('error', result.message);
                    }
                } catch (error) {
                    console.error('❌ Ошибка обработки выбора:', error);
                    socket.emit('error', 'Не удалось обработать выбор');
                }
            });

            // Чат
            socket.on('chat-message', (data) => {
                const roomId = this.playerRooms.get(socket.id);
                if (roomId) {
                    const room = this.rooms.get(roomId);
                    let playerName = 'Игрок';
                    
                    if (room.players.princess?.id === socket.id) {
                        playerName = 'Княжна';
                    } else if (room.players.helper?.id === socket.id) {
                        playerName = 'Помощница';
                    }

                    this.io.to(roomId).emit('chat-message', {
                        playerName: playerName,
                        message: data.message,
                        timestamp: new Date()
                    });
                }
            });

            // Покидание комнаты
            socket.on('leave-room', (roomId) => {
                this.handlePlayerLeave(socket, roomId);
            });

            // Отключение
            socket.on('disconnect', () => {
                console.log(`❌ Игрок отключился: ${socket.id}`);
                const roomId = this.playerRooms.get(socket.id);
                if (roomId) {
                    this.handlePlayerLeave(socket, roomId);
                }
            });
        });
    }

    handlePlayerLeave(socket, roomId) {
        const room = this.rooms.get(roomId);
        if (!room) return;

        // Удаляем игрока из комнаты
        if (room.players.princess?.id === socket.id) {
            room.players.princess = null;
        } else if (room.players.helper?.id === socket.id) {
            room.players.helper = null;
        }

        this.playerRooms.delete(socket.id);
        socket.leave(roomId);

        // Если в комнате больше никого нет, удаляем её
        if (!room.players.princess && !room.players.helper) {
            this.rooms.delete(roomId);
            this.gameLogic.removeGame(roomId);
            console.log(`🗑️ Комната ${roomId} удалена`);
        } else {
            // Уведомляем оставшихся игроков
            socket.to(roomId).emit('player-left', {
                roomId: roomId,
                players: room.players
            });
        }
    }

    generateRoomId() {
        const chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
        let result = '';
        for (let i = 0; i < 6; i++) {
            result += chars.charAt(Math.floor(Math.random() * chars.length));
        }
        return result;
    }
}

module.exports = SocketHandler;
