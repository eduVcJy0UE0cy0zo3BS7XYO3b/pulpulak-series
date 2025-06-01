// Game logic
const CoopGameLogic = require('../game/coopGameLogic');
const gameConfig = require('../config/gameConfig');

class SocketHandler {
    constructor(io, gameConfiguration) {
        this.io = io;
        this.rooms = new Map();
        this.playerRooms = new Map(); // socketId -> roomId
        this.gameConfig = gameConfiguration;
        
        // Create game logic instance with configuration
        this.gameLogic = new CoopGameLogic(gameConfiguration);
        
        this.setupEventHandlers();
        
        console.log('🎮 Socket handler initialized with CoopGameLogic and game configuration');
    }

    setupEventHandlers() {
        this.io.on('connection', (socket) => {
            this.handleConnection(socket);
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
                players: room.players,
                playerName: socket.username || 'Игрок'
            });
        }
    }

    generateRoomId() {
        const chars = gameConfig.ROOM_ID_CHARS;
        let result = '';
        for (let i = 0; i < gameConfig.ROOM_ID_LENGTH; i++) {
            result += chars.charAt(Math.floor(Math.random() * chars.length));
        }
        return result;
    }

    handleConnection(socket) {
        console.log(`🔌 Игрок подключился: ${socket.id}`);
        
        socket.username = null;
        
        socket.on('set-username', (username) => {
            socket.username = username;
            console.log(`👤 Пользователь ${username} установлен для ${socket.id}`);
        });

        socket.on('create-room', (data) => this.handleCreateRoom(socket, data));
        socket.on('join-room', (data) => this.handleJoinRoom(socket, data));
        socket.on('start-coop-game', (data) => this.handleStartCoopGame(socket, data));
        socket.on('make-choice', (data) => this.handleMakeChoice(socket, data));
        socket.on('chat-message', (data) => this.handleChatMessage(socket, data));
        socket.on('create-request', (data) => this.handleCreateRequest(socket, data));
        socket.on('respond-request', (data) => this.handleRespondRequest(socket, data));
        socket.on('npc-dialogue-choice', (data) => this.handleNPCDialogueChoice(socket, data));
        socket.on('close-npc-dialogue', () => this.handleCloseNPCDialogue(socket));
        socket.on('leave-room', (roomId) => this.handlePlayerLeave(socket, roomId));
        socket.on('disconnect', () => this.handleDisconnect(socket));
    }

    handleCreateRoom(socket, data) {
        if (data && data.username) {
            socket.username = data.username;
        }
        const roomId = this.generateRoomId();
        const playerName = socket.username || `Игрок ${socket.id.substring(0, 6)}`;
        
        const roomData = {
            id: roomId,
            players: {
                princess: { id: socket.id, name: playerName },
                helper: null
            },
            gameState: 'waiting'
        };

        this.rooms.set(roomId, roomData);
        this.playerRooms.set(socket.id, roomId);
        socket.join(roomId);

        console.log(`🏠 Комната создана: ${roomId}, игрок ${playerName} - княжна`);
        
        socket.emit('room-created', {
            roomId: roomId,
            players: roomData.players
        });
    }

    handleJoinRoom(socket, data) {
        const roomId = typeof data === 'string' ? data : data.roomId;
        
        if (data && data.username) {
            socket.username = data.username;
        }
        const room = this.rooms.get(roomId);
        if (!room) {
            socket.emit('error', 'Комната не найдена');
            return;
        }

        if (room.gameState !== 'waiting') {
            socket.emit('error', 'Игра уже началась');
            return;
        }

        const playerName = socket.username || `Игрок ${socket.id.substring(0, 6)}`;
        
        if (!room.players.princess) {
            room.players.princess = { id: socket.id, name: playerName };
        } else if (!room.players.helper) {
            room.players.helper = { id: socket.id, name: playerName };
        } else {
            socket.emit('error', 'Комната полна');
            return;
        }

        this.playerRooms.set(socket.id, roomId);
        socket.join(roomId);

        console.log(`🚪 Игрок ${socket.id} присоединился к комнате ${roomId}`);

        const lobbyData = {
            roomId: roomId,
            players: room.players
        };

        socket.emit('room-joined', lobbyData);
        socket.to(roomId).emit('lobby-update', lobbyData);
    }

    handleStartCoopGame(socket, data) {
        try {
            const room = this.rooms.get(data.roomId);
            if (!room || !room.players.princess || !room.players.helper) {
                socket.emit('error', 'Недостаточно игроков для начала игры');
                return;
            }

            room.gameState = 'playing';
            const gameData = this.gameLogic.startGame(data.roomId, room.players);
            
            this.io.to(data.roomId).emit('game-started', gameData);
        } catch (error) {
            console.error('❌ Ошибка запуска игры:', error);
            socket.emit('error', 'Не удалось запустить игру');
        }
    }

    handleMakeChoice(socket, data) {
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

            const result = this.gameLogic.makeChoice(roomId, socket.id, data.choiceId, data.character);
            
            if (result.success) {
                this.io.to(roomId).emit('game-update', result.gameData);
            } else {
                socket.emit('error', result.message);
            }
        } catch (error) {
            console.error('❌ Ошибка обработки выбора:', error);
            socket.emit('error', 'Не удалось обработать выбор');
        }
    }

    handleChatMessage(socket, data) {
        const roomId = this.playerRooms.get(socket.id);
        if (roomId) {
            const room = this.rooms.get(roomId);
            let playerName = socket.username || 'Игрок';
            let role = '';
            
            if (room.players.princess?.id === socket.id) {
                role = ' (Княжна)';
            } else if (room.players.helper?.id === socket.id) {
                role = ' (Помощница)';
            }

            this.io.to(roomId).emit('chat-message', {
                sender: playerName + role,
                message: data.message,
                timestamp: new Date()
            });
        }
    }



    handleNPCDialogueChoice(socket, data) {
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

            const result = this.gameLogic.processNPCDialogueChoice(roomId, socket.id, data.choiceId, data.character);
            
            if (result.success) {
                const updatedGameData = this.gameLogic.getGameData(roomId);
                this.io.to(roomId).emit('game-state-updated', updatedGameData);
                
                if (result.message) {
                    this.io.to(roomId).emit('game-message', {
                        type: result.type || 'info',
                        message: result.message
                    });
                }
            } else {
                socket.emit('error', result.message);
            }
        } catch (error) {
            console.error('❌ Ошибка обработки диалога NPC:', error);
            socket.emit('error', 'Не удалось обработать выбор');
        }
    }

    handleCloseNPCDialogue(socket) {
        try {
            const roomId = this.playerRooms.get(socket.id);
            if (!roomId) {
                socket.emit('error', 'Вы не находитесь в игре');
                return;
            }

            const result = this.gameLogic.closeNPCDialogue(roomId, socket.id);
            
            if (result.success) {
                const updatedGameData = this.gameLogic.getGameData(roomId);
                this.io.to(roomId).emit('game-state-updated', updatedGameData);
            } else {
                socket.emit('error', result.message);
            }
        } catch (error) {
            console.error('❌ Ошибка закрытия диалога NPC:', error);
            socket.emit('error', 'Не удалось закрыть диалог');
        }
    }

    handleDisconnect(socket) {
        console.log(`❌ Игрок отключился: ${socket.id}`);
        const roomId = this.playerRooms.get(socket.id);
        if (roomId) {
            this.handlePlayerLeave(socket, roomId);
        }
    }

    // УНИВЕРСАЛЬНАЯ СИСТЕМА ЗАПРОСОВ

    handleCreateRequest(socket, data) {
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

            const result = this.gameLogic.createRequest(
                roomId, 
                data.requestType, 
                socket.id, 
                data.character, 
                data.requestData || {}
            );
            
            if (result.success) {
                const updatedGameData = this.gameLogic.getGameData(roomId);
                
                this.io.to(roomId).emit('request-created', {
                    request: result.request,
                    message: result.message,
                    gameData: updatedGameData
                });
                
            } else {
                socket.emit('error', result.message);
            }
        } catch (error) {
            console.error('❌ Ошибка создания запроса:', error);
            socket.emit('error', 'Не удалось создать запрос');
        }
    }

    handleRespondRequest(socket, data) {
        try {
            const roomId = this.playerRooms.get(socket.id);
            if (!roomId) {
                socket.emit('error', 'Вы не находитесь в игре');
                return;
            }

            const result = this.gameLogic.respondToRequest(
                roomId, 
                socket.id, 
                data.accepted, 
                data.responseData || {}
            );
            
            if (result.success) {
                const updatedGameData = this.gameLogic.getGameData(roomId);
                
                this.io.to(roomId).emit('request-resolved', {
                    accepted: result.accepted,
                    declined: result.declined,
                    message: result.message,
                    gameData: updatedGameData
                });
                
            } else {
                socket.emit('error', result.message);
            }
        } catch (error) {
            console.error('❌ Ошибка ответа на запрос:', error);
            socket.emit('error', 'Не удалось обработать ответ');
        }
    }
}

module.exports = SocketHandler;
