// Game logic
const CoopGameLogic = require('../game/coopGameLogic');
const gameConfig = require('../config/gameConfig');

class SocketHandler {
    constructor(io, gameConfigurationOrRegistry) {
        this.io = io;
        this.rooms = new Map();
        this.playerRooms = new Map(); // socketId -> roomId
        
        // Support both GameRegistry (new) and single game config (backward compatibility)
        if (gameConfigurationOrRegistry && typeof gameConfigurationOrRegistry.scanGames === 'function') {
            // New multi-game mode with GameRegistry
            this.gameRegistry = gameConfigurationOrRegistry;
            this.gameConfig = null; // Will be loaded per room
            this.gameLogic = null; // Will be created per room
            this.multiGameMode = true;
            console.log('🎮 Socket handler initialized in multi-game mode with GameRegistry');
        } else {
            // Backward compatibility mode with single game config
            this.gameRegistry = null;
            this.gameConfig = gameConfigurationOrRegistry;
            this.gameLogic = new CoopGameLogic(gameConfigurationOrRegistry);
            this.multiGameMode = false;
            console.log('🎮 Socket handler initialized in single-game mode with game configuration');
        }
        
        this.setupEventHandlers();
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
            if (room.gameLogic && typeof room.gameLogic.removeGame === 'function') {
                room.gameLogic.removeGame(roomId);
            } else if (this.gameLogic && typeof this.gameLogic.removeGame === 'function') {
                this.gameLogic.removeGame(roomId);
            }
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
        socket.on('createRoom', (data) => this.handleCreateRoom(socket, data));
        socket.on('join-room', (data) => this.handleJoinRoom(socket, data));
        socket.on('joinRoom', (data) => this.handleJoinRoom(socket, data));
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

    async handleCreateRoom(socket, data) {
        try {
            // Extract parameters from data
            const gameId = data?.gameId;
            const playerName = data?.playerName || data?.username || socket.username || `Игрок ${socket.id.substring(0, 6)}`;
            
            // Set username if provided
            if (data && (data.username || data.playerName)) {
                socket.username = data.username || data.playerName;
            }

            // Multi-game mode: validate and load game
            if (this.multiGameMode) {
                // For backward compatibility, default to 'pulpulak' if no gameId provided
                const actualGameId = gameId || 'pulpulak';

                // Load game configuration
                const gameConfig = await this.gameRegistry.getGameConfig(actualGameId);
                if (!gameConfig) {
                    socket.emit('error', { message: `Game not found: ${actualGameId}` });
                    return;
                }

                // Create room with game-specific data
                const roomId = this.generateRoomId();
                const CoopGameLogic = require('../game/coopGameLogic');
                const gameLogic = new CoopGameLogic(gameConfig);
                
                const roomData = {
                    id: roomId,
                    gameId: actualGameId,
                    gameConfig: gameConfig,
                    gameLogic: gameLogic,
                    players: {
                        princess: { id: socket.id, name: playerName },
                        helper: null
                    },
                    gameState: 'waiting'
                };

                this.rooms.set(roomId, roomData);
                this.playerRooms.set(socket.id, roomId);
                socket.join(roomId);

                console.log(`🏠 Multi-game room created: ${roomId} for game ${actualGameId}, player ${playerName}`);
                
                socket.emit('roomCreated', {
                    success: true,
                    roomCode: roomId,
                    gameId: actualGameId,
                    players: roomData.players
                });
            } else {
                // Backward compatibility mode
                const roomId = this.generateRoomId();
                
                const roomData = {
                    id: roomId,
                    gameId: 'pulpulak', // Default to pulpulak for backward compatibility
                    gameConfig: this.gameConfig,
                    gameLogic: this.gameLogic,
                    players: {
                        princess: { id: socket.id, name: playerName },
                        helper: null
                    },
                    gameState: 'waiting'
                };

                this.rooms.set(roomId, roomData);
                this.playerRooms.set(socket.id, roomId);
                socket.join(roomId);

                console.log(`🏠 Legacy room created: ${roomId}, player ${playerName}`);
                
                // Support both old and new event names for backward compatibility
                socket.emit('room-created', {
                    roomId: roomId,
                    players: roomData.players
                });
                socket.emit('roomCreated', {
                    success: true,
                    roomCode: roomId,
                    gameId: 'pulpulak',
                    players: roomData.players
                });
            }
        } catch (error) {
            console.error('❌ Error creating room:', error);
            socket.emit('error', { message: 'Failed to create room' });
        }
    }

    handleJoinRoom(socket, data) {
        const roomId = typeof data === 'string' ? data : (data.roomId || data.roomCode);
        
        if (data && (data.username || data.playerName)) {
            socket.username = data.username || data.playerName;
        }
        const room = this.rooms.get(roomId);
        if (!room) {
            socket.emit('error', { message: 'Комната не найдена' });
            return;
        }

        if (room.gameState !== 'waiting') {
            socket.emit('error', { message: 'Игра уже началась' });
            return;
        }

        const playerName = socket.username || `Игрок ${socket.id.substring(0, 6)}`;
        
        if (!room.players.princess) {
            room.players.princess = { id: socket.id, name: playerName };
        } else if (!room.players.helper) {
            room.players.helper = { id: socket.id, name: playerName };
        } else {
            socket.emit('error', { message: 'Комната полна' });
            return;
        }

        this.playerRooms.set(socket.id, roomId);
        socket.join(roomId);

        console.log(`🚪 Игрок ${socket.id} присоединился к комнате ${roomId}`);

        const lobbyData = {
            roomId: roomId,
            players: room.players
        };

        // Support both old and new event names
        socket.emit('room-joined', lobbyData);
        socket.emit('roomJoined', {
            success: true,
            gameId: room.gameId,
            ...lobbyData
        });
        socket.to(roomId).emit('lobby-update', lobbyData);
    }

    handleStartCoopGame(socket, data) {
        try {
            const room = this.rooms.get(data.roomId);
            if (!room || !room.players.princess || !room.players.helper) {
                socket.emit('error', { message: 'Недостаточно игроков для начала игры' });
                return;
            }

            room.gameState = 'playing';
            const gameLogic = room.gameLogic || this.gameLogic;
            const gameData = gameLogic.startGame(data.roomId, room.players);
            
            this.io.to(data.roomId).emit('game-started', gameData);
        } catch (error) {
            console.error('❌ Ошибка запуска игры:', error);
            socket.emit('error', { message: 'Не удалось запустить игру' });
        }
    }

    handleMakeChoice(socket, data) {
        try {
            const roomId = this.playerRooms.get(socket.id);
            if (!roomId) {
                socket.emit('error', { message: 'Вы не находитесь в игре' });
                return;
            }

            const room = this.rooms.get(roomId);
            if (!room || room.gameState !== 'playing') {
                socket.emit('error', { message: 'Игра не найдена или не началась' });
                return;
            }

            const gameLogic = room.gameLogic || this.gameLogic;
            const result = gameLogic.makeChoice(roomId, socket.id, data.choiceId, data.character);
            
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
                socket.emit('error', { message: 'Вы не находитесь в игре' });
                return;
            }

            const room = this.rooms.get(roomId);
            if (!room || room.gameState !== 'playing') {
                socket.emit('error', { message: 'Игра не найдена или не началась' });
                return;
            }

            const gameLogic = room.gameLogic || this.gameLogic;
            const result = gameLogic.processNPCDialogueChoice(roomId, socket.id, data.choiceId, data.character);
            
            if (result.success) {
                const updatedGameData = gameLogic.getGameData(roomId);
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
                socket.emit('error', { message: 'Вы не находитесь в игре' });
                return;
            }

            const room = this.rooms.get(roomId);
            const gameLogic = room?.gameLogic || this.gameLogic;
            const result = gameLogic.closeNPCDialogue(roomId, socket.id);
            
            if (result.success) {
                const updatedGameData = gameLogic.getGameData(roomId);
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
                socket.emit('error', { message: 'Вы не находитесь в игре' });
                return;
            }

            const room = this.rooms.get(roomId);
            if (!room || room.gameState !== 'playing') {
                socket.emit('error', { message: 'Игра не найдена или не началась' });
                return;
            }

            const gameLogic = room.gameLogic || this.gameLogic;
            const result = gameLogic.createRequest(
                roomId, 
                data.requestType, 
                socket.id, 
                data.character, 
                data.requestData || {}
            );
            
            if (result.success) {
                const updatedGameData = gameLogic.getGameData(roomId);
                
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
                socket.emit('error', { message: 'Вы не находитесь в игре' });
                return;
            }

            const room = this.rooms.get(roomId);
            const gameLogic = room?.gameLogic || this.gameLogic;
            const result = gameLogic.respondToRequest(
                roomId, 
                socket.id, 
                data.accepted, 
                data.responseData || {}
            );
            
            if (result.success) {
                const updatedGameData = gameLogic.getGameData(roomId);
                
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
