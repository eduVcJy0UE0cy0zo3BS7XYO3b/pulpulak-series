// New engine system
const GameEngineFactory = require('../engine/GameEngineFactory');
const LegacyGameAdapter = require('../engine/adapters/LegacyGameAdapter');
const PulpulakGameConfig = require('../games/pulpulak/PulpulakGameConfig');
const gameConfig = require('../config/gameConfig');

class SocketHandler {
    constructor(io) {
        this.io = io;
        this.rooms = new Map();
        this.playerRooms = new Map(); // socketId -> roomId
        
        // Create the new game engine with legacy adapter for backward compatibility
        const pulpulakConfig = new PulpulakGameConfig();
        this.gameLogic = new LegacyGameAdapter(pulpulakConfig);
        
        this.setupEventHandlers();
        
        console.log('🎮 Socket handler initialized with new game engine');
        console.log('🔧 Using Pulpulak game configuration');
        console.log('🔄 Legacy adapter enabled for backward compatibility');
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
        socket.on('request-outfit-swap', (data) => this.handleOutfitSwapRequest(socket, data));
        socket.on('respond-outfit-swap', (data) => this.handleOutfitSwapResponse(socket, data));
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

            console.log(`🎯 Получен выбор: ${data.choiceId} от ${socket.id} для ${data.character}`);

            if (data.choiceId === 'request_outfit_swap') {
                socket.emit('request-outfit-swap', { character: data.character });
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

    handleOutfitSwapRequest(socket, data) {
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

            console.log(`👗 Запрос обмена одеждой от ${socket.id} для ${data.character}`);

            const result = this.gameLogic.createOutfitSwapRequest(roomId, socket.id, data.character);
            
            if (result.success) {
                const updatedGameData = this.gameLogic.getGameData(roomId);
                this.io.to(roomId).emit('outfit-request-created', {
                    request: result.request,
                    message: result.message,
                    gameData: updatedGameData
                });
            } else {
                socket.emit('error', result.message);
            }
        } catch (error) {
            console.error('❌ Ошибка создания запроса обмена одеждой:', error);
            socket.emit('error', 'Не удалось создать запрос');
        }
    }

    handleOutfitSwapResponse(socket, data) {
        try {
            const roomId = this.playerRooms.get(socket.id);
            if (!roomId) {
                socket.emit('error', 'Вы не находитесь в игре');
                return;
            }

            console.log(`👗 Ответ на запрос обмена: ${data.accepted} от ${socket.id}`);

            const result = this.gameLogic.respondToOutfitSwapRequest(roomId, socket.id, data.accepted);
            
            if (result.success) {
                const updatedGameData = this.gameLogic.getGameData(roomId);
                this.io.to(roomId).emit('outfit-request-resolved', {
                    accepted: result.accepted,
                    declined: result.declined,
                    message: result.message,
                    gameData: updatedGameData
                });
            } else {
                socket.emit('error', result.message);
            }
        } catch (error) {
            console.error('❌ Ошибка ответа на запрос обмена одеждой:', error);
            socket.emit('error', 'Не удалось обработать ответ');
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

            console.log(`💬 Выбор в диалоге NPC от ${socket.id}: ${data.choiceId}`);

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

            console.log(`💬 Закрытие диалога NPC от ${socket.id}`);

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
}

module.exports = SocketHandler;
