/**
 * Pulpulak Server - полностью на Scheme логике
 * server-scheme.js
 */

const express = require('express');
const http = require('http');
const socketIo = require('socket.io');
const path = require('path');
const PulpulakSchemeBridge = require('./game/scheme-bridge');

const app = express();
const server = http.createServer(app);
const io = socketIo(server, {
    cors: {
        origin: "*",
        methods: ["GET", "POST"]
    }
});

// Инициализация Scheme игровой системы
const gameSystem = new PulpulakSchemeBridge();

// Настройка статических файлов
app.use(express.static(path.join(__dirname, 'public'), {
    setHeaders: (res, path) => {
        if (path.endsWith('.mjs')) {
            res.set('Content-Type', 'application/javascript');
        }
    }
}));

// Главная страница
app.get('/', (req, res) => {
    res.sendFile(path.join(__dirname, 'public', 'index.html'));
});

// API для статуса системы
app.get('/api/status', (req, res) => {
    res.json(gameSystem.getStatus());
});

// Обработка WebSocket соединений
io.on('connection', (socket) => {
    console.log('[Server] Player connected:', socket.id);

    // Создание новой игры
    socket.on('createRoom', async (data) => {
        try {
            const roomId = generateRoomId();
            const gameData = await gameSystem.createGame(roomId);
            
            socket.join(roomId);
            socket.emit('roomCreated', { roomId, gameData });
            
            console.log(`[Server] Room ${roomId} created by ${socket.id}`);
        } catch (error) {
            console.error('[Server] Error creating room:', error);
            socket.emit('error', { message: 'Failed to create room' });
        }
    });

    // Присоединение к игре
    socket.on('joinRoom', async (data) => {
        try {
            const { roomId, preferredCharacter } = data;
            const result = await gameSystem.joinGame(roomId, socket.id, preferredCharacter);
            
            if (result.success) {
                socket.join(roomId);
                socket.emit('roomJoined', { 
                    roomId, 
                    character: result.character,
                    gameData: await gameSystem.getGameData(roomId)
                });
                
                // Уведомляем других игроков
                socket.to(roomId).emit('playerJoined', { 
                    playerId: socket.id, 
                    character: result.character 
                });
                
                console.log(`[Server] Player ${socket.id} joined room ${roomId} as ${result.character}`);
            } else {
                socket.emit('error', { message: result.error });
            }
        } catch (error) {
            console.error('[Server] Error joining room:', error);
            socket.emit('error', { message: 'Failed to join room' });
        }
    });

    // Обработка выборов игрока
    socket.on('makeChoice', async (data) => {
        try {
            const { roomId, choiceId, character } = data;
            const result = await gameSystem.makeChoice(roomId, socket.id, choiceId, character);
            
            if (result.success) {
                // Отправляем обновленные данные всем игрокам в комнате
                io.to(roomId).emit('gameUpdate', {
                    gameData: result.gameData,
                    lastAction: {
                        player: socket.id,
                        character: character,
                        choice: choiceId,
                        message: result.message
                    }
                });
                
                console.log(`[Server] Choice processed: ${character} chose ${choiceId} in room ${roomId}`);
            } else {
                socket.emit('error', { message: result.message });
            }
        } catch (error) {
            console.error('[Server] Error processing choice:', error);
            socket.emit('error', { message: 'Failed to process choice' });
        }
    });

    // Обработка диалогов с NPC
    socket.on('npcDialogueChoice', async (data) => {
        try {
            const { roomId, choiceId, character } = data;
            const result = await gameSystem.processNPCDialogueChoice(roomId, socket.id, choiceId, character);
            
            if (result.success) {
                io.to(roomId).emit('gameUpdate', {
                    gameData: result.gameData,
                    lastAction: {
                        player: socket.id,
                        character: character,
                        choice: choiceId,
                        message: result.message,
                        type: 'dialogue'
                    }
                });
                
                console.log(`[Server] Dialogue processed: ${character} in room ${roomId}`);
            } else {
                socket.emit('error', { message: result.message });
            }
        } catch (error) {
            console.error('[Server] Error processing dialogue:', error);
            socket.emit('error', { message: 'Failed to process dialogue' });
        }
    });

    // Получение данных игры
    socket.on('getGameData', async (data) => {
        try {
            const { roomId } = data;
            const gameData = await gameSystem.getGameData(roomId);
            
            if (gameData) {
                socket.emit('gameData', gameData);
            } else {
                socket.emit('error', { message: 'Game not found' });
            }
        } catch (error) {
            console.error('[Server] Error getting game data:', error);
            socket.emit('error', { message: 'Failed to get game data' });
        }
    });

    // Отключение игрока
    socket.on('disconnect', () => {
        console.log('[Server] Player disconnected:', socket.id);
        
        // Здесь можно добавить логику для обработки отключения игрока
        // например, уведомление других игроков в комнате
    });

    // Ping для проверки соединения
    socket.on('ping', () => {
        socket.emit('pong');
    });
});

// Утилиты
function generateRoomId() {
    return Math.random().toString(36).substring(2, 8).toUpperCase();
}

// Ожидание инициализации Scheme системы перед запуском сервера
async function startServer() {
    try {
        // Ждем пока Scheme система инициализируется
        while (!gameSystem.isReady()) {
            await new Promise(resolve => setTimeout(resolve, 100));
        }
        
        const PORT = process.env.PORT || 3000;
        server.listen(PORT, () => {
            console.log('='.repeat(50));
            console.log('🎮 PULPULAK SCHEME SERVER READY 🎮');
            console.log('='.repeat(50));
            console.log(`Server running on http://localhost:${PORT}`);
            console.log(`Game System: ${gameSystem.getStatus().engine}`);
            console.log(`Mode: ${gameSystem.getStatus().mode}`);
            console.log('All game logic is running in pure Scheme!');
            console.log('='.repeat(50));
        });
    } catch (error) {
        console.error('Failed to start server:', error);
        process.exit(1);
    }
}

// Обработка ошибок
process.on('unhandledRejection', (reason, promise) => {
    console.error('Unhandled Rejection at:', promise, 'reason:', reason);
});

process.on('uncaughtException', (error) => {
    console.error('Uncaught Exception:', error);
    process.exit(1);
});

// Graceful shutdown
process.on('SIGTERM', () => {
    console.log('[Server] SIGTERM received, shutting down gracefully');
    server.close(() => {
        console.log('[Server] Server closed');
        process.exit(0);
    });
});

// Запуск сервера
startServer();