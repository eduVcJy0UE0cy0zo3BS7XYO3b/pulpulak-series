const express = require('express');
const http = require('http');
const socketIo = require('socket.io');
const path = require('path');
const SocketHandler = require('./network/socketHandler');
const GameRegistry = require('./game/GameRegistry');

const app = express();
const server = http.createServer(app);
const io = socketIo(server, {
    cors: {
        origin: "*",
        methods: ["GET", "POST"]
    }
});

// Middleware for JSON parsing
app.use(express.json());

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

// Тестовая страница для отладки
app.get('/test-outfit', (req, res) => {
    res.sendFile(path.join(__dirname, 'test_outfit_button.html'));
});

// Initialize the game registry
const gameRegistry = new GameRegistry();

// Initialize and scan available games
async function initializeGames() {
    try {
        await gameRegistry.scanGames();
        console.log('✅ Games scanned successfully');
        
        const availableGames = gameRegistry.getAvailableGames();
        console.log(`📚 Found ${availableGames.length} games:`, availableGames.map(g => g.name).join(', '));
    } catch (error) {
        console.error('❌ Failed to scan games:', error.message);
    }
}

// API Routes for multi-game support
app.get('/api/games', async (req, res) => {
    try {
        const games = gameRegistry.getAvailableGames();
        res.json(games);
    } catch (error) {
        console.error('Failed to load games:', error);
        res.status(500).json({ error: 'Failed to load games' });
    }
});

app.get('/api/games/:gameId/config', async (req, res) => {
    try {
        const { gameId } = req.params;
        
        // Validate gameId
        if (!gameId || typeof gameId !== 'string' || gameId.trim() === '') {
            return res.status(404).json({ error: 'Game not found' });
        }
        
        const gameConfig = await gameRegistry.getGameConfig(gameId);
        
        if (!gameConfig) {
            return res.status(404).json({ error: 'Game not found' });
        }

        const clientData = gameConfig.getClientData();
        res.json(clientData);
    } catch (error) {
        console.error('Failed to load game configuration:', error);
        res.status(500).json({ error: 'Failed to load game configuration' });
    }
});

// For backward compatibility, keep the old single-game approach as fallback
// This ensures existing functionality continues to work
let defaultGameConfig = null;
let socketHandler = null;

async function initializeDefaultGame() {
    try {
        // Try to load Pulpulak as the default game
        defaultGameConfig = await gameRegistry.getGameConfig('pulpulak');
        if (defaultGameConfig) {
            // Initialize socket handler with backward compatibility
            socketHandler = new SocketHandler(io, defaultGameConfig);
            console.log('✅ Socket handler initialized with Pulpulak game');
        } else {
            console.warn('⚠️ Pulpulak game not found, multi-game mode only');
        }
    } catch (error) {
        console.error('Failed to load default game:', error.message);
    }
}

// Обработка ошибок сервера
process.on('uncaughtException', (error) => {
    console.error('Необработанная ошибка:', error);
});

process.on('unhandledRejection', (reason, promise) => {
    console.error('Необработанное отклонение промиса:', reason);
});

const PORT = process.env.PORT || 3000;

// Initialize everything and start server
async function startServer() {
    try {
        // Initialize games first
        await initializeGames();
        await initializeDefaultGame();
        
        // Start the server
        server.listen(PORT, () => {
            console.log(`🚀 Server running on port ${PORT}`);
            console.log(`🎮 Open http://localhost:${PORT} to play`);
            console.log(`🔧 API available at http://localhost:${PORT}/api/games`);
        });
    } catch (error) {
        console.error('❌ Failed to start server:', error);
        process.exit(1);
    }
}

// Only start if this is the main module (not during testing)
if (require.main === module) {
    startServer();
}

// Export for testing
module.exports = { app, server, gameRegistry };