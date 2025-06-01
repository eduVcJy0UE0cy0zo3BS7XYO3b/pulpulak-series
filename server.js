const express = require('express');
const http = require('http');
const socketIo = require('socket.io');
const path = require('path');
const SocketHandler = require('./network/socketHandler');
const PulpulakGameConfig = require('./games/pulpulak/PulpulakGameConfig');

const app = express();
const server = http.createServer(app);
const io = socketIo(server, {
    cors: {
        origin: "*",
        methods: ["GET", "POST"]
    }
});

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

// Create game configuration
const gameConfig = new PulpulakGameConfig();

// Инициализация обработчика сокетов
const socketHandler = new SocketHandler(io, gameConfig);

// Обработка ошибок сервера
process.on('uncaughtException', (error) => {
    console.error('Необработанная ошибка:', error);
});

process.on('unhandledRejection', (reason, promise) => {
    console.error('Необработанное отклонение промиса:', reason);
});

const PORT = process.env.PORT || 3000;
server.listen(PORT, () => {
    console.log(`🏰 Сервер "Княжна Пулпулак" запущен на порту ${PORT}`);
    console.log(`📖 Игра доступна по адресу: http://localhost:${PORT}`);
    console.log(`🔌 Socket.IO инициализирован`);
});
