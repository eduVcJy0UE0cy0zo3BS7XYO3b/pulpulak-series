const express = require('express');
const http = require('http');
const socketIo = require('socket.io');
const path = require('path');
const SocketHandler = require('./network/socketHandler');

const app = express();
const server = http.createServer(app);
const io = socketIo(server, {
    cors: {
        origin: "*",
        methods: ["GET", "POST"]
    }
});

// Настройка статических файлов
app.use(express.static(path.join(__dirname, 'public')));

// Главная страница
app.get('/', (req, res) => {
    res.sendFile(path.join(__dirname, 'public', 'index.html'));
});

// Инициализация обработчика сокетов
const socketHandler = new SocketHandler(io);

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
