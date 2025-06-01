class SocketManager {
    constructor() {
        this.socket = io();
        this.username = null;
        this.setupConnection();
    }

    setupConnection() {
        this.socket.on('connect', () => {
            console.log('🔌 Подключен к серверу');
            // Send username if we have it
            if (this.username) {
                this.socket.emit('set-username', this.username);
            }
        });

        this.socket.on('disconnect', () => {
            console.log('❌ Отключен от сервера');
        });

        this.socket.on('connect_error', (error) => {
            console.error('❌ Ошибка подключения:', error);
            // NotificationManager will be imported by main app
            if (window.NotificationManager) {
                window.NotificationManager.add('Ошибка подключения к серверу. Проверьте, что сервер запущен.', 'error', 10000);
            }
        });

        this.socket.on('connection-test', (data) => {
            console.log('✅ Тест соединения прошел:', data);
        });
    }

    createRoom() {
        console.log('📝 Создание комнаты...');
        this.socket.emit('create-room', { username: this.username });
    }

    joinRoom(roomId) {
        console.log(`🚪 Присоединение к комнате: ${roomId}`);
        // Support both roomId and roomCode for compatibility
        this.socket.emit('join-room', { roomId, roomCode: roomId, username: this.username });
    }

    startCoopGame(roomId) {
        console.log(`🎮 Запуск игры в комнате: ${roomId}`);
        this.socket.emit('start-coop-game', { roomId });
    }

    makeChoice(roomId, choiceId, character) {
        console.log(`🎯 Выбор: ${choiceId} для ${character}`);
        this.socket.emit('make-choice', {
            roomId,
            choiceId,
            character
        });
    }

    sendChatMessage(roomId, message) {
        this.socket.emit('chat-message', {
            roomId,
            message
        });
    }

    leaveRoom(roomId) {
        this.socket.emit('leave-room', roomId);
    }

    setUsername(username) {
        this.username = username;
        if (this.socket.connected) {
            this.socket.emit('set-username', username);
        }
    }
}

export default SocketManager;
