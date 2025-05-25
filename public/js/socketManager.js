class SocketManager {
    constructor() {
        this.socket = io();
        this.setupConnection();
    }

    setupConnection() {
        this.socket.on('connect', () => {
            console.log('🔌 Подключен к серверу');
        });

        this.socket.on('disconnect', () => {
            console.log('❌ Отключен от сервера');
        });

        this.socket.on('connect_error', (error) => {
            console.error('❌ Ошибка подключения:', error);
            alert('Ошибка подключения к серверу. Проверьте, что сервер запущен.');
        });

        this.socket.on('connection-test', (data) => {
            console.log('✅ Тест соединения прошел:', data);
        });
    }

    createRoom() {
        console.log('📝 Создание комнаты...');
        this.socket.emit('create-room');
    }

    joinRoom(roomId) {
        console.log(`🚪 Присоединение к комнате: ${roomId}`);
        this.socket.emit('join-room', roomId);
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
}

export default SocketManager;
