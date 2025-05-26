const CoopGameLogic = require('../game/coopGameLogic'); // Добавить импорт

class SocketHandler {
    constructor(io) {
        this.io = io;
        this.rooms = new Map();
        this.playerRooms = new Map();
        this.gameLogic = new CoopGameLogic(); // Добавить экземпляр игровой логики
        this.setupEventHandlers();
    }

    // В обработчике start-coop-game заменить заглушку:
    socket.on('start-coop-game', (data) => {
	try {
            const room = this.rooms.get(data.roomId);
            if (!room || !room.players.princess || !room.players.helper) {
		socket.emit('error', 'Недостаточно игроков для начала игры');
		return;
            }

            if (room.players.princess.id !== socket.id) {
		socket.emit('error', 'Только создатель комнаты может начать игру');
		return;
            }

            room.gameState = 'playing';
            
            const gameData = this.gameLogic.startGame(data.roomId, room.players);
            
            // Добавим отладку перед отправкой
            console.log('📡 Отправка game-started:', {
		helperOutfit: gameData.stats?.helper?.outfit,
		princessOutfit: gameData.stats?.princess?.outfit
            });
            
            this.io.to(data.roomId).emit('game-started', gameData);
	} catch (error) {
            console.error('❌ Ошибка запуска игры:', error);
            socket.emit('error', 'Не удалось запустить игру');
	}
    });

    // В обработчике make-choice:
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

            // Используем реальную логику
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
    });
}
