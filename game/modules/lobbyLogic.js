const GameStateManager = require('../gameStateManager');
const QuestIntegration = require('../questSystem/questIntegration');

class LobbyLogic {
    constructor(getNPCsForLocationFn) {
        this.games = new Map(); // roomId -> gameState
        this.questIntegrations = new Map(); // roomId -> questIntegration
        this.stateManager = new GameStateManager();
        this.getNPCsForLocation = getNPCsForLocationFn;
    }

    startGame(roomId, players) {
        try {
            if (!roomId || typeof roomId !== 'string') {
                throw new Error('Неверный ID комнаты');
            }
            if (!players || !players.princess || !players.helper) {
                throw new Error('Недостаточно игроков для начала игры');
            }

            const gameState = this.stateManager.createInitialState(roomId, players);
            this.games.set(roomId, gameState);
            
            this.questIntegrations.set(roomId, new QuestIntegration(this));
            
            try {
                gameState.stats.princess.npcsPresent = this.getNPCsForLocation(gameState.stats.princess.location, gameState, 'princess');
                gameState.stats.helper.npcsPresent = this.getNPCsForLocation(gameState.stats.helper.location, gameState, 'helper');
            } catch (npcError) {
                console.error('Ошибка при инициализации NPC:', npcError);
                gameState.stats.princess.npcsPresent = [];
                gameState.stats.helper.npcsPresent = [];
            }
            
            return gameState;
        } catch (error) {
            console.error('Ошибка при запуске игры:', error);
            throw new Error(`Не удалось запустить игру: ${error.message}`);
        }
    }

    removeGame(roomId) {
        this.games.delete(roomId);
        this.questIntegrations.delete(roomId);
    }

    getGameState(roomId) {
        return this.games.get(roomId) || null;
    }

    getQuestIntegration(roomId) {
        return this.questIntegrations.get(roomId);
    }

    switchTurn(gameState) {
        gameState.turnOrder = gameState.turnOrder === 'princess' ? 'helper' : 'princess';
    }
}

module.exports = LobbyLogic;