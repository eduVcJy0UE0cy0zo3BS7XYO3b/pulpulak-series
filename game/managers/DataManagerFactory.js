/**
 * DataManagerFactory - Фабрика для создания и управления менеджерами данных
 * Централизованное создание и связывание всех менеджеров данных
 */

const GameDataManager = require('./GameDataManager');
const PlayerDataManager = require('./PlayerDataManager');
const QuestDataManager = require('./QuestDataManager');
const RequestManager = require('./RequestManager');

class DataManagerFactory {
    /**
     * Create and configure all managers with game configuration
     * @param {IGameConfig} gameConfig - Game configuration implementing IGameConfig interface
     */
    static getManagers(gameConfig) {
        if (!gameConfig) {
            throw new Error('GameConfig is required');
        }

        // Create base game data manager with config
        const gameDataManager = new GameDataManager(gameConfig);
        
        // Create dependent managers
        const playerDataManager = new PlayerDataManager(gameDataManager);
        const questDataManager = new QuestDataManager(gameDataManager);
        const requestManager = new RequestManager(gameDataManager, playerDataManager);

        // Register game-specific request handlers from gameConfig
        const requestHandlers = gameConfig.getRequestHandlers();
        if (requestHandlers && typeof requestHandlers.registerHandlers === 'function') {
            requestHandlers.registerHandlers(requestManager);
        }

        return {
            gameData: gameDataManager,
            playerData: playerDataManager,
            questData: questDataManager,
            requestData: requestManager
        };
    }

    /**
     * Reset managers for testing purposes
     * Since we're now stateless, this is just a placeholder for backward compatibility
     */
    static resetManagers() {
        // This method exists for backward compatibility with tests
        // In the new architecture, managers are stateless and created on demand
    }
}

module.exports = DataManagerFactory;