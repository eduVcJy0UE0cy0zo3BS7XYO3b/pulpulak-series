/**
 * DataManagerFactory - Фабрика для создания и управления менеджерами данных
 * Централизованное создание и связывание всех менеджеров данных
 */

const GameDataManager = require('./GameDataManager');
const PlayerDataManager = require('./PlayerDataManager');
const QuestDataManager = require('./QuestDataManager');
const OutfitDataManager = require('./OutfitDataManager');

class DataManagerFactory {
    constructor() {
        this.managers = null;
    }

    /**
     * Создать и настроить всех менеджеров
     */
    createManagers() {
        if (this.managers) {
            return this.managers;
        }

        // Создаём базовый менеджер игровых данных
        const gameDataManager = new GameDataManager();
        
        // Создаём зависимые менеджеры
        const playerDataManager = new PlayerDataManager(gameDataManager);
        const questDataManager = new QuestDataManager(gameDataManager);
        const outfitDataManager = new OutfitDataManager(gameDataManager, playerDataManager);

        this.managers = {
            gameData: gameDataManager,
            playerData: playerDataManager,
            questData: questDataManager,
            outfitData: outfitDataManager
        };

        return this.managers;
    }

    /**
     * Получить менеджеров (создать если не существуют)
     */
    getManagers() {
        return this.managers || this.createManagers();
    }

    /**
     * Сбросить всех менеджеров (для тестов)
     */
    resetManagers() {
        this.managers = null;
    }
}

// Экспортируем синглтон
module.exports = new DataManagerFactory();