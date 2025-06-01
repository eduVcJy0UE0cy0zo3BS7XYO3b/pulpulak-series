/**
 * DataManagerFactory - Фабрика для создания и управления менеджерами данных
 * Централизованное создание и связывание всех менеджеров данных
 */

const GameDataManager = require('./GameDataManager');
const PlayerDataManager = require('./PlayerDataManager');
const QuestDataManager = require('./QuestDataManager');
const RequestManager = require('./RequestManager');

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
        const requestManager = new RequestManager(gameDataManager, playerDataManager);

        // Регистрируем игровые обработчики запросов
        this.registerGameRequestHandlers(requestManager);

        this.managers = {
            gameData: gameDataManager,
            playerData: playerDataManager,
            questData: questDataManager,
            requestData: requestManager
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
     * Зарегистрировать обработчики запросов для игр
     */
    registerGameRequestHandlers(requestManager) {
        try {
            // Регистрируем обработчики для игры Pulpulak
            const PulpulakRequestHandlers = require('../../games/pulpulak/requestHandlers');
            PulpulakRequestHandlers.registerHandlers(requestManager);
            console.log('✅ Зарегистрированы обработчики запросов для Pulpulak');
        } catch (error) {
            console.warn('⚠️ Не удалось зарегистрировать обработчики запросов Pulpulak:', error.message);
        }
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