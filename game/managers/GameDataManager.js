/**
 * GameDataManager - Управление игровыми данными
 * Отвечает за создание, хранение и получение игровых данных
 */

const CoopStoryData = require('../coopStoryData');
const LocationData = require('../locationData');

class GameDataManager {
    constructor() {
        this.games = new Map(); // roomId -> gameData
    }

    /**
     * Создать новую игру
     */
    createGame(roomId, players) {
        // Используем существующий GameStateManager для обратной совместимости
        const GameStateManager = require('../gameStateManager');
        const stateManager = new GameStateManager();
        const gameData = stateManager.createInitialState(roomId, players);

        this.games.set(roomId, gameData);
        return gameData;
    }

    /**
     * Получить игровые данные
     */
    getGame(roomId) {
        return this.games.get(roomId);
    }

    /**
     * Удалить игру
     */
    deleteGame(roomId) {
        return this.games.delete(roomId);
    }

    /**
     * Проверить существование игры
     */
    hasGame(roomId) {
        return this.games.has(roomId);
    }

    /**
     * Получить данные для отправки клиенту
     */
    buildClientGameData(roomId, choicesForCharacters, activeOutfitRequest) {
        const gameState = this.getGame(roomId);
        if (!gameState) return null;

        const sceneData = CoopStoryData.getScene(gameState.currentScene);
        
        return {
            roomId: roomId,
            players: gameState.players,
            scene: {
                title: sceneData.title,
                text: sceneData.text
            },
            choices: choicesForCharacters,
            stats: JSON.parse(JSON.stringify(gameState.stats)),
            currentTurn: gameState.turnOrder,
            chapter: gameState.chapter,
            locations: {
                princess: LocationData.getLocationInfo(gameState.stats.princess.location),
                helper: LocationData.getLocationInfo(gameState.stats.helper.location)
            },
            activeOutfitRequest: activeOutfitRequest,
            npcDialogues: {
                princess: gameState.npcDialogues?.princess || null,
                helper: gameState.npcDialogues?.helper || null
            },
            quests: {
                princess: {
                    active: gameState.quests.princess.active,
                    completed: gameState.quests.princess.completed.length
                },
                helper: {
                    active: gameState.quests.helper.active,
                    completed: gameState.quests.helper.completed.length
                }
            }
        };
    }

    /**
     * Обновить текущую сцену
     */
    updateScene(roomId, newScene) {
        const gameState = this.getGame(roomId);
        if (gameState) {
            // Используем замену объекта вместо мутации
            const updatedState = { ...gameState, currentScene: newScene };
            this.games.set(roomId, updatedState);
        }
    }

    /**
     * Переключить очередь хода
     */
    switchTurn(roomId) {
        const gameState = this.getGame(roomId);
        if (gameState) {
            // Используем замену объекта вместо мутации
            const newTurnOrder = gameState.turnOrder === 'princess' ? 'helper' : 'princess';
            const updatedState = { ...gameState, turnOrder: newTurnOrder };
            this.games.set(roomId, updatedState);
        }
    }

    /**
     * Обновить глобальную память квестов
     */
    updateGlobalQuestMemory(roomId, questId, value) {
        const gameState = this.getGame(roomId);
        if (gameState) {
            // Используем замену объекта вместо мутации
            const updatedState = { 
                ...gameState, 
                globalQuestMemory: { 
                    ...gameState.globalQuestMemory, 
                    [questId]: value 
                } 
            };
            this.games.set(roomId, updatedState);
        }
    }

    /**
     * Получить все активные игры
     */
    getAllGames() {
        return Array.from(this.games.keys());
    }

    /**
     * Получить количество активных игр
     */
    getGamesCount() {
        return this.games.size;
    }
}

module.exports = GameDataManager;