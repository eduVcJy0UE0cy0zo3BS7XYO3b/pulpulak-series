/**
 * GameDataManager - Управление игровыми данными
 * Отвечает за создание, хранение и получение игровых данных
 */

class GameDataManager {
    constructor(gameConfig) {
        if (!gameConfig) {
            throw new Error('GameConfig is required');
        }
        this.gameConfig = gameConfig;
        this.storyData = gameConfig.getStoryData();
        this.locationData = gameConfig.getLocationData();
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
    buildClientGameData(roomId, choicesForCharacters, activeRequest) {
        const gameState = this.getGame(roomId);
        if (!gameState) return null;

        const sceneData = this.storyData.getScene(gameState.currentScene);
        const characters = this.gameConfig.getCharacters();
        
        // Build locations data for all characters dynamically
        const locations = {};
        characters.forEach(character => {
            if (gameState.stats[character]) {
                locations[character] = this.locationData.getLocationInfo(gameState.stats[character].location);
            }
        });

        // Build quests data for all characters dynamically
        const quests = {};
        characters.forEach(character => {
            if (gameState.quests && gameState.quests[character]) {
                quests[character] = {
                    active: gameState.quests[character].active,
                    completed: gameState.quests[character].completed ? gameState.quests[character].completed.length : 0
                };
            }
        });

        // Build NPC dialogues for all characters dynamically
        const npcDialogues = {};
        characters.forEach(character => {
            npcDialogues[character] = gameState.npcDialogues && gameState.npcDialogues[character] ? gameState.npcDialogues[character] : null;
        });
        
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
            locations: locations,
            activeRequest: activeRequest,
            // Keep legacy field for backward compatibility
            activeOutfitRequest: activeRequest?.type === 'outfit_swap' ? activeRequest : null,
            npcDialogues: npcDialogues,
            quests: quests
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