/**
 * PlayerDataManager - Управление данными игроков
 * Отвечает за состояние персонажей, их местоположение, инвентарь и статистику
 */

const ImmerStateManager = require('../stateManager');

class PlayerDataManager {
    constructor(gameDataManager) {
        this.gameData = gameDataManager;
        this.npcData = gameDataManager.gameConfig.getNPCData();
        this.immerStateManager = new ImmerStateManager();
    }

    /**
     * Получить данные персонажа
     */
    getPlayerData(roomId, character) {
        const gameState = this.gameData.getGame(roomId);
        return gameState?.stats[character];
    }

    /**
     * Обновить местоположение персонажа
     */
    updateLocation(roomId, character, newLocation) {
        const gameState = this.gameData.getGame(roomId);
        if (gameState?.stats[character]) {
            const updatedGameState = this.immerStateManager.updateState(gameState, draft => {
                draft.stats[character].location = newLocation;
                // Обновляем список NPC в новой локации
                draft.stats[character].npcsPresent = this.getNPCsForLocation(newLocation, gameState, character);
            });
            this.gameData.games.set(roomId, updatedGameState);
        }
    }

    /**
     * Обновить наряд персонажа
     */
    updateOutfit(roomId, character, newOutfit) {
        const gameState = this.gameData.getGame(roomId);
        if (gameState?.stats[character]) {
            const updatedGameState = this.immerStateManager.updateState(gameState, draft => {
                draft.stats[character].outfit = newOutfit;
            });
            this.gameData.games.set(roomId, updatedGameState);
        }
    }

    /**
     * Поменять наряды между персонажами
     */
    swapOutfits(roomId) {
        const gameState = this.gameData.getGame(roomId);
        if (gameState) {
            const characters = this.gameData.gameConfig.getCharacters();
            if (characters.length >= 2) {
                const updatedGameState = this.immerStateManager.updateState(gameState, draft => {
                    const char1 = characters[0];
                    const char2 = characters[1];
                    const tempOutfit = draft.stats[char1].outfit;
                    draft.stats[char1].outfit = draft.stats[char2].outfit;
                    draft.stats[char2].outfit = tempOutfit;
                });
                // Заменяем объект в GameDataManager
                this.gameData.games.set(roomId, updatedGameState);
            }
        }
    }

    /**
     * Добавить предмет в инвентарь
     */
    addToInventory(roomId, character, item) {
        const gameState = this.gameData.getGame(roomId);
        if (gameState?.stats[character]) {
            const updatedGameState = this.immerStateManager.updateState(gameState, draft => {
                draft.stats[character].inventory.push(item);
            });
            this.gameData.games.set(roomId, updatedGameState);
        }
    }

    /**
     * Удалить предмет из инвентаря
     */
    removeFromInventory(roomId, character, item) {
        const gameState = this.gameData.getGame(roomId);
        if (gameState?.stats[character]) {
            const updatedGameState = this.immerStateManager.updateState(gameState, draft => {
                const inventory = draft.stats[character].inventory;
                const index = inventory.indexOf(item);
                if (index > -1) {
                    inventory.splice(index, 1);
                }
            });
            this.gameData.games.set(roomId, updatedGameState);
        }
    }

    /**
     * Проверить наличие предмета в инвентаре
     */
    hasItem(roomId, character, item) {
        const gameState = this.gameData.getGame(roomId);
        return gameState?.stats[character]?.inventory.includes(item) || false;
    }

    /**
     * Обновить осведомлённость персонажа
     */
    updateAwareness(roomId, character, change) {
        const gameState = this.gameData.getGame(roomId);
        if (gameState?.stats[character]) {
            const updatedGameState = this.immerStateManager.updateState(gameState, draft => {
                draft.stats[character].awareness += change;
            });
            this.gameData.games.set(roomId, updatedGameState);
        }
    }

    /**
     * Получить текущую осведомлённость
     */
    getAwareness(roomId, character) {
        const gameState = this.gameData.getGame(roomId);
        return gameState?.stats[character]?.awareness || 0;
    }

    /**
     * Проверить, находятся ли персонажи в одной локации
     */
    arePlayersInSameLocation(roomId) {
        const gameState = this.gameData.getGame(roomId);
        if (!gameState) return false;
        
        const characters = this.gameData.gameConfig.getCharacters();
        if (characters.length < 2) return false;
        
        const char1Location = gameState.stats[characters[0]]?.location;
        const char2Location = gameState.stats[characters[1]]?.location;
        
        return char1Location && char2Location && char1Location === char2Location;
    }

    /**
     * Проверить, есть ли NPC рядом с персонажем
     */
    hasNPCsNearby(roomId, character) {
        const gameState = this.gameData.getGame(roomId);
        if (!gameState?.stats[character]) return false;
        
        return gameState.stats[character].npcsPresent.length > 0;
    }

    /**
     * Проверить, нет ли NPC рядом с обоими персонажами
     */
    bothPlayersHaveNoNPCs(roomId) {
        const characters = this.gameData.gameConfig.getCharacters();
        return characters.every(character => !this.hasNPCsNearby(roomId, character));
    }

    /**
     * Получить список NPC для локации
     */
    getNPCsForLocation(location, gameState, character) {
        const npcs = this.npcData.getNPCsForLocation(location, gameState, character);
        return npcs.map(npc => npc.name);
    }

    /**
     * Обновить список NPC для персонажа
     */
    updateNPCsPresent(roomId, character) {
        const gameState = this.gameData.getGame(roomId);
        if (gameState?.stats[character]) {
            const location = gameState.stats[character].location;
            const updatedGameState = this.immerStateManager.updateState(gameState, draft => {
                draft.stats[character].npcsPresent = this.getNPCsForLocation(location, gameState, character);
            });
            this.gameData.games.set(roomId, updatedGameState);
        }
    }

    /**
     * Обновить списки NPC для всех персонажей
     */
    updateAllNPCsPresent(roomId) {
        const gameState = this.gameData.getGame(roomId);
        if (!gameState) return;

        const characters = this.gameData.gameConfig.getCharacters();

        const updatedGameState = this.immerStateManager.updateState(gameState, draft => {
            characters.forEach(character => {
                if (draft.stats[character]) {
                    draft.stats[character].npcsPresent = this.getNPCsForLocation(
                        draft.stats[character].location, 
                        gameState, 
                        character
                    );
                }
            });
        });
        
        this.gameData.games.set(roomId, updatedGameState);
    }

    /**
     * Получить ID игрока для персонажа
     */
    getPlayerId(roomId, character) {
        const gameState = this.gameData.getGame(roomId);
        return gameState?.players[character]?.id;
    }

    /**
     * Получить имя игрока для персонажа
     */
    getPlayerName(roomId, character) {
        const gameState = this.gameData.getGame(roomId);
        return gameState?.players[character]?.name;
    }

    /**
     * Проверить, принадлежит ли персонаж игроку
     */
    isPlayerCharacter(roomId, playerId, character) {
        const gameState = this.gameData.getGame(roomId);
        if (!gameState?.players[character]) return false;
        return gameState.players[character].id === playerId;
    }

    /**
     * Найти персонажа по ID игрока
     */
    findPlayerCharacter(roomId, playerId) {
        const gameState = this.gameData.getGame(roomId);
        if (!gameState) return null;

        for (const [character, player] of Object.entries(gameState.players)) {
            if (player && player.id === playerId) {
                return character;
            }
        }
        return null;
    }

    /**
     * Получить полную копию статистики игрока (для клиента)
     */
    getPlayerStats(roomId, character) {
        const gameState = this.gameData.getGame(roomId);
        if (!gameState?.stats[character]) return null;
        
        return JSON.parse(JSON.stringify(gameState.stats[character]));
    }


    /**
     * Получить полную копию статистики всех игроков
     */
    getAllPlayerStats(roomId) {
        const gameState = this.gameData.getGame(roomId);
        if (!gameState) return null;
        
        const characters = this.gameData.gameConfig.getCharacters();
        const stats = {};
        
        characters.forEach(character => {
            stats[character] = this.getPlayerStats(roomId, character);
        });
        
        return stats;
    }
}

module.exports = PlayerDataManager;