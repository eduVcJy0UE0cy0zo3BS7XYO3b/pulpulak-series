/**
 * PlayerDataManager - Управление данными игроков
 * Отвечает за состояние персонажей, их местоположение, инвентарь и статистику
 */

const NPCData = require('../npcData');
const ImmerStateManager = require('../stateManager');

class PlayerDataManager {
    constructor(gameDataManager) {
        this.gameData = gameDataManager;
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
            const updatedGameState = this.immerStateManager.updateState(gameState, draft => {
                const tempOutfit = draft.stats.princess.outfit;
                draft.stats.princess.outfit = draft.stats.helper.outfit;
                draft.stats.helper.outfit = tempOutfit;
            });
            // Заменяем объект в GameDataManager
            this.gameData.games.set(roomId, updatedGameState);
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
        
        return gameState.stats.princess.location === gameState.stats.helper.location;
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
        return !this.hasNPCsNearby(roomId, 'princess') && !this.hasNPCsNearby(roomId, 'helper');
    }

    /**
     * Получить список NPC для локации
     */
    getNPCsForLocation(location, gameState, character) {
        const npcs = NPCData.getNPCsForLocation(location, gameState, character);
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

        const updatedGameState = this.immerStateManager.updateState(gameState, draft => {
            // Обновляем NPC для княжны
            draft.stats.princess.npcsPresent = this.getNPCsForLocation(
                draft.stats.princess.location, 
                gameState, 
                'princess'
            );
            
            // Обновляем NPC для помощницы
            draft.stats.helper.npcsPresent = this.getNPCsForLocation(
                draft.stats.helper.location, 
                gameState, 
                'helper'
            );
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
        
        return {
            princess: this.getPlayerStats(roomId, 'princess'),
            helper: this.getPlayerStats(roomId, 'helper')
        };
    }
}

module.exports = PlayerDataManager;