/**
 * OutfitDataManager - Управление данными одежды и запросов на обмен
 * Отвечает за смену одежды, валидацию и обработку запросов на обмен
 */

const { OUTFIT_NAMES } = require('../../games/pulpulak/data/constants');

class OutfitDataManager {
    constructor(gameDataManager, playerDataManager) {
        this.gameData = gameDataManager;
        this.playerData = playerDataManager;
        this.outfitRequests = new Map(); // roomId -> activeRequest
    }

    /**
     * Получить название наряда
     */
    getOutfitName(outfitId) {
        return OUTFIT_NAMES[outfitId] || outfitId;
    }

    /**
     * Получить текущий наряд персонажа
     */
    getCurrentOutfit(roomId, character) {
        return this.playerData.getPlayerData(roomId, character)?.outfit;
    }

    /**
     * Проверить, можно ли переодеваться
     */
    canSwitchOutfits(roomId, character) {
        return this.validateOutfitChange(roomId, character);
    }

    /**
     * Валидация возможности смены одежды
     */
    validateOutfitChange(roomId, character) {
        const validators = [
            () => this.hasNoNPCs(roomId, character),
            () => this.playersInSameLocation(roomId),
            () => this.bothPlayersHaveNoNPCs(roomId)
        ];
        
        return validators.every(validate => validate());
    }

    /**
     * Проверить, нет ли NPC рядом с персонажем
     */
    hasNoNPCs(roomId, character) {
        return !this.playerData.hasNPCsNearby(roomId, character);
    }


    /**
     * Проверить, находятся ли игроки в одной локации
     */
    playersInSameLocation(roomId) {
        return this.playerData.arePlayersInSameLocation(roomId);
    }

    /**
     * Проверить, нет ли NPC рядом с обоими игроками
     */
    bothPlayersHaveNoNPCs(roomId) {
        return this.playerData.bothPlayersHaveNoNPCs(roomId);
    }

    /**
     * Создать запрос на обмен одеждой
     */
    createOutfitSwapRequest(roomId, fromPlayerId, fromCharacter) {
        // Проверяем валидность запроса
        const validation = this.validateOutfitSwapRequest(roomId, fromCharacter);
        if (!validation.valid) {
            return { success: false, message: validation.message };
        }

        // Создаём запрос
        const request = this.buildOutfitSwapRequest(roomId, fromPlayerId, fromCharacter);
        this.outfitRequests.set(roomId, request);

        return { 
            success: true, 
            request: request,
            message: `${this.getCharacterName(fromCharacter)} предлагает поменяться одеждой`
        };
    }

    /**
     * Валидация запроса на обмен одеждой
     */
    validateOutfitSwapRequest(roomId, fromCharacter) {
        if (!this.canSwitchOutfits(roomId, fromCharacter)) {
            return { 
                valid: false, 
                message: "Нельзя переодеваться при посторонних!" 
            };
        }

        if (this.outfitRequests.has(roomId)) {
            return { 
                valid: false, 
                message: "Уже есть активный запрос на обмен одеждой" 
            };
        }

        const gameState = this.gameData.getGame(roomId);
        const targetCharacter = fromCharacter === 'princess' ? 'helper' : 'princess';
        const targetPlayer = gameState?.players[targetCharacter];
        
        if (!targetPlayer) {
            return { valid: false, message: "Второй игрок не найден" };
        }

        return { valid: true, targetPlayer, targetCharacter };
    }

    /**
     * Создать объект запроса на обмен одеждой
     */
    buildOutfitSwapRequest(roomId, fromPlayerId, fromCharacter) {
        const gameState = this.gameData.getGame(roomId);
        const targetCharacter = fromCharacter === 'princess' ? 'helper' : 'princess';
        const targetPlayer = gameState.players[targetCharacter];
        
        return {
            id: this.generateRequestId(),
            roomId: roomId,
            fromPlayerId: fromPlayerId,
            fromCharacter: fromCharacter,
            targetPlayerId: targetPlayer.id,
            targetCharacter: targetCharacter,
            timestamp: Date.now()
        };
    }

    /**
     * Ответить на запрос обмена одеждой
     */
    respondToOutfitSwapRequest(roomId, playerId, accepted) {
        const request = this.outfitRequests.get(roomId);
        if (!request) {
            return { success: false, message: "Запрос не найден" };
        }

        if (request.targetPlayerId !== playerId) {
            return { success: false, message: "Этот запрос не для вас" };
        }

        // Удаляем запрос
        this.outfitRequests.delete(roomId);

        if (!accepted) {
            return this.handleDeclinedOutfitSwap(request);
        }

        return this.executeOutfitSwap(roomId, request);
    }

    /**
     * Обработать отклонение запроса
     */
    handleDeclinedOutfitSwap(request) {
        return { 
            success: true, 
            declined: true,
            message: `${this.getCharacterName(request.targetCharacter)} отклонила предложение обмена одеждой`
        };
    }

    /**
     * Выполнить обмен одеждой
     */
    executeOutfitSwap(roomId, request) {
        // Проверяем, что условия всё ещё позволяют обмен
        if (!this.canSwitchOutfits(roomId, request.fromCharacter)) {
            return { 
                success: false, 
                message: "Обстановка изменилась - больше нельзя переодеваться!" 
            };
        }

        // Выполняем обмен
        this.playerData.swapOutfits(roomId);

        const princessOutfit = this.getCurrentOutfit(roomId, 'princess');
        const helperOutfit = this.getCurrentOutfit(roomId, 'helper');

        return {
            success: true,
            accepted: true,
            message: `Персонажи поменялись одеждой! Княжна теперь в: ${this.getOutfitName(princessOutfit)}, помощница в: ${this.getOutfitName(helperOutfit)}`
        };
    }

    /**
     * Получить активный запрос для комнаты
     */
    getActiveOutfitRequest(roomId) {
        return this.outfitRequests.get(roomId) || null;
    }

    /**
     * Отменить запрос обмена одеждой
     */
    cancelOutfitRequest(roomId) {
        this.outfitRequests.delete(roomId);
    }

    /**
     * Проверить, есть ли активный запрос
     */
    hasActiveRequest(roomId) {
        return this.outfitRequests.has(roomId);
    }

    /**
     * Сменить наряд персонажа (административно)
     */
    changeOutfit(roomId, character, newOutfit) {
        this.playerData.updateOutfit(roomId, character, newOutfit);
        return {
            success: true,
            message: `${this.getCharacterName(character)} переоделась в: ${this.getOutfitName(newOutfit)}`
        };
    }

    /**
     * Получить информацию о нарядах всех персонажей
     */
    getAllOutfitInfo(roomId) {
        return {
            princess: {
                outfit: this.getCurrentOutfit(roomId, 'princess'),
                outfitName: this.getOutfitName(this.getCurrentOutfit(roomId, 'princess'))
            },
            helper: {
                outfit: this.getCurrentOutfit(roomId, 'helper'),
                outfitName: this.getOutfitName(this.getCurrentOutfit(roomId, 'helper'))
            }
        };
    }

    /**
     * Проверить, подходит ли наряд для взаимодействия с NPC
     */
    isOutfitSuitableForNPC(roomId, character, npcId, requiredOutfit) {
        const currentOutfit = this.getCurrentOutfit(roomId, character);
        return currentOutfit === requiredOutfit;
    }

    /**
     * Получить список доступных нарядов для персонажа
     */
    getAvailableOutfits(character) {
        if (character === 'princess') {
            return ['nightgown', 'princess_dress', 'court_dress'];
        } else {
            return ['common_dress', 'servant_outfit'];
        }
    }

    /**
     * Проверить, может ли персонаж носить определённый наряд
     */
    canWearOutfit(character, outfitId) {
        const available = this.getAvailableOutfits(character);
        return available.includes(outfitId);
    }

    /**
     * Вспомогательные методы
     */
    generateRequestId() {
        return Math.random().toString(36).substring(2, 15);
    }

    getCharacterName(character) {
        const names = {
            'princess': 'Княжна',
            'helper': 'Помощница'
        };
        return names[character] || character;
    }

    /**
     * Очистить все запросы для комнаты (при удалении игры)
     */
    clearRoomRequests(roomId) {
        this.outfitRequests.delete(roomId);
    }

    /**
     * Получить статистику запросов (для отладки)
     */
    getRequestStats() {
        return {
            activeRequests: this.outfitRequests.size,
            rooms: Array.from(this.outfitRequests.keys())
        };
    }
}

module.exports = OutfitDataManager;