/**
 * Логика переодевания для игры Pulpulak
 * Все правила и ограничения переодевания специфичные для этой игры
 */

class PulpulakOutfitLogic {
    /**
     * Проверить, можно ли переодеваться в данных условиях
     */
    static canSwitchOutfits(gameState, character) {
        return PulpulakOutfitLogic.validateOutfitChange(gameState, character);
    }

    /**
     * Валидация возможности смены одежды по правилам Pulpulak
     */
    static validateOutfitChange(gameState, character) {
        const validators = [
            () => PulpulakOutfitLogic.hasNoNPCs(gameState, character),
            () => PulpulakOutfitLogic.locationAllowsOutfitChange(gameState, character),
            () => PulpulakOutfitLogic.playersInSameLocation(gameState),
            () => PulpulakOutfitLogic.bothPlayersHaveNoNPCs(gameState)
        ];
        
        return validators.every(validate => validate());
    }

    /**
     * Проверить, нет ли NPC рядом с персонажем
     */
    static hasNoNPCs(gameState, character) {
        const npcsPresent = gameState.stats[character].npcsPresent || [];
        return npcsPresent.length === 0;
    }

    /**
     * Проверить, позволяет ли локация сменить одежду
     */
    static locationAllowsOutfitChange(gameState, character) {
        const location = gameState.stats[character].location;
        if (!gameState.gameConfig) return false;
        
        const locationData = gameState.gameConfig.getLocationData();
        return locationData.canChangeOutfit(location);
    }

    /**
     * Проверить, находятся ли игроки в одной локации
     */
    static playersInSameLocation(gameState) {
        const princessLocation = gameState.stats.princess.location;
        const helperLocation = gameState.stats.helper.location;
        return princessLocation === helperLocation;
    }

    /**
     * Проверить, нет ли NPC рядом с обоими игроками
     */
    static bothPlayersHaveNoNPCs(gameState) {
        const princessNPCs = gameState.stats.princess.npcsPresent || [];
        const helperNPCs = gameState.stats.helper.npcsPresent || [];
        return princessNPCs.length === 0 && helperNPCs.length === 0;
    }

    /**
     * Создать выбор для обмена одеждой
     */
    static createOutfitSwapChoice(character) {
        const otherCharacter = character === 'princess' ? 'помощнице' : 'княжне';
        return {
            id: 'request_outfit_swap',
            text: '👗 Предложить поменяться одеждой',
            description: `Предложить ${otherCharacter} поменяться нарядами`,
            isOutfitRequest: true
        };
    }

    /**
     * Выполнить обмен одеждой
     */
    static executeOutfitSwap(gameState) {
        const princessOutfit = gameState.stats.princess.outfit;
        const helperOutfit = gameState.stats.helper.outfit;
        
        // Меняем наряды местами
        gameState.stats.princess.outfit = helperOutfit;
        gameState.stats.helper.outfit = princessOutfit;
        
        return gameState;
    }

    /**
     * Получить сообщение об ошибке при невозможности переодеться
     */
    static getOutfitChangeErrorMessage(gameState, character) {
        if (!PulpulakOutfitLogic.hasNoNPCs(gameState, character)) {
            return "Нельзя переодеваться при посторонних!";
        }
        
        if (!PulpulakOutfitLogic.locationAllowsOutfitChange(gameState, character)) {
            return "В этом месте нельзя переодеваться!";
        }
        
        if (!PulpulakOutfitLogic.playersInSameLocation(gameState)) {
            return "Персонажи должны быть в одной локации для обмена одеждой!";
        }
        
        if (!PulpulakOutfitLogic.bothPlayersHaveNoNPCs(gameState)) {
            return "Нельзя переодеваться при посторонних!";
        }
        
        return "Переодевание невозможно по неизвестной причине";
    }

    /**
     * Проверить, может ли персонаж запросить обмен одеждой
     */
    static canRequestOutfitSwap(gameState, character, hasActiveRequest = false) {
        if (hasActiveRequest) {
            return false;
        }
        
        return PulpulakOutfitLogic.canSwitchOutfits(gameState, character);
    }
}

module.exports = PulpulakOutfitLogic;