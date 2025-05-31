const { OUTFIT_NAMES, CHARACTER_NAMES } = require('../constants');
const LocationData = require('../data/locationDataSCM');

class OutfitSystem {
    constructor() {
        this.outfitRequests = new Map(); // roomId -> activeRequest
    }

    createOutfitSwapRequest(roomId, fromPlayerId, fromCharacter, gameState) {
        if (!gameState) {
            return { success: false, message: "Игра не найдена" };
        }

        if (!this.canSwitchOutfits(gameState, fromCharacter)) {
            return { 
                success: false, 
                message: "Нельзя переодеваться при посторонних!" 
            };
        }

        if (this.outfitRequests.has(roomId)) {
            return { 
                success: false, 
                message: "Уже есть активный запрос на обмен одеждой" 
            };
        }

        const targetCharacter = fromCharacter === 'princess' ? 'helper' : 'princess';
        const targetPlayer = gameState.players[targetCharacter];

        if (!targetPlayer) {
            return { success: false, message: "Второй игрок не найден" };
        }

        const request = {
            id: this.generateRequestId(),
            roomId: roomId,
            fromPlayerId: fromPlayerId,
            fromCharacter: fromCharacter,
            targetPlayerId: targetPlayer.id,
            targetCharacter: targetCharacter,
            timestamp: Date.now()
        };

        this.outfitRequests.set(roomId, request);

        return { 
            success: true, 
            request: request,
            message: `${this.getCharacterName(fromCharacter)} предлагает поменяться одеждой`
        };
    }

    respondToOutfitSwapRequest(roomId, playerId, accepted, gameState) {
        const request = this.outfitRequests.get(roomId);
        if (!request) {
            return { success: false, message: "Запрос не найден" };
        }

        if (request.targetPlayerId !== playerId) {
            return { success: false, message: "Этот запрос не для вас" };
        }

        this.outfitRequests.delete(roomId);

        if (!accepted) {
            return { 
                success: true, 
                declined: true,
                message: `${this.getCharacterName(request.targetCharacter)} отклонила предложение обмена одеждой`
            };
        }

        if (!gameState) {
            return { success: false, message: "Игра не найдена" };
        }

        if (!this.canSwitchOutfits(gameState, request.fromCharacter)) {
            return { 
                success: false, 
                message: "Обстановка изменилась - больше нельзя переодеваться!" 
            };
        }

        const { princess, helper } = gameState.stats;
        const tempOutfit = princess.outfit;
        princess.outfit = helper.outfit;
        helper.outfit = tempOutfit;

        return {
            success: true,
            accepted: true,
            message: `Персонажи поменялись одеждой! Княжна теперь в: ${this.getOutfitName(princess.outfit)}, помощница в: ${this.getOutfitName(helper.outfit)}`
        };
    }

    canSwitchOutfits(gameState, character) {
        const validators = [
            () => this.hasNoNPCs(gameState, character),
            () => this.locationAllowsOutfitChange(gameState, character),
            () => this.playersInSameLocation(gameState, character),
            () => this.bothPlayersHaveNoNPCs(gameState, character)
        ];
        
        return validators.every(validate => validate());
    }

    hasNoNPCs(gameState, character) {
        const characterStats = gameState.stats[character];
        return !characterStats.npcsPresent || characterStats.npcsPresent.length === 0;
    }

    locationAllowsOutfitChange(gameState, character) {
        const characterStats = gameState.stats[character];
        return LocationData.canChangeOutfit(characterStats.location);
    }

    playersInSameLocation(gameState, character) {
        const characterStats = gameState.stats[character];
        const otherCharacter = character === 'princess' ? 'helper' : 'princess';
        return characterStats.location === gameState.stats[otherCharacter].location;
    }

    bothPlayersHaveNoNPCs(gameState, character) {
        const otherCharacter = character === 'princess' ? 'helper' : 'princess';
        return this.hasNoNPCs(gameState, otherCharacter);
    }

    getActiveOutfitRequest(roomId) {
        return this.outfitRequests.get(roomId) || null;
    }

    cancelOutfitRequest(roomId) {
        this.outfitRequests.delete(roomId);
    }

    generateRequestId() {
        return Math.random().toString(36).substring(2, 15);
    }

    getCharacterName(character) {
        return CHARACTER_NAMES[character] || character;
    }

    getOutfitName(outfitId) {
        return OUTFIT_NAMES[outfitId] || outfitId;
    }

    getOutfitForCharacter(gameState, character) {
        return gameState.stats[character]?.outfit || 'noble';
    }

    setOutfitForCharacter(gameState, character, outfit) {
        gameState.stats[character].outfit = outfit;
        return this.getOutfitForCharacter(gameState, character);
    }

    isWearing(gameState, character, outfit) {
        return this.getOutfitForCharacter(gameState, character) === outfit;
    }
}

module.exports = OutfitSystem;