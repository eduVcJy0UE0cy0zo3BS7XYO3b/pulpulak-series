/**
 * Обработчики запросов для игры Pulpulak
 * Содержит специфичную логику для обмена одеждой и других действий
 */

class PulpulakRequestHandlers {
    /**
     * Зарегистрировать все обработчики запросов для игры Pulpulak
     */
    static registerHandlers(requestManager) {
        // Сохраняем ссылку на requestManager для доступа к gameData и playerData
        this.requestManager = requestManager;
        
        // Основной запрос: смена одежды
        requestManager.registerRequestHandler('outfit_swap', {
            validate: this.validateOutfitSwapRequest.bind(this),
            create: this.createOutfitSwapRequest.bind(this),
            respond: this.handleOutfitSwapResponse.bind(this)
        });

        // Пример: запрос на помощь в выполнении задания
        requestManager.registerRequestHandler('quest_help', {
            validate: this.validateQuestHelpRequest.bind(this),
            create: this.createQuestHelpRequest.bind(this),
            respond: this.handleQuestHelpResponse.bind(this)
        });

        // Пример: запрос на обмен предметами
        requestManager.registerRequestHandler('item_trade', {
            validate: this.validateItemTradeRequest.bind(this),
            create: this.createItemTradeRequest.bind(this),
            respond: this.handleItemTradeResponse.bind(this)
        });

        // Пример: запрос на совместное действие
        requestManager.registerRequestHandler('joint_action', {
            validate: this.validateJointActionRequest.bind(this),
            create: this.createJointActionRequest.bind(this),
            respond: this.handleJointActionResponse.bind(this)
        });
    }

    /**
     * OUTFIT SWAP REQUEST - запрос на обмен одеждой (специфично для Pulpulak)
     */
    static validateOutfitSwapRequest(roomId, fromCharacter, requestData) {
        // Проверяем возможность смены одежды
        if (!this.canSwitchOutfits(roomId, fromCharacter)) {
            return { 
                valid: false, 
                message: "Нельзя переодеваться при посторонних!" 
            };
        }

        const gameState = this.requestManager.gameData.getGame(roomId);
        const targetCharacter = fromCharacter === 'princess' ? 'helper' : 'princess';
        const targetPlayer = gameState?.players[targetCharacter];
        
        if (!targetPlayer) {
            return { valid: false, message: "Второй игрок не найден" };
        }

        return { valid: true, targetPlayer, targetCharacter };
    }

    static createOutfitSwapRequest(roomId, fromPlayerId, fromCharacter, requestData) {
        const gameState = this.requestManager.gameData.getGame(roomId);
        const targetCharacter = fromCharacter === 'princess' ? 'helper' : 'princess';
        const targetPlayer = gameState.players[targetCharacter];

        return {
            success: true,
            targetPlayerId: targetPlayer.id,
            targetCharacter: targetCharacter,
            data: {},
            message: `${this.getCharacterName(fromCharacter)} предлагает поменяться одеждой`
        };
    }

    static handleOutfitSwapResponse(roomId, request, accepted, responseData) {
        if (!accepted) {
            return { 
                success: true, 
                declined: true,
                message: `${this.getCharacterName(request.targetCharacter)} отклонила предложение обмена одеждой`
            };
        }

        // Проверяем, что условия всё ещё позволяют обмен
        if (!this.canSwitchOutfits(roomId, request.fromCharacter)) {
            return { 
                success: false, 
                message: "Обстановка изменилась - больше нельзя переодеваться!" 
            };
        }

        // Выполняем обмен
        this.requestManager.playerData.swapOutfits(roomId);

        const princessOutfit = this.getCurrentOutfit(roomId, 'princess');
        const helperOutfit = this.getCurrentOutfit(roomId, 'helper');

        return {
            success: true,
            accepted: true,
            message: `Персонажи поменялись одеждой! Княжна теперь в: ${this.getOutfitName(princessOutfit)}, помощница в: ${this.getOutfitName(helperOutfit)}`
        };
    }

    /**
     * QUEST HELP REQUEST - запрос помощи в квесте
     */
    static validateQuestHelpRequest(roomId, fromCharacter, requestData) {
        const gameState = this.requestManager.gameData.getGame(roomId);
        
        // Проверяем, что у персонажа есть активный квест
        const activeQuest = gameState.quests[fromCharacter].active;
        if (!activeQuest) {
            return { valid: false, message: "У вас нет активного квеста" };
        }

        // Проверяем, что квест действительно требует помощи
        const currentStep = activeQuest.steps[activeQuest.currentStep];
        if (!currentStep || !currentStep.requiresHelp) {
            return { valid: false, message: "Текущий шаг квеста не требует помощи" };
        }

        const targetCharacter = fromCharacter === 'princess' ? 'helper' : 'princess';
        const targetPlayer = gameState?.players[targetCharacter];
        
        if (!targetPlayer) {
            return { valid: false, message: "Второй игрок не найден" };
        }

        return { valid: true, targetPlayer, targetCharacter };
    }

    static createQuestHelpRequest(roomId, fromPlayerId, fromCharacter, requestData) {
        const gameState = this.requestManager.gameData.getGame(roomId);
        const targetCharacter = fromCharacter === 'princess' ? 'helper' : 'princess';
        const targetPlayer = gameState.players[targetCharacter];
        const activeQuest = gameState.quests[fromCharacter].active;

        return {
            success: true,
            targetPlayerId: targetPlayer.id,
            targetCharacter: targetCharacter,
            data: {
                questId: activeQuest.id,
                questTitle: activeQuest.title,
                step: activeQuest.currentStep
            },
            message: `${this.getCharacterName(fromCharacter)} просит помощи с квестом "${activeQuest.title}"`
        };
    }

    static handleQuestHelpResponse(roomId, request, accepted, responseData) {
        if (!accepted) {
            return { 
                success: true, 
                declined: true,
                message: `${this.getCharacterName(request.targetCharacter)} отклонила просьбу о помощи`
            };
        }

        // Логика совместного выполнения квеста
        // Например, помечаем, что второй игрок помогает
        const gameState = this.requestManager.gameData.getGame(roomId);
        // ... логика обновления квеста ...

        return {
            success: true,
            accepted: true,
            message: `${this.getCharacterName(request.targetCharacter)} согласилась помочь с квестом!`
        };
    }

    /**
     * ITEM TRADE REQUEST - запрос на обмен предметами
     */
    static validateItemTradeRequest(roomId, fromCharacter, requestData) {
        const gameState = this.requestManager.gameData.getGame(roomId);
        
        // Проверяем, что у персонажа есть предмет для обмена
        const inventory = gameState.stats[fromCharacter].inventory;
        if (!requestData.offeredItem || !inventory.includes(requestData.offeredItem)) {
            return { valid: false, message: "У вас нет этого предмета" };
        }

        const targetCharacter = fromCharacter === 'princess' ? 'helper' : 'princess';
        const targetPlayer = gameState?.players[targetCharacter];
        
        if (!targetPlayer) {
            return { valid: false, message: "Второй игрок не найден" };
        }

        // Проверяем, что игроки находятся рядом
        if (!this.playersInSameLocation(roomId)) {
            return { valid: false, message: "Вы должны находиться в одном месте для обмена" };
        }

        return { valid: true, targetPlayer, targetCharacter };
    }

    static createItemTradeRequest(roomId, fromPlayerId, fromCharacter, requestData) {
        const gameState = this.requestManager.gameData.getGame(roomId);
        const targetCharacter = fromCharacter === 'princess' ? 'helper' : 'princess';
        const targetPlayer = gameState.players[targetCharacter];

        return {
            success: true,
            targetPlayerId: targetPlayer.id,
            targetCharacter: targetCharacter,
            data: {
                offeredItem: requestData.offeredItem,
                wantedItem: requestData.wantedItem
            },
            message: `${this.getCharacterName(fromCharacter)} предлагает обменять "${requestData.offeredItem}" на "${requestData.wantedItem || 'что-то другое'}"`
        };
    }

    static handleItemTradeResponse(roomId, request, accepted, responseData) {
        if (!accepted) {
            return { 
                success: true, 
                declined: true,
                message: `${this.getCharacterName(request.targetCharacter)} отклонила предложение обмена`
            };
        }

        // Логика обмена предметами
        // ... здесь была бы реализация обмена через PlayerDataManager ...

        return {
            success: true,
            accepted: true,
            message: `Обмен состоялся!`
        };
    }

    /**
     * JOINT ACTION REQUEST - запрос на совместное действие
     */
    static validateJointActionRequest(roomId, fromCharacter, requestData) {
        const gameState = this.requestManager.gameData.getGame(roomId);
        
        // Проверяем, что действие возможно
        if (!requestData.actionType) {
            return { valid: false, message: "Не указан тип действия" };
        }

        // Проверяем, что игроки находятся в подходящем месте
        if (!this.playersInSameLocation(roomId)) {
            return { valid: false, message: "Вы должны находиться вместе для совместного действия" };
        }

        const targetCharacter = fromCharacter === 'princess' ? 'helper' : 'princess';
        const targetPlayer = gameState?.players[targetCharacter];
        
        if (!targetPlayer) {
            return { valid: false, message: "Второй игрок не найден" };
        }

        return { valid: true, targetPlayer, targetCharacter };
    }

    static createJointActionRequest(roomId, fromPlayerId, fromCharacter, requestData) {
        const gameState = this.requestManager.gameData.getGame(roomId);
        const targetCharacter = fromCharacter === 'princess' ? 'helper' : 'princess';
        const targetPlayer = gameState.players[targetCharacter];

        return {
            success: true,
            targetPlayerId: targetPlayer.id,
            targetCharacter: targetCharacter,
            data: {
                actionType: requestData.actionType,
                actionData: requestData.actionData || {}
            },
            message: `${this.getCharacterName(fromCharacter)} предлагает совместное действие: ${requestData.actionType}`
        };
    }

    static handleJointActionResponse(roomId, request, accepted, responseData) {
        if (!accepted) {
            return { 
                success: true, 
                declined: true,
                message: `${this.getCharacterName(request.targetCharacter)} отклонила предложение совместного действия`
            };
        }

        // Логика выполнения совместного действия
        // ... здесь была бы реализация конкретного действия ...

        return {
            success: true,
            accepted: true,
            message: `Совместное действие "${request.data.actionType}" выполнено!`
        };
    }

    /**
     * Вспомогательные методы
     */
    static getCharacterName(character) {
        const names = {
            'princess': 'Княжна',
            'helper': 'Помощница'
        };
        return names[character] || character;
    }

    static playersInSameLocation(roomId) {
        return this.requestManager.playerData.arePlayersInSameLocation(roomId);
    }

    /**
     * ЛОГИКА ПЕРЕОДЕВАНИЯ (специфично для Pulpulak) - делегировано в outfitLogic
     */
    static canSwitchOutfits(roomId, character) {
        const PulpulakOutfitLogic = require('./data/outfitLogic');
        const gameState = this.requestManager.gameData.getGame(roomId);
        return PulpulakOutfitLogic.canSwitchOutfits(gameState, character);
    }

    static hasNoNPCs(roomId, character) {
        return !this.requestManager.playerData.hasNPCsNearby(roomId, character);
    }

    static locationAllowsOutfitChange(roomId, character) {
        const LocationData = require('./data/locationData');
        const playerData = this.requestManager.playerData.getPlayerData(roomId, character);
        if (!playerData) return false;
        
        return LocationData.canChangeOutfit(playerData.location);
    }

    static bothPlayersHaveNoNPCs(roomId) {
        return this.requestManager.playerData.bothPlayersHaveNoNPCs(roomId);
    }

    static getCurrentOutfit(roomId, character) {
        return this.requestManager.playerData.getPlayerData(roomId, character)?.outfit;
    }

    static getOutfitName(outfitId) {
        const { OUTFIT_NAMES } = require('./data/constants');
        return OUTFIT_NAMES[outfitId] || outfitId;
    }
}

module.exports = PulpulakRequestHandlers;