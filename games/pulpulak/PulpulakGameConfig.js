const IGameConfig = require('../../engine/interfaces/IGameConfig');

// Import game data from local data folder
const CoopStoryData = require('./data/coopStoryData');
const LocationData = require('./data/locationData');
const NPCData = require('./data/npcData');
const QuestData = require('./data/questData');
const { OUTFIT_NAMES, CHARACTER_NAMES, CHARACTER_ROLES } = require('./data/constants');
const PulpulakRequestHandlers = require('./requestHandlers');
const PulpulakOutfitLogic = require('./data/outfitLogic');
const questActionHandlers = require('./data/questActionHandlers');

/**
 * Pulpulak game configuration implementing IGameConfig interface
 * Provides complete game data and logic for the Pulpulak cooperative adventure
 */
class PulpulakGameConfig extends IGameConfig {
    constructor() {
        super();
        this.gameId = 'pulpulak';
        this.gameName = 'Принцесса Пулпулак';
        this.gameVersion = '1.0.0';
    }

    // ========================== Data Access Methods ==========================
    
    getStoryData() { 
        return CoopStoryData; 
    }
    
    getLocationData() { 
        return LocationData; 
    }
    
    getNPCData() { 
        return NPCData; 
    }
    
    getQuestData() { 
        return QuestData; 
    }

    // ========================== Character Configuration ==========================
    
    getCharacters() { 
        return ['princess', 'helper']; 
    }
    
    getCharacterNames() { 
        return CHARACTER_NAMES; 
    }
    
    getCharacterRoles() { 
        return CHARACTER_ROLES; 
    }
    
    getInitialLocation(character) { 
        return character === 'princess' ? 'princess_chamber' : 'servant_quarters';
    }
    
    getInitialOutfit(character) { 
        return character === 'princess' ? 'nightgown' : 'common_dress';
    }
    
    getAvailableOutfits(character) { 
        if (character === 'princess') {
            return ['nightgown', 'princess_dress', 'court_dress'];
        } else {
            return ['common_dress', 'servant_outfit'];
        }
    }

    // ========================== Game Logic Methods ==========================
    
    canSwitchOutfits(gameState, character) { 
        return PulpulakOutfitLogic.canSwitchOutfits(gameState, character);
    }
    
    getDynamicChoices(gameState, character) { 
        const choices = [];
        
        // Add outfit swap choice if available
        if (this.canSwitchOutfits(gameState, character)) {
            choices.push(this.createOutfitSwapChoice(character));
        }
        
        return choices;
    }
    
    createOutfitSwapChoice(character) { 
        const otherCharacter = character === 'princess' ? 'помощнице' : 'княжне';
        return {
            id: 'request_outfit_swap',
            text: '👗 Предложить поменяться одеждой',
            description: `Предложить ${otherCharacter} поменяться нарядами`,
            isOutfitRequest: true
        };
    }

    // ========================== Request Handlers ==========================
    
    getRequestHandlers() { 
        return PulpulakRequestHandlers;
    }
    
    /**
     * Gets quest action handlers for processing quest-related dialogue choices
     * @returns {Object} Quest action handlers object
     */
    getQuestActionHandlers() { 
        return questActionHandlers;
    }

    // ========================== Validation and Rules ==========================
    
    validateGameRules(gameState) { 
        const errors = [];
        
        // Validate characters exist and have required properties
        const characters = this.getCharacters();
        characters.forEach(character => {
            if (!gameState.stats || !gameState.stats[character]) {
                errors.push(`Missing stats for character: ${character}`);
            } else {
                const stats = gameState.stats[character];
                if (!stats.location) errors.push(`Missing location for ${character}`);
                if (!stats.outfit) errors.push(`Missing outfit for ${character}`);
            }
        });
        
        // Validate current scene exists
        if (!gameState.currentScene) {
            errors.push('Missing current scene');
        } else if (!this.getStoryData().getScene(gameState.currentScene)) {
            errors.push(`Invalid current scene: ${gameState.currentScene}`);
        }
        
        return { 
            valid: errors.length === 0, 
            errors 
        };
    }
    
    getGameConstants() { 
        return {
            OUTFIT_NAMES,
            CHARACTER_NAMES,
            CHARACTER_ROLES
        };
    }

    // ========================== Game Metadata ==========================
    
    getGameMetadata() { 
        return {
            id: this.gameId,
            name: this.gameName,
            version: this.gameVersion,
            description: 'Кооперативная текстовая приключенческая игра о принцессе и её помощнице',
            minPlayers: 2,
            maxPlayers: 2,
            estimatedPlayTime: '30-60 minutes',
            tags: ['cooperative', 'text-adventure', 'roleplay', 'medieval']
        };
    }

    // ========================== Additional Game-Specific Methods ==========================
    
    /**
     * Checks if outfit swapping feature is enabled
     * @returns {boolean} True if outfit swapping is enabled
     */
    isOutfitSwappingEnabled() {
        return true;
    }
    
    /**
     * Gets all available outfits in the game
     * @returns {Object} Map of outfit ID to outfit info
     */
    getOutfits() {
        return {
            'nightgown': {
                name: OUTFIT_NAMES['nightgown'],
                type: 'sleepwear',
                description: 'Легкая ночная рубашка'
            },
            'princess_dress': {
                name: OUTFIT_NAMES['princess_dress'],
                type: 'noble',
                description: 'Богатое платье знатной особы'
            },
            'common_dress': {
                name: OUTFIT_NAMES['common_dress'],
                type: 'common',
                description: 'Обычная одежда простолюдинки'
            },
            'court_dress': {
                name: OUTFIT_NAMES['court_dress'],
                type: 'formal',
                description: 'Торжественное придворное платье'
            }
        };
    }
    
    /**
     * Universal request system methods for backward compatibility
     */
    
    isRequestChoice(choiceId) {
        return choiceId === 'request_outfit_swap';
    }
    
    getRequestTypeFromChoice(choiceId) {
        if (choiceId === 'request_outfit_swap') {
            return 'outfit_swap';
        }
        return null;
    }
    
    canCreateRequest(gameState, requestType, character, requestData) {
        if (requestType === 'outfit_swap') {
            const canRequest = PulpulakOutfitLogic.canRequestOutfitSwap(gameState, character, false);
            return {
                allowed: canRequest,
                reason: canRequest ? null : 'Cannot swap outfits here'
            };
        }
        
        return { allowed: false, reason: `Unknown request type: ${requestType}` };
    }
    
    executeRequest(gameState, request, responseData) {
        if (request.type === 'outfit_swap') {
            const updatedGameState = PulpulakOutfitLogic.executeOutfitSwap(gameState);
            return {
                success: true,
                gameState: updatedGameState,
                message: 'Outfits swapped successfully'
            };
        }
        
        return {
            success: false,
            message: `Unknown request type: ${request.type}`
        };
    }
    
    // Backward compatibility methods
    executeOutfitSwap(gameState) {
        return PulpulakOutfitLogic.executeOutfitSwap(gameState);
    }

    canRequestOutfitSwap(gameState, character, hasActiveRequest = false) {
        return PulpulakOutfitLogic.canRequestOutfitSwap(gameState, character, hasActiveRequest);
    }
    
    registerRequestHandlers(requestManager) {
        if (PulpulakRequestHandlers && typeof PulpulakRequestHandlers.registerHandlers === 'function') {
            PulpulakRequestHandlers.registerHandlers(requestManager);
        }
    }
}

module.exports = PulpulakGameConfig;