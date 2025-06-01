const IGameConfig = require('../../../engine/interfaces/IGameConfig');

// Import actual game data for testing
const CoopStoryData = require('../../../games/pulpulak/data/coopStoryData');
const LocationData = require('../../../games/pulpulak/data/locationData');
const NPCData = require('../../../games/pulpulak/data/npcData');
const QuestData = require('../../../games/pulpulak/data/questData');
const { OUTFIT_NAMES, CHARACTER_NAMES, CHARACTER_ROLES } = require('../../../games/pulpulak/data/constants');
const PulpulakOutfitLogic = require('../../../games/pulpulak/data/outfitLogic');
const questActionHandlers = require('../../../games/pulpulak/data/questActionHandlers');
const PulpulakRequestHandlers = require('../../../games/pulpulak/requestHandlers');

/**
 * Mock GameConfig for testing - uses actual Pulpulak data
 * This ensures tests continue to work with real game data while using the new architecture
 */
class MockGameConfig extends IGameConfig {
    constructor(options = {}) {
        super();
        this.options = options;
        this.gameId = 'test_pulpulak';
        this.gameName = 'Test Pulpulak Game';
        this.gameVersion = '1.0.0-test';
    }

    // Data access methods - use real Pulpulak data
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

    // Character configuration - use real Pulpulak config
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

    // Game logic methods - use real Pulpulak logic
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

    // Request handlers - use real Pulpulak handlers for tests
    getRequestHandlers() {
        return PulpulakRequestHandlers;
    }
    
    // Quest action handlers - use real Pulpulak handlers for tests
    getQuestActionHandlers() {
        return questActionHandlers;
    }

    // Validation and rules
    validateGameRules(gameState) {
        return { valid: true, errors: [] };
    }
    
    getGameConstants() {
        return {
            OUTFIT_NAMES,
            CHARACTER_NAMES,
            CHARACTER_ROLES
        };
    }

    // Game metadata
    getGameMetadata() {
        return {
            id: this.gameId,
            name: this.gameName,
            version: this.gameVersion,
            description: 'Test version of Pulpulak cooperative adventure',
            minPlayers: 2,
            maxPlayers: 2,
            estimatedPlayTime: '30-60 minutes',
            tags: ['cooperative', 'text-adventure', 'roleplay', 'medieval', 'test']
        };
    }

    // Additional methods for testing backward compatibility
    isOutfitSwappingEnabled() {
        return true;
    }
    
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
}

module.exports = MockGameConfig;