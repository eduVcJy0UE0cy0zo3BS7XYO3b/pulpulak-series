const IGameConfig = require('../../engine/interfaces/IGameConfig');
const JsonDataAdapter = require('./adapters/JsonDataAdapter');

// Import constants and logic that remain JS-based
const { OUTFIT_NAMES, CHARACTER_NAMES, CHARACTER_ROLES } = require('./data/constants');
const PulpulakRequestHandlers = require('./requestHandlers');
const PulpulakOutfitLogic = require('./data/outfitLogic');
const questActionHandlers = require('./data/questActionHandlers');

/**
 * Enhanced Pulpulak game configuration using JSON data sources
 * Maintains full compatibility with IGameConfig interface while using JSON loaders
 */
class PulpulakGameConfigJson extends IGameConfig {
    constructor() {
        super();
        this.gameId = 'pulpulak';
        this.gameName = 'Принцесса Пулпулак';
        this.gameVersion = '1.0.0';
        
        // JSON data adapter
        this.jsonAdapter = new JsonDataAdapter();
        this.initialized = false;
    }

    /**
     * Initialize JSON data loaders
     * Must be called before using this config in game logic
     */
    async initialize() {
        if (this.initialized) {
            return;
        }

        await this.jsonAdapter.initialize();
        this.initialized = true;
    }

    /**
     * Check if config is ready for use
     */
    isInitialized() {
        return this.initialized;
    }

    // ========================== Game Metadata ==========================
    
    static getMetadata() {
        return {
            id: 'pulpulak',
            name: 'Княжна Пулпулак',
            description: 'Кооперативная средневековая приключенческая игра о княжне и её верном помощнике',
            minPlayers: 2,
            maxPlayers: 2,
            estimatedDuration: '60-90 минут',
            thumbnail: '/assets/games/pulpulak/thumbnail.jpg',
            roles: [
                { id: 'princess', name: 'Княжна', description: 'Главная героиня приключения' },
                { id: 'helper', name: 'Помощник', description: 'Верный спутник княжны' }
            ],
            features: ['outfit-system', 'loyalty-tracking', 'cooperative-choices', 'json-data'],
            tags: ['cooperative', 'story', 'medieval', 'role-playing']
        };
    }

    getClientData() {
        return {
            metadata: this.constructor.getMetadata(),
            uiConfig: {
                theme: 'medieval',
                primaryColor: '#8B4513',
                secondaryColor: '#D2691E',
                fontFamily: 'serif'
            }
        };
    }

    // ========================== Data Access Methods ==========================
    
    getStoryData() {
        this._requireInitialized();
        return this.jsonAdapter.getStoryData();
    }
    
    getLocationData() {
        this._requireInitialized();
        return this.jsonAdapter.getLocationData();
    }
    
    getNPCData() {
        this._requireInitialized();
        return this.jsonAdapter.getNPCData();
    }
    
    getQuestData() {
        this._requireInitialized();
        return this.jsonAdapter.getQuestData();
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
            description: 'Кооперативная текстовая приключенческая игра о принцессе и её помощнице (JSON версия)',
            minPlayers: 2,
            maxPlayers: 2,
            estimatedPlayTime: '30-60 minutes',
            tags: ['cooperative', 'text-adventure', 'roleplay', 'medieval', 'json-powered']
        };
    }

    // ========================== Additional Game-Specific Methods ==========================
    
    isOutfitSwappingEnabled() {
        return true;
    }
    
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

    // ========================== Request System Methods ==========================
    
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

    // ========================== Backward Compatibility ==========================
    
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

    // ========================== Helper Methods ==========================
    
    _requireInitialized() {
        if (!this.initialized) {
            throw new Error('PulpulakGameConfigJson must be initialized before accessing data methods');
        }
    }

    /**
     * Create a factory method for easy instantiation and initialization
     */
    static async create() {
        const config = new PulpulakGameConfigJson();
        await config.initialize();
        return config;
    }
}

module.exports = PulpulakGameConfigJson;