/**
 * Main game configuration interface that defines all methods the engine needs
 * to work with any game implementation
 */
class IGameConfig {
    constructor() {
        if (this.constructor === IGameConfig) {
            throw new Error('IGameConfig is an interface and cannot be instantiated directly');
        }
    }

    // ========================== Data Access Methods ==========================
    
    /**
     * Gets story data interface
     * @returns {IStoryData} Story data provider
     */
    getStoryData() { 
        throw new Error('getStoryData() must be implemented'); 
    }
    
    /**
     * Gets location data interface
     * @returns {ILocationData} Location data provider
     */
    getLocationData() { 
        throw new Error('getLocationData() must be implemented'); 
    }
    
    /**
     * Gets NPC data interface
     * @returns {INPCData} NPC data provider
     */
    getNPCData() { 
        throw new Error('getNPCData() must be implemented'); 
    }
    
    /**
     * Gets quest data interface
     * @returns {IQuestData} Quest data provider
     */
    getQuestData() { 
        throw new Error('getQuestData() must be implemented'); 
    }

    // ========================== Character Configuration ==========================
    
    /**
     * Gets list of playable characters
     * @returns {string[]} Array of character IDs
     */
    getCharacters() { 
        throw new Error('getCharacters() must be implemented'); 
    }
    
    /**
     * Gets character display names
     * @returns {Object} Map of character ID to display name
     */
    getCharacterNames() { 
        throw new Error('getCharacterNames() must be implemented'); 
    }
    
    /**
     * Gets character roles/descriptions
     * @returns {Object} Map of character ID to role description
     */
    getCharacterRoles() { 
        throw new Error('getCharacterRoles() must be implemented'); 
    }
    
    /**
     * Gets initial location for a character
     * @param {string} character - Character ID
     * @returns {string} Initial location ID
     */
    getInitialLocation(character) { 
        throw new Error('getInitialLocation() must be implemented'); 
    }
    
    /**
     * Gets initial outfit for a character
     * @param {string} character - Character ID
     * @returns {string} Initial outfit ID
     */
    getInitialOutfit(character) { 
        throw new Error('getInitialOutfit() must be implemented'); 
    }
    
    /**
     * Gets available outfits for a character
     * @param {string} character - Character ID
     * @returns {string[]} Array of outfit IDs
     */
    getAvailableOutfits(character) { 
        throw new Error('getAvailableOutfits() must be implemented'); 
    }

    // ========================== Game Logic Methods ==========================
    
    /**
     * Determines if a character can switch outfits in current state
     * @param {Object} gameState - Current game state
     * @param {string} character - Character ID
     * @returns {boolean} True if outfit switching is allowed
     */
    canSwitchOutfits(gameState, character) { 
        throw new Error('canSwitchOutfits() must be implemented'); 
    }
    
    /**
     * Gets dynamic choices available to a character
     * @param {Object} gameState - Current game state
     * @param {string} character - Character ID
     * @returns {Object[]} Array of choice objects
     */
    getDynamicChoices(gameState, character) { 
        throw new Error('getDynamicChoices() must be implemented'); 
    }
    
    /**
     * Creates an outfit swap choice for a character
     * @param {string} character - Character ID
     * @returns {Object} Outfit swap choice object
     */
    createOutfitSwapChoice(character) { 
        throw new Error('createOutfitSwapChoice() must be implemented'); 
    }

    // ========================== Request Handlers ==========================
    
    /**
     * Gets request handlers for game-specific requests
     * @returns {Object} Request handlers object with registerHandlers method
     */
    getRequestHandlers() { 
        throw new Error('getRequestHandlers() must be implemented'); 
    }

    // ========================== Validation and Rules ==========================
    
    /**
     * Validates game state against game rules
     * @param {Object} gameState - Game state to validate
     * @returns {Object} Validation result with valid flag and errors array
     */
    validateGameRules(gameState) { 
        throw new Error('validateGameRules() must be implemented'); 
    }
    
    /**
     * Gets game constants and configuration values
     * @returns {Object} Game constants object
     */
    getGameConstants() { 
        throw new Error('getGameConstants() must be implemented'); 
    }

    // ========================== Game Metadata ==========================
    
    /**
     * Gets game metadata
     * @returns {Object} Metadata with id, name, version, description, etc.
     */
    getGameMetadata() { 
        throw new Error('getGameMetadata() must be implemented'); 
    }
}

module.exports = IGameConfig;