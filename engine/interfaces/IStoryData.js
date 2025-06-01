/**
 * Interface for story data access
 * Provides access to scenes, narrative content, and story progression
 */
class IStoryData {
    constructor() {
        if (this.constructor === IStoryData) {
            throw new Error('IStoryData is an interface and cannot be instantiated directly');
        }
    }

    /**
     * Gets a scene by its ID
     * @param {string} sceneId - Scene identifier
     * @returns {Object} Scene object with title, text, choices, etc.
     */
    getScene(sceneId) { 
        throw new Error('getScene() must be implemented'); 
    }
    
    /**
     * Gets all available scenes
     * @returns {Object} Map of scene ID to scene object
     */
    getAllScenes() { 
        throw new Error('getAllScenes() must be implemented'); 
    }
    
    /**
     * Gets the initial scene for starting a new game
     * @returns {string} Initial scene ID
     */
    getInitialScene() { 
        throw new Error('getInitialScene() must be implemented'); 
    }
    
    /**
     * Gets scene content for a specific character perspective
     * @param {string} sceneId - Scene identifier
     * @param {string} character - Character viewing the scene
     * @param {Object} gameState - Current game state for dynamic content
     * @returns {Object} Scene content tailored for the character
     */
    getSceneForCharacter(sceneId, character, gameState = null) {
        // Default implementation uses getScene, can be overridden for character-specific content
        return this.getScene(sceneId);
    }
    
    /**
     * Gets available choices for a scene and character
     * @param {string} sceneId - Scene identifier
     * @param {string} character - Character making the choice
     * @param {Object} gameState - Current game state
     * @returns {Object} Choices object for the character
     */
    getChoicesForCharacter(sceneId, character, gameState = null) {
        const scene = this.getScene(sceneId);
        return scene && scene.choices ? scene.choices[character] || [] : [];
    }
    
    /**
     * Checks if a scene exists
     * @param {string} sceneId - Scene identifier
     * @returns {boolean} True if scene exists
     */
    hasScene(sceneId) {
        return !!this.getScene(sceneId);
    }
}

module.exports = IStoryData;