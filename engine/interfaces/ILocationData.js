/**
 * Interface for location data access
 * Provides access to game world locations, connections, and spatial relationships
 */
class ILocationData {
    constructor() {
        if (this.constructor === ILocationData) {
            throw new Error('ILocationData is an interface and cannot be instantiated directly');
        }
    }

    /**
     * Gets a location by its ID
     * @param {string} locationId - Location identifier
     * @returns {Object} Location object with name, description, NPCs, etc.
     */
    getLocation(locationId) { 
        throw new Error('getLocation() must be implemented'); 
    }
    
    /**
     * Gets location information for display
     * @param {string} locationId - Location identifier
     * @returns {Object} Location info with name, description, connections
     */
    getLocationInfo(locationId) { 
        throw new Error('getLocationInfo() must be implemented'); 
    }
    
    /**
     * Gets connections from a location to other locations
     * @param {string} locationId - Location identifier
     * @returns {Object[]} Array of connection objects with target location info
     */
    getConnections(locationId) { 
        throw new Error('getConnections() must be implemented'); 
    }
    
    /**
     * Gets all available locations
     * @returns {Object} Map of location ID to location object
     */
    getAllLocations() { 
        throw new Error('getAllLocations() must be implemented'); 
    }
    
    /**
     * Checks if a location exists
     * @param {string} locationId - Location identifier
     * @returns {boolean} True if location exists
     */
    hasLocation(locationId) {
        return !!this.getLocation(locationId);
    }
    
    /**
     * Checks if movement between locations is allowed
     * @param {string} fromLocation - Source location ID
     * @param {string} toLocation - Target location ID
     * @param {Object} gameState - Current game state
     * @param {string} character - Character attempting to move
     * @returns {boolean} True if movement is allowed
     */
    canMoveBetween(fromLocation, toLocation, gameState = null, character = null) {
        const connections = this.getConnections(fromLocation);
        return connections.some(conn => conn.id === toLocation);
    }
    
    /**
     * Gets NPCs present in a location
     * @param {string} locationId - Location identifier
     * @param {Object} gameState - Current game state for dynamic NPCs
     * @param {string} character - Character perspective
     * @returns {string[]} Array of NPC IDs present in the location
     */
    getNPCsInLocation(locationId, gameState = null, character = null) {
        const location = this.getLocation(locationId);
        return location && location.npcs ? location.npcs : [];
    }
    
    /**
     * Gets location description for a specific character perspective
     * @param {string} locationId - Location identifier
     * @param {string} character - Character viewing the location
     * @param {Object} gameState - Current game state
     * @returns {string} Location description tailored for the character
     */
    getLocationDescription(locationId, character, gameState = null) {
        const location = this.getLocation(locationId);
        return location ? location.description : '';
    }
}

module.exports = ILocationData;