/**
 * Interface for NPC data access
 * Provides access to non-player characters, their dialogues, and interactions
 */
class INPCData {
    constructor() {
        if (this.constructor === INPCData) {
            throw new Error('INPCData is an interface and cannot be instantiated directly');
        }
    }

    /**
     * Gets an NPC by its ID
     * @param {string} npcId - NPC identifier
     * @returns {Object} NPC object with name, dialogues, etc.
     */
    getNPC(npcId) { 
        throw new Error('getNPC() must be implemented'); 
    }
    
    /**
     * Gets NPCs for a specific location
     * @param {string} location - Location identifier
     * @param {Object} gameState - Current game state
     * @param {string} character - Character perspective
     * @returns {Object[]} Array of NPC objects present in the location
     */
    getNPCsForLocation(location, gameState = null, character = null) { 
        throw new Error('getNPCsForLocation() must be implemented'); 
    }
    
    /**
     * Gets dialogue for an NPC based on current context
     * @param {string} npcId - NPC identifier
     * @param {string} outfit - Character's current outfit
     * @param {Object} memory - Character's memory with this NPC
     * @param {string} location - Current location
     * @param {Object} questState - Current quest state
     * @param {Object} globalMemory - Global game memory
     * @returns {Object} Dialogue object with text and choices
     */
    getNPCDialogue(npcId, outfit, memory, location, questState, globalMemory) { 
        throw new Error('getNPCDialogue() must be implemented'); 
    }
    
    /**
     * Processes a dialogue choice and returns the response
     * @param {string} npcId - NPC identifier
     * @param {string} choiceId - Selected choice ID
     * @param {string} outfit - Character's current outfit
     * @param {Object} memory - Character's memory with this NPC
     * @param {boolean} isFollowUp - Whether this is a follow-up choice
     * @param {Object} currentChoices - Currently available choices
     * @param {string} location - Current location
     * @returns {Object} Dialogue response with text, effects, and next choices
     */
    processDialogueChoice(npcId, choiceId, outfit, memory, isFollowUp, currentChoices, location) { 
        throw new Error('processDialogueChoice() must be implemented'); 
    }
    
    /**
     * Gets NPC's attitude towards a character based on outfit
     * @param {string} npcId - NPC identifier
     * @param {string} outfit - Character's current outfit
     * @returns {string} Attitude level (e.g., 'friendly', 'neutral', 'hostile')
     */
    getNPCAttitude(npcId, outfit) { 
        throw new Error('getNPCAttitude() must be implemented'); 
    }
    
    /**
     * Checks if an NPC exists
     * @param {string} npcId - NPC identifier
     * @returns {boolean} True if NPC exists
     */
    hasNPC(npcId) {
        return !!this.getNPC(npcId);
    }
    
    /**
     * Gets all available NPCs
     * @returns {Object} Map of NPC ID to NPC object
     */
    getAllNPCs() {
        throw new Error('getAllNPCs() must be implemented');
    }
    
    /**
     * Checks if an NPC is available for interaction
     * @param {string} npcId - NPC identifier
     * @param {Object} gameState - Current game state
     * @param {string} character - Character attempting interaction
     * @returns {boolean} True if NPC can be interacted with
     */
    canInteractWith(npcId, gameState, character) {
        // Default implementation - can be overridden for complex availability logic
        return this.hasNPC(npcId);
    }
    
    /**
     * Gets NPC display name
     * @param {string} npcId - NPC identifier
     * @returns {string} NPC display name
     */
    getNPCName(npcId) {
        const npc = this.getNPC(npcId);
        return npc ? npc.name : npcId;
    }
}

module.exports = INPCData;