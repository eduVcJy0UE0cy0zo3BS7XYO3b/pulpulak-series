/**
 * Interface for quest data access
 * Provides access to quests, quest objectives, and quest progression logic
 */
class IQuestData {
    constructor() {
        if (this.constructor === IQuestData) {
            throw new Error('IQuestData is an interface and cannot be instantiated directly');
        }
    }

    /**
     * Gets a quest by its ID
     * @param {string} questId - Quest identifier
     * @returns {Object} Quest object with title, description, objectives, etc.
     */
    getQuest(questId) { 
        throw new Error('getQuest() must be implemented'); 
    }
    
    /**
     * Creates a new quest instance for a character
     * @param {string} questId - Quest identifier
     * @returns {Object} Quest instance with initial state
     */
    createQuestInstance(questId) { 
        throw new Error('createQuestInstance() must be implemented'); 
    }
    
    /**
     * Gets all available quests
     * @returns {Object} Map of quest ID to quest object
     */
    getAllQuests() { 
        throw new Error('getAllQuests() must be implemented'); 
    }
    
    /**
     * Gets quests available to a specific character
     * @param {string} character - Character identifier
     * @returns {Object[]} Array of quest objects available to the character
     */
    getQuestsForCharacter(character) { 
        throw new Error('getQuestsForCharacter() must be implemented'); 
    }
    
    /**
     * Checks if a quest exists
     * @param {string} questId - Quest identifier
     * @returns {boolean} True if quest exists
     */
    hasQuest(questId) {
        return !!this.getQuest(questId);
    }
    
    /**
     * Gets quest objectives for a specific quest
     * @param {string} questId - Quest identifier
     * @returns {Object[]} Array of quest objective objects
     */
    getQuestObjectives(questId) {
        const quest = this.getQuest(questId);
        return quest && quest.objectives ? quest.objectives : [];
    }
    
    /**
     * Checks if a quest is available to a character in current state
     * @param {string} questId - Quest identifier
     * @param {string} character - Character identifier
     * @param {Object} gameState - Current game state
     * @returns {boolean} True if quest is available
     */
    isQuestAvailable(questId, character, gameState) {
        // Default implementation - can be overridden for complex availability logic
        const quest = this.getQuest(questId);
        return !!quest && (!quest.requiredCharacter || quest.requiredCharacter === character);
    }
    
    /**
     * Checks if quest prerequisites are met
     * @param {string} questId - Quest identifier
     * @param {string} character - Character identifier
     * @param {Object} gameState - Current game state
     * @returns {boolean} True if prerequisites are met
     */
    arePrerequisitesMet(questId, character, gameState) {
        const quest = this.getQuest(questId);
        if (!quest || !quest.prerequisites) return true;
        
        // Default implementation - override for complex prerequisite logic
        return quest.prerequisites.every(prereq => {
            if (prereq.type === 'quest_completed') {
                return gameState.quests[character].completed.includes(prereq.questId);
            }
            return true;
        });
    }
    
    /**
     * Gets quest rewards
     * @param {string} questId - Quest identifier
     * @returns {Object} Quest rewards object
     */
    getQuestRewards(questId) {
        const quest = this.getQuest(questId);
        return quest && quest.rewards ? quest.rewards : {};
    }
    
    /**
     * Checks if a quest is completed
     * @param {string} questId - Quest identifier
     * @param {Object} questInstance - Quest instance state
     * @returns {boolean} True if quest is completed
     */
    isQuestCompleted(questId, questInstance) {
        const quest = this.getQuest(questId);
        if (!quest || !questInstance) return false;
        
        // Default implementation - all objectives must be completed
        if (quest.objectives) {
            return quest.objectives.every(obj => 
                questInstance.completedObjectives && 
                questInstance.completedObjectives.includes(obj.id)
            );
        }
        
        return questInstance.status === 'completed';
    }
    
    /**
     * Gets quest title for display
     * @param {string} questId - Quest identifier
     * @returns {string} Quest title
     */
    getQuestTitle(questId) {
        const quest = this.getQuest(questId);
        return quest ? quest.title : questId;
    }
    
    /**
     * Gets quest description for display
     * @param {string} questId - Quest identifier
     * @returns {string} Quest description
     */
    getQuestDescription(questId) {
        const quest = this.getQuest(questId);
        return quest ? quest.description : '';
    }
}

module.exports = IQuestData;