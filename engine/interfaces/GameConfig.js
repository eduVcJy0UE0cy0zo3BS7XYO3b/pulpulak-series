/**
 * Interface for game configuration
 * Defines what data the engine needs to function
 */
class GameConfigInterface {
    constructor() {
        // Game metadata
        this.gameId = '';
        this.gameName = '';
        this.gameVersion = '1.0.0';
        this.maxPlayers = 2;
        
        // Character system
        this.characters = {};
        
        // Story system
        this.scenes = {};
        this.startingScene = '';
        
        // World system
        this.locations = {};
        this.npcs = {};
        this.quests = {};
        
        // Game mechanics
        this.features = {
            turnBasedDialogue: true,
            questSystem: true,
            locationSystem: true,
            npcInteractions: true
        };
        
        // Initial state
        this.initialState = {
            startingLocation: '',
            startingItems: {},
            globalMemory: {}
        };
    }

    /**
     * Validates that the configuration is complete and valid
     */
    validate() {
        const errors = [];
        
        if (!this.gameId) errors.push('gameId is required');
        if (!this.gameName) errors.push('gameName is required');
        if (!this.startingScene) errors.push('startingScene is required');
        if (!this.initialState.startingLocation) errors.push('initialState.startingLocation is required');
        
        if (Object.keys(this.characters).length === 0) {
            errors.push('At least one character must be defined');
        }
        
        if (Object.keys(this.locations).length === 0) {
            errors.push('At least one location must be defined');
        }
        
        if (Object.keys(this.scenes).length === 0) {
            errors.push('At least one scene must be defined');
        }
        
        return {
            valid: errors.length === 0,
            errors
        };
    }

    /**
     * Gets character definition by role
     */
    getCharacter(role) {
        return this.characters[role];
    }


    /**
     * Gets scene definition by ID
     */
    getScene(sceneId) {
        return this.scenes[sceneId];
    }

    /**
     * Gets location definition by ID
     */
    getLocation(locationId) {
        return this.locations[locationId];
    }

    /**
     * Gets NPC definition by ID
     */
    getNPC(npcId) {
        return this.npcs[npcId];
    }

    /**
     * Gets quest definition by ID
     */
    getQuest(questId) {
        return this.quests[questId];
    }

    /**
     * Gets NPCs for a specific location
     */
    getNPCsForLocation(locationId, gameState = null, character = null) {
        // Default implementation - override in specific configs for dynamic NPCs
        const location = this.getLocation(locationId);
        if (!location || !location.npcs) return [];
        
        return location.npcs.map(npcId => this.getNPC(npcId)).filter(npc => npc);
    }


    /**
     * Checks if quest system is enabled
     */
    isQuestSystemEnabled() {
        return this.features.questSystem;
    }

    /**
     * Gets starting state for a new game
     */
    getInitialGameState(roomId, players) {
        return {
            roomId,
            players,
            currentScene: this.startingScene,
            turnOrder: Object.keys(this.characters)[0],
            stats: this.buildInitialCharacterStats(),
            quests: this.buildInitialQuests(),
            npcMemory: this.buildInitialNPCMemory(),
            npcDialogues: this.buildInitialDialogues(),
            globalQuestMemory: { ...this.initialState.globalMemory }
        };
    }

    buildInitialCharacterStats() {
        const stats = {};
        
        Object.keys(this.characters).forEach(character => {
            stats[character] = {
                location: this.initialState.startingLocation,
                inventory: [...(this.initialState.startingItems[character] || [])],
                awareness: 0,
                npcsPresent: []
            };
            
            // Добавляем outfit только если есть система outfits
            if (this.initialState.startingOutfits && this.initialState.startingOutfits[character]) {
                stats[character].outfit = this.initialState.startingOutfits[character];
            }
        });
        
        return stats;
    }

    buildInitialQuests() {
        const quests = {};
        
        Object.keys(this.characters).forEach(character => {
            quests[character] = {
                active: null,
                completed: []
            };
        });
        
        return quests;
    }

    buildInitialNPCMemory() {
        const memory = {};
        
        Object.keys(this.characters).forEach(character => {
            memory[character] = {};
        });
        
        return memory;
    }

    buildInitialDialogues() {
        const dialogues = {};
        
        Object.keys(this.characters).forEach(character => {
            dialogues[character] = null;
        });
        
        return dialogues;
    }
}

module.exports = GameConfigInterface;