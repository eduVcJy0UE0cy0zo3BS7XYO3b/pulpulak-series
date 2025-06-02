/**
 * JsonDataAdapter - Compatibility adapter that wraps JSON loaders to work with IGameConfig interface
 * Provides synchronous interface for data access while using JSON loaders internally with caching
 */

const JsonStoryLoader = require('../data-loaders/JsonStoryLoader');
const JsonLocationLoader = require('../data-loaders/JsonLocationLoader');
const JsonNPCLoader = require('../data-loaders/JsonNPCLoader');
const JsonQuestLoader = require('../data-loaders/JsonQuestLoader');

class JsonDataAdapter {
    constructor() {
        this.storyLoader = new JsonStoryLoader();
        this.locationLoader = new JsonLocationLoader();
        this.npcLoader = new JsonNPCLoader();
        this.questLoader = new JsonQuestLoader();
        
        // Cache for synchronous access
        this.cache = {
            storyData: null,
            locationData: null,
            npcData: null,
            questData: null,
            initialized: false
        };
    }

    /**
     * Initialize all data loaders and populate cache
     * Must be called before using synchronous methods
     */
    async initialize() {
        if (this.cache.initialized) {
            return;
        }

        try {
            // Load all data in parallel
            const [storyData, locationData, npcData, questData] = await Promise.all([
                this.storyLoader.getAllScenesData(),
                this.locationLoader._loadData(),
                this.npcLoader._loadData(),
                this.questLoader._loadData()
            ]);

            // Create adapter objects that match original JS module interfaces
            this.cache.storyData = this.createStoryDataAdapter(storyData);
            this.cache.locationData = this.createLocationDataAdapter(locationData);
            this.cache.npcData = this.createNPCDataAdapter(npcData);
            this.cache.questData = this.createQuestDataAdapter(questData);
            
            this.cache.initialized = true;
        } catch (error) {
            throw new Error(`Failed to initialize JSON data adapters: ${error.message}`);
        }
    }

    /**
     * Create story data adapter that matches CoopStoryData interface
     */
    createStoryDataAdapter(storyData) {
        return {
            getScene: (sceneId) => {
                return storyData[sceneId] || null;
            },
            getAllScenes: () => {
                return Object.keys(storyData);
            },
            getSceneIds: () => {
                return Object.keys(storyData);
            }
        };
    }

    /**
     * Create location data adapter that matches LocationData interface
     */
    createLocationDataAdapter(locationData) {
        return {
            getLocation: (locationId) => {
                return locationData[locationId] || null;
            },
            getAllLocations: () => {
                return locationData;
            },
            getLocationIds: () => {
                return Object.keys(locationData);
            },
            getConnections: (locationId) => {
                const location = locationData[locationId];
                return location ? location.connections : [];
            },
            canChangeOutfit: (locationId) => {
                const location = locationData[locationId];
                return location ? location.canChangeOutfit : false;
            },
            getLocationInfo: (locationId) => {
                const location = locationData[locationId];
                if (!location) return null;
                
                return {
                    id: locationId,
                    name: location.name,
                    description: location.description,
                    icon: location.icon,
                    canChangeOutfit: location.canChangeOutfit,
                    connections: location.connections.map(connId => ({
                        id: connId,
                        name: locationData[connId]?.name || connId,
                        icon: locationData[connId]?.icon || '❓'
                    }))
                };
            }
        };
    }

    /**
     * Create NPC data adapter that matches NPCData interface
     */
    createNPCDataAdapter(npcData) {
        // Handle nested structure where NPCs are under 'npcs' key
        const npcs = npcData.npcs || npcData;
        
        // Simple base location mapping extracted from NPC data
        const baseNPCLocations = {
            'princess_chamber': [],
            'throne_room': ['royal_advisor', 'guard_captain'],
            'kitchen': ['cook'],
            'garden': ['herbalist'],
            'secret_garden': ['gardener'],
            'armory': ['weaponsmith'],
            'great_hall': [],
            'corridor_upper': [],
            'corridor_lower': [],
            'stairs_main': [],
            'private_quarters': [],
            'chapel': ['priest'],
            'pantry': [],
            'library': ['librarian'],
            'secret_archive': [],
            'greenhouse': []
        };
        
        return {
            getNPC: (npcId) => {
                return npcs[npcId] || null;
            },
            getAllNPCs: () => {
                return npcs;
            },
            getNPCIds: () => {
                return Object.keys(npcs);
            },
            getNPCsForLocation: (location, gameState = null, character = null) => {
                // Start with base NPC list for location
                const baseNpcIds = baseNPCLocations[location] || [];
                let finalNpcIds = [...baseNpcIds];
                
                // If we have game state, check for dynamic NPC movements
                if (gameState && character && npcData.npcMovementRules) {
                    const questState = gameState.quests[character];
                    const characterMemory = gameState.npcMemory[character] || {};
                    
                    // Check each NPC that can move
                    Object.keys(npcData.npcMovementRules).forEach(npcId => {
                        const rules = npcData.npcMovementRules[npcId];
                        const npcMemory = characterMemory[npcId] || {};
                        
                        // Determine current NPC location for this player
                        let currentNpcLocation = rules.baseLocation;
                        
                        // Check movement conditions (implement the specific logic for each NPC)
                        if (npcId === 'librarian') {
                            // Librarian moves to archive if player consulted them about quest
                            if (npcMemory.noble && npcMemory.noble.librarian_consulted) {
                                currentNpcLocation = 'secret_archive';
                            }
                        } else if (npcId === 'herbalist') {
                            // Herbalist moves to greenhouse if quest started
                            if (npcMemory.common && npcMemory.common.quest_started) {
                                currentNpcLocation = 'greenhouse';
                            }
                        }
                        
                        // If NPC is in this location, add them
                        if (currentNpcLocation === location && !finalNpcIds.includes(npcId)) {
                            finalNpcIds.push(npcId);
                        }
                        
                        // If NPC moved away from this location, remove them
                        if (currentNpcLocation !== location && finalNpcIds.includes(npcId)) {
                            finalNpcIds = finalNpcIds.filter(id => id !== npcId);
                        }
                    });
                }
                
                return finalNpcIds.map(id => npcs[id]).filter(npc => npc);
            },
            getNPCDialogue: (npcId, playerOutfit, npcMemory = {}, currentLocation = null, questState = null, globalQuestMemory = null, gameState = null) => {
                const npc = npcs[npcId];
                if (!npc) return null;
                
                const isNobleOutfit = playerOutfit === 'princess_dress' || playerOutfit === 'court_dress';
                const outfitType = isNobleOutfit ? 'noble' : 'common';
                const dialogueTree = npc.dialogue[outfitType];
                
                if (!dialogueTree) return null;
                
                // Determine current dialogue based on memory and location
                let currentDialogue;
                const hasMetBefore = npcMemory[outfitType] && Object.keys(npcMemory[outfitType]).length > 0;
                
                // Special logic for NPCs in special locations (priority over return dialogue)
                if (npcId === 'librarian' && currentLocation === 'secret_archive' && dialogueTree.archive) {
                    currentDialogue = dialogueTree.archive;
                // Special logic for herbalist in greenhouse
                } else if (npcId === 'herbalist' && currentLocation === 'greenhouse' && dialogueTree.greenhouse) {
                    currentDialogue = dialogueTree.greenhouse;
                } else if (hasMetBefore && dialogueTree.return) {
                    // Check if conditions for "return" dialogue are met
                    if (dialogueTree.return.requires) {
                        const requirementMet = npcMemory[outfitType] && npcMemory[outfitType][dialogueTree.return.requires];
                        currentDialogue = requirementMet ? dialogueTree.return : dialogueTree.initial;
                    } else {
                        currentDialogue = dialogueTree.return;
                    }
                } else {
                    currentDialogue = dialogueTree.initial;
                }
                
                if (!currentDialogue) return null;
                
                // Filter choices based on memory and quest requirements
                let availableChoices = currentDialogue.choices || [];
                
                // Filter based on quest requirements and memory
                availableChoices = availableChoices.filter(choice => {
                    // Check if choice requires something that hasn't been unlocked
                    if (choice.requires_not && globalQuestMemory && globalQuestMemory[choice.requires_not]) {
                        return false; // Already completed, don't show
                    }
                    
                    // Special logic for quest state requirements
                    if (choice.requires === 'has_archive_info' && questState) {
                        // Check if player got info from archive (completed talk_to_librarian step)
                        const quest = questState.active;
                        if (quest && quest.id === 'princess_lost_relic') {
                            const archiveStep = quest.steps.find(step => step.id === 'talk_to_librarian');
                            return archiveStep && archiveStep.completed;
                        }
                        return false;
                    }
                    
                    if (choice.requires === 'has_herb_info') {
                        // Check if herbs were collected for the helper quest
                        // This allows cross-character quest progression where princess (in common outfit) 
                        // collects herbs for helper quest and can report back to cook
                        
                        // Check global memory flag set when herbs are collected
                        if (globalQuestMemory && globalQuestMemory.herbs_collected) {
                            return true;
                        }
                        
                        // For cross-character quest checking, look at the helper quest state
                        // regardless of which character is currently active
                        if (gameState && gameState.quests && gameState.quests.helper) {
                            const helperQuestState = gameState.quests.helper;
                            if (helperQuestState.active && helperQuestState.active.id === 'helper_secret_potion') {
                                const herbCollectionStep = helperQuestState.active.steps.find(step => step.id === 'talk_to_herbalist');
                                if (herbCollectionStep && herbCollectionStep.completed) {
                                    return true;
                                }
                            }
                        }
                        
                        // Fallback: check if the current character has active helper quest with herb step completed
                        if (questState && questState.active && questState.active.id === 'helper_secret_potion') {
                            const herbCollectionStep = questState.active.steps.find(step => step.id === 'talk_to_herbalist');
                            if (herbCollectionStep && herbCollectionStep.completed) {
                                return true;
                            }
                        }
                        
                        return false;
                    }
                    
                    // Check if choice requires memory that we have
                    if (choice.requires && (!npcMemory[outfitType] || !npcMemory[outfitType][choice.requires])) {
                        return false; // Required memory not present
                    }
                    
                    return true;
                });
                
                return {
                    greeting: currentDialogue.greeting,
                    choices: availableChoices
                };
            },
            getNPCAttitude: (npcId, playerOutfit) => {
                const npc = npcs[npcId];
                if (!npc) return 'neutral';
                
                const isNobleOutfit = playerOutfit === 'princess_dress' || playerOutfit === 'court_dress';
                
                if ((npc.likesNoble && isNobleOutfit) || (!npc.likesNoble && !isNobleOutfit)) {
                    return 'friendly';
                } else {
                    return 'suspicious';
                }
            },
            processDialogueChoice: (npcId, choiceId, playerOutfit, npcMemory = {}, isFollowUp = false, currentChoices = [], currentLocation = null) => {
                const npc = npcs[npcId];
                if (!npc) return null;
                
                const isNobleOutfit = playerOutfit === 'princess_dress' || playerOutfit === 'court_dress';
                const outfitType = isNobleOutfit ? 'noble' : 'common';
                
                let choice;
                
                if (isFollowUp && currentChoices.length > 0) {
                    // Find choice in current follow-up choices
                    choice = currentChoices.find(c => c.id === choiceId);
                } else {
                    // Find choice in main dialogue
                    const dialogueTree = npc.dialogue[outfitType];
                    if (!dialogueTree) return null;
                    
                    // Use same logic as getNPCDialogue to determine current dialogue
                    let currentDialogue;
                    const hasMetBefore = npcMemory[outfitType] && Object.keys(npcMemory[outfitType]).length > 0;
                    
                    // Special logic for NPCs in special locations (priority over return dialogue)
                    if (npcId === 'librarian' && currentLocation === 'secret_archive' && dialogueTree.archive) {
                        currentDialogue = dialogueTree.archive;
                    // Special logic for herbalist in greenhouse
                    } else if (npcId === 'herbalist' && currentLocation === 'greenhouse' && dialogueTree.greenhouse) {
                        currentDialogue = dialogueTree.greenhouse;
                    } else if (hasMetBefore && dialogueTree.return) {
                        // Check if conditions for "return" dialogue are met
                        if (dialogueTree.return.requires) {
                            const requirementMet = npcMemory[outfitType] && npcMemory[outfitType][dialogueTree.return.requires];
                            currentDialogue = requirementMet ? dialogueTree.return : dialogueTree.initial;
                        } else {
                            currentDialogue = dialogueTree.return;
                        }
                    } else {
                        currentDialogue = dialogueTree.initial;
                    }
                    
                    choice = currentDialogue.choices.find(c => c.id === choiceId);
                }
                
                if (!choice) return null;
                
                // Initialize memory for this outfit type if it doesn't exist
                if (!npcMemory[outfitType]) {
                    npcMemory[outfitType] = {};
                }
                
                // Save to memory that this choice was made
                if (choice.unlocks) {
                    npcMemory[outfitType][choice.unlocks] = true;
                }
                
                return {
                    response: choice.response,
                    effects: choice.effects,
                    next_choices: choice.next_choices || [],
                    quest_action: choice.quest_action,
                    updatedMemory: npcMemory
                };
            }
        };
    }

    /**
     * Create quest data adapter that matches QuestData interface
     */
    createQuestDataAdapter(questData) {
        return {
            getQuest: (questId) => {
                return questData[questId] || null;
            },
            getAllQuests: () => {
                return questData;
            },
            getQuestIds: () => {
                return Object.keys(questData);
            },
            getQuestForCharacter: (character) => {
                const quests = Object.values(questData);
                return quests.filter(quest => quest.character === character);
            },
            createQuestInstance: (questId) => {
                const questTemplate = questData[questId];
                if (!questTemplate) return null;

                // Create a copy of the quest for game instance
                return JSON.parse(JSON.stringify(questTemplate));
            }
        };
    }

    // Synchronous access methods (require initialization)
    getStoryData() {
        if (!this.cache.initialized) {
            throw new Error('JsonDataAdapter must be initialized before use');
        }
        return this.cache.storyData;
    }

    getLocationData() {
        if (!this.cache.initialized) {
            throw new Error('JsonDataAdapter must be initialized before use');
        }
        return this.cache.locationData;
    }

    getNPCData() {
        if (!this.cache.initialized) {
            throw new Error('JsonDataAdapter must be initialized before use');
        }
        return this.cache.npcData;
    }

    getQuestData() {
        if (!this.cache.initialized) {
            throw new Error('JsonDataAdapter must be initialized before use');
        }
        return this.cache.questData;
    }

    /**
     * Check if adapter is initialized
     */
    isInitialized() {
        return this.cache.initialized;
    }

    /**
     * Clear cache and force re-initialization
     */
    reset() {
        this.cache = {
            storyData: null,
            locationData: null,
            npcData: null,
            questData: null,
            initialized: false
        };
    }
}

module.exports = JsonDataAdapter;