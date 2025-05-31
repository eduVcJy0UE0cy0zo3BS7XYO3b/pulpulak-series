/**
 * NPC Data loaded from S-expression files
 */

const path = require('path');
const DataLoader = require('./dataLoader');

class NPCDataSCM {
    constructor() {
        this.loader = new DataLoader();
        this.npcs = null;
        this.baseNPCLocations = null;
        this.loadData();
    }

    loadData() {
        const npcsFile = path.join(__dirname, 'npcs.scm');
        this.npcs = this.loader.loadSExpFile(npcsFile);
        
        // Build base NPC locations map
        this.baseNPCLocations = {};
        for (const npcId in this.npcs) {
            const npc = this.npcs[npcId];
            if (npc.baseLocation) {
                if (!this.baseNPCLocations[npc.baseLocation]) {
                    this.baseNPCLocations[npc.baseLocation] = [];
                }
                this.baseNPCLocations[npc.baseLocation].push(npcId);
            }
        }
    }

    static getNPC(npcId) {
        if (!this.instance) {
            this.instance = new NPCDataSCM();
        }
        return this.instance.npcs[npcId];
    }
    
    static getNPCsForLocation(location, gameState = null, character = null) {
        if (!this.instance) {
            this.instance = new NPCDataSCM();
        }
        
        // Start with base NPCs for location
        const baseNpcIds = this.instance.baseNPCLocations[location] || [];
        let finalNpcIds = [...baseNpcIds];
        
        // If we have game state, check for dynamic movement
        if (gameState && character) {
            const questState = gameState.quests ? gameState.quests[character] : null;
            const characterMemory = gameState.npcMemory ? gameState.npcMemory[character] || {} : {};
            
            // Check each NPC that might move
            Object.keys(this.instance.npcs).forEach(npcId => {
                const npc = this.instance.npcs[npcId];
                if (!npc.movementRules) return;
                
                const npcMemory = characterMemory[npcId] || {};
                let currentNpcLocation = npc.baseLocation;
                
                // Check movement conditions
                for (const rule of npc.movementRules) {
                    if (rule.condition && rule.condition(questState, npcMemory)) {
                        currentNpcLocation = rule.location;
                    }
                }
                
                // Add or remove NPC based on current location
                if (currentNpcLocation === location && !finalNpcIds.includes(npcId)) {
                    finalNpcIds.push(npcId);
                }
                
                if (currentNpcLocation !== location && finalNpcIds.includes(npcId)) {
                    finalNpcIds = finalNpcIds.filter(id => id !== npcId);
                }
            });
        }
        
        return finalNpcIds.map(id => this.instance.npcs[id]).filter(npc => npc);
    }
    
    static getNPCDialogue(npcId, playerOutfit, npcMemory = {}, currentLocation = null, questState = null, globalQuestMemory = null, gameState = null, character = null) {
        if (!this.instance) {
            this.instance = new NPCDataSCM();
        }
        
        const npc = this.instance.npcs[npcId];
        if (!npc || !npc.dialogue) return null;
        
        // Determine outfit type
        const isNobleOutfit = playerOutfit === 'princess_dress' || playerOutfit === 'court_dress';
        const outfitType = isNobleOutfit ? 'noble' : 'common';
        
        const dialogueTree = npc.dialogue[outfitType];
        if (!dialogueTree) return null;
        
        // Determine if this is first meeting or return
        const hasMetBefore = npcMemory[outfitType] && Object.keys(npcMemory[outfitType]).length > 0;
        
        let currentDialogue;
        
        // Special location-based dialogue
        if (npcId === 'librarian' && currentLocation === 'secret_archive' && dialogueTree.archive) {
            currentDialogue = dialogueTree.archive;
        } else if (npcId === 'herbalist' && currentLocation === 'greenhouse' && dialogueTree.greenhouse) {
            currentDialogue = dialogueTree.greenhouse;
        } else if (hasMetBefore && dialogueTree.return) {
            // Check return dialogue requirements
            if (dialogueTree.return.requires) {
                const requirementMet = npcMemory[outfitType] && npcMemory[outfitType][dialogueTree.return.requires];
                currentDialogue = requirementMet ? dialogueTree.return : dialogueTree.initial;
            } else {
                currentDialogue = dialogueTree.return;
            }
        } else {
            currentDialogue = dialogueTree.initial;
        }
        
        // Filter choices based on requirements
        const availableChoices = currentDialogue.choices.filter(choice => {
            // Check global quest memory
            if (choice.quest_action && globalQuestMemory) {
                if (choice.quest_action === 'start_noble_quest' && globalQuestMemory.princess_lost_relic) return false;
                if (choice.quest_action === 'start_common_quest' && globalQuestMemory.helper_secret_potion) return false;
            }
            
            // Check negative requirements
            if (choice.requires_not) {
                const hasNegativeCondition = npcMemory[outfitType] && npcMemory[outfitType][choice.requires_not];
                if (hasNegativeCondition) return false;
            }
            
            // Check positive requirements
            if (choice.requires) {
                // Special quest-related checks
                if (choice.requires === 'has_archive_info' && questState) {
                    const quest = questState.active;
                    if (quest && quest.id === 'princess_lost_relic') {
                        const archiveStep = quest.steps.find(step => step.id === 'talk_to_librarian');
                        return archiveStep && archiveStep.completed;
                    }
                    return false;
                }
                
                if (choice.requires === 'has_herb_info' && questState) {
                    const quest = questState.active;
                    if (quest && quest.id === 'helper_secret_potion') {
                        const herbStep = quest.steps.find(step => step.id === 'find_herbalist');
                        return herbStep && herbStep.completed;
                    }
                    return false;
                }
                
                if (choice.requires === 'has_herb_quest' && questState) {
                    const quest = questState.active;
                    return quest && quest.id === 'helper_secret_potion';
                }
                
                // Standard memory check
                return npcMemory[outfitType] && npcMemory[outfitType][choice.requires];
            }
            
            return true;
        });
        
        // Add dynamic dialogue options from quest system
        if (gameState && character && gameState.dynamicDialogues && 
            gameState.dynamicDialogues[character] && 
            gameState.dynamicDialogues[character][npcId]) {
            
            const dynamicChoices = gameState.dynamicDialogues[character][npcId];
            
            for (const dynamicChoice of dynamicChoices) {
                // Check if dynamic choice requirements are met
                let requirementMet = true;
                if (dynamicChoice.requires) {
                    // Check NPC memory or quest memory
                    requirementMet = (npcMemory[outfitType] && npcMemory[outfitType][dynamicChoice.requires]) ||
                                   (gameState.questMemory && gameState.questMemory[character] && gameState.questMemory[character][dynamicChoice.requires]);
                }
                
                // Check if this dynamic choice is already in available choices (prevent duplicates)
                const alreadyExists = availableChoices.some(choice => choice.id === dynamicChoice.id);
                
                if (requirementMet && !alreadyExists) {
                    availableChoices.push({
                        id: dynamicChoice.id,
                        text: dynamicChoice.text,
                        response: dynamicChoice.response,
                        quest_action: dynamicChoice.quest_action
                    });
                }
            }
        }
        
        return {
            greeting: currentDialogue.greeting,
            choices: availableChoices
        };
    }
    
    static processDialogueChoice(npcId, choiceId, playerOutfit, npcMemory = {}, isFollowUp = false, currentChoices = [], currentLocation = null, gameState = null, character = null) {
        if (!this.instance) {
            this.instance = new NPCDataSCM();
        }
        
        const npc = this.instance.npcs[npcId];
        if (!npc || !npc.dialogue) return null;
        
        const isNobleOutfit = playerOutfit === 'princess_dress' || playerOutfit === 'court_dress';
        const outfitType = isNobleOutfit ? 'noble' : 'common';
        
        let choice;
        
        if (isFollowUp && currentChoices.length > 0) {
            choice = currentChoices.find(c => c.id === choiceId);
        } else {
            const dialogueTree = npc.dialogue[outfitType];
            if (!dialogueTree) return null;
            
            // Same logic as getNPCDialogue for finding current dialogue
            let currentDialogue;
            const hasMetBefore = npcMemory[outfitType] && Object.keys(npcMemory[outfitType]).length > 0;
            
            if (npcId === 'librarian' && currentLocation === 'secret_archive' && dialogueTree.archive) {
                currentDialogue = dialogueTree.archive;
            } else if (npcId === 'herbalist' && currentLocation === 'greenhouse' && dialogueTree.greenhouse) {
                currentDialogue = dialogueTree.greenhouse;
            } else if (hasMetBefore && dialogueTree.return) {
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
        
        // If choice not found in static dialogue, check dynamic dialogues
        if (!choice && gameState && character && gameState.dynamicDialogues && 
            gameState.dynamicDialogues[character] && 
            gameState.dynamicDialogues[character][npcId]) {
            
            const dynamicChoices = gameState.dynamicDialogues[character][npcId];
            choice = dynamicChoices.find(c => c.id === choiceId);
            
            // If found in dynamic choices, return it with appropriate structure
            if (choice) {
                return {
                    response: choice.response,
                    effects: null,
                    next_choices: [],
                    originalChoice: choice,
                    quest_action: choice.quest_action,
                    updatedMemory: npcMemory
                };
            }
        }
        
        if (!choice) return null;
        
        // Initialize memory for this outfit type
        if (!npcMemory[outfitType]) {
            npcMemory[outfitType] = {};
        }
        
        // Save memory from this choice
        if (choice.unlocks) {
            npcMemory[outfitType][choice.unlocks] = true;
        }
        
        return {
            response: choice.response,
            effects: choice.effects,
            next_choices: choice.next_choices || [],
            originalChoice: choice,
            quest_action: choice.quest_action,
            updatedMemory: npcMemory
        };
    }
    
    static getNPCAttitude(npcId, playerOutfit) {
        if (!this.instance) {
            this.instance = new NPCDataSCM();
        }
        
        const npc = this.instance.npcs[npcId];
        if (!npc) return 'neutral';
        
        const isNobleOutfit = playerOutfit === 'princess_dress' || playerOutfit === 'court_dress';
        
        if ((npc.likesNoble && isNobleOutfit) || (!npc.likesNoble && !isNobleOutfit)) {
            return 'friendly';
        } else {
            return 'hostile';
        }
    }
}

module.exports = NPCDataSCM;