/**
 * Integration layer for S-expression quest system
 */

const QuestRunner = require('./questRunner');
const path = require('path');
const fs = require('fs');

class QuestIntegration {
    constructor(gameLogic) {
        this.gameLogic = gameLogic;
        // Получаем gameState из первой игры или создаем временный
        let gameState = null;
        if (gameLogic.games && gameLogic.games.size > 0) {
            gameState = gameLogic.games.values().next().value;
        } else {
            // Создаем временный gameState для инициализации
            gameState = {
                memory: {},
                npcMemory: {},
                // Removed loyalty system
                inventory: [],
                startedQuests: new Set(),
                completedQuests: new Set(),
                activeQuests: new Map(),
                revealedLocations: new Set()
            };
        }
        
        this.questRunner = new QuestRunner(gameState);
        
        // Setup event handlers
        this.setupEventHandlers();
        
        // Load quest definitions
        this.loadAllQuests();
    }

    /**
     * Setup event handlers for quest system
     */
    setupEventHandlers() {
        // Handle dialogue additions
        this.questRunner.on('onAddDialogue', (npcId, option) => {
            const gameState = this.getCurrentGameState();
            if (!gameState) return;
            
            // Store dynamic dialogue options for the quest character, not current turn character
            // Check which character has an active quest
            let questCharacter = null;
            for (const [characterId, characterQuests] of Object.entries(gameState.quests)) {
                if (characterQuests.active) {
                    questCharacter = { id: characterId };
                    break;
                }
            }
            
            if (!questCharacter) {
                questCharacter = this.getCurrentCharacter(); // fallback
            }
            
            const character = questCharacter;
            if (!gameState.dynamicDialogues) {
                gameState.dynamicDialogues = {
                    princess: {},
                    helper: {}
                };
            }
            
            if (!gameState.dynamicDialogues[character.id]) {
                gameState.dynamicDialogues[character.id] = {};
            }
            
            if (!gameState.dynamicDialogues[character.id][npcId]) {
                gameState.dynamicDialogues[character.id][npcId] = [];
            }
            
            // Add the dynamic dialogue option
            const questAction = option.effects || option.quest_action || null;
            console.log(`Adding dynamic dialogue: ${option.id}, effects: ${option.effects}, questAction: ${questAction}`);
            gameState.dynamicDialogues[character.id][npcId].push({
                id: option.id,
                text: option.text,
                response: option.response,
                requires: option.condition,
                quest_action: questAction
            });
            
            console.log(`Added dynamic dialogue option for ${character.id} with ${npcId}:`, option);
        });

        // Handle dialogue removal
        this.questRunner.on('onRemoveDialogue', (npcId, optionId) => {
            const gameState = this.getCurrentGameState();
            if (!gameState || !gameState.npcs) return;
            
            const npc = gameState.npcs[npcId];
            if (npc && npc.dialogue && npc.dialogue.choices) {
                npc.dialogue.choices = npc.dialogue.choices.filter(
                    choice => choice.id !== optionId
                );
            }
        });

        // Handle item spawning
        this.questRunner.on('onSpawnItem', (location, itemId) => {
            const gameState = this.getCurrentGameState();
            if (!gameState) return;
            
            if (!gameState.locationItems) {
                gameState.locationItems = {};
            }
            if (!gameState.locationItems[location]) {
                gameState.locationItems[location] = [];
            }
            gameState.locationItems[location].push(itemId);
        });

        // Handle quest start
        this.questRunner.on('onStartQuest', (questId) => {
            const character = this.getCurrentCharacter();
            if (this.questRunner.canStartQuest(questId, character)) {
                this.questRunner.startQuest(questId, character);
            }
        });

        // Handle scene trigger
        this.questRunner.on('onTriggerScene', (sceneId) => {
            if (this.gameLogic.triggerScene) {
                this.gameLogic.triggerScene(sceneId);
            }
        });

        // Handle message display
        this.questRunner.on('onShowMessage', (message) => {
            if (this.gameLogic.showMessage) {
                this.gameLogic.showMessage(message);
            }
        });

        // Handle quest completion
        this.questRunner.on('onQuestComplete', (data) => {
            const character = this.getCurrentCharacter();
            const gameState = this.getCurrentGameState();
            
            if (!gameState) return;
            
            // Ensure quests structure exists
            if (!gameState.quests) {
                gameState.quests = {
                    princess: { active: null, completed: [] },
                    helper: { active: null, completed: [] }
                };
            }
            
            const characterQuests = gameState.quests[character.id];
            
            if (characterQuests) {
                // Move from active to completed
                characterQuests.completed.push(data.questId);
                characterQuests.active = null;
            }

            // Log completion
            console.log(`Quest completed: ${data.quest.title}`);
        });

        // Handle quest progress
        this.questRunner.on('onQuestProgress', (data) => {
            console.log(`Quest progress: ${data.questId} - ${data.event}`);
        });
    }

    /**
     * Handle quest action from dialogue system
     */
    handleQuestAction(questAction, character) {
        console.log(`Handling quest action: ${questAction} for character: ${character.id}`);
        
        // First try to advance existing quest steps
        const stepAdvanced = this.tryAdvanceQuestStep(questAction, character);
        if (stepAdvanced) {
            console.log(`Advanced quest step with action: ${questAction}`);
            return true;
        }
        
        // If no step was advanced, try to find quest that has this quest action as a trigger
        const questId = this.findQuestByAction(questAction, character);
        
        if (!questId) {
            console.log(`No S-expression quest found for action: ${questAction}`);
            return false;
        }
        
        // Check if quest can be started and start it
        const started = this.startQuest(questId, true, character);
        if (started) {
            console.log(`Started S-expression quest: ${questId}`);
            return true;
        } else {
            console.log(`Cannot start quest ${questId} for character ${character.id}`);
            return false;
        }
    }

    /**
     * Try to advance quest step based on quest action
     */
    tryAdvanceQuestStep(questAction, character) {
        const gameState = this.getCurrentGameState();
        if (!gameState || !gameState.quests) return false;
        
        const characterQuests = gameState.quests[character.id];
        if (!characterQuests || !characterQuests.active) return false;
        
        const activeQuestId = characterQuests.active.id;
        const quest = this.questRunner.engine.quests.get(activeQuestId);
        if (!quest) return false;
        
        // Find quest step that matches this action
        const currentStepIndex = characterQuests.active.currentStep || 0;
        const questStep = quest.steps[currentStepIndex];
        
        if (!questStep) return false;
        
        // Check if this step id matches the quest action or if step should handle this action
        if (questStep.id === questAction) {
            // Update quest runner state
            this.updateQuestRunnerState();
            
            // Process the current step
            const result = this.processQuestStep(character.id);
            
            if (result && result.processed) {
                console.log(`Quest step '${questStep.id}' completed for quest '${activeQuestId}'`);
                return true;
            }
        }
        
        return false;
    }

    /**
     * Find quest that can be triggered by the given quest action
     */
    findQuestByAction(questAction, character) {
        for (const [questId, quest] of this.questRunner.engine.quests) {
            // Check if quest is for this character
            if (quest.metadata.character && quest.metadata.character !== character.id) {
                continue;
            }
            
            // Check if quest has quest-action mapping in its variables or metadata
            if (quest.variables && quest.variables.has('quest_action')) {
                const mappedAction = quest.variables.get('quest_action');
                if (mappedAction === questAction) {
                    return questId;
                }
            }
            
            // Check if quest metadata has quest-action
            if (quest.metadata.quest_action === questAction) {
                return questId;
            }
            
            // Check if any trigger references this quest action
            for (const trigger of quest.triggers) {
                if (trigger.quest_action === questAction) {
                    return questId;
                }
            }
        }
        
        return null;
    }

    /**
     * Load all quest definitions
     */
    loadAllQuests() {
        const questDir = path.join(__dirname, 'quests');
        
        // Create quest directory if it doesn't exist
        if (!fs.existsSync(questDir)) {
            fs.mkdirSync(questDir, { recursive: true });
        }

        // Load all .scm files
        const files = fs.readdirSync(questDir).filter(f => f.endsWith('.scm'));
        
        for (const file of files) {
            try {
                const filePath = path.join(questDir, file);
                this.questRunner.loadQuestFromFile(filePath);
                console.log(`Loaded quest: ${file}`);
            } catch (error) {
                console.error(`Failed to load quest ${file}:`, error);
            }
        }
    }

    /**
     * Get current game state
     */
    getCurrentGameState() {
        if (this.gameLogic.games && this.gameLogic.games.size > 0) {
            return this.gameLogic.games.values().next().value;
        }
        return null;
    }

    /**
     * Get current character
     */
    getCurrentCharacter() {
        // Получаем текущий gameState из gameLogic
        const gameState = this.getCurrentGameState();
        if (gameState) {
            const currentTurn = gameState.turnOrder || 'princess';
            
            return {
                id: currentTurn,
                currentOutfit: gameState.stats[currentTurn]?.outfit || 'noble'
            };
        }
        
        // Fallback
        return {
            id: 'princess',
            currentOutfit: 'noble'
        };
    }

    /**
     * Update quest runner game state
     */
    updateQuestRunnerState() {
        const gameState = this.getCurrentGameState();
        if (!gameState) return;
        
        const character = this.getCurrentCharacter();
        
        // Update quest runner's game state with current game data
        this.questRunner.gameState.currentLocation = gameState.stats[character.id]?.location;
        this.questRunner.gameState.currentOutfit = gameState.stats[character.id]?.outfit;
        
        // Merge memory from game state and NPC memory
        this.questRunner.gameState.memory = { ...(gameState.memory || {}) };
        
        // Add NPC memory for current character
        if (gameState.npcMemory && gameState.npcMemory[character.id]) {
            for (const [npcId, npcData] of Object.entries(gameState.npcMemory[character.id])) {
                if (npcData && typeof npcData === 'object') {
                    for (const [outfit, memoryData] of Object.entries(npcData)) {
                        if (memoryData && typeof memoryData === 'object') {
                            // Merge NPC memory into quest runner memory
                            Object.assign(this.questRunner.gameState.memory, memoryData);
                        }
                    }
                }
            }
        }
        
        this.questRunner.gameState.inventory = gameState.stats[character.id]?.inventory || [];
    }

    /**
     * Check if any quest can be started for current context
     */
    checkQuestTriggers() {
        this.updateQuestRunnerState();
        const character = this.getCurrentCharacter();
        const availableQuests = [];

        for (const [questId, quest] of this.questRunner.engine.quests) {
            if (this.questRunner.canStartQuest(questId, character)) {
                availableQuests.push({
                    id: questId,
                    title: quest.metadata.title,
                    description: quest.metadata.description
                });
            }
        }

        return availableQuests;
    }

    /**
     * Handle NPC interaction
     */
    handleNPCInteraction(npcId) {
        this.updateQuestRunnerState();
        this.questRunner.gameState.currentNPC = npcId;
        
        const character = this.getCurrentCharacter();
        const results = this.questRunner.handleNPCDialogue(npcId, character);
        
        // Process results
        const updates = {
            newQuests: [],
            questProgress: [],
            dialogueOptions: []
        };

        for (const result of results) {
            switch (result.type) {
                case 'can_start_quest':
                    updates.newQuests.push({
                        questId: result.questId,
                        title: result.questTitle,
                        description: result.questDescription,
                        startDialogue: `Начать квест: ${result.questTitle}`
                    });
                    break;
                
                case 'quest_progress':
                    updates.questProgress.push({
                        questId: result.questId,
                        stepCompleted: result.stepCompleted,
                        description: result.description,
                        completed: result.completed
                    });
                    break;
            }
        }

        return updates;
    }

    /**
     * Handle dialogue choice made by player to advance quest steps
     */
    handleDialogueChoice(npcId, choiceId, character) {
        this.updateQuestRunnerState();
        
        // Set current NPC context for quest evaluation
        this.questRunner.gameState.currentNPC = npcId;
        
        // Check if player has active quest and try to advance it
        const gameState = this.getCurrentGameState();
        if (!gameState || !gameState.quests) return false;
        
        const characterQuests = gameState.quests[character.id || character];
        if (!characterQuests || !characterQuests.active) return false;
        
        const activeQuestId = characterQuests.active.id;
        console.log(`Checking quest progress for ${activeQuestId} after dialogue choice ${choiceId} with ${npcId}`);
        
        // Try to process current quest step
        const result = this.processQuestStep(character.id || character);
        if (result && result.processed) {
            console.log(`Quest step advanced for ${activeQuestId}`);
            return true;
        }
        
        return false;
    }

    /**
     * Handle location change
     */
    handleLocationChange(location) {
        const character = this.getCurrentCharacter();
        const results = this.questRunner.handleLocationChange(location, character);
        
        return results.map(result => ({
            type: result.type,
            questId: result.questId,
            title: result.questTitle
        }));
    }

    /**
     * Start a specific quest
     */
    startQuest(questId, force = false, specificCharacter = null) {
        this.updateQuestRunnerState();
        const character = specificCharacter || this.getCurrentCharacter();
        
        // Check if quest exists and not already started/completed
        const quest = this.questRunner.engine.quests.get(questId);
        if (!quest) {
            return false;
        }
        
        if (this.questRunner.gameState.startedQuests.has(questId) || 
            this.questRunner.gameState.completedQuests.has(questId)) {
            return false;
        }
        
        // For forced start (from dialogue action), skip trigger checks
        const canStart = force || this.questRunner.canStartQuest(questId, character);
        
        if (canStart) {
            const instance = this.questRunner.startQuest(questId, character);
            
            // Get current game state
            const gameState = this.getCurrentGameState();
            
            // Ensure quests structure exists
            if (!gameState.quests) {
                gameState.quests = {
                    princess: { active: null, completed: [] },
                    helper: { active: null, completed: [] }
                };
            }
            
            // Update game state
            const characterQuests = gameState.quests[character.id];
            if (characterQuests) {
                const quest = this.questRunner.engine.quests.get(questId);
                characterQuests.active = {
                    id: questId,
                    title: quest.metadata.title,
                    description: quest.metadata.description,
                    currentStep: 0,
                    steps: quest.steps.map((step, index) => ({
                        id: step.id || `step_${index}`,
                        description: step.description || 'Шаг квеста',
                        completed: false
                    }))
                };
            }
            
            return true;
        }
        
        return false;
    }

    /**
     * Get current quest status
     */
    getCurrentQuestStatus(specificCharacter = null) {
        const character = specificCharacter || this.getCurrentCharacter();
        const gameState = this.getCurrentGameState();
        
        if (!gameState) return null;
        
        // Ensure quests structure exists
        if (!gameState.quests) {
            gameState.quests = {
                princess: { active: null, completed: [] },
                helper: { active: null, completed: [] }
            };
        }
        
        const characterQuests = gameState.quests[character.id];
        
        if (!characterQuests || !characterQuests.active) {
            return null;
        }

        const questId = characterQuests.active.id;
        return this.questRunner.getQuestStatus(questId);
    }

    /**
     * Process current quest step
     */
    processQuestStep(specificCharacter = null) {
        const character = specificCharacter || this.getCurrentCharacter();
        const gameState = this.getCurrentGameState();
        
        if (!gameState) return null;
        
        // Ensure quests structure exists
        if (!gameState.quests) {
            gameState.quests = {
                princess: { active: null, completed: [] },
                helper: { active: null, completed: [] }
            };
        }
        
        const characterQuests = gameState.quests[character.id];
        
        if (!characterQuests || !characterQuests.active) {
            return null;
        }

        const questId = characterQuests.active.id;
        const result = this.questRunner.processCurrentStep(questId, character);
        
        // Update game state quest progress using the correct character
        if (result.processed && result.stepCompleted) {
            const questStatus = this.questRunner.getQuestStatus(questId);
            if (questStatus && questStatus.currentStep) {
                // Update the quest state for the specified character (not getCurrentCharacter)
                const targetCharacterQuests = gameState.quests[character.id];
                if (targetCharacterQuests && targetCharacterQuests.active) {
                    targetCharacterQuests.active.currentStep = questStatus.currentStep.index;
                    
                    // Mark completed steps
                    for (let i = 0; i < questStatus.currentStep.index; i++) {
                        if (targetCharacterQuests.active.steps[i]) {
                            targetCharacterQuests.active.steps[i].completed = true;
                        }
                    }
                }
            }
        }
        
        return result;
    }

    /**
     * Convert existing quest to S-expression format
     */
    convertQuestToSExp(questData) {
        let sexp = `;; Converted quest: ${questData.id}\n`;
        sexp += `(quest ${questData.id}\n`;
        
        // Metadata
        sexp += `  (metadata\n`;
        sexp += `    (title "${questData.title}")\n`;
        sexp += `    (description "${questData.description}")\n`;
        sexp += `    (character ${questData.character}))\n\n`;
        
        // Variables
        if (questData.variables) {
            sexp += `  (variables\n`;
            for (const [key, value] of Object.entries(questData.variables)) {
                sexp += `    (${key} ${JSON.stringify(value)})\n`;
            }
            sexp += `  )\n\n`;
        }
        
        // Triggers
        sexp += `  (triggers\n`;
        
        // Convert NPC-based quest starts
        if (questData.startNPC) {
            sexp += `    (on-dialogue ${questData.startNPC}\n`;
            sexp += `      (when (not (quest-started "${questData.id}"))))\n`;
        } else {
            // Default trigger
            sexp += `    (on-dialogue any\n`;
            sexp += `      (when (and\n`;
            sexp += `        (not (quest-started "${questData.id}"))\n`;
            if (questData.requiresOutfit) {
                sexp += `        (outfit-is "${questData.requiresOutfit}")\n`;
            }
            sexp += `      )))\n`;
        }
        sexp += `  )\n\n`;
        
        // Steps
        sexp += `  (steps\n`;
        questData.steps.forEach((step, index) => {
            sexp += `    (step ${step.id}\n`;
            sexp += `      (description "${step.description}")\n`;
            sexp += `      (require\n`;
            
            if (step.location) {
                sexp += `        (at-location ${step.location})\n`;
            }
            if (step.npc) {
                sexp += `        (talking-to ${step.npc})\n`;
            }
            if (step.requires) {
                sexp += `        (has-memory "${step.requires}")\n`;
            }
            
            sexp += `      )\n`;
            sexp += `      (actions\n`;
            sexp += `        (set-memory "${step.id}_completed" true)\n`;
            
            if (step.gives) {
                sexp += `        (give-items ${step.gives})\n`;
            }
            if (step.reveals) {
                sexp += `        (reveal-location ${step.reveals})\n`;
            }
            
            // Add next step actions
            if (index < questData.steps.length - 1) {
                const nextStep = questData.steps[index + 1];
                if (nextStep.npc) {
                    sexp += `        (add-dialogue ${nextStep.npc}\n`;
                    sexp += `          (option continue_${nextStep.id}\n`;
                    sexp += `            "Продолжить квест"\n`;
                    sexp += `            "Давайте продолжим..."))\n`;
                }
            }
            
            sexp += `      ))\n`;
        });
        sexp += `  )\n\n`;
        
        // Completion
        sexp += `  (on-complete\n`;
        if (questData.rewards) {
            questData.rewards.forEach(reward => {
                sexp += `    (give-items ${reward})\n`;
            });
        }
        // Removed loyalty system rewards
        sexp += `    (show-message "Квест '${questData.title}' завершен!")\n`;
        sexp += `  ))\n`;
        
        return sexp;
    }

    /**
     * Save converted quest to file
     */
    saveConvertedQuest(questData) {
        const sexp = this.convertQuestToSExp(questData);
        const questDir = path.join(__dirname, 'quests');
        const filePath = path.join(questDir, `${questData.id}.scm`);
        
        // Ensure directory exists
        if (!fs.existsSync(questDir)) {
            fs.mkdirSync(questDir, { recursive: true });
        }
        
        fs.writeFileSync(filePath, sexp, 'utf-8');
        console.log(`Saved converted quest to: ${filePath}`);
        
        // Reload the quest
        this.questRunner.loadQuestFromFile(filePath);
        
        return filePath;
    }
}

module.exports = QuestIntegration;