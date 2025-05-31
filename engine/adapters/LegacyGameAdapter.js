const GameEngine = require('../GameEngine');
// Use dynamic imports for game-specific data
const path = require('path');

/**
 * Adapter to make the new GameEngine compatible with existing CoopGameLogic API
 * This ensures tests and existing code continue to work
 */
class LegacyGameAdapter {
    constructor(gameConfig) {
        this.engine = new GameEngine(gameConfig);
        this.config = gameConfig;
        
        // For legacy compatibility
        this.stateManager = this.engine.stateManager;
        this.immerStateManager = this.engine.stateManager;
        
        // Try to load game-specific modules
        this.loadGameModules();
    }

    loadGameModules() {
        try {
            // Try to load modules based on config
            if (this.config.gameId === 'pulpulak_coop') {
                this.NPCData = require('../../games/pulpulak/data/npcData');
                this.processQuestActionFn = require('../../games/pulpulak/data/questActionHandlers').processQuestAction;
            } else {
                // Fallback to original files for other games
                this.NPCData = require('../../game/npcData');
                this.processQuestActionFn = require('../../game/questActionHandlers').processQuestAction;
            }
        } catch (error) {
            console.warn('Could not load game-specific modules, using fallback:', error.message);
            // Fallback to original files
            this.NPCData = require('../../game/npcData');
            this.processQuestActionFn = require('../../game/questActionHandlers').processQuestAction;
        }
    }

    // === LEGACY API COMPATIBILITY ===

    // Legacy game management
    startGame(roomId, players) {
        const result = this.engine.startGame(roomId, players);
        // Return data in legacy format
        return this.getGameData(roomId);
    }

    removeGame(roomId) {
        return this.engine.removeGame(roomId);
    }

    getGameData(roomId) {
        const gameData = this.engine.getGameData(roomId);
        if (!gameData) return null;
        
        const gameState = this.engine.getGame(roomId);
        if (!gameState) return gameData;
        
        // Convert new format to legacy format for backward compatibility
        const legacyGameData = {
            ...gameData,
            // Add legacy fields that client expects
            players: gameState.players,
            stats: gameState.stats,
            currentScene: gameState.currentScene,
            turnOrder: gameState.turnOrder,
            npcDialogues: gameState.npcDialogues,
            quests: gameState.quests,
            globalQuestMemory: gameState.globalQuestMemory,
            npcMemory: gameState.npcMemory
        };
        
        return legacyGameData;
    }

    // Legacy choice handling
    makeChoice(roomId, playerId, choiceId, character) {
        const result = this.engine.makeChoice(roomId, playerId, choiceId, character);
        
        if (result.success && result.gameData) {
            // Convert gameData to legacy format
            const gameState = this.engine.getGame(roomId);
            result.gameData = {
                ...result.gameData,
                players: gameState?.players,
                stats: gameState?.stats,
                currentScene: gameState?.currentScene,
                npcDialogues: gameState?.npcDialogues,
                quests: gameState?.quests,
                globalQuestMemory: gameState?.globalQuestMemory,
                npcMemory: gameState?.npcMemory
            };
        }
        
        return result;
    }

    // Legacy outfit system
    createOutfitSwapRequest(roomId, fromPlayerId, fromCharacter) {
        return this.engine.processOutfitSwapRequest(roomId, fromPlayerId, fromCharacter);
    }

    respondToOutfitSwapRequest(roomId, playerId, accepted) {
        return this.engine.respondToOutfitSwapRequest(roomId, playerId, accepted);
    }

    getActiveOutfitRequest(roomId) {
        return this.engine.getActiveOutfitRequest(roomId);
    }

    cancelOutfitRequest(roomId) {
        return this.engine.cancelOutfitRequest(roomId);
    }

    // === ENHANCED NPC DIALOGUE SYSTEM ===

    /**
     * Process NPC interaction with full original logic
     */
    processNPCInteraction(gameState, npcId, character) {
        try {
            const npc = this.NPCData.getNPC(npcId);
            if (!npc) {
                return { success: false, message: "NPC не найден" };
            }

            // Get character outfit
            const outfit = gameState.stats[character].outfit;
            
            // Get NPC memory and create dialogue through Immer
            let updatedGameState = gameState;
            if (!gameState.npcMemory[character][npcId]) {
                updatedGameState = this.engine.stateManager.updateState(gameState, draft => {
                    draft.npcMemory[character][npcId] = {};
                });
                this.engine.updateGame(updatedGameState.roomId, updatedGameState);
            }
            const npcMemory = updatedGameState.npcMemory[character][npcId];
            
            // Get dialogue based on outfit, memory, location and quest state
            const currentLocation = updatedGameState.stats[character].location;
            const questState = updatedGameState.quests[character];
            const globalQuestMemory = updatedGameState.globalQuestMemory;
            const dialogue = this.NPCData.getNPCDialogue(npcId, outfit, npcMemory, currentLocation, questState, globalQuestMemory);
            
            if (!dialogue) {
                return { success: false, message: "Диалог не найден" };
            }

            // Save dialogue information for specific character
            updatedGameState = this.engine.stateManager.updateState(updatedGameState, draft => {
                draft.npcDialogues[character] = {
                    npcId: npcId,
                    npcName: npc.name,
                    greeting: dialogue.greeting,
                    choices: dialogue.choices,
                    attitude: this.NPCData.getNPCAttitude(npcId, outfit),
                    activeCharacter: character,
                    isFollowUp: false
                };
            });
            this.engine.updateGame(updatedGameState.roomId, updatedGameState);

            return { 
                success: true, 
                showDialogue: true,
                message: `Начат диалог с ${npc.name}`
            };
        } catch (error) {
            console.error('Ошибка при взаимодействии с NPC:', error);
            return { 
                success: false, 
                message: `Не удалось начать диалог: ${error.message}` 
            };
        }
    }

    /**
     * Process NPC dialogue choice with full original logic
     */
    processNPCDialogueChoice(roomId, playerId, choiceId, character) {
        let gameState = this.engine.getGame(roomId);
        if (!gameState) {
            return { success: false, message: "Игра не найдена" };
        }

        // Validate player
        const playerCharacter = gameState.players[character];
        if (!playerCharacter || playerCharacter.id !== playerId) {
            return { success: false, message: "Вы управляете другим персонажем" };
        }

        // Check active dialogue
        if (!gameState.npcDialogues[character]) {
            return { success: false, message: "Нет активного диалога" };
        }

        const npcId = gameState.npcDialogues[character].npcId;
        const outfit = gameState.stats[character].outfit;

        // Get NPC memory for this character
        if (!gameState.npcMemory[character][npcId]) {
            gameState.npcMemory[character][npcId] = {};
        }

        // Process choice through NPCData
        const isFollowUp = gameState.npcDialogues[character].isFollowUp || false;
        const currentChoices = isFollowUp ? gameState.npcDialogues[character].choices : [];
        
        // Create mutable copy of NPC memory for NPCData
        const npcMemoryCopy = JSON.parse(JSON.stringify(gameState.npcMemory[character][npcId]));
        
        const result = this.NPCData.processDialogueChoice(
            npcId, 
            choiceId, 
            outfit, 
            npcMemoryCopy,
            isFollowUp,
            currentChoices,
            gameState.stats[character].location
        );
        
        if (!result) {
            return { success: false, message: "Неверный выбор" };
        }

        // Update NPC memory and apply effects
        gameState = this.engine.stateManager.updateState(gameState, draft => {
            draft.npcMemory[character][npcId] = result.updatedMemory;
            
            // Apply choice effects
            if (result.effects) {
                if (result.effects.item) {
                    draft.stats[character].inventory.push(result.effects.item);
                }
                if (result.effects.info) {
                    draft.stats[character][result.effects.info] = true;
                }
            }
        });
        this.engine.updateGame(gameState.roomId, gameState);

        // Process quest actions
        const questResult = this.processQuestAction(gameState, character, choiceId, result);
        if (questResult && questResult.success && questResult.gameState) {
            gameState = questResult.gameState;
            this.engine.updateGame(gameState.roomId, gameState);
        }

        // Update NPCs in locations after quest actions
        gameState = this.engine.stateManager.updateState(gameState, draft => {
            draft.stats.princess.npcsPresent = this.getNPCsForLocation(draft.stats.princess.location, gameState, 'princess');
            draft.stats.helper.npcsPresent = this.getNPCsForLocation(draft.stats.helper.location, gameState, 'helper');
        });
        this.engine.updateGame(gameState.roomId, gameState);

        // Save attitude before clearing dialogue
        const attitude = gameState.npcDialogues[character]?.attitude;

        // Handle follow-up choices or end dialogue
        if (result.next_choices && result.next_choices.length > 0) {
            gameState = this.engine.stateManager.updateState(gameState, draft => {
                draft.npcDialogues[character].choices = result.next_choices;
                draft.npcDialogues[character].greeting = result.response;
                draft.npcDialogues[character].isFollowUp = true;
            });
            this.engine.updateGame(gameState.roomId, gameState);

            return { 
                success: true, 
                message: result.response,
                type: attitude === 'hostile' ? 'warning' : 'success',
                hasFollowUp: true
            };
        } else {
            // Clear dialogue and switch turn
            gameState = this.engine.stateManager.updateState(gameState, draft => {
                draft.npcDialogues[character] = null;
            });
            gameState = this.switchTurn(gameState);
            this.engine.updateGame(gameState.roomId, gameState);

            return { 
                success: true, 
                message: result.response,
                type: attitude === 'hostile' ? 'warning' : 'success'
            };
        }
    }

    /**
     * Close NPC dialogue
     */
    closeNPCDialogue(roomId, playerId) {
        let gameState = this.engine.getGame(roomId);
        if (!gameState) {
            return { success: false, message: "Игра не найдена" };
        }

        // Find character that belongs to this player
        let character = null;
        for (const [char, player] of Object.entries(gameState.players)) {
            if (player && player.id === playerId) {
                character = char;
                break;
            }
        }

        if (!character) {
            return { success: false, message: "Игрок не найден" };
        }

        // Check active dialogue
        if (!gameState.npcDialogues[character]) {
            return { success: false, message: "Нет активного диалога" };
        }

        // Close dialogue
        gameState = this.engine.stateManager.updateState(gameState, draft => {
            draft.npcDialogues[character] = null;
        });
        this.engine.updateGame(gameState.roomId, gameState);

        return { 
            success: true, 
            message: "Диалог закрыт",
            gameState: gameState
        };
    }

    // === QUEST SYSTEM ===

    processQuestAction(gameState, character, choiceId, dialogueResult) {
        return this.processQuestActionFn(gameState, character, choiceId, dialogueResult, this);
    }

    startQuest(gameState, character, questId) {
        // Delegate to real quest system - would need to implement
        return { success: false, message: "Quest system not yet migrated" };
    }

    // === HELPER METHODS ===

    getNPCsForLocation(location, gameState, character) {
        return this.NPCData.getNPCsForLocation(location, gameState, character).map(npc => npc.name);
    }

    switchTurn(gameState) {
        return this.engine.stateManager.updateState(gameState, draft => {
            draft.turnOrder = draft.turnOrder === 'princess' ? 'helper' : 'princess';
        });
    }

    applyEffects(gameState, effects, character) {
        return this.engine.applyEffects(gameState, effects, character);
    }

    // === LEGACY PROPERTIES FOR TESTS ===

    get games() {
        return {
            get: (roomId) => this.engine.getGame(roomId),
            set: (roomId, gameState) => {
                this.engine.updateGame(roomId, gameState);
            },
            has: (roomId) => !!this.engine.getGame(roomId),
            delete: (roomId) => this.engine.removeGame(roomId)
        };
    }

    get outfitRequests() {
        return {
            get: (roomId) => this.engine.getActiveOutfitRequest(roomId),
            has: (roomId) => !!this.engine.getActiveOutfitRequest(roomId),
            delete: (roomId) => this.engine.cancelOutfitRequest(roomId)
        };
    }
}

module.exports = LegacyGameAdapter;