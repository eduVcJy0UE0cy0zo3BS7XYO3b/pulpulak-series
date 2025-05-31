const ImmerStateManager = require('../game/stateManager');

/**
 * Generic Game Engine
 * Handles core game mechanics independent of specific game content
 */
class GameEngine {
    constructor(gameConfig) {
        this.config = gameConfig;
        this.games = new Map();
        this.outfitRequests = new Map();
        this.stateManager = new ImmerStateManager();
        
        // Validate configuration
        const validation = this.config.validate();
        if (!validation.valid) {
            throw new Error(`Invalid game configuration: ${validation.errors.join(', ')}`);
        }
    }

    // === GAME LIFECYCLE ===

    /**
     * Start a new game
     */
    startGame(roomId, players) {
        if (this.games.has(roomId)) {
            throw new Error('Game already exists for this room');
        }

        const playerRoles = Object.keys(this.config.characters);
        if (Object.keys(players).length !== playerRoles.length) {
            throw new Error(`Expected ${playerRoles.length} players, got ${Object.keys(players).length}`);
        }

        const gameState = this.config.getInitialGameState(roomId, players);
        
        // Initialize NPCs for starting location
        this.updateNPCsForAllPlayers(gameState);
        
        this.games.set(roomId, gameState);
        
        return this.getGameData(roomId);
    }

    /**
     * Remove a game
     */
    removeGame(roomId) {
        this.games.delete(roomId);
        this.outfitRequests.delete(roomId);
    }

    // === GAME STATE ===

    /**
     * Get current game state
     */
    getGame(roomId) {
        return this.games.get(roomId);
    }

    /**
     * Update game state
     */
    updateGame(roomId, newState) {
        this.games.set(roomId, newState);
    }

    /**
     * Get formatted game data for clients
     */
    getGameData(roomId) {
        const gameState = this.getGame(roomId);
        if (!gameState) return null;

        const sceneData = this.config.getScene(gameState.currentScene);
        const choicesForCharacters = this.buildChoicesData(gameState, sceneData);
        const activeOutfitRequest = this.getActiveOutfitRequest(roomId);
        
        return {
            roomId,
            scene: {
                title: sceneData.title,
                text: sceneData.text
            },
            characters: this.buildCharacterData(gameState),
            choices: choicesForCharacters,
            activeOutfitRequest,
            turnOrder: gameState.turnOrder
        };
    }

    buildCharacterData(gameState) {
        const characters = {};
        
        Object.keys(this.config.characters).forEach(character => {
            const stats = gameState.stats[character];
            const location = this.config.getLocation(stats.location);
            
            characters[character] = {
                name: this.config.getCharacter(character).name,
                outfit: stats.outfit,
                location: {
                    id: stats.location,
                    name: location.name,
                    description: location.description,
                    icon: location.icon
                },
                inventory: stats.inventory,
                npcsPresent: stats.npcsPresent,
                activeQuest: gameState.quests[character].active,
                completedQuests: gameState.quests[character].completed.length,
                dialogue: gameState.npcDialogues[character]
            };
        });
        
        return characters;
    }

    buildChoicesData(gameState, sceneData) {
        const choices = {};
        
        Object.keys(this.config.characters).forEach(character => {
            choices[character] = this.getChoicesForCharacter(gameState, character, sceneData);
        });
        
        return choices;
    }

    // === TURN SYSTEM ===

    /**
     * Switch turn to next character
     */
    switchTurn(roomId) {
        const gameState = this.getGame(roomId);
        if (!gameState) return null;

        const characters = Object.keys(this.config.characters);
        const currentIndex = characters.indexOf(gameState.turnOrder);
        const nextIndex = (currentIndex + 1) % characters.length;
        
        const updatedState = this.stateManager.updateState(gameState, draft => {
            draft.turnOrder = characters[nextIndex];
        });
        
        this.updateGame(roomId, updatedState);
        return updatedState;
    }

    // === SCENE SYSTEM ===

    /**
     * Change to a new scene
     */
    changeScene(roomId, sceneId) {
        const gameState = this.getGame(roomId);
        if (!gameState) return null;

        const scene = this.config.getScene(sceneId);
        if (!scene) {
            throw new Error(`Scene ${sceneId} not found`);
        }

        const updatedState = this.stateManager.updateState(gameState, draft => {
            draft.currentScene = sceneId;
        });
        
        this.updateGame(roomId, updatedState);
        
        // Cancel any active outfit requests when scene changes
        this.cancelOutfitRequest(roomId);
        
        return updatedState;
    }

    // === CHOICE SYSTEM ===

    /**
     * Get available choices for a character
     */
    getChoicesForCharacter(gameState, character, sceneData) {
        const choices = [];
        
        // Scene-specific choices
        if (gameState.turnOrder === character && sceneData.choices && sceneData.choices[character]) {
            choices.push(...sceneData.choices[character].filter(choice => 
                this.isChoiceAvailable(choice, gameState, character)
            ));
        }
        
        // Dynamic choices
        choices.push(...this.getDynamicChoices(gameState, character));
        
        return choices;
    }

    /**
     * Get dynamic choices (movement, interactions, etc.)
     */
    getDynamicChoices(gameState, character) {
        const choices = [];
        
        // Outfit swapping
        if (this.config.isOutfitSwappingEnabled() && this.canRequestOutfitSwap(gameState, character)) {
            choices.push(this.createOutfitSwapChoice(character));
        }
        
        // Movement
        choices.push(...this.getMovementChoices(gameState, character));
        
        // NPC interactions
        choices.push(...this.getNPCInteractionChoices(gameState, character));
        
        return choices;
    }

    /**
     * Check if a choice is available
     */
    isChoiceAvailable(choice, gameState, character) {
        // Basic availability check - can be extended by specific games
        return true;
    }

    /**
     * Process a choice made by a player
     */
    makeChoice(roomId, playerId, choiceId, character) {
        const gameState = this.getGame(roomId);
        if (!gameState) {
            return { success: false, message: 'Game not found' };
        }

        // Validate player
        if (!this.validatePlayer(gameState, playerId, character)) {
            return { success: false, message: 'Invalid player' };
        }

        // Process different types of choices
        if (choiceId === 'request_outfit_swap') {
            return this.processOutfitSwapRequest(roomId, playerId, character);
        }
        
        if (choiceId.startsWith('move_to_')) {
            const targetLocation = choiceId.replace('move_to_', '');
            return this.processMovement(roomId, character, targetLocation);
        }
        
        if (choiceId.startsWith('talk_to_')) {
            const npcId = choiceId.replace('talk_to_', '');
            return this.processNPCInteraction(roomId, character, npcId);
        }
        
        // Scene choices
        return this.processSceneChoice(roomId, character, choiceId);
    }

    // === MOVEMENT SYSTEM ===

    /**
     * Get movement choices for a character
     */
    getMovementChoices(gameState, character) {
        const currentLocation = gameState.stats[character].location;
        const locationData = this.config.getLocation(currentLocation);
        
        if (!locationData || !locationData.connections) return [];
        
        return locationData.connections.map(connectionId => {
            const targetLocation = this.config.getLocation(connectionId);
            return {
                id: `move_to_${connectionId}`,
                text: `${targetLocation.icon} Перейти: ${targetLocation.name}`,
                description: `Отправиться в ${targetLocation.name}`,
                isMovement: true,
                targetLocation: connectionId
            };
        });
    }

    /**
     * Process character movement
     */
    processMovement(roomId, character, targetLocation) {
        const gameState = this.getGame(roomId);
        const currentLocation = gameState.stats[character].location;
        
        // Validate movement
        const locationData = this.config.getLocation(currentLocation);
        if (!locationData.connections.includes(targetLocation)) {
            return { success: false, message: 'Cannot reach that location from here' };
        }
        
        const targetLocationData = this.config.getLocation(targetLocation);
        if (!targetLocationData) {
            return { success: false, message: 'Unknown location' };
        }
        
        // Cancel outfit requests when moving
        this.cancelOutfitRequest(roomId);
        
        // Update character location
        const updatedState = this.stateManager.updateState(gameState, draft => {
            draft.stats[character].location = targetLocation;
            draft.stats[character].npcsPresent = this.config.getNPCsForLocation(
                targetLocation, gameState, character
            ).map(npc => npc.name);
        });
        
        this.updateGame(roomId, updatedState);
        
        return {
            success: true,
            message: `Moved to ${targetLocationData.name}`,
            gameData: this.getGameData(roomId)
        };
    }

    // === NPC INTERACTION SYSTEM ===

    /**
     * Get NPC interaction choices
     */
    getNPCInteractionChoices(gameState, character) {
        const currentLocation = gameState.stats[character].location;
        const npcs = this.config.getNPCsForLocation(currentLocation, gameState, character);
        
        return npcs.map(npc => ({
            id: `talk_to_${npc.id}`,
            text: `💬 Поговорить с ${npc.name}`,
            description: npc.description,
            isNPCInteraction: true,
            npcId: npc.id
        }));
    }

    /**
     * Process NPC interaction
     */
    processNPCInteraction(roomId, character, npcId) {
        const gameState = this.getGame(roomId);
        const npc = this.config.getNPC(npcId);
        
        if (!npc) {
            return { success: false, message: 'NPC not found' };
        }

        // This is a basic implementation - specific games should override
        // the dialogue system in their configuration
        
        return {
            success: true,
            message: `Started conversation with ${npc.name}`,
            showDialogue: true
        };
    }

    // === OUTFIT SYSTEM ===

    /**
     * Check if character can request outfit swap
     */
    canRequestOutfitSwap(gameState, character) {
        if (!this.config.isOutfitSwappingEnabled()) return false;
        if (this.outfitRequests.has(gameState.roomId)) return false;
        
        const currentLocation = gameState.stats[character].location;
        const locationData = this.config.getLocation(currentLocation);
        
        return locationData.canChangeOutfit && this.playersInSameLocation(gameState, character);
    }

    /**
     * Check if players are in same location
     */
    playersInSameLocation(gameState, character) {
        const characters = Object.keys(this.config.characters);
        const characterLocation = gameState.stats[character].location;
        
        return characters.every(char => 
            gameState.stats[char].location === characterLocation
        );
    }

    /**
     * Create outfit swap choice
     */
    createOutfitSwapChoice(character) {
        return {
            id: 'request_outfit_swap',
            text: '👗 Предложить поменяться одеждой',
            description: 'Предложить обмен нарядами',
            isOutfitRequest: true
        };
    }

    /**
     * Process outfit swap request
     */
    processOutfitSwapRequest(roomId, playerId, character) {
        const gameState = this.getGame(roomId);
        
        if (!this.canRequestOutfitSwap(gameState, character)) {
            return { success: false, message: 'Cannot swap outfits here' };
        }
        
        const requestId = this.generateRequestId();
        const characters = Object.keys(this.config.characters);
        const otherCharacter = characters.find(char => char !== character);
        
        const request = {
            id: requestId,
            fromCharacter: character,
            toCharacter: otherCharacter,
            fromPlayer: playerId,
            status: 'pending'
        };
        
        this.outfitRequests.set(roomId, request);
        
        return {
            success: true,
            message: 'Outfit swap request sent',
            request
        };
    }

    /**
     * Respond to outfit swap request
     */
    respondToOutfitSwapRequest(roomId, playerId, accepted) {
        const request = this.outfitRequests.get(roomId);
        if (!request) {
            return { success: false, message: 'No active request' };
        }
        
        const gameState = this.getGame(roomId);
        
        if (accepted) {
            // Swap outfits
            const fromOutfit = gameState.stats[request.fromCharacter].outfit;
            const toOutfit = gameState.stats[request.toCharacter].outfit;
            
            const updatedState = this.stateManager.updateState(gameState, draft => {
                draft.stats[request.fromCharacter].outfit = toOutfit;
                draft.stats[request.toCharacter].outfit = fromOutfit;
            });
            
            this.updateGame(roomId, updatedState);
        }
        
        this.outfitRequests.delete(roomId);
        
        return {
            success: true,
            message: accepted ? 'Outfits swapped!' : 'Request declined',
            gameData: this.getGameData(roomId)
        };
    }

    /**
     * Get active outfit request
     */
    getActiveOutfitRequest(roomId) {
        return this.outfitRequests.get(roomId) || null;
    }

    /**
     * Cancel outfit request
     */
    cancelOutfitRequest(roomId) {
        this.outfitRequests.delete(roomId);
    }

    // === SCENE CHOICES ===

    /**
     * Process scene-specific choice
     */
    processSceneChoice(roomId, character, choiceId) {
        const gameState = this.getGame(roomId);
        const sceneData = this.config.getScene(gameState.currentScene);
        
        if (gameState.turnOrder !== character) {
            return { success: false, message: 'Not your turn' };
        }
        
        const choice = sceneData.choices[character]?.find(c => c.id === choiceId);
        if (!choice) {
            return { success: false, message: 'Invalid choice' };
        }
        
        let updatedState = gameState;
        
        // Apply effects
        if (choice.effects) {
            updatedState = this.applyEffects(updatedState, choice.effects, character);
        }
        
        // Change scene if specified
        if (choice.nextScene) {
            updatedState = this.stateManager.updateState(updatedState, draft => {
                draft.currentScene = choice.nextScene;
            });
            this.cancelOutfitRequest(roomId);
        }
        
        // Switch turn
        this.updateGame(roomId, updatedState);
        this.switchTurn(roomId);
        
        return {
            success: true,
            message: choice.resultText || 'Choice made',
            gameData: this.getGameData(roomId)
        };
    }

    /**
     * Apply choice effects
     */
    applyEffects(gameState, effects, character) {
        return this.stateManager.updateState(gameState, draft => {
            if (effects.outfit) {
                draft.stats[character].outfit = effects.outfit;
            }
            if (effects.location) {
                draft.stats[character].location = effects.location;
                draft.stats[character].npcsPresent = this.config.getNPCsForLocation(
                    effects.location, gameState, character
                ).map(npc => npc.name);
            }
            if (effects.awareness) {
                draft.stats[character].awareness += effects.awareness;
            }
            if (effects.item) {
                draft.stats[character].inventory.push(effects.item);
            }
        });
    }

    // === UTILITIES ===

    /**
     * Validate player controls character
     */
    validatePlayer(gameState, playerId, character) {
        const player = gameState.players[character];
        return player && player.id === playerId;
    }

    /**
     * Update NPCs for all players
     */
    updateNPCsForAllPlayers(gameState) {
        Object.keys(this.config.characters).forEach(character => {
            const location = gameState.stats[character].location;
            gameState.stats[character].npcsPresent = this.config.getNPCsForLocation(
                location, gameState, character
            ).map(npc => npc.name);
        });
    }

    /**
     * Generate unique request ID
     */
    generateRequestId() {
        return Math.random().toString(36).substring(2, 15);
    }
}

module.exports = GameEngine;