const GameStateManager = require('./gameStateManager');
const ImmerStateManager = require('./stateManager');
const dataManagerFactory = require('./managers/DataManagerFactory');
const ValidationHelpers = require('./utils/validationHelpers');
const EffectsProcessor = require('./utils/effectsProcessor');
const ChoiceBuilder = require('./utils/choiceBuilder');
const MovementProcessor = require('./utils/movementProcessor');
const NPCDialogueProcessor = require('./utils/npcDialogueProcessor');
const QuestProcessor = require('./utils/questProcessor');
const ChoiceProcessor = require('./utils/choiceProcessor');
const RequestProcessor = require('./utils/requestProcessor');

class CoopGameLogic {
    constructor(gameConfig) {
        if (!gameConfig || typeof gameConfig.getStoryData !== 'function') {
            throw new Error('GameConfig must implement IGameConfig interface');
        }
        
        this.gameConfig = gameConfig;
        
        // Get all data through gameConfig
        this.storyData = gameConfig.getStoryData();
        this.locationData = gameConfig.getLocationData();
        this.npcData = gameConfig.getNPCData();
        this.questData = gameConfig.getQuestData();
        this.constants = gameConfig.getGameConstants();
        
        // Data managers now receive gameConfig
        const managers = dataManagerFactory.getManagers(gameConfig);
        this.gameData = managers.gameData;
        this.playerData = managers.playerData;
        this.questDataManager = managers.questData;
        this.requestData = managers.requestData;
        
        // Register request handlers
        const requestHandlers = gameConfig.getRequestHandlers();
        if (requestHandlers && typeof requestHandlers.registerHandlers === 'function') {
            requestHandlers.registerHandlers(this.requestData);
        }
        
        this.stateManager = new GameStateManager();
        this.immerStateManager = new ImmerStateManager();
    }

    // Game startup
    startGame(roomId, players) {
        try {
            this.validateGameStartParameters(roomId, players);
            
            // Create game through GameDataManager
            const gameState = this.gameData.createGame(roomId, players);
            
            this.initializeNPCs(gameState);
            
            return this.getGameData(roomId);
        } catch (error) {
            console.error('Error starting game:', error);
            throw new Error(`Failed to start game: ${error.message}`);
        }
    }

    validateGameStartParameters(roomId, players) {
        ValidationHelpers.validateGameStartParameters(roomId, players);
    }

    initializeNPCs(gameState) {
        try {
            this.playerData.updateAllNPCsPresent(gameState.roomId);
        } catch (npcError) {
            console.error('Error initializing NPCs:', npcError);
            // In case of error, leave empty NPC arrays
        }
    }

    // Create outfit swap request (through universal system)
    createOutfitSwapRequest(roomId, fromPlayerId, fromCharacter) {
        return RequestProcessor.createOutfitSwapRequest(roomId, fromPlayerId, fromCharacter, this.requestData);
    }

    // Respond to outfit swap request (through universal system)
    respondToOutfitSwapRequest(roomId, playerId, accepted) {
        return RequestProcessor.respondToOutfitSwapRequest(roomId, playerId, accepted, this.requestData);
    }

    // Create request of any type
    createRequest(roomId, requestType, fromPlayerId, fromCharacter, requestData = {}) {
        return RequestProcessor.createRequest(roomId, requestType, fromPlayerId, fromCharacter, this.requestData, requestData);
    }

    // Respond to request of any type
    respondToRequest(roomId, playerId, accepted, responseData = {}) {
        return RequestProcessor.respondToRequest(roomId, playerId, accepted, this.requestData, responseData);
    }

    // Check if outfit switching is allowed (delegated to game)
    canSwitchOutfits(gameState, character) {
        return this.gameConfig.canSwitchOutfits(gameState, character);
    }

    // Get choices for character
    getChoicesForCharacter(gameState, character, sceneData) {
        const choices = this.getSceneChoices(gameState, character, sceneData);
        const specialChoices = this.getSpecialChoices(gameState, character);
        
        return [...choices, ...specialChoices];
    }

    getSceneChoices(gameState, character, sceneData) {
        return ChoiceBuilder.getSceneChoices(gameState, character, sceneData, this.isChoiceAvailable.bind(this));
    }

    getSpecialChoices(gameState, character) {
        // Get dynamic choices from game configuration
        const getDynamicChoices = (gameState, character) => {
            return this.gameConfig.getDynamicChoices(gameState, character);
        };
        
        return ChoiceBuilder.getSpecialChoices(
            gameState, 
            character, 
            this.getMovementChoices.bind(this),
            this.getNPCInteractionChoices.bind(this),
            getDynamicChoices
        );
    }

    createOutfitSwapChoice(character) {
        return this.gameConfig.createOutfitSwapChoice(character);
    }

    // Process regular choices (NOT outfit requests)
    makeChoice(roomId, playerId, choiceId, character) {
        const validators = {
            validateGameState: this.validateGameState.bind(this),
            validatePlayer: this.validatePlayer.bind(this),
            validateTurn: this.validateTurn.bind(this)
        };

        return ChoiceProcessor.makeChoice(
            roomId, 
            playerId, 
            choiceId, 
            character, 
            validators, 
            this, 
            this.processChoice.bind(this), 
            this.getGameData.bind(this)
        );
    }

    validateTurn(gameState, character, choiceId) {
        return ValidationHelpers.validateTurn(gameState, character, choiceId);
    }

    // General validation methods
    validateGameState(roomId) {
        const gameState = this.gameData.getGame(roomId);
        return gameState ? { valid: true, gameState } : { valid: false, error: 'Game not found' };
    }

    validatePlayer(gameState, playerId, character) {
        return ValidationHelpers.validatePlayer(gameState, playerId, character);
    }

    processChoice(gameState, choiceId, character) {
        const processors = {
            MovementProcessor: MovementProcessor,
            NPCDialogueProcessor: NPCDialogueProcessor,
            EffectsProcessor: EffectsProcessor,
            createRequest: this.createRequest.bind(this),
            processMovement: this.processMovement.bind(this),
            processNPCInteraction: this.processNPCInteraction.bind(this)
        };

        return ChoiceProcessor.processChoice(
            gameState, 
            choiceId, 
            character, 
            processors, 
            this.storyData, 
            this, 
            this.requestData
        );
    }

    // Get active request for room
    getActiveOutfitRequest(roomId) {
        return RequestProcessor.getActiveOutfitRequest(roomId, this.requestData);
    }

    // Get active request of any type
    getActiveRequest(roomId) {
        return RequestProcessor.getActiveRequest(roomId, this.requestData);
    }

    // Cancel request 
    cancelOutfitRequest(roomId) {
        RequestProcessor.cancelOutfitRequest(roomId, this.requestData);
    }

    // Cancel request of any type
    cancelRequest(roomId) {
        return RequestProcessor.cancelRequest(roomId, this.requestData);
    }

    // Apply choice effects
    applyEffects(gameState, effects, character) {
        return EffectsProcessor.applyEffects(
            this.immerStateManager, 
            gameState, 
            effects, 
            character, 
            this.getNPCsForLocation.bind(this)
        );
    }

    // Check choice availability
    isChoiceAvailable(choice, gameState, character) {
        return ChoiceProcessor.isChoiceAvailable(choice, gameState, character);
    }

    // Switch turn order
    switchTurn(gameState) {
        return ChoiceProcessor.switchTurn(gameState, this.immerStateManager);
    }

    // Get game data
    getGameData(roomId) {
        const gameState = this.gameData.getGame(roomId);
        if (!gameState) return null;

        const sceneData = this.storyData.getScene(gameState.currentScene);
        const choicesForCharacters = this.buildChoicesData(gameState, sceneData);
        const activeRequest = this.getActiveRequest(roomId);
        
        return this.gameData.buildClientGameData(roomId, choicesForCharacters, activeRequest);
    }

    buildSceneData(sceneData) {
        return ChoiceBuilder.buildSceneData(sceneData);
    }

    buildChoicesData(gameState, sceneData) {
        return ChoiceBuilder.buildChoicesData(gameState, sceneData, this.getChoicesForCharacter.bind(this), this.gameConfig);
    }

    buildLocationsData(gameState) {
        return ChoiceBuilder.buildLocationsData(gameState, this.locationData, this.gameConfig);
    }

    buildDialoguesData(gameState) {
        return ChoiceBuilder.buildDialoguesData(gameState, this.gameConfig);
    }

    buildQuestsData(gameState) {
        return ChoiceBuilder.buildQuestsData(gameState, this.gameConfig);
    }

    // Helper methods
    generateRequestId() {
        return ValidationHelpers.generateRequestId();
    }

    getCharacterName(character) {
        return ChoiceBuilder.getCharacterName(character, this.gameConfig);
    }

    getOutfitName(outfitId) {
        const constants = this.gameConfig.getGameConstants();
        return constants.OUTFIT_NAMES[outfitId] || outfitId;
    }

    getNPCsForLocation(location, gameState = null, character = null) {
        // Get NPCs from NPCData with game state consideration
        const npcs = this.npcData.getNPCsForLocation(location, gameState, character);
        // Return only names for backward compatibility
        return npcs.map(npc => npc.name);
    }

    getMovementChoices(gameState, character) {
        return ChoiceBuilder.getMovementChoices(gameState, character, this.locationData);
    }

    processMovement(gameState, targetLocation, character) {
        return MovementProcessor.processMovement(
            gameState, 
            targetLocation, 
            character, 
            this.locationData, 
            this.requestData, 
            this.playerData, 
            this.gameData
        );
    }

    removeGame(roomId) {
        this.gameData.deleteGame(roomId);
        this.requestData.clearRoomRequests(roomId);
    }

    // Get NPC interaction choices
    getNPCInteractionChoices(gameState, character) {
        return ChoiceBuilder.getNPCInteractionChoices(gameState, character, this.npcData);
    }

    // Process NPC interaction
    processNPCInteraction(gameState, npcId, character) {
        return NPCDialogueProcessor.processNPCInteraction(
            gameState, 
            npcId, 
            character, 
            this.npcData, 
            this.immerStateManager, 
            this.gameData
        );
    }

    // Process NPC dialogue choice
    processNPCDialogueChoice(roomId, playerId, choiceId, character) {
        return NPCDialogueProcessor.processNPCDialogueChoice(
            roomId, 
            playerId, 
            choiceId, 
            character, 
            this.npcData, 
            this.immerStateManager, 
            this.gameData, 
            EffectsProcessor, 
            this.processQuestAction.bind(this), 
            this.switchTurn.bind(this), 
            this.getNPCsForLocation.bind(this)
        );
    }

    // Close NPC dialogue
    closeNPCDialogue(roomId, playerId) {
        return NPCDialogueProcessor.closeNPCDialogue(
            roomId, 
            playerId, 
            this.immerStateManager, 
            this.gameData
        );
    }

    // === QUEST SYSTEM ===

    // Start quest
    startQuest(gameState, character, questId) {
        return QuestProcessor.startQuest(
            gameState, 
            character, 
            questId, 
            this.questData, 
            EffectsProcessor, 
            this
        );
    }

    // Update quest progress
    updateQuestProgress(gameState, character, stepId) {
        return QuestProcessor.updateQuestProgress(
            gameState, 
            character, 
            stepId, 
            EffectsProcessor, 
            this, 
            this.completeQuest.bind(this)
        );
    }

    // Complete quest
    completeQuest(gameState, character) {
        return QuestProcessor.completeQuest(
            gameState, 
            character, 
            EffectsProcessor, 
            this
        );
    }

    // Get current character quest
    getCurrentQuest(gameState, character) {
        return QuestProcessor.getCurrentQuest(gameState, character);
    }

    // Get current quest step
    getCurrentQuestStep(gameState, character) {
        return QuestProcessor.getCurrentQuestStep(gameState, character);
    }

    // Check if character can start quest
    canStartQuest(gameState, character, questId) {
        return QuestProcessor.canStartQuest(gameState, character, questId, this.questData);
    }

    // Process quest action from dialogue
    processQuestAction(gameState, character, choiceId, dialogueResult) {
        // Quest action processing needs to be delegated to game-specific logic
        const questActionHandlers = this.gameConfig.getQuestActionHandlers ? this.gameConfig.getQuestActionHandlers() : null;
        
        // Create a function that calls the quest action handlers module
        const questActionFn = questActionHandlers && questActionHandlers.processQuestAction
            ? (gameState, character, choiceId, dialogueResult) => {
                return questActionHandlers.processQuestAction(gameState, character, choiceId, dialogueResult, this);
            }
            : null;
        
        return QuestProcessor.processQuestAction(
            gameState, 
            character, 
            choiceId, 
            dialogueResult, 
            questActionFn
        );
    }

    // === TESTING METHODS (backward compatibility) ===
    
    // Simulate old gameLogic.games for tests
    get games() {
        return {
            get: (roomId) => this.gameData.getGame(roomId),
            set: (roomId, gameState) => {
                // For backward compatibility - just replace object in Map
                this.gameData.games.set(roomId, gameState);
            },
            has: (roomId) => this.gameData.hasGame(roomId),
            delete: (roomId) => this.gameData.deleteGame(roomId)
        };
    }

    // Simulate old gameLogic.outfitRequests for tests
    get outfitRequests() {
        return {
            get: (roomId) => this.requestData.getActiveRequest(roomId),
            has: (roomId) => this.requestData.hasActiveRequest(roomId),
            delete: (roomId) => this.requestData.cancelRequest(roomId)
        };
    }
}

module.exports = CoopGameLogic;