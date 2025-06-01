const CoopStoryData = require('../games/pulpulak/data/coopStoryData');
const LocationData = require('../games/pulpulak/data/locationData');
const NPCData = require('../games/pulpulak/data/npcData');
const QuestData = require('../games/pulpulak/data/questData');
const { OUTFIT_NAMES, CHARACTER_NAMES, CHARACTER_ROLES } = require('../games/pulpulak/data/constants');
const { processQuestAction } = require('../games/pulpulak/data/questActionHandlers');
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
    constructor() {
        // Получаем менеджеров данных
        const managers = dataManagerFactory.getManagers();
        this.gameData = managers.gameData;
        this.playerData = managers.playerData;
        this.questData = managers.questData;
        this.requestData = managers.requestData;
        
        // Оставляем для обратной совместимости
        this.stateManager = new GameStateManager();
        this.immerStateManager = new ImmerStateManager();
    }

    // Запуск игры
    startGame(roomId, players) {
        try {
            this.validateGameStartParameters(roomId, players);
            
            // Создаём игру через GameDataManager
            const gameState = this.gameData.createGame(roomId, players);
            
            this.initializeNPCs(gameState);
            
            return this.getGameData(roomId);
        } catch (error) {
            console.error('Ошибка при запуске игры:', error);
            throw new Error(`Не удалось запустить игру: ${error.message}`);
        }
    }

    validateGameStartParameters(roomId, players) {
        ValidationHelpers.validateGameStartParameters(roomId, players);
    }

    initializeNPCs(gameState) {
        try {
            this.playerData.updateAllNPCsPresent(gameState.roomId);
        } catch (npcError) {
            console.error('Ошибка при инициализации NPC:', npcError);
            // В случае ошибки оставляем пустые массивы NPC
        }
    }

    // Создать запрос на обмен одеждой (через универсальную систему)
    createOutfitSwapRequest(roomId, fromPlayerId, fromCharacter) {
        return RequestProcessor.createOutfitSwapRequest(roomId, fromPlayerId, fromCharacter, this.requestData);
    }

    // Ответить на запрос обмена одеждой (через универсальную систему)
    respondToOutfitSwapRequest(roomId, playerId, accepted) {
        return RequestProcessor.respondToOutfitSwapRequest(roomId, playerId, accepted, this.requestData);
    }

    // Создать запрос любого типа
    createRequest(roomId, requestType, fromPlayerId, fromCharacter, requestData = {}) {
        return RequestProcessor.createRequest(roomId, requestType, fromPlayerId, fromCharacter, this.requestData, requestData);
    }

    // Ответить на запрос любого типа
    respondToRequest(roomId, playerId, accepted, responseData = {}) {
        return RequestProcessor.respondToRequest(roomId, playerId, accepted, this.requestData, responseData);
    }

    // Проверить, можно ли переодеваться (делегируется игре)
    canSwitchOutfits(gameState, character) {
        return RequestProcessor.canSwitchOutfits(gameState, character);
    }


    // Получить выборы для персонажа
    getChoicesForCharacter(gameState, character, sceneData) {
        const choices = this.getSceneChoices(gameState, character, sceneData);
        const specialChoices = this.getSpecialChoices(gameState, character);
        
        return [...choices, ...specialChoices];
    }

    getSceneChoices(gameState, character, sceneData) {
        return ChoiceBuilder.getSceneChoices(gameState, character, sceneData, this.isChoiceAvailable.bind(this));
    }

    getSpecialChoices(gameState, character) {
        // Получаем динамические выборы из игровой конфигурации
        const PulpulakGameConfig = require('../games/pulpulak/PulpulakGameConfig');
        const gameConfig = new PulpulakGameConfig();
        
        const getDynamicChoices = (gameState, character) => {
            if (typeof gameConfig.getDynamicChoices === 'function') {
                return gameConfig.getDynamicChoices(gameState, character);
            }
            return [];
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
        // Делегируем создание выбора в конкретную игру через GameConfig
        const PulpulakGameConfig = require('../games/pulpulak/PulpulakGameConfig');
        const gameConfig = new PulpulakGameConfig();
        
        const gameConfigCreateFn = (character) => {
            if (typeof gameConfig.createOutfitSwapChoice === 'function') {
                return gameConfig.createOutfitSwapChoice(character);
            }
            return null;
        };
        
        return ChoiceBuilder.createOutfitSwapChoice(character, gameConfigCreateFn);
    }

    // Обработка обычных выборов (НЕ запросов одежды)
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

    // Общие методы валидации
    validateGameState(roomId) {
        const gameState = this.gameData.getGame(roomId);
        return gameState ? { valid: true, gameState } : { valid: false, error: 'Игра не найдена' };
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
            CoopStoryData, 
            this, 
            this.requestData
        );
    }

    // Получить активный запрос для комнаты
    getActiveOutfitRequest(roomId) {
        return RequestProcessor.getActiveOutfitRequest(roomId, this.requestData);
    }

    // Получить активный запрос любого типа
    getActiveRequest(roomId) {
        return RequestProcessor.getActiveRequest(roomId, this.requestData);
    }

    // Отменить запрос 
    cancelOutfitRequest(roomId) {
        RequestProcessor.cancelOutfitRequest(roomId, this.requestData);
    }

    // Отменить запрос любого типа
    cancelRequest(roomId) {
        return RequestProcessor.cancelRequest(roomId, this.requestData);
    }

    // Применить эффекты выбора
    applyEffects(gameState, effects, character) {
        return EffectsProcessor.applyEffects(
            this.immerStateManager, 
            gameState, 
            effects, 
            character, 
            this.getNPCsForLocation.bind(this)
        );
    }

    // Проверить доступность выбора
    isChoiceAvailable(choice, gameState, character) {
        return ChoiceProcessor.isChoiceAvailable(choice, gameState, character);
    }

    // Сменить очередь хода
    switchTurn(gameState) {
        return ChoiceProcessor.switchTurn(gameState, this.immerStateManager);
    }

    // Получить данные игры
    getGameData(roomId) {
        const gameState = this.gameData.getGame(roomId);
        if (!gameState) return null;

        const sceneData = CoopStoryData.getScene(gameState.currentScene);
        const choicesForCharacters = this.buildChoicesData(gameState, sceneData);
        const activeRequest = this.getActiveRequest(roomId);
        
        return this.gameData.buildClientGameData(roomId, choicesForCharacters, activeRequest);
    }

    buildSceneData(sceneData) {
        return ChoiceBuilder.buildSceneData(sceneData);
    }

    buildChoicesData(gameState, sceneData) {
        return ChoiceBuilder.buildChoicesData(gameState, sceneData, this.getChoicesForCharacter.bind(this));
    }

    buildLocationsData(gameState) {
        return ChoiceBuilder.buildLocationsData(gameState, LocationData);
    }

    buildDialoguesData(gameState) {
        return ChoiceBuilder.buildDialoguesData(gameState);
    }

    buildQuestsData(gameState) {
        return ChoiceBuilder.buildQuestsData(gameState);
    }

    // Вспомогательные методы
    generateRequestId() {
        return ValidationHelpers.generateRequestId();
    }

    getCharacterName(character) {
        return ChoiceBuilder.getCharacterName(character);
    }

    getOutfitName(outfitId) {
        return OUTFIT_NAMES[outfitId] || outfitId;
    }

    getNPCsForLocation(location, gameState = null, character = null) {
        // Получаем NPC из NPCData с учётом состояния игры
        const npcs = NPCData.getNPCsForLocation(location, gameState, character);
        // Возвращаем только имена для обратной совместимости
        return npcs.map(npc => npc.name);
    }

    getMovementChoices(gameState, character) {
        return ChoiceBuilder.getMovementChoices(gameState, character, LocationData);
    }

    processMovement(gameState, targetLocation, character) {
        return MovementProcessor.processMovement(
            gameState, 
            targetLocation, 
            character, 
            LocationData, 
            this.requestData, 
            this.playerData, 
            this.gameData
        );
    }

    removeGame(roomId) {
        this.gameData.deleteGame(roomId);
        this.requestData.clearRoomRequests(roomId);
    }

    // Получить выборы взаимодействия с NPC
    getNPCInteractionChoices(gameState, character) {
        return ChoiceBuilder.getNPCInteractionChoices(gameState, character, NPCData);
    }

    // Обработка взаимодействия с NPC
    processNPCInteraction(gameState, npcId, character) {
        return NPCDialogueProcessor.processNPCInteraction(
            gameState, 
            npcId, 
            character, 
            NPCData, 
            this.immerStateManager, 
            this.gameData
        );
    }

    // Обработка выбора в диалоге с NPC
    processNPCDialogueChoice(roomId, playerId, choiceId, character) {
        return NPCDialogueProcessor.processNPCDialogueChoice(
            roomId, 
            playerId, 
            choiceId, 
            character, 
            NPCData, 
            this.immerStateManager, 
            this.gameData, 
            EffectsProcessor, 
            this.processQuestAction.bind(this), 
            this.switchTurn.bind(this), 
            this.getNPCsForLocation.bind(this)
        );
    }

    // Закрытие диалога с NPC
    closeNPCDialogue(roomId, playerId) {
        return NPCDialogueProcessor.closeNPCDialogue(
            roomId, 
            playerId, 
            this.immerStateManager, 
            this.gameData
        );
    }

    // === СИСТЕМА КВЕСТОВ ===

    // Начать квест
    startQuest(gameState, character, questId) {
        return QuestProcessor.startQuest(
            gameState, 
            character, 
            questId, 
            QuestData, 
            EffectsProcessor, 
            this
        );
    }

    // Обновить прогресс квеста
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

    // Завершить квест
    completeQuest(gameState, character) {
        return QuestProcessor.completeQuest(
            gameState, 
            character, 
            EffectsProcessor, 
            this
        );
    }

    // Получить текущий квест персонажа
    getCurrentQuest(gameState, character) {
        return QuestProcessor.getCurrentQuest(gameState, character);
    }

    // Получить текущий шаг квеста
    getCurrentQuestStep(gameState, character) {
        return QuestProcessor.getCurrentQuestStep(gameState, character);
    }

    // Проверить, может ли персонаж начать квест
    canStartQuest(gameState, character, questId) {
        return QuestProcessor.canStartQuest(gameState, character, questId, QuestData);
    }

    // Обработать квестовое действие из диалога
    processQuestAction(gameState, character, choiceId, dialogueResult) {
        return QuestProcessor.processQuestAction(
            gameState, 
            character, 
            choiceId, 
            dialogueResult, 
            processQuestAction.bind(null, gameState, character, choiceId, dialogueResult, this)
        );
    }

    // === МЕТОДЫ ДЛЯ ТЕСТИРОВАНИЯ (обратная совместимость) ===
    
    // Имитация старого gameLogic.games для тестов
    get games() {
        return {
            get: (roomId) => this.gameData.getGame(roomId),
            set: (roomId, gameState) => {
                // Для обратной совместимости - просто заменяем объект в Map
                this.gameData.games.set(roomId, gameState);
            },
            has: (roomId) => this.gameData.hasGame(roomId),
            delete: (roomId) => this.gameData.deleteGame(roomId)
        };
    }

    // Имитация старого gameLogic.outfitRequests для тестов
    get outfitRequests() {
        return {
            get: (roomId) => this.requestData.getActiveRequest(roomId),
            has: (roomId) => this.requestData.hasActiveRequest(roomId),
            delete: (roomId) => this.requestData.cancelRequest(roomId)
        };
    }
}

module.exports = CoopGameLogic;
