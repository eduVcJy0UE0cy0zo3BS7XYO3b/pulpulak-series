/**
 * Choice Processing utility extracted from CoopGameLogic
 * Handles all player choice processing and validation
 */

class ChoiceProcessor {
    /**
     * Process a player choice with full validation and execution
     * @param {string} roomId - Room identifier
     * @param {string} playerId - Player identifier
     * @param {string} choiceId - Choice identifier
     * @param {string} character - Character making the choice
     * @param {Object} validators - Validation functions
     * @param {Object} gameData - Game data manager
     * @param {Function} processChoiceFn - Choice processing function
     * @param {Function} getGameDataFn - Get game data function
     * @returns {Object} Choice processing result
     */
    static makeChoice(roomId, playerId, choiceId, character, validators, gameData, processChoiceFn, getGameDataFn) {
        try {
            const validation = validators.validateGameState(roomId);
            if (!validation.valid) {
                return { success: false, message: validation.error };
            }

            if (!validators.validatePlayer(validation.gameState, playerId, character)) {
                return { success: false, message: "Вы управляете другим персонажем" };
            }

            if (!validators.validateTurn(validation.gameState, character, choiceId)) {
                return { success: false, message: "Сейчас не ваш ход" };
            }

            const result = processChoiceFn(validation.gameState, choiceId, character);
            if (result.success) {
                return {
                    success: true,
                    gameData: getGameDataFn(roomId),
                    message: result.message
                };
            }

            return result;
        } catch (error) {
            console.error('Ошибка при обработке выбора:', error);
            return { 
                success: false, 
                message: `Ошибка при выполнении действия: ${error.message}` 
            };
        }
    }

    /**
     * Process a specific choice (core choice logic)
     * @param {Object} gameState - Current game state
     * @param {string} choiceId - Choice identifier
     * @param {string} character - Character making the choice
     * @param {Object} processors - Various processing modules
     * @param {Object} CoopStoryData - Story data module
     * @param {Object} gameData - Game data manager
     * @param {Object} requestData - Request data manager
     * @returns {Object} Choice processing result
     */
    static processChoice(gameState, choiceId, character, processors, CoopStoryData, gameData, requestData) {
        // Запросы одежды теперь обрабатываются через универсальную систему
        if (choiceId === 'request_outfit_swap') {
            // Получаем ID игрока из gameState
            const playerId = gameState.players[character]?.id;
            if (!playerId) {
                return { success: false, message: "Игрок не найден" };
            }
            
            // Создаем запрос через универсальную систему
            return processors.createRequest(gameState.roomId, 'outfit_swap', playerId, character, {});
        }

        // Проверка на перемещение
        const movementValidation = processors.MovementProcessor.validateMovementChoice(choiceId);
        if (movementValidation.isMovement) {
            return processors.processMovement(gameState, movementValidation.targetLocation, character);
        }

        // Проверка на взаимодействие с NPC
        const npcValidation = processors.NPCDialogueProcessor.validateNPCInteractionChoice(choiceId);
        if (npcValidation.isNPCInteraction) {
            return processors.processNPCInteraction(gameState, npcValidation.npcId, character);
        }

        // Обработка обычных выборов
        return ChoiceProcessor.processSceneChoice(gameState, choiceId, character, CoopStoryData, processors.EffectsProcessor, gameData, requestData);
    }

    /**
     * Process scene-based choices (story choices)
     * @param {Object} gameState - Current game state
     * @param {string} choiceId - Choice identifier
     * @param {string} character - Character making the choice
     * @param {Object} CoopStoryData - Story data module
     * @param {Object} EffectsProcessor - Effects processor
     * @param {Object} gameData - Game data manager
     * @param {Object} requestData - Request data manager
     * @returns {Object} Choice processing result
     */
    static processSceneChoice(gameState, choiceId, character, CoopStoryData, EffectsProcessor, gameData, requestData) {
        const sceneData = CoopStoryData.getScene(gameState.currentScene);
        const choice = sceneData.choices[character]?.find(c => c.id === choiceId);
        
        if (!choice) {
            return { success: false, message: "Неверный выбор" };
        }

        // Применяем эффекты выбора
        if (choice.effects) {
            gameState = EffectsProcessor.applyEffects(
                gameData.immerStateManager, 
                gameState, 
                choice.effects, 
                character, 
                gameData.getNPCsForLocation.bind(gameData)
            );
            // Сохраняем обновленное состояние
            gameData.gameData.games.set(gameState.roomId, gameState);
        }

        // Проверяем, меняется ли сцена
        if (choice.nextScene) {
            gameData.gameData.updateScene(gameState.roomId, choice.nextScene);
            
            // При смене сцены отменяем активные запросы
            requestData.cancelRequest(gameState.roomId);
        }

        // Меняем очередь хода
        gameData.gameData.switchTurn(gameState.roomId);

        return { 
            success: true, 
            message: choice.resultText || "Выбор сделан"
        };
    }

    /**
     * Check if a choice is available for a character
     * @param {Object} choice - Choice object
     * @param {Object} gameState - Current game state
     * @param {string} character - Character to check
     * @returns {boolean} True if choice is available
     */
    static isChoiceAvailable(choice, gameState, character) {
        // Базовая проверка - можно расширить
        return true;
    }

    /**
     * Switch turn order in game state
     * @param {Object} gameState - Current game state
     * @param {Object} immerStateManager - Immer state manager
     * @returns {Object} Updated game state
     */
    static switchTurn(gameState, immerStateManager) {
        return immerStateManager.updateState(gameState, draft => {
            draft.turnOrder = draft.turnOrder === 'princess' ? 'helper' : 'princess';
        });
    }

    /**
     * Validate if choice is a special action (not requiring turn validation)
     * @param {string} choiceId - Choice identifier
     * @returns {boolean} True if special action
     */
    static isSpecialAction(choiceId) {
        return choiceId === 'request_outfit_swap' || 
               choiceId.startsWith('move_to_') || 
               choiceId.startsWith('talk_to_');
    }

    /**
     * Get choice type for processing routing
     * @param {string} choiceId - Choice identifier
     * @returns {string} Choice type
     */
    static getChoiceType(choiceId) {
        if (choiceId === 'request_outfit_swap') return 'outfit_request';
        if (choiceId.startsWith('move_to_')) return 'movement';
        if (choiceId.startsWith('talk_to_')) return 'npc_interaction';
        return 'scene_choice';
    }

    /**
     * Validate choice exists in current scene
     * @param {string} choiceId - Choice identifier
     * @param {Object} gameState - Current game state
     * @param {string} character - Character making choice
     * @param {Object} CoopStoryData - Story data module
     * @returns {Object} Validation result
     */
    static validateSceneChoice(choiceId, gameState, character, CoopStoryData) {
        const sceneData = CoopStoryData.getScene(gameState.currentScene);
        const choice = sceneData.choices[character]?.find(c => c.id === choiceId);
        
        if (!choice) {
            return { valid: false, error: "Выбор недоступен в текущей сцене" };
        }

        return { valid: true, choice };
    }
}

module.exports = ChoiceProcessor;