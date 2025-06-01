/**
 * Quest Processing utility extracted from CoopGameLogic
 * Handles all quest-related operations and state management
 */

class QuestProcessor {
    /**
     * Start a new quest for a character
     * @param {Object} gameState - Current game state
     * @param {string} character - Character starting the quest
     * @param {string} questId - Quest identifier
     * @param {Object} QuestData - Quest data module
     * @param {Object} EffectsProcessor - Effects processor
     * @param {Object} gameData - Game data manager
     * @returns {Object} Quest start result
     */
    static startQuest(gameState, character, questId, QuestData, EffectsProcessor, gameData) {
        const quest = QuestData.createQuestInstance(questId);
        if (!quest) {
            return { success: false, message: "Квест не найден" };
        }

        if (gameState.quests[character].active) {
            return { success: false, message: "У вас уже есть активный квест" };
        }

        const updatedState = EffectsProcessor.startQuest(gameData.immerStateManager, gameState, character, quest);
        // Сохраняем обновленное состояние
        gameData.gameData.games.set(updatedState.roomId, updatedState);
        
        return { 
            success: true, 
            message: `Начат квест: ${quest.title}`,
            quest: quest,
            gameState: updatedState
        };
    }

    /**
     * Update quest progress for a character
     * @param {Object} gameState - Current game state
     * @param {string} character - Character progressing the quest
     * @param {string} stepId - Quest step identifier
     * @param {Object} EffectsProcessor - Effects processor
     * @param {Object} gameData - Game data manager
     * @param {Function} completeQuestFn - Quest completion function
     * @returns {Object} Quest progress result
     */
    static updateQuestProgress(gameState, character, stepId, EffectsProcessor, gameData, completeQuestFn) {
        const activeQuest = gameState.quests[character].active;
        if (!activeQuest) {
            return { success: false, message: "Нет активного квеста" };
        }

        const currentStep = activeQuest.steps[activeQuest.currentStep];
        if (currentStep && currentStep.id === stepId) {
            const updatedState = EffectsProcessor.updateQuestProgress(gameData.immerStateManager, gameState, character, stepId);
            // Сохраняем обновленное состояние
            gameData.gameData.games.set(updatedState.roomId, updatedState);

            if (updatedState.quests[character].active.currentStep >= updatedState.quests[character].active.steps.length) {
                // Квест завершён
                const completedState = completeQuestFn(updatedState, character);
                return { 
                    success: true, 
                    completed: true,
                    message: `Квест завершён: ${activeQuest.title}!`,
                    rewards: activeQuest.rewards,
                    gameState: completedState
                };
            } else {
                return { 
                    success: true, 
                    message: `Шаг квеста выполнен: ${currentStep.description}`,
                    nextStep: updatedState.quests[character].active.steps[updatedState.quests[character].active.currentStep],
                    gameState: updatedState
                };
            }
        }

        return { success: false, message: "Неверный шаг квеста" };
    }

    /**
     * Complete a quest for a character
     * @param {Object} gameState - Current game state
     * @param {string} character - Character completing the quest
     * @param {Object} EffectsProcessor - Effects processor
     * @param {Object} gameData - Game data manager
     * @returns {Object} Updated game state
     */
    static completeQuest(gameState, character, EffectsProcessor, gameData) {
        const activeQuest = gameState.quests[character].active;
        if (activeQuest) {
            const updatedState = EffectsProcessor.applyQuestCompletionEffects(gameData.immerStateManager, gameState, character, activeQuest);
            // Сохраняем обновленное состояние
            gameData.gameData.games.set(updatedState.roomId, updatedState);
            return updatedState;
        }
        return gameState;
    }

    /**
     * Get current active quest for a character
     * @param {Object} gameState - Current game state
     * @param {string} character - Character to check
     * @returns {Object|null} Active quest or null
     */
    static getCurrentQuest(gameState, character) {
        return gameState.quests[character].active;
    }

    /**
     * Get current quest step for a character
     * @param {Object} gameState - Current game state
     * @param {string} character - Character to check
     * @returns {Object|null} Current quest step or null
     */
    static getCurrentQuestStep(gameState, character) {
        const quest = QuestProcessor.getCurrentQuest(gameState, character);
        if (!quest || quest.currentStep >= quest.steps.length) {
            return null;
        }
        return quest.steps[quest.currentStep];
    }

    /**
     * Check if character can start a quest
     * @param {Object} gameState - Current game state
     * @param {string} character - Character to check
     * @param {string} questId - Quest identifier
     * @param {Object} QuestData - Quest data module
     * @returns {boolean} True if quest can be started
     */
    static canStartQuest(gameState, character, questId, QuestData) {
        const quest = QuestData.getQuest(questId);
        if (!quest || quest.character !== character) {
            return false;
        }

        // Проверяем, что нет активного квеста
        if (gameState.quests[character].active) {
            return false;
        }

        // Проверяем, что квест не был завершён ранее
        const completed = gameState.quests[character].completed;
        return !completed.some(q => q.id === questId);
    }

    /**
     * Process quest action from dialogue
     * @param {Object} gameState - Current game state
     * @param {string} character - Character performing action
     * @param {string} choiceId - Choice identifier
     * @param {Object} dialogueResult - Dialogue processing result
     * @param {Function} processQuestActionFn - Quest action handler function
     * @returns {Object} Quest action result
     */
    static processQuestAction(gameState, character, choiceId, dialogueResult, processQuestActionFn) {
        // Делегируем обработку в отдельный модуль
        return processQuestActionFn(gameState, character, choiceId, dialogueResult);
    }

    /**
     * Check if choice is quest-related
     * @param {string} choiceId - Choice identifier
     * @returns {boolean} True if quest-related choice
     */
    static isQuestChoice(choiceId) {
        // Quest choices typically start with quest_ or are handled in dialogue
        return choiceId.startsWith('quest_') || choiceId.includes('_quest_');
    }

    /**
     * Validate quest state for a character
     * @param {Object} gameState - Current game state
     * @param {string} character - Character to validate
     * @returns {Object} Validation result
     */
    static validateQuestState(gameState, character) {
        if (!gameState.quests || !gameState.quests[character]) {
            return { 
                valid: false, 
                error: `Quest state not found for character: ${character}` 
            };
        }
        
        return { valid: true };
    }

    /**
     * Get quest summary for a character
     * @param {Object} gameState - Current game state
     * @param {string} character - Character to check
     * @returns {Object} Quest summary
     */
    static getQuestSummary(gameState, character) {
        const activeQuest = QuestProcessor.getCurrentQuest(gameState, character);
        const completedQuests = gameState.quests[character].completed || [];
        
        return {
            active: activeQuest,
            completed: completedQuests.length,
            completedQuests: completedQuests,
            currentStep: activeQuest ? QuestProcessor.getCurrentQuestStep(gameState, character) : null
        };
    }
}

module.exports = QuestProcessor;