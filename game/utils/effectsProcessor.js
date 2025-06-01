/**
 * Effects processing utility extracted from CoopGameLogic
 * Handles applying choice effects to game state
 */

class EffectsProcessor {
    /**
     * Apply effects to game state using Immer state manager
     * @param {Object} immerStateManager - The Immer state manager instance
     * @param {Object} gameState - Current game state
     * @param {Object} effects - Effects to apply
     * @param {string} character - Character to apply effects to
     * @param {Function} getNPCsForLocation - Function to get NPCs for location
     * @returns {Object} Updated game state
     */
    static applyEffects(immerStateManager, gameState, effects, character, getNPCsForLocation) {
        return immerStateManager.updateState(gameState, draft => {
            if (effects.outfit) {
                draft.stats[character].outfit = effects.outfit;
            }
            if (effects.location) {
                draft.stats[character].location = effects.location;
                draft.stats[character].npcsPresent = getNPCsForLocation(effects.location, gameState, character);
            }
            if (effects.awareness) {
                draft.stats[character].awareness += effects.awareness;
            }
        });
    }

    /**
     * Apply NPC dialogue choice effects to game state
     * @param {Object} immerStateManager - The Immer state manager instance
     * @param {Object} gameState - Current game state
     * @param {string} character - Character performing the action
     * @param {Object} effects - Effects from dialogue choice
     * @returns {Object} Updated game state
     */
    static applyNPCDialogueEffects(immerStateManager, gameState, character, effects) {
        return immerStateManager.updateState(gameState, draft => {
            if (effects.item) {
                draft.stats[character].inventory.push(effects.item);
            }
            if (effects.info) {
                draft.stats[character][effects.info] = true;
            }
        });
    }

    /**
     * Apply quest completion effects
     * @param {Object} immerStateManager - The Immer state manager instance
     * @param {Object} gameState - Current game state
     * @param {string} character - Character completing the quest
     * @param {Object} activeQuest - The quest being completed
     * @returns {Object} Updated game state
     */
    static applyQuestCompletionEffects(immerStateManager, gameState, character, activeQuest) {
        return immerStateManager.updateState(gameState, draft => {
            // Добавляем награды в инвентарь
            if (activeQuest.rewards) {
                activeQuest.rewards.forEach(reward => {
                    draft.stats[character].inventory.push(reward);
                });
            }

            // Перемещаем квест в завершённые
            draft.quests[character].completed.push(activeQuest);
            draft.quests[character].active = null;
            
            // Обновляем глобальную память квестов
            if (activeQuest.id === 'princess_lost_relic') {
                draft.globalQuestMemory.princess_lost_relic = true;
            } else if (activeQuest.id === 'helper_secret_potion') {
                draft.globalQuestMemory.helper_secret_potion = true;
            }
        });
    }

    /**
     * Update quest progress
     * @param {Object} immerStateManager - The Immer state manager instance
     * @param {Object} gameState - Current game state
     * @param {string} character - Character performing the quest action
     * @param {string} stepId - Quest step ID
     * @returns {Object} Updated game state
     */
    static updateQuestProgress(immerStateManager, gameState, character, stepId) {
        const activeQuest = gameState.quests[character].active;
        if (!activeQuest) {
            return gameState;
        }

        const currentStep = activeQuest.steps[activeQuest.currentStep];
        if (currentStep && currentStep.id === stepId) {
            return immerStateManager.updateState(gameState, draft => {
                const draftQuest = draft.quests[character].active;
                draftQuest.steps[draftQuest.currentStep].completed = true;
                draftQuest.currentStep++;
            });
        }

        return gameState;
    }

    /**
     * Start a new quest
     * @param {Object} immerStateManager - The Immer state manager instance
     * @param {Object} gameState - Current game state
     * @param {string} character - Character starting the quest
     * @param {Object} quest - Quest instance to start
     * @returns {Object} Updated game state
     */
    static startQuest(immerStateManager, gameState, character, quest) {
        return immerStateManager.updateState(gameState, draft => {
            draft.quests[character].active = quest;
            
            // Сразу отмечаем квест как взятый в глобальной памяти
            if (quest.id === 'princess_lost_relic') {
                draft.globalQuestMemory.princess_lost_relic = true;
            } else if (quest.id === 'helper_secret_potion') {
                draft.globalQuestMemory.helper_secret_potion = true;
            }
        });
    }
}

module.exports = EffectsProcessor;