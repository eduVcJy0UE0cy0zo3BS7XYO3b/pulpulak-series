/**
 * QuestDataManager - Управление данными квестов
 * Отвечает за состояние квестов, их прогресс и завершение
 */

class QuestDataManager {
    constructor(gameDataManager) {
        this.gameData = gameDataManager;
        this.questData = gameDataManager.gameConfig.getQuestData();
    }

    /**
     * Получить данные квестов персонажа
     */
    getCharacterQuests(roomId, character) {
        const gameState = this.gameData.getGame(roomId);
        return gameState?.quests[character];
    }

    /**
     * Получить активный квест персонажа
     */
    getActiveQuest(roomId, character) {
        const questData = this.getCharacterQuests(roomId, character);
        return questData?.active;
    }

    /**
     * Получить завершённые квесты персонажа
     */
    getCompletedQuests(roomId, character) {
        const questData = this.getCharacterQuests(roomId, character);
        return questData?.completed || [];
    }

    /**
     * Проверить, есть ли активный квест
     */
    hasActiveQuest(roomId, character) {
        return this.getActiveQuest(roomId, character) !== null;
    }

    /**
     * Проверить, завершён ли квест
     */
    isQuestCompleted(roomId, character, questId) {
        const completed = this.getCompletedQuests(roomId, character);
        return completed.some(quest => quest.id === questId);
    }

    /**
     * Проверить, можно ли начать квест
     */
    canStartQuest(roomId, character, questId) {
        const quest = this.questData.getQuest(questId);
        
        // Проверяем, что квест существует и предназначен для данного персонажа
        if (!quest || quest.character !== character) {
            return false;
        }

        // Проверяем, что нет активного квеста
        if (this.hasActiveQuest(roomId, character)) {
            return false;
        }

        // Проверяем, что квест не был завершён ранее
        if (this.isQuestCompleted(roomId, character, questId)) {
            return false;
        }

        return true;
    }

    /**
     * Начать квест
     */
    startQuest(roomId, character, questId) {
        if (!this.canStartQuest(roomId, character, questId)) {
            return { success: false, message: "Нельзя начать этот квест" };
        }

        const quest = this.questData.createQuestInstance(questId);
        if (!quest) {
            return { success: false, message: "Квест не найден" };
        }

        const gameState = this.gameData.getGame(roomId);
        if (!gameState) {
            return { success: false, message: "Игра не найдена" };
        }

        // Устанавливаем активный квест
        gameState.quests[character].active = quest;
        
        // Отмечаем квест как взятый в глобальной памяти
        this.markQuestAsStarted(roomId, questId);

        return { 
            success: true, 
            message: `Начат квест: ${quest.title}`,
            quest: quest
        };
    }

    /**
     * Отметить квест как начатый в глобальной памяти
     */
    markQuestAsStarted(roomId, questId) {
        if (questId === 'princess_lost_relic') {
            this.gameData.updateGlobalQuestMemory(roomId, 'princess_lost_relic', true);
        } else if (questId === 'helper_secret_potion') {
            this.gameData.updateGlobalQuestMemory(roomId, 'helper_secret_potion', true);
        }
    }

    /**
     * Получить текущий шаг квеста
     */
    getCurrentQuestStep(roomId, character) {
        const quest = this.getActiveQuest(roomId, character);
        if (!quest || quest.currentStep >= quest.steps.length) {
            return null;
        }
        return quest.steps[quest.currentStep];
    }

    /**
     * Проверить, завершён ли текущий шаг
     */
    isCurrentStepCompleted(roomId, character, stepId) {
        const currentStep = this.getCurrentQuestStep(roomId, character);
        return currentStep && currentStep.id === stepId;
    }

    /**
     * Завершить текущий шаг квеста
     */
    completeQuestStep(roomId, character, stepId) {
        if (!this.isCurrentStepCompleted(roomId, character, stepId)) {
            return { success: false, message: "Неверный шаг квеста" };
        }

        const quest = this.getActiveQuest(roomId, character);
        const gameState = this.gameData.getGame(roomId);
        
        if (!quest || !gameState) {
            return { success: false, message: "Квест или игра не найдены" };
        }

        // Отмечаем шаг как завершённый
        quest.steps[quest.currentStep].completed = true;
        quest.currentStep++;

        // Проверяем, завершён ли весь квест
        if (quest.currentStep >= quest.steps.length) {
            return this.completeQuest(roomId, character);
        }

        const nextStep = quest.steps[quest.currentStep];
        return { 
            success: true, 
            message: `Шаг квеста выполнен: ${quest.steps[quest.currentStep - 1].description}`,
            nextStep: nextStep,
            completed: false
        };
    }

    /**
     * Завершить квест
     */
    completeQuest(roomId, character) {
        const gameState = this.gameData.getGame(roomId);
        if (!gameState) {
            return { success: false, message: "Игра не найдена" };
        }

        const activeQuest = gameState.quests[character].active;
        if (!activeQuest) {
            return { success: false, message: "Нет активного квеста" };
        }

        // Добавляем награды в инвентарь (если есть PlayerDataManager)
        if (activeQuest.rewards) {
            activeQuest.rewards.forEach(reward => {
                gameState.stats[character].inventory.push(reward);
            });
        }

        // Перемещаем квест в завершённые
        gameState.quests[character].completed.push(activeQuest);
        gameState.quests[character].active = null;
        
        // Обновляем глобальную память квестов
        this.markQuestAsCompleted(roomId, activeQuest.id);

        return { 
            success: true, 
            completed: true,
            message: `Квест завершён: ${activeQuest.title}!`,
            rewards: activeQuest.rewards
        };
    }

    /**
     * Отметить квест как завершённый в глобальной памяти
     */
    markQuestAsCompleted(roomId, questId) {
        if (questId === 'princess_lost_relic') {
            this.gameData.updateGlobalQuestMemory(roomId, 'princess_lost_relic', true);
        } else if (questId === 'helper_secret_potion') {
            this.gameData.updateGlobalQuestMemory(roomId, 'helper_secret_potion', true);
        }
    }

    /**
     * Получить глобальную память квестов
     */
    getGlobalQuestMemory(roomId) {
        const gameState = this.gameData.getGame(roomId);
        return gameState?.globalQuestMemory || {};
    }

    /**
     * Проверить, взят ли квест глобально
     */
    isQuestTakenGlobally(roomId, questId) {
        const globalMemory = this.getGlobalQuestMemory(roomId);
        return globalMemory[questId] === true;
    }

    /**
     * Получить статистику квестов для персонажа
     */
    getQuestStats(roomId, character) {
        const questData = this.getCharacterQuests(roomId, character);
        if (!questData) return null;

        return {
            active: questData.active,
            completed: questData.completed.length,
            completedQuests: questData.completed.map(q => q.title)
        };
    }

    /**
     * Получить все доступные квесты для персонажа
     */
    getAvailableQuests(roomId, character) {
        const allQuests = this.questData.getAllQuests();
        const availableQuests = [];

        for (const quest of allQuests) {
            if (this.canStartQuest(roomId, character, quest.id)) {
                availableQuests.push(quest);
            }
        }

        return availableQuests;
    }

    /**
     * Сбросить все квесты персонажа (для тестов)
     */
    resetCharacterQuests(roomId, character) {
        const gameState = this.gameData.getGame(roomId);
        if (gameState?.quests[character]) {
            gameState.quests[character] = {
                active: null,
                completed: []
            };
        }
    }

    /**
     * Сбросить глобальную память квестов (для тестов)
     */
    resetGlobalQuestMemory(roomId) {
        const gameState = this.gameData.getGame(roomId);
        if (gameState) {
            gameState.globalQuestMemory = {};
        }
    }
}

module.exports = QuestDataManager;