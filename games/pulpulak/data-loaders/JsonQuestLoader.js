/**
 * Загрузчик данных квестов из JSON файлов
 */

const fs = require('fs');
const path = require('path');

class JsonQuestLoader {
    constructor(dataPath) {
        this.dataPath = dataPath || path.join(__dirname, '../data-json/questData.json');
        this._quests = null;
    }

    /**
     * Загружает данные из JSON файла
     */
    async _loadData() {
        if (this._quests !== null) {
            return this._quests;
        }

        try {
            const data = await fs.promises.readFile(this.dataPath, 'utf8');
            this._quests = JSON.parse(data);
            return this._quests;
        } catch (error) {
            console.error('Failed to load quest data from JSON:', error);
            throw new Error(`Cannot load quest data: ${error.message}`);
        }
    }

    /**
     * Получить квест по ID
     */
    async getQuest(questId) {
        const quests = await this._loadData();
        return quests[questId] || null;
    }

    /**
     * Получить квест для персонажа
     */
    async getQuestForCharacter(character) {
        const questKey = character === 'princess' ? 'princess_lost_relic' : 'helper_secret_potion';
        return await this.getQuest(questKey);
    }

    /**
     * Получить все квесты
     */
    async getAllQuests() {
        return await this._loadData();
    }

    /**
     * Создать экземпляр квеста
     */
    async createQuestInstance(questId) {
        const questTemplate = await this.getQuest(questId);
        if (!questTemplate) return null;

        // Создаем глубокую копию квеста для экземпляра игры
        return JSON.parse(JSON.stringify(questTemplate));
    }

    /**
     * Получить квесты по персонажу
     */
    async getQuestsByCharacter(character) {
        const quests = await this._loadData();
        const result = {};
        
        for (const [questId, quest] of Object.entries(quests)) {
            if (quest.character === character) {
                result[questId] = quest;
            }
        }
        
        return result;
    }

    /**
     * Проверить завершенность квеста
     */
    async isQuestCompleted(questInstance) {
        if (!questInstance || !questInstance.steps) {
            return false;
        }
        
        return questInstance.steps.every(step => step.completed);
    }

    /**
     * Получить текущий шаг квеста
     */
    async getCurrentQuestStep(questInstance) {
        if (!questInstance || !questInstance.steps) {
            return null;
        }
        
        const currentStepIndex = questInstance.currentStep || 0;
        return questInstance.steps[currentStepIndex] || null;
    }

    /**
     * Продвинуть квест на следующий шаг
     */
    async advanceQuest(questInstance) {
        if (!questInstance) return false;
        
        const currentStep = await this.getCurrentQuestStep(questInstance);
        if (currentStep) {
            currentStep.completed = true;
            questInstance.currentStep = (questInstance.currentStep || 0) + 1;
            return true;
        }
        
        return false;
    }

    /**
     * Синхронные методы для совместимости
     */
    static getQuest(questId) {
        console.warn('Using deprecated synchronous getQuest method. Use async version instead.');
        const loader = new JsonQuestLoader();
        return loader.getQuest(questId);
    }

    static getQuestForCharacter(character) {
        console.warn('Using deprecated synchronous getQuestForCharacter method. Use async version instead.');
        const loader = new JsonQuestLoader();
        return loader.getQuestForCharacter(character);
    }

    static getAllQuests() {
        console.warn('Using deprecated synchronous getAllQuests method. Use async version instead.');
        const loader = new JsonQuestLoader();
        return loader.getAllQuests();
    }

    static createQuestInstance(questId) {
        console.warn('Using deprecated synchronous createQuestInstance method. Use async version instead.');
        const loader = new JsonQuestLoader();
        return loader.createQuestInstance(questId);
    }

    /**
     * Валидация структуры квеста
     */
    static validateQuest(quest) {
        const required = ['id', 'character', 'title', 'description', 'steps', 'currentStep', 'rewards'];
        
        for (const field of required) {
            if (!(field in quest)) {
                return { valid: false, error: `Missing required field: ${field}` };
            }
        }

        if (!['princess', 'helper'].includes(quest.character)) {
            return { valid: false, error: 'character must be either "princess" or "helper"' };
        }

        if (!Array.isArray(quest.steps)) {
            return { valid: false, error: 'steps must be an array' };
        }

        if (!Array.isArray(quest.rewards)) {
            return { valid: false, error: 'rewards must be an array' };
        }

        if (typeof quest.currentStep !== 'number' || quest.currentStep < 0) {
            return { valid: false, error: 'currentStep must be a non-negative number' };
        }

        // Валидируем каждый шаг
        for (let i = 0; i < quest.steps.length; i++) {
            const step = quest.steps[i];
            const stepRequired = ['id', 'description', 'location', 'npc', 'completed'];
            
            for (const field of stepRequired) {
                if (!(field in step)) {
                    return { valid: false, error: `Step ${i}: Missing required field: ${field}` };
                }
            }
            
            if (typeof step.completed !== 'boolean') {
                return { valid: false, error: `Step ${i}: completed must be a boolean` };
            }
        }

        return { valid: true };
    }

    /**
     * Валидация всех загруженных данных
     */
    async validateAllData() {
        const quests = await this._loadData();
        const errors = [];

        for (const [questId, quest] of Object.entries(quests)) {
            const validation = JsonQuestLoader.validateQuest(quest);
            if (!validation.valid) {
                errors.push(`Quest ${questId}: ${validation.error}`);
            }
        }

        return {
            valid: errors.length === 0,
            errors
        };
    }
}

module.exports = JsonQuestLoader;