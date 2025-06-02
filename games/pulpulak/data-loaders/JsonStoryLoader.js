/**
 * Загрузчик данных историй из JSON файлов
 */

const fs = require('fs');
const path = require('path');

class JsonStoryLoader {
    constructor(dataPath) {
        this.dataPath = dataPath || path.join(__dirname, '../data-json/storyData.json');
        this._scenes = null;
    }

    /**
     * Загружает данные из JSON файла
     */
    async _loadData() {
        if (this._scenes !== null) {
            return this._scenes;
        }

        try {
            const data = await fs.promises.readFile(this.dataPath, 'utf8');
            this._scenes = JSON.parse(data);
            return this._scenes;
        } catch (error) {
            console.error('Failed to load story data from JSON:', error);
            throw new Error(`Cannot load story data: ${error.message}`);
        }
    }

    /**
     * Получить сцену по ID
     */
    async getScene(sceneId) {
        const scenes = await this._loadData();
        return scenes[sceneId] || null;
    }

    /**
     * Получить все ID сцен
     */
    async getAllScenes() {
        const scenes = await this._loadData();
        return Object.keys(scenes);
    }

    /**
     * Получить все сцены
     */
    async getAllScenesData() {
        return await this._loadData();
    }

    /**
     * Синхронные методы для совместимости с существующим API
     */
    static getScene(sceneId) {
        // Для обратной совместимости - будет удалено после полной миграции
        console.warn('Using deprecated synchronous getScene method. Use async version instead.');
        const loader = new JsonStoryLoader();
        // Возвращаем Promise для асинхронной загрузки
        return loader.getScene(sceneId);
    }

    static getAllScenes() {
        console.warn('Using deprecated synchronous getAllScenes method. Use async version instead.');
        const loader = new JsonStoryLoader();
        return loader.getAllScenes();
    }

    /**
     * Валидация структуры сцены
     */
    static validateScene(scene) {
        const required = ['title', 'text', 'location', 'choices'];
        
        for (const field of required) {
            if (!(field in scene)) {
                return { valid: false, error: `Missing required field: ${field}` };
            }
        }

        if (typeof scene.choices !== 'object') {
            return { valid: false, error: 'Choices must be an object' };
        }

        const roles = ['princess', 'helper'];
        for (const role of roles) {
            if (scene.choices[role] && !Array.isArray(scene.choices[role])) {
                return { valid: false, error: `Choices for ${role} must be an array` };
            }
        }

        return { valid: true };
    }

    /**
     * Валидация всех загруженных данных
     */
    async validateAllData() {
        const scenes = await this._loadData();
        const errors = [];

        for (const [sceneId, scene] of Object.entries(scenes)) {
            const validation = JsonStoryLoader.validateScene(scene);
            if (!validation.valid) {
                errors.push(`Scene ${sceneId}: ${validation.error}`);
            }
        }

        return {
            valid: errors.length === 0,
            errors
        };
    }
}

module.exports = JsonStoryLoader;