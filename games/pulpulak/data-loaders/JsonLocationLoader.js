/**
 * Загрузчик данных локаций из JSON файлов
 */

const fs = require('fs');
const path = require('path');

class JsonLocationLoader {
    constructor(dataPath) {
        this.dataPath = dataPath || path.join(__dirname, '../data-json/locationData.json');
        this._locations = null;
    }

    /**
     * Загружает данные из JSON файла
     */
    async _loadData() {
        if (this._locations !== null) {
            return this._locations;
        }

        try {
            const data = await fs.promises.readFile(this.dataPath, 'utf8');
            this._locations = JSON.parse(data);
            return this._locations;
        } catch (error) {
            console.error('Failed to load location data from JSON:', error);
            throw new Error(`Cannot load location data: ${error.message}`);
        }
    }

    /**
     * Получить локацию по ID
     */
    async getLocation(locationId) {
        const locations = await this._loadData();
        return locations[locationId] || null;
    }

    /**
     * Получить все ID локаций
     */
    async getAllLocations() {
        const locations = await this._loadData();
        return Object.keys(locations);
    }

    /**
     * Получить соединения локации
     */
    async getConnections(locationId) {
        const location = await this.getLocation(locationId);
        return location ? location.connections : [];
    }

    /**
     * Проверить, можно ли сменить одежду в локации
     */
    async canChangeOutfit(locationId) {
        const location = await this.getLocation(locationId);
        return location ? location.canChangeOutfit : false;
    }

    /**
     * Получить полную информацию о локации
     */
    async getLocationInfo(locationId) {
        const locations = await this._loadData();
        const location = locations[locationId];
        
        if (!location) return null;
        
        return {
            id: locationId,
            name: location.name,
            description: location.description,
            icon: location.icon,
            canChangeOutfit: location.canChangeOutfit,
            connections: await Promise.all(
                location.connections.map(async connId => ({
                    id: connId,
                    name: locations[connId]?.name || connId,
                    icon: locations[connId]?.icon || '❓'
                }))
            )
        };
    }

    /**
     * Синхронные методы для совместимости
     */
    static getLocation(locationId) {
        console.warn('Using deprecated synchronous getLocation method. Use async version instead.');
        const loader = new JsonLocationLoader();
        return loader.getLocation(locationId);
    }

    static getAllLocations() {
        console.warn('Using deprecated synchronous getAllLocations method. Use async version instead.');
        const loader = new JsonLocationLoader();
        return loader.getAllLocations();
    }

    static getConnections(locationId) {
        console.warn('Using deprecated synchronous getConnections method. Use async version instead.');
        const loader = new JsonLocationLoader();
        return loader.getConnections(locationId);
    }

    static canChangeOutfit(locationId) {
        console.warn('Using deprecated synchronous canChangeOutfit method. Use async version instead.');
        const loader = new JsonLocationLoader();
        return loader.canChangeOutfit(locationId);
    }

    static getLocationInfo(locationId) {
        console.warn('Using deprecated synchronous getLocationInfo method. Use async version instead.');
        const loader = new JsonLocationLoader();
        return loader.getLocationInfo(locationId);
    }

    /**
     * Валидация структуры локации
     */
    static validateLocation(location) {
        const required = ['name', 'description', 'connections', 'canChangeOutfit', 'icon'];
        
        for (const field of required) {
            if (!(field in location)) {
                return { valid: false, error: `Missing required field: ${field}` };
            }
        }

        if (!Array.isArray(location.connections)) {
            return { valid: false, error: 'Connections must be an array' };
        }

        if (typeof location.canChangeOutfit !== 'boolean') {
            return { valid: false, error: 'canChangeOutfit must be a boolean' };
        }

        return { valid: true };
    }

    /**
     * Валидация всех загруженных данных
     */
    async validateAllData() {
        const locations = await this._loadData();
        const errors = [];

        for (const [locationId, location] of Object.entries(locations)) {
            const validation = JsonLocationLoader.validateLocation(location);
            if (!validation.valid) {
                errors.push(`Location ${locationId}: ${validation.error}`);
            }
            
            // Проверяем что все соединения указывают на существующие локации
            for (const connId of location.connections || []) {
                if (!locations[connId]) {
                    errors.push(`Location ${locationId}: Invalid connection to non-existent location ${connId}`);
                }
            }
        }

        return {
            valid: errors.length === 0,
            errors
        };
    }
}

module.exports = JsonLocationLoader;