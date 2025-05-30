class SceneHandler {
    constructor(locationData, npcData) {
        this.locationData = locationData;
        this.npcData = npcData;
    }

    validateMovement(gameState, character, newLocation) {
        const currentLocation = gameState.stats[character].location;
        const locationInfo = this.locationData.getLocation(currentLocation);
        
        if (!locationInfo || !locationInfo.connections) {
            return { valid: false, reason: "Текущая локация не найдена" };
        }

        const connection = locationInfo.connections.find(conn => conn.to === newLocation);
        if (!connection) {
            return { valid: false, reason: "Нет прямого пути к этой локации" };
        }

        // Проверяем требования для перехода
        if (connection.requirements) {
            for (const req of connection.requirements) {
                if (req.type === 'outfit' && gameState.stats[character].outfit !== req.value) {
                    return { 
                        valid: false, 
                        reason: `Для прохода требуется ${req.value === 'noble' ? 'парадная' : 'простая'} одежда` 
                    };
                }
                if (req.type === 'item' && !this.hasItem(gameState, character, req.value)) {
                    return { valid: false, reason: `Требуется предмет: ${req.value}` };
                }
            }
        }

        return { valid: true };
    }

    moveCharacter(gameState, character, newLocation) {
        const validation = this.validateMovement(gameState, character, newLocation);
        if (!validation.valid) {
            return { success: false, message: validation.reason };
        }

        const oldLocation = gameState.stats[character].location;
        gameState.stats[character].location = newLocation;
        
        // Обновляем список NPC в новой локации
        gameState.stats[character].npcsPresent = this.getNPCsForLocation(newLocation, gameState, character);

        return {
            success: true,
            message: `${character} перешёл из ${oldLocation} в ${newLocation}`,
            oldLocation,
            newLocation,
            npcsPresent: gameState.stats[character].npcsPresent
        };
    }

    getNPCsForLocation(location, gameState, character) {
        try {
            // Используем существующую логику NPCData для совместимости
            const npcs = this.npcData.getNPCsForLocation(location, gameState, character);
            // Возвращаем только имена для обратной совместимости
            return npcs.map(npc => npc.name);
        } catch (error) {
            console.error(`Error getting NPCs for location ${location}:`, error);
            return [];
        }
    }

    checkTimeCondition(condition, gameState) {
        // Реализация проверки временных условий
        // Пока что возвращаем true для всех временных условий
        return true;
    }

    hasItem(gameState, character, itemId) {
        return gameState.inventory?.[character]?.includes(itemId) || false;
    }

    getAvailableLocations(gameState, character) {
        const currentLocation = gameState.stats[character].location;
        const locationInfo = this.locationData.getLocation(currentLocation);
        
        if (!locationInfo || !locationInfo.connections) {
            return [];
        }

        return locationInfo.connections
            .filter(connection => {
                const validation = this.validateMovement(gameState, character, connection.to);
                return validation.valid;
            })
            .map(connection => ({
                id: connection.to,
                name: this.locationData.getLocation(connection.to)?.name || connection.to,
                description: connection.description
            }));
    }
}

module.exports = SceneHandler;