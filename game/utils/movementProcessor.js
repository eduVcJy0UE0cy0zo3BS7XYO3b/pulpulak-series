/**
 * Movement processing utility extracted from CoopGameLogic
 * Handles character movement between locations
 */

class MovementProcessor {
    /**
     * Process character movement between locations
     * @param {Object} gameState - Current game state
     * @param {string} targetLocation - Target location ID
     * @param {string} character - Character being moved
     * @param {Object} LocationData - Location data module
     * @param {Object} requestData - Request data manager
     * @param {Object} playerData - Player data manager
     * @param {Object} gameData - Game data manager
     * @returns {Object} Movement result
     */
    static processMovement(gameState, targetLocation, character, LocationData, requestData, playerData, gameData) {
        const characterStats = gameState.stats[character];
        
        // Проверяем, что целевая локация доступна из текущей
        const currentConnections = LocationData.getConnections(characterStats.location);
        if (!currentConnections.includes(targetLocation)) {
            return { 
                success: false, 
                message: "Вы не можете попасть туда отсюда" 
            };
        }

        // Проверяем, что локация существует
        const locationInfo = LocationData.getLocation(targetLocation);
        if (!locationInfo) {
            return { 
                success: false, 
                message: "Неизвестная локация" 
            };
        }

        // Отменяем активные запросы при перемещении любого персонажа
        if (requestData.hasActiveRequest(gameState.roomId)) {
            requestData.cancelRequest(gameState.roomId);
        }

        // Перемещаем конкретного персонажа через PlayerDataManager
        playerData.updateLocation(gameState.roomId, character, targetLocation);
        const updatedGameState = gameData.getGame(gameState.roomId);

        // НЕ меняем очередь хода при перемещении
        // Это позволяет игрокам свободно перемещаться

        return { 
            success: true, 
            message: `${character === 'princess' ? 'Княжна' : 'Помощница'} переместилась в ${locationInfo.name}`,
            gameState: updatedGameState
        };
    }

    /**
     * Validate movement request
     * @param {string} choiceId - Choice ID
     * @returns {Object} Validation result with extracted location
     */
    static validateMovementChoice(choiceId) {
        if (!choiceId.startsWith('move_to_')) {
            return { isMovement: false };
        }
        
        const targetLocation = choiceId.replace('move_to_', '');
        return { 
            isMovement: true, 
            targetLocation: targetLocation 
        };
    }

    /**
     * Check if choice is a movement action
     * @param {string} choiceId - Choice ID to check
     * @returns {boolean} True if movement choice
     */
    static isMovementChoice(choiceId) {
        return choiceId.startsWith('move_to_');
    }
}

module.exports = MovementProcessor;