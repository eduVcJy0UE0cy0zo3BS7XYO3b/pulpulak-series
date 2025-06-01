/**
 * Request Processing utility extracted from CoopGameLogic
 * Handles all request management and processing (outfit swaps, etc.)
 */

class RequestProcessor {
    /**
     * Create an outfit swap request
     * @param {string} roomId - Room identifier
     * @param {string} fromPlayerId - Player making the request
     * @param {string} fromCharacter - Character making the request
     * @param {Object} requestData - Request data manager
     * @returns {Object} Request creation result
     */
    static createOutfitSwapRequest(roomId, fromPlayerId, fromCharacter, requestData) {
        return requestData.createRequest(roomId, 'outfit_swap', fromPlayerId, fromCharacter);
    }

    /**
     * Respond to an outfit swap request
     * @param {string} roomId - Room identifier
     * @param {string} playerId - Player responding
     * @param {boolean} accepted - Whether request is accepted
     * @param {Object} requestData - Request data manager
     * @returns {Object} Response result
     */
    static respondToOutfitSwapRequest(roomId, playerId, accepted, requestData) {
        return requestData.respondToRequest(roomId, playerId, accepted);
    }

    /**
     * Create a request of any type
     * @param {string} roomId - Room identifier
     * @param {string} requestType - Type of request
     * @param {string} fromPlayerId - Player making the request
     * @param {string} fromCharacter - Character making the request
     * @param {Object} requestData - Request data manager
     * @param {Object} additionalData - Additional request data
     * @returns {Object} Request creation result
     */
    static createRequest(roomId, requestType, fromPlayerId, fromCharacter, requestData, additionalData = {}) {
        return requestData.createRequest(roomId, requestType, fromPlayerId, fromCharacter, additionalData);
    }

    /**
     * Respond to a request of any type
     * @param {string} roomId - Room identifier
     * @param {string} playerId - Player responding
     * @param {boolean} accepted - Whether request is accepted
     * @param {Object} requestData - Request data manager
     * @param {Object} responseData - Response data
     * @returns {Object} Response result
     */
    static respondToRequest(roomId, playerId, accepted, requestData, responseData = {}) {
        return requestData.respondToRequest(roomId, playerId, accepted, responseData);
    }

    /**
     * Get active outfit request for a room
     * @param {string} roomId - Room identifier
     * @param {Object} requestData - Request data manager
     * @returns {Object|null} Active request or null
     */
    static getActiveOutfitRequest(roomId, requestData) {
        return requestData.getActiveRequest(roomId);
    }

    /**
     * Get active request of any type for a room
     * @param {string} roomId - Room identifier
     * @param {Object} requestData - Request data manager
     * @returns {Object|null} Active request or null
     */
    static getActiveRequest(roomId, requestData) {
        return requestData.getActiveRequest(roomId);
    }

    /**
     * Cancel outfit request for a room
     * @param {string} roomId - Room identifier
     * @param {Object} requestData - Request data manager
     */
    static cancelOutfitRequest(roomId, requestData) {
        requestData.cancelRequest(roomId);
    }

    /**
     * Cancel any request for a room
     * @param {string} roomId - Room identifier
     * @param {Object} requestData - Request data manager
     * @returns {boolean} True if request was cancelled
     */
    static cancelRequest(roomId, requestData) {
        return requestData.cancelRequest(roomId);
    }

    /**
     * Check if player can switch outfits
     * @param {Object} gameState - Current game state
     * @param {string} character - Character to check
     * @returns {boolean} True if can switch outfits
     */
    static canSwitchOutfits(gameState, character) {
        // Делегируем проверку в конкретную игру через GameConfig
        const PulpulakGameConfig = require('../../games/pulpulak/PulpulakGameConfig');
        const gameConfig = new PulpulakGameConfig();
        
        if (typeof gameConfig.canSwitchOutfits === 'function') {
            return gameConfig.canSwitchOutfits(gameState, character);
        }
        
        // Fallback - разрешаем переодевание
        return true;
    }

    /**
     * Validate request type
     * @param {string} requestType - Type to validate
     * @returns {boolean} True if valid request type
     */
    static isValidRequestType(requestType) {
        const validTypes = ['outfit_swap', 'quest_help', 'item_trade', 'joint_action'];
        return validTypes.includes(requestType);
    }

    /**
     * Check if room has active request
     * @param {string} roomId - Room identifier
     * @param {Object} requestData - Request data manager
     * @returns {boolean} True if has active request
     */
    static hasActiveRequest(roomId, requestData) {
        return requestData.hasActiveRequest(roomId);
    }

    /**
     * Get request type description
     * @param {string} requestType - Request type
     * @returns {string} Human-readable description
     */
    static getRequestTypeDescription(requestType) {
        const descriptions = {
            'outfit_swap': 'Обмен одеждой',
            'quest_help': 'Помощь с квестом',
            'item_trade': 'Обмен предметами',
            'joint_action': 'Совместное действие'
        };
        return descriptions[requestType] || 'Неизвестный запрос';
    }

    /**
     * Validate request participants
     * @param {Object} gameState - Current game state
     * @param {string} fromPlayerId - Player making request
     * @param {string} fromCharacter - Character making request
     * @returns {Object} Validation result
     */
    static validateRequestParticipants(gameState, fromPlayerId, fromCharacter) {
        const playerCharacter = gameState.players[fromCharacter];
        if (!playerCharacter || playerCharacter.id !== fromPlayerId) {
            return { 
                valid: false, 
                error: "Неверный игрок или персонаж" 
            };
        }

        // Check if target player exists
        const targetCharacter = fromCharacter === 'princess' ? 'helper' : 'princess';
        const targetPlayer = gameState.players[targetCharacter];
        if (!targetPlayer) {
            return { 
                valid: false, 
                error: "Целевой игрок не найден" 
            };
        }

        return { 
            valid: true, 
            targetCharacter, 
            targetPlayer 
        };
    }
}

module.exports = RequestProcessor;