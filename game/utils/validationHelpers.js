/**
 * Validation utility functions extracted from CoopGameLogic
 * Small, pure functions that don't depend on game state
 */

class ValidationHelpers {
    /**
     * Validate game start parameters
     */
    static validateGameStartParameters(roomId, players) {
        if (!roomId || typeof roomId !== 'string') {
            throw new Error('Неверный ID комнаты');
        }
        if (!players || !players.princess || !players.helper) {
            throw new Error('Недостаточно игроков для начала игры');
        }
    }

    /**
     * Validate that player controls the specified character
     */
    static validatePlayer(gameState, playerId, character) {
        const playerCharacter = gameState.players[character];
        return playerCharacter && playerCharacter.id === playerId;
    }

    /**
     * Validate game state exists
     */
    static validateGameState(gameState) {
        return gameState ? { valid: true, gameState } : { valid: false, error: 'Игра не найдена' };
    }

    /**
     * Validate turn order for specific actions
     */
    static validateTurn(gameState, character, choiceId) {
        // Import processors here to avoid circular dependency
        const MovementProcessor = require('./movementProcessor');
        const NPCDialogueProcessor = require('./npcDialogueProcessor');
        
        const isMovement = MovementProcessor.isMovementChoice(choiceId);
        const isNPCInteraction = NPCDialogueProcessor.isNPCInteractionChoice(choiceId);
        const isSpecialAction = choiceId === 'request_outfit_swap';
        
        // Специальные действия не требуют проверки хода
        if (isMovement || isNPCInteraction || isSpecialAction) {
            return true;
        }
        
        return gameState.turnOrder === character;
    }

    /**
     * Generate unique request ID
     */
    static generateRequestId() {
        return Math.random().toString(36).substring(2, 15);
    }
}

module.exports = ValidationHelpers;