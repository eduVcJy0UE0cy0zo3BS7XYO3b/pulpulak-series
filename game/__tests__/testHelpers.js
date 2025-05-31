/**
 * Вспомогательные функции для тестирования с Immer
 */

/**
 * Получить текущее состояние игры из gameLogic
 * @param {CoopGameLogic} gameLogic 
 * @param {string} roomId 
 * @returns {Object} Актуальное состояние игры
 */
function getCurrentGameState(gameLogic, roomId) {
    return gameLogic.games.get(roomId);
}

/**
 * Обновить переменную gameState в тесте
 * @param {CoopGameLogic} gameLogic 
 * @param {string} roomId 
 * @returns {Object} Актуальное состояние игры
 */
function refreshGameState(gameLogic, roomId) {
    return gameLogic.games.get(roomId);
}

/**
 * Выполнить операцию и получить обновленное состояние
 * @param {Function} operation - Операция для выполнения
 * @param {CoopGameLogic} gameLogic 
 * @param {string} roomId 
 * @returns {Object} { result, gameState }
 */
function executeAndRefresh(operation, gameLogic, roomId) {
    const result = operation();
    const gameState = gameLogic.games.get(roomId);
    return { result, gameState };
}

module.exports = {
    getCurrentGameState,
    refreshGameState,
    executeAndRefresh
};