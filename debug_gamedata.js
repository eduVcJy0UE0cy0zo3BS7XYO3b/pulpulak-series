/**
 * Отладка структуры данных
 */

const LegacyGameAdapter = require('./engine/adapters/LegacyGameAdapter');
const PulpulakGameConfig = require('./games/pulpulak/PulpulakGameConfig');

console.log('=== ОТЛАДКА СТРУКТУРЫ ДАННЫХ ===\n');

// Создаем адаптер
const gameConfig = new PulpulakGameConfig();
const gameLogic = new LegacyGameAdapter(gameConfig);

// Создаем игру
const players = {
    princess: { id: 'socket_123', name: 'Тест Княжна' },
    helper: { id: 'socket_456', name: 'Тест Помощница' }
};

console.log('1. Создание игры...');
const gameData = gameLogic.startGame('debug_test', players);

console.log('2. Структура возвращенных данных:');
console.log('Ключи верхнего уровня:', Object.keys(gameData));

console.log('\n3. Детальная структура:');
console.log('gameData.players:', gameData.players);
console.log('gameData.stats:', Object.keys(gameData.stats || {}));
console.log('gameData.scene:', gameData.scene?.title);
console.log('gameData.choices:', Object.keys(gameData.choices || {}));
console.log('gameData.turnOrder:', gameData.turnOrder);

console.log('\n4. Проверим внутреннее состояние движка:');
const gameState = gameLogic.engine.getGame('debug_test');
console.log('gameState.players:', gameState?.players);
console.log('gameState.stats:', Object.keys(gameState?.stats || {}));

console.log('\n5. Проверим прямой вызов engine.getGameData:');
const directData = gameLogic.engine.getGameData('debug_test');
console.log('directData keys:', Object.keys(directData));
console.log('directData.players:', directData.players);

// Очистка
gameLogic.removeGame('debug_test');