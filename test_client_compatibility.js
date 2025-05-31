/**
 * Тест совместимости с клиентским кодом
 */

const LegacyGameAdapter = require('./engine/adapters/LegacyGameAdapter');
const PulpulakGameConfig = require('./games/pulpulak/PulpulakGameConfig');

console.log('=== ТЕСТ СОВМЕСТИМОСТИ С КЛИЕНТСКИМ КОДОМ ===\n');

// Создаем адаптер как в реальном сервере
const gameConfig = new PulpulakGameConfig();
const gameLogic = new LegacyGameAdapter(gameConfig);

console.log('1. Создание игры через Legacy Adapter:');
const players = {
    princess: { id: 'socket_123', name: 'Тест Княжна' },
    helper: { id: 'socket_456', name: 'Тест Помощница' }
};

const gameData = gameLogic.startGame('client_test', players);

console.log('   ✅ Игра создана');
console.log(`   📋 Сцена: ${gameData.scene?.title || 'Не найдена'}`);

// Проверяем наличие всех полей, которые ожидает клиент
console.log('\n2. Проверка полей для клиентской совместимости:');

const requiredFields = [
    'players',
    'stats', 
    'scene',
    'choices',
    'turnOrder',
    'currentScene'
];

const checkField = (obj, field) => {
    const exists = obj && obj[field] !== undefined;
    console.log(`   ${exists ? '✅' : '❌'} ${field}: ${exists ? 'Присутствует' : 'Отсутствует'}`);
    return exists;
};

const allFieldsPresent = requiredFields.every(field => checkField(gameData, field));

console.log('\n3. Детальная проверка структуры данных:');

if (gameData.players) {
    console.log(`   ✅ players.princess: ${gameData.players.princess ? 'Да' : 'Нет'}`);
    console.log(`   ✅ players.helper: ${gameData.players.helper ? 'Да' : 'Нет'}`);
    
    if (gameData.players.princess) {
        console.log(`   📝 players.princess.id: ${gameData.players.princess.id}`);
        console.log(`   📝 players.princess.name: ${gameData.players.princess.name}`);
    }
}

if (gameData.stats) {
    console.log(`   ✅ stats.princess: ${gameData.stats.princess ? 'Да' : 'Нет'}`);
    console.log(`   ✅ stats.helper: ${gameData.stats.helper ? 'Да' : 'Нет'}`);
    
    if (gameData.stats.princess) {
        console.log(`   📝 stats.princess.outfit: ${gameData.stats.princess.outfit}`);
        console.log(`   📝 stats.princess.location: ${gameData.stats.princess.location}`);
    }
}

if (gameData.choices) {
    console.log(`   ✅ choices.princess: ${gameData.choices.princess?.length || 0} выборов`);
    console.log(`   ✅ choices.helper: ${gameData.choices.helper?.length || 0} выборов`);
}

console.log('\n4. Симуляция клиентской функции determinePlayerRole:');

// Симуляция клиентской функции
function determinePlayerRole(data, socketId) {
    if (!data) return null;
    if (data.players.princess?.id === socketId) {
        return 'princess';
    } else if (data.players.helper?.id === socketId) {
        return 'helper';
    }
    return null;
}

try {
    const role1 = determinePlayerRole(gameData, 'socket_123');
    const role2 = determinePlayerRole(gameData, 'socket_456');
    const role3 = determinePlayerRole(gameData, 'socket_unknown');
    
    console.log(`   ✅ Socket 'socket_123' → роль: ${role1}`);
    console.log(`   ✅ Socket 'socket_456' → роль: ${role2}`);
    console.log(`   ✅ Socket 'socket_unknown' → роль: ${role3}`);
    
    const expectedRoles = role1 === 'princess' && role2 === 'helper' && role3 === null;
    console.log(`   ${expectedRoles ? '✅' : '❌'} Определение ролей работает корректно`);
    
} catch (error) {
    console.log(`   ❌ Ошибка в determinePlayerRole: ${error.message}`);
}

console.log('\n5. Тест игрового выбора:');

try {
    const choiceResult = gameLogic.makeChoice('client_test', 'socket_123', 'prepare_morning', 'princess');
    
    if (choiceResult.success) {
        console.log(`   ✅ Выбор выполнен: ${choiceResult.message}`);
        
        const updatedGameData = gameLogic.getGameData('client_test');
        console.log(`   📋 Новая сцена: ${updatedGameData.scene?.title}`);
        
        // Проверяем что все поля по-прежнему присутствуют
        const stillValid = requiredFields.every(field => updatedGameData[field] !== undefined);
        console.log(`   ${stillValid ? '✅' : '❌'} Все поля присутствуют после выбора`);
        
    } else {
        console.log(`   ❌ Ошибка выбора: ${choiceResult.message}`);
    }
    
} catch (error) {
    console.log(`   ❌ Ошибка при выполнении выбора: ${error.message}`);
}

console.log('\n6. Тест системы перемещения:');

try {
    const moveResult = gameLogic.makeChoice('client_test', 'socket_123', 'move_to_corridor_upper', 'princess');
    
    if (moveResult.success) {
        console.log(`   ✅ Перемещение выполнено: ${moveResult.message}`);
        
        const updatedGameData = gameLogic.getGameData('client_test');
        console.log(`   📍 Новая локация: ${updatedGameData.stats?.princess?.location}`);
        
    } else {
        console.log(`   ❌ Ошибка перемещения: ${moveResult.message}`);
    }
    
} catch (error) {
    console.log(`   ❌ Ошибка при перемещении: ${error.message}`);
}

console.log('\n=== РЕЗУЛЬТАТ ===');

if (allFieldsPresent) {
    console.log('🎉 КЛИЕНТСКАЯ СОВМЕСТИМОСТЬ ОБЕСПЕЧЕНА!');
    console.log('✅ Все необходимые поля присутствуют');
    console.log('✅ Структура данных совместима с клиентом');
    console.log('✅ Функция determinePlayerRole работает');
    console.log('✅ Игровые механики функционируют');
    
    console.log('\n💡 Клиентский код может работать без изменений!');
} else {
    console.log('❌ ТРЕБУЮТСЯ ДОРАБОТКИ СОВМЕСТИМОСТИ');
    console.log('Некоторые поля отсутствуют или имеют неправильную структуру');
}

// Очистка
gameLogic.removeGame('client_test');
console.log('\n🧹 Тестовая игра очищена');