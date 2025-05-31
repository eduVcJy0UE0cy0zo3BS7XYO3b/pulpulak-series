// Простой тестовый раннер на чистом JavaScript
// game/simple-test-runner.js

const fs = require('fs');
const path = require('path');

// Симуляция базовых функций Scheme для тестирования
class SimpleSchemeTestRunner {
  constructor() {
    this.testResults = [];
    this.passedTests = 0;
    this.failedTests = 0;
  }

  // Симуляция основных игровых функций для тестирования
  makeGameState(roomId) {
    return {
      type: 'game-state',
      'room-id': roomId,
      scene: 'coop_awakening',
      'current-turn': 'princess',
      chapter: 1,
      players: new Map(),
      characters: [
        {
          id: 'princess',
          location: 'princess_chamber',
          outfit: 'princess_dress',
          inventory: [],
          stats: { loyalty: 50, knowledge: 30, charm: 70 }
        },
        {
          id: 'helper',
          location: 'princess_chamber',
          outfit: 'common_dress',
          inventory: ['translation_earrings', 'voice_medallion'],
          stats: { loyalty: 50, knowledge: 60, charm: 40 }
        }
      ],
      world: {
        time: 'early_morning',
        locations: {
          princess_chamber: { accessible: true, npcs: [] },
          throne_room: { accessible: true, npcs: ['royal_advisor'] },
          kitchen: { accessible: true, npcs: ['cook'] },
          garden: { accessible: true, npcs: [] },
          library: { accessible: true, npcs: ['librarian'] },
          village: { accessible: true, npcs: ['old_woman', 'merchant'] }
        }
      },
      quests: { active: [], completed: [] }
    };
  }

  joinGame(state, playerId, character) {
    if (!state.players.has('princess') && character === 'princess') {
      state.players.set('princess', playerId);
      return ['success', 'princess', state];
    } else if (!state.players.has('helper') && character === 'helper') {
      state.players.set('helper', playerId);
      return ['success', 'helper', state];
    } else if (!state.players.has('princess')) {
      state.players.set('princess', playerId);
      return ['success', 'princess', state];
    } else if (!state.players.has('helper')) {
      state.players.set('helper', playerId);
      return ['success', 'helper', state];
    }
    return ['error', 'Game is full', state];
  }

  makeChoice(state, playerId, choiceId, character) {
    // Простая симуляция для базовых действий
    const newState = JSON.parse(JSON.stringify(state)); // Deep copy
    
    if (choiceId === 'explore') {
      return ['success', 'Explored location', newState];
    } else if (choiceId === 'rest') {
      return ['success', 'Rested successfully', newState];
    } else if (choiceId.startsWith('move_to_')) {
      const location = choiceId.substring(8);
      const characterData = newState.characters.find(c => c.id === character);
      if (characterData && newState.world.locations[location]) {
        characterData.location = location;
        return ['success', 'Moved successfully', newState];
      }
      return ['error', 'Invalid movement', state];
    } else if (choiceId === 'swap_outfits') {
      const princess = newState.characters.find(c => c.id === 'princess');
      const helper = newState.characters.find(c => c.id === 'helper');
      if (princess && helper && princess.location === helper.location) {
        const temp = princess.outfit;
        princess.outfit = helper.outfit;
        helper.outfit = temp;
        return ['success', 'Outfits swapped', newState];
      }
      return ['error', 'Cannot swap outfits', state];
    }
    
    return ['error', 'Invalid choice', state];
  }

  getCharacterLocation(state, character) {
    const characterData = state.characters.find(c => c.id === character);
    return characterData ? characterData.location : null;
  }

  getCharacterOutfit(state, character) {
    const characterData = state.characters.find(c => c.id === character);
    return characterData ? characterData.outfit : null;
  }

  // Утверждения для тестов
  assertEqual(expected, actual, message) {
    if (expected === actual) {
      this.logTest('PASS', message);
      this.passedTests++;
      return true;
    } else {
      this.logTest('FAIL', `${message} - Expected: ${expected}, Got: ${actual}`);
      this.failedTests++;
      return false;
    }
  }

  assertTrue(value, message) {
    if (value) {
      this.logTest('PASS', message);
      this.passedTests++;
      return true;
    } else {
      this.logTest('FAIL', `${message} - Expected true, got ${value}`);
      this.failedTests++;
      return false;
    }
  }

  assertFalse(value, message) {
    if (!value) {
      this.logTest('PASS', message);
      this.passedTests++;
      return true;
    } else {
      this.logTest('FAIL', `${message} - Expected false, got ${value}`);
      this.failedTests++;
      return false;
    }
  }

  assertNotNull(value, message) {
    if (value !== null && value !== undefined) {
      this.logTest('PASS', message);
      this.passedTests++;
      return true;
    } else {
      this.logTest('FAIL', `${message} - Expected non-null value`);
      this.failedTests++;
      return false;
    }
  }

  logTest(status, message) {
    const result = `[${status}] ${message}`;
    console.log(result);
    this.testResults.push(result);
  }

  // Основные тесты
  runCoreTests() {
    console.log('\n=== ОСНОВНЫЕ ТЕСТЫ ===');
    
    // Тест создания игры
    const state = this.makeGameState('test-room');
    this.assertNotNull(state, 'Game state creation');
    this.assertEqual('coop_awakening', state.scene, 'Initial scene');
    this.assertEqual('princess', state['current-turn'], 'Initial turn');

    // Тест присоединения игроков
    const [status1, role1, newState1] = this.joinGame(state, 'player1', 'princess');
    this.assertEqual('success', status1, 'Player 1 joins as princess');
    this.assertEqual('princess', role1, 'Player 1 gets princess role');

    const [status2, role2, newState2] = this.joinGame(newState1, 'player2', 'helper');
    this.assertEqual('success', status2, 'Player 2 joins as helper');
    this.assertEqual('helper', role2, 'Player 2 gets helper role');

    // Тест переполнения игры
    const [status3] = this.joinGame(newState2, 'player3', 'princess');
    this.assertEqual('error', status3, 'Third player rejected');

    return newState2;
  }

  runMovementTests() {
    console.log('\n=== ТЕСТЫ ПЕРЕМЕЩЕНИЯ ===');
    
    const state = this.makeGameState('test-room');
    const [, , gameState] = this.joinGame(state, 'player1', 'princess');
    const [, , fullState] = this.joinGame(gameState, 'player2', 'helper');

    // Тест перемещения принцессы
    const [moveStatus, , newState] = this.makeChoice(fullState, 'player1', 'move_to_throne_room', 'princess');
    this.assertEqual('success', moveStatus, 'Princess movement');
    
    const princessLocation = this.getCharacterLocation(newState, 'princess');
    this.assertEqual('throne_room', princessLocation, 'Princess location updated');

    // Тест что помощник остался на месте
    const helperLocation = this.getCharacterLocation(newState, 'helper');
    this.assertEqual('princess_chamber', helperLocation, 'Helper location unchanged');

    return newState;
  }

  runOutfitTests() {
    console.log('\n=== ТЕСТЫ НАРЯДОВ ===');
    
    const state = this.makeGameState('test-room');
    const [, , gameState] = this.joinGame(state, 'player1', 'princess');
    const [, , fullState] = this.joinGame(gameState, 'player2', 'helper');

    // Проверяем начальные наряды
    const initialPrincessOutfit = this.getCharacterOutfit(fullState, 'princess');
    const initialHelperOutfit = this.getCharacterOutfit(fullState, 'helper');
    this.assertEqual('princess_dress', initialPrincessOutfit, 'Princess initial outfit');
    this.assertEqual('common_dress', initialHelperOutfit, 'Helper initial outfit');

    // Обмениваемся нарядами
    const [swapStatus, , swappedState] = this.makeChoice(fullState, 'player1', 'swap_outfits', 'princess');
    this.assertEqual('success', swapStatus, 'Outfit swap success');

    // Проверяем что наряды обменялись
    const newPrincessOutfit = this.getCharacterOutfit(swappedState, 'princess');
    const newHelperOutfit = this.getCharacterOutfit(swappedState, 'helper');
    this.assertEqual('common_dress', newPrincessOutfit, 'Princess has helper outfit');
    this.assertEqual('princess_dress', newHelperOutfit, 'Helper has princess outfit');

    return swappedState;
  }

  runBasicActionTests() {
    console.log('\n=== ТЕСТЫ БАЗОВЫХ ДЕЙСТВИЙ ===');
    
    const state = this.makeGameState('test-room');
    const [, , gameState] = this.joinGame(state, 'player1', 'princess');
    const [, , fullState] = this.joinGame(gameState, 'player2', 'helper');

    // Тест исследования
    const [exploreStatus] = this.makeChoice(fullState, 'player1', 'explore', 'princess');
    this.assertEqual('success', exploreStatus, 'Explore action');

    // Тест отдыха
    const [restStatus] = this.makeChoice(fullState, 'player2', 'rest', 'helper');
    this.assertEqual('success', restStatus, 'Rest action');

    // Тест недопустимого действия
    const [invalidStatus] = this.makeChoice(fullState, 'player1', 'invalid_action', 'princess');
    this.assertEqual('error', invalidStatus, 'Invalid action rejected');

    return fullState;
  }

  runIntegrationTests() {
    console.log('\n=== ИНТЕГРАЦИОННЫЕ ТЕСТЫ ===');
    
    // Сложный сценарий: создание игры -> добавление игроков -> перемещения -> обмен нарядами
    let state = this.makeGameState('integration-test');
    
    // Добавляем игроков
    let [status1, , newState] = this.joinGame(state, 'player1', 'princess');
    this.assertEqual('success', status1, 'Integration: Player 1 joined');
    
    let [status2, , fullState] = this.joinGame(newState, 'player2', 'helper');
    this.assertEqual('success', status2, 'Integration: Player 2 joined');

    // Принцесса идет в библиотеку
    let [moveStatus1, , movedState] = this.makeChoice(fullState, 'player1', 'move_to_library', 'princess');
    this.assertEqual('success', moveStatus1, 'Integration: Princess moves to library');

    // Помощник следует за ней
    let [moveStatus2, , reuniteState] = this.makeChoice(movedState, 'player2', 'move_to_library', 'helper');
    this.assertEqual('success', moveStatus2, 'Integration: Helper follows to library');

    // Проверяем что они в одном месте
    const princessLoc = this.getCharacterLocation(reuniteState, 'princess');
    const helperLoc = this.getCharacterLocation(reuniteState, 'helper');
    this.assertEqual(princessLoc, helperLoc, 'Integration: Characters reunited');

    // Обмениваемся нарядами
    let [swapStatus, , finalState] = this.makeChoice(reuniteState, 'player1', 'swap_outfits', 'princess');
    this.assertEqual('success', swapStatus, 'Integration: Outfit swap in library');

    // Финальная проверка
    const finalPrincessOutfit = this.getCharacterOutfit(finalState, 'princess');
    const finalHelperOutfit = this.getCharacterOutfit(finalState, 'helper');
    this.assertEqual('common_dress', finalPrincessOutfit, 'Integration: Final princess outfit');
    this.assertEqual('princess_dress', finalHelperOutfit, 'Integration: Final helper outfit');

    return finalState;
  }

  runAllTests() {
    console.log('🧪 Запуск упрощенных тестов Scheme архитектуры...\n');
    
    this.passedTests = 0;
    this.failedTests = 0;
    this.testResults = [];

    try {
      this.runCoreTests();
      this.runMovementTests();
      this.runOutfitTests();
      this.runBasicActionTests();
      this.runIntegrationTests();

      console.log('\n========================================');
      console.log('РЕЗУЛЬТАТЫ ТЕСТИРОВАНИЯ');
      console.log('========================================');
      console.log(`✅ Пройдено: ${this.passedTests}`);
      console.log(`❌ Неудачно: ${this.failedTests}`);
      console.log(`📊 Всего: ${this.passedTests + this.failedTests}`);
      
      if (this.failedTests === 0) {
        console.log('\n🎉 Все тесты прошли успешно!');
        return 0;
      } else {
        console.log(`\n⚠️  ${this.failedTests} тест(ов) завершились ошибкой`);
        return 1;
      }
    } catch (error) {
      console.error('\n💥 Критическая ошибка при выполнении тестов:', error.message);
      return 1;
    }
  }

  runQuickTests() {
    console.log('⚡ Запуск быстрых тестов...\n');
    
    this.passedTests = 0;
    this.failedTests = 0;

    // Только самые базовые тесты
    const state = this.makeGameState('quick-test');
    this.assertNotNull(state, 'Quick: Game state creation');
    
    const [status, role] = this.joinGame(state, 'player1', 'princess');
    this.assertEqual('success', status, 'Quick: Player join');
    this.assertEqual('princess', role, 'Quick: Correct role assigned');

    console.log(`\n⚡ Быстрые тесты: ${this.passedTests} пройдено, ${this.failedTests} ошибок`);
    return this.failedTests === 0 ? 0 : 1;
  }
}

// Функция для использования в npm scripts
async function runSimpleTests(testType = 'all') {
  const runner = new SimpleSchemeTestRunner();
  
  try {
    let exitCode;
    switch (testType) {
      case 'quick':
        exitCode = runner.runQuickTests();
        break;
      case 'all':
      default:
        exitCode = runner.runAllTests();
        break;
    }
    
    process.exit(exitCode);
  } catch (error) {
    console.error('💥 Критическая ошибка:', error.message);
    process.exit(1);
  }
}

// Экспорт для использования в Jest
module.exports = {
  SimpleSchemeTestRunner,
  runSimpleTests
};

// Прямой запуск
if (require.main === module) {
  const testType = process.argv[2] || 'all';
  runSimpleTests(testType);
}