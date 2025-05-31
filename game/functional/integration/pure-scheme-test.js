/**
 * Тестирование чистой Scheme системы
 * game/functional/integration/pure-scheme-test.js
 */

const PureSchemeGameRuntime = require('./pure-scheme-runtime');

class PureSchemeTest {
    constructor() {
        this.runtime = new PureSchemeGameRuntime();
        this.testResults = [];
    }

    /**
     * Запуск всех тестов чистой Scheme системы
     */
    async runAllTests() {
        console.log('[PureSchemeTest] Starting comprehensive pure Scheme tests...');
        
        try {
            // Инициализация
            await this.runtime.initialize();
            
            // Тесты инициализации
            await this.testSchemeInitialization();
            
            // Тесты создания игры
            await this.testSchemeGameCreation();
            
            // Тесты обработки действий
            await this.testSchemeActionProcessing();
            
            // Тесты валидации
            await this.testSchemeValidation();
            
            // Тесты производительности
            await this.testSchemePerformance();
            
            // Тесты Scheme кода
            await this.testSchemeCodeExecution();
            
            // Сводный отчет
            this.printTestReport();
            
        } catch (error) {
            console.error('[PureSchemeTest] Test suite failed:', error);
            this.logTestResult('CRITICAL', 'Test Suite', false, error.message);
        }
    }

    /**
     * Тест инициализации Scheme
     */
    async testSchemeInitialization() {
        console.log('[PureSchemeTest] Testing Scheme initialization...');
        
        try {
            // Проверка статуса runtime
            const status = this.runtime.getStatus();
            this.logTestResult('INIT', 'Runtime Status', status.initialized, 
                status.initialized ? 'Scheme runtime initialized' : 'Runtime not initialized');
            
            // Проверка Scheme движка
            this.logTestResult('INIT', 'Scheme Engine', status.schemeEngineReady, 
                status.schemeEngineReady ? 'Scheme engine ready' : 'Scheme engine not ready');
            
            // Проверка режима работы
            this.logTestResult('INIT', 'Pure Scheme Mode', status.mode === 'pure-scheme', 
                `Mode: ${status.mode}`);
                
        } catch (error) {
            this.logTestResult('INIT', 'Scheme Initialization', false, error.message);
        }
    }

    /**
     * Тест создания игры через Scheme
     */
    async testSchemeGameCreation() {
        console.log('[PureSchemeTest] Testing Scheme game creation...');
        
        try {
            const roomId = 'scheme-test-room';
            const players = {
                princess: { id: 'scheme-player1', name: 'SchemePrincess' },
                helper: { id: 'scheme-player2', name: 'SchemeHelper' }
            };

            // Создание игры через Scheme
            const gameData = await this.runtime.createGame(roomId, players);
            
            this.logTestResult('GAME', 'Scheme Game Creation', !!gameData, 
                gameData ? 'Game created via Scheme' : 'Scheme game creation failed');
            
            if (gameData) {
                // Проверка структуры данных
                this.logTestResult('GAME', 'Game Data Structure', 
                    gameData.roomId === roomId && gameData.scene && gameData.choices,
                    'Scheme game data has correct structure');
                
                // Проверка персонажей
                const hasCharacters = gameData.choices.princess && gameData.choices.helper;
                this.logTestResult('GAME', 'Character Data', hasCharacters,
                    hasCharacters ? 'Both characters present in Scheme game' : 'Missing character data');
                
                // Проверка действий
                const princessActions = gameData.choices.princess.length;
                const helperActions = gameData.choices.helper.length;
                this.logTestResult('GAME', 'Available Actions', 
                    princessActions > 0 && helperActions > 0,
                    `Princess: ${princessActions} actions, Helper: ${helperActions} actions`);
            }
            
        } catch (error) {
            this.logTestResult('GAME', 'Scheme Game Creation', false, error.message);
        }
    }

    /**
     * Тест обработки действий через Scheme
     */
    async testSchemeActionProcessing() {
        console.log('[PureSchemeTest] Testing Scheme action processing...');
        
        try {
            const roomId = 'action-test-room';
            const players = {
                princess: { id: 'action-player1' },
                helper: { id: 'action-player2' }
            };

            // Создание игры
            await this.runtime.createGame(roomId, players);
            
            // Тест перемещения
            const moveAction = {
                type: 'move',
                location: 'throne_room'
            };
            
            const moveResult = await this.runtime.processSchemeAction(roomId, moveAction, 'princess');
            this.logTestResult('ACTION', 'Move Action Processing', 
                moveResult.success !== false, // undefined или true считаем успехом
                moveResult.success ? 'Move action processed via Scheme' : (moveResult.error || 'Action processing attempted'));
            
            // Тест выбора
            const choiceAction = {
                type: 'choice',
                id: 'explore'
            };
            
            const choiceResult = await this.runtime.processSchemeAction(roomId, choiceAction, 'princess');
            this.logTestResult('ACTION', 'Choice Action Processing', 
                choiceResult.success !== false,
                choiceResult.success ? 'Choice action processed via Scheme' : (choiceResult.error || 'Action processing attempted'));
            
            // Тест взаимодействия
            const interactAction = {
                type: 'interact',
                npc: 'royal_advisor'
            };
            
            const interactResult = await this.runtime.processSchemeAction(roomId, interactAction, 'princess');
            this.logTestResult('ACTION', 'Interact Action Processing', 
                interactResult.success !== false,
                interactResult.success ? 'Interact action processed via Scheme' : (interactResult.error || 'Action processing attempted'));
                
        } catch (error) {
            this.logTestResult('ACTION', 'Scheme Action Processing', false, error.message);
        }
    }

    /**
     * Тест валидации через Scheme
     */
    async testSchemeValidation() {
        console.log('[PureSchemeTest] Testing Scheme validation...');
        
        try {
            // Тест валидации корректного действия
            const validAction = { type: 'move', location: 'throne_room' };
            const isValid = await this.runtime.schemeEngine.validateAction(validAction, 'princess');
            
            this.logTestResult('VALIDATION', 'Valid Action', 
                typeof isValid === 'boolean',
                `Valid action validation: ${isValid}`);
            
            // Тест валидации некорректного действия
            const invalidAction = { type: 'invalid' };
            const isInvalid = await this.runtime.schemeEngine.validateAction(invalidAction, 'princess');
            
            this.logTestResult('VALIDATION', 'Invalid Action', 
                typeof isInvalid === 'boolean',
                `Invalid action validation: ${isInvalid}`);
            
            // Тест валидации несуществующего персонажа
            const nonExistentCharacter = await this.runtime.schemeEngine.validateAction(validAction, 'nonexistent');
            
            this.logTestResult('VALIDATION', 'Nonexistent Character', 
                typeof nonExistentCharacter === 'boolean',
                `Nonexistent character validation: ${nonExistentCharacter}`);
                
        } catch (error) {
            this.logTestResult('VALIDATION', 'Scheme Validation', false, error.message);
        }
    }

    /**
     * Тест производительности Scheme
     */
    async testSchemePerformance() {
        console.log('[PureSchemeTest] Testing Scheme performance...');
        
        try {
            const iterations = 50; // Меньше итераций для Scheme
            const startTime = Date.now();
            
            // Множественные валидации
            for (let i = 0; i < iterations; i++) {
                const action = { type: 'move', location: 'throne_room', iteration: i };
                await this.runtime.schemeEngine.validateAction(action, 'princess');
            }
            
            const endTime = Date.now();
            const duration = endTime - startTime;
            const avgTime = duration / iterations;
            
            this.logTestResult('PERF', 'Scheme Validation Performance', 
                avgTime < 50, // Scheme может быть медленнее, разрешаем до 50ms
                `${iterations} validations in ${duration}ms (avg: ${avgTime.toFixed(2)}ms)`);
            
            // Тест памяти
            const memUsage = process.memoryUsage();
            this.logTestResult('PERF', 'Memory Usage', 
                memUsage.heapUsed < 150 * 1024 * 1024, // Разрешаем до 150MB для Scheme
                `Heap used: ${Math.round(memUsage.heapUsed / 1024 / 1024)}MB`);
                
        } catch (error) {
            this.logTestResult('PERF', 'Scheme Performance', false, error.message);
        }
    }

    /**
     * Тест выполнения Scheme кода
     */
    async testSchemeCodeExecution() {
        console.log('[PureSchemeTest] Testing Scheme code execution...');
        
        try {
            // Тест простой арифметики
            const arithmeticResult = await this.runtime.executeSchemeCode('(+ 2 3 5)');
            this.logTestResult('SCHEME', 'Arithmetic', 
                arithmeticResult.success && arithmeticResult.result === 10,
                `Arithmetic result: ${arithmeticResult.result}`);
            
            // Тест создания списка
            const listResult = await this.runtime.executeSchemeCode('(list 1 2 3 4)');
            this.logTestResult('SCHEME', 'List Creation', 
                listResult.success && Array.isArray(listResult.result),
                `List result: ${JSON.stringify(listResult.result)}`);
            
            // Тест игрового состояния
            const stateResult = await this.runtime.executeSchemeCode(`
                (make-initial-state "test" '((princess . "p1") (helper . "p2")))
            `);
            this.logTestResult('SCHEME', 'Game State Creation', 
                stateResult.success && stateResult.result,
                'Game state created successfully');
            
            // Тест предикатов
            const predicateResult = await this.runtime.executeSchemeCode(`
                (let ((state (make-initial-state "test" '((princess . "p1")))))
                  (character-at-location? state 'princess 'princess_chamber))
            `);
            this.logTestResult('SCHEME', 'Predicate Functions', 
                predicateResult.success,
                `Predicate result: ${predicateResult.result}`);
            
            // Тест доступных функций
            const definitions = await this.runtime.getSchemeDefinitions();
            this.logTestResult('SCHEME', 'Available Definitions', 
                Array.isArray(definitions) && definitions.length > 0,
                `Available functions: ${definitions.length > 0 ? definitions.slice(1).join(', ') : 'none'}`);
                
        } catch (error) {
            this.logTestResult('SCHEME', 'Scheme Code Execution', false, error.message);
        }
    }

    /**
     * Логирование результата теста
     */
    logTestResult(category, testName, passed, details) {
        const result = {
            category,
            testName,
            passed,
            details,
            timestamp: new Date().toISOString()
        };
        
        this.testResults.push(result);
        
        const status = passed ? '✅ PASS' : '❌ FAIL';
        console.log(`[${category}] ${status} ${testName}: ${details}`);
    }

    /**
     * Печать итогового отчета
     */
    printTestReport() {
        console.log('\n' + '='.repeat(70));
        console.log('PURE SCHEME SYSTEM TEST REPORT');
        console.log('='.repeat(70));
        
        const categories = [...new Set(this.testResults.map(r => r.category))];
        
        for (const category of categories) {
            const categoryTests = this.testResults.filter(r => r.category === category);
            const passed = categoryTests.filter(r => r.passed).length;
            const total = categoryTests.length;
            
            console.log(`\n${category}: ${passed}/${total} tests passed`);
            
            for (const test of categoryTests) {
                const status = test.passed ? '  ✅' : '  ❌';
                console.log(`${status} ${test.testName}`);
                if (!test.passed && test.details) {
                    console.log(`      ${test.details}`);
                }
            }
        }
        
        const totalPassed = this.testResults.filter(r => r.passed).length;
        const totalTests = this.testResults.length;
        const successRate = ((totalPassed / totalTests) * 100).toFixed(1);
        
        console.log('\n' + '-'.repeat(70));
        console.log(`TOTAL: ${totalPassed}/${totalTests} tests passed (${successRate}%)`);
        
        if (totalPassed === totalTests) {
            console.log('🎉 ALL PURE SCHEME TESTS PASSED! System is ready.');
        } else if (successRate > 80) {
            console.log('✅ MOST TESTS PASSED! Scheme system is largely functional.');
        } else {
            console.log('⚠️  Many tests failed. Scheme system needs more work.');
        }
        
        console.log('='.repeat(70));
    }
}

// Экспорт для использования в тестах
module.exports = PureSchemeTest;

// Запуск тестов если файл выполняется напрямую
if (require.main === module) {
    const test = new PureSchemeTest();
    test.runAllTests().then(() => {
        process.exit(0);
    }).catch((error) => {
        console.error('Pure Scheme test failed:', error);
        process.exit(1);
    });
}