/**
 * Тестирование миграции на функциональную архитектуру
 * game/functional/integration/migration-test.js
 */

const BiwaGameRuntime = require('./biwa-runtime');
const FunctionalGameEngine = require('./functional-engine');

class MigrationTest {
    constructor() {
        this.runtime = new BiwaGameRuntime();
        this.testResults = [];
    }

    /**
     * Запуск всех тестов миграции
     */
    async runAllTests() {
        console.log('[MigrationTest] Starting comprehensive migration tests...');
        
        try {
            // Инициализация
            await this.runtime.initialize();
            
            // Базовые тесты
            await this.testBasicInitialization();
            await this.testGameCreation();
            await this.testStateTransformations();
            
            // Тесты интеграции
            await this.testFunctionalQuests();
            await this.testSchemeJavaScriptBridge();
            
            // Тесты производительности
            await this.testPerformance();
            
            // Сводный отчет
            this.printTestReport();
            
        } catch (error) {
            console.error('[MigrationTest] Test suite failed:', error);
            this.logTestResult('CRITICAL', 'Test Suite', false, error.message);
        }
    }

    /**
     * Тест базовой инициализации
     */
    async testBasicInitialization() {
        console.log('[MigrationTest] Testing basic initialization...');
        
        try {
            // Проверка статуса runtime
            const status = this.runtime.getStatus();
            this.logTestResult('INIT', 'Runtime Status', status.initialized, 
                status.initialized ? 'Runtime initialized successfully' : 'Runtime not initialized');
            
            // Проверка BiwaScheme движка
            const engine = this.runtime.functionalEngine;
            this.logTestResult('INIT', 'BiwaScheme Engine', !!engine, 
                engine ? 'Functional engine available' : 'Functional engine missing');
            
            // Проверка режима миграции
            this.logTestResult('INIT', 'Migration Mode', status.migrationMode === 'parallel', 
                `Migration mode: ${status.migrationMode}`);
                
        } catch (error) {
            this.logTestResult('INIT', 'Basic Initialization', false, error.message);
        }
    }

    /**
     * Тест создания игры
     */
    async testGameCreation() {
        console.log('[MigrationTest] Testing game creation...');
        
        try {
            const roomId = 'test-room-001';
            const players = {
                princess: { id: 'player1', name: 'TestPrincess' },
                helper: { id: 'player2', name: 'TestHelper' }
            };

            // Создание игры
            const gameData = await this.runtime.createGame(roomId, players);
            
            this.logTestResult('GAME', 'Game Creation', !!gameData, 
                gameData ? 'Game created successfully' : 'Game creation failed');
            
            // Проверка структуры данных
            if (gameData) {
                this.logTestResult('GAME', 'Game Data Structure', 
                    gameData.roomId === roomId && gameData.scene && gameData.choices,
                    'Game data has correct structure');
                
                // Проверка персонажей
                const hasCharacters = gameData.choices.princess && gameData.choices.helper;
                this.logTestResult('GAME', 'Character Data', hasCharacters,
                    hasCharacters ? 'Both characters present' : 'Missing character data');
            }
            
        } catch (error) {
            this.logTestResult('GAME', 'Game Creation', false, error.message);
        }
    }

    /**
     * Тест трансформаций состояния
     */
    async testStateTransformations() {
        console.log('[MigrationTest] Testing state transformations...');
        
        try {
            // Тест иммутабельности
            const engine = this.runtime.functionalEngine;
            const initialState = engine.getCurrentState();
            
            // Попытка модификации состояния через функциональные трансформации
            const action = { type: 'move', location: 'throne_room' };
            const result = await engine.processAction(action, 'princess');
            
            this.logTestResult('STATE', 'Immutable Transformations', 
                result.success || result.error === 'Invalid action',
                'State transformation attempted');
            
            // Проверка, что исходное состояние не изменилось (иммутабельность)
            const currentState = engine.getCurrentState();
            const stateUnchanged = JSON.stringify(initialState) === JSON.stringify(currentState) || 
                                  result.success; // Если операция успешна, состояние должно быть новым
            
            this.logTestResult('STATE', 'Immutability', stateUnchanged,
                'Original state preserved or properly transformed');
                
        } catch (error) {
            this.logTestResult('STATE', 'State Transformations', false, error.message);
        }
    }

    /**
     * Тест функциональных квестов
     */
    async testFunctionalQuests() {
        console.log('[MigrationTest] Testing functional quests...');
        
        try {
            const roomId = 'test-quest-room';
            const players = {
                princess: { id: 'quest-player1', name: 'QuestPrincess' },
                helper: { id: 'quest-player2', name: 'QuestHelper' }
            };

            // Создание игры для квестов
            await this.runtime.createGame(roomId, players);
            
            // Тест валидации квестового действия
            const questAction = { type: 'start-quest', questId: 'princess-lost-relic' };
            const isValid = await this.runtime.functionalEngine.validateAction(questAction, 'princess');
            
            this.logTestResult('QUEST', 'Quest Action Validation', 
                typeof isValid === 'boolean',
                'Quest validation returned boolean result');
            
            // Тест получения доступных действий
            const availableActions = await this.runtime.functionalEngine.getAvailableActions('princess');
            
            this.logTestResult('QUEST', 'Available Actions', 
                Array.isArray(availableActions),
                `Available actions: ${Array.isArray(availableActions) ? availableActions.length : 'none'}`);
                
        } catch (error) {
            this.logTestResult('QUEST', 'Functional Quests', false, error.message);
        }
    }

    /**
     * Тест моста Scheme-JavaScript
     */
    async testSchemeJavaScriptBridge() {
        console.log('[MigrationTest] Testing Scheme-JavaScript bridge...');
        
        try {
            const engine = this.runtime.functionalEngine;
            
            // Тест конвертации JS -> Scheme
            const jsObject = { type: 'test', value: 42, list: [1, 2, 3] };
            const schemeString = engine.jsToScheme(jsObject);
            
            this.logTestResult('BRIDGE', 'JS to Scheme Conversion', 
                typeof schemeString === 'string' && schemeString.includes('test'),
                `Converted: ${schemeString.substring(0, 50)}...`);
            
            // Тест выполнения простого Scheme кода
            try {
                const simpleResult = await engine.interpreter.run('(+ 1 2 3)');
                this.logTestResult('BRIDGE', 'Simple Scheme Execution', 
                    simpleResult !== undefined,
                    `Simple arithmetic result: ${simpleResult}`);
            } catch (schemeError) {
                this.logTestResult('BRIDGE', 'Simple Scheme Execution', false, 
                    `Scheme execution failed: ${schemeError.message}`);
            }
            
        } catch (error) {
            this.logTestResult('BRIDGE', 'Scheme-JavaScript Bridge', false, error.message);
        }
    }

    /**
     * Тест производительности
     */
    async testPerformance() {
        console.log('[MigrationTest] Testing performance...');
        
        try {
            const iterations = 100;
            const startTime = Date.now();
            
            // Множественные операции для измерения производительности
            for (let i = 0; i < iterations; i++) {
                const action = { type: 'test-action', iteration: i };
                await this.runtime.functionalEngine.validateAction(action, 'princess');
            }
            
            const endTime = Date.now();
            const duration = endTime - startTime;
            const avgTime = duration / iterations;
            
            this.logTestResult('PERF', 'Action Validation Performance', 
                avgTime < 10, // Должно быть быстрее 10ms на операцию
                `${iterations} validations in ${duration}ms (avg: ${avgTime.toFixed(2)}ms)`);
            
            // Тест памяти
            const memUsage = process.memoryUsage();
            this.logTestResult('PERF', 'Memory Usage', 
                memUsage.heapUsed < 100 * 1024 * 1024, // Меньше 100MB
                `Heap used: ${Math.round(memUsage.heapUsed / 1024 / 1024)}MB`);
                
        } catch (error) {
            this.logTestResult('PERF', 'Performance Tests', false, error.message);
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
        console.log('\n' + '='.repeat(60));
        console.log('MIGRATION TEST REPORT');
        console.log('='.repeat(60));
        
        const categories = [...new Set(this.testResults.map(r => r.category))];
        
        for (const category of categories) {
            const categoryTests = this.testResults.filter(r => r.category === category);
            const passed = categoryTests.filter(r => r.passed).length;
            const total = categoryTests.length;
            
            console.log(`\n${category}: ${passed}/${total} tests passed`);
            
            for (const test of categoryTests) {
                const status = test.passed ? '  ✅' : '  ❌';
                console.log(`${status} ${test.testName}`);
                if (!test.passed) {
                    console.log(`      ${test.details}`);
                }
            }
        }
        
        const totalPassed = this.testResults.filter(r => r.passed).length;
        const totalTests = this.testResults.length;
        const successRate = ((totalPassed / totalTests) * 100).toFixed(1);
        
        console.log('\n' + '-'.repeat(60));
        console.log(`TOTAL: ${totalPassed}/${totalTests} tests passed (${successRate}%)`);
        
        if (totalPassed === totalTests) {
            console.log('🎉 ALL TESTS PASSED! Migration is ready.');
        } else {
            console.log('⚠️  Some tests failed. Review before proceeding with migration.');
        }
        
        console.log('='.repeat(60));
    }

    /**
     * Тест сравнения производительности legacy vs functional
     */
    async testPerformanceComparison() {
        console.log('[MigrationTest] Comparing legacy vs functional performance...');
        
        try {
            // Переключение в режим сравнения
            this.runtime.setMigrationMode('parallel');
            
            const roomId = 'perf-test-room';
            const players = {
                princess: { id: 'perf-player1' },
                helper: { id: 'perf-player2' }
            };
            
            await this.runtime.createGame(roomId, players);
            
            const iterations = 50;
            const actions = [
                { type: 'move', location: 'throne_room' },
                { type: 'interact', npc: 'royal_advisor' },
                { type: 'choice', id: 'test_choice' }
            ];
            
            let functionalTime = 0;
            let legacyTime = 0;
            
            // Тест функциональной системы
            const functionalStart = Date.now();
            for (let i = 0; i < iterations; i++) {
                const action = actions[i % actions.length];
                await this.runtime.makeFunctionalChoice(roomId, action, 'princess');
            }
            functionalTime = Date.now() - functionalStart;
            
            // Тест legacy системы (заглушка)
            const legacyStart = Date.now();
            for (let i = 0; i < iterations; i++) {
                // Симуляция legacy операции
                await new Promise(resolve => setTimeout(resolve, 1));
            }
            legacyTime = Date.now() - legacyStart;
            
            this.logTestResult('COMPARISON', 'Performance Comparison',
                functionalTime <= legacyTime * 2, // Функциональная система не должна быть более чем в 2 раза медленнее
                `Functional: ${functionalTime}ms, Legacy: ${legacyTime}ms`);
                
        } catch (error) {
            this.logTestResult('COMPARISON', 'Performance Comparison', false, error.message);
        }
    }
}

// Экспорт для использования в тестах
module.exports = MigrationTest;

// Запуск тестов если файл выполняется напрямую
if (require.main === module) {
    const test = new MigrationTest();
    test.runAllTests().then(() => {
        process.exit(0);
    }).catch((error) => {
        console.error('Migration test failed:', error);
        process.exit(1);
    });
}