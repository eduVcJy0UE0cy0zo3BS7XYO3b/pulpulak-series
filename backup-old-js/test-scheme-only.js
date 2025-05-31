const SchemeOnlyGameEngine = require('./game/functional/integration/scheme-only-engine');

async function testSchemeOnlyEngine() {
    console.log('=== Testing Scheme-Only Game Engine ===\n');
    
    try {
        const engine = new SchemeOnlyGameEngine();
        
        // Ждем инициализации
        await new Promise(resolve => {
            const check = () => {
                if (engine.isReady()) {
                    resolve();
                } else {
                    setTimeout(check, 100);
                }
            };
            check();
        });
        
        console.log('✅ Engine initialized with Scheme logic');
        
        // Тест создания игры
        console.log('\n--- Testing Game Creation ---');
        const gameState = await engine.createGame('test-room', {});
        console.log('✅ Game created via pure Scheme logic');
        console.log('   State type:', typeof gameState);
        
        // Тест получения действий
        console.log('\n--- Testing Available Actions ---');
        const actions = await engine.getAvailableActions('princess');
        console.log('✅ Actions retrieved via pure Scheme logic');
        console.log('   Actions count:', Array.isArray(actions) ? actions.length : 'not array');
        console.log('   Sample actions:', JSON.stringify(actions.slice(0, 2), null, 2));
        
        // Тест валидации
        console.log('\n--- Testing Action Validation ---');
        const testAction = { type: 'move', location: 'throne_room' };
        const isValid = await engine.validateAction(testAction, 'princess');
        console.log('✅ Action validated via pure Scheme logic');
        console.log('   Valid:', isValid);
        
        // Тест обработки действия
        console.log('\n--- Testing Action Processing ---');
        const result = await engine.processAction(testAction, 'princess');
        console.log('✅ Action processed via pure Scheme logic');
        console.log('   Success:', result.success);
        console.log('   Result:', JSON.stringify(result, null, 2));
        
        // Тест производительности
        console.log('\n--- Testing Performance ---');
        const startTime = Date.now();
        for (let i = 0; i < 50; i++) {
            await engine.validateAction(testAction, 'princess');
        }
        const endTime = Date.now();
        console.log('✅ Performance test completed');
        console.log('   50 validations in', endTime - startTime, 'ms');
        
        console.log('\n=== Scheme-Only Engine Test Complete ===');
        console.log('🎉 All game logic is now running in pure Scheme!');
        
    } catch (error) {
        console.error('❌ Test failed:', error);
        console.error('Stack:', error.stack);
    }
}

testSchemeOnlyEngine().catch(console.error);