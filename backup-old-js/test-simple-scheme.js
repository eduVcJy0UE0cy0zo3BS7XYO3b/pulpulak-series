const PureSchemeSimpleEngine = require('./game/functional/integration/pure-scheme-simple');

async function testSimpleScheme() {
    console.log('=== Testing Simple Pure Scheme Engine ===\n');
    
    const engine = new PureSchemeSimpleEngine();
    
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
    
    console.log('✅ Engine initialized');
    
    // Тест состояния
    const state = engine.getCurrentState();
    console.log('✅ Current state retrieved:', !!state);
    
    // Тест получения действий
    const actions = await engine.getAvailableActions('princess');
    console.log('✅ Available actions:', actions.length, 'actions');
    console.log('   Actions:', JSON.stringify(actions.slice(0, 2), null, 2));
    
    // Тест валидации
    const testAction = { type: 'move', location: 'throne_room' };
    const isValid = await engine.validateAction(testAction, 'princess');
    console.log('✅ Action validation:', isValid);
    
    // Тест обработки действия
    const result = await engine.processAction(testAction, 'princess');
    console.log('✅ Action processing:', result.success ? 'SUCCESS' : 'FAILED');
    console.log('   Result:', JSON.stringify(result, null, 2));
    
    // Тест производительности
    const startTime = Date.now();
    for (let i = 0; i < 100; i++) {
        await engine.validateAction(testAction, 'princess');
    }
    const endTime = Date.now();
    console.log('✅ Performance: 100 validations in', endTime - startTime, 'ms');
    
    console.log('\n=== Simple Scheme Engine Test Complete ===');
}

testSimpleScheme().catch(console.error);