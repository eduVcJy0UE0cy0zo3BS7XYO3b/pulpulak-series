const SchemeOnlyGameEngine = require('./game/functional/integration/scheme-only-engine');

async function debug() {
    const engine = new SchemeOnlyGameEngine();
    
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
    
    console.log('=== Debug Scheme-Only Engine ===');
    
    // Создаем игру
    const gameState = await engine.createGame('test', {});
    console.log('Game state created:', !!gameState);
    console.log('Game state type:', typeof gameState);
    
    // Тестируем прямой вызов Scheme функций
    try {
        const testState = await engine.biwa.evaluate(`
            (scheme-create-game "debug-room" '())
        `);
        console.log('Direct Scheme create game result:', !!testState);
        
        const testActions = await engine.biwa.evaluate(`
            (let ((state (scheme-create-game "debug-room" '())))
              (scheme-get-actions state 'princess))
        `);
        console.log('Direct Scheme actions result:', !!testActions);
        
        const testValidation = await engine.biwa.evaluate(`
            (let ((state (scheme-create-game "debug-room" '()))
                  (action '(action (type move) (location throne_room))))
              (scheme-validate-action state 'princess action))
        `);
        console.log('Direct Scheme validation result:', testValidation);
        
    } catch (error) {
        console.error('Direct Scheme test error:', error.message);
    }
}

debug().catch(console.error);