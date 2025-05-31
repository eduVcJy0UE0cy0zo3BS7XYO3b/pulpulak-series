const PureSchemeSimpleEngine = require('./game/functional/integration/pure-scheme-simple');

async function debug() {
    const engine = new PureSchemeSimpleEngine();
    
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
    
    console.log('Engine ready');
    
    // Прямой тест Scheme кода
    try {
        const result = await engine.biwa.evaluate(`
            (let ((action '(move throne_room))
                  (character 'princess)
                  (current-state (js-get-state)))
              (if (validate-action-simple action character)
                'success
                'error))
        `);
        
        console.log('Direct result:', result);
        console.log('Result type:', typeof result);
        console.log('Result name:', result && result.name);
        console.log('Result toString:', result && result.toString());
    } catch (error) {
        console.error('Direct test error:', error);
    }
}

debug().catch(console.error);