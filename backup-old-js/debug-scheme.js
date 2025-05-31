const PureSchemeGameEngine = require('./game/functional/integration/pure-scheme-engine-fixed');

async function debug() {
    const engine = new PureSchemeGameEngine();
    
    // Wait for initialization
    await new Promise(resolve => {
        const checkReady = () => {
            if (engine.isReady()) {
                resolve();
            } else {
                setTimeout(checkReady, 100);
            }
        };
        checkReady();
    });
    
    console.log('Engine initialized. Current state:', JSON.stringify(engine.getCurrentState(), null, 2));
    
    // Test get available actions
    try {
        const actions = await engine.getAvailableActions('princess');
        console.log('Available actions for princess:', JSON.stringify(actions, null, 2));
    } catch (error) {
        console.error('Error getting actions:', error);
    }
    
    // Test direct Scheme evaluation
    try {
        const directResult = await engine.biwa.evaluate(`
            (let ((current-state (get-current-state)))
              (get-available-actions current-state 'princess))
        `);
        console.log('Direct Scheme result:', directResult);
        const jsResult = engine.schemeValueToJS(directResult);
        console.log('Converted to JS:', JSON.stringify(jsResult, null, 2));
    } catch (error) {
        console.error('Direct Scheme error:', error);
    }
    
    // Test action processing
    try {
        const action = { type: 'move', location: 'throne_room' };
        const result = await engine.processAction(action, 'princess');
        console.log('Action result:', JSON.stringify(result, null, 2));
    } catch (error) {
        console.error('Action processing error:', error);
    }
}

debug().catch(console.error);