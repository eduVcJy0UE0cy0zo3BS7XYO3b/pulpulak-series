/**
 * Example showing how to integrate the new game engine with the existing server
 * This demonstrates how to switch between different games using the same infrastructure
 */

const GameEngineFactory = require('./engine/GameEngineFactory');

// Example: Creating different game engines
console.log('=== Available Games ===');
const availableGames = GameEngineFactory.getAvailableGames();
availableGames.forEach(game => {
    console.log(`${game.id}: ${game.name} (v${game.version}) - ${game.maxPlayers} players`);
    console.log(`  Features: ${Object.keys(game.features).filter(f => game.features[f]).join(', ')}`);
});

// Example: Validate game configurations
console.log('\n=== Validating Game Configurations ===');
availableGames.forEach(game => {
    const validation = GameEngineFactory.validateGame(game.id);
    console.log(`${game.id}: ${validation.valid ? '✅ Valid' : '❌ Invalid'}`);
    if (!validation.valid) {
        validation.errors.forEach(error => console.log(`  - ${error}`));
    }
});

// Example: Start a pulpulak game
console.log('\n=== Starting Pulpulak Game ===');
const pulpulakEngine = GameEngineFactory.createEngine('pulpulak');
const pulpulakGame = pulpulakEngine.startGame('room_001', {
    princess: { id: 'player1', name: 'Alice' },
    helper: { id: 'player2', name: 'Bob' }
});

console.log('Pulpulak game started:');
console.log(`- Scene: ${pulpulakGame.scene.title}`);
console.log(`- Characters: ${Object.keys(pulpulakGame.characters).join(', ')}`);
console.log(`- Turn order: ${pulpulakGame.turnOrder}`);

// Example: Start a detective game  
console.log('\n=== Starting Detective Game ===');
const detectiveEngine = GameEngineFactory.createEngine('detective');
const detectiveGame = detectiveEngine.startGame('room_002', {
    detective: { id: 'player3', name: 'Charlie' },
    journalist: { id: 'player4', name: 'Dana' }
});

console.log('Detective game started:');
console.log(`- Scene: ${detectiveGame.scene.title}`);
console.log(`- Characters: ${Object.keys(detectiveGame.characters).join(', ')}`);
console.log(`- Turn order: ${detectiveGame.turnOrder}`);

// Example: Making choices in different games
console.log('\n=== Making Choices ===');

// Pulpulak choice
const pulpulakChoice = pulpulakEngine.makeChoice('room_001', 'player1', 'prepare_morning', 'princess');
if (pulpulakChoice.success) {
    console.log(`Pulpulak: ${pulpulakChoice.message}`);
}

// Detective choice
const detectiveChoice = detectiveEngine.makeChoice('room_002', 'player3', 'examine_scene', 'detective');
if (detectiveChoice.success) {
    console.log(`Detective: ${detectiveChoice.message}`);
}

// Example: Movement in different games
console.log('\n=== Movement Examples ===');

// Pulpulak movement (medieval castle)
const pulpulakMovement = pulpulakEngine.makeChoice('room_001', 'player1', 'move_to_corridor_upper', 'princess');
if (pulpulakMovement.success) {
    console.log(`Pulpulak: Moved to upper corridor`);
}

// Detective movement (modern city)
const detectiveMovement = detectiveEngine.makeChoice('room_002', 'player3', 'move_to_garden', 'detective');
if (detectiveMovement.success) {
    console.log(`Detective: Moved to garden`);
}

/**
 * Integration with Express server would look like this:
 */
function createGameRoutes(app) {
    // Route to list available games
    app.get('/api/games', (req, res) => {
        res.json(GameEngineFactory.getAvailableGames());
    });
    
    // Route to start a game of a specific type
    app.post('/api/games/:gameId/start', (req, res) => {
        try {
            const { gameId } = req.params;
            const { roomId, players } = req.body;
            
            const engine = GameEngineFactory.createEngine(gameId);
            const gameData = engine.startGame(roomId, players);
            
            res.json({ success: true, gameData });
        } catch (error) {
            res.status(400).json({ success: false, message: error.message });
        }
    });
    
    // Route to make a choice (works for any game)
    app.post('/api/games/:gameId/choice', (req, res) => {
        try {
            const { gameId } = req.params;
            const { roomId, playerId, choiceId, character } = req.body;
            
            const engine = GameEngineFactory.createEngine(gameId);
            const result = engine.makeChoice(roomId, playerId, choiceId, character);
            
            res.json(result);
        } catch (error) {
            res.status(400).json({ success: false, message: error.message });
        }
    });
}

/**
 * Socket.IO integration would look like this:
 */
function createGameSockets(io) {
    // Store engines by room
    const roomEngines = new Map();
    
    io.on('connection', (socket) => {
        socket.on('start_game', ({ gameId, roomId, players }) => {
            try {
                const engine = GameEngineFactory.createEngine(gameId);
                const gameData = engine.startGame(roomId, players);
                
                roomEngines.set(roomId, { engine, gameId });
                
                socket.join(roomId);
                io.to(roomId).emit('game_started', gameData);
            } catch (error) {
                socket.emit('error', { message: error.message });
            }
        });
        
        socket.on('make_choice', ({ roomId, playerId, choiceId, character }) => {
            const roomData = roomEngines.get(roomId);
            if (!roomData) {
                socket.emit('error', { message: 'Game not found' });
                return;
            }
            
            const result = roomData.engine.makeChoice(roomId, playerId, choiceId, character);
            
            if (result.success) {
                io.to(roomId).emit('choice_result', result);
            } else {
                socket.emit('error', result);
            }
        });
    });
}

console.log('\n=== Integration Complete ===');
console.log('The same engine infrastructure can now power multiple different games!');
console.log('Add new games by creating new config classes - no engine changes needed.');

module.exports = {
    createGameRoutes,
    createGameSockets
};