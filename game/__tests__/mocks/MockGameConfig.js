const PulpulakGameConfig = require('../../../games/pulpulak/PulpulakGameConfig');

/**
 * Mock GameConfig for testing - extends real PulpulakGameConfig
 * This ensures tests continue to work with real game data while using the new architecture
 */
class MockGameConfig extends PulpulakGameConfig {
    constructor(options = {}) {
        super();
        this.options = options;
        this.gameId = 'test_pulpulak';
        this.gameName = 'Test Pulpulak Game';
        this.gameVersion = '1.0.0-test';
    }

    // Inherit all methods from PulpulakGameConfig
    // Override if needed for testing
    
    getMetadata() {
        return {
            id: this.gameId,
            name: this.gameName,
            version: this.gameVersion,
            description: 'Mock version of Pulpulak game for testing',
            authors: ['Test Author'],
            maxPlayers: 2,
            minPlayers: 2,
            tags: ['cooperative', 'text-adventure', 'test'],
            difficulty: 'medium'
        };
    }
}

module.exports = MockGameConfig;