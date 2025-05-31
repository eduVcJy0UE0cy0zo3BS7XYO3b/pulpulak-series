const GameEngine = require('./GameEngine');
const PulpulakGameConfig = require('../games/pulpulak/PulpulakGameConfig');
const DetectiveGameConfig = require('../games/detective/DetectiveGameConfig');

/**
 * Factory for creating game engines with different configurations
 */
class GameEngineFactory {
    constructor() {
        this.registeredGames = new Map();
        this.registerDefaultGames();
    }

    /**
     * Register default games
     */
    registerDefaultGames() {
        this.registerGame('pulpulak', PulpulakGameConfig);
        this.registerGame('detective', DetectiveGameConfig);
    }

    /**
     * Register a new game configuration
     */
    registerGame(gameId, GameConfigClass) {
        this.registeredGames.set(gameId, GameConfigClass);
    }

    /**
     * Create a game engine instance for the specified game
     */
    createEngine(gameId) {
        const GameConfigClass = this.registeredGames.get(gameId);
        if (!GameConfigClass) {
            throw new Error(`Game '${gameId}' not found. Available games: ${Array.from(this.registeredGames.keys()).join(', ')}`);
        }

        const config = new GameConfigClass();
        return new GameEngine(config);
    }

    /**
     * Get list of available games
     */
    getAvailableGames() {
        const games = [];
        for (const [gameId, GameConfigClass] of this.registeredGames) {
            const config = new GameConfigClass();
            games.push({
                id: gameId,
                name: config.gameName,
                version: config.gameVersion,
                maxPlayers: config.maxPlayers,
                features: config.features
            });
        }
        return games;
    }

    /**
     * Validate a game configuration
     */
    validateGame(gameId) {
        const GameConfigClass = this.registeredGames.get(gameId);
        if (!GameConfigClass) {
            return { valid: false, errors: [`Game '${gameId}' not found`] };
        }

        try {
            const config = new GameConfigClass();
            return config.validate();
        } catch (error) {
            return { valid: false, errors: [error.message] };
        }
    }
}

// Singleton instance
const factory = new GameEngineFactory();

module.exports = factory;