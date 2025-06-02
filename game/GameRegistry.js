const fs = require('fs').promises;
const path = require('path');

class GameRegistry {
    constructor() {
        this.games = new Map(); // gameId -> GameConfig instance
        this.gameMetadata = new Map(); // gameId -> metadata
        this.lastAccess = new Map(); // gameId -> timestamp
        this.gamesDirectory = path.join(__dirname, '..', 'games');
        this.unloadTimeout = 30 * 60 * 1000; // 30 minutes
        this.maxLoadedGames = 10; // Maximum number of games to keep in memory
        this.scanPromise = null; // To handle concurrent scan requests
        this.loadingPromises = new Map(); // To handle concurrent loading of same game
    }

    /**
     * Scan the games directory and discover available games
     */
    async scanGames() {
        // Prevent concurrent scans
        if (this.scanPromise) {
            return this.scanPromise;
        }

        this.scanPromise = this._performScan();
        try {
            await this.scanPromise;
        } finally {
            this.scanPromise = null;
        }
    }

    async _performScan() {
        try {
            // Clear existing metadata
            this.gameMetadata.clear();

            // Check if games directory exists
            try {
                await fs.access(this.gamesDirectory);
            } catch (err) {
                // Games directory doesn't exist, no games available
                return;
            }

            // Read all subdirectories in games folder
            const entries = await fs.readdir(this.gamesDirectory, { withFileTypes: true });
            const gameDirectories = entries
                .filter(entry => entry.isDirectory() && !entry.name.startsWith('.'))
                .map(entry => entry.name);

            // Process each game directory
            await Promise.all(gameDirectories.map(async (gameDir) => {
                try {
                    await this._scanGameDirectory(gameDir);
                } catch (err) {
                    // Log error but don't fail entire scan
                    console.warn(`Failed to scan game directory ${gameDir}:`, err.message);
                }
            }));

        } catch (err) {
            console.error('Failed to scan games directory:', err.message);
            // Don't throw - just log and continue with empty game list
        }
    }

    async _scanGameDirectory(gameDir) {
        const gameDirPath = path.join(this.gamesDirectory, gameDir);
        
        // Look for GameConfig.js file (following naming convention)
        const configFileName = `${gameDir.charAt(0).toUpperCase() + gameDir.slice(1)}GameConfig.js`;
        const configPath = path.join(gameDirPath, configFileName);

        try {
            // Check if config file exists
            await fs.access(configPath);
            
            // Try to load and validate the config
            const GameConfigClass = require(configPath);
            
            // Validate that it has required static method
            if (typeof GameConfigClass.getMetadata !== 'function') {
                console.warn(`Game ${gameDir} missing getMetadata() static method`);
                return;
            }

            // Get metadata
            const metadata = GameConfigClass.getMetadata();
            
            // Validate metadata
            if (!this._validateMetadata(metadata)) {
                console.warn(`Game ${gameDir} has invalid metadata`);
                return;
            }

            // Store metadata
            this.gameMetadata.set(metadata.id, {
                ...metadata,
                configPath,
                GameConfigClass
            });

        } catch (err) {
            console.warn(`Failed to load game config for ${gameDir}:`, err.message);
        }
    }

    _validateMetadata(metadata) {
        return metadata &&
               typeof metadata.id === 'string' && metadata.id.length > 0 &&
               typeof metadata.name === 'string' && metadata.name.length > 0 &&
               typeof metadata.description === 'string';
    }

    /**
     * Get list of available games
     */
    getAvailableGames() {
        return Array.from(this.gameMetadata.values()).map(({ GameConfigClass, configPath, ...metadata }) => metadata);
    }

    /**
     * Get game configuration (lazy loading)
     */
    async getGameConfig(gameId) {
        // Check if already loaded
        if (this.games.has(gameId)) {
            this._updateLastAccess(gameId);
            return this.games.get(gameId);
        }

        // Check if game exists in metadata
        const metadata = this.gameMetadata.get(gameId);
        if (!metadata) {
            return null;
        }

        // Handle concurrent loading of same game
        if (this.loadingPromises.has(gameId)) {
            return this.loadingPromises.get(gameId);
        }

        // Start loading
        const loadingPromise = this._loadGame(gameId, metadata);
        this.loadingPromises.set(gameId, loadingPromise);

        try {
            const gameConfig = await loadingPromise;
            return gameConfig;
        } finally {
            this.loadingPromises.delete(gameId);
        }
    }

    async _loadGame(gameId, metadata) {
        try {
            // Enforce memory limits before loading
            this._enforceMemoryLimits();

            // Create instance
            const gameConfig = new metadata.GameConfigClass();
            
            // Initialize if the config supports it (for async data loading)
            if (typeof gameConfig.initialize === 'function') {
                await gameConfig.initialize();
            }
            
            // Store in cache
            this.games.set(gameId, gameConfig);
            this._updateLastAccess(gameId);

            return gameConfig;

        } catch (err) {
            console.error(`Failed to load game ${gameId}:`, err.message);
            return null;
        }
    }

    _updateLastAccess(gameId) {
        this.lastAccess.set(gameId, Date.now());
    }

    _enforceMemoryLimits() {
        // Unload games until we're under the limit
        while (this.games.size >= this.maxLoadedGames) {
            // Find least recently used game
            let oldestGameId = null;
            let oldestTime = Date.now();

            for (const [gameId, lastAccess] of this.lastAccess) {
                if (this.games.has(gameId) && lastAccess < oldestTime) {
                    oldestTime = lastAccess;
                    oldestGameId = gameId;
                }
            }

            // Unload oldest game
            if (oldestGameId) {
                this._unloadGame(oldestGameId);
            } else {
                // No games to unload, break to avoid infinite loop
                break;
            }
        }
    }

    _unloadGame(gameId) {
        this.games.delete(gameId);
        this.lastAccess.delete(gameId);
    }

    /**
     * Clean up unused games based on timeout
     */
    cleanupUnusedGames() {
        const now = Date.now();
        const gamesToUnload = [];

        for (const [gameId, lastAccess] of this.lastAccess) {
            if (this.games.has(gameId) && (now - lastAccess) > this.unloadTimeout) {
                gamesToUnload.push(gameId);
            }
        }

        gamesToUnload.forEach(gameId => this._unloadGame(gameId));
    }

    /**
     * Get metadata for a specific game without loading it
     */
    getGameMetadata(gameId) {
        const metadata = this.gameMetadata.get(gameId);
        if (!metadata) {
            return null;
        }

        const { GameConfigClass, configPath, ...publicMetadata } = metadata;
        return publicMetadata;
    }

    /**
     * Check if a game is currently loaded in memory
     */
    isGameLoaded(gameId) {
        return this.games.has(gameId);
    }

    /**
     * Get statistics about registry state
     */
    getStats() {
        return {
            totalGames: this.gameMetadata.size,
            loadedGames: this.games.size,
            maxLoadedGames: this.maxLoadedGames,
            unloadTimeout: this.unloadTimeout
        };
    }

    /**
     * Force unload a specific game
     */
    unloadGame(gameId) {
        if (this.games.has(gameId)) {
            this._unloadGame(gameId);
            return true;
        }
        return false;
    }

    /**
     * Force unload all games
     */
    unloadAllGames() {
        const loadedGames = Array.from(this.games.keys());
        loadedGames.forEach(gameId => this._unloadGame(gameId));
        return loadedGames.length;
    }
}

module.exports = GameRegistry;