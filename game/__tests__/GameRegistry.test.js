const fs = require('fs').promises;
const path = require('path');
const GameRegistry = require('../GameRegistry');

// Mock games for testing
const mockGamesDir = path.join(__dirname, 'mocks', 'testGames');

describe('GameRegistry', () => {
    let gameRegistry;
    let originalGamesDir;

    beforeEach(async () => {
        // Create fresh instance for each test
        gameRegistry = new GameRegistry();
        
        // Override games directory for testing
        originalGamesDir = gameRegistry.gamesDirectory;
        gameRegistry.gamesDirectory = mockGamesDir;
        
        // Ensure test games directory exists
        await fs.mkdir(mockGamesDir, { recursive: true });
        
        // Clear any cached data
        gameRegistry.games.clear();
        gameRegistry.gameMetadata.clear();
        gameRegistry.lastAccess.clear();
    });

    afterEach(async () => {
        // Restore original directory
        if (originalGamesDir) {
            gameRegistry.gamesDirectory = originalGamesDir;
        }
        
        // Clean up test games directory
        try {
            await fs.rm(mockGamesDir, { recursive: true, force: true });
        } catch (err) {
            // Ignore cleanup errors
        }
    });

    describe('Game scanning', () => {
        test('should automatically discover games in /games/ directory', async () => {
            // Setup: Create mock game directories
            await createMockGame('testgame1', {
                id: 'testgame1',
                name: 'Test Game 1',
                description: 'A test game'
            });
            await createMockGame('testgame2', {
                id: 'testgame2', 
                name: 'Test Game 2',
                description: 'Another test game'
            });

            // Action
            await gameRegistry.scanGames();

            // Assertion
            const availableGames = gameRegistry.getAvailableGames();
            expect(availableGames).toHaveLength(2);
            expect(availableGames.map(g => g.id)).toContain('testgame1');
            expect(availableGames.map(g => g.id)).toContain('testgame2');
        });

        test('should handle missing game directories gracefully', async () => {
            // Setup: Point to non-existent directory
            gameRegistry.gamesDirectory = path.join(__dirname, 'nonexistent');

            // Action & Assertion - should not throw
            await expect(gameRegistry.scanGames()).resolves.not.toThrow();
            
            const availableGames = gameRegistry.getAvailableGames();
            expect(availableGames).toHaveLength(0);
        });

        test('should handle games with missing config files', async () => {
            // Setup: Create directory without GameConfig.js
            const gameDir = path.join(mockGamesDir, 'brokengame');
            await fs.mkdir(gameDir, { recursive: true });

            // Action & Assertion
            await expect(gameRegistry.scanGames()).resolves.not.toThrow();
            
            const availableGames = gameRegistry.getAvailableGames();
            expect(availableGames).toHaveLength(0);
        });

        test('should validate game config interfaces', async () => {
            // Setup: Create game with invalid config
            await createMockGame('invalidgame', {
                // Missing required fields
                name: 'Invalid Game'
                // Missing id, description, etc.
            });

            // Action
            await gameRegistry.scanGames();

            // Assertion - invalid games should be filtered out
            const availableGames = gameRegistry.getAvailableGames();
            expect(availableGames.map(g => g.id)).not.toContain('invalidgame');
        });

        test('should handle corrupted game config files', async () => {
            // Setup: Create game with syntax error in config
            const gameDir = path.join(mockGamesDir, 'corruptedgame');
            await fs.mkdir(gameDir, { recursive: true });
            
            const configPath = path.join(gameDir, 'CorruptedgameGameConfig.js');
            await fs.writeFile(configPath, 'invalid javascript syntax {{{');

            // Action & Assertion
            await expect(gameRegistry.scanGames()).resolves.not.toThrow();
            
            const availableGames = gameRegistry.getAvailableGames();
            expect(availableGames.map(g => g.id)).not.toContain('corruptedgame');
        });
    });

    describe('Lazy loading', () => {
        test('should load games only when requested', async () => {
            // Setup
            await createMockGame('lazygame', {
                id: 'lazygame',
                name: 'Lazy Game',
                description: 'Test lazy loading'
            });
            await gameRegistry.scanGames();

            // Assertion - game should not be loaded yet
            expect(gameRegistry.games.has('lazygame')).toBe(false);

            // Action - request game config
            const config = await gameRegistry.getGameConfig('lazygame');

            // Assertion - now it should be loaded
            expect(config).toBeTruthy();
            expect(gameRegistry.games.has('lazygame')).toBe(true);
        });

        test('should cache loaded games', async () => {
            // Setup
            await createMockGame('cachedgame', {
                id: 'cachedgame',
                name: 'Cached Game',
                description: 'Test caching'
            });
            await gameRegistry.scanGames();

            // Action - load game twice
            const config1 = await gameRegistry.getGameConfig('cachedgame');
            const config2 = await gameRegistry.getGameConfig('cachedgame');

            // Assertion - should be same instance (cached)
            expect(config1).toBe(config2);
        });

        test('should handle loading failures', async () => {
            // Action - try to load non-existent game
            const config = await gameRegistry.getGameConfig('nonexistent');

            // Assertion
            expect(config).toBeNull();
        });

        test('should update last access time when loading games', async () => {
            // Setup
            await createMockGame('accessgame', {
                id: 'accessgame',
                name: 'Access Game',
                description: 'Test access tracking'
            });
            await gameRegistry.scanGames();

            const beforeTime = Date.now();
            
            // Action
            await gameRegistry.getGameConfig('accessgame');
            
            const afterTime = Date.now();

            // Assertion
            const lastAccess = gameRegistry.lastAccess.get('accessgame');
            expect(lastAccess).toBeGreaterThanOrEqual(beforeTime);
            expect(lastAccess).toBeLessThanOrEqual(afterTime);
        });
    });

    describe('Memory management', () => {
        test('should unload unused games after timeout', async () => {
            // Setup
            await createMockGame('timeoutgame', {
                id: 'timeoutgame',
                name: 'Timeout Game',
                description: 'Test timeout unloading'
            });
            await gameRegistry.scanGames();
            
            // Set short timeout for testing
            gameRegistry.unloadTimeout = 100; // 100ms

            // Action - load game
            await gameRegistry.getGameConfig('timeoutgame');
            expect(gameRegistry.games.has('timeoutgame')).toBe(true);

            // Wait for timeout
            await new Promise(resolve => setTimeout(resolve, 150));
            
            // Trigger cleanup
            gameRegistry.cleanupUnusedGames();

            // Assertion - game should be unloaded
            expect(gameRegistry.games.has('timeoutgame')).toBe(false);
        });

        test('should respect memory limits', async () => {
            // Setup - create multiple games
            const gameCount = 5;
            for (let i = 1; i <= gameCount; i++) {
                await createMockGame(`memgame${i}`, {
                    id: `memgame${i}`,
                    name: `Memory Game ${i}`,
                    description: `Test memory limit ${i}`
                });
            }
            await gameRegistry.scanGames();
            
            // Set low memory limit
            gameRegistry.maxLoadedGames = 2;

            // Action - load games one by one to trigger memory management
            for (let i = 1; i <= gameCount; i++) {
                await gameRegistry.getGameConfig(`memgame${i}`);
                // Small delay to ensure different timestamps
                await new Promise(resolve => setTimeout(resolve, 1));
            }

            // Assertion - only maxLoadedGames should be in memory
            expect(gameRegistry.games.size).toBeLessThanOrEqual(gameRegistry.maxLoadedGames);
        });

        test('should keep recently accessed games in memory', async () => {
            // Setup
            await createMockGame('recentgame', {
                id: 'recentgame',
                name: 'Recent Game',
                description: 'Test recent access'
            });
            await createMockGame('oldgame', {
                id: 'oldgame',
                name: 'Old Game', 
                description: 'Test old access'
            });
            await gameRegistry.scanGames();
            
            gameRegistry.maxLoadedGames = 1;

            // Action - load old game first, then recent game
            await gameRegistry.getGameConfig('oldgame');
            await new Promise(resolve => setTimeout(resolve, 10));
            await gameRegistry.getGameConfig('recentgame');

            // Assertion - recent game should be kept, old game evicted
            expect(gameRegistry.games.has('recentgame')).toBe(true);
            expect(gameRegistry.games.has('oldgame')).toBe(false);
        });
    });

    describe('Error handling', () => {
        test('should handle filesystem errors gracefully', async () => {
            // Setup - create game then make directory unreadable (if possible)
            await createMockGame('fserror', {
                id: 'fserror',
                name: 'FS Error Game',
                description: 'Test filesystem errors'
            });

            // Mock fs.readdir to throw error
            const originalReaddir = fs.readdir;
            fs.readdir = jest.fn().mockRejectedValue(new Error('Permission denied'));

            try {
                // Action & Assertion
                await expect(gameRegistry.scanGames()).resolves.not.toThrow();
                
                const availableGames = gameRegistry.getAvailableGames();
                expect(availableGames).toHaveLength(0);
            } finally {
                // Restore original function
                fs.readdir = originalReaddir;
            }
        });

        test('should handle concurrent access safely', async () => {
            // Setup
            await createMockGame('concurrent', {
                id: 'concurrent',
                name: 'Concurrent Game',
                description: 'Test concurrent access'
            });
            await gameRegistry.scanGames();

            // Action - load same game concurrently
            const promises = Array(10).fill().map(() => 
                gameRegistry.getGameConfig('concurrent')
            );
            
            const results = await Promise.all(promises);

            // Assertion - all should return same instance
            const firstResult = results[0];
            results.forEach(result => {
                expect(result).toBe(firstResult);
            });
        });
    });

    // Helper function to create mock games
    async function createMockGame(gameId, metadata) {
        const gameDir = path.join(mockGamesDir, gameId);
        await fs.mkdir(gameDir, { recursive: true });
        
        const configFileName = `${gameId.charAt(0).toUpperCase() + gameId.slice(1)}GameConfig.js`;
        const configPath = path.join(gameDir, configFileName);
        
        const configContent = `
class ${gameId.charAt(0).toUpperCase() + gameId.slice(1)}GameConfig {
    static getMetadata() {
        return ${JSON.stringify(metadata, null, 8)};
    }
    
    getClientData() {
        return {
            metadata: this.constructor.getMetadata(),
            uiConfig: {
                theme: 'test',
                primaryColor: '#000000'
            }
        };
    }
    
    // Implement required IGameConfig interface methods
    getStoryData() { return {}; }
    getLocationData() { return {}; }
    getNPCData() { return {}; }
    getQuestData() { return {}; }
}

module.exports = ${gameId.charAt(0).toUpperCase() + gameId.slice(1)}GameConfig;
`;
        
        await fs.writeFile(configPath, configContent);
    }
});