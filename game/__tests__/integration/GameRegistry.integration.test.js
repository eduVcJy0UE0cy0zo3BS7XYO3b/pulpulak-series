const GameRegistry = require('../../GameRegistry');
const path = require('path');

describe('GameRegistry Integration Tests', () => {
    let gameRegistry;

    beforeEach(() => {
        gameRegistry = new GameRegistry();
        // Use real games directory for integration tests
        gameRegistry.gamesDirectory = path.join(__dirname, '..', '..', '..', 'games');
    });

    describe('Real game discovery', () => {
        test('should discover existing Pulpulak game', async () => {
            await gameRegistry.scanGames();
            
            const availableGames = gameRegistry.getAvailableGames();
            const pulpulakGame = availableGames.find(game => game.id === 'pulpulak');
            
            expect(pulpulakGame).toBeTruthy();
            expect(pulpulakGame.name).toBe('Княжна Пулпулак');
            expect(pulpulakGame.description).toContain('Кооперативная');
        });

        test('should discover existing Detective game', async () => {
            await gameRegistry.scanGames();
            
            const availableGames = gameRegistry.getAvailableGames();
            const detectiveGame = availableGames.find(game => game.id === 'detective');
            
            expect(detectiveGame).toBeTruthy();
            expect(detectiveGame.name).toBe('Детективное дело');
        });

        test('should load real game configurations', async () => {
            await gameRegistry.scanGames();
            
            // Load Pulpulak config
            const pulpulakConfig = await gameRegistry.getGameConfig('pulpulak');
            expect(pulpulakConfig).toBeTruthy();
            expect(typeof pulpulakConfig.getStoryData).toBe('function');
            expect(typeof pulpulakConfig.getLocationData).toBe('function');
            expect(typeof pulpulakConfig.getNPCData).toBe('function');
            expect(typeof pulpulakConfig.getQuestData).toBe('function');
            
            // Load Detective config
            const detectiveConfig = await gameRegistry.getGameConfig('detective');
            expect(detectiveConfig).toBeTruthy();
            expect(typeof detectiveConfig.getStoryData).toBe('function');
            expect(typeof detectiveConfig.getLocationData).toBe('function');
            expect(typeof detectiveConfig.getNPCData).toBe('function');
            expect(typeof detectiveConfig.getQuestData).toBe('function');
        });
    });

    describe('Real game data validation', () => {
        test('Pulpulak game should have valid story data', async () => {
            await gameRegistry.scanGames();
            const config = await gameRegistry.getGameConfig('pulpulak');
            
            const storyData = config.getStoryData();
            expect(storyData).toBeTruthy();
            expect(typeof storyData === 'object' || typeof storyData === 'function').toBe(true);
            
            // Check for essential story elements (Pulpulak returns a class)
            if (typeof storyData === 'function') {
                const instance = new storyData();
                expect(instance).toBeTruthy();
            } else {
                expect(storyData.prologue || storyData.crime_scene_arrival).toBeDefined();
            }
        });

        test('Detective game should have valid story data', async () => {
            await gameRegistry.scanGames();
            const config = await gameRegistry.getGameConfig('detective');
            
            const storyData = config.getStoryData();
            expect(storyData).toBeTruthy();
            expect(typeof storyData).toBe('object');
        });

        test('both games should provide client-safe data', async () => {
            await gameRegistry.scanGames();
            
            const pulpulakConfig = await gameRegistry.getGameConfig('pulpulak');
            const detectiveConfig = await gameRegistry.getGameConfig('detective');
            
            const pulpulakClientData = pulpulakConfig.getClientData();
            const detectiveClientData = detectiveConfig.getClientData();
            
            // Both should have metadata and uiConfig
            expect(pulpulakClientData.metadata).toBeDefined();
            expect(pulpulakClientData.uiConfig).toBeDefined();
            expect(detectiveClientData.metadata).toBeDefined();
            expect(detectiveClientData.uiConfig).toBeDefined();
            
            // Metadata should contain required fields
            expect(pulpulakClientData.metadata.id).toBe('pulpulak');
            expect(pulpulakClientData.metadata.name).toBeTruthy();
            expect(detectiveClientData.metadata.id).toBe('detective');
            expect(detectiveClientData.metadata.name).toBeTruthy();
        });
    });

    describe('Performance with real games', () => {
        test('should scan real games quickly', async () => {
            const startTime = Date.now();
            await gameRegistry.scanGames();
            const endTime = Date.now();
            
            const scanTime = endTime - startTime;
            expect(scanTime).toBeLessThan(1000); // Should complete within 1 second
        });

        test('should load game configs quickly', async () => {
            await gameRegistry.scanGames();
            
            const startTime = Date.now();
            await gameRegistry.getGameConfig('pulpulak');
            await gameRegistry.getGameConfig('detective');
            const endTime = Date.now();
            
            const loadTime = endTime - startTime;
            expect(loadTime).toBeLessThan(500); // Should load within 500ms
        });

        test('should handle concurrent loading of real games', async () => {
            await gameRegistry.scanGames();
            
            const startTime = Date.now();
            const [pulpulakConfig, detectiveConfig] = await Promise.all([
                gameRegistry.getGameConfig('pulpulak'),
                gameRegistry.getGameConfig('detective')
            ]);
            const endTime = Date.now();
            
            expect(pulpulakConfig).toBeTruthy();
            expect(detectiveConfig).toBeTruthy();
            
            const loadTime = endTime - startTime;
            expect(loadTime).toBeLessThan(1000); // Concurrent loading should be fast
        });
    });

    describe('Memory management with real games', () => {
        test('should manage memory when loading multiple real games', async () => {
            await gameRegistry.scanGames();
            
            // Set low memory limit
            gameRegistry.maxLoadedGames = 1;
            
            // Load both games with small delay to ensure different timestamps
            await gameRegistry.getGameConfig('pulpulak');
            await new Promise(resolve => setTimeout(resolve, 1));
            await gameRegistry.getGameConfig('detective');
            
            // Should respect memory limit
            expect(gameRegistry.games.size).toBeLessThanOrEqual(gameRegistry.maxLoadedGames);
            
            // Should have the most recently accessed game (detective)
            expect(gameRegistry.isGameLoaded('detective')).toBe(true);
            expect(gameRegistry.isGameLoaded('pulpulak')).toBe(false);
        });

        test('should cleanup unused real games', async () => {
            await gameRegistry.scanGames();
            
            // Set very short timeout
            gameRegistry.unloadTimeout = 50;
            
            // Load a game
            await gameRegistry.getGameConfig('pulpulak');
            expect(gameRegistry.isGameLoaded('pulpulak')).toBe(true);
            
            // Wait for timeout
            await new Promise(resolve => setTimeout(resolve, 100));
            
            // Trigger cleanup
            gameRegistry.cleanupUnusedGames();
            
            // Should be unloaded
            expect(gameRegistry.isGameLoaded('pulpulak')).toBe(false);
        });
    });

    describe('Error handling with real filesystem', () => {
        test('should handle permission errors gracefully', async () => {
            // Point to a directory we might not have access to
            gameRegistry.gamesDirectory = '/root/nonexistent';
            
            // Should not throw
            await expect(gameRegistry.scanGames()).resolves.not.toThrow();
            
            // Should return empty list
            const availableGames = gameRegistry.getAvailableGames();
            expect(availableGames).toHaveLength(0);
        });

        test('should handle corrupted game directory gracefully', async () => {
            // Point to this test file instead of a directory
            gameRegistry.gamesDirectory = __filename;
            
            // Should not throw
            await expect(gameRegistry.scanGames()).resolves.not.toThrow();
            
            // Should return empty list
            const availableGames = gameRegistry.getAvailableGames();
            expect(availableGames).toHaveLength(0);
        });
    });

    describe('Statistics and monitoring', () => {
        test('should provide accurate statistics', async () => {
            await gameRegistry.scanGames();
            
            const stats = gameRegistry.getStats();
            
            expect(stats.totalGames).toBeGreaterThanOrEqual(2); // At least Pulpulak and Detective
            expect(stats.loadedGames).toBe(0); // Nothing loaded yet
            expect(stats.maxLoadedGames).toBe(gameRegistry.maxLoadedGames);
            expect(stats.unloadTimeout).toBe(gameRegistry.unloadTimeout);
            
            // Load a game and check stats again
            await gameRegistry.getGameConfig('pulpulak');
            
            const newStats = gameRegistry.getStats();
            expect(newStats.loadedGames).toBe(1);
        });

        test('should track game metadata without loading', async () => {
            await gameRegistry.scanGames();
            
            const pulpulakMetadata = gameRegistry.getGameMetadata('pulpulak');
            expect(pulpulakMetadata).toBeTruthy();
            expect(pulpulakMetadata.id).toBe('pulpulak');
            expect(pulpulakMetadata.name).toBe('Княжна Пулпулак');
            
            // Game should not be loaded
            expect(gameRegistry.isGameLoaded('pulpulak')).toBe(false);
        });
    });
});