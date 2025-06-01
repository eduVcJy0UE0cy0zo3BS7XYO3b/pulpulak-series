const GameRegistry = require('../../game/GameRegistry');
const path = require('path');

describe('Server + GameRegistry Integration', () => {
    let gameRegistry;

    beforeEach(() => {
        gameRegistry = new GameRegistry();
        gameRegistry.gamesDirectory = path.join(__dirname, '..', '..', 'games');
    });

    describe('GameRegistry integration with real server components', () => {
        test('should scan and load real games successfully', async () => {
            await gameRegistry.scanGames();
            
            const availableGames = gameRegistry.getAvailableGames();
            expect(availableGames.length).toBeGreaterThanOrEqual(2);
            
            // Verify games have all required API data
            availableGames.forEach(game => {
                expect(game).toHaveProperty('id');
                expect(game).toHaveProperty('name');
                expect(game).toHaveProperty('description');
                expect(game).toHaveProperty('minPlayers');
                expect(game).toHaveProperty('maxPlayers');
                expect(game).toHaveProperty('roles');
            });
        });

        test('should load game configurations with client data', async () => {
            await gameRegistry.scanGames();
            
            const pulpulakConfig = await gameRegistry.getGameConfig('pulpulak');
            expect(pulpulakConfig).toBeTruthy();
            
            const clientData = pulpulakConfig.getClientData();
            expect(clientData).toHaveProperty('metadata');
            expect(clientData).toHaveProperty('uiConfig');
            expect(clientData.metadata.id).toBe('pulpulak');
        });

        test('should maintain backward compatibility with existing game interfaces', async () => {
            await gameRegistry.scanGames();
            
            const pulpulakConfig = await gameRegistry.getGameConfig('pulpulak');
            
            // Test that all required game methods still exist
            expect(typeof pulpulakConfig.getStoryData).toBe('function');
            expect(typeof pulpulakConfig.getLocationData).toBe('function');
            expect(typeof pulpulakConfig.getNPCData).toBe('function');
            expect(typeof pulpulakConfig.getQuestData).toBe('function');
            expect(typeof pulpulakConfig.getCharacters).toBe('function');
        });

        test('should handle performance requirements', async () => {
            const startTime = Date.now();
            await gameRegistry.scanGames();
            const scanTime = Date.now() - startTime;
            
            expect(scanTime).toBeLessThan(1000); // Scan should complete within 1 second
            
            const loadStartTime = Date.now();
            await gameRegistry.getGameConfig('pulpulak');
            await gameRegistry.getGameConfig('detective');
            const loadTime = Date.now() - loadStartTime;
            
            expect(loadTime).toBeLessThan(500); // Loading should be fast
        });

        test('should handle concurrent access safely', async () => {
            await gameRegistry.scanGames();
            
            // Simulate concurrent API requests
            const promises = [
                gameRegistry.getGameConfig('pulpulak'),
                gameRegistry.getGameConfig('detective'),
                gameRegistry.getGameConfig('pulpulak'), // Duplicate request
                gameRegistry.getAvailableGames(),
                gameRegistry.getGameConfig('detective') // Another duplicate
            ];
            
            const results = await Promise.all(promises);
            
            // All requests should succeed
            expect(results[0]).toBeTruthy(); // pulpulak config
            expect(results[1]).toBeTruthy(); // detective config
            expect(results[2]).toBe(results[0]); // Same instance (cached)
            expect(Array.isArray(results[3])).toBe(true); // games list
            expect(results[4]).toBe(results[1]); // Same instance (cached)
        });

        test('should manage memory efficiently', async () => {
            await gameRegistry.scanGames();
            
            // Set low memory limit for testing
            gameRegistry.maxLoadedGames = 1;
            
            // Load games to test memory management
            await gameRegistry.getGameConfig('pulpulak');
            expect(gameRegistry.games.size).toBe(1);
            
            // Add small delay to ensure different timestamps
            await new Promise(resolve => setTimeout(resolve, 1));
            await gameRegistry.getGameConfig('detective');
            expect(gameRegistry.games.size).toBeLessThanOrEqual(gameRegistry.maxLoadedGames);
            
            // Most recently accessed should be in memory
            expect(gameRegistry.isGameLoaded('detective')).toBe(true);
        });

        test('should provide accurate statistics', async () => {
            await gameRegistry.scanGames();
            
            const stats = gameRegistry.getStats();
            expect(stats.totalGames).toBeGreaterThanOrEqual(2);
            expect(stats.loadedGames).toBe(0); // Nothing loaded yet
            
            await gameRegistry.getGameConfig('pulpulak');
            
            const newStats = gameRegistry.getStats();
            expect(newStats.loadedGames).toBe(1);
        });
    });

    describe('Error handling and edge cases', () => {
        test('should handle missing games gracefully', async () => {
            await gameRegistry.scanGames();
            
            const nonExistentConfig = await gameRegistry.getGameConfig('nonexistent');
            expect(nonExistentConfig).toBeNull();
        });

        test('should handle corrupted game directories', async () => {
            // Point to a test directory with mixed content
            gameRegistry.gamesDirectory = __dirname;
            
            // Should not throw, just return empty or filtered results
            await expect(gameRegistry.scanGames()).resolves.not.toThrow();
            
            const games = gameRegistry.getAvailableGames();
            // Should return only valid games (likely none from test directory)
            expect(Array.isArray(games)).toBe(true);
        });

        test('should handle filesystem permission errors', async () => {
            // Point to a directory that might not exist
            gameRegistry.gamesDirectory = '/nonexistent/path';
            
            await expect(gameRegistry.scanGames()).resolves.not.toThrow();
            
            const games = gameRegistry.getAvailableGames();
            expect(games).toHaveLength(0);
        });
    });
});

// Simple functionality test without starting actual server
describe('Server Components Integration', () => {
    test('server should export correct components', () => {
        // Test that server module exports what we expect
        const serverModule = require('../../server.js');
        
        expect(serverModule).toHaveProperty('app');
        expect(serverModule).toHaveProperty('server');
        expect(serverModule).toHaveProperty('gameRegistry');
        expect(typeof serverModule.gameRegistry.scanGames).toBe('function');
    });
});