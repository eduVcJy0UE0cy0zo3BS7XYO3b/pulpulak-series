const request = require('supertest');
const http = require('http');
const express = require('express');
const path = require('path');
const GameRegistry = require('../game/GameRegistry');

// Mock socket.io to avoid real socket connections during testing
jest.mock('socket.io', () => {
    return jest.fn(() => ({
        on: jest.fn(),
        emit: jest.fn(),
        to: jest.fn(() => ({
            emit: jest.fn()
        }))
    }));
});

// Mock SocketHandler to avoid real game logic during API testing
jest.mock('../network/socketHandler', () => {
    return jest.fn().mockImplementation(() => ({
        handleConnection: jest.fn()
    }));
});

describe('Multi-game Server API', () => {
    let app;
    let server;
    let gameRegistry;
    let originalGamesDir;

    beforeAll(async () => {
        // Create test instance of express app similar to server.js
        app = express();
        app.use(express.json());
        app.use(express.static('public'));

        // Create GameRegistry for testing
        gameRegistry = new GameRegistry();
        
        // Override games directory to use real games for integration testing
        originalGamesDir = gameRegistry.gamesDirectory;
        gameRegistry.gamesDirectory = path.join(__dirname, '..', 'games');
        
        // Scan games for testing
        await gameRegistry.scanGames();

        // Add API routes that we're going to implement
        app.get('/api/games', async (req, res) => {
            try {
                const games = gameRegistry.getAvailableGames();
                res.json(games);
            } catch (error) {
                res.status(500).json({ error: 'Failed to load games' });
            }
        });

        app.get('/api/games/:gameId/config', async (req, res) => {
            try {
                const { gameId } = req.params;
                const gameConfig = await gameRegistry.getGameConfig(gameId);
                
                if (!gameConfig) {
                    return res.status(404).json({ error: 'Game not found' });
                }

                const clientData = gameConfig.getClientData();
                res.json(clientData);
            } catch (error) {
                res.status(500).json({ error: 'Failed to load game configuration' });
            }
        });

        // Start test server
        server = http.createServer(app);
    });

    afterAll(async () => {
        if (server) {
            server.close();
        }
        if (originalGamesDir) {
            gameRegistry.gamesDirectory = originalGamesDir;
        }
    });

    describe('GET /api/games', () => {
        test('should return list of available games', async () => {
            const response = await request(app)
                .get('/api/games')
                .expect(200);

            expect(Array.isArray(response.body)).toBe(true);
            expect(response.body.length).toBeGreaterThanOrEqual(2); // At least Pulpulak and Detective

            // Check for required game properties
            response.body.forEach(game => {
                expect(game).toHaveProperty('id');
                expect(game).toHaveProperty('name');
                expect(game).toHaveProperty('description');
                expect(game).toHaveProperty('minPlayers');
                expect(game).toHaveProperty('maxPlayers');
            });
        });

        test('should include Pulpulak game', async () => {
            const response = await request(app)
                .get('/api/games')
                .expect(200);

            const pulpulakGame = response.body.find(game => game.id === 'pulpulak');
            expect(pulpulakGame).toBeTruthy();
            expect(pulpulakGame.name).toBe('Княжна Пулпулак');
            expect(pulpulakGame.minPlayers).toBe(2);
            expect(pulpulakGame.maxPlayers).toBe(2);
        });

        test('should include Detective game', async () => {
            const response = await request(app)
                .get('/api/games')
                .expect(200);

            const detectiveGame = response.body.find(game => game.id === 'detective');
            expect(detectiveGame).toBeTruthy();
            expect(detectiveGame.name).toBe('Детективное дело');
            expect(detectiveGame.minPlayers).toBe(2);
            expect(detectiveGame.maxPlayers).toBe(2);
        });

        test('should not expose sensitive data', async () => {
            const response = await request(app)
                .get('/api/games')
                .expect(200);

            response.body.forEach(game => {
                expect(game).not.toHaveProperty('GameConfigClass');
                expect(game).not.toHaveProperty('configPath');
                // Constructor is a built-in property, check that sensitive data isn't exposed
                expect(game.constructor).toBe(Object);
            });
        });

        test('should handle server errors gracefully', async () => {
            // Temporarily break gameRegistry
            const originalGetAvailableGames = gameRegistry.getAvailableGames;
            gameRegistry.getAvailableGames = () => {
                throw new Error('Test error');
            };

            const response = await request(app)
                .get('/api/games')
                .expect(500);

            expect(response.body).toHaveProperty('error');
            expect(response.body.error).toBe('Failed to load games');

            // Restore original method
            gameRegistry.getAvailableGames = originalGetAvailableGames;
        });

        test('should respond quickly', async () => {
            const startTime = Date.now();
            
            await request(app)
                .get('/api/games')
                .expect(200);
            
            const responseTime = Date.now() - startTime;
            expect(responseTime).toBeLessThan(200); // Should respond within 200ms
        });
    });

    describe('GET /api/games/:gameId/config', () => {
        test('should return Pulpulak game configuration', async () => {
            const response = await request(app)
                .get('/api/games/pulpulak/config')
                .expect(200);

            expect(response.body).toHaveProperty('metadata');
            expect(response.body).toHaveProperty('uiConfig');
            
            expect(response.body.metadata.id).toBe('pulpulak');
            expect(response.body.metadata.name).toBe('Княжна Пулпулак');
            expect(response.body.uiConfig.theme).toBe('medieval');
        });

        test('should return Detective game configuration', async () => {
            const response = await request(app)
                .get('/api/games/detective/config')
                .expect(200);

            expect(response.body).toHaveProperty('metadata');
            expect(response.body).toHaveProperty('uiConfig');
            
            expect(response.body.metadata.id).toBe('detective');
            expect(response.body.metadata.name).toBe('Детективное дело');
            expect(response.body.uiConfig.theme).toBe('detective');
        });

        test('should return 404 for non-existent games', async () => {
            const response = await request(app)
                .get('/api/games/nonexistent/config')
                .expect(404);

            expect(response.body).toHaveProperty('error');
            expect(response.body.error).toBe('Game not found');
        });

        test('should validate gameId parameter', async () => {
            // Test with various invalid gameIds
            const invalidIds = ['../../etc/passwd', '<script>', 'null', 'undefined'];
            
            for (const invalidId of invalidIds) {
                const response = await request(app)
                    .get(`/api/games/${encodeURIComponent(invalidId)}/config`)
                    .expect(404);

                expect(response.body).toHaveProperty('error');
                expect(response.body.error).toBe('Game not found');
            }
            
            // Test empty string separately as it may behave differently
            const emptyResponse = await request(app)
                .get('/api/games//config')
                .expect(404);
            // Express may handle empty params differently, so just check 404 status
        });

        test('should not expose sensitive game data', async () => {
            const response = await request(app)
                .get('/api/games/pulpulak/config')
                .expect(200);

            // Should not expose internal game methods or sensitive data
            expect(response.body).not.toHaveProperty('getStoryData');
            expect(response.body).not.toHaveProperty('getNPCData');
            expect(response.body).not.toHaveProperty('getQuestData');
            // Constructor is a built-in property, ensure it's the basic Object constructor
            expect(response.body.constructor).toBe(Object);
        });

        test('should handle loading errors gracefully', async () => {
            // Temporarily break gameRegistry
            const originalGetGameConfig = gameRegistry.getGameConfig;
            gameRegistry.getGameConfig = () => {
                throw new Error('Loading failed');
            };

            const response = await request(app)
                .get('/api/games/pulpulak/config')
                .expect(500);

            expect(response.body).toHaveProperty('error');
            expect(response.body.error).toBe('Failed to load game configuration');

            // Restore original method
            gameRegistry.getGameConfig = originalGetGameConfig;
        });

        test('should respond quickly', async () => {
            const startTime = Date.now();
            
            await request(app)
                .get('/api/games/pulpulak/config')
                .expect(200);
            
            const responseTime = Date.now() - startTime;
            expect(responseTime).toBeLessThan(200); // Should respond within 200ms
        });

        test('should handle concurrent requests safely', async () => {
            const promises = Array(10).fill().map(() => 
                request(app)
                    .get('/api/games/pulpulak/config')
                    .expect(200)
            );

            const responses = await Promise.all(promises);
            
            // All responses should be identical
            const firstResponse = responses[0].body;
            responses.forEach(response => {
                expect(response.body).toEqual(firstResponse);
            });
        });
    });

    describe('API Performance', () => {
        test('should handle multiple simultaneous requests', async () => {
            const startTime = Date.now();
            
            const promises = [
                request(app).get('/api/games'),
                request(app).get('/api/games/pulpulak/config'),
                request(app).get('/api/games/detective/config'),
                request(app).get('/api/games'),
                request(app).get('/api/games/pulpulak/config')
            ];

            const responses = await Promise.all(promises);
            
            const totalTime = Date.now() - startTime;
            expect(totalTime).toBeLessThan(1000); // All requests within 1 second

            // All requests should succeed
            responses.forEach(response => {
                expect(response.status).toBe(200);
            });
        });
    });

    describe('API Security', () => {
        test('should properly handle malformed requests', async () => {
            // Test various malformed requests
            await request(app)
                .get('/api/games/../../../etc/passwd')
                .expect(404);

            await request(app)
                .get('/api/games/<script>alert("xss")</script>/config')
                .expect(404);
        });

        test('should set appropriate headers', async () => {
            const response = await request(app)
                .get('/api/games')
                .expect(200);

            expect(response.headers['content-type']).toMatch(/application\/json/);
        });
    });

    describe('Static file serving', () => {
        test('should serve static files', async () => {
            await request(app)
                .get('/index.html')
                .expect(200);
        });

        test('should serve CSS files', async () => {
            await request(app)
                .get('/css/main.css')
                .expect(200);
        });
    });
});