const request = require('supertest');
const { Server } = require('socket.io');
const Client = require('socket.io-client');
const http = require('http');
const express = require('express');
const SocketHandler = require('../network/socketHandler');
const GameRegistry = require('../game/GameRegistry');

describe('Multi-Game Integration Tests', () => {
    let app, server, io, socketHandler, gameRegistry;
    let clientSocket1, clientSocket2;
    let serverPort;

    beforeAll(async () => {
        // Create test server
        app = express();
        app.use(express.json());
        server = http.createServer(app);
        io = new Server(server, {
            cors: { origin: "*", methods: ["GET", "POST"] }
        });

        // Initialize GameRegistry with test games
        gameRegistry = new GameRegistry();
        
        // Mock games for testing
        const mockPulpulakConfig = {
            gameId: 'pulpulak',
            gameName: 'Княжна Пулпулак',
            gameVersion: '1.0.0',
            getCharacters: () => ['princess', 'helper'],
            getCharacterNames: () => ({ princess: 'Княжна', helper: 'Помощница' }),
            startGame: jest.fn((roomId, players) => ({
                roomId,
                players,
                currentScene: 'intro',
                gameState: 'playing'
            })),
            makeChoice: jest.fn(() => ({ success: true, gameData: {} })),
            removeGame: jest.fn()
        };

        const mockDetectiveConfig = {
            gameId: 'detective',
            gameName: 'Детективное дело',
            gameVersion: '1.0.0',
            getCharacters: () => ['detective', 'journalist'],
            getCharacterNames: () => ({ detective: 'Детектив', journalist: 'Журналист' }),
            startGame: jest.fn((roomId, players) => ({
                roomId,
                players,
                currentScene: 'crime_scene',
                gameState: 'playing'
            })),
            makeChoice: jest.fn(() => ({ success: true, gameData: {} })),
            removeGame: jest.fn()
        };

        gameRegistry.getGameConfig = jest.fn((gameId) => {
            if (gameId === 'pulpulak') return Promise.resolve(mockPulpulakConfig);
            if (gameId === 'detective') return Promise.resolve(mockDetectiveConfig);
            return Promise.resolve(null);
        });

        gameRegistry.getAvailableGames = jest.fn(() => Promise.resolve([
            {
                id: 'pulpulak',
                name: 'Княжна Пулпулак',
                description: 'Кооперативная средневековая приключенческая игра',
                minPlayers: 2,
                maxPlayers: 2,
                roles: [
                    { id: 'princess', name: 'Княжна', description: 'Главная героиня' },
                    { id: 'helper', name: 'Помощница', description: 'Верная спутница' }
                ]
            },
            {
                id: 'detective',
                name: 'Детективное дело',
                description: 'Загадочное расследование в викторианском Лондоне',
                minPlayers: 2,
                maxPlayers: 2,
                roles: [
                    { id: 'detective', name: 'Детектив', description: 'Опытный сыщик' },
                    { id: 'journalist', name: 'Журналист', description: 'Любопытный репортёр' }
                ]
            }
        ]));

        // Initialize SocketHandler
        socketHandler = new SocketHandler(io, gameRegistry);

        // Add API routes
        app.get('/api/games', async (req, res) => {
            try {
                const games = await gameRegistry.getAvailableGames();
                res.json(games);
            } catch (error) {
                res.status(500).json({ error: error.message });
            }
        });

        // Start server
        server.listen(0);
        serverPort = server.address().port;
    });

    beforeEach(async () => {
        // Create client connections
        clientSocket1 = Client(`http://localhost:${serverPort}`);
        clientSocket2 = Client(`http://localhost:${serverPort}`);

        // Wait for connections
        await Promise.all([
            new Promise(resolve => clientSocket1.on('connect', resolve)),
            new Promise(resolve => clientSocket2.on('connect', resolve))
        ]);

        // Clear any existing rooms
        socketHandler.rooms.clear();
        socketHandler.playerRooms.clear();
    });

    afterEach(() => {
        clientSocket1.close();
        clientSocket2.close();
    });

    afterAll(() => {
        server.close();
    });

    describe('Game Selection Flow', () => {
        test('should fetch available games via API', async () => {
            const response = await request(app).get('/api/games');
            
            expect(response.status).toBe(200);
            expect(response.body).toHaveLength(2);
            expect(response.body[0].id).toBe('pulpulak');
            expect(response.body[1].id).toBe('detective');
        });

        test('should create room with selected game (Pulpulak)', async () => {
            return new Promise((resolve) => {
                clientSocket1.emit('createRoom', {
                    gameId: 'pulpulak',
                    playerName: 'TestPlayer1'
                });

                clientSocket1.on('roomCreated', (data) => {
                    expect(data.success).toBe(true);
                    expect(data.gameId).toBe('pulpulak');
                    expect(data.roomCode).toBeTruthy();
                    expect(data.players.princess.name).toBe('TestPlayer1');
                    resolve();
                });
            });
        });

        test('should create room with selected game (Detective)', async () => {
            return new Promise((resolve) => {
                clientSocket1.emit('createRoom', {
                    gameId: 'detective',
                    playerName: 'TestDetective'
                });

                clientSocket1.on('roomCreated', (data) => {
                    expect(data.success).toBe(true);
                    expect(data.gameId).toBe('detective');
                    expect(data.roomCode).toBeTruthy();
                    expect(data.players.princess.name).toBe('TestDetective');
                    resolve();
                });
            });
        });

        test('should handle invalid game selection', async () => {
            return new Promise((resolve) => {
                clientSocket1.emit('createRoom', {
                    gameId: 'nonexistent',
                    playerName: 'TestPlayer1'
                });

                clientSocket1.on('error', (error) => {
                    expect(error.message).toContain('Game not found');
                    resolve();
                });
            });
        });
    });

    describe('Multi-Game Room Isolation', () => {
        test('should support multiple games simultaneously', async () => {
            const roomData = await Promise.all([
                new Promise((resolve) => {
                    clientSocket1.emit('createRoom', {
                        gameId: 'pulpulak',
                        playerName: 'PulpulakPlayer'
                    });
                    clientSocket1.on('roomCreated', resolve);
                }),
                new Promise((resolve) => {
                    clientSocket2.emit('createRoom', {
                        gameId: 'detective',
                        playerName: 'DetectivePlayer'
                    });
                    clientSocket2.on('roomCreated', resolve);
                })
            ]);

            // Verify both rooms exist with different games
            expect(roomData[0].gameId).toBe('pulpulak');
            expect(roomData[1].gameId).toBe('detective');
            expect(roomData[0].roomCode).not.toBe(roomData[1].roomCode);

            // Verify room isolation
            const room1 = socketHandler.rooms.get(roomData[0].roomCode);
            const room2 = socketHandler.rooms.get(roomData[1].roomCode);

            expect(room1.gameId).toBe('pulpulak');
            expect(room2.gameId).toBe('detective');
            expect(room1.gameLogic).not.toBe(room2.gameLogic);
        });

        test('should start different games independently', async () => {
            // Create and join Pulpulak room
            const pulpulakRoom = await new Promise((resolve) => {
                clientSocket1.emit('createRoom', {
                    gameId: 'pulpulak',
                    playerName: 'Princess'
                });
                clientSocket1.on('roomCreated', resolve);
            });

            await new Promise((resolve) => {
                clientSocket2.emit('joinRoom', {
                    roomId: pulpulakRoom.roomCode,
                    playerName: 'Helper'
                });
                clientSocket2.on('roomJoined', resolve);
            });

            // Start Pulpulak game
            const pulpulakGameData = await new Promise((resolve) => {
                clientSocket1.emit('start-coop-game', {
                    roomId: pulpulakRoom.roomCode
                });
                clientSocket1.on('game-started', resolve);
            });

            expect(pulpulakGameData.currentScene).toBe('intro');
            expect(pulpulakGameData.gameState).toBe('playing');

            // Verify game logic was called for Pulpulak
            const room = socketHandler.rooms.get(pulpulakRoom.roomCode);
            expect(room.gameLogic.startGame).toHaveBeenCalledWith(
                pulpulakRoom.roomCode,
                expect.objectContaining({
                    princess: expect.objectContaining({ name: 'Princess' }),
                    helper: expect.objectContaining({ name: 'Helper' })
                })
            );
        });
    });

    describe('Game State Management', () => {
        test('should maintain separate game states for different games', async () => {
            // Create two rooms with different games
            const rooms = await Promise.all([
                new Promise((resolve) => {
                    clientSocket1.emit('createRoom', {
                        gameId: 'pulpulak',
                        playerName: 'Player1'
                    });
                    clientSocket1.on('roomCreated', resolve);
                }),
                new Promise((resolve) => {
                    clientSocket2.emit('createRoom', {
                        gameId: 'detective',
                        playerName: 'Player2'
                    });
                    clientSocket2.on('roomCreated', resolve);
                })
            ]);

            // Verify each room has its own game state
            const room1 = socketHandler.rooms.get(rooms[0].roomCode);
            const room2 = socketHandler.rooms.get(rooms[1].roomCode);

            expect(room1.gameState).toBe('waiting');
            expect(room2.gameState).toBe('waiting');
            expect(room1.gameId).toBe('pulpulak');
            expect(room2.gameId).toBe('detective');

            // Verify game logic instances are different
            expect(room1.gameLogic).not.toBe(room2.gameLogic);
            expect(room1.gameConfig).not.toBe(room2.gameConfig);
        });

        test('should handle game cleanup when room is destroyed', async () => {
            const roomData = await new Promise((resolve) => {
                clientSocket1.emit('createRoom', {
                    gameId: 'detective',
                    playerName: 'TestPlayer'
                });
                clientSocket1.on('roomCreated', resolve);
            });

            const room = socketHandler.rooms.get(roomData.roomCode);
            const gameLogic = room.gameLogic;

            // Disconnect player (should trigger room cleanup)
            clientSocket1.disconnect();

            // Wait for cleanup
            await new Promise(resolve => setTimeout(resolve, 100));

            // Verify room was cleaned up
            expect(socketHandler.rooms.has(roomData.roomCode)).toBe(false);
            expect(gameLogic.removeGame).toHaveBeenCalledWith(roomData.roomCode);
        });
    });

    describe('Error Handling', () => {
        test('should handle game loading errors gracefully', async () => {
            // Mock game loading failure
            gameRegistry.getGameConfig.mockRejectedValueOnce(new Error('Game load failed'));

            return new Promise((resolve) => {
                clientSocket1.emit('createRoom', {
                    gameId: 'pulpulak',
                    playerName: 'TestPlayer'
                });

                clientSocket1.on('error', (error) => {
                    expect(error.message).toBe('Failed to create room');
                    resolve();
                });
            });
        });

        test('should handle missing game configurations', async () => {
            gameRegistry.getGameConfig.mockResolvedValueOnce(null);

            return new Promise((resolve) => {
                clientSocket1.emit('createRoom', {
                    gameId: 'missing-game',
                    playerName: 'TestPlayer'
                });

                clientSocket1.on('error', (error) => {
                    expect(error.message).toBe('Game not found: missing-game');
                    resolve();
                });
            });
        });
    });

    describe('Backward Compatibility', () => {
        test('should default to pulpulak when no gameId provided', async () => {
            return new Promise((resolve) => {
                clientSocket1.emit('createRoom', {
                    playerName: 'TestPlayer'
                    // No gameId provided
                });

                clientSocket1.on('roomCreated', (data) => {
                    expect(data.success).toBe(true);
                    expect(data.gameId).toBe('pulpulak');
                    resolve();
                });
            });
        });

        test('should support legacy create-room event', async () => {
            return new Promise((resolve) => {
                clientSocket1.emit('create-room', {
                    username: 'LegacyPlayer'
                });

                clientSocket1.on('roomCreated', (data) => {
                    expect(data.success).toBe(true);
                    expect(data.gameId).toBe('pulpulak');
                    expect(data.players.princess.name).toBe('LegacyPlayer');
                    resolve();
                });
            });
        });
    });

    describe('Performance', () => {
        test('should handle multiple simultaneous room creations', async () => {
            const promises = [];
            const numRooms = 5;

            for (let i = 0; i < numRooms; i++) {
                const client = Client(`http://localhost:${serverPort}`);
                const promise = new Promise((resolve) => {
                    client.on('connect', () => {
                        client.emit('createRoom', {
                            gameId: i % 2 === 0 ? 'pulpulak' : 'detective',
                            playerName: `Player${i}`
                        });
                        client.on('roomCreated', (data) => {
                            client.close();
                            resolve(data);
                        });
                    });
                });
                promises.push(promise);
            }

            const results = await Promise.all(promises);

            expect(results).toHaveLength(numRooms);
            expect(socketHandler.rooms.size).toBe(numRooms);

            // Verify alternating game types
            results.forEach((result, index) => {
                expect(result.gameId).toBe(index % 2 === 0 ? 'pulpulak' : 'detective');
            });
        });
    });
});