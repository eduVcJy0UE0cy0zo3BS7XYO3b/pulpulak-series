const http = require('http');
const socketIo = require('socket.io');
const Client = require('socket.io-client');
const SocketHandler = require('../socketHandler');
const GameRegistry = require('../../game/GameRegistry');
const path = require('path');

describe('Multi-game Socket Handler', () => {
    let server;
    let io;
    let gameRegistry;
    let socketHandler;
    let clientSocket1;
    let clientSocket2;
    let serverSocket1;
    let serverSocket2;

    beforeAll(async () => {
        // Create test server
        server = http.createServer();
        io = socketIo(server);
        
        // Setup GameRegistry with real games
        gameRegistry = new GameRegistry();
        gameRegistry.gamesDirectory = path.join(__dirname, '..', '..', 'games');
        await gameRegistry.scanGames();
        
        // Create SocketHandler with GameRegistry
        socketHandler = new SocketHandler(io, gameRegistry);
        
        // Start server on random port
        await new Promise(resolve => {
            server.listen(resolve);
        });
        
        const port = server.address().port;
        
        // Create client connections
        clientSocket1 = Client(`http://localhost:${port}`);
        clientSocket2 = Client(`http://localhost:${port}`);
        
        // Wait for connections
        await Promise.all([
            new Promise(resolve => clientSocket1.on('connect', resolve)),
            new Promise(resolve => clientSocket2.on('connect', resolve))
        ]);
    });

    beforeEach(() => {
        // Clear any existing rooms before each test
        socketHandler.rooms.clear();
        socketHandler.playerRooms.clear();
    });

    afterAll(async () => {
        if (clientSocket1) clientSocket1.close();
        if (clientSocket2) clientSocket2.close();
        if (server) {
            await new Promise(resolve => server.close(resolve));
        }
    });

    describe('Room creation with gameId', () => {
        test('should create rooms with gameId for Pulpulak', (done) => {
            const roomData = {
                gameId: 'pulpulak',
                playerName: 'TestPlayer1'
            };

            clientSocket1.emit('createRoom', roomData);

            clientSocket1.on('roomCreated', (response) => {
                expect(response.success).toBe(true);
                expect(response.roomCode).toBeTruthy();
                expect(response.gameId).toBe('pulpulak');
                
                // Verify room was created with correct game
                const room = socketHandler.rooms.get(response.roomCode);
                expect(room).toBeTruthy();
                expect(room.gameId).toBe('pulpulak');
                expect(room.gameConfig).toBeTruthy();
                
                done();
            });
        });

        test('should create rooms with gameId for Detective', (done) => {
            const roomData = {
                gameId: 'detective',
                playerName: 'TestDetective'
            };

            clientSocket1.emit('createRoom', roomData);

            clientSocket1.on('roomCreated', (response) => {
                expect(response.success).toBe(true);
                expect(response.roomCode).toBeTruthy();
                expect(response.gameId).toBe('detective');
                
                // Verify room was created with correct game
                const room = socketHandler.rooms.get(response.roomCode);
                expect(room).toBeTruthy();
                expect(room.gameId).toBe('detective');
                expect(room.gameConfig).toBeTruthy();
                
                done();
            });
        });

        test('should validate gameId before room creation', (done) => {
            const roomData = {
                gameId: 'nonexistent',
                playerName: 'TestPlayer'
            };

            clientSocket1.emit('createRoom', roomData);

            clientSocket1.on('error', (error) => {
                expect(error.message).toContain('Game not found');
                done();
            });
        });

        test('should handle missing gameId in room creation', (done) => {
            const roomData = {
                playerName: 'TestPlayer'
                // Missing gameId
            };

            clientSocket1.emit('createRoom', roomData);

            clientSocket1.on('error', (error) => {
                expect(error.message).toContain('gameId is required');
                done();
            });
        });

        test('should isolate rooms by game type', async () => {
            // Create Pulpulak room
            const pulpulakData = { gameId: 'pulpulak', playerName: 'PulpulakPlayer' };
            const detectiveData = { gameId: 'detective', playerName: 'DetectivePlayer' };

            let pulpulakRoom, detectiveRoom;

            // Create Pulpulak room
            await new Promise(resolve => {
                clientSocket1.emit('createRoom', pulpulakData);
                clientSocket1.on('roomCreated', (response) => {
                    pulpulakRoom = response;
                    resolve();
                });
            });

            // Create Detective room
            await new Promise(resolve => {
                clientSocket2.emit('createRoom', detectiveData);
                clientSocket2.on('roomCreated', (response) => {
                    detectiveRoom = response;
                    resolve();
                });
            });

            // Verify both rooms exist with different games
            expect(pulpulakRoom.gameId).toBe('pulpulak');
            expect(detectiveRoom.gameId).toBe('detective');
            expect(pulpulakRoom.roomCode).not.toBe(detectiveRoom.roomCode);

            // Verify rooms are isolated
            const room1 = socketHandler.rooms.get(pulpulakRoom.roomCode);
            const room2 = socketHandler.rooms.get(detectiveRoom.roomCode);
            
            expect(room1.gameId).toBe('pulpulak');
            expect(room2.gameId).toBe('detective');
            expect(room1.gameConfig).not.toBe(room2.gameConfig);
        });
    });

    describe('Game session management', () => {
        test('should load correct game logic for room', (done) => {
            const roomData = {
                gameId: 'pulpulak',
                playerName: 'TestPlayer'
            };

            clientSocket1.emit('createRoom', roomData);

            clientSocket1.on('roomCreated', (response) => {
                const room = socketHandler.rooms.get(response.roomCode);
                
                // Verify game logic is specific to Pulpulak
                expect(room.gameLogic).toBeTruthy();
                expect(room.gameConfig.getCharacters()).toContain('princess');
                expect(room.gameConfig.getCharacters()).toContain('helper');
                
                done();
            });
        });

        test('should handle joining rooms with correct game context', async () => {
            // Create a Pulpulak room
            const roomData = { gameId: 'pulpulak', playerName: 'Player1' };
            
            let roomCode;
            await new Promise(resolve => {
                clientSocket1.emit('createRoom', roomData);
                clientSocket1.on('roomCreated', (response) => {
                    roomCode = response.roomCode;
                    resolve();
                });
            });

            // Join the room with second player
            await new Promise(resolve => {
                clientSocket2.emit('joinRoom', { 
                    roomCode: roomCode, 
                    playerName: 'Player2' 
                });
                clientSocket2.on('roomJoined', (response) => {
                    expect(response.success).toBe(true);
                    expect(response.gameId).toBe('pulpulak');
                    resolve();
                });
            });

            // Verify room has both players with correct game context
            const room = socketHandler.rooms.get(roomCode);
            expect(room.players.length).toBe(2);
            expect(room.gameId).toBe('pulpulak');
        });

        test('should handle simultaneous sessions of different games', async () => {
            // Create multiple rooms of different games
            const rooms = [];
            
            // Create Pulpulak room
            await new Promise(resolve => {
                clientSocket1.emit('createRoom', { 
                    gameId: 'pulpulak', 
                    playerName: 'PulpulakPlayer' 
                });
                clientSocket1.on('roomCreated', (response) => {
                    rooms.push(response);
                    resolve();
                });
            });

            // Create Detective room
            await new Promise(resolve => {
                clientSocket2.emit('createRoom', { 
                    gameId: 'detective', 
                    playerName: 'DetectivePlayer' 
                });
                clientSocket2.on('roomCreated', (response) => {
                    rooms.push(response);
                    resolve();
                });
            });

            // Verify both sessions are running independently
            expect(rooms).toHaveLength(2);
            expect(rooms[0].gameId).toBe('pulpulak');
            expect(rooms[1].gameId).toBe('detective');

            // Verify isolation - changes in one room don't affect the other
            const room1 = socketHandler.rooms.get(rooms[0].roomCode);
            const room2 = socketHandler.rooms.get(rooms[1].roomCode);
            
            expect(room1.gameState).not.toBe(room2.gameState);
            expect(room1.gameLogic).not.toBe(room2.gameLogic);
        });

        test('should prevent cross-game state interference', async () => {
            // Create two different game rooms
            let pulpulakRoomCode, detectiveRoomCode;

            await new Promise(resolve => {
                clientSocket1.emit('createRoom', { 
                    gameId: 'pulpulak', 
                    playerName: 'Player1' 
                });
                clientSocket1.on('roomCreated', (response) => {
                    pulpulakRoomCode = response.roomCode;
                    resolve();
                });
            });

            await new Promise(resolve => {
                clientSocket2.emit('createRoom', { 
                    gameId: 'detective', 
                    playerName: 'Player2' 
                });
                clientSocket2.on('roomCreated', (response) => {
                    detectiveRoomCode = response.roomCode;
                    resolve();
                });
            });

            // Get room states
            const pulpulakRoom = socketHandler.rooms.get(pulpulakRoomCode);
            const detectiveRoom = socketHandler.rooms.get(detectiveRoomCode);

            // Modify one room's state
            const originalPulpulakState = JSON.stringify(pulpulakRoom.gameState);
            const originalDetectiveState = JSON.stringify(detectiveRoom.gameState);

            // Simulate game action in Pulpulak room
            pulpulakRoom.gameState.currentScene = 'modified_scene';

            // Verify Detective room is unaffected
            expect(JSON.stringify(detectiveRoom.gameState)).toBe(originalDetectiveState);
            expect(detectiveRoom.gameState.currentScene).not.toBe('modified_scene');
        });
    });

    describe('Backward compatibility', () => {
        test('should maintain compatibility with existing room creation', (done) => {
            // Test old-style room creation without gameId (should default to Pulpulak)
            const roomData = {
                playerName: 'LegacyPlayer'
                // No gameId specified
            };

            clientSocket1.emit('createRoom', roomData);

            clientSocket1.on('roomCreated', (response) => {
                expect(response.success).toBe(true);
                expect(response.roomCode).toBeTruthy();
                
                // Should default to Pulpulak for backward compatibility
                const room = socketHandler.rooms.get(response.roomCode);
                expect(room.gameId).toBe('pulpulak');
                
                done();
            });
        });

        test('should handle legacy join room format', async () => {
            // Create room first
            let roomCode;
            await new Promise(resolve => {
                clientSocket1.emit('createRoom', { 
                    gameId: 'pulpulak', 
                    playerName: 'Player1' 
                });
                clientSocket1.on('roomCreated', (response) => {
                    roomCode = response.roomCode;
                    resolve();
                });
            });

            // Join with legacy format (no gameId)
            await new Promise(resolve => {
                clientSocket2.emit('joinRoom', { 
                    roomCode: roomCode, 
                    playerName: 'Player2' 
                });
                clientSocket2.on('roomJoined', (response) => {
                    expect(response.success).toBe(true);
                    resolve();
                });
            });

            const room = socketHandler.rooms.get(roomCode);
            expect(room.players.length).toBe(2);
        });
    });

    describe('Error handling', () => {
        test('should handle invalid gameId gracefully', (done) => {
            const roomData = {
                gameId: 'invalid_game_123',
                playerName: 'TestPlayer'
            };

            clientSocket1.emit('createRoom', roomData);

            clientSocket1.on('error', (error) => {
                expect(error).toBeTruthy();
                expect(error.message).toMatch(/Game.*not found/i);
                done();
            });
        });

        test('should handle game loading failures', async () => {
            // Temporarily break game loading
            const originalGetGameConfig = gameRegistry.getGameConfig;
            gameRegistry.getGameConfig = jest.fn().mockResolvedValue(null);

            await new Promise(resolve => {
                clientSocket1.emit('createRoom', { 
                    gameId: 'pulpulak', 
                    playerName: 'TestPlayer' 
                });
                
                clientSocket1.on('error', (error) => {
                    expect(error.message).toContain('Failed to load game');
                    resolve();
                });
            });

            // Restore original method
            gameRegistry.getGameConfig = originalGetGameConfig;
        });

        test('should handle socket disconnections during game sessions', async () => {
            // Create room with two players
            let roomCode;
            await new Promise(resolve => {
                clientSocket1.emit('createRoom', { 
                    gameId: 'pulpulak', 
                    playerName: 'Player1' 
                });
                clientSocket1.on('roomCreated', (response) => {
                    roomCode = response.roomCode;
                    resolve();
                });
            });

            await new Promise(resolve => {
                clientSocket2.emit('joinRoom', { 
                    roomCode: roomCode, 
                    playerName: 'Player2' 
                });
                clientSocket2.on('roomJoined', () => resolve());
            });

            // Verify room exists with 2 players
            let room = socketHandler.rooms.get(roomCode);
            expect(room.players.length).toBe(2);

            // Disconnect one player
            clientSocket2.disconnect();

            // Wait a bit for cleanup
            await new Promise(resolve => setTimeout(resolve, 100));

            // Room should still exist but with fewer players
            room = socketHandler.rooms.get(roomCode);
            expect(room).toBeTruthy();
        });
    });

    describe('Performance and scalability', () => {
        test('should handle multiple concurrent room creations', async () => {
            const promises = [];
            const roomCount = 5;

            // Create multiple rooms concurrently
            for (let i = 0; i < roomCount; i++) {
                const promise = new Promise(resolve => {
                    const tempSocket = Client(`http://localhost:${server.address().port}`);
                    tempSocket.on('connect', () => {
                        tempSocket.emit('createRoom', { 
                            gameId: i % 2 === 0 ? 'pulpulak' : 'detective', 
                            playerName: `Player${i}` 
                        });
                        tempSocket.on('roomCreated', (response) => {
                            tempSocket.close();
                            resolve(response);
                        });
                    });
                });
                promises.push(promise);
            }

            const results = await Promise.all(promises);
            
            // All rooms should be created successfully
            expect(results).toHaveLength(roomCount);
            results.forEach(result => {
                expect(result.success).toBe(true);
                expect(result.roomCode).toBeTruthy();
            });

            // Verify all rooms exist
            expect(socketHandler.rooms.size).toBe(roomCount);
        });

        test('should maintain good performance with multiple game types', async () => {
            const startTime = Date.now();
            
            // Create rooms of both game types
            const promises = [];
            for (let i = 0; i < 10; i++) {
                promises.push(new Promise(resolve => {
                    const tempSocket = Client(`http://localhost:${server.address().port}`);
                    tempSocket.on('connect', () => {
                        tempSocket.emit('createRoom', { 
                            gameId: i % 2 === 0 ? 'pulpulak' : 'detective', 
                            playerName: `PerfPlayer${i}` 
                        });
                        tempSocket.on('roomCreated', (response) => {
                            tempSocket.close();
                            resolve(response);
                        });
                    });
                }));
            }

            await Promise.all(promises);
            const endTime = Date.now();
            
            // Should complete within reasonable time
            expect(endTime - startTime).toBeLessThan(2000); // 2 seconds
        });
    });
});