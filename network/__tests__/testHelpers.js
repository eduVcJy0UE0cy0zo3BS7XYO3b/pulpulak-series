const Client = require('socket.io-client');

/**
 * Helper to create a socket client and wait for connection
 */
async function createConnectedClient(serverPort) {
    const client = Client(`http://localhost:${serverPort}`);
    
    await new Promise(resolve => {
        client.on('connect', resolve);
    });
    
    return client;
}

/**
 * Helper to create a room and return the room code
 */
async function createTestRoom(client, gameId = 'pulpulak', playerName = 'TestPlayer') {
    return new Promise((resolve, reject) => {
        client.emit('createRoom', { gameId, playerName });
        
        client.on('roomCreated', (response) => {
            if (response.success) {
                resolve(response.roomCode);
            } else {
                reject(new Error(response.error || 'Failed to create room'));
            }
        });
        
        client.on('error', reject);
        
        // Timeout after 5 seconds
        setTimeout(() => reject(new Error('Timeout creating room')), 5000);
    });
}

/**
 * Helper to join a room
 */
async function joinTestRoom(client, roomCode, playerName = 'TestPlayer2') {
    return new Promise((resolve, reject) => {
        client.emit('joinRoom', { roomCode, playerName });
        
        client.on('roomJoined', (response) => {
            if (response.success) {
                resolve(response);
            } else {
                reject(new Error(response.error || 'Failed to join room'));
            }
        });
        
        client.on('error', reject);
        
        // Timeout after 5 seconds
        setTimeout(() => reject(new Error('Timeout joining room')), 5000);
    });
}

/**
 * Helper to wait for a specific event on a socket
 */
function waitForEvent(socket, eventName, timeout = 5000) {
    return new Promise((resolve, reject) => {
        const timeoutId = setTimeout(() => {
            reject(new Error(`Timeout waiting for event: ${eventName}`));
        }, timeout);
        
        socket.once(eventName, (data) => {
            clearTimeout(timeoutId);
            resolve(data);
        });
    });
}

/**
 * Helper to clean up multiple socket connections
 */
function cleanupSockets(...sockets) {
    sockets.forEach(socket => {
        if (socket && socket.connected) {
            socket.close();
        }
    });
}

/**
 * Helper to simulate a game action
 */
async function simulateGameAction(client, roomCode, action, expectedResponse = 'actionResult') {
    return new Promise((resolve, reject) => {
        client.emit('gameAction', { roomCode, action });
        
        client.on(expectedResponse, resolve);
        client.on('error', reject);
        
        setTimeout(() => reject(new Error('Timeout waiting for game action response')), 5000);
    });
}

module.exports = {
    createConnectedClient,
    createTestRoom,
    joinTestRoom,
    waitForEvent,
    cleanupSockets,
    simulateGameAction
};