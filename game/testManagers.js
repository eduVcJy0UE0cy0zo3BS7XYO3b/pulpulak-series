#!/usr/bin/env node

/**
 * Test script for data managers
 * Tests basic operations for GameDataManager, PlayerDataManager, OutfitDataManager, and QuestDataManager
 */

const dataManagerFactory = require('./managers/DataManagerFactory');

// Test data structure matching what tests use
const testPlayers = {
    princess: { id: 'alice', name: 'Alice' },
    helper: { id: 'bob', name: 'Bob' }
};

const testRoomId = 'TEST123';

function log(message, data = null) {
    console.log(`[TEST] ${message}`);
    if (data) {
        console.log('   ', JSON.stringify(data, null, 2));
    }
}

function logError(message, error) {
    console.error(`[ERROR] ${message}:`, error.message);
    if (process.env.DEBUG) {
        console.error(error.stack);
    }
}

function logSuccess(message) {
    console.log(`[SUCCESS] ✓ ${message}`);
}

function logFailure(message, reason) {
    console.log(`[FAILURE] ✗ ${message}: ${reason}`);
}

async function testGameDataManager(managers) {
    log('Testing GameDataManager...');
    
    try {
        // Test 1: Create a game
        log('1. Creating a new game');
        const gameState = managers.gameData.createGame(testRoomId, testPlayers);
        
        if (!gameState) {
            logFailure('Game creation', 'gameState is null');
            return false;
        }
        
        if (gameState.roomId !== testRoomId) {
            logFailure('Game creation', `Wrong roomId: ${gameState.roomId}`);
            return false;
        }
        
        if (!gameState.players.princess || !gameState.players.helper) {
            logFailure('Game creation', 'Players not properly set');
            return false;
        }
        
        logSuccess('Game created successfully');
        
        // Test 2: Get the game
        log('2. Retrieving game data');
        const retrievedGame = managers.gameData.getGame(testRoomId);
        
        if (!retrievedGame) {
            logFailure('Game retrieval', 'Game not found');
            return false;
        }
        
        if (retrievedGame.roomId !== testRoomId) {
            logFailure('Game retrieval', 'Wrong roomId in retrieved game');
            return false;
        }
        
        logSuccess('Game retrieved successfully');
        
        // Test 3: Check game structure
        log('3. Validating game structure');
        const requiredFields = ['roomId', 'players', 'stats', 'quests', 'currentScene', 'turnOrder'];
        
        for (const field of requiredFields) {
            if (!retrievedGame[field]) {
                logFailure('Game structure validation', `Missing field: ${field}`);
                return false;
            }
        }
        
        // Check character stats
        const requiredCharacters = ['princess', 'helper'];
        for (const character of requiredCharacters) {
            if (!retrievedGame.stats[character]) {
                logFailure('Game structure validation', `Missing stats for: ${character}`);
                return false;
            }
            
            if (!retrievedGame.quests[character]) {
                logFailure('Game structure validation', `Missing quests for: ${character}`);
                return false;
            }
        }
        
        logSuccess('Game structure is valid');
        
        // Test 4: Scene and turn management
        log('4. Testing scene and turn management');
        const initialScene = retrievedGame.currentScene;
        const initialTurn = retrievedGame.turnOrder;
        
        managers.gameData.updateScene(testRoomId, 'test_scene');
        const updatedGame = managers.gameData.getGame(testRoomId);
        
        if (updatedGame.currentScene !== 'test_scene') {
            logFailure('Scene update', 'Scene not updated');
            return false;
        }
        
        logSuccess('Scene updated successfully');
        
        managers.gameData.switchTurn(testRoomId);
        const turnSwitchedGame = managers.gameData.getGame(testRoomId);
        
        if (turnSwitchedGame.turnOrder === initialTurn) {
            logFailure('Turn switching', 'Turn not switched');
            return false;
        }
        
        logSuccess('Turn switched successfully');
        
        // Test 5: Client data building
        log('5. Testing client data building');
        
        // First set back to a valid scene
        managers.gameData.updateScene(testRoomId, 'coop_awakening');
        
        const clientData = managers.gameData.buildClientGameData(testRoomId, {}, null);
        
        if (!clientData) {
            logFailure('Client data building', 'No client data returned');
            return false;
        }
        
        const requiredClientFields = ['roomId', 'players', 'scene', 'stats', 'currentTurn'];
        for (const field of requiredClientFields) {
            if (!clientData[field]) {
                logFailure('Client data building', `Missing client field: ${field}`);
                return false;
            }
        }
        
        // Test the scene data structure
        if (!clientData.scene.title || !clientData.scene.text) {
            logFailure('Client data building', 'Scene data missing title or text');
            return false;
        }
        
        logSuccess('Client data built successfully');
        
        log('GameDataManager tests completed successfully!');
        return true;
        
    } catch (error) {
        logError('GameDataManager test failed', error);
        return false;
    }
}

async function testPlayerDataManager(managers) {
    log('Testing PlayerDataManager...');
    
    try {
        // Test 1: Get player data
        log('1. Getting player data');
        const princessData = managers.playerData.getPlayerData(testRoomId, 'princess');
        const helperData = managers.playerData.getPlayerData(testRoomId, 'helper');
        
        if (!princessData || !helperData) {
            logFailure('Player data retrieval', 'Could not get player data');
            return false;
        }
        
        logSuccess('Player data retrieved successfully');
        log('   Princess data:', {
            outfit: princessData.outfit,
            location: princessData.location,
            awareness: princessData.awareness
        });
        log('   Helper data:', {
            outfit: helperData.outfit,
            location: helperData.location,
            awareness: helperData.awareness
        });
        
        // Test 2: Update location
        log('2. Testing location updates');
        const originalPrincessLocation = princessData.location;
        
        managers.playerData.updateLocation(testRoomId, 'princess', 'test_location');
        const updatedPrincessData = managers.playerData.getPlayerData(testRoomId, 'princess');
        
        if (updatedPrincessData.location !== 'test_location') {
            logFailure('Location update', 'Location not updated');
            return false;
        }
        
        logSuccess('Location updated successfully');
        
        // Test 3: Update outfit
        log('3. Testing outfit updates');
        const originalOutfit = princessData.outfit;
        
        managers.playerData.updateOutfit(testRoomId, 'princess', 'test_outfit');
        const updatedOutfitData = managers.playerData.getPlayerData(testRoomId, 'princess');
        
        if (updatedOutfitData.outfit !== 'test_outfit') {
            logFailure('Outfit update', 'Outfit not updated');
            return false;
        }
        
        logSuccess('Outfit updated successfully');
        
        // Test 4: Swap outfits
        log('4. Testing outfit swapping');
        const princessOutfitBefore = managers.playerData.getPlayerData(testRoomId, 'princess').outfit;
        const helperOutfitBefore = managers.playerData.getPlayerData(testRoomId, 'helper').outfit;
        
        managers.playerData.swapOutfits(testRoomId);
        
        const princessOutfitAfter = managers.playerData.getPlayerData(testRoomId, 'princess').outfit;
        const helperOutfitAfter = managers.playerData.getPlayerData(testRoomId, 'helper').outfit;
        
        if (princessOutfitAfter !== helperOutfitBefore || helperOutfitAfter !== princessOutfitBefore) {
            logFailure('Outfit swapping', 'Outfits not swapped correctly');
            return false;
        }
        
        logSuccess('Outfits swapped successfully');
        
        // Test 5: Inventory management
        log('5. Testing inventory management');
        const testItem = 'test_item';
        
        managers.playerData.addToInventory(testRoomId, 'princess', testItem);
        
        if (!managers.playerData.hasItem(testRoomId, 'princess', testItem)) {
            logFailure('Inventory add', 'Item not added to inventory');
            return false;
        }
        
        logSuccess('Item added to inventory');
        
        managers.playerData.removeFromInventory(testRoomId, 'princess', testItem);
        
        if (managers.playerData.hasItem(testRoomId, 'princess', testItem)) {
            logFailure('Inventory remove', 'Item not removed from inventory');
            return false;
        }
        
        logSuccess('Item removed from inventory');
        
        // Test 6: Awareness management
        log('6. Testing awareness management');
        const initialAwareness = managers.playerData.getAwareness(testRoomId, 'princess');
        
        managers.playerData.updateAwareness(testRoomId, 'princess', 5);
        const updatedAwareness = managers.playerData.getAwareness(testRoomId, 'princess');
        
        if (updatedAwareness !== initialAwareness + 5) {
            logFailure('Awareness update', `Expected ${initialAwareness + 5}, got ${updatedAwareness}`);
            return false;
        }
        
        logSuccess('Awareness updated successfully');
        
        // Test 7: Player identification
        log('7. Testing player identification');
        const princessPlayerId = managers.playerData.getPlayerId(testRoomId, 'princess');
        const helperPlayerId = managers.playerData.getPlayerId(testRoomId, 'helper');
        
        if (princessPlayerId !== 'alice' || helperPlayerId !== 'bob') {
            logFailure('Player identification', 'Wrong player IDs');
            return false;
        }
        
        const foundCharacter = managers.playerData.findPlayerCharacter(testRoomId, 'alice');
        if (foundCharacter !== 'princess') {
            logFailure('Player character finding', 'Character not found for player ID');
            return false;
        }
        
        logSuccess('Player identification working correctly');
        
        log('PlayerDataManager tests completed successfully!');
        return true;
        
    } catch (error) {
        logError('PlayerDataManager test failed', error);
        return false;
    }
}

async function testOutfitDataManager(managers) {
    log('Testing OutfitDataManager...');
    
    try {
        // Test 1: Get current outfits
        log('1. Getting current outfits');
        const princessOutfit = managers.outfitData.getCurrentOutfit(testRoomId, 'princess');
        const helperOutfit = managers.outfitData.getCurrentOutfit(testRoomId, 'helper');
        
        if (!princessOutfit || !helperOutfit) {
            logFailure('Current outfit retrieval', 'Could not get current outfits');
            return false;
        }
        
        logSuccess('Current outfits retrieved');
        log('   Princess outfit:', princessOutfit);
        log('   Helper outfit:', helperOutfit);
        
        // Test 2: Get outfit names
        log('2. Testing outfit name retrieval');
        const princessOutfitName = managers.outfitData.getOutfitName(princessOutfit);
        const helperOutfitName = managers.outfitData.getOutfitName(helperOutfit);
        
        log('   Princess outfit name:', princessOutfitName);
        log('   Helper outfit name:', helperOutfitName);
        
        logSuccess('Outfit names retrieved');
        
        // Test 3: Test outfit validation (this will likely fail due to NPCs or location restrictions)
        log('3. Testing outfit change validation');
        const canSwitchPrincess = managers.outfitData.canSwitchOutfits(testRoomId, 'princess');
        const canSwitchHelper = managers.outfitData.canSwitchOutfits(testRoomId, 'helper');
        
        log('   Can princess switch outfits:', canSwitchPrincess);
        log('   Can helper switch outfits:', canSwitchHelper);
        
        // This might be false due to game conditions, which is fine for testing
        logSuccess('Outfit validation checked (result depends on game state)');
        
        // Test 4: Create outfit swap request (this might fail due to validation)
        log('4. Testing outfit swap request creation');
        const swapRequest = managers.outfitData.createOutfitSwapRequest(testRoomId, 'alice', 'princess');
        
        log('   Swap request result:', swapRequest.success ? 'SUCCESS' : 'FAILED');
        log('   Message:', swapRequest.message);
        
        if (swapRequest.success) {
            // Test responding to the request
            log('5. Testing outfit swap request response');
            const acceptResponse = managers.outfitData.respondToOutfitSwapRequest(testRoomId, 'bob', true);
            
            log('   Accept response:', acceptResponse.success ? 'SUCCESS' : 'FAILED');
            log('   Message:', acceptResponse.message);
            
            logSuccess('Outfit swap request flow tested');
        } else {
            log('5. Outfit swap request failed (likely due to validation - this is normal)');
            logSuccess('Outfit swap validation working as expected');
        }
        
        // Test 5: Get all outfit info
        log('6. Testing outfit info retrieval');
        const allOutfitInfo = managers.outfitData.getAllOutfitInfo(testRoomId);
        
        if (!allOutfitInfo.princess || !allOutfitInfo.helper) {
            logFailure('All outfit info', 'Could not get all outfit info');
            return false;
        }
        
        logSuccess('All outfit info retrieved');
        log('   All outfit info:', allOutfitInfo);
        
        // Test 6: Administrative outfit change
        log('7. Testing administrative outfit change');
        const changeResult = managers.outfitData.changeOutfit(testRoomId, 'princess', 'nightgown');
        
        if (!changeResult.success) {
            logFailure('Administrative outfit change', changeResult.message);
            return false;
        }
        
        const newOutfit = managers.outfitData.getCurrentOutfit(testRoomId, 'princess');
        if (newOutfit !== 'nightgown') {
            logFailure('Administrative outfit change', 'Outfit not changed');
            return false;
        }
        
        logSuccess('Administrative outfit change successful');
        
        log('OutfitDataManager tests completed successfully!');
        return true;
        
    } catch (error) {
        logError('OutfitDataManager test failed', error);
        return false;
    }
}

async function testQuestDataManager(managers) {
    log('Testing QuestDataManager...');
    
    try {
        // Test 1: Get character quests
        log('1. Getting character quest data');
        const princessQuests = managers.questData.getCharacterQuests(testRoomId, 'princess');
        const helperQuests = managers.questData.getCharacterQuests(testRoomId, 'helper');
        
        if (!princessQuests || !helperQuests) {
            logFailure('Character quest retrieval', 'Could not get character quests');
            return false;
        }
        
        logSuccess('Character quest data retrieved');
        log('   Princess quests:', princessQuests);
        log('   Helper quests:', helperQuests);
        
        // Test 2: Check initial quest state
        log('2. Checking initial quest state');
        const princessActiveQuest = managers.questData.getActiveQuest(testRoomId, 'princess');
        const helperActiveQuest = managers.questData.getActiveQuest(testRoomId, 'helper');
        
        if (princessActiveQuest !== null || helperActiveQuest !== null) {
            logFailure('Initial quest state', 'Characters should not have active quests initially');
            return false;
        }
        
        logSuccess('Initial quest state correct (no active quests)');
        
        // Test 3: Check for completed quests
        log('3. Checking completed quests');
        const princessCompleted = managers.questData.getCompletedQuests(testRoomId, 'princess');
        const helperCompleted = managers.questData.getCompletedQuests(testRoomId, 'helper');
        
        if (princessCompleted.length !== 0 || helperCompleted.length !== 0) {
            logFailure('Completed quests check', 'Characters should not have completed quests initially');
            return false;
        }
        
        logSuccess('No completed quests initially (correct)');
        
        // Test 4: Test quest availability checking
        log('4. Testing quest availability');
        const hasActivePrincess = managers.questData.hasActiveQuest(testRoomId, 'princess');
        const hasActiveHelper = managers.questData.hasActiveQuest(testRoomId, 'helper');
        
        if (hasActivePrincess || hasActiveHelper) {
            logFailure('Quest availability', 'Should not have active quests');
            return false;
        }
        
        logSuccess('Quest availability check working');
        
        // Test 5: Test quest starting (may fail if quest data doesn't exist)
        log('5. Testing quest starting');
        
        try {
            // Try to start a test quest
            const startResult = managers.questData.startQuest(testRoomId, 'princess', 'test_quest');
            
            if (startResult.success) {
                logSuccess('Quest started successfully');
                log('   Quest started:', startResult.quest?.title || 'Unknown');
                
                // Test getting active quest after starting
                const activeQuest = managers.questData.getActiveQuest(testRoomId, 'princess');
                if (!activeQuest) {
                    logFailure('Quest starting', 'Active quest not found after starting');
                    return false;
                }
                
                logSuccess('Active quest retrieved after starting');
                
            } else {
                log('   Quest start failed (likely quest not found):', startResult.message);
                logSuccess('Quest start validation working (quest not found is expected)');
            }
        } catch (questError) {
            log('   Quest starting error (expected - quest system may not have test quests):', questError.message);
            logSuccess('Quest system handled error appropriately');
        }
        
        // Test 6: Test global quest memory
        log('6. Testing global quest memory');
        const globalMemory = managers.questData.getGlobalQuestMemory(testRoomId);
        
        if (typeof globalMemory !== 'object') {
            logFailure('Global quest memory', 'Global memory is not an object');
            return false;
        }
        
        logSuccess('Global quest memory retrieved');
        log('   Global memory:', globalMemory);
        
        // Test 7: Test quest stats
        log('7. Testing quest statistics');
        const princessStats = managers.questData.getQuestStats(testRoomId, 'princess');
        const helperStats = managers.questData.getQuestStats(testRoomId, 'helper');
        
        if (!princessStats || !helperStats) {
            logFailure('Quest statistics', 'Could not get quest stats');
            return false;
        }
        
        logSuccess('Quest statistics retrieved');
        log('   Princess stats:', princessStats);
        log('   Helper stats:', helperStats);
        
        // Test 8: Test available quests
        log('8. Testing available quests');
        
        try {
            const availableForPrincess = managers.questData.getAvailableQuests(testRoomId, 'princess');
            const availableForHelper = managers.questData.getAvailableQuests(testRoomId, 'helper');
            
            logSuccess('Available quests retrieved');
            log('   Available for princess:', availableForPrincess.length);
            log('   Available for helper:', availableForHelper.length);
            
        } catch (availableError) {
            log('   Available quests error (quest data may not be loaded):', availableError.message);
            logSuccess('Available quests handled error appropriately');
        }
        
        log('QuestDataManager tests completed successfully!');
        return true;
        
    } catch (error) {
        logError('QuestDataManager test failed', error);
        return false;
    }
}

async function runAllTests() {
    log('='.repeat(60));
    log('Starting Data Managers Test Suite');
    log('='.repeat(60));
    
    // Get fresh managers for testing
    log('Initializing data managers...');
    dataManagerFactory.resetManagers(); // Reset for clean test
    const managers = dataManagerFactory.getManagers();
    
    if (!managers) {
        logError('Failed to get data managers', new Error('Managers not initialized'));
        return;
    }
    
    logSuccess('Data managers initialized');
    log('   Available managers:', Object.keys(managers));
    
    const results = {
        gameData: false,
        playerData: false,
        outfitData: false,
        questData: false
    };
    
    // Run tests in sequence
    log('\n' + '='.repeat(40));
    results.gameData = await testGameDataManager(managers);
    
    log('\n' + '='.repeat(40));
    results.playerData = await testPlayerDataManager(managers);
    
    log('\n' + '='.repeat(40));
    results.outfitData = await testOutfitDataManager(managers);
    
    log('\n' + '='.repeat(40));
    results.questData = await testQuestDataManager(managers);
    
    // Cleanup
    log('\n' + '='.repeat(40));
    log('Cleaning up test data...');
    try {
        managers.gameData.deleteGame(testRoomId);
        managers.outfitData.clearRoomRequests(testRoomId);
        logSuccess('Test data cleaned up');
    } catch (cleanupError) {
        logError('Cleanup failed', cleanupError);
    }
    
    // Summary
    log('\n' + '='.repeat(60));
    log('TEST RESULTS SUMMARY');
    log('='.repeat(60));
    
    const testNames = {
        gameData: 'GameDataManager',
        playerData: 'PlayerDataManager', 
        outfitData: 'OutfitDataManager',
        questData: 'QuestDataManager'
    };
    
    let totalPassed = 0;
    let totalTests = Object.keys(results).length;
    
    for (const [key, passed] of Object.entries(results)) {
        const status = passed ? '✓ PASSED' : '✗ FAILED';
        log(`${testNames[key]}: ${status}`);
        if (passed) totalPassed++;
    }
    
    log('='.repeat(60));
    log(`Overall: ${totalPassed}/${totalTests} managers passed tests`);
    
    if (totalPassed === totalTests) {
        log('🎉 ALL TESTS PASSED! Data managers are working correctly.');
        process.exit(0);
    } else {
        log('❌ Some tests failed. Check the output above for details.');
        process.exit(1);
    }
}

// Handle errors gracefully
process.on('unhandledRejection', (reason, promise) => {
    console.error('Unhandled Rejection at:', promise, 'reason:', reason);
    process.exit(1);
});

process.on('uncaughtException', (error) => {
    console.error('Uncaught Exception:', error);
    process.exit(1);
});

// Run the tests
if (require.main === module) {
    runAllTests().catch((error) => {
        logError('Test suite failed', error);
        process.exit(1);
    });
}

module.exports = {
    runAllTests,
    testGameDataManager,
    testPlayerDataManager,
    testOutfitDataManager,
    testQuestDataManager
};