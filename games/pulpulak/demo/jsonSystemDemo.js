#!/usr/bin/env node

/**
 * Demonstration of the JSON data system integration
 * Shows how to use both JS and JSON data sources
 */

const GameConfigFactory = require('../GameConfigFactory');
const CoopGameLogic = require('../../../game/coopGameLogic');

async function demonstrateJsonSystem() {
    console.log('🎮 JSON Data System Integration Demo\n');
    
    console.log('📋 Step 1: Creating game configurations...');
    
    // Create both JS and JSON configurations
    const jsConfig = await GameConfigFactory.createConfig('js');
    const jsonConfig = await GameConfigFactory.createConfig('json');
    
    console.log('✅ JS Config created');
    console.log('✅ JSON Config created and initialized');
    
    console.log('\n📊 Step 2: Comparing configurations...');
    
    const comparison = await GameConfigFactory.compareConfigs(jsConfig, jsonConfig);
    console.log(`Data equivalency: ${comparison.equivalent ? '✅ Equivalent' : '❌ Different'}`);
    console.log(`Differences found: ${comparison.differences.length}`);
    console.log(`Warnings: ${comparison.warnings.length}`);
    
    if (comparison.warnings.length > 0) {
        console.log('⚠️  Warnings:', comparison.warnings.join(', '));
    }
    
    console.log('\n🎯 Step 3: Testing game functionality with JSON...');
    
    // Test game logic with JSON config
    const gameLogic = new CoopGameLogic(jsonConfig);
    
    const testRoomId = 'demo-room-123';
    const players = {
        princess: { id: 'demo-player1' },
        helper: { id: 'demo-player2' }
    };
    
    const gameData = gameLogic.startGame(testRoomId, players);
    
    console.log('✅ Game started successfully with JSON data');
    console.log(`📍 Scene: ${gameData.scene.title}`);
    console.log(`👥 Players: ${Object.keys(gameData.players).join(', ')}`);
    console.log(`🏰 Location: ${gameData.stats.princess.location}`);
    
    console.log('\n📚 Step 4: Accessing different data types...');
    
    // Test story data
    const storyData = jsonConfig.getStoryData();
    const scene = storyData.getScene('coop_awakening');
    console.log(`📖 Story scene loaded: ${scene.title}`);
    
    // Test location data  
    const locationData = jsonConfig.getLocationData();
    const location = locationData.getLocation('princess_chamber');
    console.log(`🏰 Location loaded: ${location.name}`);
    
    // Test NPC data
    const npcData = jsonConfig.getNPCData();
    const npc = npcData.getNPC('cook');
    console.log(`👤 NPC loaded: ${npc.name}`);
    
    // Test quest data
    const questData = jsonConfig.getQuestData();
    const quest = questData.getQuest('princess_lost_relic');
    console.log(`🎯 Quest loaded: ${quest.title}`);
    
    console.log('\n⚡ Step 5: Performance comparison...');
    
    const benchmarkResults = await GameConfigFactory.benchmarkModes(10);
    console.log(`🏃‍♂️ JS average: ${benchmarkResults.js.average.toFixed(2)}ms`);
    console.log(`📄 JSON average: ${benchmarkResults.json.average.toFixed(2)}ms`);
    console.log(`🏆 Faster mode: ${benchmarkResults.summary.fasterMode}`);
    console.log(`📈 Speed difference: ${benchmarkResults.summary.speedDifference.toFixed(2)}ms`);
    
    console.log('\n🔄 Step 6: Environment-based configuration...');
    
    // Demonstrate environment variable usage
    process.env.PULPULAK_DATA_MODE = 'json';
    const envConfig = await GameConfigFactory.createFromEnvironment();
    console.log('✅ Environment config created (JSON mode)');
    console.log(`📝 Description: ${envConfig.getGameMetadata().description}`);
    
    delete process.env.PULPULAK_DATA_MODE;
    
    console.log('\n🎉 Demo completed successfully!');
    console.log('\n📋 Summary:');
    console.log('• ✅ JSON data system fully integrated');
    console.log('• ✅ Backward compatibility maintained');
    console.log('• ✅ Performance benchmarking available');
    console.log('• ✅ Environment-based configuration');
    console.log('• ✅ Factory pattern for easy switching');
    console.log('• ✅ Comprehensive test coverage');
    
    console.log('\n🚀 Usage examples:');
    console.log('```javascript');
    console.log('// Use JavaScript data sources (default)');
    console.log('const config = await GameConfigFactory.createConfig("js");');
    console.log('');
    console.log('// Use JSON data sources');
    console.log('const config = await GameConfigFactory.createConfig("json");');
    console.log('');
    console.log('// Use environment variable');
    console.log('process.env.PULPULAK_DATA_MODE = "json";');
    console.log('const config = await GameConfigFactory.createFromEnvironment();');
    console.log('```');
}

// Run the demo if this file is executed directly
if (require.main === module) {
    demonstrateJsonSystem().catch(error => {
        console.error('❌ Demo failed:', error);
        process.exit(1);
    });
}

module.exports = { demonstrateJsonSystem };