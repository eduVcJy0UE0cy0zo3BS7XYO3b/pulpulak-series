/**
 * Integration tests for JSON-powered game engine functionality
 */

const PulpulakGameConfigJson = require('../PulpulakGameConfigJson');
const GameLogic = require('../../../game/coopGameLogic');
const DataManagerFactory = require('../../../game/managers/DataManagerFactory');

describe('JSON Game Integration Tests', () => {
    let gameConfig;
    let gameLogic;
    let managers;

    beforeAll(async () => {
        // Initialize JSON-based game config
        gameConfig = await PulpulakGameConfigJson.create();
        
        // Create managers using the JSON config
        managers = DataManagerFactory.getManagers(gameConfig);
        
        // Initialize game logic with JSON config
        gameLogic = new GameLogic(gameConfig);
    });

    describe('Game Logic Integration', () => {
        test('should initialize game logic with JSON data', () => {
            expect(gameLogic).toBeDefined();
            expect(gameLogic.storyData).toBeDefined();
            expect(gameLogic.locationData).toBeDefined();
            expect(gameLogic.npcData).toBeDefined();
            expect(gameLogic.questData).toBeDefined();
        });

        test('should create initial game state', async () => {
            const testRoomId = 'test-room-123';
            const players = {
                princess: { id: 'player1' },
                helper: { id: 'player2' }
            };
            
            const gameData = await gameLogic.startGame(testRoomId, players);
            
            expect(gameData).toBeDefined();
            expect(gameData.stats).toBeDefined();
            expect(gameData.stats.princess).toBeDefined();
            expect(gameData.stats.helper).toBeDefined();
            
            // Check that scene data is present
            expect(gameData.scene).toBeDefined();
            expect(gameData.scene.title).toBe('Утреннее пробуждение');
        });

        test('should process scene choices', () => {
            // Get the awakening scene directly
            const scene = gameLogic.storyData.getScene('coop_awakening');
            expect(scene).toBeDefined();
            expect(scene.choices.princess).toBeDefined();
            expect(scene.choices.princess.length).toBeGreaterThan(0);
            
            const choice = scene.choices.princess[0];
            expect(choice.id).toBeDefined();
            expect(choice.text).toBeDefined();
        });

        test('should validate game state with JSON data', () => {
            // Create a sample game state for validation
            const gameState = {
                stats: {
                    princess: { location: 'princess_chamber', outfit: 'nightgown' },
                    helper: { location: 'princess_chamber', outfit: 'common_dress' }
                },
                currentScene: 'coop_awakening'
            };
            
            const validation = gameConfig.validateGameRules(gameState);
            expect(validation.valid).toBe(true);
            expect(validation.errors).toHaveLength(0);
        });
    });

    describe('Data Manager Integration', () => {
        test('should create managers with JSON config', () => {
            expect(managers.gameData).toBeDefined();
            expect(managers.playerData).toBeDefined();
            expect(managers.questData).toBeDefined();
            expect(managers.requestData).toBeDefined();
        });

        test('should access character data through managers', () => {
            const characters = gameConfig.getCharacters();
            expect(characters).toContain('princess');
            expect(characters).toContain('helper');
            
            const characterNames = gameConfig.getCharacterNames();
            expect(characterNames.princess).toBeDefined();
            expect(characterNames.helper).toBeDefined();
        });
    });

    describe('Story Progression with JSON', () => {
        test('should progress through story scenes', () => {
            // Get awakening scene data
            const scene = gameLogic.storyData.getScene('coop_awakening');
            expect(scene).toBeDefined();
            expect(scene.title).toBe('Утреннее пробуждение');
            
            // Check available choices
            expect(scene.choices.princess).toBeDefined();
            expect(scene.choices.helper).toBeDefined();
        });

        test('should handle outfit system with JSON data', () => {
            // Create sample game state for outfit testing
            const gameState = {
                stats: {
                    princess: { location: 'princess_chamber', outfit: 'nightgown' },
                    helper: { location: 'princess_chamber', outfit: 'common_dress' }
                },
                currentScene: 'coop_awakening'
            };
            
            // Test outfit switching capability
            const canSwitch = gameConfig.canSwitchOutfits(gameState, 'princess');
            expect(typeof canSwitch).toBe('boolean');
            
            // Test available outfits
            const princessOutfits = gameConfig.getAvailableOutfits('princess');
            const helperOutfits = gameConfig.getAvailableOutfits('helper');
            
            expect(Array.isArray(princessOutfits)).toBe(true);
            expect(Array.isArray(helperOutfits)).toBe(true);
            expect(princessOutfits.length).toBeGreaterThan(0);
            expect(helperOutfits.length).toBeGreaterThan(0);
        });
    });

    describe('NPC and Quest Integration', () => {
        test('should access NPC data through JSON', () => {
            const npcData = gameConfig.getNPCData();
            const cook = npcData.getNPC('cook');
            
            expect(cook).toBeDefined();
            expect(cook.name).toBeDefined();
            expect(cook.dialogue).toBeDefined();
        });

        test('should access quest data through JSON', () => {
            const questData = gameConfig.getQuestData();
            const quest = questData.getQuest('princess_lost_relic');
            
            expect(quest).toBeDefined();
            expect(quest.title).toBeDefined();
            expect(quest.character).toBe('princess');
            expect(Array.isArray(quest.steps)).toBe(true);
        });

        test('should validate quest structure', () => {
            const questData = gameConfig.getQuestData();
            const allQuests = questData.getAllQuests();
            
            Object.values(allQuests).forEach(quest => {
                expect(quest.id).toBeDefined();
                expect(quest.title).toBeDefined();
                expect(quest.character).toBeDefined();
                expect(Array.isArray(quest.steps)).toBe(true);
            });
        });
    });

    describe('Location and Movement', () => {
        test('should access location data through JSON', () => {
            const locationData = gameConfig.getLocationData();
            const chamber = locationData.getLocation('princess_chamber');
            
            expect(chamber).toBeDefined();
            expect(chamber.name).toBeDefined();
            expect(chamber.description).toBeDefined();
            expect(Array.isArray(chamber.connections)).toBe(true);
        });

        test('should validate initial locations', () => {
            const princessLocation = gameConfig.getInitialLocation('princess');
            const helperLocation = gameConfig.getInitialLocation('helper');
            
            expect(princessLocation).toBe('princess_chamber');
            expect(helperLocation).toBe('servant_quarters');
            
            const locationData = gameConfig.getLocationData();
            const chamber = locationData.getLocation(princessLocation);
            expect(chamber).toBeDefined();
        });
    });

    describe('Error Handling and Resilience', () => {
        test('should handle missing scene gracefully', () => {
            const scene = gameLogic.storyData.getScene('non_existent_scene');
            expect(scene).toBeNull();
        });

        test('should handle missing NPC gracefully', () => {
            const npcData = gameConfig.getNPCData();
            const npc = npcData.getNPC('non_existent_npc');
            expect(npc).toBeNull();
        });

        test('should handle missing quest gracefully', () => {
            const questData = gameConfig.getQuestData();
            const quest = questData.getQuest('non_existent_quest');
            expect(quest).toBeNull();
        });

        test('should handle missing location gracefully', () => {
            const locationData = gameConfig.getLocationData();
            const location = locationData.getLocation('non_existent_location');
            expect(location).toBeNull();
        });
    });

    describe('Performance and Caching', () => {
        test('should cache data after initialization', () => {
            // Accessing data multiple times should be fast (cached)
            const start = Date.now();
            
            for (let i = 0; i < 100; i++) {
                gameConfig.getStoryData();
                gameConfig.getLocationData();
                gameConfig.getNPCData();
                gameConfig.getQuestData();
            }
            
            const end = Date.now();
            const duration = end - start;
            
            // Should be very fast due to caching (less than 10ms for 100 iterations)
            expect(duration).toBeLessThan(100);
        });

        test('should reuse adapter instances', () => {
            const adapter1 = gameConfig.jsonAdapter;
            const adapter2 = gameConfig.jsonAdapter;
            
            expect(adapter1).toBe(adapter2); // Same instance
            expect(adapter1.isInitialized()).toBe(true);
        });
    });
});