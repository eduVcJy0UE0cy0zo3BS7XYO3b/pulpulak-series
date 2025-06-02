/**
 * Tests for GameConfigFactory - migration between JS and JSON data sources
 */

const GameConfigFactory = require('../GameConfigFactory');

describe('GameConfigFactory', () => {
    describe('Config Creation', () => {
        test('should create JS-based config', async () => {
            const config = await GameConfigFactory.createConfig('js');
            
            expect(config).toBeDefined();
            expect(config.getGameMetadata().description).not.toContain('JSON версия');
            
            // Should work after initialization  
            await config.initialize();
            const storyData = config.getStoryData();
            expect(storyData).toBeDefined();
        });

        test('should create JSON-based config', async () => {
            const config = await GameConfigFactory.createConfig('json');
            
            expect(config).toBeDefined();
            expect(config.getGameMetadata().description).toContain('JSON версия');
            expect(config.isInitialized()).toBe(true);
            
            // Should work after initialization
            const storyData = config.getStoryData();
            expect(storyData).toBeDefined();
        });

        test('should handle invalid mode', async () => {
            await expect(GameConfigFactory.createConfig('invalid')).rejects.toThrow('Unknown data source mode');
        });

        test('should create from environment variable', async () => {
            // Test default mode
            const defaultConfig = await GameConfigFactory.createFromEnvironment();
            expect(defaultConfig).toBeDefined();
            
            // Test with environment override
            process.env.PULPULAK_DATA_MODE = 'json';
            const envConfig = await GameConfigFactory.createFromEnvironment();
            expect(envConfig.getGameMetadata().description).toContain('JSON версия');
            
            // Clean up
            delete process.env.PULPULAK_DATA_MODE;
        });
    });

    describe('Mode Support', () => {
        test('should return available modes', () => {
            const modes = GameConfigFactory.getAvailableModes();
            expect(modes).toContain('js');
            expect(modes).toContain('json');
            expect(modes.length).toBe(2);
        });

        test('should check mode support', () => {
            expect(GameConfigFactory.isModeSupported('js')).toBe(true);
            expect(GameConfigFactory.isModeSupported('json')).toBe(true);
            expect(GameConfigFactory.isModeSupported('JS')).toBe(true); // Case insensitive
            expect(GameConfigFactory.isModeSupported('invalid')).toBe(false);
        });
    });

    describe('Config Comparison', () => {
        test('should compare JS and JSON configs', async () => {
            const jsConfig = await GameConfigFactory.createConfig('js');
            const jsonConfig = await GameConfigFactory.createConfig('json');
            
            const comparison = await GameConfigFactory.compareConfigs(jsConfig, jsonConfig);
            
            expect(comparison).toBeDefined();
            expect(comparison.equivalent).toBeDefined();
            expect(Array.isArray(comparison.differences)).toBe(true);
            expect(Array.isArray(comparison.warnings)).toBe(true);
            
            // Should be mostly equivalent with only version differences
            console.log('Config comparison:', comparison);
            if (!comparison.equivalent) {
                console.log('Differences found:', comparison.differences);
            }
        });

        test('should identify data consistency', async () => {
            const jsConfig = await GameConfigFactory.createConfig('js');
            const jsonConfig = await GameConfigFactory.createConfig('json');
            
            // Initialize JS config
            await jsConfig.initialize();
            
            // Both should have the same basic game structure
            expect(jsConfig.getCharacters()).toEqual(jsonConfig.getCharacters());
            expect(jsConfig.getGameMetadata().id).toBe(jsonConfig.getGameMetadata().id);
            
            // Scene data should be equivalent
            const jsStory = jsConfig.getStoryData();
            const jsonStory = jsonConfig.getStoryData();
            
            const jsScene = jsStory.getScene('coop_awakening');
            const jsonScene = jsonStory.getScene('coop_awakening');
            
            expect(jsScene.title).toBe(jsonScene.title);
            
            // Quest data should be equivalent
            const jsQuest = jsConfig.getQuestData();
            const jsonQuest = jsonConfig.getQuestData();
            
            const jsQuestData = jsQuest.getQuest('princess_lost_relic');
            const jsonQuestData = jsonQuest.getQuest('princess_lost_relic');
            
            expect(jsQuestData.title).toBe(jsonQuestData.title);
        });
    });

    describe('Performance Benchmarking', () => {
        test('should benchmark different modes', async () => {
            // Run a quick benchmark with few iterations for testing
            const results = await GameConfigFactory.benchmarkModes(5);
            
            expect(results).toBeDefined();
            expect(results.js).toBeDefined();
            expect(results.json).toBeDefined();
            expect(results.summary).toBeDefined();
            
            expect(typeof results.js.total).toBe('number');
            expect(typeof results.json.total).toBe('number');
            expect(typeof results.js.average).toBe('number');
            expect(typeof results.json.average).toBe('number');
            
            expect(results.summary.fasterMode).toMatch(/^(js|json)$/);
            expect(typeof results.summary.speedDifference).toBe('number');
            expect(typeof results.summary.jsAdvantage).toBe('number');
            
            console.log('Performance benchmark results:');
            console.log(`JS average: ${results.js.average.toFixed(2)}ms`);
            console.log(`JSON average: ${results.json.average.toFixed(2)}ms`);
            console.log(`Faster mode: ${results.summary.fasterMode}`);
            console.log(`Speed difference: ${results.summary.speedDifference.toFixed(2)}ms`);
        }, 30000); // Longer timeout for benchmark
    });

    describe('Migration Scenarios', () => {
        test('should handle switching from JS to JSON', async () => {
            // Start with JS config
            const jsConfig = await GameConfigFactory.createConfig('js');
            await jsConfig.initialize();
            const jsData = jsConfig.getStoryData().getScene('coop_awakening');
            
            // Switch to JSON config
            const jsonConfig = await GameConfigFactory.createConfig('json');
            const jsonData = jsonConfig.getStoryData().getScene('coop_awakening');
            
            // Data should be equivalent
            expect(jsonData.title).toBe(jsData.title);
        });

        test('should handle multiple config instances', async () => {
            // Create multiple instances of both types
            const configs = await Promise.all([
                GameConfigFactory.createConfig('js'),
                GameConfigFactory.createConfig('json'),
                GameConfigFactory.createConfig('js'),
                GameConfigFactory.createConfig('json')
            ]);
            
            // All should be functional
            configs.forEach(config => {
                expect(config.getCharacters()).toContain('princess');
                expect(config.getCharacters()).toContain('helper');
            });
        });

        test('should maintain interface compatibility', async () => {
            const jsConfig = await GameConfigFactory.createConfig('js');
            const jsonConfig = await GameConfigFactory.createConfig('json');
            
            // Both should implement the same interface methods
            const requiredMethods = [
                'getStoryData', 'getLocationData', 'getNPCData', 'getQuestData',
                'getCharacters', 'getCharacterNames', 'getCharacterRoles',
                'getInitialLocation', 'getInitialOutfit', 'getAvailableOutfits',
                'canSwitchOutfits', 'getDynamicChoices', 'createOutfitSwapChoice',
                'getRequestHandlers', 'validateGameRules', 'getGameConstants',
                'getGameMetadata'
            ];

            requiredMethods.forEach(method => {
                expect(typeof jsConfig[method]).toBe('function');
                expect(typeof jsonConfig[method]).toBe('function');
            });
        });
    });

    describe('Error Handling', () => {
        test('should handle config creation failures gracefully', async () => {
            // Mock a failure in JSON config creation
            const originalCreate = require('../PulpulakGameConfigJson').create;
            require('../PulpulakGameConfigJson').create = jest.fn().mockRejectedValue(new Error('JSON load failed'));
            
            await expect(GameConfigFactory.createConfig('json')).rejects.toThrow('JSON load failed');
            
            // Restore original function
            require('../PulpulakGameConfigJson').create = originalCreate;
        });

        test('should validate environment configuration', async () => {
            // Test with invalid environment variable
            process.env.PULPULAK_DATA_MODE = 'invalid_mode';
            
            await expect(GameConfigFactory.createFromEnvironment()).rejects.toThrow('Unknown data source mode');
            
            // Clean up
            delete process.env.PULPULAK_DATA_MODE;
        });
    });
});