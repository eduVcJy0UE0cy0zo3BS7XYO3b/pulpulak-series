/**
 * Factory for creating PulpulakGameConfig instances with different data sources
 * Supports both JavaScript modules and JSON data sources
 */

const PulpulakGameConfig = require('./PulpulakGameConfig');
const PulpulakGameConfigJson = require('./PulpulakGameConfigJson');

class GameConfigFactory {
    /**
     * Create a game config instance based on the specified data source mode
     * @param {string} mode - 'js' for JavaScript modules, 'json' for JSON data sources
     * @returns {Promise<IGameConfig>} Initialized game configuration
     */
    static async createConfig(mode = 'js') {
        switch (mode.toLowerCase()) {
            case 'js':
            case 'javascript':
                return new PulpulakGameConfig();
                
            case 'json':
                return await PulpulakGameConfigJson.create();
                
            default:
                throw new Error(`Unknown data source mode: ${mode}. Use 'js' or 'json'.`);
        }
    }

    /**
     * Create config from environment variable or default
     * @param {string} defaultMode - Default mode if env var not set
     * @returns {Promise<IGameConfig>} Initialized game configuration
     */
    static async createFromEnvironment(defaultMode = 'js') {
        const mode = process.env.PULPULAK_DATA_MODE || defaultMode;
        return this.createConfig(mode);
    }

    /**
     * Get available data source modes
     * @returns {string[]} Array of supported modes
     */
    static getAvailableModes() {
        return ['js', 'json'];
    }

    /**
     * Check if a mode is supported
     * @param {string} mode - Mode to check
     * @returns {boolean} True if mode is supported
     */
    static isModeSupported(mode) {
        return this.getAvailableModes().includes(mode.toLowerCase());
    }

    /**
     * Compare two config instances to ensure they provide equivalent data
     * @param {IGameConfig} config1 - First config to compare
     * @param {IGameConfig} config2 - Second config to compare
     * @returns {Promise<Object>} Comparison result with details
     */
    static async compareConfigs(config1, config2) {
        const comparison = {
            equivalent: true,
            differences: [],
            warnings: []
        };

        try {
            // Compare metadata
            const meta1 = config1.getGameMetadata();
            const meta2 = config2.getGameMetadata();
            
            if (meta1.id !== meta2.id) {
                comparison.differences.push(`Game ID differs: ${meta1.id} vs ${meta2.id}`);
                comparison.equivalent = false;
            }

            // Compare character configuration
            const chars1 = config1.getCharacters();
            const chars2 = config2.getCharacters();
            
            if (JSON.stringify(chars1) !== JSON.stringify(chars2)) {
                comparison.differences.push('Character lists differ');
                comparison.equivalent = false;
            }

            // Compare scene data
            const story1 = config1.getStoryData();
            const story2 = config2.getStoryData();
            
            const scene1 = story1.getScene('coop_awakening');
            const scene2 = story2.getScene('coop_awakening');
            
            if (scene1?.title !== scene2?.title) {
                comparison.differences.push('Story scene titles differ');
                comparison.equivalent = false;
            }

            // Compare quest data
            const quest1 = config1.getQuestData();
            const quest2 = config2.getQuestData();
            
            const q1 = quest1.getQuest('princess_lost_relic');
            const q2 = quest2.getQuest('princess_lost_relic');
            
            if (q1?.title !== q2?.title) {
                comparison.differences.push('Quest titles differ');
                comparison.equivalent = false;
            }

            // Version differences are expected
            if (meta1.description !== meta2.description && 
                meta2.description.includes('JSON версия')) {
                comparison.warnings.push('Description indicates JSON version');
            }

        } catch (error) {
            comparison.equivalent = false;
            comparison.differences.push(`Comparison error: ${error.message}`);
        }

        return comparison;
    }

    /**
     * Benchmark different config modes for performance comparison
     * @param {number} iterations - Number of iterations to run
     * @returns {Promise<Object>} Performance comparison results
     */
    static async benchmarkModes(iterations = 100) {
        const results = {
            js: { initialization: 0, dataAccess: 0 },
            json: { initialization: 0, dataAccess: 0 }
        };

        // Benchmark JavaScript mode
        const jsStart = Date.now();
        for (let i = 0; i < iterations; i++) {
            const config = new PulpulakGameConfig();
            await config.initialize();
            // Access some data to measure full cost
            config.getStoryData();
            config.getLocationData();
            config.getNPCData();
            config.getQuestData();
        }
        results.js.total = Date.now() - jsStart;

        // Benchmark JSON mode (including initialization cost)
        const jsonStart = Date.now();
        for (let i = 0; i < iterations; i++) {
            const config = await PulpulakGameConfigJson.create();
            // Access some data
            config.getStoryData();
            config.getLocationData();
            config.getNPCData();
            config.getQuestData();
        }
        results.json.total = Date.now() - jsonStart;

        // Calculate averages
        results.js.average = results.js.total / iterations;
        results.json.average = results.json.total / iterations;
        
        results.summary = {
            fasterMode: results.js.average < results.json.average ? 'js' : 'json',
            speedDifference: Math.abs(results.js.average - results.json.average),
            jsAdvantage: results.json.average / results.js.average
        };

        return results;
    }
}

module.exports = GameConfigFactory;