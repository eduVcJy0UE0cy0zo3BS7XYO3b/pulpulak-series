/**
 * Cooperative Story Data loaded from S-expression files
 */

const path = require('path');
const DataLoader = require('./dataLoader');

class CoopStoryDataSCM {
    constructor() {
        this.loader = new DataLoader();
        this.scenes = null;
        this.loadData();
    }

    loadData() {
        const storyFile = path.join(__dirname, 'story.scm');
        this.scenes = this.loader.loadSExpFile(storyFile);
    }

    static getScene(sceneId) {
        if (!this.instance) {
            this.instance = new CoopStoryDataSCM();
        }
        return this.instance.scenes[sceneId];
    }

    static getAllScenes() {
        if (!this.instance) {
            this.instance = new CoopStoryDataSCM();
        }
        return Object.keys(this.instance.scenes);
    }
}

module.exports = CoopStoryDataSCM;