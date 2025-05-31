const { Princess, WitchHelper } = require('./princess');

class GameState {
    constructor() {
        this.currentScene = "awakening";
        this.princess = new Princess();
        this.witchHelper = new WitchHelper();
        this.gameTime = "early_morning";
        this.invasionStatus = "approaching"; // приближается вторжение
        this.availableActions = [];
        this.visitedLocations = [];
        this.conversationHistory = [];
        this.relationships = {}; // отношения с NPC
    }

    // Получить текущую сцену
    getCurrentScene() {
        return {
            scene: this.currentScene,
            princess: this.princess,
            witchHelper: this.witchHelper,
            gameTime: this.gameTime,
            availableActions: this.availableActions
        };
    }

    // Изменить сцену
    changeScene(newScene) {
        this.currentScene = newScene;
    }

}

module.exports = GameState;
