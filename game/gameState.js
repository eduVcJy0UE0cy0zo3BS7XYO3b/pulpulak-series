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
        this.updateAvailableActions();
    }

    // Обновить доступные действия в зависимости от текущей сцены
    updateAvailableActions() {
        switch(this.currentScene) {
        case "awakening":
            this.availableActions = [
                { id: "wake_up", name: "Проснуться", description: "Открыть глаза и осознать происходящее" },
                { id: "stay_in_bed", name: "Остаться в постели", description: "Попытаться поспать еще немного" }
            ];
            break;
            
        case "meeting_helper":
            this.availableActions = [
                { id: "listen_explanation", name: "Выслушать объяснение", description: "Внимательно выслушать 'сестру'" },
                { id: "ask_questions", name: "Задать вопросы", description: "Расспросить о родителях и ситуации" },
                { id: "look_around", name: "Осмотреться", description: "Изучить комнату и обстановку" }
            ];
            break;
            
        case "outfit_selection":
            this.availableActions = [
                { id: "keep_nightgown", name: "Остаться в ночной рубашке", description: "Не переодеваться пока" },
                { id: "wear_princess_dress", name: "Надеть княжеское платье", description: "Одеться подобающе статусу" },
                { id: "wear_common_dress", name: "Надеть простое платье", description: "Переодеться в одежду 'сестры'" },
                { id: "switch_outfits", name: "Поменяться одеждой", description: "Поменяться нарядами с 'сестрой'" }
            ];
            break;
            
        case "castle_exploration":
            this.availableActions = [
                { id: "visit_parents", name: "Пойти к родителям", description: "Навестить родителей в тронном зале" },
                { id: "explore_kitchen", name: "Пойти на кухню", description: "Найти что-нибудь поесть" },
                { id: "visit_garden", name: "Пойти в сад", description: "Прогуляться по замковому саду" },
                { id: "check_armory", name: "Заглянуть в арсенал", description: "Посмотреть оружие и доспехи" },
                { id: "gather_belongings", name: "Собрать вещи", description: "Подготовиться к путешествию" }
            ];
            break;
        }
    }

    // Выполнить действие
    executeAction(actionId, playerId) {
        const action = this.availableActions.find(a => a.id === actionId);
        if (!action) {
            return { success: false, message: "Неизвестное действие" };
        }

        return this.processAction(actionId);
    }

    processAction(actionId) {
        switch(actionId) {
        case "wake_up":
            this.changeScene("meeting_helper");
            return {
                success: true,
                message: "Вы проснулись. Рядом с кроватью стоит девушка, очень похожая на вас...",
                newScene: "meeting_helper"
            };
            
        case "listen_explanation":
            this.conversationHistory.push("listened_to_helper");
            return {
                success: true,
                message: "'Сестра' рассказывает: 'Родители уехали на войну. Ведьма-тетя велела нам оставаться здесь и ждать писем.'",
                nextAction: "outfit_selection"
            };
            
        case "switch_outfits":
            // Меняемся одеждой с помощницей
            const princessOutfit = this.princess.currentOutfit;
            const helperOutfit = this.witchHelper.currentOutfit;
            
            this.princess.changeOutfit(helperOutfit);
            this.witchHelper.currentOutfit = princessOutfit;
            
            this.changeScene("castle_exploration");
            return {
                success: true,
                message: "Вы поменялись одеждой. Теперь вы выглядите как простолюдинка, а 'сестра' - как княжна.",
                newScene: "castle_exploration"
            };
            
        case "visit_parents":
            // Критический момент - видит убийство родителей
            this.changeScene("tragic_discovery");
            this.princess.increaseAwareness(50);
            return {
                success: true,
                message: "Подходя к тронному залу, вы видите ужасную сцену...",
                newScene: "tragic_discovery",
                critical: true
            };
            
        default:
            return { success: false, message: "Действие пока не реализовано" };
        }
    }
}

module.exports = GameState;
