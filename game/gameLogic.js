const GameState = require('./gameState');
const storyData = require('./storyData'); // Создадим отдельный файл с сюжетом

class GameLogic {
    constructor() {
        this.games = new Map();
    }

    createGame(playerId, difficulty = 'normal') {
        const gameState = new GameState(difficulty);
        this.games.set(playerId, gameState);
        return this.getGameData(playerId);
    }

    makeChoice(playerId, choiceId) {
        const gameState = this.games.get(playerId);
        if (!gameState) {
            return { success: false, message: "Игра не найдена" };
        }

        const result = gameState.processChoice(choiceId);
        if (result.success) {
            return {
                success: true,
                gameData: this.getGameData(playerId),
                message: result.message
            };
        }
        
        return result;
    }

    getGameData(playerId) {
        const gameState = this.games.get(playerId);
        if (!gameState) return null;

        const currentScene = gameState.getCurrentScene();
        const sceneData = storyData.getScene(currentScene.id, gameState);
        
        return {
            currentScene: currentScene.id,
            scene: {
                title: sceneData.title,
                text: sceneData.text
            },
            choices: this.getAvailableChoices(gameState, sceneData),
            stats: {
                loyalty: gameState.loyalty,
                awareness: gameState.awareness,
                chapter: gameState.chapter,
                currentOutfit: gameState.princess.currentOutfit
            },
            inventory: gameState.getInventoryDisplay(),
            flags: gameState.flags
        };
    }

    getAvailableChoices(gameState, sceneData) {
        return sceneData.choices.map(choice => {
            const available = this.isChoiceAvailable(choice, gameState);
            return {
                id: choice.id,
                text: choice.text,
                description: choice.description,
                consequence: choice.consequence,
                disabled: !available.allowed,
                reason: available.reason
            };
        });
    }

    isChoiceAvailable(choice, gameState) {
        // Проверяем требования для выбора
        if (choice.requirements) {
            for (const req of choice.requirements) {
                switch (req.type) {
                case 'outfit':
                    if (gameState.princess.currentOutfit !== req.value) {
                        return { allowed: false, reason: `Нужен наряд: ${req.value}` };
                    }
                    break;
                case 'loyalty':
                    if (gameState.loyalty < req.value) {
                        return { allowed: false, reason: `Нужна лояльность: ${req.value}` };
                    }
                    break;
                case 'awareness':
                    if (gameState.awareness < req.value) {
                        return { allowed: false, reason: `Нужно понимание: ${req.value}` };
                    }
                    break;
                case 'item':
                    if (!gameState.hasItem(req.value)) {
                        return { allowed: false, reason: `Нужен предмет: ${req.value}` };
                    }
                    break;
                case 'flag':
                    if (!gameState.flags[req.value]) {
                        return { allowed: false, reason: req.reason || 'Условие не выполнено' };
                    }
                    break;
                }
            }
        }
        
        return { allowed: true };
    }

    // Сохранение и загрузка
    saveGame(playerId, slot) {
        const gameState = this.games.get(playerId);
        if (!gameState) return { success: false, message: "Игра не найдена" };

        // В реальном приложении здесь была бы база данных
        const saveData = {
            gameState: gameState.serialize(),
            saveTime: new Date().toISOString(),
            chapter: gameState.chapter,
            sceneName: storyData.getScene(gameState.currentScene).title
        };

        // Временно сохраняем в localStorage (для демо)
        if (typeof localStorage !== 'undefined') {
            localStorage.setItem(`pulpulak_save_${slot}`, JSON.stringify(saveData));
        }

        return { success: true, message: "Игра сохранена" };
    }

    loadGame(playerId, slot) {
        // Загрузка из localStorage (для демо)
        if (typeof localStorage === 'undefined') {
            return { success: false, message: "Сохранения недоступны" };
        }

        const saveData = localStorage.getItem(`pulpulak_save_${slot}`);
        if (!saveData) {
            return { success: false, message: "Сохранение не найдено" };
        }

        try {
            const data = JSON.parse(saveData);
            const gameState = GameState.deserialize(data.gameState);
            this.games.set(playerId, gameState);
            return { success: true, gameData: this.getGameData(playerId) };
        } catch (error) {
            return { success: false, message: "Ошибка загрузки сохранения" };
        }
    }

    getSaveSlots() {
        const slots = {};
        for (let i = 1; i <= 3; i++) {
            const slotKey = `slot${i}`;
            if (typeof localStorage !== 'undefined') {
                const saveData = localStorage.getItem(`pulpulak_save_${slotKey}`);
                if (saveData) {
                    try {
                        slots[slotKey] = JSON.parse(saveData);
                    } catch (error) {
                        // Игнорируем поврежденные сохранения
                    }
                }
            }
        }
        return slots;
    }
}

module.exports = GameLogic;
