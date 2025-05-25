const CoopStoryData = require('./coopStoryData'); // Адаптированный сюжет для двух игроков

class CoopGameLogic {
    constructor() {
        this.games = new Map(); // roomId -> gameState
    }

    startGame(roomId, players) {
        const gameState = {
            roomId: roomId,
            players: players,
            currentScene: 'coop_awakening',
            chapter: 1,
            stats: {
                princess: {
                    awareness: 0,
                    outfit: 'Ночная рубашка',
                    loyalty: 50
                },
                helper: {
                    influence: 50,
                    secretsKnown: ['parents_dead', 'magic_items'],
                    trustLevel: 75
                }
            },
            flags: {},
            turnOrder: 'princess', // Чья очередь делать выбор
            gamePhase: 'introduction'
        };

        this.games.set(roomId, gameState);
        return this.getGameData(roomId);
    }

    makeChoice(roomId, playerId, choiceId, character) {
        const gameState = this.games.get(roomId);
        if (!gameState) {
            return { success: false, message: "Игра не найдена" };
        }

        // Проверяем, что игрок может делать выбор за этого персонажа
        const playerRole = this.getPlayerRole(gameState, playerId);
        if (playerRole !== character) {
            return { success: false, message: "Вы не можете управлять этим персонажем" };
        }

        // Обрабатываем выбор
        const result = this.processChoice(gameState, choiceId, character);
        if (result.success) {
            return {
                success: true,
                gameData: this.getGameData(roomId),
                message: result.message
            };
        }

        return result;
    }

    processChoice(gameState, choiceId, character) {
        const sceneData = CoopStoryData.getScene(gameState.currentScene);
        const choice = sceneData.choices[character]?.find(c => c.id === choiceId);
        
        if (!choice) {
            return { success: false, message: "Неверный выбор" };
        }

        // Применяем эффекты выбора
        if (choice.effects) {
            this.applyEffects(gameState, choice.effects, character);
        }

        // Проверяем, меняется ли сцена
        if (choice.nextScene) {
            gameState.currentScene = choice.nextScene;
        }

        // Меняем очередь хода
        this.switchTurn(gameState);

        return { 
            success: true, 
            message: choice.resultText || "Выбор сделан"
        };
    }

    applyEffects(gameState, effects, character) {
        Object.keys(effects).forEach(effect => {
            const value = effects[effect];
            
            switch (effect) {
            case 'awareness':
                gameState.stats.princess.awareness += value;
                break;
            case 'influence':
                gameState.stats.helper.influence += value;
                break;
            case 'loyalty':
                gameState.stats.princess.loyalty += value;
                break;
            case 'outfit':
                gameState.stats.princess.outfit = value;
                break;
            case 'flag':
                gameState.flags[value] = true;
                break;
            }
        });
    }

    switchTurn(gameState) {
        // Простая схема: попеременно
        gameState.turnOrder = gameState.turnOrder === 'princess' ? 'helper' : 'princess';
    }

    getGameData(roomId) {
        const gameState = this.games.get(roomId);
        if (!gameState) return null;

        const sceneData = CoopStoryData.getScene(gameState.currentScene);
        
        return {
            roomId: roomId,
            players: gameState.players,
            scene: {
                title: sceneData.title,
                text: sceneData.text
            },
            choices: {
                princess: this.getChoicesForCharacter(gameState, 'princess', sceneData),
                helper: this.getChoicesForCharacter(gameState, 'helper', sceneData)
            },
            stats: gameState.stats,
            currentTurn: gameState.turnOrder,
            chapter: gameState.chapter
        };
    }

    getChoicesForCharacter(gameState, character, sceneData) {
        // Возвращаем выборы только если сейчас ход этого персонажа
        if (gameState.turnOrder !== character) {
            return [];
        }

        const choices = sceneData.choices[character] || [];
        return choices.filter(choice => {
            return this.isChoiceAvailable(choice, gameState, character);
        });
    }

    isChoiceAvailable(choice, gameState, character) {
        if (!choice.requirements) return true;

        return choice.requirements.every(req => {
            switch (req.type) {
            case 'awareness':
                return gameState.stats.princess.awareness >= req.value;
            case 'influence':
                return gameState.stats.helper.influence >= req.value;
            case 'flag':
                return gameState.flags[req.value];
            default:
                return true;
            }
        });
    }

    getPlayerRole(gameState, playerId) {
        if (gameState.players.princess?.id === playerId) {
            return 'princess';
        }
        if (gameState.players.helper?.id === playerId) {
            return 'helper';
        }
        return null;
    }

    removeGame(roomId) {
        this.games.delete(roomId);
    }
}

module.exports = CoopGameLogic;
