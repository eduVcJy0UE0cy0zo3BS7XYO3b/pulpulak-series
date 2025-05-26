const CoopStoryData = require('./coopStoryData');

class CoopGameLogic {
    constructor() {
        this.games = new Map(); // roomId -> gameState
        this.outfitRequests = new Map(); // roomId -> activeRequest
    }

    // Запуск игры
    startGame(roomId, players) {
        const gameState = {
            roomId: roomId,
            players: players,
            currentScene: 'coop_awakening',
            turnOrder: 'princess', 
            chapter: 1,
            location: 'princess_chamber',
            npcsPresent: [], // ВАЖНО: в начале никого нет - можно переодеваться!
            stats: {
                princess: {
                    outfit: 'nightgown',
                    awareness: 0,
                    loyalty: {},
                    inventory: []
                },
                helper: {
                    outfit: 'common_dress',
                    awareness: 0,
                    secrets_revealed: 0,
                    inventory: ['translation_earrings', 'voice_medallion']
                }
            }
        };

        this.games.set(roomId, gameState);
        return this.getGameData(roomId);
    }

    // Создать запрос на обмен одеждой
    createOutfitSwapRequest(roomId, fromPlayerId, fromCharacter) {
        const gameState = this.games.get(roomId);
        if (!gameState) {
            return { success: false, message: "Игра не найдена" };
        }

        // Проверяем, можно ли переодеваться (нет посторонних)
        if (!this.canSwitchOutfits(gameState)) {
            return { 
                success: false, 
                message: "Нельзя переодеваться при посторонних!" 
            };
        }

        // Проверяем, нет ли уже активного запроса
        if (this.outfitRequests.has(roomId)) {
            return { 
                success: false, 
                message: "Уже есть активный запрос на обмен одеждой" 
            };
        }

        const targetCharacter = fromCharacter === 'princess' ? 'helper' : 'princess';
        const targetPlayer = gameState.players[targetCharacter];

        if (!targetPlayer) {
            return { success: false, message: "Второй игрок не найден" };
        }

        // Создаем запрос
        const request = {
            id: this.generateRequestId(),
            roomId: roomId,
            fromPlayerId: fromPlayerId,
            fromCharacter: fromCharacter,
            targetPlayerId: targetPlayer.id,
            targetCharacter: targetCharacter,
            timestamp: Date.now()
        };

        this.outfitRequests.set(roomId, request);

        return { 
            success: true, 
            request: request,
            message: `${this.getCharacterName(fromCharacter)} предлагает поменяться одеждой`
        };
    }

    // Ответить на запрос обмена одеждой
    respondToOutfitSwapRequest(roomId, playerId, accepted) {
        const request = this.outfitRequests.get(roomId);
        if (!request) {
            return { success: false, message: "Запрос не найден" };
        }

        if (request.targetPlayerId !== playerId) {
            return { success: false, message: "Этот запрос не для вас" };
        }

        // Удаляем запрос
        this.outfitRequests.delete(roomId);

        if (!accepted) {
            return { 
                success: true, 
                declined: true,
                message: `${this.getCharacterName(request.targetCharacter)} отклонила предложение обмена одеждой`
            };
        }

        // Выполняем обмен одеждой
        const gameState = this.games.get(roomId);
        if (!gameState) {
            return { success: false, message: "Игра не найдена" };
        }

        if (!this.canSwitchOutfits(gameState)) {
            return { 
                success: false, 
                message: "Обстановка изменилась - больше нельзя переодеваться!" 
            };
        }

        // Меняем наряды местами
        const { princess, helper } = gameState.stats;
        const tempOutfit = princess.outfit;
        princess.outfit = helper.outfit;
        helper.outfit = tempOutfit;

        console.log('👗 Обмен выполнен:', {
            princessNew: princess.outfit,
            helperNew: helper.outfit
        });

        return {
            success: true,
            accepted: true,
            message: `Персонажи поменялись одеждой! Княжна теперь в: ${this.getOutfitName(princess.outfit)}, помощница в: ${this.getOutfitName(helper.outfit)}`
        };
    }

    // Проверить, можно ли переодеваться
    canSwitchOutfits(gameState) {
        // Можно переодеваться только когда персонажи наедине
        return !gameState.npcsPresent || gameState.npcsPresent.length === 0;
    }

    // Получить выборы для персонажа
    getChoicesForCharacter(gameState, character, sceneData) {
        let choices = [];
        
        // Основные выборы сцены (только для игрока, чей ход)
        if (gameState.turnOrder === character) {
            choices = sceneData.choices[character] || [];
            choices = choices.filter(choice => {
                return this.isChoiceAvailable(choice, gameState, character);
            });
        }

        // КНОПКА ПРЕДЛОЖЕНИЯ обмена одеждой доступна для ОБОИХ персонажей
        // если они наедине и нет активного запроса
        if (this.canSwitchOutfits(gameState) && !this.outfitRequests.has(gameState.roomId)) {
            const otherCharacter = character === 'princess' ? 'помощнице' : 'княжне';
            choices.push({
                id: 'request_outfit_swap',
                text: '👗 Предложить поменяться одеждой',
                description: `Предложить ${otherCharacter} поменяться нарядами`,
                isOutfitRequest: true
            });
        }

        return choices;
    }

    // Обработка обычных выборов (НЕ запросов одежды)
    makeChoice(roomId, playerId, choiceId, character) {
        const gameState = this.games.get(roomId);
        if (!gameState) {
            return { success: false, message: "Игра не найдена" };
        }

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
        // Запросы одежды обрабатываются отдельно
        if (choiceId === 'request_outfit_swap') {
            return { 
                success: false, 
                message: "Используйте отдельный обработчик для запросов обмена одеждой" 
            };
        }

        // Обработка обычных выборов
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
            
            // При смене сцены отменяем активные запросы
            this.cancelOutfitRequest(gameState.roomId);
            
            // При смене сцены обновляем локацию если она указана
            const newSceneData = CoopStoryData.getScene(choice.nextScene);
            if (newSceneData.location) {
                gameState.location = newSceneData.location;
                gameState.npcsPresent = this.getNPCsForLocation(newSceneData.location);
            }
        }

        // Меняем очередь хода
        this.switchTurn(gameState);

        return { 
            success: true, 
            message: choice.resultText || "Выбор сделан"
        };
    }

    // Получить активный запрос для комнаты
    getActiveOutfitRequest(roomId) {
        return this.outfitRequests.get(roomId) || null;
    }

    // Отменить запрос 
    cancelOutfitRequest(roomId) {
        this.outfitRequests.delete(roomId);
    }

    // Применить эффекты выбора
    applyEffects(gameState, effects, character) {
        if (effects.outfit) {
            gameState.stats[character].outfit = effects.outfit;
        }
        if (effects.location) {
            gameState.location = effects.location;
            gameState.npcsPresent = this.getNPCsForLocation(effects.location);
        }
        if (effects.awareness) {
            gameState.stats[character].awareness += effects.awareness;
        }
    }

    // Проверить доступность выбора
    isChoiceAvailable(choice, gameState, character) {
        // Базовая проверка - можно расширить
        return true;
    }

    // Сменить очередь хода
    switchTurn(gameState) {
        gameState.turnOrder = gameState.turnOrder === 'princess' ? 'helper' : 'princess';
    }

    // Получить данные игры
    getGameData(roomId) {
        const gameState = this.games.get(roomId);
        if (!gameState) return null;

        const sceneData = CoopStoryData.getScene(gameState.currentScene);
        
        const gameData = {
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
            stats: JSON.parse(JSON.stringify(gameState.stats)), // Глубокая копия
            currentTurn: gameState.turnOrder,
            chapter: gameState.chapter,
            location: gameState.location,
            npcsPresent: gameState.npcsPresent,
            activeOutfitRequest: this.getActiveOutfitRequest(roomId) // Информация о запросе
        };

        return gameData;
    }

    // Вспомогательные методы
    generateRequestId() {
        return Math.random().toString(36).substring(2, 15);
    }

    getCharacterName(character) {
        return character === 'princess' ? 'Княжна' : 'Помощница';
    }

    getOutfitName(outfitId) {
        const outfitNames = {
            'nightgown': 'Ночная рубашка',
            'princess_dress': 'Княжеское платье',
            'common_dress': 'Простое платье',
            'court_dress': 'Придворное платье'
        };
        return outfitNames[outfitId] || outfitId;
    }

    getNPCsForLocation(location) {
        const npcsByLocation = {
            'princess_chamber': [], // Никого нет - можно переодеваться
            'throne_room': ['Король', 'Королева', 'Стражники'],
            'kitchen': ['Повар', 'Слуги'],
            'garden': [],
            'armory': ['Оружейник']
        };
        return npcsByLocation[location] || [];
    }

    removeGame(roomId) {
        this.games.delete(roomId);
        this.outfitRequests.delete(roomId);
    }
}

module.exports = CoopGameLogic;
