const CoopStoryData = require('./coopStoryData');

class CoopGameLogic {
    constructor() {
        this.games = new Map(); // roomId -> gameState
        this.outfitRequests = new Map(); // roomId -> activeRequest
    }

    // Создать запрос на обмен одеждой
    createOutfitSwapRequest(roomId, fromPlayerId, fromCharacter) {
        const gameState = this.games.get(roomId);
        if (!gameState) {
            return { success: false, message: "Игра не найдена" };
        }

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

        // Меняем наряды
        const { princess, helper } = gameState.stats;
        [princess.outfit, helper.outfit] = [helper.outfit, princess.outfit];

        return {
            success: true,
            accepted: true,
            message: `Персонажи поменялись одеждой! Княжна теперь в: ${this.getOutfitName(princess.outfit)}, помощница в: ${this.getOutfitName(helper.outfit)}`
        };
    }

    // Получить активный запрос для комнаты
    getActiveOutfitRequest(roomId) {
        return this.outfitRequests.get(roomId) || null;
    }

    // Отменить запрос (при выходе игрока или смене сцены)
    cancelOutfitRequest(roomId) {
        this.outfitRequests.delete(roomId);
    }

    generateRequestId() {
        return Math.random().toString(36).substring(2, 15);
    }

    getCharacterName(character) {
        return character === 'princess' ? 'Княжна' : 'Помощница';
    }

    // Обновить getChoicesForCharacter для новой логики
    getChoicesForCharacter(gameState, character, sceneData) {
        let choices = [];
        
        // Основные выборы сцены (только для игрока, чей ход)
        if (gameState.turnOrder === character) {
            choices = sceneData.choices[character] || [];
            choices = choices.filter(choice => {
                return this.isChoiceAvailable(choice, gameState, character);
            });
        }

        // Предложение обмена одеждой доступно ВСЕГДА, если персонажи наедине
        // И если нет активного запроса
        if (this.canSwitchOutfits(gameState) && !this.outfitRequests.has(gameState.roomId)) {
            choices.push({
                id: 'request_outfit_swap',
                text: '👗 Предложить обмен одеждой',
                description: `Предложить поменяться нарядами`,
                isOutfitRequest: true
            });
        }

        return choices;
    }

    // Также нужно обновить processChoice для обработки запросов
    processChoice(gameState, choiceId, character) {
        // Убираем старую логику switch_outfits, заменяем на request_outfit_swap
        if (choiceId === 'request_outfit_swap') {
            // Эта логика теперь обрабатывается отдельно через createOutfitSwapRequest
            return { 
                success: false, 
                message: "Используйте отдельный обработчик для запросов обмена одеждой" 
            };
        }

        // Остальная логика остается без изменений...
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

    // Обновить getGameData для включения информации о запросах
    getGameData(roomId) {
        const gameState = this.games.get(roomId);
        if (!gameState) return null;

        const sceneData = CoopStoryData.getScene(gameState.currentScene);
        
        // Создаем глубокую копию stats
        const deepCopyStats = JSON.parse(JSON.stringify(gameState.stats));
        
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
            stats: deepCopyStats,
            currentTurn: gameState.turnOrder,
            chapter: gameState.chapter,
            location: gameState.location,
            npcsPresent: gameState.npcsPresent,
            activeOutfitRequest: this.getActiveOutfitRequest(roomId) // Добавляем информацию о запросе
        };

        return gameData;
    }
}

module.exports = CoopGameLogic;
