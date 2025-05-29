const CoopStoryData = require('./coopStoryData');
const LocationData = require('./locationData');
const NPCData = require('./npcData');

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
            // Убираем общую локацию, теперь у каждого своя
            stats: {
                princess: {
                    outfit: 'princess_dress', // Княжна начинает в княжеском платье
                    awareness: 0,
                    loyalty: {},
                    inventory: [],
                    location: 'princess_chamber', // Индивидуальная локация
                    npcsPresent: [] // Индивидуальные NPC
                },
                helper: {
                    outfit: 'common_dress',
                    awareness: 0,
                    secrets_revealed: 0,
                    inventory: ['translation_earrings', 'voice_medallion'],
                    location: 'princess_chamber', // Начинают вместе
                    npcsPresent: [] // Индивидуальные NPC
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
        if (!this.canSwitchOutfits(gameState, fromCharacter)) {
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

        if (!this.canSwitchOutfits(gameState, request.fromCharacter)) {
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


        return {
            success: true,
            accepted: true,
            message: `Персонажи поменялись одеждой! Княжна теперь в: ${this.getOutfitName(princess.outfit)}, помощница в: ${this.getOutfitName(helper.outfit)}`
        };
    }

    // Проверить, можно ли переодеваться
    canSwitchOutfits(gameState, character) {
        const characterStats = gameState.stats[character];
        
        // Проверяем, что нет NPC у этого персонажа
        const noNpcs = !characterStats.npcsPresent || characterStats.npcsPresent.length === 0;
        
        // Проверяем, что локация позволяет переодеваться
        const locationAllows = LocationData.canChangeOutfit(characterStats.location);
        
        // Проверяем, что оба персонажа в одной локации (для обмена)
        const otherCharacter = character === 'princess' ? 'helper' : 'princess';
        const sameLocation = characterStats.location === gameState.stats[otherCharacter].location;
        
        // Проверяем, что у второго персонажа тоже нет NPC
        const otherHasNoNpcs = !gameState.stats[otherCharacter].npcsPresent || 
                              gameState.stats[otherCharacter].npcsPresent.length === 0;
        
        return noNpcs && locationAllows && sameLocation && otherHasNoNpcs;
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
        if (this.canSwitchOutfits(gameState, character) && !this.outfitRequests.has(gameState.roomId)) {
            const otherCharacter = character === 'princess' ? 'помощнице' : 'княжне';
            choices.push({
                id: 'request_outfit_swap',
                text: '👗 Предложить поменяться одеждой',
                description: `Предложить ${otherCharacter} поменяться нарядами`,
                isOutfitRequest: true
            });
        }

        // Добавляем выборы перемещения для ОБОИХ игроков (индивидуальные)
        const movementChoices = this.getMovementChoices(gameState, character);
        choices.push(...movementChoices);

        // Добавляем выборы взаимодействия с NPC (только для игрока, чей ход)
        if (gameState.turnOrder === character) {
            const npcChoices = this.getNPCInteractionChoices(gameState, character);
            choices.push(...npcChoices);
        }

        return choices;
    }

    // Обработка обычных выборов (НЕ запросов одежды)
    makeChoice(roomId, playerId, choiceId, character) {
        const gameState = this.games.get(roomId);
        if (!gameState) {
            return { success: false, message: "Игра не найдена" };
        }

        // Проверяем, что игрок управляет правильным персонажем
        const playerCharacter = gameState.players[character];
        if (!playerCharacter || playerCharacter.id !== playerId) {
            return { success: false, message: "Вы управляете другим персонажем" };
        }

        // Проверяем, что сейчас ход этого персонажа (для обычных выборов, но не для движения)
        const isMovement = choiceId.startsWith('move_to_');
        if (!isMovement && choiceId !== 'request_outfit_swap' && gameState.turnOrder !== character) {
            return { success: false, message: "Сейчас не ваш ход" };
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

        // Проверка на перемещение
        if (choiceId.startsWith('move_to_')) {
            const targetLocation = choiceId.replace('move_to_', '');
            return this.processMovement(gameState, targetLocation, character);
        }

        // Проверка на взаимодействие с NPC
        if (choiceId.startsWith('talk_to_')) {
            const npcId = choiceId.replace('talk_to_', '');
            return this.processNPCInteraction(gameState, npcId, character);
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
            // Больше не нужно, так как локации индивидуальные
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
            gameState.stats[character].location = effects.location;
            gameState.stats[character].npcsPresent = this.getNPCsForLocation(effects.location);
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
        
        // Теперь локации индивидуальные для каждого персонажа
        const princessLocationInfo = LocationData.getLocationInfo(gameState.stats.princess.location);
        const helperLocationInfo = LocationData.getLocationInfo(gameState.stats.helper.location);
        
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
            // Индивидуальная информация о локациях
            locations: {
                princess: princessLocationInfo,
                helper: helperLocationInfo
            },
            activeOutfitRequest: this.getActiveOutfitRequest(roomId), // Информация о запросе
            currentNPCDialogue: gameState.currentNPCDialogue || null // Информация о текущем диалоге с NPC
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
        // Получаем NPC из NPCData
        const npcs = NPCData.getNPCsForLocation(location);
        // Возвращаем только имена для обратной совместимости
        return npcs.map(npc => npc.name);
    }

    getMovementChoices(gameState, character) {
        const currentLocation = gameState.stats[character].location;
        const locationInfo = LocationData.getLocationInfo(currentLocation);
        
        if (!locationInfo) return [];
        
        const choices = [];
        
        // Добавляем кнопки для перехода в соседние локации
        locationInfo.connections.forEach(connection => {
            choices.push({
                id: `move_to_${connection.id}`,
                text: `${connection.icon} Перейти: ${connection.name}`,
                description: `Отправиться в ${connection.name}`,
                isMovement: true,
                targetLocation: connection.id
            });
        });
        
        return choices;
    }

    processMovement(gameState, targetLocation, character) {
        const characterStats = gameState.stats[character];
        
        // Проверяем, что целевая локация доступна из текущей
        const currentConnections = LocationData.getConnections(characterStats.location);
        if (!currentConnections.includes(targetLocation)) {
            return { 
                success: false, 
                message: "Вы не можете попасть туда отсюда" 
            };
        }

        // Проверяем, что локация существует
        const locationInfo = LocationData.getLocation(targetLocation);
        if (!locationInfo) {
            return { 
                success: false, 
                message: "Неизвестная локация" 
            };
        }

        // Отменяем активные запросы при перемещении любого персонажа
        if (this.outfitRequests.has(gameState.roomId)) {
            this.cancelOutfitRequest(gameState.roomId);
        }

        // Перемещаем конкретного персонажа
        characterStats.location = targetLocation;
        
        // Обновляем NPC для новой локации этого персонажа
        characterStats.npcsPresent = this.getNPCsForLocation(targetLocation);

        // НЕ меняем очередь хода при перемещении
        // Это позволяет игрокам свободно перемещаться

        return { 
            success: true, 
            message: `${character === 'princess' ? 'Княжна' : 'Помощница'} переместилась в ${locationInfo.name}`
        };
    }

    removeGame(roomId) {
        this.games.delete(roomId);
        this.outfitRequests.delete(roomId);
    }

    // Получить выборы взаимодействия с NPC
    getNPCInteractionChoices(gameState, character) {
        const choices = [];
        const characterLocation = gameState.stats[character].location;
        const npcs = NPCData.getNPCsForLocation(characterLocation);
        
        npcs.forEach(npc => {
            choices.push({
                id: `talk_to_${npc.id}`,
                text: `💬 Поговорить с ${npc.name}`,
                description: npc.description,
                isNPCInteraction: true,
                npcId: npc.id
            });
        });
        
        return choices;
    }

    // Обработка взаимодействия с NPC
    processNPCInteraction(gameState, npcId, character) {
        const npc = NPCData.getNPC(npcId);
        if (!npc) {
            return { success: false, message: "NPC не найден" };
        }

        // Получаем наряд персонажа
        const outfit = gameState.stats[character].outfit;
        
        // Получаем диалог в зависимости от наряда
        const dialogue = NPCData.getNPCDialogue(npcId, outfit);
        if (!dialogue) {
            return { success: false, message: "Диалог не найден" };
        }

        // Сохраняем информацию о текущем диалоге
        gameState.currentNPCDialogue = {
            npcId: npcId,
            npcName: npc.name,
            greeting: dialogue.greeting,
            choices: dialogue.choices,
            attitude: NPCData.getNPCAttitude(npcId, outfit),
            activeCharacter: character // Кто ведет диалог
        };

        return { 
            success: true, 
            showDialogue: true,
            message: `Начат диалог с ${npc.name}`
        };
    }

    // Обработка выбора в диалоге с NPC
    processNPCDialogueChoice(roomId, playerId, choiceId, character) {
        const gameState = this.games.get(roomId);
        if (!gameState) {
            return { success: false, message: "Игра не найдена" };
        }

        // Проверяем, что игрок управляет правильным персонажем
        const playerCharacter = gameState.players[character];
        if (!playerCharacter || playerCharacter.id !== playerId) {
            return { success: false, message: "Вы управляете другим персонажем" };
        }

        // Проверяем, что есть активный диалог
        if (!gameState.currentNPCDialogue) {
            return { success: false, message: "Нет активного диалога" };
        }

        // Находим выбранный вариант
        const choice = gameState.currentNPCDialogue.choices.find(c => c.id === choiceId);
        if (!choice) {
            return { success: false, message: "Неверный выбор" };
        }

        // Применяем эффекты выбора
        if (choice.effects) {
            if (choice.effects.item) {
                gameState.stats[character].inventory.push(choice.effects.item);
            }
            if (choice.effects.info) {
                gameState.stats[character][choice.effects.info] = true;
            }
        }

        // Сохраняем attitude до очистки диалога
        const attitude = gameState.currentNPCDialogue?.attitude;

        // Очищаем текущий диалог
        gameState.currentNPCDialogue = null;

        // Меняем очередь хода
        this.switchTurn(gameState);

        return { 
            success: true, 
            message: choice.response,
            type: attitude === 'hostile' ? 'warning' : 'success'
        };
    }

    // Закрытие диалога с NPC
    closeNPCDialogue(roomId, playerId) {
        const gameState = this.games.get(roomId);
        if (!gameState) {
            return { success: false, message: "Игра не найдена" };
        }

        // Проверяем, что есть активный диалог и игрок может его закрыть
        if (!gameState.currentNPCDialogue) {
            return { success: false, message: "Нет активного диалога" };
        }

        // Проверяем, что игрок может закрыть диалог (тот, кто его начал)
        const activeCharacter = gameState.currentNPCDialogue.activeCharacter;
        const playerCharacter = gameState.players[activeCharacter];
        
        if (!playerCharacter || playerCharacter.id !== playerId) {
            return { success: false, message: "Вы не можете закрыть этот диалог" };
        }

        // Закрываем диалог
        gameState.currentNPCDialogue = null;

        return { 
            success: true, 
            message: "Диалог закрыт"
        };
    }
}

module.exports = CoopGameLogic;
