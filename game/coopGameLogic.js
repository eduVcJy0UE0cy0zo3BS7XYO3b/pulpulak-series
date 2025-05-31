const CoopStoryData = require('./coopStoryData');
const LocationData = require('./locationData');
const NPCData = require('./npcData');
const QuestData = require('./questData');
const { OUTFIT_NAMES, CHARACTER_NAMES, CHARACTER_ROLES } = require('./constants');
const { processQuestAction } = require('./questActionHandlers');
const GameStateManager = require('./gameStateManager');
const ImmerStateManager = require('./stateManager');

class CoopGameLogic {
    constructor() {
        this.games = new Map(); // roomId -> gameState
        this.outfitRequests = new Map(); // roomId -> activeRequest
        this.stateManager = new GameStateManager();
        this.immerStateManager = new ImmerStateManager();
    }

    // Запуск игры
    startGame(roomId, players) {
        try {
            if (!roomId || typeof roomId !== 'string') {
                throw new Error('Неверный ID комнаты');
            }
            if (!players || !players.princess || !players.helper) {
                throw new Error('Недостаточно игроков для начала игры');
            }

            const gameState = this.stateManager.createInitialState(roomId, players);

            this.games.set(roomId, gameState);
            
            // Инициализируем NPC для начальных локаций
            try {
                gameState.stats.princess.npcsPresent = this.getNPCsForLocation(gameState.stats.princess.location, gameState, 'princess');
                gameState.stats.helper.npcsPresent = this.getNPCsForLocation(gameState.stats.helper.location, gameState, 'helper');
            } catch (npcError) {
                console.error('Ошибка при инициализации NPC:', npcError);
                // Продолжаем с пустыми массивами NPC
                gameState.stats.princess.npcsPresent = [];
                gameState.stats.helper.npcsPresent = [];
            }
            
            return this.getGameData(roomId);
        } catch (error) {
            console.error('Ошибка при запуске игры:', error);
            throw new Error(`Не удалось запустить игру: ${error.message}`);
        }
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

        // Меняем наряды местами с помощью Immer
        const updatedGameState = this.immerStateManager.updateState(gameState, draft => {
            const tempOutfit = draft.stats.princess.outfit;
            draft.stats.princess.outfit = draft.stats.helper.outfit;
            draft.stats.helper.outfit = tempOutfit;
        });
        
        // Обновляем состояние игры
        this.games.set(roomId, updatedGameState);


        return {
            success: true,
            accepted: true,
            message: `Персонажи поменялись одеждой! Княжна теперь в: ${this.getOutfitName(updatedGameState.stats.princess.outfit)}, помощница в: ${this.getOutfitName(updatedGameState.stats.helper.outfit)}`
        };
    }

    // Проверить, можно ли переодеваться
    canSwitchOutfits(gameState, character) {
        const validators = [
            () => this.hasNoNPCs(gameState, character),
            () => this.locationAllowsOutfitChange(gameState, character),
            () => this.playersInSameLocation(gameState, character),
            () => this.bothPlayersHaveNoNPCs(gameState, character)
        ];
        
        return validators.every(validate => validate());
    }

    hasNoNPCs(gameState, character) {
        const characterStats = gameState.stats[character];
        return !characterStats.npcsPresent || characterStats.npcsPresent.length === 0;
    }

    locationAllowsOutfitChange(gameState, character) {
        const characterStats = gameState.stats[character];
        return LocationData.canChangeOutfit(characterStats.location);
    }

    playersInSameLocation(gameState, character) {
        const characterStats = gameState.stats[character];
        const otherCharacter = character === 'princess' ? 'helper' : 'princess';
        return characterStats.location === gameState.stats[otherCharacter].location;
    }

    bothPlayersHaveNoNPCs(gameState, character) {
        const otherCharacter = character === 'princess' ? 'helper' : 'princess';
        return this.hasNoNPCs(gameState, otherCharacter);
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

        // Добавляем выборы взаимодействия с NPC (доступно всегда)
        const npcChoices = this.getNPCInteractionChoices(gameState, character);
        choices.push(...npcChoices);

        return choices;
    }

    // Обработка обычных выборов (НЕ запросов одежды)
    makeChoice(roomId, playerId, choiceId, character) {
        try {
            const gameState = this.games.get(roomId);
            if (!gameState) {
                return { success: false, message: "Игра не найдена" };
            }

        // Проверяем, что игрок управляет правильным персонажем
        const playerCharacter = gameState.players[character];
        if (!playerCharacter || playerCharacter.id !== playerId) {
            return { success: false, message: "Вы управляете другим персонажем" };
        }

        // Проверяем, что сейчас ход этого персонажа (для обычных выборов, но не для движения и NPC)
        const isMovement = choiceId.startsWith('move_to_');
        const isNPCInteraction = choiceId.startsWith('talk_to_');
        if (!isMovement && !isNPCInteraction && choiceId !== 'request_outfit_swap' && gameState.turnOrder !== character) {
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
        } catch (error) {
            console.error('Ошибка при обработке выбора:', error);
            return { 
                success: false, 
                message: `Ошибка при выполнении действия: ${error.message}` 
            };
        }
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
            gameState = this.applyEffects(gameState, choice.effects, character);
            this.games.set(gameState.roomId, gameState);
        }

        // Проверяем, меняется ли сцена
        if (choice.nextScene) {
            gameState = this.immerStateManager.updateState(gameState, draft => {
                draft.currentScene = choice.nextScene;
            });
            this.games.set(gameState.roomId, gameState);
            
            // При смене сцены отменяем активные запросы
            this.cancelOutfitRequest(gameState.roomId);
        }

        // Меняем очередь хода
        gameState = this.switchTurn(gameState);
        this.games.set(gameState.roomId, gameState);

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
        return this.immerStateManager.updateState(gameState, draft => {
            if (effects.outfit) {
                draft.stats[character].outfit = effects.outfit;
            }
            if (effects.location) {
                draft.stats[character].location = effects.location;
                draft.stats[character].npcsPresent = this.getNPCsForLocation(effects.location, gameState, character);
            }
            if (effects.awareness) {
                draft.stats[character].awareness += effects.awareness;
            }
        });
    }

    // Проверить доступность выбора
    isChoiceAvailable(choice, gameState, character) {
        // Базовая проверка - можно расширить
        return true;
    }

    // Сменить очередь хода
    switchTurn(gameState) {
        return this.immerStateManager.updateState(gameState, draft => {
            draft.turnOrder = draft.turnOrder === 'princess' ? 'helper' : 'princess';
        });
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
            // Индивидуальные диалоги для каждого персонажа
            npcDialogues: {
                princess: gameState.npcDialogues?.princess || null,
                helper: gameState.npcDialogues?.helper || null
            },
            // Информация о квестах
            quests: {
                princess: {
                    active: gameState.quests.princess.active,
                    completed: gameState.quests.princess.completed.length
                },
                helper: {
                    active: gameState.quests.helper.active,
                    completed: gameState.quests.helper.completed.length
                }
            }
        };

        return gameData;
    }

    // Вспомогательные методы
    generateRequestId() {
        return Math.random().toString(36).substring(2, 15);
    }

    getCharacterName(character) {
        return CHARACTER_NAMES[character] || character;
    }

    getOutfitName(outfitId) {
        return OUTFIT_NAMES[outfitId] || outfitId;
    }

    getNPCsForLocation(location, gameState = null, character = null) {
        // Получаем NPC из NPCData с учётом состояния игры
        const npcs = NPCData.getNPCsForLocation(location, gameState, character);
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

        // Перемещаем конкретного персонажа с помощью Immer
        const updatedGameState = this.immerStateManager.updateState(gameState, draft => {
            draft.stats[character].location = targetLocation;
            draft.stats[character].npcsPresent = this.getNPCsForLocation(targetLocation, gameState, character);
        });
        this.games.set(updatedGameState.roomId, updatedGameState);

        // НЕ меняем очередь хода при перемещении
        // Это позволяет игрокам свободно перемещаться

        return { 
            success: true, 
            message: `${character === 'princess' ? 'Княжна' : 'Помощница'} переместилась в ${locationInfo.name}`,
            gameState: updatedGameState
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
        const npcs = NPCData.getNPCsForLocation(characterLocation, gameState, character);
        
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
        try {
            const npc = NPCData.getNPC(npcId);
            if (!npc) {
                return { success: false, message: "NPC не найден" };
            }

        // Получаем наряд персонажа
        const outfit = gameState.stats[character].outfit;
        
        // Получаем память NPC и создаем диалог через Immer
        let updatedGameState = gameState;
        if (!gameState.npcMemory[character][npcId]) {
            updatedGameState = this.immerStateManager.updateState(gameState, draft => {
                draft.npcMemory[character][npcId] = {};
            });
            this.games.set(updatedGameState.roomId, updatedGameState);
        }
        const npcMemory = updatedGameState.npcMemory[character][npcId];
        
        // Получаем диалог в зависимости от наряда, памяти, локации и состояния квеста
        const currentLocation = updatedGameState.stats[character].location;
        const questState = updatedGameState.quests[character];
        const globalQuestMemory = updatedGameState.globalQuestMemory;
        const dialogue = NPCData.getNPCDialogue(npcId, outfit, npcMemory, currentLocation, questState, globalQuestMemory);
        if (!dialogue) {
            return { success: false, message: "Диалог не найден" };
        }

        // Сохраняем информацию о диалоге для конкретного персонажа
        updatedGameState = this.immerStateManager.updateState(updatedGameState, draft => {
            draft.npcDialogues[character] = {
                npcId: npcId,
                npcName: npc.name,
                greeting: dialogue.greeting,
                choices: dialogue.choices,
                attitude: NPCData.getNPCAttitude(npcId, outfit),
                activeCharacter: character, // Кто ведет диалог
                isFollowUp: false // Флаг для дополнительных выборов
            };
        });
        this.games.set(updatedGameState.roomId, updatedGameState);

        return { 
            success: true, 
            showDialogue: true,
            message: `Начат диалог с ${npc.name}`
        };
        } catch (error) {
            console.error('Ошибка при взаимодействии с NPC:', error);
            return { 
                success: false, 
                message: `Не удалось начать диалог: ${error.message}` 
            };
        }
    }

    // Обработка выбора в диалоге с NPC
    processNPCDialogueChoice(roomId, playerId, choiceId, character) {
        let gameState = this.games.get(roomId);
        if (!gameState) {
            return { success: false, message: "Игра не найдена" };
        }

        // Проверяем, что игрок управляет правильным персонажем
        const playerCharacter = gameState.players[character];
        if (!playerCharacter || playerCharacter.id !== playerId) {
            return { success: false, message: "Вы управляете другим персонажем" };
        }

        // Проверяем, что есть активный диалог для данного персонажа
        if (!gameState.npcDialogues[character]) {
            return { success: false, message: "Нет активного диалога" };
        }

        const npcId = gameState.npcDialogues[character].npcId;
        const outfit = gameState.stats[character].outfit;

        // Получаем память NPC для этого персонажа
        if (!gameState.npcMemory[character][npcId]) {
            gameState.npcMemory[character][npcId] = {};
        }

        // Обрабатываем выбор через NPCData
        const isFollowUp = gameState.npcDialogues[character].isFollowUp || false;
        const currentChoices = isFollowUp ? gameState.npcDialogues[character].choices : [];
        
        // Создаем мутабельную копию памяти NPC для NPCData
        const npcMemoryCopy = JSON.parse(JSON.stringify(gameState.npcMemory[character][npcId]));
        
        const result = NPCData.processDialogueChoice(
            npcId, 
            choiceId, 
            outfit, 
            npcMemoryCopy,
            isFollowUp,
            currentChoices,
            gameState.stats[character].location
        );
        if (!result) {
            return { success: false, message: "Неверный выбор" };
        }

        // Обновляем память NPC и применяем эффекты
        gameState = this.immerStateManager.updateState(gameState, draft => {
            draft.npcMemory[character][npcId] = result.updatedMemory;
            
            // Применяем эффекты выбора
            if (result.effects) {
                if (result.effects.item) {
                    draft.stats[character].inventory.push(result.effects.item);
                }
                if (result.effects.info) {
                    draft.stats[character][result.effects.info] = true;
                }
            }
        });
        this.games.set(gameState.roomId, gameState);

        // Обрабатываем квестовые действия
        const questResult = this.processQuestAction(gameState, character, choiceId, result);
        if (questResult && questResult.success && questResult.gameState) {
            gameState = questResult.gameState;
            this.games.set(gameState.roomId, gameState);
        }

        // Обновляем NPC в локациях после квестовых действий (NPC могли переместиться)
        gameState = this.immerStateManager.updateState(gameState, draft => {
            draft.stats.princess.npcsPresent = this.getNPCsForLocation(draft.stats.princess.location, gameState, 'princess');
            draft.stats.helper.npcsPresent = this.getNPCsForLocation(draft.stats.helper.location, gameState, 'helper');
        });
        this.games.set(gameState.roomId, gameState);

        // Сохраняем attitude до очистки диалога
        const attitude = gameState.npcDialogues[character]?.attitude;

        // Если есть дополнительные выборы, показываем их
        if (result.next_choices && result.next_choices.length > 0) {
            gameState = this.immerStateManager.updateState(gameState, draft => {
                draft.npcDialogues[character].choices = result.next_choices;
                draft.npcDialogues[character].greeting = result.response;
                draft.npcDialogues[character].isFollowUp = true;
            });
            this.games.set(gameState.roomId, gameState);

            return { 
                success: true, 
                message: result.response,
                type: attitude === 'hostile' ? 'warning' : 'success',
                hasFollowUp: true
            };
        } else {
            // Очищаем диалог для данного персонажа и меняем ход
            gameState = this.immerStateManager.updateState(gameState, draft => {
                draft.npcDialogues[character] = null;
            });
            gameState = this.switchTurn(gameState);
            this.games.set(gameState.roomId, gameState);

            return { 
                success: true, 
                message: result.response,
                type: attitude === 'hostile' ? 'warning' : 'success'
            };
        }
    }

    // Закрытие диалога с NPC
    closeNPCDialogue(roomId, playerId) {
        let gameState = this.games.get(roomId);
        if (!gameState) {
            return { success: false, message: "Игра не найдена" };
        }

        // Находим персонажа, который принадлежит данному игроку
        let character = null;
        for (const [char, player] of Object.entries(gameState.players)) {
            if (player && player.id === playerId) {
                character = char;
                break;
            }
        }

        if (!character) {
            return { success: false, message: "Игрок не найден" };
        }

        // Проверяем, что есть активный диалог для данного персонажа
        if (!gameState.npcDialogues[character]) {
            return { success: false, message: "Нет активного диалога" };
        }

        // Закрываем диалог для данного персонажа
        gameState = this.immerStateManager.updateState(gameState, draft => {
            draft.npcDialogues[character] = null;
        });
        this.games.set(gameState.roomId, gameState);

        return { 
            success: true, 
            message: "Диалог закрыт",
            gameState: gameState
        };
    }

    // === СИСТЕМА КВЕСТОВ ===

    // Начать квест
    startQuest(gameState, character, questId) {
        const quest = QuestData.createQuestInstance(questId);
        if (!quest) {
            return { success: false, message: "Квест не найден" };
        }

        if (gameState.quests[character].active) {
            return { success: false, message: "У вас уже есть активный квест" };
        }

        const updatedState = this.immerStateManager.updateState(gameState, draft => {
            draft.quests[character].active = quest;
            
            // Сразу отмечаем квест как взятый в глобальной памяти
            if (questId === 'princess_lost_relic') {
                draft.globalQuestMemory.princess_lost_relic = true;
            } else if (questId === 'helper_secret_potion') {
                draft.globalQuestMemory.helper_secret_potion = true;
            }
        });
        this.games.set(updatedState.roomId, updatedState);
        
        return { 
            success: true, 
            message: `Начат квест: ${quest.title}`,
            quest: quest,
            gameState: updatedState
        };
    }

    // Обновить прогресс квеста
    updateQuestProgress(gameState, character, stepId) {
        const activeQuest = gameState.quests[character].active;
        if (!activeQuest) {
            return { success: false, message: "Нет активного квеста" };
        }

        const currentStep = activeQuest.steps[activeQuest.currentStep];
        if (currentStep && currentStep.id === stepId) {
            const updatedState = this.immerStateManager.updateState(gameState, draft => {
                const draftQuest = draft.quests[character].active;
                draftQuest.steps[draftQuest.currentStep].completed = true;
                draftQuest.currentStep++;
            });
            this.games.set(updatedState.roomId, updatedState);

            if (updatedState.quests[character].active.currentStep >= updatedState.quests[character].active.steps.length) {
                // Квест завершён
                const completedState = this.completeQuest(updatedState, character);
                return { 
                    success: true, 
                    completed: true,
                    message: `Квест завершён: ${activeQuest.title}!`,
                    rewards: activeQuest.rewards,
                    gameState: completedState
                };
            } else {
                return { 
                    success: true, 
                    message: `Шаг квеста выполнен: ${currentStep.description}`,
                    nextStep: updatedState.quests[character].active.steps[updatedState.quests[character].active.currentStep],
                    gameState: updatedState
                };
            }
        }

        return { success: false, message: "Неверный шаг квеста" };
    }

    // Завершить квест
    completeQuest(gameState, character) {
        const activeQuest = gameState.quests[character].active;
        if (activeQuest) {
            const updatedState = this.immerStateManager.updateState(gameState, draft => {
                // Добавляем награды в инвентарь
                if (activeQuest.rewards) {
                    activeQuest.rewards.forEach(reward => {
                        draft.stats[character].inventory.push(reward);
                    });
                }

                // Перемещаем квест в завершённые
                draft.quests[character].completed.push(activeQuest);
                draft.quests[character].active = null;
                
                // Обновляем глобальную память квестов
                if (activeQuest.id === 'princess_lost_relic') {
                    draft.globalQuestMemory.princess_lost_relic = true;
                } else if (activeQuest.id === 'helper_secret_potion') {
                    draft.globalQuestMemory.helper_secret_potion = true;
                }
            });
            this.games.set(updatedState.roomId, updatedState);
            return updatedState;
        }
        return gameState;
    }

    // Получить текущий квест персонажа
    getCurrentQuest(gameState, character) {
        return gameState.quests[character].active;
    }

    // Получить текущий шаг квеста
    getCurrentQuestStep(gameState, character) {
        const quest = this.getCurrentQuest(gameState, character);
        if (!quest || quest.currentStep >= quest.steps.length) {
            return null;
        }
        return quest.steps[quest.currentStep];
    }

    // Проверить, может ли персонаж начать квест
    canStartQuest(gameState, character, questId) {
        const quest = QuestData.getQuest(questId);
        if (!quest || quest.character !== character) {
            return false;
        }

        // Проверяем, что нет активного квеста
        if (gameState.quests[character].active) {
            return false;
        }

        // Проверяем, что квест не был завершён ранее
        const completed = gameState.quests[character].completed;
        return !completed.some(q => q.id === questId);
    }

    // Обработать квестовое действие из диалога
    processQuestAction(gameState, character, choiceId, dialogueResult) {
        // Делегируем обработку в отдельный модуль
        return processQuestAction(gameState, character, choiceId, dialogueResult, this);
    }
}

module.exports = CoopGameLogic;
