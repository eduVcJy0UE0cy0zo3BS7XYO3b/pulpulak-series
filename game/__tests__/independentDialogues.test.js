const CoopGameLogic = require('../coopGameLogic');
const { refreshGameState } = require('./testHelpers');
const MockGameConfig = require('./mocks/MockGameConfig');

describe('Independent Dialogues System', () => {
    let gameLogic;
    let gameConfig;
    let roomId;

    beforeEach(() => {
        gameConfig = new MockGameConfig();
        gameLogic = new CoopGameLogic(gameConfig);
        
        const players = {
            princess: { id: 'alice', name: 'Alice' },
            helper: { id: 'bob', name: 'Bob' }
        };
        
        const gameData = gameLogic.startGame('test-room', players);
        roomId = gameData.roomId;
    });

    test('должен позволить одновременные диалоги для разных игроков', () => {
        let gameState = gameLogic.games.get(roomId);
        
        // Размещаем обоих персонажей в разных локациях с NPC через Immer
        gameState = gameLogic.immerStateManager.updateState(gameState, draft => {
            draft.stats.princess.location = 'throne_room'; // королевский советник
            draft.stats.helper.location = 'kitchen'; // повар
        });
        gameLogic.games.set(roomId, gameState);
        
        // Княжна начинает диалог с советником
        let result1 = gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'princess');
            gameState = refreshGameState(gameLogic, roomId);
        expect(result1.success).toBe(true);
        expect(gameState.npcDialogues.princess).toBeDefined();
        expect(gameState.npcDialogues.princess.npcName).toBe('Королевский советник Эдвард');
        
        // Помощница начинает диалог с поваром (одновременно!)
        let result2 = gameLogic.processNPCInteraction(gameState, 'cook', 'helper');
        gameState = refreshGameState(gameLogic, roomId);
        expect(result2.success).toBe(true);
        expect(gameState.npcDialogues.helper).toBeDefined();
        expect(gameState.npcDialogues.helper.npcName).toBe('Повар Марта');
        
        // Оба диалога должны оставаться активными
        expect(gameState.npcDialogues.princess).toBeDefined();
        expect(gameState.npcDialogues.helper).toBeDefined();
        
        // Диалоги должны быть независимы
        expect(gameState.npcDialogues.princess.npcId).toBe('royal_advisor');
        expect(gameState.npcDialogues.helper.npcId).toBe('cook');
    });

    test('должен обрабатывать выборы независимо для каждого игрока', () => {
        let gameState = gameLogic.games.get(roomId);
        
        // Размещаем обоих персонажей через Immer
        gameState = gameLogic.immerStateManager.updateState(gameState, draft => {
            draft.stats.princess.location = 'throne_room';
            draft.stats.helper.location = 'kitchen';
            draft.stats.helper.outfit = 'common_dress'; // чтобы повар был дружелюбен
        });
        gameLogic.games.set(roomId, gameState);
        
        // Начинаем диалоги
        gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'princess');
        gameState = refreshGameState(gameLogic, roomId);
        gameLogic.processNPCInteraction(gameState, 'cook', 'helper');
        gameState = refreshGameState(gameLogic, roomId);
        
        // Получаем выборы для каждого персонажа
        const princessChoice = gameState.npcDialogues.princess.choices[0];
        const helperChoice = gameState.npcDialogues.helper.choices[0];
        
        // Княжна делает выбор
        let result1 = gameLogic.processNPCDialogueChoice(roomId, 'alice', princessChoice.id, 'princess');
            gameState = refreshGameState(gameLogic, roomId);
        expect(result1.success).toBe(true);
        
        // Помощница делает выбор
        let result2 = gameLogic.processNPCDialogueChoice(roomId, 'bob', helperChoice.id, 'helper');
        gameState = refreshGameState(gameLogic, roomId);
        expect(result2.success).toBe(true);
        
        // Проверяем, что выборы обработались независимо
        expect(result1.message).toBeDefined();
        expect(result2.message).toBeDefined();
        expect(result1.message).not.toBe(result2.message);
    });

    test('должен позволить закрывать диалоги независимо', () => {
        let gameState = gameLogic.games.get(roomId);
        
        // Размещаем обоих персонажей через Immer
        gameState = gameLogic.immerStateManager.updateState(gameState, draft => {
            draft.stats.princess.location = 'throne_room';
            draft.stats.helper.location = 'kitchen';
        });
        gameLogic.games.set(roomId, gameState);
        
        // Начинаем диалоги
        gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'princess');
        gameState = refreshGameState(gameLogic, roomId);
        gameLogic.processNPCInteraction(gameState, 'cook', 'helper');
        gameState = refreshGameState(gameLogic, roomId);
        
        // Оба диалога активны
        expect(gameState.npcDialogues.princess).toBeDefined();
        expect(gameState.npcDialogues.helper).toBeDefined();
        
        // Княжна закрывает свой диалог
        let result1 = gameLogic.closeNPCDialogue(roomId, 'alice');
            gameState = refreshGameState(gameLogic, roomId);
        expect(result1.success).toBe(true);
        
        // Диалог княжны закрыт, диалог помощницы остается открытым
        expect(gameState.npcDialogues.princess).toBeNull();
        expect(gameState.npcDialogues.helper).toBeDefined();
        
        // Помощница закрывает свой диалог
        let result2 = gameLogic.closeNPCDialogue(roomId, 'bob');
        gameState = refreshGameState(gameLogic, roomId);
        expect(result2.success).toBe(true);
        
        // Оба диалога закрыты
        expect(gameState.npcDialogues.princess).toBeNull();
        expect(gameState.npcDialogues.helper).toBeNull();
    });

    test('игрок не может закрыть диалог другого игрока', () => {
        let gameState = gameLogic.games.get(roomId);
        
        // Только княжна начинает диалог через Immer
        gameState = gameLogic.immerStateManager.updateState(gameState, draft => {
            draft.stats.princess.location = 'throne_room';
        });
        gameLogic.games.set(roomId, gameState);
        gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'princess');
        gameState = refreshGameState(gameLogic, roomId);
        
        expect(gameState.npcDialogues.princess).toBeDefined();
        expect(gameState.npcDialogues.helper).toBeNull();
        
        // Помощница пытается закрыть диалог (но у неё нет активного диалога)
        let result = gameLogic.closeNPCDialogue(roomId, 'bob');
        expect(result.success).toBe(false);
        expect(result.message).toContain('Нет активного диалога');
        
        // Диалог княжны остается открытым
        expect(gameState.npcDialogues.princess).toBeDefined();
    });

    test('должен передавать правильные данные диалогов клиенту', () => {
        let gameState = gameLogic.games.get(roomId);
        
        // Размещаем обоих персонажей через Immer
        gameState = gameLogic.immerStateManager.updateState(gameState, draft => {
            draft.stats.princess.location = 'throne_room';
            draft.stats.helper.location = 'kitchen';
        });
        gameLogic.games.set(roomId, gameState);
        
        // Начинаем диалоги
        gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'princess');
        gameState = refreshGameState(gameLogic, roomId);
        gameLogic.processNPCInteraction(gameState, 'cook', 'helper');
        gameState = refreshGameState(gameLogic, roomId);
        
        // Получаем данные игры
        const gameData = gameLogic.getGameData(roomId);
        
        // Проверяем, что данные диалогов переданы правильно
        expect(gameData.npcDialogues).toBeDefined();
        expect(gameData.npcDialogues.princess).toBeDefined();
        expect(gameData.npcDialogues.helper).toBeDefined();
        
        expect(gameData.npcDialogues.princess.npcName).toBe('Королевский советник Эдвард');
        expect(gameData.npcDialogues.helper.npcName).toBe('Повар Марта');
        
        expect(gameData.npcDialogues.princess.choices.length).toBeGreaterThan(0);
        expect(gameData.npcDialogues.helper.choices.length).toBeGreaterThan(0);
    });
});