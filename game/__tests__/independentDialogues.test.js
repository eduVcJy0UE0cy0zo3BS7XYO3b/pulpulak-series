const CoopGameLogic = require('../coopGameLogic');

describe('Independent Dialogues System', () => {
    let gameLogic;
    let roomId;

    beforeEach(() => {
        gameLogic = new CoopGameLogic();
        
        const players = {
            princess: { id: 'alice', name: 'Alice' },
            helper: { id: 'bob', name: 'Bob' }
        };
        
        const gameData = gameLogic.startGame('test-room', players);
        roomId = gameData.roomId;
    });

    test('должен позволить одновременные диалоги для разных игроков', () => {
        const gameState = gameLogic.games.get(roomId);
        
        // Размещаем обоих персонажей в разных локациях с NPC
        gameState.stats.princess.location = 'throne_room'; // королевский советник
        gameState.stats.helper.location = 'kitchen'; // повар
        
        // Княжна начинает диалог с советником
        let result1 = gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'princess');
        expect(result1.success).toBe(true);
        expect(gameState.npcDialogues.princess).toBeDefined();
        expect(gameState.npcDialogues.princess.npcName).toBe('Королевский советник Эдвард');
        
        // Помощница начинает диалог с поваром (одновременно!)
        let result2 = gameLogic.processNPCInteraction(gameState, 'cook', 'helper');
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
        const gameState = gameLogic.games.get(roomId);
        
        // Размещаем обоих персонажей
        gameState.stats.princess.location = 'throne_room';
        gameState.stats.helper.location = 'kitchen';
        gameState.stats.helper.outfit = 'common_dress'; // чтобы повар был дружелюбен
        
        // Начинаем диалоги
        gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'princess');
        gameLogic.processNPCInteraction(gameState, 'cook', 'helper');
        
        // Получаем выборы для каждого персонажа
        const princessChoice = gameState.npcDialogues.princess.choices[0];
        const helperChoice = gameState.npcDialogues.helper.choices[0];
        
        // Княжна делает выбор
        let result1 = gameLogic.processNPCDialogueChoice(roomId, 'alice', princessChoice.id, 'princess');
        expect(result1.success).toBe(true);
        
        // Помощница делает выбор
        let result2 = gameLogic.processNPCDialogueChoice(roomId, 'bob', helperChoice.id, 'helper');
        expect(result2.success).toBe(true);
        
        // Проверяем, что выборы обработались независимо
        expect(result1.message).toBeDefined();
        expect(result2.message).toBeDefined();
        expect(result1.message).not.toBe(result2.message);
    });

    test('должен позволить закрывать диалоги независимо', () => {
        const gameState = gameLogic.games.get(roomId);
        
        // Размещаем обоих персонажей
        gameState.stats.princess.location = 'throne_room';
        gameState.stats.helper.location = 'kitchen';
        
        // Начинаем диалоги
        gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'princess');
        gameLogic.processNPCInteraction(gameState, 'cook', 'helper');
        
        // Оба диалога активны
        expect(gameState.npcDialogues.princess).toBeDefined();
        expect(gameState.npcDialogues.helper).toBeDefined();
        
        // Княжна закрывает свой диалог
        let result1 = gameLogic.closeNPCDialogue(roomId, 'alice');
        expect(result1.success).toBe(true);
        
        // Диалог княжны закрыт, диалог помощницы остается открытым
        expect(gameState.npcDialogues.princess).toBeNull();
        expect(gameState.npcDialogues.helper).toBeDefined();
        
        // Помощница закрывает свой диалог
        let result2 = gameLogic.closeNPCDialogue(roomId, 'bob');
        expect(result2.success).toBe(true);
        
        // Оба диалога закрыты
        expect(gameState.npcDialogues.princess).toBeNull();
        expect(gameState.npcDialogues.helper).toBeNull();
    });

    test('игрок не может закрыть диалог другого игрока', () => {
        const gameState = gameLogic.games.get(roomId);
        
        // Только княжна начинает диалог
        gameState.stats.princess.location = 'throne_room';
        gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'princess');
        
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
        const gameState = gameLogic.games.get(roomId);
        
        // Размещаем обоих персонажей
        gameState.stats.princess.location = 'throne_room';
        gameState.stats.helper.location = 'kitchen';
        
        // Начинаем диалоги
        gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'princess');
        gameLogic.processNPCInteraction(gameState, 'cook', 'helper');
        
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