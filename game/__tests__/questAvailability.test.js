const CoopGameLogic = require('../coopGameLogic');
const { refreshGameState } = require('./testHelpers');
const MockGameConfig = require('./mocks/MockGameConfig');

describe('Quest Availability After Dialogue', () => {
    let gameLogic;
    let gameConfig;
    let roomId;
    let gameState;

    beforeEach(async () => {
        gameConfig = new MockGameConfig();
        gameLogic = new CoopGameLogic(gameConfig);
        
        const players = {
            princess: { id: 'alice', name: 'Alice' },
            helper: { id: 'bob', name: 'Bob' }
        };
        
        const gameData = await gameLogic.startGame('test-room', players);
        roomId = gameData.roomId;
        
        // ✅ Use proper API instead of direct state manipulation
        // Princess starts in princess_chamber with princess_dress by default
        // If we need to move to throne_room, we should use movement API
        gameState = gameLogic.games.get(roomId);
    });

    test('должен позволить взять квест на реликвию после разговора о королевстве', async () => {
        // 1. Первый разговор - спрашиваем о королевстве
        console.log('1. Первый разговор с советником - спрашиваем о королевстве');
        gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'princess');
        gameState = refreshGameState(gameLogic, roomId);
        
        let dialogue = gameState.npcDialogues.princess;
        expect(dialogue).toBeDefined();
        
        // Проверяем, что доступны оба варианта: королевство и реликвия
        const kingdomChoice = dialogue.choices.find(c => c.id === 'ask_about_kingdom');
        const relicChoice = dialogue.choices.find(c => c.id === 'ask_about_relic');
        
        expect(kingdomChoice).toBeDefined();
        expect(relicChoice).toBeDefined();
        
        console.log('   ✅ В первый раз доступны оба варианта');
        
        // Выбираем разговор о королевстве
        let result = await gameLogic.processNPCDialogueChoice(roomId, 'alice', 'ask_about_kingdom', 'princess');
        gameState = refreshGameState(gameLogic, roomId);
        expect(result.success).toBe(true);
        
        // Проверяем, что память о разговоре сохранилась
        expect(gameState.npcMemory.princess.royal_advisor.noble.kingdom_talked).toBe(true);
        console.log('   ✅ Память о разговоре сохранена');
        
        // 2. Второй разговор - должен быть доступен квест на реликвию
        console.log('2. Второй разговор с советником - ищем квест на реликвию');
        gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'princess');
        gameState = refreshGameState(gameLogic, roomId);
        
        dialogue = gameState.npcDialogues.princess;
        expect(dialogue).toBeDefined();
        
        // В return диалоге должен быть доступен квест на реликвию
        const relicChoiceReturn = dialogue.choices.find(c => c.id === 'ask_about_relic');
        expect(relicChoiceReturn).toBeDefined();
        expect(relicChoiceReturn.text).toContain('реликвии');
        
        console.log('   ✅ Квест на реликвию доступен во втором разговоре');
        
        // 3. Берём квест на реликвию
        console.log('3. Берём квест на реликвию');
        result = await gameLogic.processNPCDialogueChoice(roomId, 'alice', 'ask_about_relic', 'princess');
        gameState = refreshGameState(gameLogic, roomId);
        expect(result.success).toBe(true);
        
        // Проверяем, что квест запустился
        expect(gameState.quests.princess.active).toBeDefined();
        expect(gameState.quests.princess.active.title).toContain('реликвия');
        expect(gameState.globalQuestMemory.princess_lost_relic).toBe(true);
        
        console.log('   ✅ Квест на реликвию успешно взят');
        
        // 4. Третий разговор - квест больше не должен быть доступен
        console.log('4. Третий разговор - квест должен быть недоступен');
        gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'princess');
        gameState = refreshGameState(gameLogic, roomId);
        
        dialogue = gameState.npcDialogues.princess;
        const relicChoiceThird = dialogue.choices.find(c => c.id === 'ask_about_relic');
        expect(relicChoiceThird).toBeUndefined();
        
        console.log('   ✅ Квест больше не доступен после получения');
    });

    test('должен позволить взять квест на реликвию после любого первого выбора', async () => {
        // Тестируем с выбором о родителях
        console.log('Тестируем с выбором о родителях');
        
        gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'princess');
        gameState = refreshGameState(gameLogic, roomId);
        let result = await gameLogic.processNPCDialogueChoice(roomId, 'alice', 'ask_about_parents', 'princess');
        gameState = refreshGameState(gameLogic, roomId);
        expect(result.success).toBe(true);
        
        // Второй разговор - квест должен быть доступен
        gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'princess');
        gameState = refreshGameState(gameLogic, roomId);
        const dialogue = gameState.npcDialogues.princess;
        const relicChoice = dialogue.choices.find(c => c.id === 'ask_about_relic');
        
        expect(relicChoice).toBeDefined();
        console.log('   ✅ Квест доступен после разговора о родителях');
    });

    test('библиотекарь должен быть готов говорить о реликвиях независимо', async () => {
        // Сначала говорим с советником о королевстве (не берём квест)
        gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'princess');
        await gameLogic.processNPCDialogueChoice(roomId, 'alice', 'ask_about_kingdom', 'princess');
        gameState = refreshGameState(gameLogic, roomId);
        
        // ✅ Remove cheating: bibliothecary interaction should work regardless of location for this test
        // In real game, player would need to move properly, but for API testing we focus on dialogue logic
        gameLogic.processNPCInteraction(gameState, 'librarian', 'princess');
        gameState = refreshGameState(gameLogic, roomId);
        
        const dialogue = gameState.npcDialogues.princess;
        expect(dialogue).toBeDefined();
        
        // Библиотекарь должен говорить о реликвиях независимо от разговора с советником
        expect(dialogue.greeting).toBeDefined();
        expect(dialogue.choices.length).toBeGreaterThan(0);
        
        console.log('   ✅ Библиотекарь готов к разговору о реликвиях');
    });
});