const CoopGameLogic = require('../coopGameLogic');
const NPCData = require('../npcData');

describe('NPC Interactions', () => {
    let gameLogic;
    const roomId = 'TEST_NPC';
    const players = {
        princess: { id: 'alice', name: 'Алиса' },
        helper: { id: 'bob', name: 'Боб' }
    };

    beforeEach(() => {
        gameLogic = new CoopGameLogic();
        gameLogic.startGame(roomId, players);
    });

    describe('Outfit-based NPC reactions', () => {
        test('королевский советник должен хорошо относиться к княжескому наряду', () => {
            const gameState = gameLogic.games.get(roomId);
            // Княжна уже начинает в княжеском платье
            expect(gameState.stats.princess.outfit).toBe('princess_dress');
            
            const dialogue = NPCData.getNPCDialogue('royal_advisor', 'princess_dress');
            const attitude = NPCData.getNPCAttitude('royal_advisor', 'princess_dress');
            
            expect(dialogue).toBeDefined();
            expect(dialogue.greeting).toContain('Ваше высочество');
            expect(attitude).toBe('friendly');
        });

        test('королевский советник должен плохо относиться к простому наряду', () => {
            const gameState = gameLogic.games.get(roomId);
            gameState.stats.princess.outfit = 'common_dress';
            
            const dialogue = NPCData.getNPCDialogue('royal_advisor', 'common_dress');
            const attitude = NPCData.getNPCAttitude('royal_advisor', 'common_dress');
            
            expect(dialogue).toBeDefined();
            expect(dialogue.greeting).toContain('простолюдинка');
            expect(attitude).toBe('hostile');
        });

        test('повар должен лучше относиться к простому наряду', () => {
            const gameState = gameLogic.games.get(roomId);
            gameState.stats.princess.outfit = 'common_dress';
            
            const dialogue = NPCData.getNPCDialogue('cook', 'common_dress');
            const attitude = NPCData.getNPCAttitude('cook', 'common_dress');
            
            expect(dialogue).toBeDefined();
            expect(dialogue.greeting).toContain('милая');
            expect(attitude).toBe('friendly');
        });

        test('повар должен нервничать с княжеским нарядом', () => {
            const gameState = gameLogic.games.get(roomId);
            gameState.stats.princess.outfit = 'princess_dress';
            
            const dialogue = NPCData.getNPCDialogue('cook', 'princess_dress');
            const attitude = NPCData.getNPCAttitude('cook', 'princess_dress');
            
            expect(dialogue).toBeDefined();
            expect(dialogue.greeting).toContain('нервно');
            expect(attitude).toBe('hostile');
        });
    });

    describe('NPC dialogue system', () => {
        test('должен начинать диалог с NPC', () => {
            const gameState = gameLogic.games.get(roomId);
            gameState.stats.princess.location = 'throne_room';
            
            const result = gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'princess');
            
            expect(result.success).toBe(true);
            expect(gameState.currentNPCDialogue).toBeDefined();
            expect(gameState.currentNPCDialogue.choices.length).toBeGreaterThan(0);
        });

        test('должен обрабатывать выбор в диалоге', () => {
            const gameState = gameLogic.games.get(roomId);
            gameState.stats.princess.location = 'throne_room';
            
            // Начинаем диалог
            gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'princess');
            
            // Получаем первый доступный выбор
            const firstChoice = gameState.currentNPCDialogue.choices[0];
            
            // Обрабатываем выбор
            const result = gameLogic.processNPCDialogueChoice(roomId, 'alice', firstChoice.id, 'princess');
            
            expect(result.success).toBe(true);
            expect(result.message).toBeDefined();
            expect(gameState.currentNPCDialogue).toBeNull();
        });

        test('должен сохранять информацию о том, кто ведет диалог', () => {
            const gameState = gameLogic.games.get(roomId);
            gameState.stats.princess.location = 'throne_room';
            
            // Начинаем диалог как княжна
            gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'princess');
            
            expect(gameState.currentNPCDialogue.activeCharacter).toBe('princess');
        });

        test('должен показывать разные диалоги в зависимости от наряда', () => {
            const gameState = gameLogic.games.get(roomId);
            gameState.stats.princess.location = 'throne_room';
            
            // Тест с княжеским нарядом (дружелюбный диалог)
            gameState.stats.princess.outfit = 'princess_dress';
            let result = gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'princess');
            
            expect(result.success).toBe(true);
            expect(gameState.currentNPCDialogue.greeting).toContain('Ваше высочество');
            expect(gameState.currentNPCDialogue.attitude).toBe('friendly');
            
            // Очищаем диалог
            gameState.currentNPCDialogue = null;
            
            // Тест с простым нарядом (враждебный диалог)
            gameState.stats.princess.outfit = 'common_dress';
            result = gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'princess');
            
            expect(result.success).toBe(true);
            expect(gameState.currentNPCDialogue.greeting).toContain('простолюдинка');
            expect(gameState.currentNPCDialogue.attitude).toBe('hostile');
        });

        test('должен позволять закрыть диалог', () => {
            const gameState = gameLogic.games.get(roomId);
            gameState.stats.princess.location = 'throne_room';
            
            // Начинаем диалог
            gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'princess');
            expect(gameState.currentNPCDialogue).toBeDefined();
            
            // Закрываем диалог
            const result = gameLogic.closeNPCDialogue(roomId, 'alice');
            
            expect(result.success).toBe(true);
            expect(gameState.currentNPCDialogue).toBeNull();
        });

        test('не должен позволить другому игроку закрыть диалог', () => {
            const gameState = gameLogic.games.get(roomId);
            gameState.stats.princess.location = 'throne_room';
            
            // Начинаем диалог как княжна
            gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'princess');
            
            // Пытаемся закрыть диалог как помощница
            const result = gameLogic.closeNPCDialogue(roomId, 'bob');
            
            expect(result.success).toBe(false);
            expect(result.message).toContain('не можете закрыть');
            expect(gameState.currentNPCDialogue).toBeDefined(); // Диалог остается открытым
        });

        test('не должен позволить другому игроку отвечать в диалоге', () => {
            const gameState = gameLogic.games.get(roomId);
            gameState.stats.princess.location = 'throne_room';
            
            // Начинаем диалог как княжна
            gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'princess');
            const firstChoice = gameState.currentNPCDialogue.choices[0];
            
            // Пытаемся ответить как помощница (но с её player id)
            const result = gameLogic.processNPCDialogueChoice(roomId, 'bob', firstChoice.id, 'princess');
            
            expect(result.success).toBe(false);
            expect(result.message).toContain('управляете другим персонажем');
        });
    });

    describe('NPC location management', () => {
        test('должен правильно размещать NPC по локациям', () => {
            const throneRoomNpcs = NPCData.getNPCsForLocation('throne_room');
            const kitchenNpcs = NPCData.getNPCsForLocation('kitchen');
            const secretGardenNpcs = NPCData.getNPCsForLocation('secret_garden');
            
            expect(throneRoomNpcs.length).toBe(2);
            expect(throneRoomNpcs.some(npc => npc.id === 'royal_advisor')).toBe(true);
            expect(throneRoomNpcs.some(npc => npc.id === 'guard_captain')).toBe(true);
            
            expect(kitchenNpcs.length).toBe(1);
            expect(kitchenNpcs[0].id).toBe('cook');
            
            expect(secretGardenNpcs.length).toBe(1);
            expect(secretGardenNpcs[0].id).toBe('gardener');
        });

        test('должен добавлять NPC выборы только в локациях где они есть', () => {
            const gameState = gameLogic.games.get(roomId);
            
            // В спальне никого нет
            gameState.stats.princess.location = 'princess_chamber';
            const chamberChoices = gameLogic.getNPCInteractionChoices(gameState, 'princess');
            expect(chamberChoices.length).toBe(0);
            
            // В тронном зале есть NPC
            gameState.stats.princess.location = 'throne_room';
            const throneChoices = gameLogic.getNPCInteractionChoices(gameState, 'princess');
            expect(throneChoices.length).toBeGreaterThan(0);
            expect(throneChoices.every(c => c.isNPCInteraction)).toBe(true);
        });
    });

    describe('NPC effects and rewards', () => {
        test('должен давать предметы через диалог', () => {
            const gameState = gameLogic.games.get(roomId);
            gameState.stats.princess.location = 'kitchen';
            gameState.stats.princess.outfit = 'common_dress'; // Чтобы повар был дружелюбен
            
            // Начинаем диалог с поваром
            gameLogic.processNPCInteraction(gameState, 'cook', 'princess');
            
            // Ищем выбор который даёт еду
            const foodChoice = gameState.currentNPCDialogue.choices.find(c => c.id === 'accept_food');
            expect(foodChoice).toBeDefined();
            
            // Принимаем еду
            const inventoryBefore = gameState.stats.princess.inventory.length;
            gameLogic.processNPCDialogueChoice(roomId, 'alice', 'accept_food', 'princess');
            
            expect(gameState.stats.princess.inventory.length).toBe(inventoryBefore + 1);
            expect(gameState.stats.princess.inventory).toContain('marta_pie');
        });
    });
});