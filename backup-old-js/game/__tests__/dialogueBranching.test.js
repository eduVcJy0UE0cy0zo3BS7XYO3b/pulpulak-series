const CoopGameLogic = require('../coopGameLogic');
const NPCData = require('../data/npcDataSCM');

describe('Branching Dialogue System', () => {
    let gameLogic;
    const roomId = 'TEST_DIALOGUE';
    const players = {
        princess: { id: 'alice', name: 'Алиса' },
        helper: { id: 'bob', name: 'Боб' }
    };

    beforeEach(() => {
        gameLogic = new CoopGameLogic();
        gameLogic.startGame(roomId, players);
    });

    describe('Memory System', () => {
        test('должен инициализировать пустую память для NPC', () => {
            const gameState = gameLogic.lobbyLogic.getGameState(roomId);
            
            expect(gameState.npcMemory).toBeDefined();
            expect(gameState.npcMemory.princess).toEqual({});
            expect(gameState.npcMemory.helper).toEqual({});
        });

        test('должен показывать разные диалоги при первой встрече и возвращении', () => {
            const gameState = gameLogic.lobbyLogic.getGameState(roomId);
            gameState.stats.princess.location = 'throne_room';
            const questIntegration = gameLogic.lobbyLogic.getQuestIntegration(roomId);
            
            // Первая встреча с советником в княжеском платье
            let result = gameLogic.choiceHandler.processNPCInteraction(gameState, 'royal_advisor', 'princess', questIntegration);
            expect(result.success).toBe(true);
            
            const firstMeetingGreeting = gameState.npcDialogues.princess.greeting;
            expect(firstMeetingGreeting).toContain('Рад видеть вас в добром здравии');
            
            // Делаем выбор, который добавит память
            gameLogic.processNPCDialogueChoice(roomId, 'alice', 'ask_about_kingdom', 'princess');
            
            // Проверяем, что память обновилась
            expect(gameState.npcMemory.princess.royal_advisor.noble.kingdom_talked).toBe(true);
            
            // Второе взаимодействие - должен показать диалог возвращения
            result = gameLogic.choiceHandler.processNPCInteraction(gameState, 'royal_advisor', 'princess', questIntegration);
            expect(result.success).toBe(true);
            
            const returnGreeting = gameState.npcDialogues.princess.greeting;
            expect(returnGreeting).toContain('Снова приветствую вас');
            expect(returnGreeting).not.toBe(firstMeetingGreeting);
        });

        test('должен помнить персонажа по наряду', () => {
            const gameState = gameLogic.lobbyLogic.getGameState(roomId);
            gameState.stats.princess.location = 'kitchen';
            const questIntegration = gameLogic.lobbyLogic.getQuestIntegration(roomId);
            
            // Первая встреча в княжеском платье
            gameLogic.choiceHandler.processNPCInteraction(gameState, 'cook', 'princess', questIntegration);
            gameLogic.processNPCDialogueChoice(roomId, 'alice', 'demand_food', 'princess');
            
            // Меняем одежду на простую
            gameState.stats.princess.outfit = 'common_dress';
            
            // Взаимодействие в простом платье - должен показать диалог для простолюдина
            let result = gameLogic.choiceHandler.processNPCInteraction(gameState, 'cook', 'princess', questIntegration);
            expect(result.success).toBe(true);
            expect(gameState.npcDialogues.princess.greeting).toContain('милая!');
            
            // Делаем выбор чтобы создать память для common наряда
            gameLogic.processNPCDialogueChoice(roomId, 'alice', 'accept_food', 'princess');
            
            // Возвращаемся в княжеское платье
            gameState.stats.princess.outfit = 'princess_dress';
            gameState.npcDialogues.princess = null;
            
            // Взаимодействие в княжеском платье - должен показать return диалог, так как встречались раньше в этом наряде
            result = gameLogic.choiceHandler.processNPCInteraction(gameState, 'cook', 'princess', questIntegration);
            expect(result.success).toBe(true);
            expect(gameState.npcDialogues.princess.greeting).toContain('чем могу служить');
        });

        test('должен показывать выборы на основе памяти', () => {
            const gameState = gameLogic.lobbyLogic.getGameState(roomId);
            gameState.stats.princess.location = 'throne_room';
            const questIntegration = gameLogic.lobbyLogic.getQuestIntegration(roomId);
            
            // Первая встреча
            gameLogic.choiceHandler.processNPCInteraction(gameState, 'royal_advisor', 'princess', questIntegration);
            gameLogic.processNPCDialogueChoice(roomId, 'alice', 'ask_about_kingdom', 'princess');
            
            // Второе взаимодействие
            gameLogic.choiceHandler.processNPCInteraction(gameState, 'royal_advisor', 'princess', questIntegration);
            
            // Должен появиться выбор, который требует предыдущего разговора о королевстве
            const kingdomChoice = gameState.npcDialogues.princess.choices.find(c => c.id === 'continue_kingdom');
            expect(kingdomChoice).toBeDefined();
            expect(kingdomChoice.text).toContain('Ещё о делах королевства');
        });
    });

    describe('Branching Dialogues', () => {
        test('должен показывать дополнительные выборы после основного выбора', () => {
            const gameState = gameLogic.lobbyLogic.getGameState(roomId);
            gameState.stats.princess.location = 'secret_garden';
            gameState.stats.princess.outfit = 'common_dress';
            const questIntegration = gameLogic.lobbyLogic.getQuestIntegration(roomId);
            
            // Начинаем диалог с садовником
            gameLogic.choiceHandler.processNPCInteraction(gameState, 'gardener', 'princess', questIntegration);
            
            // Выбираем "Посмотреть розы" - у этого выбора есть next_choices
            const result = gameLogic.processNPCDialogueChoice(roomId, 'alice', 'see_roses', 'princess');
            
            expect(result.success).toBe(true);
            expect(result.hasFollowUp).toBe(true);
            
            // Диалог должен остаться активным с новыми выборами
            expect(gameState.npcDialogues.princess).not.toBeNull();
            expect(gameState.npcDialogues.princess.isFollowUp).toBe(true);
            
            // Должен быть доступен дополнительный выбор
            const followUpChoice = gameState.npcDialogues.princess.choices.find(c => c.id === 'learn_about_herbs');
            expect(followUpChoice).toBeDefined();
            expect(followUpChoice.text).toContain('Узнать о травах');
        });

        test('должен корректно завершать диалог после дополнительных выборов', () => {
            const gameState = gameLogic.lobbyLogic.getGameState(roomId);
            gameState.stats.princess.location = 'secret_garden';
            gameState.stats.princess.outfit = 'common_dress';
            const questIntegration = gameLogic.lobbyLogic.getQuestIntegration(roomId);
            
            // Начинаем диалог и делаем выбор с дополнительными опциями
            gameLogic.choiceHandler.processNPCInteraction(gameState, 'gardener', 'princess', questIntegration);
            let result = gameLogic.processNPCDialogueChoice(roomId, 'alice', 'see_roses', 'princess');
            
            expect(result.hasFollowUp).toBe(true);
            expect(gameState.npcDialogues.princess.isFollowUp).toBe(true);
            
            // Проверяем, что есть дополнительный выбор
            const followUpChoice = gameState.npcDialogues.princess.choices.find(c => c.id === 'learn_about_herbs');
            expect(followUpChoice).toBeDefined();
            
            // Делаем дополнительный выбор
            result = gameLogic.processNPCDialogueChoice(roomId, 'alice', 'learn_about_herbs', 'princess');
            
            expect(result.success).toBe(true);
            expect(result.hasFollowUp).toBeFalsy();
            
            // Диалог должен завершиться
            expect(gameState.npcDialogues.princess).toBeNull();
        });

        test('должен применять эффекты из основного выбора', () => {
            const gameState = gameLogic.lobbyLogic.getGameState(roomId);
            gameState.stats.princess.location = 'secret_garden';
            gameState.stats.princess.outfit = 'common_dress';
            const questIntegration = gameLogic.lobbyLogic.getQuestIntegration(roomId);
            
            const inventoryBefore = gameState.stats.princess.inventory.length;
            
            // Выбираем опцию, которая даёт предмет
            gameLogic.choiceHandler.processNPCInteraction(gameState, 'gardener', 'princess', questIntegration);
            gameLogic.processNPCDialogueChoice(roomId, 'alice', 'love_flowers', 'princess');
            
            // Проверяем, что предмет добавился
            expect(gameState.stats.princess.inventory.length).toBe(inventoryBefore + 1);
            expect(gameState.stats.princess.inventory).toContain('garden_rose');
        });
    });

    describe('NPC Data Methods', () => {
        test('должен обрабатывать выбор диалога и обновлять память', () => {
            const npcMemory = {};
            const result = NPCData.processDialogueChoice('royal_advisor', 'ask_about_kingdom', 'princess_dress', npcMemory);
            
            expect(result).toBeDefined();
            expect(result.response).toContain('прекрасно');
            expect(result.updatedMemory.noble.kingdom_talked).toBe(true);
            expect(result.next_choices).toBeDefined();
            expect(result.next_choices.length).toBeGreaterThan(0);
        });

        test('должен возвращать разные диалоги для новых и возвращающихся персонажей', () => {
            // Первая встреча
            let dialogue = NPCData.getNPCDialogue('cook', 'common_dress', {});
            expect(dialogue.greeting).toContain('милая!');
            
            // После взаимодействия (любая память означает, что встречались раньше)
            const memory = { common: { helped_cooking: true } };
            dialogue = NPCData.getNPCDialogue('cook', 'common_dress', memory);
            expect(dialogue.greeting).toContain('снова ты!');
        });

        test('должен фильтровать выборы на основе памяти', () => {
            // Без памяти - базовые выборы
            let dialogue = NPCData.getNPCDialogue('cook', 'common_dress', {});
            let moreFoodChoice = dialogue.choices.find(c => c.id === 'more_food');
            expect(moreFoodChoice).toBeUndefined();
            
            // С памятью - дополнительные выборы
            const memory = { common: { helped_cooking: true } };
            dialogue = NPCData.getNPCDialogue('cook', 'common_dress', memory);
            moreFoodChoice = dialogue.choices.find(c => c.id === 'more_food');
            expect(moreFoodChoice).toBeDefined();
        });
    });

    describe('Integration with Game Logic', () => {
        test('должен сохранять память через полный игровой цикл', () => {
            const gameState = gameLogic.lobbyLogic.getGameState(roomId);
            gameState.stats.princess.location = 'kitchen';
            gameState.stats.princess.outfit = 'common_dress';
            const questIntegration = gameLogic.lobbyLogic.getQuestIntegration(roomId);
            
            // Полный цикл взаимодействия
            gameLogic.choiceHandler.processNPCInteraction(gameState, 'cook', 'princess', questIntegration);
            gameLogic.processNPCDialogueChoice(roomId, 'alice', 'accept_food', 'princess');
            
            // Проверяем память
            expect(gameState.npcMemory.princess.cook.common.ate_together).toBe(true);
            
            // Проверяем, что предмет добавился
            expect(gameState.stats.princess.inventory).toContain('marta_pie');
            
            // Второе взаимодействие должно показать диалог возвращения
            gameLogic.choiceHandler.processNPCInteraction(gameState, 'cook', 'princess', questIntegration);
            expect(gameState.npcDialogues.princess.greeting).toContain('снова ты!');
        });

        test('должен работать независимо для разных персонажей', () => {
            const gameState = gameLogic.lobbyLogic.getGameState(roomId);
            gameState.stats.princess.location = 'kitchen';
            gameState.stats.helper.location = 'kitchen';
            gameState.stats.princess.outfit = 'common_dress';
            gameState.stats.helper.outfit = 'common_dress';
            const questIntegration = gameLogic.lobbyLogic.getQuestIntegration(roomId);
            
            // Взаимодействие княжны
            gameLogic.choiceHandler.processNPCInteraction(gameState, 'cook', 'princess', questIntegration);
            gameLogic.processNPCDialogueChoice(roomId, 'alice', 'accept_food', 'princess');
            
            // Взаимодействие помощницы - должно быть первое знакомство
            gameLogic.choiceHandler.processNPCInteraction(gameState, 'cook', 'helper', questIntegration);
            expect(gameState.npcDialogues.helper.greeting).toContain('милая!');
            
            // Проверяем, что память разная
            expect(gameState.npcMemory.princess.cook.common.ate_together).toBe(true);
            expect(gameState.npcMemory.helper.cook).toEqual({});
        });
    });
});