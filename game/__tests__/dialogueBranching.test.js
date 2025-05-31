const CoopGameLogic = require('../coopGameLogic');
const { refreshGameState } = require('./testHelpers');
const NPCData = require('../npcData');

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
            let gameState = gameLogic.games.get(roomId);
            
            expect(gameState.npcMemory).toBeDefined();
            expect(gameState.npcMemory.princess).toEqual({});
            expect(gameState.npcMemory.helper).toEqual({});
        });

        test('должен показывать разные диалоги при первой встрече и возвращении', () => {
            let gameState = gameLogic.games.get(roomId);
            gameState.stats.princess.location = 'throne_room';
            
            // Первая встреча с советником в княжеском платье
            let result = gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'princess');
            gameState = refreshGameState(gameLogic, roomId);
            expect(result.success).toBe(true);
            
            const firstMeetingGreeting = gameState.npcDialogues.princess.greeting;
            expect(firstMeetingGreeting).toContain('Рад видеть вас в добром здравии');
            
            // Делаем выбор, который добавит память
            gameLogic.processNPCDialogueChoice(roomId, 'alice', 'ask_about_kingdom', 'princess');
            gameState = refreshGameState(gameLogic, roomId);
            
            // Проверяем, что память обновилась
            expect(gameState.npcMemory.princess.royal_advisor.noble.kingdom_talked).toBe(true);
            
            // Второе взаимодействие - должен показать диалог возвращения
            result = gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'princess');
            gameState = refreshGameState(gameLogic, roomId);
            expect(result.success).toBe(true);
            
            const returnGreeting = gameState.npcDialogues.princess.greeting;
            expect(returnGreeting).toContain('Снова приветствую вас');
            expect(returnGreeting).not.toBe(firstMeetingGreeting);
        });

        test('должен помнить персонажа по наряду', () => {
            let gameState = gameLogic.games.get(roomId);
            gameState.stats.princess.location = 'kitchen';
            
            // Первая встреча в княжеском платье
            gameLogic.processNPCInteraction(gameState, 'cook', 'princess');
            gameState = refreshGameState(gameLogic, roomId);
            gameLogic.processNPCDialogueChoice(roomId, 'alice', 'ask_for_food', 'princess');
            gameState = refreshGameState(gameLogic, roomId);
            
            // Меняем одежду на простую через Immer
            gameState = gameLogic.immerStateManager.updateState(gameState, draft => {
                draft.stats.princess.outfit = 'common_dress';
            });
            gameLogic.games.set(roomId, gameState);
            
            // Взаимодействие в простом платье - должен показать диалог для простолюдина
            let result = gameLogic.processNPCInteraction(gameState, 'cook', 'princess');
            gameState = refreshGameState(gameLogic, roomId);
            expect(result.success).toBe(true);
            expect(gameState.npcDialogues.princess.greeting).toContain('милая!');
            
            // Возвращаемся в княжеское платье через Immer
            gameState = gameLogic.immerStateManager.updateState(gameState, draft => {
                draft.stats.princess.outfit = 'princess_dress';
                draft.npcDialogues.princess = null;
            });
            gameLogic.games.set(roomId, gameState);
            
            // Взаимодействие в княжеском платье - должен помнить предыдущее взаимодействие
            result = gameLogic.processNPCInteraction(gameState, 'cook', 'princess');
            gameState = refreshGameState(gameLogic, roomId);
            expect(result.success).toBe(true);
            expect(gameState.npcDialogues.princess.greeting).toContain('Снова изволите меня посетить');
        });

        test('должен показывать выборы на основе памяти', () => {
            let gameState = gameLogic.games.get(roomId);
            gameState.stats.princess.location = 'throne_room';
            
            // Первая встреча
            gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'princess');
            gameLogic.processNPCDialogueChoice(roomId, 'alice', 'ask_about_kingdom', 'princess');
            gameState = refreshGameState(gameLogic, roomId);
            
            // Второе взаимодействие
            gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'princess');
            gameState = refreshGameState(gameLogic, roomId);
            
            // Должен появиться выбор, который требует предыдущего разговора о королевстве
            const kingdomChoice = gameState.npcDialogues.princess.choices.find(c => c.id === 'continue_kingdom');
            expect(kingdomChoice).toBeDefined();
            expect(kingdomChoice.text).toContain('Ещё о делах королевства');
        });
    });

    describe('Branching Dialogues', () => {
        test('должен показывать дополнительные выборы после основного выбора', () => {
            let gameState = gameLogic.games.get(roomId);
            // Используем Immer для безопасного обновления состояния
            gameState = gameLogic.immerStateManager.updateState(gameState, draft => {
                draft.stats.princess.location = 'secret_garden';
                draft.stats.princess.outfit = 'common_dress';
            });
            gameLogic.games.set(roomId, gameState);
            
            // Начинаем диалог с садовником
            gameLogic.processNPCInteraction(gameState, 'gardener', 'princess');
            gameState = refreshGameState(gameLogic, roomId);
            
            // Выбираем "Посмотреть розы" - у этого выбора есть next_choices
            const result = gameLogic.processNPCDialogueChoice(roomId, 'alice', 'see_roses', 'princess');
            gameState = refreshGameState(gameLogic, roomId);
            
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
            let gameState = gameLogic.games.get(roomId);
            // Используем Immer для безопасного обновления состояния
            gameState = gameLogic.immerStateManager.updateState(gameState, draft => {
                draft.stats.princess.location = 'secret_garden';
                draft.stats.princess.outfit = 'common_dress';
            });
            gameLogic.games.set(roomId, gameState);
            
            // Начинаем диалог и делаем выбор с дополнительными опциями
            gameLogic.processNPCInteraction(gameState, 'gardener', 'princess');
            let result = gameLogic.processNPCDialogueChoice(roomId, 'alice', 'see_roses', 'princess');
            gameState = refreshGameState(gameLogic, roomId);
            
            expect(result.hasFollowUp).toBe(true);
            expect(gameState.npcDialogues.princess.isFollowUp).toBe(true);
            
            // Проверяем, что есть дополнительный выбор
            const followUpChoice = gameState.npcDialogues.princess.choices.find(c => c.id === 'learn_about_herbs');
            expect(followUpChoice).toBeDefined();
            
            // Делаем дополнительный выбор
            result = gameLogic.processNPCDialogueChoice(roomId, 'alice', 'learn_about_herbs', 'princess');
            gameState = refreshGameState(gameLogic, roomId);
            
            expect(result.success).toBe(true);
            expect(result.hasFollowUp).toBeFalsy();
            
            // Диалог должен завершиться
            expect(gameState.npcDialogues.princess).toBeNull();
        });

        test('должен применять эффекты из основного выбора', () => {
            let gameState = gameLogic.games.get(roomId);
            // Используем Immer для безопасного обновления состояния
            gameState = gameLogic.immerStateManager.updateState(gameState, draft => {
                draft.stats.princess.location = 'secret_garden';
                draft.stats.princess.outfit = 'common_dress';
            });
            gameLogic.games.set(roomId, gameState);
            
            const inventoryBefore = gameState.stats.princess.inventory.length;
            
            // Выбираем опцию, которая даёт предмет
            gameLogic.processNPCInteraction(gameState, 'gardener', 'princess');
            gameLogic.processNPCDialogueChoice(roomId, 'alice', 'see_roses', 'princess');
            gameState = refreshGameState(gameLogic, roomId);
            
            // Проверяем, что предмет добавился
            expect(gameState.stats.princess.inventory.length).toBe(inventoryBefore + 1);
            expect(gameState.stats.princess.inventory).toContain('healing_herbs');
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
            expect(dialogue.greeting).toContain('милая! Проголодалась?');
            
            // После взаимодействия
            const memory = { common: { ate_together: true } };
            dialogue = NPCData.getNPCDialogue('cook', 'common_dress', memory);
            expect(dialogue.greeting).toContain('моя дорогая вернулась!');
        });

        test('должен фильтровать выборы на основе памяти', () => {
            // Без памяти - базовые выборы
            let dialogue = NPCData.getNPCDialogue('cook', 'common_dress', {});
            let moreFoodChoice = dialogue.choices.find(c => c.id === 'more_food');
            expect(moreFoodChoice).toBeUndefined();
            
            // С памятью - дополнительные выборы
            const memory = { common: { ate_together: true } };
            dialogue = NPCData.getNPCDialogue('cook', 'common_dress', memory);
            moreFoodChoice = dialogue.choices.find(c => c.id === 'more_food');
            expect(moreFoodChoice).toBeDefined();
        });
    });

    describe('Integration with Game Logic', () => {
        test('должен сохранять память через полный игровой цикл', () => {
            let gameState = gameLogic.games.get(roomId);
            // Используем Immer для безопасного обновления состояния
            gameState = gameLogic.immerStateManager.updateState(gameState, draft => {
                draft.stats.princess.location = 'kitchen';
                draft.stats.princess.outfit = 'common_dress';
            });
            gameLogic.games.set(roomId, gameState);
            
            // Полный цикл взаимодействия
            gameLogic.processNPCInteraction(gameState, 'cook', 'princess');
            gameLogic.processNPCDialogueChoice(roomId, 'alice', 'accept_food', 'princess');
            gameState = refreshGameState(gameLogic, roomId);
            
            // Проверяем память
            expect(gameState.npcMemory.princess.cook.common.ate_together).toBe(true);
            
            // Проверяем, что предмет добавился
            expect(gameState.stats.princess.inventory).toContain('marta_pie');
            
            // Второе взаимодействие должно показать диалог возвращения
            gameLogic.processNPCInteraction(gameState, 'cook', 'princess');
            gameState = refreshGameState(gameLogic, roomId);
            expect(gameState.npcDialogues.princess.greeting).toContain('вернулась');
        });

        test('должен работать независимо для разных персонажей', () => {
            let gameState = gameLogic.games.get(roomId);
            // Используем Immer для безопасного обновления состояния
            gameState = gameLogic.immerStateManager.updateState(gameState, draft => {
                draft.stats.princess.location = 'kitchen';
                draft.stats.helper.location = 'kitchen';
                draft.stats.princess.outfit = 'common_dress';
                draft.stats.helper.outfit = 'common_dress';
            });
            gameLogic.games.set(roomId, gameState);
            
            // Взаимодействие княжны
            gameLogic.processNPCInteraction(gameState, 'cook', 'princess');
            gameLogic.processNPCDialogueChoice(roomId, 'alice', 'accept_food', 'princess');
            gameState = refreshGameState(gameLogic, roomId);
            
            // Взаимодействие помощницы - должно быть первое знакомство
            gameLogic.processNPCInteraction(gameState, 'cook', 'helper');
            gameState = refreshGameState(gameLogic, roomId);
            expect(gameState.npcDialogues.helper.greeting).toContain('Проголодалась?');
            expect(gameState.npcDialogues.helper.greeting).not.toContain('вернулась');
            
            // Проверяем, что память разная
            expect(gameState.npcMemory.princess.cook.common.ate_together).toBe(true);
            expect(gameState.npcMemory.helper.cook).toEqual({});
        });
    });
});