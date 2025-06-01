const CoopGameLogic = require('../coopGameLogic');
const { refreshGameState } = require('./testHelpers');
const QuestData = require('../../games/pulpulak/data/questData');
const MockGameConfig = require('./mocks/MockGameConfig');

describe('Quest System', () => {
    let gameLogic;
    let gameConfig;
    const roomId = 'TEST_QUEST';
    const players = {
        princess: { id: 'alice', name: 'Алиса' },
        helper: { id: 'bob', name: 'Боб' }
    };

    beforeEach(() => {
        gameConfig = new MockGameConfig();
        gameLogic = new CoopGameLogic(gameConfig);
        gameLogic.startGame(roomId, players);
    });

    describe('Quest Data', () => {
        test('должен содержать квест для княжны', () => {
            const quest = QuestData.getQuest('princess_lost_relic');
            expect(quest).toBeDefined();
            expect(quest.character).toBe('princess');
            expect(quest.title).toContain('реликвия');
            expect(quest.steps.length).toBeGreaterThan(0);
        });

        test('должен содержать квест для помощницы', () => {
            const quest = QuestData.getQuest('helper_secret_potion');
            expect(quest).toBeDefined();
            expect(quest.character).toBe('helper');
            expect(quest.title).toContain('зелье');
            expect(quest.steps.length).toBeGreaterThan(0);
        });

        test('должен создавать копию квеста', () => {
            const quest = QuestData.createQuestInstance('princess_lost_relic');
            expect(quest).toBeDefined();
            expect(quest.currentStep).toBe(0);
            expect(quest.steps[0].completed).toBe(false);
        });
    });

    describe('Quest Management', () => {
        test('должен инициализировать пустые квесты', () => {
            let gameState = gameLogic.games.get(roomId);
            expect(gameState.quests).toBeDefined();
            expect(gameState.quests.princess.active).toBeNull();
            expect(gameState.quests.helper.active).toBeNull();
            expect(gameState.quests.princess.completed).toEqual([]);
            expect(gameState.quests.helper.completed).toEqual([]);
        });

        test('должен начинать квест для княжны', () => {
            let gameState = gameLogic.games.get(roomId);
            const result = gameLogic.startQuest(gameState, 'princess', 'princess_lost_relic');
            gameState = refreshGameState(gameLogic, roomId);
            
            expect(result.success).toBe(true);
            expect(result.quest).toBeDefined();
            
            // Получаем обновленное состояние
            const updatedGameState = gameLogic.games.get(roomId);
            expect(updatedGameState.quests.princess.active).toBeDefined();
            expect(updatedGameState.quests.princess.active.title).toContain('реликвия');
        });

        test('должен проверять возможность начать квест', () => {
            let gameState = gameLogic.games.get(roomId);
            
            // Изначально можно начать квест
            expect(gameLogic.canStartQuest(gameState, 'princess', 'princess_lost_relic')).toBe(true);
            
            // После начала квеста нельзя начать другой
            gameLogic.startQuest(gameState, 'princess', 'princess_lost_relic');
            gameState = gameLogic.games.get(roomId);
            expect(gameLogic.canStartQuest(gameState, 'princess', 'princess_lost_relic')).toBe(false);
        });

        test('должен обновлять прогресс квеста', () => {
            let gameState = gameLogic.games.get(roomId);
            gameLogic.startQuest(gameState, 'princess', 'princess_lost_relic');
            gameState = gameLogic.games.get(roomId);
            
            const result = gameLogic.updateQuestProgress(gameState, 'princess', 'get_quest');
            gameState = gameLogic.games.get(roomId);
            
            expect(result.success).toBe(true);
            expect(gameState.quests.princess.active.currentStep).toBe(1);
            expect(gameState.quests.princess.active.steps[0].completed).toBe(true);
        });

        test('должен завершать квест и давать награды', () => {
            let gameState = gameLogic.games.get(roomId);
            gameLogic.startQuest(gameState, 'princess', 'princess_lost_relic');
            gameState = gameLogic.games.get(roomId);
            
            const quest = gameState.quests.princess.active;
            const inventoryBefore = gameState.stats.princess.inventory.length;
            
            // Проходим все шаги квеста
            quest.steps.forEach((step, index) => {
                let currentGameState = gameLogic.games.get(roomId);
                // Устанавливаем текущий шаг через Immer
                currentGameState = gameLogic.immerStateManager.updateState(currentGameState, draft => {
                    draft.quests.princess.active.currentStep = index;
                });
                gameLogic.games.set(roomId, currentGameState);
                gameLogic.updateQuestProgress(currentGameState, 'princess', step.id);
            });
            
            // Получаем финальное состояние
            gameState = gameLogic.games.get(roomId);
            
            // Квест должен быть завершён
            expect(gameState.quests.princess.active).toBeNull();
            expect(gameState.quests.princess.completed.length).toBe(1);
            expect(gameState.stats.princess.inventory.length).toBeGreaterThan(inventoryBefore);
        });
    });

    describe('Quest Integration with NPCs', () => {
        test('должен начинать квест княжны через диалог с советником', () => {
            let gameState = gameLogic.games.get(roomId);
            gameState.stats.princess.location = 'throne_room';
            
            // Начинаем диалог с советником
            gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'princess');
            gameState = refreshGameState(gameLogic, roomId);
            
            // Выбираем квестовый диалог
            const result = gameLogic.processNPCDialogueChoice(roomId, 'alice', 'ask_about_relic', 'princess');
            gameState = refreshGameState(gameLogic, roomId);
            
            expect(result.success).toBe(true);
            expect(gameState.quests.princess.active).toBeDefined();
            expect(gameState.quests.princess.active.title).toContain('реликвия');
        });

        test('должен начинать квест помощницы через диалог с поваром', () => {
            let gameState = gameLogic.games.get(roomId);
            gameState.stats.helper.location = 'kitchen';
            gameState.stats.helper.outfit = 'common_dress';
            
            // Начинаем диалог с поваром
            gameLogic.processNPCInteraction(gameState, 'cook', 'helper');
            
            // Выбираем квестовый диалог
            const result = gameLogic.processNPCDialogueChoice(roomId, 'bob', 'ask_about_herbs', 'helper');
            gameState = refreshGameState(gameLogic, roomId);
            
            expect(result.success).toBe(true);
            expect(gameState.quests.helper.active).toBeDefined();
            expect(gameState.quests.helper.active.title).toContain('зелье');
        });

        test('должен показывать квест только соответствующему персонажу', () => {
            let gameState = gameLogic.games.get(roomId);
            
            // Начинаем квест для княжны
            gameLogic.startQuest(gameState, 'princess', 'princess_lost_relic');
            
            // Проверяем, что квест не появился у помощницы
            expect(gameState.quests.helper.active).toBeNull();
            expect(gameLogic.canStartQuest(gameState, 'helper', 'princess_lost_relic')).toBe(false);
        });
    });

    describe('Game Data Integration', () => {
        test('должен включать информацию о квестах в gameData', () => {
            let gameState = gameLogic.games.get(roomId);
            gameLogic.startQuest(gameState, 'princess', 'princess_lost_relic');
            
            const gameData = gameLogic.getGameData(roomId);
            
            expect(gameData.quests).toBeDefined();
            expect(gameData.quests.princess.active).toBeDefined();
            expect(gameData.quests.princess.active.title).toContain('реликвия');
            expect(gameData.quests.helper.active).toBeNull();
        });

        test('должен показывать количество завершённых квестов', () => {
            let gameState = gameLogic.games.get(roomId);
            gameLogic.startQuest(gameState, 'princess', 'princess_lost_relic');
            gameState = gameLogic.games.get(roomId);
            
            // Завершаем квест
            const completedState = gameLogic.completeQuest(gameState, 'princess');
            
            const gameData = gameLogic.getGameData(roomId);
            
            expect(gameData.quests.princess.completed).toBe(1);
            expect(gameData.quests.princess.active).toBeNull();
        });
    });

    describe('Quest Flow', () => {
        test('квест княжны: полный цикл поиска реликвии', () => {
            let gameState = gameLogic.games.get(roomId);
            
            // 1. Получаем квест от советника
            gameState.stats.princess.location = 'throne_room';
            gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'princess');
            gameLogic.processNPCDialogueChoice(roomId, 'alice', 'ask_about_relic', 'princess');
            gameState = refreshGameState(gameLogic, roomId);
            
            expect(gameState.quests.princess.active).toBeDefined();
            
            // 2. Проверяем, что квест активен и первый шаг выполнен
            let currentStep = gameLogic.getCurrentQuestStep(gameState, 'princess');
            expect(currentStep.location).toBe('library');
            
            // Идём в библиотеку
            gameState.stats.princess.location = 'library';
            
            // 3. Говорим с библиотекарем
            gameLogic.processNPCInteraction(gameState, 'librarian', 'princess');
            gameLogic.processNPCDialogueChoice(roomId, 'alice', 'start_quest', 'princess');
            
            // 4. Идём в секретный архив
            gameState.stats.princess.location = 'secret_archive';
            
            // 5. Возвращаемся к советнику
            gameState.stats.princess.location = 'throne_room';
            
            const quest = gameState.quests.princess.active;
            expect(quest.steps.length).toBe(4);
        });

        test('квест помощницы: полный цикл создания зелья', () => {
            let gameState = gameLogic.games.get(roomId);
            gameState.stats.helper.outfit = 'common_dress';
            
            // 1. Получаем квест от повара
            gameState.stats.helper.location = 'kitchen';
            gameLogic.processNPCInteraction(gameState, 'cook', 'helper');
            gameLogic.processNPCDialogueChoice(roomId, 'bob', 'ask_about_herbs', 'helper');
            gameState = refreshGameState(gameLogic, roomId);
            
            expect(gameState.quests.helper.active).toBeDefined();
            
            // 2. Проверяем, что квест активен и первый шаг выполнен
            let currentStep = gameLogic.getCurrentQuestStep(gameState, 'helper');
            expect(currentStep.location).toBe('garden');
            
            // Идём в сад
            gameState.stats.helper.location = 'garden';
            
            // 3. Говорим с травником
            gameLogic.processNPCInteraction(gameState, 'herbalist', 'helper');
            gameLogic.processNPCDialogueChoice(roomId, 'bob', 'start_quest', 'helper');
            
            // 4. Идём в теплицу
            gameState.stats.helper.location = 'greenhouse';
            
            // 5. Возвращаемся к повару
            gameState.stats.helper.location = 'kitchen';
            
            const quest = gameState.quests.helper.active;
            expect(quest.steps.length).toBe(4);
        });
    });
});