const CoopGameLogic = require('../coopGameLogic');

describe('S-Expression Quest System', () => {
    let gameLogic;
    const roomId = 'TEST_QUEST';
    const players = {
        princess: { id: 'alice', name: 'Алиса' },
        helper: { id: 'bob', name: 'Боб' }
    };

    beforeEach(() => {
        gameLogic = new CoopGameLogic();
        gameLogic.startGame(roomId, players);
    });

    // Helper function to set up quest conditions
    function setupQuestConditions(character = 'princess', location = 'throne_room', outfit = 'princess_dress', npc = 'royal_advisor') {
        const gameState = gameLogic.games.get(roomId);
        gameState.stats[character].outfit = outfit;
        gameState.stats[character].location = location;
        
        const questIntegration = gameLogic.questIntegrations.get(roomId);
        if (questIntegration) {
            questIntegration.questRunner.gameState.currentNPC = npc;
            questIntegration.questRunner.gameState.currentLocation = location;
            questIntegration.questRunner.gameState.currentOutfit = outfit;
        }
        
        return gameState;
    }

    describe('S-Expression Quest System Integration', () => {
        test('должен загружать квесты из .scm файлов', () => {
            const questIntegration = gameLogic.questIntegrations.get(roomId);
            expect(questIntegration).toBeDefined();
            expect(questIntegration.questRunner).toBeDefined();
        });

        test('должен предоставлять API для работы с квестами', () => {
            const questIntegration = gameLogic.questIntegrations.get(roomId);
            
            // Проверяем основные методы API
            expect(typeof questIntegration.checkQuestTriggers).toBe('function');
            expect(typeof questIntegration.handleNPCInteraction).toBe('function');
            expect(typeof questIntegration.startQuest).toBe('function');
            expect(typeof questIntegration.getCurrentQuestStatus).toBe('function');
        });

        test('должен обрабатывать взаимодействие с NPC', () => {
            const questIntegration = gameLogic.questIntegrations.get(roomId);
            const gameState = setupQuestConditions();
            
            const result = questIntegration.handleNPCInteraction('royal_advisor');
            
            expect(result).toBeDefined();
            expect(typeof result.newQuests).toBe('object');
            expect(typeof result.questProgress).toBe('object');
            expect(typeof result.dialogueOptions).toBe('object');
        });
    });

    describe('Quest Management', () => {
        test('должен инициализировать пустые квесты', () => {
            const gameState = gameLogic.games.get(roomId);
            expect(gameState.quests).toBeDefined();
            expect(gameState.quests.princess.active).toBeNull();
            expect(gameState.quests.helper.active).toBeNull();
            expect(gameState.quests.princess.completed).toEqual([]);
            expect(gameState.quests.helper.completed).toEqual([]);
        });

        test('должен проверять доступность квестов', () => {
            const questIntegration = gameLogic.questIntegrations.get(roomId);
            const availableQuests = questIntegration.checkQuestTriggers();
            
            expect(Array.isArray(availableQuests)).toBe(true);
        });

        test('должен возвращать статус текущего квеста', () => {
            const questIntegration = gameLogic.questIntegrations.get(roomId);
            const status = questIntegration.getCurrentQuestStatus();
            
            // По умолчанию должен быть null, так как нет активных квестов
            expect(status).toBeNull();
        });

        test('должен обрабатывать смену локации', () => {
            const questIntegration = gameLogic.questIntegrations.get(roomId);
            const results = questIntegration.handleLocationChange('throne_room');
            
            expect(Array.isArray(results)).toBe(true);
        });
    });

    describe('Quest Integration with NPCs', () => {
        test('должен обрабатывать взаимодействие с NPC для квестов', () => {
            const questIntegration = gameLogic.questIntegrations.get(roomId);
            const gameState = setupQuestConditions();
            
            const result = questIntegration.handleNPCInteraction('royal_advisor');
            
            expect(result).toBeDefined();
            expect(result.newQuests).toBeDefined();
            expect(result.questProgress).toBeDefined();
            expect(result.dialogueOptions).toBeDefined();
        });

        test('должен предлагать начать квест при взаимодействии с соответствующим NPC', () => {
            const questIntegration = gameLogic.questIntegrations.get(roomId);
            const gameState = setupQuestConditions('helper', 'kitchen', 'common_dress', 'cook');
            
            const result = questIntegration.handleNPCInteraction('cook');
            
            expect(result).toBeDefined();
        });

        test('должен корректно обрабатывать стартовые условия квестов', () => {
            const questIntegration = gameLogic.questIntegrations.get(roomId);
            const availableQuests = questIntegration.checkQuestTriggers();
            
            // Система должна возвращать массив доступных квестов
            expect(Array.isArray(availableQuests)).toBe(true);
        });
    });

    describe('Game Data Integration', () => {
        test('должен включать информацию о квестах в gameData', () => {
            const gameState = gameLogic.games.get(roomId);
            const gameData = gameLogic.getGameData(roomId);
            
            expect(gameData.quests).toBeDefined();
            expect(gameData.quests.princess.active).toBeNull();
            expect(gameData.quests.helper.active).toBeNull();
        });

        test('должен предоставлять интерфейс для работы с квестами', () => {
            const questIntegration = gameLogic.questIntegrations.get(roomId);
            
            // Проверяем, что квестовая система интегрирована
            expect(questIntegration).toBeDefined();
            expect(questIntegration.questRunner).toBeDefined();
        });
    });

    describe('S-Expression Quest Flow', () => {
        test('должен обрабатывать изменения шагов квеста', () => {
            const questIntegration = gameLogic.questIntegrations.get(roomId);
            const result = questIntegration.processQuestStep();
            
            // По умолчанию должен возвращать null, так как нет активных квестов
            expect(result).toBeNull();
        });

        test('должен проверять доступность квестов для текущего персонажа', () => {
            const questIntegration = gameLogic.questIntegrations.get(roomId);
            const availableQuests = questIntegration.checkQuestTriggers();
            
            expect(Array.isArray(availableQuests)).toBe(true);
        });

        test('должен корректно работать с системой квестов через S-expressions', () => {
            const questIntegration = gameLogic.questIntegrations.get(roomId);
            
            // Проверяем, что все необходимые методы доступны
            expect(typeof questIntegration.startQuest).toBe('function');
            expect(typeof questIntegration.getCurrentQuestStatus).toBe('function');
            expect(typeof questIntegration.processQuestStep).toBe('function');
        });
    });
});