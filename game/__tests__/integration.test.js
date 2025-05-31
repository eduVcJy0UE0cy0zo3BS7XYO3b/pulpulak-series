const CoopGameLogic = require('../coopGameLogic');
const CoopStoryData = require('../data/coopStoryDataSCM');
const { wait } = require('./testUtils');

describe('Game Integration Tests', () => {
    let gameLogic;
    const roomId = 'TEST_ROOM';
    const players = {
        princess: { id: 'alice', name: 'Алиса' },
        helper: { id: 'bob', name: 'Боб' }
    };

    beforeEach(() => {
        gameLogic = new CoopGameLogic();
    });

    describe('Полный игровой сценарий', () => {
        test('должен пройти типичный игровой цикл с обменом одеждой', async () => {
            // 1. Начинаем игру
            const initialData = gameLogic.startGame(roomId, players);
            expect(initialData.scene.title).toBe('Утреннее пробуждение');
            expect(initialData.stats.princess.outfit).toBe('princess_dress');
            expect(initialData.stats.helper.outfit).toBe('common_dress');

            // 2. Княжна делает первый выбор
            const firstChoice = initialData.choices.princess[0];
            const afterFirstChoice = gameLogic.makeChoice(
                roomId, 
                'alice', 
                firstChoice.id, 
                'princess'
            );
            expect(afterFirstChoice.success).toBe(true);
            expect(afterFirstChoice.gameData.currentTurn).toBe('helper');

            // 3. Помощница делает выбор
            const helperChoice = afterFirstChoice.gameData.choices.helper[0];
            const afterHelperChoice = gameLogic.makeChoice(
                roomId, 
                'bob', 
                helperChoice.id, 
                'helper'
            );
            expect(afterHelperChoice.success).toBe(true);

            // 4. Создаем запрос на обмен одеждой
            const swapRequest = gameLogic.createOutfitSwapRequest(roomId, 'alice', 'princess');
            expect(swapRequest.success).toBe(true);
            
            // 5. Проверяем, что кнопка обмена скрылась
            const dataWithRequest = gameLogic.getGameData(roomId);
            const princessChoices = dataWithRequest.choices.princess;
            const swapButton = princessChoices.find(c => c.id === 'request_outfit_swap');
            expect(swapButton).toBeUndefined();

            // 6. Принимаем запрос
            const swapResponse = gameLogic.respondToOutfitSwapRequest(roomId, 'bob', true);
            expect(swapResponse.success).toBe(true);
            expect(swapResponse.accepted).toBe(true);

            // 7. Проверяем, что одежда поменялась
            const afterSwap = gameLogic.getGameData(roomId);
            expect(afterSwap.stats.princess.outfit).toBe('common_dress');
            expect(afterSwap.stats.helper.outfit).toBe('princess_dress');

            // 8. Проверяем, что кнопка обмена снова появилась
            const swapButtonAfter = afterSwap.choices.princess.find(c => c.id === 'request_outfit_swap');
            expect(swapButtonAfter).toBeDefined();
        });

        test('должен корректно обработать отклонение запроса на обмен', () => {
            // Начинаем игру
            gameLogic.startGame(roomId, players);

            // Создаем запрос
            const swapRequest = gameLogic.createOutfitSwapRequest(roomId, 'alice', 'princess');
            expect(swapRequest.success).toBe(true);

            // Отклоняем запрос
            const swapResponse = gameLogic.respondToOutfitSwapRequest(roomId, 'bob', false);
            expect(swapResponse.success).toBe(true);
            expect(swapResponse.declined).toBe(true);

            // Проверяем, что одежда НЕ поменялась
            const afterDecline = gameLogic.getGameData(roomId);
            expect(afterDecline.stats.princess.outfit).toBe('princess_dress');
            expect(afterDecline.stats.helper.outfit).toBe('common_dress');
        });
    });

    describe('Граничные случаи', () => {
        test('не должен позволить обмен одеждой при переходе в локацию с NPC', () => {
            gameLogic.startGame(roomId, players);
            
            // Мокаем переход в тронный зал для обеих персонажей
            const gameState = gameLogic.lobbyLogic.getGameState(roomId);
            gameState.stats.princess.location = 'throne_room';
            gameState.stats.princess.npcsPresent = gameLogic.getNPCsForLocation('throne_room');
            gameState.stats.helper.location = 'throne_room';
            gameState.stats.helper.npcsPresent = gameLogic.getNPCsForLocation('throne_room');

            // Пытаемся создать запрос
            const swapRequest = gameLogic.createOutfitSwapRequest(roomId, 'alice', 'princess');
            expect(swapRequest.success).toBe(false);
            expect(swapRequest.message).toContain('при посторонних');
        });

        test('должен обработать одновременные запросы корректно', () => {
            gameLogic.startGame(roomId, players);

            // Первый запрос успешен
            const request1 = gameLogic.createOutfitSwapRequest(roomId, 'alice', 'princess');
            expect(request1.success).toBe(true);

            // Второй запрос должен быть отклонен
            const request2 = gameLogic.createOutfitSwapRequest(roomId, 'bob', 'helper');
            expect(request2.success).toBe(false);
            expect(request2.message).toContain('активный запрос');
        });

        test('должен корректно обработать попытку выбора не в свой ход', () => {
            const gameData = gameLogic.startGame(roomId, players);
            
            // Ход княжны, но пытается выбрать помощница
            // Берем первый выбор из начальной сцены
            const sceneData = CoopStoryData.getScene('coop_awakening');
            const helperChoice = sceneData.choices.helper[0];
            const result = gameLogic.makeChoice(roomId, 'bob', helperChoice.id, 'helper');
            
            expect(result.success).toBe(false);
            expect(result.message).toContain('не ваш ход');
        });
    });

    describe('Сохранение состояния между действиями', () => {
        test('должен сохранять эффекты выборов', () => {
            gameLogic.startGame(roomId, players);
            const gameState = gameLogic.lobbyLogic.getGameState(roomId);

            // Имитируем выбор с эффектом awareness
            gameState.stats.princess.awareness = 5;
            
            // Делаем другие действия
            gameLogic.createOutfitSwapRequest(roomId, 'alice', 'princess');
            gameLogic.respondToOutfitSwapRequest(roomId, 'bob', false);

            // Проверяем, что awareness сохранился
            const currentData = gameLogic.getGameData(roomId);
            expect(currentData.stats.princess.awareness).toBe(5);
        });

        test('должен правильно отслеживать очередность ходов', () => {
            gameLogic.startGame(roomId, players);
            const turns = [];

            // Делаем несколько ходов и записываем очередность
            for (let i = 0; i < 4; i++) {
                const gameData = gameLogic.getGameData(roomId);
                turns.push(gameData.currentTurn);
                
                const currentPlayer = gameData.currentTurn;
                const playerId = currentPlayer === 'princess' ? 'alice' : 'bob';
                const choices = gameData.choices[currentPlayer];
                
                // Фильтруем только основные выборы сцены (не движение и не обмен одеждой)
                const storyChoices = choices.filter(c => !c.isMovement && !c.isOutfitRequest && !c.isNPCInteraction);
                
                if (storyChoices && storyChoices.length > 0) {
                    gameLogic.makeChoice(roomId, playerId, storyChoices[0].id, currentPlayer);
                } else {
                    // Если нет основных выборов, прерываем цикл
                    break;
                }
            }

            // Проверяем что было минимум 2 хода со сменой очереди
            expect(turns.length).toBeGreaterThanOrEqual(2);
            if (turns.length >= 2) {
                expect(turns[0]).toBe('princess');
                expect(turns[1]).toBe('helper');
            }
        });
    });
});