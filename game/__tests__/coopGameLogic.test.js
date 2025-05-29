const CoopGameLogic = require('../coopGameLogic');
const CoopStoryData = require('../coopStoryData');

// Mock для CoopStoryData
jest.mock('../coopStoryData', () => ({
    getScene: jest.fn()
}));

describe('CoopGameLogic', () => {
    let gameLogic;
    let roomId;
    let players;

    beforeEach(() => {
        gameLogic = new CoopGameLogic();
        roomId = 'TEST123';
        players = {
            princess: { id: 'player1', name: 'Алиса' },
            helper: { id: 'player2', name: 'Боб' }
        };

        // Сброс моков
        jest.clearAllMocks();
        
        // Настройка базового мока для getScene
        CoopStoryData.getScene.mockReturnValue({
            title: 'Тестовая сцена',
            text: 'Тестовый текст',
            location: 'princess_chamber',
            choices: {
                princess: [
                    { id: 'test_choice_1', text: 'Выбор 1' }
                ],
                helper: [
                    { id: 'test_choice_2', text: 'Выбор 2' }
                ]
            }
        });
    });

    describe('startGame', () => {
        test('должна создать новую игру с правильным начальным состоянием', () => {
            const gameData = gameLogic.startGame(roomId, players);

            expect(gameData).toBeDefined();
            expect(gameData.roomId).toBe(roomId);
            expect(gameData.players).toEqual(players);
            expect(gameData.currentTurn).toBe('princess');
            expect(gameData.chapter).toBe(1);
            expect(gameData.location).toBe('princess_chamber');
            expect(gameData.npcsPresent).toEqual([]);
        });

        test('должна установить правильные начальные наряды', () => {
            const gameData = gameLogic.startGame(roomId, players);

            expect(gameData.stats.princess.outfit).toBe('nightgown');
            expect(gameData.stats.helper.outfit).toBe('common_dress');
        });

        test('должна инициализировать инвентари персонажей', () => {
            const gameData = gameLogic.startGame(roomId, players);

            expect(gameData.stats.princess.inventory).toEqual([]);
            expect(gameData.stats.helper.inventory).toContain('translation_earrings');
            expect(gameData.stats.helper.inventory).toContain('voice_medallion');
        });
    });

    describe('Outfit Swap System', () => {
        beforeEach(() => {
            // Создаем игру перед каждым тестом
            gameLogic.startGame(roomId, players);
        });

        describe('createOutfitSwapRequest', () => {
            test('должна создать запрос на обмен одеждой', () => {
                const result = gameLogic.createOutfitSwapRequest(roomId, 'player1', 'princess');

                expect(result.success).toBe(true);
                expect(result.request).toBeDefined();
                expect(result.request.fromPlayerId).toBe('player1');
                expect(result.request.fromCharacter).toBe('princess');
                expect(result.request.targetCharacter).toBe('helper');
                expect(result.request.targetPlayerId).toBe('player2');
            });

            test('не должна создавать запрос если есть NPC', () => {
                // Устанавливаем NPC в локации
                const gameState = gameLogic.games.get(roomId);
                gameState.npcsPresent = ['Стражник'];

                const result = gameLogic.createOutfitSwapRequest(roomId, 'player1', 'princess');

                expect(result.success).toBe(false);
                expect(result.message).toContain('Нельзя переодеваться при посторонних');
            });

            test('не должна создавать второй запрос пока первый активен', () => {
                // Создаем первый запрос
                gameLogic.createOutfitSwapRequest(roomId, 'player1', 'princess');
                
                // Пытаемся создать второй
                const result = gameLogic.createOutfitSwapRequest(roomId, 'player2', 'helper');

                expect(result.success).toBe(false);
                expect(result.message).toContain('Уже есть активный запрос');
            });
        });

        describe('respondToOutfitSwapRequest', () => {
            test('должна принять запрос и поменять одежду', () => {
                // Создаем запрос
                gameLogic.createOutfitSwapRequest(roomId, 'player1', 'princess');
                
                // Принимаем его
                const result = gameLogic.respondToOutfitSwapRequest(roomId, 'player2', true);

                expect(result.success).toBe(true);
                expect(result.accepted).toBe(true);
                
                // Проверяем, что одежда поменялась
                const gameData = gameLogic.getGameData(roomId);
                expect(gameData.stats.princess.outfit).toBe('common_dress');
                expect(gameData.stats.helper.outfit).toBe('nightgown');
            });

            test('должна отклонить запрос', () => {
                // Создаем запрос
                gameLogic.createOutfitSwapRequest(roomId, 'player1', 'princess');
                
                // Отклоняем его
                const result = gameLogic.respondToOutfitSwapRequest(roomId, 'player2', false);

                expect(result.success).toBe(true);
                expect(result.declined).toBe(true);
                
                // Проверяем, что одежда НЕ поменялась
                const gameData = gameLogic.getGameData(roomId);
                expect(gameData.stats.princess.outfit).toBe('nightgown');
                expect(gameData.stats.helper.outfit).toBe('common_dress');
            });

            test('не должна позволить ответить не тому игроку', () => {
                // Создаем запрос от player1
                gameLogic.createOutfitSwapRequest(roomId, 'player1', 'princess');
                
                // Пытается ответить player1 (сам себе)
                const result = gameLogic.respondToOutfitSwapRequest(roomId, 'player1', true);

                expect(result.success).toBe(false);
                expect(result.message).toContain('Этот запрос не для вас');
            });
        });

        describe('canSwitchOutfits', () => {
            test('должна разрешать смену одежды когда нет NPC', () => {
                const gameState = gameLogic.games.get(roomId);
                gameState.npcsPresent = [];

                const canSwitch = gameLogic.canSwitchOutfits(gameState);
                expect(canSwitch).toBe(true);
            });

            test('не должна разрешать смену одежды когда есть NPC', () => {
                const gameState = gameLogic.games.get(roomId);
                gameState.npcsPresent = ['Король', 'Стражник'];

                const canSwitch = gameLogic.canSwitchOutfits(gameState);
                expect(canSwitch).toBe(false);
            });
        });
    });

    describe('Game Flow', () => {
        beforeEach(() => {
            gameLogic.startGame(roomId, players);
        });

        describe('makeChoice', () => {
            test('должна обработать обычный выбор', () => {
                const result = gameLogic.makeChoice(roomId, 'player1', 'test_choice_1', 'princess');

                expect(result.success).toBe(true);
                expect(result.gameData).toBeDefined();
            });

            test('не должна позволить сделать выбор за чужого персонажа', () => {
                const result = gameLogic.makeChoice(roomId, 'player1', 'test_choice_2', 'helper');

                expect(result.success).toBe(false);
                expect(result.message).toContain('управляете другим персонажем');
            });

            test('должна сменить очередь хода после выбора', () => {
                const beforeTurn = gameLogic.games.get(roomId).turnOrder;
                gameLogic.makeChoice(roomId, 'player1', 'test_choice_1', 'princess');
                const afterTurn = gameLogic.games.get(roomId).turnOrder;

                expect(beforeTurn).toBe('princess');
                expect(afterTurn).toBe('helper');
            });
        });

        describe('processChoice', () => {
            test('не должна обрабатывать запрос на смену одежды как обычный выбор', () => {
                const gameState = gameLogic.games.get(roomId);
                const result = gameLogic.processChoice(gameState, 'request_outfit_swap', 'princess');

                expect(result.success).toBe(false);
                expect(result.message).toContain('отдельный обработчик');
            });

            test('должна применить эффекты выбора', () => {
                // Мокаем выбор с эффектами
                CoopStoryData.getScene.mockReturnValue({
                    title: 'Тест',
                    text: 'Текст',
                    choices: {
                        princess: [{
                            id: 'magic_choice',
                            text: 'Использовать магию',
                            effects: {
                                awareness: 5,
                                flag: 'magic_used'
                            }
                        }]
                    }
                });

                const gameState = gameLogic.games.get(roomId);
                gameLogic.processChoice(gameState, 'magic_choice', 'princess');

                expect(gameState.stats.princess.awareness).toBe(5);
            });
        });

        describe('getChoicesForCharacter', () => {
            test('должна показывать выборы только для текущего хода', () => {
                const gameState = gameLogic.games.get(roomId);
                const sceneData = CoopStoryData.getScene();

                // Ход княжны
                gameState.turnOrder = 'princess';
                const princessChoices = gameLogic.getChoicesForCharacter(gameState, 'princess', sceneData);
                const helperChoices = gameLogic.getChoicesForCharacter(gameState, 'helper', sceneData);

                expect(princessChoices.length).toBeGreaterThan(0);
                expect(helperChoices.filter(c => !c.isOutfitRequest).length).toBe(0);
            });

            test('должна добавлять кнопку смены одежды когда нет NPC', () => {
                const gameState = gameLogic.games.get(roomId);
                gameState.npcsPresent = [];
                const sceneData = CoopStoryData.getScene();

                const choices = gameLogic.getChoicesForCharacter(gameState, 'princess', sceneData);
                const outfitChoice = choices.find(c => c.id === 'request_outfit_swap');

                expect(outfitChoice).toBeDefined();
                expect(outfitChoice.isOutfitRequest).toBe(true);
            });
        });
    });

    describe('Game State Management', () => {
        test('должна удалить игру', () => {
            gameLogic.startGame(roomId, players);
            expect(gameLogic.games.has(roomId)).toBe(true);

            gameLogic.removeGame(roomId);
            expect(gameLogic.games.has(roomId)).toBe(false);
            expect(gameLogic.outfitRequests.has(roomId)).toBe(false);
        });

        test('должна генерировать уникальные ID для запросов', () => {
            const id1 = gameLogic.generateRequestId();
            const id2 = gameLogic.generateRequestId();

            expect(id1).not.toBe(id2);
            expect(id1.length).toBeGreaterThan(5);
        });

        test('должна правильно определять NPC для локаций', () => {
            const throneRoomNpcs = gameLogic.getNPCsForLocation('throne_room');
            const chamberNpcs = gameLogic.getNPCsForLocation('princess_chamber');

            expect(throneRoomNpcs).toContain('Король');
            expect(throneRoomNpcs).toContain('Королева');
            expect(chamberNpcs).toEqual([]);
        });
    });
});