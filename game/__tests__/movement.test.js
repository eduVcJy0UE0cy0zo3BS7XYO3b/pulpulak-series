const CoopGameLogic = require('../coopGameLogic');
const LocationData = require('../../games/pulpulak/data/locationData');
const MockGameConfig = require('./mocks/MockGameConfig');

describe('Movement System', () => {
    let gameLogic;
    let gameConfig;
    const roomId = 'TEST_ROOM';
    const players = {
        princess: { id: 'alice', name: 'Алиса' },
        helper: { id: 'bob', name: 'Боб' }
    };

    beforeEach(() => {
        gameConfig = new MockGameConfig();
        gameLogic = new CoopGameLogic(gameConfig);
        gameLogic.startGame(roomId, players);
    });

    describe('Movement choices generation', () => {
        test('должны генерироваться выборы перемещения', () => {
            const gameData = gameLogic.getGameData(roomId);
            
            // Проверяем выборы княжны
            const princessChoices = gameData.choices.princess;
            const movementChoices = princessChoices.filter(c => c.isMovement);
            
            expect(movementChoices.length).toBeGreaterThan(0);
            
            // Проверяем, что есть выборы для перехода в коридор и личные покои
            const targetLocations = movementChoices.map(c => c.targetLocation);
            expect(targetLocations).toContain('corridor_upper');
            expect(targetLocations).toContain('private_quarters');
        });

        test('выборы перемещения должны быть доступны обоим игрокам', () => {
            const gameData = gameLogic.getGameData(roomId);
            
            const princessMovements = gameData.choices.princess.filter(c => c.isMovement);
            const helperMovements = gameData.choices.helper.filter(c => c.isMovement);
            
            expect(princessMovements.length).toBeGreaterThan(0);
            expect(helperMovements.length).toBeGreaterThan(0);
            expect(princessMovements.length).toBe(helperMovements.length);
        });
    });

    describe('processMovement', () => {
        test('должен успешно перемещать персонажа', () => {
            const gameState = gameLogic.games.get(roomId);
            
            const result = gameLogic.processMovement(gameState, 'corridor_upper', 'princess');
            
            expect(result.success).toBe(true);
            expect(result.gameState.stats.princess.location).toBe('corridor_upper');
            expect(result.message).toContain('Княжна переместилась');
        });

        test('не должен позволить переместиться в недоступную локацию', () => {
            const gameState = gameLogic.games.get(roomId);
            
            // Пытаемся переместиться в тронный зал напрямую из спальни
            const result = gameLogic.processMovement(gameState, 'throne_room', 'princess');
            
            expect(result.success).toBe(false);
            expect(result.message).toContain('не можете попасть туда отсюда');
            expect(gameState.stats.princess.location).toBe('princess_chamber');
        });

        test('должен отменять активные запросы при перемещении', () => {
            // Создаем запрос на обмен одеждой
            gameLogic.createOutfitSwapRequest(roomId, 'alice', 'princess');
            expect(gameLogic.outfitRequests.has(roomId)).toBe(true);
            
            // Перемещаемся
            const gameState = gameLogic.games.get(roomId);
            gameLogic.processMovement(gameState, 'corridor_upper', 'princess');
            
            // Запрос должен быть отменен
            expect(gameLogic.outfitRequests.has(roomId)).toBe(false);
        });

        test('не должен менять очередь хода при перемещении', () => {
            const gameState = gameLogic.games.get(roomId);
            const turnBefore = gameState.turnOrder;
            
            gameLogic.processMovement(gameState, 'corridor_upper', 'princess');
            
            expect(gameState.turnOrder).toBe(turnBefore);
        });
    });

    describe('makeChoice with movement', () => {
        test('должен обработать выбор перемещения через makeChoice', () => {
            const result = gameLogic.makeChoice(roomId, 'alice', 'move_to_corridor_upper', 'princess');
            
            expect(result.success).toBe(true);
            expect(result.gameData.stats.princess.location).toBe('corridor_upper');
        });

        test('любой игрок может двигаться в любой момент', () => {
            const gameData = gameLogic.getGameData(roomId);
            
            // Сейчас ход княжны, но помощница тоже может двигаться
            expect(gameData.currentTurn).toBe('princess');
            
            const result = gameLogic.makeChoice(roomId, 'bob', 'move_to_corridor_upper', 'helper');
            expect(result.success).toBe(true);
        });
    });

    describe('Outfit restrictions based on location', () => {
        test('не должен позволить менять одежду в публичных местах', () => {
            // Перемещаемся в тронный зал
            let gameState = gameLogic.games.get(roomId);
            gameState = gameLogic.immerStateManager.updateState(gameState, draft => {
                draft.stats.princess.location = 'throne_room';
                draft.stats.helper.location = 'throne_room';
            });
            gameLogic.games.set(roomId, gameState);
            
            // Проверяем, что нельзя переодеваться
            const canSwitch = gameLogic.canSwitchOutfits(gameState, 'princess');
            expect(canSwitch).toBe(false);
            
            // Проверяем, что кнопка обмена не появляется
            const gameData = gameLogic.getGameData(roomId);
            const outfitChoices = gameData.choices.princess.filter(c => c.id === 'request_outfit_swap');
            expect(outfitChoices.length).toBe(0);
        });

        test('должен позволить менять одежду в уединенных местах', () => {
            // Перемещаемся в тайный сад
            const gameState = gameLogic.games.get(roomId);
            gameState.stats.princess.location = 'secret_garden';
            gameState.stats.helper.location = 'secret_garden';
            
            // Проверяем, что можно переодеваться
            const canSwitch = gameLogic.canSwitchOutfits(gameState, 'princess');
            expect(canSwitch).toBe(true);
            
            // Проверяем, что кнопка обмена появляется
            const gameData = gameLogic.getGameData(roomId);
            const outfitChoices = gameData.choices.princess.filter(c => c.id === 'request_outfit_swap');
            expect(outfitChoices.length).toBe(1);
        });
    });

    describe('Complex movement scenarios', () => {
        test('должен позволить перемещаться через несколько локаций', () => {
            // Спальня -> Коридор -> Тронный зал -> Большой зал
            const moves = [
                { to: 'corridor_upper', from: 'princess_chamber' },
                { to: 'throne_room', from: 'corridor_upper' },
                { to: 'great_hall', from: 'throne_room' }
            ];
            
            moves.forEach(move => {
                const result = gameLogic.makeChoice(roomId, 'alice', `move_to_${move.to}`, 'princess');
                expect(result.success).toBe(true);
                expect(result.gameData.stats.princess.location).toBe(move.to);
            });
        });

        test('информация о локации должна обновляться при перемещении', () => {
            // Перемещаемся в кухню через правильный путь
            let result;
            
            // Из спальни в коридор
            result = gameLogic.makeChoice(roomId, 'alice', 'move_to_corridor_upper', 'princess');
            expect(result.success).toBe(true);
            
            // Из коридора на лестницу
            result = gameLogic.makeChoice(roomId, 'alice', 'move_to_stairs_main', 'princess');
            expect(result.success).toBe(true);
            
            // С лестницы в большой зал
            result = gameLogic.makeChoice(roomId, 'alice', 'move_to_great_hall', 'princess');
            expect(result.success).toBe(true);
            
            // Из большого зала в кухню
            result = gameLogic.makeChoice(roomId, 'alice', 'move_to_kitchen', 'princess');
            expect(result.success).toBe(true);
            
            const gameData = gameLogic.getGameData(roomId);
            expect(gameData.locations.princess.name).toBe('Кухня');
            expect(gameData.locations.princess.icon).toBe('🍳');
            expect(gameData.locations.princess.canChangeOutfit).toBe(false);
        });
    });
});