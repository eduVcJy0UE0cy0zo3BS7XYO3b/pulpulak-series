// Утилиты для тестирования

/**
 * Создает мок игрового состояния
 */
function createMockGameState(overrides = {}) {
    return {
        roomId: 'TEST123',
        players: {
            princess: { id: 'player1', name: 'Тестовая княжна' },
            helper: { id: 'player2', name: 'Тестовая помощница' }
        },
        currentScene: 'coop_awakening',
        turnOrder: 'princess',
        chapter: 1,
        location: 'princess_chamber',
        npcsPresent: [],
        stats: {
            princess: {
                outfit: 'nightgown',
                awareness: 0,
                loyalty: {},
                inventory: []
            },
            helper: {
                outfit: 'common_dress',
                awareness: 0,
                secrets_revealed: 0,
                inventory: ['translation_earrings', 'voice_medallion']
            }
        },
        ...overrides
    };
}

/**
 * Создает мок сцены
 */
function createMockScene(overrides = {}) {
    return {
        title: 'Тестовая сцена',
        text: 'Это тестовая сцена.',
        location: 'princess_chamber',
        choices: {
            princess: [
                {
                    id: 'princess_choice_1',
                    text: 'Выбор княжны 1',
                    description: 'Описание выбора',
                    resultText: 'Результат выбора'
                }
            ],
            helper: [
                {
                    id: 'helper_choice_1',
                    text: 'Выбор помощницы 1',
                    description: 'Описание выбора',
                    resultText: 'Результат выбора'
                }
            ]
        },
        ...overrides
    };
}

/**
 * Создает мок запроса на обмен одеждой
 */
function createMockOutfitRequest(overrides = {}) {
    return {
        id: 'request_123',
        roomId: 'TEST123',
        fromPlayerId: 'player1',
        fromCharacter: 'princess',
        targetPlayerId: 'player2',
        targetCharacter: 'helper',
        timestamp: Date.now(),
        ...overrides
    };
}

/**
 * Ожидает определенное количество миллисекунд
 */
function wait(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

/**
 * Проверяет, что объект имеет все обязательные поля
 */
function expectRequiredFields(obj, fields) {
    fields.forEach(field => {
        expect(obj).toHaveProperty(field);
        expect(obj[field]).toBeDefined();
    });
}

module.exports = {
    createMockGameState,
    createMockScene,
    createMockOutfitRequest,
    wait,
    expectRequiredFields
};