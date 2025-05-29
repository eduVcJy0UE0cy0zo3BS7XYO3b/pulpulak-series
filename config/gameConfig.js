module.exports = {
    // Конфигурация комнаты
    ROOM_ID_LENGTH: 6,
    ROOM_ID_CHARS: 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',
    MAX_PLAYERS_PER_ROOM: 2,
    
    // Таймауты
    CONNECTION_TIMEOUT: 30000, // 30 секунд
    OUTFIT_REQUEST_TIMEOUT: 30000, // 30 секунд
    
    // Начальные параметры игры
    INITIAL_STATS: {
        awareness: 0,
        loyalty: {
            villagers: 0
        },
        relationship: 50,
        inventory: []
    },
    
    // Конфигурация хода
    TURN_ORDER: {
        FIRST: 'princess',
        SWITCHING_ENABLED: true
    },
    
    // Конфигурация квестов
    QUEST_CONFIG: {
        MAX_ACTIVE_QUESTS: 1,
        ALLOW_QUEST_SHARING: false
    },
    
    // Лимиты
    MAX_INVENTORY_SIZE: 20,
    MAX_DIALOGUE_LENGTH: 1000,
    MAX_SCENE_TEXT_LENGTH: 5000,
    
    // Игровые константы
    RELATIONSHIP_THRESHOLDS: {
        VERY_LOW: 20,
        LOW: 40,
        NEUTRAL: 60,
        HIGH: 80,
        VERY_HIGH: 100
    },
    
    // Логирование
    DEBUG_MODE: process.env.NODE_ENV !== 'production',
    LOG_SOCKET_EVENTS: process.env.LOG_SOCKETS === 'true'
};