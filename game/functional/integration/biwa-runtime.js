/**
 * Интеграционный слой BiwaScheme с существующей системой
 * game/functional/integration/biwa-runtime.js
 */

const FunctionalGameEngine = require('./functional-engine');
const GameStateManager = require('../../gameStateManager');
const EventEmitter = require('events');

class BiwaGameRuntime extends EventEmitter {
    constructor() {
        super();
        this.functionalEngine = new FunctionalGameEngine();
        this.legacyStateManager = new GameStateManager();
        this.gameStates = new Map(); // roomId -> gameState
        this.isInitialized = false;
        this.migrationMode = 'parallel'; // 'parallel', 'functional-only', 'legacy-only'
    }

    /**
     * Инициализация runtime
     */
    async initialize() {
        try {
            console.log('[BiwaRuntime] Initializing...');
            
            // Инициализация функционального движка
            await this.functionalEngine.setupRuntime();
            
            // Настройка обработчиков событий
            this.setupEventHandlers();
            
            this.isInitialized = true;
            console.log('[BiwaRuntime] Initialized successfully');
        } catch (error) {
            console.error('[BiwaRuntime] Initialization failed:', error);
            throw error;
        }
    }

    /**
     * Настройка обработчиков событий
     */
    setupEventHandlers() {
        // Обработка событий от функционального движка
        this.functionalEngine.setEventEmitter(this);
        
        this.on('gameEvent', (event) => {
            this.handleFunctionalEvent(event);
        });
    }

    /**
     * Создание новой игры
     */
    async createGame(roomId, players) {
        if (!this.isInitialized) {
            await this.initialize();
        }

        try {
            let gameState;

            switch (this.migrationMode) {
                case 'functional-only':
                    gameState = await this.createFunctionalGame(roomId, players);
                    break;
                
                case 'legacy-only':
                    gameState = this.createLegacyGame(roomId, players);
                    break;
                
                case 'parallel':
                default:
                    // Создаем состояние в обеих системах для сравнения
                    const functionalState = await this.createFunctionalGame(roomId, players);
                    const legacyState = this.createLegacyGame(roomId, players);
                    
                    // Используем функциональное состояние как основное
                    gameState = functionalState;
                    
                    // Сохраняем legacy состояние для сравнения
                    this.storeLegacyState(roomId, legacyState);
                    break;
            }

            this.gameStates.set(roomId, gameState);
            
            console.log(`[BiwaRuntime] Game created for room ${roomId}`);
            return this.getGameData(roomId);
        } catch (error) {
            console.error(`[BiwaRuntime] Failed to create game for room ${roomId}:`, error);
            throw error;
        }
    }

    /**
     * Создание функциональной игры
     */
    async createFunctionalGame(roomId, players) {
        if (this.functionalEngine.isReady && this.functionalEngine.interpreter && 
            typeof this.functionalEngine.interpreter.run === 'function') {
            // Создание начального состояния через Scheme
            const initialState = await this.functionalEngine.interpreter.run(`
                (make-initial-state "${roomId}" 
                                  '((princess . "${players.princess?.id || 'player1'}") 
                                    (helper . "${players.helper?.id || 'player2'}")))
            `);

            return this.functionalEngine.schemeToJS(initialState);
        } else {
            // Fallback создание состояния
            console.log('[BiwaRuntime] Using fallback game creation');
            return this.functionalEngine.createFallbackState();
        }
    }

    /**
     * Создание legacy игры  
     */
    createLegacyGame(roomId, players) {
        return this.legacyStateManager.createInitialState(roomId, players);
    }

    /**
     * Обработка выбора игрока
     */
    async makeChoice(roomId, playerId, choiceId, character) {
        if (!this.gameStates.has(roomId)) {
            return { success: false, message: "Игра не найдена" };
        }

        try {
            const action = {
                type: 'choice',
                id: choiceId,
                playerId: playerId
            };

            switch (this.migrationMode) {
                case 'functional-only':
                    return await this.makeFunctionalChoice(roomId, action, character);
                
                case 'legacy-only':
                    return this.makeLegacyChoice(roomId, playerId, choiceId, character);
                
                case 'parallel':
                default:
                    // Выполняем в обеих системах и сравниваем результаты
                    const functionalResult = await this.makeFunctionalChoice(roomId, action, character);
                    const legacyResult = this.makeLegacyChoice(roomId, playerId, choiceId, character);
                    
                    // Логируем различия для анализа
                    this.compareSystems(functionalResult, legacyResult, 'makeChoice');
                    
                    // Возвращаем результат функциональной системы
                    return functionalResult;
            }
        } catch (error) {
            console.error(`[BiwaRuntime] Error making choice in room ${roomId}:`, error);
            return { success: false, message: error.message };
        }
    }

    /**
     * Функциональная обработка выбора
     */
    async makeFunctionalChoice(roomId, action, character) {
        // Валидация действия
        const isValid = await this.functionalEngine.validateAction(action, character);
        if (!isValid) {
            return { success: false, message: "Недопустимое действие" };
        }

        // Выполнение действия
        const result = await this.functionalEngine.processAction(action, character);
        
        if (result.success) {
            // Обновление сохраненного состояния
            this.gameStates.set(roomId, result.newState);
            
            // Обработка побочных эффектов
            this.processSideEffects(roomId, result.sideEffects);
            
            return {
                success: true,
                gameData: await this.getGameData(roomId),
                message: result.result?.message || "Действие выполнено"
            };
        } else {
            return {
                success: false,
                message: result.error
            };
        }
    }

    /**
     * Legacy обработка выбора (заглушка)
     */
    makeLegacyChoice(roomId, playerId, choiceId, character) {
        // Здесь будет вызов существующей системы
        // Пока возвращаем заглушку
        return { success: true, message: "Legacy choice processed" };
    }

    /**
     * Получение данных игры
     */
    async getGameData(roomId) {
        const gameState = this.gameStates.get(roomId);
        if (!gameState) {
            return null;
        }

        try {
            // Получение доступных действий для каждого персонажа
            const princessChoices = await this.functionalEngine.getAvailableActions('princess');
            const helperChoices = await this.functionalEngine.getAvailableActions('helper');

            // Формирование данных в ожидаемом формате
            return {
                roomId: roomId,
                scene: {
                    title: gameState.scene || 'Cooperative Adventure',
                    text: this.getSceneText(gameState)
                },
                choices: {
                    princess: princessChoices,
                    helper: helperChoices
                },
                stats: this.extractCharacterStats(gameState),
                currentTurn: gameState.turnOrder || 'princess',
                chapter: gameState.chapter || 1,
                locations: this.extractLocationData(gameState),
                quests: this.extractQuestData(gameState),
                npcDialogues: this.extractDialogueData(gameState)
            };
        } catch (error) {
            console.error(`[BiwaRuntime] Error getting game data for room ${roomId}:`, error);
            return null;
        }
    }

    /**
     * Обработка диалога с NPC
     */
    async processNPCDialogueChoice(roomId, playerId, choiceId, character) {
        const action = {
            type: 'dialogue',
            id: choiceId,
            playerId: playerId
        };

        return await this.makeFunctionalChoice(roomId, action, character);
    }

    /**
     * Закрытие диалога с NPC
     */
    async closeNPCDialogue(roomId, playerId) {
        const character = this.getCharacterByPlayerId(roomId, playerId);
        if (!character) {
            return { success: false, message: "Игрок не найден" };
        }

        const action = {
            type: 'close-dialogue',
            playerId: playerId
        };

        return await this.makeFunctionalChoice(roomId, action, character);
    }

    /**
     * Обработка побочных эффектов
     */
    processSideEffects(roomId, sideEffects) {
        for (const effect of sideEffects) {
            const [type, ...data] = effect;

            switch (type) {
                case 'log':
                    console.log(`[Game ${roomId}]`, ...data);
                    break;
                
                case 'event':
                    this.emit('gameEvent', { 
                        roomId, 
                        type: data[0], 
                        data: data[1] 
                    });
                    break;
                
                case 'error':
                    console.error(`[Game ${roomId} Error]`, ...data);
                    break;
                
                default:
                    console.warn(`[Game ${roomId} Unknown Effect]`, type, data);
            }
        }
    }

    /**
     * Обработка событий от функциональной системы
     */
    handleFunctionalEvent(event) {
        console.log('[BiwaRuntime] Functional event:', event);
        
        // Перенаправление событий клиентам
        this.emit('clientEvent', {
            type: event.type,
            data: event.data
        });
    }

    /**
     * Сравнение результатов систем
     */
    compareSystems(functionalResult, legacyResult, operation) {
        // Простое логирование различий
        if (JSON.stringify(functionalResult) !== JSON.stringify(legacyResult)) {
            console.warn(`[BiwaRuntime] System mismatch in ${operation}:`);
            console.warn('Functional:', functionalResult);
            console.warn('Legacy:', legacyResult);
        }
    }

    /**
     * Вспомогательные методы для извлечения данных
     */
    getSceneText(gameState) {
        return "Утренний свет пробивается сквозь окна..."; // Заглушка
    }

    extractCharacterStats(gameState) {
        if (!gameState.characters) return {};
        
        const stats = {};
        for (const [charId, charData] of Object.entries(gameState.characters)) {
            stats[charId] = {
                location: charData.location,
                outfit: charData.outfit,
                inventory: charData.inventory || [],
                ...charData.stats
            };
        }
        return stats;
    }

    extractLocationData(gameState) {
        return {
            princess: { name: "Princess Chamber", description: "Elegant royal bedroom" },
            helper: { name: "Princess Chamber", description: "Elegant royal bedroom" }
        };
    }

    extractQuestData(gameState) {
        return {
            princess: { active: null, completed: 0 },
            helper: { active: null, completed: 0 }
        };
    }

    extractDialogueData(gameState) {
        return {
            princess: null,
            helper: null
        };
    }

    getCharacterByPlayerId(roomId, playerId) {
        const gameState = this.gameStates.get(roomId);
        if (!gameState?.players) return null;

        for (const [character, player] of Object.entries(gameState.players)) {
            if (player?.id === playerId) {
                return character;
            }
        }
        return null;
    }

    storeLegacyState(roomId, legacyState) {
        // Сохранение legacy состояния для сравнения
        console.log(`[BiwaRuntime] Storing legacy state for room ${roomId}`);
    }

    /**
     * Удаление игры
     */
    removeGame(roomId) {
        this.gameStates.delete(roomId);
        console.log(`[BiwaRuntime] Game removed for room ${roomId}`);
    }

    /**
     * Получение статуса runtime
     */
    getStatus() {
        return {
            initialized: this.isInitialized,
            migrationMode: this.migrationMode,
            activeGames: this.gameStates.size,
            functionalEngineReady: !!this.functionalEngine
        };
    }

    /**
     * Переключение режима миграции
     */
    setMigrationMode(mode) {
        if (['parallel', 'functional-only', 'legacy-only'].includes(mode)) {
            this.migrationMode = mode;
            console.log(`[BiwaRuntime] Migration mode set to: ${mode}`);
        } else {
            throw new Error(`Invalid migration mode: ${mode}`);
        }
    }
}

module.exports = BiwaGameRuntime;