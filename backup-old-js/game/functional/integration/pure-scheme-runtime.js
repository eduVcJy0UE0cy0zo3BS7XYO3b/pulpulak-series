/**
 * Чистый Scheme runtime для игровой системы
 * game/functional/integration/pure-scheme-runtime.js
 */

const PureSchemeGameEngine = require('./pure-scheme-simple');
const EventEmitter = require('events');

class PureSchemeGameRuntime extends EventEmitter {
    constructor() {
        super();
        this.schemeEngine = new PureSchemeGameEngine();
        this.gameStates = new Map(); // roomId -> gameState
        this.isInitialized = false;
    }

    /**
     * Инициализация runtime
     */
    async initialize() {
        try {
            console.log('[PureSchemeRuntime] Initializing...');
            
            // Инициализация Scheme движка
            await this.schemeEngine.initialize();
            
            // Настройка обработчиков событий
            this.setupEventHandlers();
            
            this.isInitialized = true;
            console.log('[PureSchemeRuntime] Initialized successfully');
        } catch (error) {
            console.error('[PureSchemeRuntime] Initialization failed:', error);
            throw error;
        }
    }

    /**
     * Настройка обработчиков событий
     */
    setupEventHandlers() {
        // Перенаправление событий от Scheme движка
        this.schemeEngine.setEventEmitter(this);
        
        this.on('gameEvent', (event) => {
            this.handleSchemeEvent(event);
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
            console.log(`[PureSchemeRuntime] Creating game for room ${roomId}`);
            
            // Создание игрового состояния через Scheme
            const gameState = await this.createSchemeGame(roomId, players);
            
            // Сохранение состояния
            this.gameStates.set(roomId, gameState);
            
            console.log(`[PureSchemeRuntime] Game created for room ${roomId}`);
            return this.getGameData(roomId);
        } catch (error) {
            console.error(`[PureSchemeRuntime] Failed to create game for room ${roomId}:`, error);
            throw error;
        }
    }

    /**
     * Создание игры через Scheme
     */
    async createSchemeGame(roomId, players) {
        // Вызываем Scheme функцию создания игры
        const result = await this.schemeEngine.biwa.evaluate(`
            (let ((room-id "${roomId}")
                  (players '((princess . "${players.princess?.id || 'player1'}") 
                            (helper . "${players.helper?.id || 'player2'}"))))
              (make-initial-state room-id players))
        `);

        // Простой движок возвращает уже JavaScript объекты
        return result;
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

            // Обработка через чистый Scheme
            const result = await this.processSchemeAction(roomId, action, character);
            
            if (result.success) {
                // Обновление сохраненного состояния
                this.gameStates.set(roomId, result.newState);
                
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
        } catch (error) {
            console.error(`[PureSchemeRuntime] Error making choice in room ${roomId}:`, error);
            return { success: false, message: error.message };
        }
    }

    /**
     * Обработка действия через Scheme
     */
    async processSchemeAction(roomId, action, character) {
        // Установка текущего состояния в Scheme движке
        const currentState = this.gameStates.get(roomId);
        this.schemeEngine.gameState = currentState;
        
        // Обработка действия
        return await this.schemeEngine.processAction(action, character);
    }

    /**
     * Обработка диалога с NPC через Scheme
     */
    async processNPCDialogueChoice(roomId, playerId, choiceId, character) {
        const action = {
            type: 'dialogue',
            id: choiceId,
            playerId: playerId
        };

        const result = await this.processSchemeAction(roomId, action, character);
        
        if (result.success) {
            this.gameStates.set(roomId, result.newState);
        }

        return result;
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

        const result = await this.processSchemeAction(roomId, action, character);
        
        if (result.success) {
            this.gameStates.set(roomId, result.newState);
        }

        return result;
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
            // Установка текущего состояния в движке
            this.schemeEngine.gameState = gameState;
            
            // Получение доступных действий через Scheme
            const princessChoices = await this.schemeEngine.getAvailableActions('princess');
            const helperChoices = await this.schemeEngine.getAvailableActions('helper');

            // Формирование данных игры
            return {
                roomId: roomId,
                scene: {
                    title: this.extractSceneTitle(gameState),
                    text: this.extractSceneText(gameState)
                },
                choices: {
                    princess: princessChoices,
                    helper: helperChoices
                },
                stats: this.extractCharacterStats(gameState),
                currentTurn: gameState.currentTurn || 'princess',
                chapter: gameState.chapter || 1,
                locations: this.extractLocationData(gameState),
                quests: this.extractQuestData(gameState),
                npcDialogues: this.extractDialogueData(gameState)
            };
        } catch (error) {
            console.error(`[PureSchemeRuntime] Error getting game data for room ${roomId}:`, error);
            return null;
        }
    }

    /**
     * Обработка событий от Scheme
     */
    handleSchemeEvent(event) {
        console.log('[PureSchemeRuntime] Scheme event:', event);
        
        // Перенаправление событий клиентам
        this.emit('clientEvent', {
            type: event.type,
            data: event.data
        });
    }

    /**
     * Извлечение данных из состояния игры
     */
    extractSceneTitle(gameState) {
        return gameState.scene?.title || "Кооперативное приключение";
    }

    extractSceneText(gameState) {
        return gameState.scene?.text || "Утренний свет пробивается сквозь окна...";
    }

    extractCharacterStats(gameState) {
        if (!gameState.characters) return {};
        
        const stats = {};
        
        // gameState.characters должен быть массивом пар [character-id, character-data]
        if (Array.isArray(gameState.characters)) {
            for (const [charId, charData] of gameState.characters) {
                if (Array.isArray(charData)) {
                    // Конвертируем ассоциативный список в объект
                    const charObj = {};
                    for (let i = 0; i < charData.length; i += 2) {
                        charObj[charData[i]] = charData[i + 1];
                    }
                    stats[charId] = charObj;
                } else {
                    stats[charId] = charData;
                }
            }
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
        const quests = gameState.quests || {};
        return {
            princess: { 
                active: quests.active?.princess || null, 
                completed: quests.completed?.princess?.length || 0 
            },
            helper: { 
                active: quests.active?.helper || null, 
                completed: quests.completed?.helper?.length || 0 
            }
        };
    }

    extractDialogueData(gameState) {
        return {
            princess: null,
            helper: null
        };
    }

    /**
     * Получение персонажа по ID игрока
     */
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

    /**
     * Удаление игры
     */
    removeGame(roomId) {
        this.gameStates.delete(roomId);
        console.log(`[PureSchemeRuntime] Game removed for room ${roomId}`);
    }

    /**
     * Получение статуса runtime
     */
    getStatus() {
        return {
            initialized: this.isInitialized,
            activeGames: this.gameStates.size,
            schemeEngineReady: this.schemeEngine.isReady(),
            mode: 'pure-scheme'
        };
    }

    /**
     * Выполнение произвольного Scheme кода (для отладки)
     */
    async executeSchemeCode(code) {
        try {
            const result = await this.schemeEngine.biwa.evaluate(code);
            return {
                success: true,
                result: result // Простой движок возвращает JavaScript объекты
            };
        } catch (error) {
            return {
                success: false,
                error: error.message
            };
        }
    }

    /**
     * Получение определений доступных в Scheme
     */
    async getSchemeDefinitions() {
        try {
            const result = await this.schemeEngine.biwa.evaluate(`
                (list 'available-functions
                      '(make-initial-state get-character-data character-at-location?
                        character-has-item? move-character add-item-to-character
                        validate-action get-available-actions))
            `);
            // Простой движок возвращает уже JavaScript объекты
        return result;
        } catch (error) {
            console.error('[PureSchemeRuntime] Error getting Scheme definitions:', error);
            return [];
        }
    }
}

module.exports = PureSchemeGameRuntime;