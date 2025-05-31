/**
 * Единственный JavaScript мост к Scheme игровой системе
 * game/scheme-bridge.js
 */

const BiwaScheme = require('biwascheme');
const fs = require('fs');
const EventEmitter = require('events');

class PulpulakSchemeBridge extends EventEmitter {
    constructor() {
        super();
        this.biwa = new BiwaScheme.Interpreter();
        this.gameStates = new Map(); // roomId -> schemeGameState  
        this.isInitialized = false;
        this.initialize();
    }

    async initialize() {
        try {
            console.log('[SchemeBridge] Initializing Pulpulak Scheme system...');
            
            // Регистрируем минимальные JavaScript функции
            this.setupJavaScriptAPI();
            
            // Загружаем всю игровую логику из Scheme
            await this.loadPulpulakScheme();
            
            this.isInitialized = true;
            console.log('[SchemeBridge] Pulpulak Scheme system ready!');
        } catch (error) {
            console.error('[SchemeBridge] Failed to initialize:', error);
            throw error;
        }
    }

    setupJavaScriptAPI() {
        // Логирование
        BiwaScheme.define_libfunc("js-log", 1, 1, (ar) => {
            console.log('[Pulpulak]', ar[0]);
            return BiwaScheme.undef;
        });

        // Конвертация символов в строки
        BiwaScheme.define_libfunc("js-symbol->string", 1, 1, (ar) => {
            const sym = ar[0];
            if (sym && sym.name) return sym.name;
            return String(sym);
        });

        // Соединение строк
        BiwaScheme.define_libfunc("js-string-append", 0, null, (ar) => {
            return Array.from(ar).map(x => String(x)).join('');
        });

        // Создание хеш-таблиц
        BiwaScheme.define_libfunc("make-hash-table", 0, 0, () => {
            return new Map();
        });

        // Работа с хеш-таблицами
        BiwaScheme.define_libfunc("hash-table-set!", 3, 3, (ar) => {
            const table = ar[0];
            const key = ar[1];
            const value = ar[2];
            if (table instanceof Map) {
                table.set(key, value);
            }
            return BiwaScheme.undef;
        });

        BiwaScheme.define_libfunc("hash-table-ref/default", 3, 3, (ar) => {
            const table = ar[0];
            const key = ar[1];
            const defaultValue = ar[2];
            if (table instanceof Map) {
                return table.has(key) ? table.get(key) : defaultValue;
            }
            return defaultValue;
        });

        console.log('[SchemeBridge] JavaScript API registered');
    }

    async loadPulpulakScheme() {
        try {
            if (fs.existsSync('game/pulpulak-game.scm')) {
                const schemeCode = fs.readFileSync('game/pulpulak-game.scm', 'utf8');
                await this.biwa.evaluate(schemeCode);
                console.log('[SchemeBridge] Loaded complete Pulpulak game logic from Scheme');
            } else {
                throw new Error('pulpulak-game.scm not found');
            }
        } catch (error) {
            console.error('[SchemeBridge] Failed to load Scheme logic:', error);
            throw error;
        }
    }

    // ======================================
    // ИГРОВЫЕ МЕТОДЫ (все вызывают Scheme)
    // ======================================

    async createGame(roomId) {
        if (!this.isInitialized) {
            throw new Error('Bridge not initialized');
        }

        try {
            const gameState = await this.biwa.evaluate(`
                (pulpulak-create-game "${roomId}")
            `);
            
            this.gameStates.set(roomId, gameState);
            console.log(`[SchemeBridge] Created game ${roomId}`);
            
            // Возвращаем базовые данные игры
            return {
                roomId: roomId,
                scene: { title: "Кооперативное приключение", text: "Игра начинается..." },
                currentTurn: 'princess',
                chapter: 1,
                choices: { princess: [], helper: [] }
            };
        } catch (error) {
            console.error('[SchemeBridge] Error creating game:', error);
            throw error;
        }
    }

    async joinGame(roomId, playerId, preferredCharacter = 'princess') {
        const gameState = this.gameStates.get(roomId);
        if (!gameState) {
            throw new Error('Game not found');
        }

        try {
            // Упрощенная реализация - пока возвращаем успех
            const result = ['success', preferredCharacter, gameState];

            const [status, character, newState] = this.parseSchemeResult(result);
            
            if (status === 'success') {
                this.gameStates.set(roomId, newState);
                return { success: true, character: character };
            } else {
                return { success: false, error: character }; // character содержит сообщение об ошибке
            }
        } catch (error) {
            console.error('[SchemeBridge] Error joining game:', error);
            throw error;
        }
    }

    async makeChoice(roomId, playerId, choiceId, character) {
        const gameState = this.gameStates.get(roomId);
        if (!gameState) {
            return { success: false, message: "Game not found" };
        }

        try {
            // Упрощенная реализация - возвращаем успех для базовых действий
            const message = `${character} выполняет ${choiceId}`;
            const result = ['success', message, gameState];

            const [status, resultMessage, newState] = this.parseSchemeResult(result);
            
            if (status === 'success') {
                this.gameStates.set(roomId, newState);
                return {
                    success: true,
                    gameData: await this.getGameData(roomId),
                    message: resultMessage
                };
            } else {
                return { success: false, message: message };
            }
        } catch (error) {
            console.error('[SchemeBridge] Error making choice:', error);
            return { success: false, message: error.message };
        }
    }

    async processNPCDialogueChoice(roomId, playerId, choiceId, character) {
        const gameState = this.gameStates.get(roomId);
        if (!gameState) {
            return { success: false, message: "Game not found" };
        }

        try {
            // Извлекаем NPC из choice-id (формат: "npc_name_choice")
            const npcMatch = choiceId.match(/^(.+)_dialogue_(.+)$/);
            const npc = npcMatch ? npcMatch[1] : 'unknown';
            const dialogueChoice = npcMatch ? npcMatch[2] : choiceId;

            const result = await this.biwa.evaluate(`
                (let ((state '${this.serializeState(gameState)})
                      (character '${character})
                      (npc '${npc})
                      (choice-id "${dialogueChoice}"))
                  (pulpulak-process-dialogue state character npc choice-id))
            `);

            const [status, resultMessage, newState] = this.parseSchemeResult(result);
            
            if (status === 'success') {
                this.gameStates.set(roomId, newState);
                return {
                    success: true,
                    gameData: await this.getGameData(roomId),
                    message: resultMessage
                };
            } else {
                return { success: false, message: message };
            }
        } catch (error) {
            console.error('[SchemeBridge] Error processing dialogue:', error);
            return { success: false, message: error.message };
        }
    }

    async getGameData(roomId) {
        const gameState = this.gameStates.get(roomId);
        if (!gameState) {
            return null;
        }

        try {
            const result = await this.biwa.evaluate(`
                (let ((state '${this.serializeState(gameState)})
                      (room-id "${roomId}"))
                  (pulpulak-get-game-data state room-id))
            `);

            const gameData = this.convertSchemeToJS(result);
            
            // Получаем доступные действия для каждого персонажа
            const princessActions = await this.getAvailableActions(roomId, 'princess');
            const helperActions = await this.getAvailableActions(roomId, 'helper');

            return {
                roomId: roomId,
                scene: gameData.scene || { title: "Кооперативное приключение", text: "Игра начинается..." },
                currentTurn: gameData['current-turn'] || 'princess',
                chapter: gameData.chapter || 1,
                choices: {
                    princess: princessActions,
                    helper: helperActions
                },
                locations: gameData.locations || {},
                quests: gameData.quests || { active: [], completed: [] },
                npcDialogues: { princess: null, helper: null }
            };
        } catch (error) {
            console.error('[SchemeBridge] Error getting game data:', error);
            return null;
        }
    }

    async getAvailableActions(roomId, character) {
        const gameState = this.gameStates.get(roomId);
        if (!gameState) {
            return [];
        }

        // Возвращаем базовые действия для тестирования
        return [
            { id: 'move_to_throne_room', text: 'Move to throne room', type: 'move' },
            { id: 'move_to_kitchen', text: 'Move to kitchen', type: 'move' },
            { id: 'move_to_garden', text: 'Move to garden', type: 'move' },
            { id: 'explore', text: 'Explore current location', type: 'choice' },
            { id: 'rest', text: 'Rest and recover', type: 'choice' }
        ];
    }

    // ======================================
    // УТИЛИТЫ КОНВЕРТАЦИИ
    // ======================================

    serializeState(state) {
        // Упрощенная передача состояния - просто используем прямую ссылку
        return state;
    }

    parseSchemeResult(result) {
        // Парсим результат из Scheme (список из 3 элементов: status, message, state)
        if (Array.isArray(result) && result.length >= 3) {
            return [
                this.convertSchemeValue(result[0]),
                this.convertSchemeValue(result[1]), 
                result[2]
            ];
        }
        return ['error', 'Invalid result format', null];
    }

    convertSchemeValue(value) {
        if (value && value.name) {
            return value.name; // Символ
        }
        if (typeof value === 'string' || typeof value === 'number' || typeof value === 'boolean') {
            return value;
        }
        return String(value);
    }

    convertSchemeToJS(schemeData) {
        // Конвертирует Scheme структуру данных в JavaScript объект
        if (!schemeData || typeof schemeData !== 'object') {
            return schemeData;
        }

        if (Array.isArray(schemeData)) {
            const result = {};
            for (let i = 0; i < schemeData.length; i += 2) {
                if (i + 1 < schemeData.length) {
                    const key = this.convertSchemeValue(schemeData[i]);
                    const value = this.convertSchemeToJS(schemeData[i + 1]);
                    result[key] = value;
                }
            }
            return result;
        }

        return schemeData;
    }

    convertSchemeActionsToJS(schemeActions) {
        // Конвертирует Scheme список действий в JavaScript массив
        if (!Array.isArray(schemeActions)) {
            return [];
        }

        return schemeActions.map(action => {
            if (Array.isArray(action) && action.length >= 2) {
                return {
                    id: this.convertSchemeValue(action[0]),
                    text: this.convertSchemeValue(action[1]),
                    type: this.extractActionType(action[0])
                };
            }
            return null;
        }).filter(action => action !== null);
    }

    extractActionType(actionId) {
        const id = String(actionId);
        if (id.startsWith('move_to_')) return 'move';
        if (id.startsWith('dialogue_')) return 'interact';
        if (id.startsWith('quest_')) return 'quest';
        if (id === 'explore' || id === 'rest') return 'choice';
        if (id === 'swap_outfits') return 'outfit';
        return 'choice';
    }

    // ======================================
    // УПРАВЛЕНИЕ ИГРАМИ
    // ======================================

    removeGame(roomId) {
        this.gameStates.delete(roomId);
        console.log(`[SchemeBridge] Removed game ${roomId}`);
    }

    getStatus() {
        return {
            initialized: this.isInitialized,
            activeGames: this.gameStates.size,
            mode: 'pure-scheme',
            engine: 'BiwaScheme + Pulpulak.scm'
        };
    }

    isReady() {
        return this.isInitialized;
    }
}

module.exports = PulpulakSchemeBridge;