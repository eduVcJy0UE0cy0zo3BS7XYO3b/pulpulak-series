/**
 * Функциональный игровой движок на основе BiwaScheme
 * game/functional/integration/functional-engine.js
 */

const BiwaScheme = require('biwascheme');
const fs = require('fs');
const path = require('path');

class FunctionalGameEngine {
    constructor() {
        // Правильная инициализация BiwaScheme
        this.interpreter = new BiwaScheme.Interpreter();
        this.gameState = null;
        this.sideEffects = [];
        this.isReady = false;
    }

    /**
     * Настройка среды выполнения BiwaScheme
     */
    async setupRuntime() {
        try {
            // Проверяем корректность инициализации BiwaScheme
            if (!this.interpreter || typeof this.interpreter.run !== 'function') {
                console.warn('[FunctionalEngine] BiwaScheme not properly initialized, using fallback mode');
                this.useFallbackMode();
                return;
            }
            
            // Настройка JavaScript мостов
            this.setupJavaScriptBridge();
            
            // Простая проверка работоспособности
            try {
                await this.interpreter.run('(+ 1 1)');
                console.log('[FunctionalEngine] BiwaScheme basic test passed');
            } catch (testError) {
                console.warn('[FunctionalEngine] BiwaScheme test failed, using fallback:', testError.message);
                this.useFallbackMode();
                return;
            }
            
            // Загрузка функциональных библиотек (опционально)
            await this.loadSchemeLibraries();
            
            // Инициализация начального состояния
            await this.initializeGameState();
            
            this.isReady = true;
            console.log('[FunctionalEngine] Runtime initialized successfully');
        } catch (error) {
            console.error('[FunctionalEngine] Failed to initialize runtime:', error);
            this.useFallbackMode();
        }
    }

    /**
     * Загрузка Scheme библиотек
     */
    async loadSchemeLibraries() {
        const libraryPaths = [
            'game/functional/core/game-state.scm',
            'game/functional/core/state-transformers.scm', 
            'game/functional/core/effects.scm'
        ];

        for (const libPath of libraryPaths) {
            try {
                const absolutePath = path.resolve(libPath);
                console.log(`[FunctionalEngine] Loading library: ${absolutePath}`);
                await this.interpreter.run(`(load "${absolutePath}")`);
            } catch (error) {
                console.error(`[FunctionalEngine] Failed to load ${libPath}:`, error);
                // Для разработки - продолжаем работу без библиотек
                console.warn(`[FunctionalEngine] Continuing without ${libPath}`);
            }
        }
    }

    /**
     * Настройка мостов между JavaScript и Scheme
     */
    setupJavaScriptBridge() {
        // Проверяем доступность функций BiwaScheme
        if (!this.interpreter || typeof this.interpreter.define_libfunc !== 'function') {
            console.warn('[FunctionalEngine] BiwaScheme bridge functions not available');
            return;
        }

        try {
            // Логирование из Scheme
            this.interpreter.define_libfunc("js-log", 1, 1, (ar) => {
                console.log('[Scheme]', this.schemeToJS(ar[0]));
                return BiwaScheme.undef;
            });

            // Эмиссия событий из Scheme
            this.interpreter.define_libfunc("js-emit-event", 2, 2, (ar) => {
                const eventType = this.schemeToJS(ar[0]);
                const eventData = this.schemeToJS(ar[1]);
                this.emitGameEvent(eventType, eventData);
                return BiwaScheme.undef;
            });

            // Сохранение состояния из Scheme
            this.interpreter.define_libfunc("js-save-state", 1, 1, (ar) => {
                this.gameState = this.schemeToJS(ar[0]);
                return BiwaScheme.undef;
            });

            // Получение текущего времени
            this.interpreter.define_libfunc("js-current-time", 0, 0, () => {
                return new BiwaScheme.Number(Date.now());
            });

            // Генерация UUID
            this.interpreter.define_libfunc("js-generate-uuid", 0, 0, () => {
                return new BiwaScheme.Str(this.generateUUID());
            });

            // Валидация данных
            this.interpreter.define_libfunc("js-validate-data", 2, 2, (ar) => {
                const data = this.schemeToJS(ar[0]);
                const schema = this.schemeToJS(ar[1]);
                return this.validateData(data, schema) ? BiwaScheme.true : BiwaScheme.false;
            });
            
            console.log('[FunctionalEngine] JavaScript bridge functions registered');
        } catch (bridgeError) {
            console.warn('[FunctionalEngine] Failed to setup JavaScript bridge:', bridgeError.message);
        }
    }

    /**
     * Инициализация игрового состояния
     */
    async initializeGameState() {
        try {
            if (this.interpreter && typeof this.interpreter.run === 'function') {
                // Попытка создать состояние через Scheme
                const initialState = await this.interpreter.run(`
                    (make-initial-state "default-room" 
                                      '((princess . "player1") 
                                        (helper . "player2")))
                `);
                
                this.gameState = this.schemeToJS(initialState);
                console.log('[FunctionalEngine] Initial game state created via Scheme');
            } else {
                throw new Error('BiwaScheme not available');
            }
        } catch (error) {
            console.warn('[FunctionalEngine] Failed to create Scheme state, using fallback:', error.message);
            // Fallback к базовому состоянию
            this.gameState = this.createFallbackState();
            console.log('[FunctionalEngine] Fallback game state created');
        }
    }

    /**
     * Использовать fallback режим без Scheme
     */
    useFallbackMode() {
        console.log('[FunctionalEngine] Switching to fallback mode');
        this.isReady = true;
        this.gameState = this.createFallbackState();
    }

    /**
     * Обработка игрового действия (чистая функция)
     */
    async processAction(action, character) {
        try {
            // Сброс побочных эффектов
            this.sideEffects = [];
            
            if (this.isReady && this.interpreter && typeof this.interpreter.run === 'function') {
                // Полная функциональная обработка через Scheme
                return await this.processActionViaScheme(action, character);
            } else {
                // Fallback обработка через JavaScript
                return this.processActionFallback(action, character);
            }
        } catch (error) {
            console.error('[FunctionalEngine] Error processing action:', error);
            return {
                success: false,
                error: error.message,
                sideEffects: []
            };
        }
    }

    /**
     * Обработка действия через Scheme
     */
    async processActionViaScheme(action, character) {
        // Конвертация действия в Scheme формат
        const schemeAction = this.jsToScheme(action);
        const schemeCharacter = this.jsToScheme(character);
        
        // Выполнение действия через функциональную логику
        const result = await this.interpreter.run(`
            (let* ((action ${schemeAction})
                   (character ${schemeCharacter})
                   (current-state (js-get-current-state))
                   (validation-result (validate-action current-state character action)))
              (if validation-result
                (let* ((effect (action->effect action character))
                       (effect-result (run-effect effect current-state))
                       (value (first effect-result))
                       (new-state (second effect-result))
                       (side-effects (third effect-result)))
                  (js-save-state new-state)
                  (js-process-side-effects side-effects)
                  (list 'success value))
                (list 'error "Invalid action")))
        `);
        
        const jsResult = this.schemeToJS(result);
        
        if (jsResult[0] === 'success') {
            return {
                success: true,
                result: jsResult[1],
                sideEffects: this.sideEffects,
                newState: this.gameState
            };
        } else {
            return {
                success: false,
                error: jsResult[1],
                sideEffects: this.sideEffects
            };
        }
    }

    /**
     * Fallback обработка действия через JavaScript
     */
    processActionFallback(action, character) {
        console.log(`[FunctionalEngine] Processing action (fallback): ${action.type} for ${character}`);
        
        // Простая валидация
        if (!this.gameState || !this.gameState.characters || !this.gameState.characters[character]) {
            return {
                success: false,
                error: `Character ${character} not found`,
                sideEffects: []
            };
        }

        // Базовая обработка действий
        const characterData = this.gameState.characters[character];
        let newState = JSON.parse(JSON.stringify(this.gameState)); // Глубокое копирование
        let result = { message: "Action processed" };

        switch (action.type) {
            case 'move':
                if (action.location) {
                    newState.characters[character].location = action.location;
                    result.message = `${character} moved to ${action.location}`;
                    this.sideEffects.push(['log', result.message]);
                } else {
                    return {
                        success: false,
                        error: "No location specified for move action",
                        sideEffects: []
                    };
                }
                break;

            case 'choice':
                result.message = `${character} made choice: ${action.id}`;
                this.sideEffects.push(['log', result.message]);
                break;

            case 'interact':
                if (action.npc) {
                    result.message = `${character} interacts with ${action.npc}`;
                    this.sideEffects.push(['log', result.message]);
                } else {
                    return {
                        success: false,
                        error: "No NPC specified for interact action",
                        sideEffects: []
                    };
                }
                break;

            default:
                result.message = `${character} performed ${action.type}`;
                this.sideEffects.push(['log', result.message]);
        }

        // Обновление состояния
        this.gameState = newState;

        return {
            success: true,
            result,
            sideEffects: this.sideEffects,
            newState: this.gameState
        };
    }

    /**
     * Получение доступных действий (чистая функция)
     */
    async getAvailableActions(character) {
        try {
            if (this.isReady && this.interpreter && typeof this.interpreter.run === 'function') {
                const schemeCharacter = this.jsToScheme(character);
                
                const result = await this.interpreter.run(`
                    (let ((character ${schemeCharacter})
                          (current-state (js-get-current-state)))
                      (get-available-actions current-state character))
                `);
                
                return this.schemeToJS(result);
            } else {
                // Fallback: базовые действия
                return this.getAvailableActionsFallback(character);
            }
        } catch (error) {
            console.error('[FunctionalEngine] Error getting available actions:', error);
            return this.getAvailableActionsFallback(character);
        }
    }

    /**
     * Fallback получение доступных действий
     */
    getAvailableActionsFallback(character) {
        if (!this.gameState || !this.gameState.characters || !this.gameState.characters[character]) {
            return [];
        }

        const characterData = this.gameState.characters[character];
        const actions = [];

        // Базовые действия перемещения
        const locations = ['throne_room', 'kitchen', 'garden', 'library', 'village'];
        for (const location of locations) {
            if (location !== characterData.location) {
                actions.push({
                    type: 'move',
                    id: `move_to_${location}`,
                    text: `Move to ${location}`,
                    location: location
                });
            }
        }

        // Базовые игровые действия
        actions.push({
            type: 'choice',
            id: 'explore',
            text: 'Explore current location'
        });

        actions.push({
            type: 'choice',
            id: 'rest',
            text: 'Rest and recover'
        });

        return actions;
    }

    /**
     * Валидация действия (чистая функция)
     */
    async validateAction(action, character) {
        try {
            if (this.isReady && this.interpreter && typeof this.interpreter.run === 'function') {
                const schemeAction = this.jsToScheme(action);
                const schemeCharacter = this.jsToScheme(character);
                
                const result = await this.interpreter.run(`
                    (let ((action ${schemeAction})
                          (character ${schemeCharacter})
                          (current-state (js-get-current-state)))
                      (validate-action current-state character action))
                `);
                
                return result === BiwaScheme.true;
            } else {
                // Fallback валидация
                return this.validateActionFallback(action, character);
            }
        } catch (error) {
            console.error('[FunctionalEngine] Error validating action:', error);
            return this.validateActionFallback(action, character);
        }
    }

    /**
     * Fallback валидация действия
     */
    validateActionFallback(action, character) {
        if (!this.gameState || !this.gameState.characters || !this.gameState.characters[character]) {
            return false;
        }

        if (!action || !action.type) {
            return false;
        }

        const characterData = this.gameState.characters[character];

        switch (action.type) {
            case 'move':
                return action.location && action.location !== characterData.location;
            
            case 'choice':
                return action.id && typeof action.id === 'string';
            
            case 'interact':
                return action.npc && typeof action.npc === 'string';
            
            default:
                return true; // Разрешаем неизвестные действия в fallback режиме
        }
    }

    /**
     * Получение состояния персонажа
     */
    async getCharacterState(characterId) {
        try {
            const schemeCharacter = this.jsToScheme(characterId);
            
            const result = await this.interpreter.run(`
                (let ((character-id ${schemeCharacter})
                      (current-state (js-get-current-state)))
                  (get-character-data current-state character-id))
            `);
            
            return this.schemeToJS(result);
        } catch (error) {
            console.error('[FunctionalEngine] Error getting character state:', error);
            return null;
        }
    }

    /**
     * Конвертация JavaScript объектов в Scheme S-expressions
     */
    jsToScheme(obj) {
        if (obj === null) return "'()";
        if (obj === undefined) return "#f";
        if (typeof obj === 'string') return `"${obj.replace(/"/g, '\\"')}"`;
        if (typeof obj === 'number') return obj.toString();
        if (typeof obj === 'boolean') return obj ? '#t' : '#f';
        if (Array.isArray(obj)) {
            const elements = obj.map(item => this.jsToScheme(item)).join(' ');
            return `(${elements})`;
        }
        if (typeof obj === 'object') {
            const pairs = Object.entries(obj)
                .map(([k, v]) => `(${k} . ${this.jsToScheme(v)})`)
                .join(' ');
            return `(${pairs})`;
        }
        return obj.toString();
    }

    /**
     * Конвертация Scheme значений в JavaScript объекты
     */
    schemeToJS(schemeValue) {
        // Базовая имплементация - нужно расширить для BiwaScheme специфики
        if (schemeValue === BiwaScheme.true) return true;
        if (schemeValue === BiwaScheme.false) return false;
        if (schemeValue === BiwaScheme.nil) return null;
        if (schemeValue === BiwaScheme.undef) return undefined;
        
        if (schemeValue instanceof BiwaScheme.Number) {
            return schemeValue.value;
        }
        
        if (schemeValue instanceof BiwaScheme.Str) {
            return schemeValue.str;
        }
        
        if (schemeValue instanceof BiwaScheme.Pair) {
            // Конвертация списков и ассоциативных списков
            return this.pairToJS(schemeValue);
        }
        
        return schemeValue;
    }

    /**
     * Конвертация Scheme пар в JavaScript структуры
     */
    pairToJS(pair) {
        const result = [];
        let current = pair;
        
        while (current !== BiwaScheme.nil) {
            if (!(current instanceof BiwaScheme.Pair)) {
                // Неправильно сформированный список
                break;
            }
            
            result.push(this.schemeToJS(current.car));
            current = current.cdr;
        }
        
        return result;
    }

    /**
     * Обработка побочных эффектов из Scheme
     */
    processSideEffects(effects) {
        for (const effect of effects) {
            const [type, ...data] = effect;
            
            switch (type) {
                case 'log':
                    console.log('[Effect]', ...data);
                    break;
                case 'event':
                    this.emitGameEvent(data[0], data[1]);
                    break;
                case 'error':
                    console.error('[Effect Error]', ...data);
                    break;
                default:
                    console.warn('[Unknown Effect]', type, data);
            }
        }
        
        this.sideEffects.push(...effects);
    }

    /**
     * Эмиссия игровых событий
     */
    emitGameEvent(type, data) {
        // Интеграция с системой событий
        if (this.eventEmitter) {
            this.eventEmitter.emit('gameEvent', { type, data });
        }
        
        console.log('[GameEvent]', type, data);
    }

    /**
     * Установка обработчика событий
     */
    setEventEmitter(emitter) {
        this.eventEmitter = emitter;
    }

    /**
     * Генерация UUID
     */
    generateUUID() {
        return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
            const r = Math.random() * 16 | 0;
            const v = c === 'x' ? r : (r & 0x3 | 0x8);
            return v.toString(16);
        });
    }

    /**
     * Валидация данных по схеме
     */
    validateData(data, schema) {
        // Базовая валидация - можно расширить
        return typeof data === schema.type;
    }

    /**
     * Создание запасного состояния при ошибках
     */
    createFallbackState() {
        return {
            scene: 'coop_awakening',
            characters: {
                princess: {
                    id: 'princess',
                    location: 'princess_chamber',
                    outfit: 'princess_dress',
                    inventory: [],
                    stats: { loyalty: 50, knowledge: 30, charm: 70 }
                },
                helper: {
                    id: 'helper', 
                    location: 'princess_chamber',
                    outfit: 'common_dress',
                    inventory: ['translation_earrings', 'voice_medallion'],
                    stats: { loyalty: 50, knowledge: 60, charm: 40 }
                }
            },
            world: {
                time: 'early_morning',
                events: []
            },
            quests: {
                active: [],
                completed: [],
                globalMemory: {}
            }
        };
    }

    /**
     * Получение текущего состояния игры
     */
    getCurrentState() {
        return this.gameState;
    }

    /**
     * Установка состояния игры (для тестирования)
     */
    setGameState(newState) {
        this.gameState = newState;
    }
}

module.exports = FunctionalGameEngine;