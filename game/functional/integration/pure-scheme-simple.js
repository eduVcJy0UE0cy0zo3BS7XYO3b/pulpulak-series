/**
 * Простой чистый Scheme движок - только BiwaScheme, никаких парсеров
 * game/functional/integration/pure-scheme-simple.js
 */

const BiwaScheme = require('biwascheme');
const fs = require('fs');

class PureSchemeSimpleEngine {
    constructor() {
        this.biwa = new BiwaScheme.Interpreter();
        this.gameState = null;
        this.isInitialized = false;
        this.initialize();
    }

    async initialize() {
        try {
            console.log('[PureSchemeSimple] Initializing...');
            
            // Загружаем Scheme библиотеки
            await this.loadSchemeLibraries();
            
            // Регистрируем JavaScript функции
            this.setupJavaScriptAPI();
            
            // Создаем начальное состояние
            await this.createInitialState();
            
            this.isInitialized = true;
            console.log('[PureSchemeSimple] Initialized successfully');
        } catch (error) {
            console.error('[PureSchemeSimple] Failed to initialize:', error);
            throw error;
        }
    }

    async loadSchemeLibraries() {
        // Загружаем game-core.scm если есть
        try {
            if (fs.existsSync('game/functional/core/game-core.scm')) {
                const schemeCode = fs.readFileSync('game/functional/core/game-core.scm', 'utf8');
                await this.biwa.evaluate(schemeCode);
                console.log('[PureSchemeSimple] Loaded game-core.scm');
                return;
            }
        } catch (error) {
            console.warn('[PureSchemeSimple] Could not load game-core.scm:', error.message);
        }

        // Базовые определения - используем JavaScript объекты напрямую
        await this.biwa.evaluate(`
            ;; Создание игрового состояния
            (define (make-game-state room-id)
              (js-create-state room-id))

            ;; Получение доступных действий  
            (define (get-actions character)
              (js-get-actions character))

            ;; Валидация действия
            (define (validate-action-simple action character)
              #t)

            ;; Обработка действия
            (define (process-action-simple action character state)
              (js-log "Processing action in Scheme")
              state)
        `);
        
        console.log('[PureSchemeSimple] Loaded basic definitions');
    }

    setupJavaScriptAPI() {
        const self = this;

        // Логирование
        BiwaScheme.define_libfunc("js-log", 1, 1, function(ar) {
            console.log('[Scheme]', ar[0]);
            return BiwaScheme.undef;
        });

        // Получение состояния
        BiwaScheme.define_libfunc("js-get-state", 0, 0, function() {
            return self.gameState;
        });

        // Сохранение состояния
        BiwaScheme.define_libfunc("js-set-state", 1, 1, function(ar) {
            self.gameState = ar[0];
            return BiwaScheme.undef;
        });

        // Создание состояния
        BiwaScheme.define_libfunc("js-create-state", 1, 1, function(ar) {
            const roomId = ar[0];
            return {
                roomId: roomId,
                scene: 'coop_awakening',
                characters: {
                    princess: { location: 'princess_chamber', outfit: 'princess_dress' },
                    helper: { location: 'princess_chamber', outfit: 'common_dress' }
                }
            };
        });

        // Получение действий
        BiwaScheme.define_libfunc("js-get-actions", 1, 1, function(ar) {
            const character = ar[0];
            return [
                { type: 'move', location: 'throne_room', text: 'Move to throne room' },
                { type: 'move', location: 'kitchen', text: 'Move to kitchen' },
                { type: 'move', location: 'garden', text: 'Move to garden' },
                { type: 'choice', id: 'explore', text: 'Explore location' }
            ];
        });

        console.log('[PureSchemeSimple] JavaScript API registered');
    }

    async createInitialState() {
        try {
            this.gameState = await this.biwa.evaluate(`(make-game-state "default")`);
            console.log('[PureSchemeSimple] Initial state created');
        } catch (error) {
            console.error('[PureSchemeSimple] Failed to create initial state:', error);
            // Fallback состояние
            this.gameState = null;
        }
    }

    async getAvailableActions(character) {
        if (!this.isInitialized) {
            return [];
        }

        try {
            // BiwaScheme вызывает нашу js-get-actions функцию и возвращает JavaScript массив
            const result = await this.biwa.evaluate(`(get-actions '${character})`);
            
            // Результат уже JavaScript объект
            return Array.isArray(result) ? result : this.getFallbackActions();
        } catch (error) {
            console.error('[PureSchemeSimple] Error getting actions:', error);
            return this.getFallbackActions();
        }
    }

    async validateAction(action, character) {
        if (!this.isInitialized) {
            return false;
        }

        try {
            const result = await this.biwa.evaluate(`
                (validate-action-simple '${this.actionToScheme(action)} '${character})
            `);
            return result === BiwaScheme.true;
        } catch (error) {
            console.error('[PureSchemeSimple] Error validating action:', error);
            return false;
        }
    }

    async processAction(action, character) {
        if (!this.isInitialized) {
            return { success: false, error: 'Not initialized' };
        }

        try {
            // Простая реализация - всегда успешно
            const isValid = await this.validateAction(action, character);
            
            if (isValid) {
                // Логируем действие через Scheme
                await this.biwa.evaluate(`(js-log "Action processed successfully")`);
                
                return {
                    success: true,
                    result: { message: 'Action processed successfully' },
                    newState: this.gameState
                };
            } else {
                return {
                    success: false,
                    error: 'Action validation failed'
                };
            }
        } catch (error) {
            console.error('[PureSchemeSimple] Error processing action:', error);
            return {
                success: false,
                error: error.message
            };
        }
    }

    // Простые утилиты конвертации
    actionToScheme(action) {
        if (!action || !action.type) {
            return '()';
        }
        
        switch (action.type) {
            case 'move':
                return `(move ${action.location || 'unknown'})`;
            case 'choice':
                return `(choice ${action.id || 'unknown'})`;
            case 'interact':
                return `(interact ${action.npc || 'unknown'})`;
            default:
                return `(${action.type})`;
        }
    }

    convertSchemeToJS(schemeValue) {
        // Простая конвертация - BiwaScheme уже делает большую часть работы
        if (schemeValue === BiwaScheme.nil) {
            return [];
        }
        if (schemeValue === BiwaScheme.true) {
            return true;
        }
        if (schemeValue === BiwaScheme.false) {
            return false;
        }
        if (Array.isArray(schemeValue)) {
            return schemeValue.map(item => this.convertSchemeToJS(item));
        }
        return schemeValue;
    }

    getFallbackActions() {
        return [
            { type: 'move', location: 'throne_room', text: 'Move to throne room' },
            { type: 'move', location: 'kitchen', text: 'Move to kitchen' },
            { type: 'choice', id: 'explore', text: 'Explore location' }
        ];
    }

    getCurrentState() {
        return this.gameState;
    }

    isReady() {
        return this.isInitialized;
    }

    setEventEmitter(emitter) {
        this.eventEmitter = emitter;
    }
}

module.exports = PureSchemeSimpleEngine;