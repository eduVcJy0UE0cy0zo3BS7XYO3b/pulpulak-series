/**
 * Движок только на Scheme - вся логика в .scm файлах
 * game/functional/integration/scheme-only-engine.js
 */

const BiwaScheme = require('biwascheme');
const fs = require('fs');

class SchemeOnlyGameEngine {
    constructor() {
        this.biwa = new BiwaScheme.Interpreter();
        this.gameState = null;
        this.isInitialized = false;
        this.initialize();
    }

    async initialize() {
        try {
            console.log('[SchemeOnly] Initializing Scheme-only engine...');
            
            // Регистрируем минимальные JavaScript функции
            this.setupMinimalJavaScriptAPI();
            
            // Регистрируем API для состояния
            this.setupSchemeStateAPI();
            
            // Загружаем Scheme логику
            await this.loadSchemeLogic();
            
            this.isInitialized = true;
            console.log('[SchemeOnly] Scheme-only engine initialized successfully');
        } catch (error) {
            console.error('[SchemeOnly] Failed to initialize:', error);
            throw error;
        }
    }

    setupMinimalJavaScriptAPI() {
        // Только самые необходимые функции для взаимодействия с JavaScript

        // Логирование
        BiwaScheme.define_libfunc("js-log", 1, 1, function(ar) {
            console.log('[Scheme]', ar[0]);
            return BiwaScheme.undef;
        });

        // Конвертация символов в строки (для BiwaScheme)
        BiwaScheme.define_libfunc("js-symbol->string", 1, 1, function(ar) {
            const sym = ar[0];
            if (sym && sym.name) {
                return sym.name;
            }
            return String(sym);
        });

        // Соединение строк
        BiwaScheme.define_libfunc("js-string-append", 2, 2, function(ar) {
            const str1 = String(ar[0]);
            const str2 = String(ar[1]);
            return str1 + str2;
        });

        console.log('[SchemeOnly] Minimal JavaScript API registered');
    }

    async loadSchemeLogic() {
        // Загружаем основную игровую логику
        try {
            if (fs.existsSync('game/functional/core/game-logic.scm')) {
                const schemeCode = fs.readFileSync('game/functional/core/game-logic.scm', 'utf8');
                await this.biwa.evaluate(schemeCode);
                console.log('[SchemeOnly] Loaded game-logic.scm');
            } else {
                throw new Error('game-logic.scm not found');
            }
        } catch (error) {
            console.error('[SchemeOnly] Failed to load Scheme logic:', error);
            throw error;
        }
    }

    // ======================================
    // ОСНОВНЫЕ МЕТОДЫ ДВИЖКА
    // ======================================

    async createGame(roomId, players) {
        if (!this.isInitialized) {
            throw new Error('Engine not initialized');
        }

        try {
            // Вся логика создания игры в Scheme
            const result = await this.biwa.evaluate(`
                (scheme-create-game "${roomId}" '())
            `);
            
            this.gameState = result;
            return result;
        } catch (error) {
            console.error('[SchemeOnly] Error creating game:', error);
            throw error;
        }
    }

    async getAvailableActions(characterId) {
        if (!this.isInitialized || !this.gameState) {
            return this.getFallbackActions();
        }

        try {
            // Вся логика получения действий в Scheme
            const result = await this.biwa.evaluate(`
                (let ((state (js-get-current-state))
                      (character '${characterId}))
                  (scheme-get-actions state character))
            `);
            
            const actions = this.convertSchemeListToJS(result);
            return actions.length > 0 ? actions : this.getFallbackActions();
        } catch (error) {
            console.error('[SchemeOnly] Error getting actions:', error);
            return this.getFallbackActions();
        }
    }

    async validateAction(action, characterId) {
        if (!this.isInitialized) {
            return false;
        }

        try {
            // Создаем состояние если его нет
            if (!this.gameState) {
                this.gameState = await this.createGame('temp', {});
            }

            // Вся логика валидации в Scheme
            const schemeAction = this.convertJSActionToScheme(action);
            
            const result = await this.biwa.evaluate(`
                (let ((state (js-get-current-state))
                      (character '${characterId})
                      (action '${schemeAction}))
                  (scheme-validate-action state character action))
            `);
            
            return result === BiwaScheme.true;
        } catch (error) {
            console.error('[SchemeOnly] Error validating action:', error);
            return false;
        }
    }

    async processAction(action, characterId) {
        if (!this.isInitialized || !this.gameState) {
            return { success: false, error: 'Engine not initialized' };
        }

        try {
            // Сначала валидируем
            const isValid = await this.validateAction(action, characterId);
            
            if (!isValid) {
                return { success: false, error: 'Invalid action' };
            }

            // Вся логика обработки в Scheme
            const schemeAction = this.convertJSActionToScheme(action);
            
            const newState = await this.biwa.evaluate(`
                (let ((state (js-get-current-state))
                      (character '${characterId})
                      (action '${schemeAction}))
                  (scheme-process-action state character action))
            `);
            
            this.gameState = newState;
            
            return {
                success: true,
                result: { message: 'Action processed by Scheme' },
                newState: newState
            };
        } catch (error) {
            console.error('[SchemeOnly] Error processing action:', error);
            return { success: false, error: error.message };
        }
    }

    // ======================================
    // УТИЛИТЫ КОНВЕРТАЦИИ
    // ======================================

    convertJSActionToScheme(action) {
        if (!action || !action.type) {
            return '()';
        }

        // Конвертируем JavaScript действие в Scheme S-expression
        switch (action.type) {
            case 'move':
                return `(action (type move) (location ${action.location || 'unknown'}))`;
            case 'choice':
                return `(action (type choice) (id ${action.id || 'unknown'}))`;
            case 'interact':
                return `(action (type interact) (npc ${action.npc || 'unknown'}))`;
            default:
                return `(action (type ${action.type}))`;
        }
    }

    convertSchemeListToJS(schemeList) {
        // Простая конвертация Scheme списка в JavaScript массив
        if (!schemeList || schemeList === BiwaScheme.nil) {
            return [];
        }

        const result = [];
        let current = schemeList;

        // Проходим по Scheme списку
        while (current && current !== BiwaScheme.nil) {
            if (current.car) {
                const action = this.convertSchemeActionToJS(current.car);
                if (action) {
                    result.push(action);
                }
                current = current.cdr;
            } else {
                break;
            }
        }

        return result;
    }

    convertSchemeActionToJS(schemeAction) {
        // Конвертируем Scheme действие в JavaScript объект
        if (!schemeAction || schemeAction === BiwaScheme.nil) {
            return null;
        }

        const action = {};
        let current = schemeAction;

        // Проходим по свойствам действия
        while (current && current !== BiwaScheme.nil) {
            if (current.car && current.car.car) {
                const key = this.getSchemeSymbolName(current.car.car);
                const value = this.getSchemeValue(current.car.cdr);
                
                if (key && value !== undefined) {
                    action[key] = value;
                }
            }
            current = current.cdr;
        }

        return Object.keys(action).length > 0 ? action : null;
    }

    getSchemeSymbolName(symbol) {
        if (symbol && symbol.name) {
            return symbol.name;
        }
        return String(symbol);
    }

    getSchemeValue(schemeValue) {
        if (!schemeValue) return undefined;
        if (schemeValue === BiwaScheme.nil) return null;
        if (schemeValue === BiwaScheme.true) return true;
        if (schemeValue === BiwaScheme.false) return false;
        
        if (schemeValue.car) {
            // Это список, берем первый элемент
            return this.getSchemeSymbolName(schemeValue.car);
        }
        
        if (schemeValue.name) {
            return schemeValue.name;
        }
        
        return String(schemeValue);
    }

    // ======================================
    // API ДЛЯ SCHEME
    // ======================================

    setupSchemeStateAPI() {
        const self = this;

        // Получение текущего состояния для Scheme
        BiwaScheme.define_libfunc("js-get-current-state", 0, 0, function() {
            return self.gameState;
        });

        // Обновление состояния из Scheme
        BiwaScheme.define_libfunc("js-set-current-state", 1, 1, function(ar) {
            self.gameState = ar[0];
            return BiwaScheme.undef;
        });
    }

    // ======================================
    // УТИЛИТЫ
    // ======================================

    getCurrentState() {
        return this.gameState;
    }

    isReady() {
        return this.isInitialized;
    }

    setEventEmitter(emitter) {
        this.eventEmitter = emitter;
    }

    getFallbackActions() {
        // Простые действия на случай если Scheme код не работает
        return [
            { type: 'move', location: 'throne_room', text: 'Move to throne room' },
            { type: 'move', location: 'kitchen', text: 'Move to kitchen' },
            { type: 'move', location: 'garden', text: 'Move to garden' },
            { type: 'choice', id: 'explore', text: 'Explore location' },
            { type: 'choice', id: 'rest', text: 'Rest and recover' }
        ];
    }
}

module.exports = SchemeOnlyGameEngine;