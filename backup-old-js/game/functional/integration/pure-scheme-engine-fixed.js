/**
 * Исправленный чистый Scheme функциональный движок на BiwaScheme
 * game/functional/integration/pure-scheme-engine-fixed.js
 */

const BiwaScheme = require('biwascheme');
const fs = require('fs');
const path = require('path');

class PureSchemeGameEngine {
    constructor() {
        // Инициализация BiwaScheme интерпретатора
        this.biwa = new BiwaScheme.Interpreter();
        this.gameState = null;
        this.isInitialized = false;
        
        // Инициализация при создании
        this.initialize();
    }

    /**
     * Инициализация Scheme среды
     */
    async initialize() {
        try {
            console.log('[PureSchemeEngine] Initializing BiwaScheme environment...');
            
            // Загружаем Scheme код из файлов
            await this.loadSchemeLibraries();
            
            // Регистрируем JavaScript функции для Scheme
            this.setupSchemeAPI();
            
            // Создаем начальное состояние игры
            await this.initializeGameState();
            
            this.isInitialized = true;
            console.log('[PureSchemeEngine] Initialization completed successfully');
            
        } catch (error) {
            console.error('[PureSchemeEngine] Initialization failed:', error);
            throw error;
        }
    }

    /**
     * Загрузка Scheme библиотек
     */
    async loadSchemeLibraries() {
        // Используем встроенный game-core.scm если доступен
        try {
            const gameCorePath = 'game/functional/core/game-core.scm';
            if (fs.existsSync(gameCorePath)) {
                console.log(`[PureSchemeEngine] Loading ${gameCorePath}...`);
                const schemeCode = fs.readFileSync(gameCorePath, 'utf8');
                await this.biwa.evaluate(schemeCode);
                console.log(`[PureSchemeEngine] Successfully loaded ${gameCorePath}`);
                return;
            }
        } catch (error) {
            console.warn('[PureSchemeEngine] Error loading game-core.scm:', error.message);
        }

        // Fallback: загружаем базовые определения
        await this.loadBasicDefinitions();
    }

    /**
     * Загрузка базовых определений если файлы недоступны
     */
    async loadBasicDefinitions() {
        console.log('[PureSchemeEngine] Loading basic Scheme definitions...');
        
        await this.biwa.evaluate(`
            ;; Базовые определения состояния игры
            (define (make-initial-state room-id players)
              (list 'game-state
                    'room-id room-id
                    'scene 'coop_awakening
                    'characters (list 
                      (list 'princess 
                            'id 'princess
                            'location 'princess_chamber
                            'outfit 'princess_dress
                            'inventory '()
                            'stats (list (cons 'loyalty 50) (cons 'knowledge 30) (cons 'charm 70)))
                      (list 'helper
                            'id 'helper
                            'location 'princess_chamber  
                            'outfit 'common_dress
                            'inventory '(translation_earrings voice_medallion)
                            'stats (list (cons 'loyalty 50) (cons 'knowledge 60) (cons 'charm 40))))
                    'world (list 'time 'early_morning 'events '())
                    'quests (list 'active '() 'completed '() 'memory '())))
            
            (define (game-state? obj)
              (and (list? obj) (eq? (car obj) 'game-state)))
            
            (define (get-value state key)
              (let ((pair (assq key (cdr state))))
                (if pair (cadr pair) #f)))
            
            (define (get-character-data state character-id)
              (let ((characters (get-value state 'characters)))
                (assq character-id characters)))
            
            (define (get-character-value character-data key)
              (let ((pair (assq key (cdr character-data))))
                (if pair (cadr pair) #f)))
            
            ;; Предикаты состояния
            (define (character-at-location? state character-id location)
              (let ((character (get-character-data state character-id)))
                (and character
                     (eq? (get-character-value character 'location) location))))
            
            (define (character-has-item? state character-id item)
              (let ((character (get-character-data state character-id)))
                (and character
                     (member item (get-character-value character 'inventory)))))
            
            (define (character-can-move? state character-id location)
              (and (get-character-data state character-id)
                   (not (character-at-location? state character-id location))))
            
            ;; Трансформеры состояния
            (define (move-character state character-id new-location)
              (let* ((characters (get-value state 'characters))
                     (character (assq character-id characters))
                     (updated-char (list character-id
                                       'id character-id
                                       'location new-location
                                       'outfit (get-character-value character 'outfit)
                                       'inventory (get-character-value character 'inventory)
                                       'stats (get-character-value character 'stats)))
                     (updated-characters (cons updated-char
                                             (filter (lambda (c) (not (eq? (car c) character-id))) characters))))
                (cons 'game-state
                      (map (lambda (pair)
                             (if (eq? (car pair) 'characters)
                               (list 'characters updated-characters)
                               pair))
                           (cdr state)))))
            
            ;; Валидация действий
            (define (validate-action state character action)
              (cond
                ((not (get-character-data state character)) #f)
                ((not (list? action)) #f)
                ((not (>= (length action) 1)) #f)
                (else
                  (let ((action-type (car action)))
                    (case action-type
                      ((move)
                       (if (>= (length action) 2)
                         (let ((location (cadr action)))
                           (character-can-move? state character location))
                         #f))
                      ((interact)
                       (if (>= (length action) 2)
                         #t  ; Базовая валидация для взаимодействий
                         #f))
                      ((choice dialogue)
                       #t) ; Базовая валидация для выборов и диалогов
                      (else #f))))))
            
            ;; Обработка игровых действий
            (define (process-game-action state character action)
              (if (validate-action state character action)
                (let ((action-type (car action)))
                  (case action-type
                    ((move)
                     (let ((location (cadr action)))
                       (js-log (string-append (symbol->string character) " moves to " (symbol->string location)))
                       (emit-event! 'character-moved (list character location))
                       (move-character state character location)))
                    ((interact)
                     (let ((npc-id (cadr action)))
                       (js-log (string-append (symbol->string character) " interacts with " (symbol->string npc-id)))
                       (emit-event! 'interaction (list character npc-id))
                       state)) ; Базовая реализация - состояние не меняется
                    ((choice)
                     (let ((choice-id (cadr action)))
                       (js-log (string-append (symbol->string character) " makes choice: " (symbol->string choice-id)))
                       (emit-event! 'choice-made (list character choice-id))
                       state)) ; Базовая реализация
                    (else state)))
                (begin
                  (js-log "Invalid action")
                  state)))
            
            ;; Получение доступных действий
            (define (get-available-actions state character)
              (let* ((character-data (get-character-data state character))
                     (current-location (get-character-value character-data 'location)))
                ;; Возвращаем базовые действия
                (list 
                  (list 'move 'throne_room "Move to throne room")
                  (list 'move 'kitchen "Move to kitchen")
                  (list 'move 'garden "Move to garden")
                  (list 'move 'library "Move to library")
                  (list 'move 'village "Move to village")
                  (list 'choice 'explore "Explore current location")
                  (list 'choice 'rest "Rest and recover"))))
            
            ;; Вспомогательные функции
            (define (symbol->string sym)
              (if (symbol? sym)
                sym ; Упрощенная конвертация - BiwaScheme может обрабатывать это автоматически
                (if (string? sym) sym "unknown")))
            
            (define (string-append . strings)
              (fold-left string-concat "" strings))
            
            (define (string-concat s1 s2)
              s1) ; Упрощенная реализация
            
            (define (fold-left proc init lst)
              (if (null? lst)
                init
                (fold-left proc (proc init (car lst)) (cdr lst))))
            
            (define (filter pred lst)
              (cond
                ((null? lst) '())
                ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
                (else (filter pred (cdr lst)))))
        `);
        
        console.log('[PureSchemeEngine] Basic definitions loaded');
    }

    /**
     * Настройка API для взаимодействия с JavaScript
     */
    setupSchemeAPI() {
        // Сохраняем ссылку на this для использования в замыканиях
        const self = this;
        
        // Логирование
        BiwaScheme.define_libfunc("js-log", 1, 1, function(ar) {
            const message = self.schemeValueToJS(ar[0]);
            console.log('[Scheme]', message);
            return BiwaScheme.undef;
        });

        // Получение текущего состояния
        BiwaScheme.define_libfunc("get-current-state", 0, 0, function() {
            return self.jsValueToScheme(self.gameState);
        });

        // Сохранение состояния
        BiwaScheme.define_libfunc("save-state!", 1, 1, function(ar) {
            self.gameState = self.schemeValueToJS(ar[0]);
            return BiwaScheme.undef;
        });

        // Эмиссия событий
        BiwaScheme.define_libfunc("emit-event!", 2, 2, function(ar) {
            const eventType = self.schemeValueToJS(ar[0]);
            const eventData = self.schemeValueToJS(ar[1]);
            self.emitEvent(eventType, eventData);
            return BiwaScheme.undef;
        });

        // Валидация действий
        BiwaScheme.define_libfunc("validate-action", 3, 3, function(ar) {
            const state = self.schemeValueToJS(ar[0]);
            const character = self.schemeValueToJS(ar[1]);
            const action = self.schemeValueToJS(ar[2]);
            
            // Базовая валидация
            const isValid = self.validateActionBasic(state, character, action);
            return isValid ? BiwaScheme.true : BiwaScheme.false;
        });

        console.log('[PureSchemeEngine] JavaScript API registered');
    }

    /**
     * Инициализация начального состояния игры
     */
    async initializeGameState() {
        try {
            const result = await this.biwa.evaluate(`
                (make-initial-state "default-room" 
                                  '((princess . "player1") 
                                    (helper . "player2")))
            `);
            
            this.gameState = this.schemeValueToJS(result);
            console.log('[PureSchemeEngine] Initial game state created');
        } catch (error) {
            console.error('[PureSchemeEngine] Failed to create initial state:', error);
            throw error;
        }
    }

    /**
     * Обработка игрового действия
     */
    async processAction(action, character) {
        if (!this.isInitialized) {
            throw new Error('Engine not initialized');
        }

        try {
            // Преобразуем действие в список для Scheme
            const actionList = this.actionToSchemeList(action);
            const characterSymbol = typeof character === 'string' ? character : character.toString();
            
            const result = await this.biwa.evaluate(`
                (let* ((action '${actionList})
                       (character '${characterSymbol})
                       (current-state (get-current-state))
                       (valid? (validate-action current-state character action)))
                  (if valid?
                    (let ((new-state (process-game-action current-state character action)))
                      (save-state! new-state)
                      (list 'success "Action processed successfully"))
                    (list 'error "Invalid action")))
            `);

            const jsResult = this.schemeValueToJS(result);
            
            if (jsResult && jsResult[0] === 'success') {
                return {
                    success: true,
                    result: { message: jsResult[1] },
                    newState: this.gameState
                };
            } else {
                return {
                    success: false,
                    error: jsResult ? jsResult[1] : 'Unknown error'
                };
            }
        } catch (error) {
            console.error('[PureSchemeEngine] Error processing action:', error);
            return {
                success: false,
                error: error.message
            };
        }
    }

    /**
     * Получение доступных действий
     */
    async getAvailableActions(character) {
        if (!this.isInitialized) {
            return [];
        }

        try {
            const characterSymbol = typeof character === 'string' ? character : character.toString();
            
            const result = await this.biwa.evaluate(`
                (let ((character '${characterSymbol})
                      (current-state (get-current-state)))
                  (get-available-actions current-state character))
            `);
            
            return this.schemeValueToJS(result) || [];
        } catch (error) {
            console.error('[PureSchemeEngine] Error getting available actions:', error);
            
            // Возвращаем базовые действия при ошибке
            return this.getBasicActions(character);
        }
    }

    /**
     * Валидация действия
     */
    async validateAction(action, character) {
        if (!this.isInitialized) {
            return false;
        }

        try {
            const actionList = this.actionToSchemeList(action);
            const characterSymbol = typeof character === 'string' ? character : character.toString();
            
            const result = await this.biwa.evaluate(`
                (let ((action '${actionList})
                      (character '${characterSymbol})
                      (current-state (get-current-state)))
                  (validate-action current-state character action))
            `);
            
            return result === BiwaScheme.true;
        } catch (error) {
            console.error('[PureSchemeEngine] Error validating action:', error);
            return this.validateActionBasic(this.gameState, character, action);
        }
    }

    /**
     * Преобразование действия в Scheme список
     */
    actionToSchemeList(action) {
        if (!action || typeof action !== 'object') {
            return '()';
        }

        const type = action.type || 'unknown';
        
        switch (type) {
            case 'move':
                return `(move ${action.location || 'unknown'})`;
            case 'choice':
                return `(choice ${action.id || 'unknown'})`;
            case 'interact':
                return `(interact ${action.npc || 'unknown'})`;
            default:
                return `(${type})`;
        }
    }

    /**
     * Конвертация JavaScript значений в Scheme (исправленная версия)
     */
    jsValueToScheme(value) {
        if (value === null || value === undefined) {
            return BiwaScheme.nil;
        }
        if (typeof value === 'boolean') {
            return value ? BiwaScheme.true : BiwaScheme.false;
        }
        if (typeof value === 'number') {
            // Простое решение - возвращаем число как есть, BiwaScheme должен его понять
            return value;
        }
        if (typeof value === 'string') {
            // Простое решение - возвращаем строку как есть, BiwaScheme должен ее понять
            return value;
        }
        if (Array.isArray(value)) {
            try {
                const schemePairs = value.map(item => this.jsValueToScheme(item));
                if (BiwaScheme.array_to_list) {
                    return BiwaScheme.array_to_list(schemePairs);
                } else {
                    return schemePairs; // Fallback
                }
            } catch (error) {
                console.warn('[PureSchemeEngine] Error converting array to Scheme:', error.message);
                return BiwaScheme.nil;
            }
        }
        if (typeof value === 'object') {
            try {
                // Упрощенная конвертация объектов
                const pairs = Object.entries(value).map(([k, v]) => {
                    return [k, this.jsValueToScheme(v)];
                });
                return pairs;
            } catch (error) {
                console.warn('[PureSchemeEngine] Error converting object to Scheme:', error.message);
                return BiwaScheme.nil;
            }
        }
        return value;
    }

    /**
     * Конвертация Scheme значений в JavaScript (исправленная версия)
     */
    schemeValueToJS(value) {
        if (value === BiwaScheme.nil || value === null || value === undefined) {
            return null;
        }
        if (value === BiwaScheme.true) {
            return true;
        }
        if (value === BiwaScheme.false) {
            return false;
        }
        if (value === BiwaScheme.undef) {
            return undefined;
        }
        
        // Проверка на BiwaScheme типы более безопасно
        if (value && typeof value === 'object') {
            // Обработка символов BiwaScheme
            if (value.name && typeof value.name === 'string') {
                return value.name; // Символ BiwaScheme
            }
            if (value.constructor && value.constructor.name === 'Number') {
                return value.value;
            }
            if (value.constructor && value.constructor.name === 'Str') {
                return value.str;
            }
            if (value.constructor && value.constructor.name === 'Pair') {
                return this.pairToJS(value);
            }
        }
        
        // Для примитивных типов
        if (typeof value === 'number' || typeof value === 'string' || typeof value === 'boolean') {
            return value;
        }
        
        // Для массивов
        if (Array.isArray(value)) {
            return value.map(item => this.schemeValueToJS(item));
        }
        
        return value;
    }

    /**
     * Конвертация Scheme пары в JavaScript
     */
    pairToJS(pair) {
        const result = [];
        let current = pair;
        
        // Безопасная конвертация пар
        while (current && current !== BiwaScheme.nil) {
            if (current.car !== undefined) {
                result.push(this.schemeValueToJS(current.car));
                current = current.cdr;
            } else {
                break;
            }
        }
        
        return result;
    }

    /**
     * Базовая валидация действий
     */
    validateActionBasic(state, character, action) {
        if (!state || !state.characters || !action || !action.type) {
            return false;
        }

        // Ищем персонажа в массиве
        let characterData = null;
        if (Array.isArray(state.characters)) {
            for (const charPair of state.characters) {
                if (Array.isArray(charPair) && charPair[0] === character) {
                    characterData = charPair;
                    break;
                }
            }
        }

        if (!characterData) {
            return false;
        }

        switch (action.type) {
            case 'move':
                return action.location && typeof action.location === 'string';
            case 'choice':
                return action.id && typeof action.id === 'string';
            case 'interact':
                return action.npc && typeof action.npc === 'string';
            default:
                return true;
        }
    }

    /**
     * Получение базовых действий
     */
    getBasicActions(character) {
        const locations = ['throne_room', 'kitchen', 'garden', 'library', 'village'];
        const actions = [];

        for (const location of locations) {
            actions.push({
                type: 'move',
                id: `move_to_${location}`,
                text: `Move to ${location}`,
                location: location
            });
        }

        actions.push({
            type: 'choice',
            id: 'explore',
            text: 'Explore current location'
        });

        return actions;
    }

    /**
     * Эмиссия событий
     */
    emitEvent(type, data) {
        console.log(`[PureSchemeEngine] Event: ${type}`, data);
        if (this.eventEmitter) {
            this.eventEmitter.emit('gameEvent', { type, data });
        }
    }

    /**
     * Установка обработчика событий
     */
    setEventEmitter(emitter) {
        this.eventEmitter = emitter;
    }

    /**
     * Получение текущего состояния
     */
    getCurrentState() {
        return this.gameState;
    }

    /**
     * Проверка готовности движка
     */
    isReady() {
        return this.isInitialized;
    }
}

module.exports = PureSchemeGameEngine;