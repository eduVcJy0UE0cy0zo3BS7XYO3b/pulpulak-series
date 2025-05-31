/**
 * Чистый Scheme функциональный движок на BiwaScheme
 * game/functional/integration/pure-scheme-engine.js
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
        // Загружаем библиотеки напрямую как строки и выполняем
        const libraries = [
            'game/functional/core/game-state.scm',
            'game/functional/core/state-transformers.scm',
            'game/functional/core/effects.scm',
            'game/functional/domains/character.scm',
            'game/functional/domains/quest.scm'
        ];

        for (const libPath of libraries) {
            try {
                console.log(`[PureSchemeEngine] Loading ${libPath}...`);
                
                if (fs.existsSync(libPath)) {
                    const schemeCode = fs.readFileSync(libPath, 'utf8');
                    
                    // Выполняем Scheme код напрямую
                    await this.biwa.evaluate(schemeCode);
                    console.log(`[PureSchemeEngine] Successfully loaded ${libPath}`);
                } else {
                    console.warn(`[PureSchemeEngine] Library not found: ${libPath}`);
                    // Загружаем базовые определения вместо файла
                    await this.loadBasicDefinitions(libPath);
                }
            } catch (error) {
                console.error(`[PureSchemeEngine] Error loading ${libPath}:`, error.message);
                // Продолжаем с базовыми определениями
                await this.loadBasicDefinitions(libPath);
            }
        }
    }

    /**
     * Загрузка базовых определений если файлы недоступны
     */
    async loadBasicDefinitions(libPath) {
        const baseName = path.basename(libPath, '.scm');
        
        switch (baseName) {
            case 'game-state':
                await this.biwa.evaluate(`
                    ;; Базовые определения состояния игры
                    (define (make-initial-state room-id players)
                      (list 'game-state
                            'scene 'coop_awakening
                            'characters (list 
                              (cons 'princess 
                                    (list 'id 'princess
                                          'location 'princess_chamber
                                          'outfit 'princess_dress
                                          'inventory '()
                                          'stats '((loyalty . 50) (knowledge . 30) (charm . 70))))
                              (cons 'helper
                                    (list 'id 'helper
                                          'location 'princess_chamber  
                                          'outfit 'common_dress
                                          'inventory '(translation_earrings voice_medallion)
                                          'stats '((loyalty . 50) (knowledge . 60) (charm . 40)))))
                            'world (list 'time 'early_morning 'events '())
                            'quests (list 'active '() 'completed '() 'memory '())))
                    
                    (define (game-state? obj)
                      (and (list? obj) (eq? (car obj) 'game-state)))
                    
                    (define (get-character-data state character-id)
                      (let ((characters (cdr (assq 'characters state))))
                        (cdr (assq character-id characters))))
                `);
                break;
                
            case 'state-transformers':
                await this.biwa.evaluate(`
                    ;; Базовые трансформеры состояния
                    (define (move-character state character-id new-location)
                      (let* ((characters (cdr (assq 'characters state)))
                             (character (assq character-id characters))
                             (updated-char (list 'id character-id
                                               'location new-location
                                               'outfit (cdr (assq 'outfit (cdr character)))
                                               'inventory (cdr (assq 'inventory (cdr character)))
                                               'stats (cdr (assq 'stats (cdr character)))))
                             (updated-characters (cons (cons character-id updated-char)
                                                     (filter (lambda (c) (not (eq? (car c) character-id))) characters))))
                        (cons 'game-state
                              (map (lambda (pair)
                                     (if (eq? (car pair) 'characters)
                                       (cons 'characters updated-characters)
                                       pair))
                                   (cdr state)))))
                    
                    (define (add-item-to-character state character-id item)
                      (let* ((characters (cdr (assq 'characters state)))
                             (character (assq character-id characters))
                             (char-data (cdr character))
                             (current-inventory (cdr (assq 'inventory char-data)))
                             (updated-char (list 'id character-id
                                               'location (cdr (assq 'location char-data))
                                               'outfit (cdr (assq 'outfit char-data))
                                               'inventory (cons item current-inventory)
                                               'stats (cdr (assq 'stats char-data))))
                             (updated-characters (cons (cons character-id updated-char)
                                                     (filter (lambda (c) (not (eq? (car c) character-id))) characters))))
                        (cons 'game-state
                              (map (lambda (pair)
                                     (if (eq? (car pair) 'characters)
                                       (cons 'characters updated-characters)
                                       pair))
                                   (cdr state)))))
                `);
                break;
                
            case 'effects':
                await this.biwa.evaluate(`
                    ;; Базовая система эффектов
                    (define (return-effect value)
                      (lambda (state)
                        (list value state '())))
                    
                    (define (bind-effect effect f)
                      (lambda (state)
                        (let* ((result (effect state))
                               (value (car result))
                               (new-state (cadr result))
                               (side-effects (caddr result))
                               (next-result ((f value) new-state))
                               (final-value (car next-result))
                               (final-state (cadr next-result))
                               (next-side-effects (caddr next-result)))
                          (list final-value 
                                final-state 
                                (append side-effects next-side-effects)))))
                    
                    (define (run-effect effect initial-state)
                      (effect initial-state))
                    
                    (define (log-effect message)
                      (lambda (state)
                        (js-log message)
                        (list #t state (list (list 'log message)))))
                `);
                break;
                
            case 'character':
                await this.biwa.evaluate(`
                    ;; Базовая логика персонажей
                    (define (character-at-location? state character-id location)
                      (let ((character (get-character-data state character-id)))
                        (and character
                             (eq? (cdr (assq 'location character)) location))))
                    
                    (define (character-has-item? state character-id item)
                      (let ((character (get-character-data state character-id)))
                        (and character
                             (member item (cdr (assq 'inventory character))))))
                    
                    (define (character-can-move? state character-id location)
                      (and (get-character-data state character-id)
                           (not (character-at-location? state character-id location))))
                `);
                break;
                
            case 'quest':
                await this.biwa.evaluate(`
                    ;; Базовая система квестов
                    (define (quest-active? state character-id quest-id)
                      (let ((quests (cdr (assq 'quests state))))
                        (let ((active-quests (cdr (assq 'active quests))))
                          (not (null? (assq quest-id active-quests))))))
                    
                    (define (quest-completed? state character-id quest-id)
                      (let ((quests (cdr (assq 'quests state))))
                        (let ((completed-quests (cdr (assq 'completed quests))))
                          (not (null? (assq quest-id completed-quests))))))
                `);
                break;
        }
        
        console.log(`[PureSchemeEngine] Loaded basic definitions for ${baseName}`);
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
            const schemeAction = this.jsValueToScheme(action);
            const schemeCharacter = this.jsValueToScheme(character);
            
            const result = await this.biwa.evaluate(`
                (let* ((action ${this.schemeValueToString(schemeAction)})
                       (character ${this.schemeValueToString(schemeCharacter)})
                       (current-state (get-current-state))
                       (valid? (validate-action current-state character action)))
                  (if valid?
                    (let ((new-state (process-game-action current-state character action)))
                      (save-state! new-state)
                      (list 'success "Action processed successfully"))
                    (list 'error "Invalid action")))
            `);

            const jsResult = this.schemeValueToJS(result);
            
            if (jsResult[0] === 'success') {
                return {
                    success: true,
                    result: { message: jsResult[1] },
                    newState: this.gameState
                };
            } else {
                return {
                    success: false,
                    error: jsResult[1]
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
            const schemeCharacter = this.jsValueToScheme(character);
            
            const result = await this.biwa.evaluate(`
                (let ((character ${this.schemeValueToString(schemeCharacter)})
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
            const schemeAction = this.jsValueToScheme(action);
            const schemeCharacter = this.jsValueToScheme(character);
            
            const result = await this.biwa.evaluate(`
                (let ((action ${this.schemeValueToString(schemeAction)})
                      (character ${this.schemeValueToString(schemeCharacter)})
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
     * Конвертация JavaScript значений в Scheme
     */
    jsValueToScheme(value) {
        if (value === null || value === undefined) {
            return BiwaScheme.nil;
        }
        if (typeof value === 'boolean') {
            return value ? BiwaScheme.true : BiwaScheme.false;
        }
        if (typeof value === 'number') {
            return new BiwaScheme.Number(value);
        }
        if (typeof value === 'string') {
            return new BiwaScheme.Str(value);
        }
        if (Array.isArray(value)) {
            const schemePairs = value.map(item => this.jsValueToScheme(item));
            return BiwaScheme.array_to_list(schemePairs);
        }
        if (typeof value === 'object') {
            const pairs = Object.entries(value).map(([k, v]) => 
                new BiwaScheme.Pair(new BiwaScheme.Str(k), this.jsValueToScheme(v))
            );
            return BiwaScheme.array_to_list(pairs);
        }
        return value;
    }

    /**
     * Конвертация Scheme значений в JavaScript
     */
    schemeValueToJS(value) {
        if (value === BiwaScheme.nil) {
            return null;
        }
        if (value === BiwaScheme.true) {
            return true;
        }
        if (value === BiwaScheme.false) {
            return false;
        }
        if (value instanceof BiwaScheme.Number) {
            return value.value;
        }
        if (value instanceof BiwaScheme.Str) {
            return value.str;
        }
        if (value instanceof BiwaScheme.Pair) {
            return this.pairToJS(value);
        }
        return value;
    }

    /**
     * Конвертация Scheme пары в JavaScript
     */
    pairToJS(pair) {
        const result = [];
        let current = pair;
        
        while (current !== BiwaScheme.nil && current instanceof BiwaScheme.Pair) {
            result.push(this.schemeValueToJS(current.car));
            current = current.cdr;
        }
        
        return result;
    }

    /**
     * Конвертация значения в строку для выполнения в Scheme
     */
    schemeValueToString(value) {
        if (value === null || value === undefined) {
            return "'()";
        }
        if (typeof value === 'boolean') {
            return value ? '#t' : '#f';
        }
        if (typeof value === 'number') {
            return value.toString();
        }
        if (typeof value === 'string') {
            return `"${value.replace(/"/g, '\\"')}"`;
        }
        if (Array.isArray(value)) {
            const elements = value.map(item => this.schemeValueToString(item));
            return `(${elements.join(' ')})`;
        }
        if (typeof value === 'object') {
            const pairs = Object.entries(value)
                .map(([k, v]) => `(${k} . ${this.schemeValueToString(v)})`)
                .join(' ');
            return `(${pairs})`;
        }
        return value.toString();
    }

    /**
     * Базовая валидация действий
     */
    validateActionBasic(state, character, action) {
        if (!state || !state.characters || !action || !action.type) {
            return false;
        }

        const characterData = state.characters.find(c => c[0] === character);
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