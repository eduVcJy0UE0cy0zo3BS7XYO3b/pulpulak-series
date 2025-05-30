/**
 * Quest Engine based on s-expression.js
 * 
 * Installation: npm install s-expression.js
 */

const SExpr = require('s-expression.js');

/**
 * Main Quest Engine for parsing and executing S-expression based quests
 */
class QuestEngine {
    constructor() {
        this.quests = new Map();
        this.parser = new SExpr();
        this.conditions = this.initConditions();
        this.actions = this.initActions();
        this.macros = new Map();
    }

    /**
     * Load and parse a quest definition
     * @param {string} questDefinition - S-expression string defining the quest
     * @returns {Object} Parsed quest object
     */
    loadQuest(questDefinition) {
        try {
            const ast = this.parser.parse(questDefinition);
            const quest = this.parseQuestAST(ast);
            this.quests.set(quest.id, quest);
            return quest;
        } catch (error) {
            throw new Error(`Failed to parse quest: ${error.message}`);
        }
    }

    /**
     * Parse quest AST into quest object
     */
    parseQuestAST(ast) {
        if (!Array.isArray(ast) || ast[0] !== 'quest') {
            throw new Error('Invalid quest definition: must start with (quest ...)');
        }

        const [, questId, ...elements] = ast;
        const quest = {
            id: questId,
            metadata: {},
            triggers: [],
            steps: [],
            onComplete: [],
            variables: new Map()
        };

        for (const element of elements) {
            if (!Array.isArray(element)) continue;

            const [head, ...rest] = element;
            switch (head) {
                case 'metadata':
                    quest.metadata = this.parseMetadata(rest);
                    break;
                case 'triggers':
                    quest.triggers = this.parseTriggers(rest);
                    break;
                case 'variables':
                    quest.variables = this.parseVariables(rest);
                    break;
                case 'steps':
                    quest.steps = this.parseSteps(rest);
                    break;
                case 'on-complete':
                    quest.onComplete = rest;
                    break;
                case 'defmacro':
                    this.defineMacro(rest);
                    break;
            }
        }

        return quest;
    }

    /**
     * Parse metadata section
     */
    parseMetadata(elements) {
        const metadata = {};
        for (const element of elements) {
            if (Array.isArray(element) && element.length === 2) {
                const [key, value] = element;
                metadata[key] = this.cleanValue(value);
            }
        }
        return metadata;
    }
    
    /**
     * Clean value from quotes if it's a string
     */
    cleanValue(value) {
        if (typeof value === 'string' && value.startsWith('"') && value.endsWith('"')) {
            return value.slice(1, -1);
        }
        return value;
    }

    /**
     * Parse variables section
     */
    parseVariables(elements) {
        const vars = new Map();
        for (const element of elements) {
            if (Array.isArray(element) && element.length >= 2) {
                const [name, value] = element;
                // Process the value - if it's an array of strings, clean them
                let processedValue = value;
                if (Array.isArray(value)) {
                    processedValue = value.map(v => this.cleanValue(v));
                } else {
                    processedValue = this.cleanValue(value);
                }
                vars.set(name, processedValue);
            }
        }
        return vars;
    }

    /**
     * Parse triggers
     */
    parseTriggers(elements) {
        return elements.map(trigger => {
            if (!Array.isArray(trigger)) return null;

            const [type, ...params] = trigger;
            
            switch (type) {
                case 'on-dialogue':
                    return {
                        type: 'dialogue',
                        npc: params[0],
                        condition: params.find(p => Array.isArray(p) && p[0] === 'when')?.[1]
                    };
                case 'on-location':
                    return {
                        type: 'location',
                        location: params[0],
                        condition: params.find(p => Array.isArray(p) && p[0] === 'when')?.[1]
                    };
                case 'on-item':
                    return {
                        type: 'item',
                        item: params[0],
                        condition: params.find(p => Array.isArray(p) && p[0] === 'when')?.[1]
                    };
                default:
                    return { type, params };
            }
        }).filter(Boolean);
    }

    /**
     * Parse quest steps
     */
    parseSteps(elements) {
        return elements.map(step => {
            if (!Array.isArray(step)) return null;

            const [type, ...content] = step;

            if (type === 'step') {
                return this.parseStep(content);
            } else if (type === 'branch') {
                return this.parseBranch(content);
            } else if (type === 'loop') {
                return this.parseLoop(content);
            }

            return null;
        }).filter(Boolean);
    }

    /**
     * Parse a single step
     */
    parseStep(content) {
        const [id, ...elements] = content;
        const step = {
            type: 'step',
            id,
            description: '',
            require: [],
            actions: [],
            optional: false
        };

        for (const element of elements) {
            if (!Array.isArray(element)) continue;

            const [head, ...rest] = element;
            switch (head) {
                case 'description':
                    step.description = this.cleanValue(rest[0]);
                    break;
                case 'require':
                    step.require = rest;
                    break;
                case 'actions':
                    step.actions = rest;
                    break;
                case 'optional':
                    step.optional = rest[0] === true || rest[0] === 'true';
                    break;
            }
        }

        return step;
    }

    /**
     * Parse branch structure
     */
    parseBranch(content) {
        const branches = [];
        
        for (const element of content) {
            if (Array.isArray(element) && element[0] === 'case') {
                const [, condition, ...actions] = element;
                branches.push({ condition, actions });
            } else if (Array.isArray(element) && element[0] === 'default') {
                const [, ...actions] = element;
                branches.push({ condition: true, actions });
            }
        }

        return {
            type: 'branch',
            branches
        };
    }

    /**
     * Parse loop structure
     */
    parseLoop(content) {
        const [condition, ...body] = content;
        return {
            type: 'loop',
            condition,
            body
        };
    }

    /**
     * Initialize condition evaluators
     */
    initConditions() {
        return {
            // Comparison operators
            '=': (ctx, a, b) => this.getValue(ctx, a) === this.getValue(ctx, b),
            '!=': (ctx, a, b) => this.getValue(ctx, a) !== this.getValue(ctx, b),
            '>': (ctx, a, b) => this.getValue(ctx, a) > this.getValue(ctx, b),
            '>=': (ctx, a, b) => this.getValue(ctx, a) >= this.getValue(ctx, b),
            '<': (ctx, a, b) => this.getValue(ctx, a) < this.getValue(ctx, b),
            '<=': (ctx, a, b) => this.getValue(ctx, a) <= this.getValue(ctx, b),

            // Logical operators
            'and': (ctx, ...args) => args.every(arg => this.evaluate(ctx, arg)),
            'or': (ctx, ...args) => args.some(arg => this.evaluate(ctx, arg)),
            'not': (ctx, arg) => !this.evaluate(ctx, arg),

            // Game state checks
            'at-location': (ctx, loc) => ctx.gameState.currentLocation === this.getValue(ctx, loc),
            'has-item': (ctx, item) => ctx.gameState.inventory.includes(this.getValue(ctx, item)),
            'has-memory': (ctx, key) => ctx.gameState.memory[this.getValue(ctx, key)] !== undefined,
            'talking-to': (ctx, npc) => ctx.gameState.currentNPC === this.getValue(ctx, npc),
            'outfit-is': (ctx, outfit) => ctx.gameState.currentOutfit === this.getValue(ctx, outfit),
            
            // Quest checks
            'quest-started': (ctx, questId) => ctx.gameState.startedQuests.has(this.getValue(ctx, questId)),
            'quest-completed': (ctx, questId) => ctx.gameState.completedQuests.has(this.getValue(ctx, questId)),
            'quest-step': (ctx, questId, stepId) => {
                const quest = ctx.gameState.activeQuests.get(this.getValue(ctx, questId));
                return quest?.currentStep === this.getValue(ctx, stepId);
            },

            // NPC checks
            'npc-memory': (ctx, npc, key) => {
                const npcName = this.getValue(ctx, npc);
                const memKey = this.getValue(ctx, key);
                return ctx.gameState.npcMemory[npcName]?.[memKey] === true;
            },
            // Removed loyalty system functions

            // Collection operations
            'in': (ctx, item, list) => {
                const itemValue = this.getValue(ctx, item);
                const listValue = this.getValue(ctx, list);
                return Array.isArray(listValue) && listValue.includes(itemValue);
            },
            'all': (ctx, list, predicate) => {
                const listValue = this.getValue(ctx, list);
                return Array.isArray(listValue) && listValue.every(item => {
                    // Create new context with item variable
                    const itemCtx = { ...ctx, localVars: new Map(ctx.localVars) };
                    itemCtx.localVars.set('item', item);
                    return this.evaluate(itemCtx, predicate);
                });
            },
            'any': (ctx, list, predicate) => {
                const listValue = this.getValue(ctx, list);
                return Array.isArray(listValue) && listValue.some(item => {
                    // Create new context with item variable
                    const itemCtx = { ...ctx, localVars: new Map(ctx.localVars) };
                    itemCtx.localVars.set('item', item);
                    return this.evaluate(itemCtx, predicate);
                });
            }
        };
    }

    /**
     * Initialize action handlers
     */
    initActions() {
        return {
            // Memory actions
            'set-memory': (ctx, key, value) => {
                const processedValue = this.getValue(ctx, value);
                // Convert string "true"/"false" to boolean
                let finalValue = processedValue;
                if (processedValue === 'true') finalValue = true;
                else if (processedValue === 'false') finalValue = false;
                ctx.gameState.memory[this.getValue(ctx, key)] = finalValue;
            },
            'set-quest-var': (ctx, key, value) => {
                const varName = this.getValue(ctx, key);
                const varValue = this.getValue(ctx, value);
                if (ctx.questVars) {
                    ctx.questVars.set(varName, varValue);
                }
            },
            'remove-memory': (ctx, key) => {
                delete ctx.gameState.memory[this.getValue(ctx, key)];
            },
            'set-npc-memory': (ctx, npc, key, value) => {
                const npcName = this.getValue(ctx, npc);
                const memKey = this.getValue(ctx, key);
                if (!ctx.gameState.npcMemory[npcName]) {
                    ctx.gameState.npcMemory[npcName] = {};
                }
                ctx.gameState.npcMemory[npcName][memKey] = this.getValue(ctx, value);
            },

            // Dialogue actions
            'add-dialogue': (ctx, npc, ...optionData) => {
                const npcName = this.getValue(ctx, npc);
                const option = this.parseDialogueOption(ctx, optionData);
                if (ctx.onAddDialogue) {
                    ctx.onAddDialogue(npcName, option);
                }
            },
            'remove-dialogue': (ctx, npc, optionId) => {
                const npcName = this.getValue(ctx, npc);
                const id = this.getValue(ctx, optionId);
                if (ctx.onRemoveDialogue) {
                    ctx.onRemoveDialogue(npcName, id);
                }
            },

            // World state actions
            'reveal-location': (ctx, location) => {
                const loc = this.getValue(ctx, location);
                ctx.gameState.revealedLocations.add(loc);
            },
            'hide-location': (ctx, location) => {
                const loc = this.getValue(ctx, location);
                ctx.gameState.revealedLocations.delete(loc);
            },
            'spawn-item': (ctx, location, item) => {
                const loc = this.getValue(ctx, location);
                const itemId = this.getValue(ctx, item);
                if (ctx.onSpawnItem) {
                    ctx.onSpawnItem(loc, itemId);
                }
            },

            // Inventory actions
            'give-items': (ctx, ...items) => {
                items.forEach(item => {
                    ctx.gameState.inventory.push(this.getValue(ctx, item));
                });
            },
            'take-items': (ctx, ...items) => {
                items.forEach(item => {
                    const itemId = this.getValue(ctx, item);
                    const index = ctx.gameState.inventory.indexOf(itemId);
                    if (index !== -1) {
                        ctx.gameState.inventory.splice(index, 1);
                    }
                });
            },

            // Quest flow actions
            'complete-quest': (ctx) => {
                if (ctx.currentQuest) {
                    ctx.currentQuest.completed = true;
                    ctx.gameState.completedQuests.add(ctx.currentQuest.id);
                    ctx.gameState.activeQuests.delete(ctx.currentQuest.id);
                }
            },
            'fail-quest': (ctx, reason) => {
                if (ctx.currentQuest) {
                    ctx.currentQuest.failed = true;
                    ctx.currentQuest.failReason = this.getValue(ctx, reason);
                    ctx.gameState.activeQuests.delete(ctx.currentQuest.id);
                }
            },
            'start-quest': (ctx, questId) => {
                const id = this.getValue(ctx, questId);
                if (ctx.onStartQuest) {
                    ctx.onStartQuest(id);
                }
            },
            'goto-step': (ctx, stepId) => {
                if (ctx.currentQuest) {
                    ctx.currentQuest.currentStep = this.getValue(ctx, stepId);
                }
            },

            // Character actions (loyalty system removed)
            'set-outfit': (ctx, outfit) => {
                ctx.gameState.currentOutfit = this.getValue(ctx, outfit);
            },
            'trigger-scene': (ctx, sceneId) => {
                if (ctx.onTriggerScene) {
                    ctx.onTriggerScene(this.getValue(ctx, sceneId));
                }
            },
            'show-message': (ctx, message) => {
                if (ctx.onShowMessage) {
                    ctx.onShowMessage(this.getValue(ctx, message));
                }
            },

            // Control flow
            'if': (ctx, condition, thenBranch, elseBranch) => {
                if (this.evaluate(ctx, condition)) {
                    this.executeActions(ctx, [thenBranch]);
                } else if (elseBranch) {
                    this.executeActions(ctx, [elseBranch]);
                }
            },
            'cond': (ctx, ...branches) => {
                for (const branch of branches) {
                    if (Array.isArray(branch) && branch.length >= 2) {
                        const [condition, ...actions] = branch;
                        if (this.evaluate(ctx, condition)) {
                            this.executeActions(ctx, actions);
                            break;
                        }
                    }
                }
            },
            'progn': (ctx, ...actions) => {
                this.executeActions(ctx, actions);
            },
            'let': (ctx, bindings, ...body) => {
                const newCtx = { ...ctx, localVars: new Map(ctx.localVars) };
                
                // Set local variables
                for (const binding of bindings) {
                    if (Array.isArray(binding) && binding.length === 2) {
                        const [name, value] = binding;
                        newCtx.localVars.set(name, this.getValue(ctx, value));
                    }
                }
                
                // Execute body with new context
                this.executeActions(newCtx, body);
            }
        };
    }

    /**
     * Define a macro
     */
    defineMacro(content) {
        const [name, params, ...body] = content;
        this.macros.set(name, { params, body });
    }

    /**
     * Expand macro
     */
    expandMacro(ctx, name, args) {
        const macro = this.macros.get(name);
        if (!macro) return null;

        const newCtx = { ...ctx, localVars: new Map(ctx.localVars) };
        
        // Bind arguments to parameters
        macro.params.forEach((param, index) => {
            newCtx.localVars.set(param, args[index]);
        });

        return macro.body.map(expr => this.substituteVars(newCtx, expr));
    }

    /**
     * Substitute variables in expression
     */
    substituteVars(ctx, expr) {
        if (typeof expr === 'string' && expr.startsWith('$')) {
            const varName = expr.substring(1);
            if (ctx.localVars.has(varName)) {
                return ctx.localVars.get(varName);
            }
        }
        
        if (Array.isArray(expr)) {
            return expr.map(e => this.substituteVars(ctx, e));
        }
        
        return expr;
    }

    /**
     * Get value from context
     */
    getValue(ctx, expr) {
        // Clean string literals
        if (typeof expr === 'string') {
            // Handle variables
            if (expr.startsWith('$')) {
                const varName = expr.substring(1);
                if (ctx.localVars && ctx.localVars.has(varName)) {
                    return ctx.localVars.get(varName);
                }
                if (ctx.questVars && ctx.questVars.has(varName)) {
                    return ctx.questVars.get(varName);
                }
            }
            // Remove quotes from string literals
            return this.cleanValue(expr);
        }

        // Handle list access
        if (Array.isArray(expr) && expr[0] === 'get') {
            const [, list, index] = expr;
            const listValue = this.getValue(ctx, list);
            const indexValue = this.getValue(ctx, index);
            return listValue[indexValue];
        }

        // Handle arithmetic
        if (Array.isArray(expr)) {
            const [op, ...args] = expr;
            switch (op) {
                case '+': return args.reduce((a, b) => this.getValue(ctx, a) + this.getValue(ctx, b));
                case '-': return args.reduce((a, b) => this.getValue(ctx, a) - this.getValue(ctx, b));
                case '*': return args.reduce((a, b) => this.getValue(ctx, a) * this.getValue(ctx, b));
                case '/': return args.reduce((a, b) => this.getValue(ctx, a) / this.getValue(ctx, b));
            }
        }

        return expr;
    }

    /**
     * Evaluate expression
     */
    evaluate(ctx, expr) {
        if (!Array.isArray(expr)) {
            return this.getValue(ctx, expr);
        }

        const [op, ...args] = expr;
        
        // Check for macro
        if (this.macros.has(op)) {
            const expanded = this.expandMacro(ctx, op, args);
            return this.evaluate(ctx, expanded[0]);
        }

        // Check for condition
        if (this.conditions[op]) {
            return this.conditions[op](ctx, ...args);
        }

        throw new Error(`Unknown operator: ${op}`);
    }

    /**
     * Execute action
     */
    executeAction(ctx, action) {
        if (!Array.isArray(action)) return;

        const [op, ...args] = action;

        // Check for macro
        if (this.macros.has(op)) {
            const expanded = this.expandMacro(ctx, op, args);
            expanded.forEach(act => this.executeAction(ctx, act));
            return;
        }

        // Check for action
        if (this.actions[op]) {
            this.actions[op](ctx, ...args);
        } else {
            console.warn(`Unknown action: ${op}`);
        }
    }

    /**
     * Execute multiple actions
     */
    executeActions(ctx, actions) {
        for (const action of actions) {
            this.executeAction(ctx, action);
        }
    }

    /**
     * Parse dialogue option
     */
    parseDialogueOption(ctx, optionData) {
        // Simple format: (option id text response) or (option id text response (quest_action action))
        if (optionData.length >= 3 && typeof optionData[0] === 'string') {
            const option = {
                id: this.getValue(ctx, optionData[0]),
                text: this.getValue(ctx, optionData[1]),
                response: this.getValue(ctx, optionData[2])
            };
            
            // Check for quest_action parameter
            if (optionData.length > 3 && Array.isArray(optionData[3]) && optionData[3][0] === 'quest_action') {
                option.quest_action = this.getValue(ctx, optionData[3][1]);
            }
            
            return option;
        }

        // Complex format with nested structure
        const option = {};
        for (const element of optionData) {
            if (Array.isArray(element)) {
                const [key, ...values] = element;
                switch (key) {
                    case 'option':
                        option.id = values[0];
                        option.text = values[1];
                        option.response = values[2];
                        break;
                    case 'when':
                        option.condition = values[0];
                        break;
                    case 'effects':
                        option.effects = values;
                        break;
                    case 'quest_action':
                        option.quest_action = this.getValue(ctx, values[0]);
                        break;
                }
            }
        }
        return option;
    }
}

module.exports = QuestEngine;