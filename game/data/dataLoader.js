/**
 * Data Loader for S-expression game data files
 */

const fs = require('fs');
const path = require('path');
const SExpr = require('s-expression.js');

class DataLoader {
    constructor() {
        this.parser = new SExpr();
        this.dataCache = new Map();
    }

    /**
     * Clean string value from quotes
     */
    cleanValue(value) {
        if (typeof value === 'string' && value.startsWith('"') && value.endsWith('"')) {
            return value.slice(1, -1);
        }
        return value;
    }

    /**
     * Load and parse an S-expression data file
     */
    loadSExpFile(filePath) {
        if (this.dataCache.has(filePath)) {
            return this.dataCache.get(filePath);
        }

        let content = fs.readFileSync(filePath, 'utf-8');
        
        // Remove comments and clean up
        content = content.split('\n')
            .filter(line => !line.trim().startsWith(';;') && line.trim().length > 0)
            .map(line => {
                const commentIndex = line.indexOf(';;');
                if (commentIndex !== -1) {
                    return line.substring(0, commentIndex).trimEnd();
                }
                return line;
            })
            .filter(line => line.trim().length > 0)
            .join('\n');

        try {
            const ast = this.parser.parse(content);
            const parsed = this.parseDataStructure(ast);
            this.dataCache.set(filePath, parsed);
            return parsed;
        } catch (error) {
            throw new Error(`Failed to parse ${filePath}: ${error.message}`);
        }
    }

    /**
     * Parse S-expression AST into structured data
     */
    parseDataStructure(ast) {
        if (!Array.isArray(ast)) {
            return this.cleanValue(ast);
        }

        const [head, ...rest] = ast;
        
        switch (head) {
            case 'locations':
                return this.parseLocations(rest);
            case 'npcs':
                return this.parseNPCs(rest);
            case 'story':
                return this.parseStory(rest);
            default:
                // Generic object parsing
                const obj = {};
                for (let i = 0; i < rest.length; i += 2) {
                    if (i + 1 < rest.length) {
                        const key = rest[i];
                        const value = this.parseDataStructure(rest[i + 1]);
                        obj[key] = value;
                    }
                }
                return obj;
        }
    }

    /**
     * Parse locations data
     */
    parseLocations(elements) {
        const locations = {};
        
        for (const element of elements) {
            if (Array.isArray(element) && element[0] === 'location') {
                const [, locationId, ...props] = element;
                locations[locationId] = this.parseLocationProps(props);
            }
        }
        
        return locations;
    }

    /**
     * Parse location properties
     */
    parseLocationProps(props) {
        const location = {};
        
        for (const prop of props) {
            if (!Array.isArray(prop)) continue;
            
            const [key, value] = prop;
            switch (key) {
                case 'name':
                case 'description':
                case 'icon':
                    location[key] = this.cleanValue(value);
                    break;
                case 'connections':
                    location[key] = Array.isArray(value) ? value : [value];
                    break;
                case 'can-change-outfit':
                    location.canChangeOutfit = value === true || value === 'true';
                    break;
                default:
                    location[key] = this.parseDataStructure(value);
            }
        }
        
        return location;
    }

    /**
     * Parse NPCs data
     */
    parseNPCs(elements) {
        const npcs = {};
        
        for (const element of elements) {
            if (Array.isArray(element) && element[0] === 'npc') {
                const [, npcId, ...props] = element;
                npcs[npcId] = this.parseNPCProps(props);
                npcs[npcId].id = npcId;
            }
        }
        
        return npcs;
    }

    /**
     * Parse NPC properties
     */
    parseNPCProps(props) {
        const npc = {};
        
        for (const prop of props) {
            if (!Array.isArray(prop)) continue;
            
            const [key, ...values] = prop;
            const value = values.length === 1 ? values[0] : values;
            switch (key) {
                case 'name':
                case 'description':
                    npc[key] = this.cleanValue(value);
                    break;
                case 'likes-noble':
                    npc.likesNoble = value === true || value === 'true';
                    break;
                case 'base-location':
                    npc.baseLocation = value;
                    break;
                case 'dialogue':
                    // values contains the outfit sections
                    npc.dialogue = this.parseDialogue(values);
                    break;
                case 'movement-rules':
                    npc.movementRules = this.parseMovementRules(value);
                    break;
                default:
                    npc[key] = this.parseDataStructure(value);
            }
        }
        
        return npc;
    }

    /**
     * Parse dialogue structure
     */
    parseDialogue(dialogueData) {
        if (!Array.isArray(dialogueData)) return {};
        
        const dialogue = {};
        
        // dialogueData is an array of outfit sections
        for (let i = 0; i < dialogueData.length; i++) {
            const outfitSection = dialogueData[i];
            if (!Array.isArray(outfitSection) || outfitSection.length < 2) continue;
            
            const [outfit, ...states] = outfitSection;
            dialogue[outfit] = {};
            
            for (const state of states) {
                if (!Array.isArray(state) || state.length < 2) continue;
                
                const [stateName, ...stateProps] = state;
                dialogue[outfit][stateName] = this.parseDialogueState(stateProps);
            }
        }
        
        return dialogue;
    }

    /**
     * Parse dialogue state
     */
    parseDialogueState(stateData) {
        const state = {};
        
        for (const prop of stateData) {
            if (!Array.isArray(prop)) continue;
            
            const [key, ...rest] = prop;
            switch (key) {
                case 'greeting':
                    state.greeting = this.cleanValue(rest[0]);
                    break;
                case 'location':
                    state.location = rest[0];
                    break;
                case 'requires':
                    state.requires = rest[0];
                    break;
                case 'choices':
                    // rest is an array of choice elements
                    state.choices = this.parseChoices(rest);
                    break;
                default:
                    state[key] = this.parseDataStructure(rest[0]);
            }
        }
        
        return state;
    }

    /**
     * Parse choices
     */
    parseChoices(choicesData) {
        const choices = [];
        
        for (const choice of choicesData) {
            if (!Array.isArray(choice) || choice[0] !== 'choice') continue;
            
            const [, choiceId, ...choiceProps] = choice;
            const choiceObj = { id: choiceId };
            
            for (const prop of choiceProps) {
                if (!Array.isArray(prop)) continue;
                
                const [key, ...values] = prop;
                const value = values.length === 1 ? values[0] : values;
                switch (key) {
                    case 'text':
                    case 'response':
                    case 'description':
                    case 'result-text':
                        choiceObj[key.replace('-', '_')] = this.cleanValue(value);
                        break;
                    case 'quest-action':
                        choiceObj.quest_action = value;
                        break;
                    case 'requires-not':
                        choiceObj.requires_not = value;
                        break;
                    case 'next-scene':
                        choiceObj.nextScene = value;
                        break;
                    case 'next-choices':
                        // values contains all the choice elements
                        choiceObj.next_choices = this.parseChoices(values);
                        break;
                    case 'effects':
                        choiceObj.effects = this.parseEffects(value);
                        break;
                    case 'unlocks':
                        choiceObj.unlocks = value;
                        break;
                    case 'requires':
                        choiceObj.requires = value;
                        break;
                    default:
                        choiceObj[key] = this.parseDataStructure(value);
                }
            }
            
            choices.push(choiceObj);
        }
        
        return choices;
    }

    /**
     * Parse effects
     */
    parseEffects(effectsData) {
        if (!Array.isArray(effectsData)) return {};
        
        const effects = {};
        
        // effectsData is like (give-item marta_pie)
        if (effectsData.length >= 2) {
            const [effectType, ...values] = effectsData;
            
            switch (effectType) {
                case 'give-item':
                    effects.item = values[0];
                    break;
                // Removed loyalty system
                case 'reveal-location':
                    effects.location = values[0];
                    break;
                default:
                    effects[effectType] = values.length === 1 ? values[0] : values;
            }
        }
        
        return effects;
    }

    /**
     * Parse movement rules
     */
    parseMovementRules(rulesData) {
        if (!Array.isArray(rulesData)) return [];
        
        const rules = [];
        for (const rule of rulesData) {
            if (!Array.isArray(rule) || rule[0] !== 'condition') continue;
            
            const [, condition, ...props] = rule;
            const ruleObj = { condition: this.parseCondition(condition) };
            
            for (const prop of props) {
                if (Array.isArray(prop) && prop[0] === 'location') {
                    ruleObj.location = prop[1];
                }
            }
            
            rules.push(ruleObj);
        }
        
        return rules;
    }

    /**
     * Parse condition for movement rules
     */
    parseCondition(conditionData) {
        if (!Array.isArray(conditionData)) return null;
        
        const [type, flag, outfit] = conditionData;
        if (type === 'has-flag') {
            return (questState, npcMemory) => {
                return npcMemory[outfit] && npcMemory[outfit][flag];
            };
        }
        
        return null;
    }

    /**
     * Parse story data
     */
    parseStory(elements) {
        const scenes = {};
        
        for (const element of elements) {
            if (Array.isArray(element) && element[0] === 'scene') {
                const [, sceneId, ...props] = element;
                scenes[sceneId] = this.parseSceneProps(props);
            }
        }
        
        return scenes;
    }

    /**
     * Parse scene properties
     */
    parseSceneProps(props) {
        const scene = {};
        
        for (const prop of props) {
            if (!Array.isArray(prop)) continue;
            
            const [key, ...rest] = prop;
            switch (key) {
                case 'title':
                case 'text':
                case 'location':
                    scene[key] = this.cleanValue(rest[0]);
                    break;
                case 'choices':
                    scene.choices = this.parseStoryChoices(rest);
                    break;
                default:
                    scene[key] = this.parseDataStructure(rest[0]);
            }
        }
        
        return scene;
    }

    /**
     * Parse story choices
     */
    parseStoryChoices(choicesData) {
        if (!Array.isArray(choicesData)) return {};
        
        const choices = {};
        
        for (const characterChoices of choicesData) {
            if (!Array.isArray(characterChoices)) continue;
            
            const [character, ...characterChoicesList] = characterChoices;
            choices[character] = [];
            
            for (const choice of characterChoicesList) {
                if (Array.isArray(choice) && choice[0] === 'choice') {
                    const choiceObj = this.parseStoryChoice(choice);
                    choices[character].push(choiceObj);
                }
            }
        }
        
        return choices;
    }

    /**
     * Parse a single story choice
     */
    parseStoryChoice(choiceData) {
        const [, choiceId, ...choiceProps] = choiceData;
        const choice = { id: choiceId };
        
        for (const prop of choiceProps) {
            if (!Array.isArray(prop)) continue;
            
            const [key, value] = prop;
            switch (key) {
                case 'text':
                case 'description':
                    choice[key] = this.cleanValue(value);
                    break;
                case 'result-text':
                    choice.resultText = this.cleanValue(value);
                    break;
                case 'next-scene':
                    choice.nextScene = value;
                    break;
                default:
                    choice[key] = this.parseDataStructure(value);
            }
        }
        
        return choice;
    }
}

module.exports = DataLoader;