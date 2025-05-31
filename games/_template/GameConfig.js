const GameConfigInterface = require('../../engine/interfaces/GameConfig');

// Import game data from local data folder
// const StoryData = require('./data/storyData');
// const LocationData = require('./data/locationData');
// const NPCData = require('./data/npcData');

/**
 * Template configuration for a new game
 * Copy this file and modify for your game
 */
class TemplateGameConfig extends GameConfigInterface {
    constructor() {
        super();
        
        // Basic game info
        this.gameId = 'template_game';
        this.gameName = 'Template Game';
        this.gameVersion = '1.0.0';
        this.maxPlayers = 2;
        
        // Characters - define who can play
        this.characters = {
            player1: {
                name: 'Игрок 1',
                description: 'Первый игрок'
            },
            player2: {
                name: 'Игрок 2', 
                description: 'Второй игрок'
            }
        };
        
        // Outfits system (optional)
        this.outfits = {
            default: {
                name: 'Обычная одежда',
                type: 'default',
                description: 'Стандартный наряд'
            }
        };
        
        // Story scenes
        this.scenes = this.loadScenes();
        this.startingScene = 'intro';
        
        // World data
        this.locations = this.loadLocations();
        this.npcs = this.loadNPCs();
        this.quests = this.loadQuests();
        
        // Game features - enable/disable as needed
        this.features = {
            outfitSwapping: false,      // Система смены одежды
            turnBasedDialogue: true,    // Пошаговые диалоги
            questSystem: false,         // Система квестов
            locationSystem: true,       // Система локаций
            npcInteractions: false,     // Взаимодействие с NPC
            freeMovement: true          // Свободное перемещение
        };
        
        // Initial state
        this.initialState = {
            startingLocation: 'main_room',
            startingOutfits: {
                player1: 'default',
                player2: 'default'
            },
            startingItems: {
                player1: [],
                player2: []
            },
            globalMemory: {}
        };
    }

    loadScenes() {
        // Define your story scenes here
        // Or import from ./data/storyData.js
        return {
            "intro": {
                title: "Введение",
                text: `Добро пожаловать в новую игру!
                
                🎮 <strong>Описание:</strong> Это шаблон для создания новой игры.
                
                <div style="background: rgba(0,255,0,0.1); padding: 10px; border-radius: 5px; margin: 10px 0;">
                💡 <strong>Совет:</strong> Измените этот текст и добавьте свои сцены!
                </div>`,
                
                choices: {
                    player1: [{
                        id: "start_game",
                        text: "Начать игру",
                        description: "Начать новое приключение",
                        resultText: "Игра началась!",
                        nextScene: "main_game"
                    }],
                    player2: [{
                        id: "start_game",
                        text: "Начать игру", 
                        description: "Присоединиться к приключению",
                        resultText: "Игра началась!",
                        nextScene: "main_game"
                    }]
                }
            },
            
            "main_game": {
                title: "Основная игра",
                text: `Вы находитесь в главной комнате. Что будете делать?`,
                
                choices: {
                    player1: [{
                        id: "explore",
                        text: "Исследовать",
                        description: "Осмотреть окрестности",
                        resultText: "Вы исследуете комнату"
                    }],
                    player2: [{
                        id: "wait",
                        text: "Подождать",
                        description: "Дождаться партнера",
                        resultText: "Вы ждете"
                    }]
                }
            }
        };
    }

    loadLocations() {
        // Define your game world here
        // Or import from ./data/locationData.js
        return {
            main_room: {
                name: 'Главная комната',
                description: 'Просторная комната с несколькими выходами.',
                connections: ['north_room', 'south_room'],
                canChangeOutfit: true,
                icon: '🏠',
                npcs: []
            },
            
            north_room: {
                name: 'Северная комната',
                description: 'Тихая комната на севере.',
                connections: ['main_room'],
                canChangeOutfit: false,
                icon: '⬆️',
                npcs: []
            },
            
            south_room: {
                name: 'Южная комната',
                description: 'Просторная комната на юге.',
                connections: ['main_room'],
                canChangeOutfit: false,
                icon: '⬇️',
                npcs: []
            }
        };
    }

    loadNPCs() {
        // Define your NPCs here
        // Or import from ./data/npcData.js
        return {
            // Example NPC (commented out)
            /*
            'example_npc': {
                id: 'example_npc',
                name: 'Пример NPC',
                description: 'Пример персонажа',
                likesNoble: true,
                dialogue: {
                    default: {
                        initial: {
                            greeting: 'Привет!',
                            choices: [{
                                id: 'greet',
                                text: 'Поздороваться',
                                response: 'И тебе привет!'
                            }]
                        }
                    }
                }
            }
            */
        };
    }

    loadQuests() {
        // Define your quests here
        // Or import from ./data/questData.js
        return {
            // Example quest (commented out)
            /*
            'example_quest': {
                id: 'example_quest',
                character: 'player1',
                title: 'Пример квеста',
                description: 'Описание квеста',
                steps: [{
                    id: 'step1',
                    description: 'Сделать что-то',
                    completed: false
                }],
                rewards: ['example_item']
            }
            */
        };
    }

    // Override if you need custom NPC location logic
    getNPCsForLocation(locationId, gameState = null, character = null) {
        const location = this.getLocation(locationId);
        if (!location) return [];
        
        // Simple implementation - just return NPCs assigned to location
        return location.npcs.map(id => this.getNPC(id)).filter(npc => npc);
    }
}

module.exports = TemplateGameConfig;