const GameConfigInterface = require('../../engine/interfaces/GameConfig');

/**
 * Configuration for a Detective mystery game
 * This demonstrates how the same engine can power a completely different game
 */
class DetectiveGameConfig extends GameConfigInterface {
    constructor() {
        super();
        
        // Basic game info
        this.gameId = 'detective_mystery';
        this.gameName = 'Detective Mystery: The Missing Jewels';
        this.gameVersion = '1.0.0';
        this.maxPlayers = 2;
        
        // Characters - completely different from Pulpulak
        this.characters = {
            detective: {
                name: 'Детектив',
                description: 'Опытный сыщик'
            },
            journalist: {
                name: 'Журналист',
                description: 'Любопытный репортёр'
            }
        };
        
        // Different outfit system - formal vs casual
        this.outfits = {
            detective_coat: {
                name: 'Детективное пальто',
                type: 'professional',
                description: 'Строгое пальто детектива'
            },
            casual_clothes: {
                name: 'Повседневная одежда',
                type: 'casual',
                description: 'Обычная одежда'
            },
            disguise: {
                name: 'Маскировка',
                type: 'disguise',
                description: 'Одежда для маскировки'
            }
        };
        
        // Different story
        this.scenes = this.loadScenes();
        this.startingScene = 'crime_scene_arrival';
        
        // Different world - modern city
        this.locations = this.loadLocations();
        this.npcs = this.loadNPCs();
        this.quests = this.loadQuests();
        
        // Same features but different context
        this.features = {
            outfitSwapping: true, // For disguises
            turnBasedDialogue: true,
            questSystem: true, // Investigation tasks
            locationSystem: true,
            npcInteractions: true,
            evidenceSystem: true // New feature for detective game
        };
        
        // Different starting conditions
        this.initialState = {
            startingLocation: 'crime_scene',
            startingOutfits: {
                detective: 'detective_coat',
                journalist: 'casual_clothes'
            },
            startingItems: {
                detective: ['badge', 'notebook'],
                journalist: ['camera', 'recorder']
            },
            globalMemory: {
                crime_discovered: true
            }
        };
    }

    loadScenes() {
        return {
            "crime_scene_arrival": {
                title: "Прибытие на место преступления",
                text: `Вы прибываете к особняку миллионера, где произошла кража редких драгоценностей.
                
                🔍 <strong>Ситуация:</strong> Детектив и журналист работают вместе, чтобы раскрыть это дело.
                
                <div style="background: rgba(0,0,255,0.1); padding: 10px; border-radius: 5px; margin: 10px 0;">
                💡 <strong>Подсказка:</strong> Разные люди по-разному реагируют на детектива и журналиста. Используйте это в своих интересах!
                </div>`,
                
                choices: {
                    detective: [{
                        id: "examine_scene",
                        text: "Осмотреть место преступления",
                        description: "Профессионально изучить улики",
                        resultText: "Вы находите несколько зацепок",
                        nextScene: "investigation_begins"
                    }],
                    journalist: [{
                        id: "interview_witnesses",
                        text: "Опросить свидетелей",
                        description: "Поговорить с очевидцами",
                        resultText: "Вы собираете информацию от свидетелей",
                        nextScene: "investigation_begins"
                    }]
                }
            },
            
            "investigation_begins": {
                title: "Начало расследования",
                text: `Вы начинаете собирать улики и опрашивать подозреваемых.
                
                🔍 <strong>Важно:</strong> Некоторые люди охотнее говорят с журналистом, другие - с детективом.`,
                
                choices: {
                    detective: [{
                        id: "check_security",
                        text: "Проверить систему безопасности",
                        description: "Изучить записи камер",
                        resultText: "Вы находите важные записи",
                        nextScene: "evidence_gathering"
                    }],
                    journalist: [{
                        id: "research_background",
                        text: "Изучить предысторию",
                        description: "Найти связи между подозреваемыми",
                        resultText: "Вы обнаруживаете интересные связи",
                        nextScene: "evidence_gathering"
                    }]
                }
            },
            
            "evidence_gathering": {
                title: "Сбор улик",
                text: `Продолжайте расследование, опрашивая подозреваемых и собирая улики.
                
                🔍 <strong>Совет:</strong> Перемещайтесь по локациям и говорите с разными людьми!`,
                
                choices: {
                    detective: [],
                    journalist: []
                }
            }
        };
    }

    loadLocations() {
        return {
            crime_scene: {
                name: 'Место преступления',
                description: 'Роскошный особняк, где произошла кража.',
                connections: ['garden', 'main_entrance'],
                canChangeOutfit: false,
                icon: '🏛️',
                npcs: ['security_guard', 'butler']
            },
            
            main_entrance: {
                name: 'Главный вход',
                description: 'Парадный вход в особняк.',
                connections: ['crime_scene', 'street'],
                canChangeOutfit: false,
                icon: '🚪',
                npcs: []
            },
            
            garden: {
                name: 'Сад',
                description: 'Ухоженный сад с множеством укромных мест.',
                connections: ['crime_scene', 'greenhouse'],
                canChangeOutfit: true, // Можно переодеться в укромном месте
                icon: '🌳',
                npcs: ['gardener']
            },
            
            greenhouse: {
                name: 'Оранжерея',
                description: 'Стеклянная оранжерея с экзотическими растениями.',
                connections: ['garden'],
                canChangeOutfit: true,
                icon: '🪴',
                npcs: []
            },
            
            street: {
                name: 'Улица',
                description: 'Тихая улица в богатом районе.',
                connections: ['main_entrance', 'cafe', 'police_station'],
                canChangeOutfit: false,
                icon: '🛣️',
                npcs: ['neighbor']
            },
            
            cafe: {
                name: 'Кафе',
                description: 'Уютное кафе, где можно спокойно поговорить.',
                connections: ['street'],
                canChangeOutfit: false,
                icon: '☕',
                npcs: ['waitress', 'regular_customer']
            },
            
            police_station: {
                name: 'Полицейский участок',
                description: 'Местный полицейский участок.',
                connections: ['street'],
                canChangeOutfit: false,
                icon: '🚔',
                npcs: ['police_chief', 'officer']
            }
        };
    }

    loadNPCs() {
        return {
            security_guard: {
                id: 'security_guard',
                name: 'Охранник Степан',
                description: 'Нервный охранник, дежуривший в ночь кражи',
                likesNoble: true, // Больше доверяет детективу
                baseLocation: 'crime_scene',
                dialogue: {
                    professional: {
                        initial: {
                            greeting: 'Детектив! Наконец-то! Я всю ночь ждал вас.',
                            choices: [{
                                id: 'what_happened',
                                text: 'Что произошло?',
                                response: 'Около полуночи сработала сигнализация в главном зале. Когда я прибежал, воров уже не было.'
                            }]
                        }
                    },
                    casual: {
                        initial: {
                            greeting: 'Вы из прессы? Я не могу давать комментарии...',
                            choices: [{
                                id: 'unofficial_chat',
                                text: 'Неофициально поговорить',
                                response: 'Ну... может быть, но только между нами.'
                            }]
                        }
                    }
                }
            },
            
            butler: {
                id: 'butler',
                name: 'Дворецкий Артур',
                description: 'Пожилой дворецкий семьи',
                likesNoble: false, // Больше доверяет журналисту (менее формально)
                baseLocation: 'crime_scene',
                dialogue: {
                    casual: {
                        initial: {
                            greeting: 'Ах, журналист! Может быть, кто-то наконец расскажет правду.',
                            choices: [{
                                id: 'ask_about_family',
                                text: 'Расскажите о семье',
                                response: 'У хозяев много секретов... Не все так просто, как кажется.'
                            }]
                        }
                    },
                    professional: {
                        initial: {
                            greeting: 'Детектив, я уже дал показания вашим коллегам.',
                            choices: [{
                                id: 'formal_questions',
                                text: 'Задать формальные вопросы',
                                response: 'Я служу этой семье 20 лет. Ничего подозрительного не замечал.'
                            }]
                        }
                    }
                }
            },
            
            gardener: {
                id: 'gardener',
                name: 'Садовник Михаил',
                description: 'Молодой садовник, недавно нанятый',
                likesNoble: false,
                baseLocation: 'garden',
                dialogue: {
                    casual: {
                        initial: {
                            greeting: 'Слушайте, я рад что вы пришли. Полиция меня не очень слушает.',
                            choices: [{
                                id: 'what_did_you_see',
                                text: 'Что вы видели?',
                                response: 'Я работал поздно в оранжерее. Видел странные тени у главного входа.'
                            }]
                        }
                    }
                }
            },

            neighbor: {
                id: 'neighbor',
                name: 'Соседка Елена',
                description: 'Любопытная соседка',
                likesNoble: false,
                baseLocation: 'street',
                dialogue: {
                    casual: {
                        initial: {
                            greeting: 'О, вы журналист? У меня есть что рассказать!',
                            choices: [{
                                id: 'neighborhood_gossip',
                                text: 'Послушать сплетни',
                                response: 'В последнее время у них часто бывали странные гости...'
                            }]
                        }
                    }
                }
            },

            police_chief: {
                id: 'police_chief',
                name: 'Начальник полиции Иванов',
                description: 'Опытный полицейский',
                likesNoble: true,
                baseLocation: 'police_station',
                dialogue: {
                    professional: {
                        initial: {
                            greeting: 'Детектив! Хорошо что вы здесь. Дело непростое.',
                            choices: [{
                                id: 'case_details',
                                text: 'Подробности дела',
                                response: 'Украли коллекцию редких бриллиантов стоимостью в миллионы.'
                            }]
                        }
                    }
                }
            },

            waitress: {
                id: 'waitress',
                name: 'Официантка Анна',
                description: 'Дружелюбная официантка',
                likesNoble: false,
                baseLocation: 'cafe',
                dialogue: {
                    casual: {
                        initial: {
                            greeting: 'Добро пожаловать! Слышала, что в особняке что-то случилось.',
                            choices: [{
                                id: 'local_info',
                                text: 'Узнать местные новости',
                                response: 'Вчера вечером видела подозрительную машину рядом с особняком.'
                            }]
                        }
                    }
                }
            },

            regular_customer: {
                id: 'regular_customer',
                name: 'Постоянный клиент Борис',
                description: 'Местный житель, часто бывает в кафе',
                likesNoble: false,
                baseLocation: 'cafe',
                dialogue: {
                    casual: {
                        initial: {
                            greeting: 'А, новые лица! Вы по поводу кражи?',
                            choices: [{
                                id: 'local_rumors',
                                text: 'Местные слухи',
                                response: 'Говорят, что дело нечисто. У хозяина были долги.'
                            }]
                        }
                    }
                }
            },

            officer: {
                id: 'officer',
                name: 'Офицер Петров',
                description: 'Молодой полицейский',
                likesNoble: true,
                baseLocation: 'police_station',
                dialogue: {
                    professional: {
                        initial: {
                            greeting: 'Детектив! Я первым прибыл на место происшествия.',
                            choices: [{
                                id: 'first_observations',
                                text: 'Первые наблюдения',
                                response: 'Замки не были взломаны. Кто-то знал коды сигнализации.'
                            }]
                        }
                    }
                }
            }
        };
    }

    loadQuests() {
        return {
            detective_investigation: {
                id: 'detective_investigation',
                character: 'detective',
                title: 'Официальное расследование',
                description: 'Раскрыть кражу драгоценностей',
                steps: [
                    {
                        id: 'interview_security',
                        description: 'Опросить охрану',
                        location: 'crime_scene',
                        npc: 'security_guard'
                    },
                    {
                        id: 'check_police_report',
                        description: 'Изучить полицейский отчёт',
                        location: 'police_station',
                        npc: 'police_chief'
                    },
                    {
                        id: 'examine_evidence',
                        description: 'Проанализировать улики',
                        location: 'crime_scene',
                        npc: 'butler'
                    }
                ],
                rewards: ['police_commendation', 'case_files']
            },
            
            journalist_investigation: {
                id: 'journalist_investigation',
                character: 'journalist',
                title: 'Журналистское расследование',
                description: 'Найти скрытую правду за кражей',
                steps: [
                    {
                        id: 'gather_rumors',
                        description: 'Собрать слухи и сплетни',
                        location: 'cafe',
                        npc: 'waitress'
                    },
                    {
                        id: 'interview_neighbors',
                        description: 'Опросить соседей',
                        location: 'street',
                        npc: 'neighbor'
                    },
                    {
                        id: 'uncover_family_secrets',
                        description: 'Раскрыть семейные тайны',
                        location: 'crime_scene',
                        npc: 'butler'
                    }
                ],
                rewards: ['exclusive_story', 'insider_contacts']
            }
        };
    }

    // Override outfit type mapping for detective game
    getNPCsForLocation(locationId, gameState = null, character = null) {
        const location = this.getLocation(locationId);
        if (!location) return [];
        
        // Detective game doesn't have complex NPC movement (yet)
        return location.npcs.map(id => this.getNPC(id)).filter(npc => npc);
    }

    // Helper to determine outfit type for dialogue
    getOutfitType(outfit) {
        const outfitData = this.outfits[outfit];
        if (!outfitData) return 'casual';
        
        // Map detective game outfit types to dialogue types
        switch (outfitData.type) {
            case 'professional':
                return 'professional';
            case 'disguise':
                return 'disguise';
            default:
                return 'casual';
        }
    }
}

module.exports = DetectiveGameConfig;