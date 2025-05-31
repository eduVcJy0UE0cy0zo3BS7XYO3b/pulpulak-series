// NPC data with outfit preferences, branching dialogues and memory
const NPCs = {
    'royal_advisor': {
        id: 'royal_advisor',
        name: 'Королевский советник Эдвард',
        description: 'Важный вельможа в богатых одеждах с золотой цепью',
        likesNoble: true, // Хорошо относится к княжескому наряду
        dialogue: {
            noble: {
                initial: {
                    greeting: 'Ваше высочество! Рад видеть вас в добром здравии. Надеюсь, утро выдалось приятным?',
                    choices: [
                        {
                            id: 'ask_about_kingdom',
                            text: 'Спросить о делах королевства',
                            response: 'Дела идут прекрасно, ваше высочество. Казна полна, народ доволен.',
                            unlocks: 'kingdom_talked',
                            next_choices: [
                                {
                                    id: 'ask_about_taxes',
                                    text: 'Спросить о налогах',
                                    response: 'Недавно мы снизили налоги для крестьян. Это помогло экономике.'
                                },
                                {
                                    id: 'ask_about_neighbors',
                                    text: 'Узнать о соседних королевствах',
                                    response: 'Отношения мирные, но всегда есть те, кто завидует нашему процветанию.'
                                }
                            ]
                        },
                        {
                            id: 'ask_about_parents',
                            text: 'Узнать о родителях',
                            response: 'Их величества сейчас на совете. Обсуждают важные государственные дела.',
                            unlocks: 'parents_talked',
                            next_choices: [
                                {
                                    id: 'ask_about_council',
                                    text: 'Что за дела обсуждают?',
                                    response: 'Торговые соглашения с восточными землями, ваше высочество.'
                                }
                            ]
                        },
                        {
                            id: 'ask_about_relic',
                            text: 'Спросить о пропавшей реликвии',
                            response: 'Ах, да! Древний амулет пропал из сокровищницы. Мне нужна ваша помощь, ваше высочество. Начните с библиотеки - там работает учёный Марк.',
                            unlocks: 'relic_quest_given',
                            quest_action: 'start_noble_quest',
                            requires_not: 'relic_quest_given'
                        }
                    ]
                },
                return: {
                    greeting: 'Снова приветствую вас, ваше высочество! Что на этот раз вас интересует?',
                    choices: [
                        {
                            id: 'continue_kingdom',
                            text: 'Ещё о делах королевства',
                            response: 'Всегда готов доложить о наших успехах!',
                            requires: 'kingdom_talked',
                            next_choices: [
                                {
                                    id: 'ask_about_economy',
                                    text: 'Как обстоят дела с торговлей?',
                                    response: 'Превосходно! Наши торговцы активно работают с соседними землями.'
                                }
                            ]
                        },
                        {
                            id: 'ask_about_relic',
                            text: 'Спросить о пропавшей реликвии',
                            response: 'Ах, да! Древний амулет пропал из сокровищницы. Мне нужна ваша помощь, ваше высочество. Начните с библиотеки - там работает учёный Марк.',
                            unlocks: 'relic_quest_given',
                            quest_action: 'start_noble_quest',
                            requires_not: 'relic_quest_given'
                        },
                        {
                            id: 'new_topic',
                            text: 'Поговорить о чём-то новом',
                            response: 'О чём бы вы хотели поговорить, ваше высочество?',
                            next_choices: [
                                {
                                    id: 'ask_about_history',
                                    text: 'Об истории замка',
                                    response: 'Этот замок стоит уже 300 лет! Много историй хранят эти стены.'
                                }
                            ]
                        },
                        {
                            id: 'report_relic_findings',
                            text: 'Доложить о находках по реликвии',
                            response: 'Превосходно! Вы проделали отличную работу, ваше высочество. Благодаря информации из архивов мы теперь знаем где искать амулет. Квест завершён!',
                            requires: 'has_archive_info',
                            quest_action: 'complete_princess_quest',
                            unlocks: 'quest_completed'
                        }
                    ]
                }
            },
            common: {
                initial: {
                    greeting: 'Эй ты! Что простолюдинка делает в тронном зале? Стража!',
                    choices: [
                        {
                            id: 'explain_presence',
                            text: 'Объяснить своё присутствие',
                            response: 'Не важно! Убирайся отсюда, пока я не вызвал стражу!',
                            unlocks: 'explained_once'
                        },
                        {
                            id: 'run_away',
                            text: 'Быстро уйти',
                            response: 'И не смей больше показываться здесь!'
                        }
                    ]
                },
                return: {
                    greeting: 'Ты опять?! Я же сказал не показываться здесь!',
                    choices: [
                        {
                            id: 'apologize',
                            text: 'Извиниться',
                            response: 'Извинения не помогут! Вон отсюда!',
                            requires: 'explained_once'
                        },
                        {
                            id: 'insist_stay',
                            text: 'Настаивать на своём праве быть здесь',
                            response: 'Какое право?! У тебя нет никаких прав здесь!'
                        }
                    ]
                }
            }
        }
    },
    
    'cook': {
        id: 'cook',
        name: 'Повар Марта',
        description: 'Добродушная полная женщина в белом фартуке, испачканном мукой',
        likesNoble: false, // Предпочитает простых людей
        dialogue: {
            common: {
                initial: {
                    greeting: 'Ох, милая! Проголодалась? Садись, садись! Сейчас накормлю досыта!',
                    choices: [
                        {
                            id: 'accept_food',
                            text: 'С радостью поесть',
                            response: 'Вот свежий хлеб, сыр и мой фирменный пирог! Ешь, не стесняйся!',
                            effects: { item: 'marta_pie' },
                            unlocks: 'ate_together',
                            next_choices: [
                                {
                                    id: 'compliment_food',
                                    text: 'Похвалить еду',
                                    response: 'Ох, спасибо, деточка! Хочешь, дам рецепт?'
                                },
                                {
                                    id: 'ask_for_more',
                                    text: 'Попросить добавки',
                                    response: 'Конечно, милая! Растущему организму нужна хорошая еда!'
                                }
                            ]
                        },
                        {
                            id: 'ask_about_castle',
                            text: 'Расспросить о замке',
                            response: 'Ох, деточка, я тут уже 20 лет работаю! Многое повидала. Хочешь, расскажу пару секретов? *подмигивает*',
                            effects: { info: 'castle_secrets' },
                            unlocks: 'knows_secrets',
                            next_choices: [
                                {
                                    id: 'hear_more_secrets',
                                    text: 'Выслушать ещё секреты',
                                    response: 'Ну слушай... говорят, в подвале есть потайная комната!'
                                }
                            ]
                        },
                        {
                            id: 'ask_about_herbs',
                            text: 'Спросить о лечебных травах',
                            response: 'О, милая! Мне как раз нужна помощь с особым зельем. В саду есть травник Элиас - он знает все о редких растениях. Найди его!',
                            unlocks: 'herb_quest_given',
                            quest_action: 'start_common_quest',
                            requires_not: 'herb_quest_given'
                        }
                    ]
                },
                return: {
                    greeting: 'А, моя дорогая вернулась! Как дела, милая? Опять проголодалась?',
                    choices: [
                        {
                            id: 'more_food',
                            text: 'Попросить ещё еды',
                            response: 'Конечно! Для тебя всё что угодно!',
                            requires: 'ate_together'
                        },
                        {
                            id: 'more_secrets',
                            text: 'Узнать больше секретов',
                            response: 'Ох, любопытная какая! Хорошо, расскажу ещё кое-что...',
                            requires: 'knows_secrets',
                            next_choices: [
                                {
                                    id: 'secret_passage',
                                    text: 'О тайных проходах',
                                    response: 'За старым гобеленом в большом зале есть скрытая дверь!'
                                }
                            ]
                        },
                        {
                            id: 'just_chat',
                            text: 'Просто поболтать',
                            response: 'Всегда рада поговорить с милым человеком!'
                        },
                        {
                            id: 'report_herb_findings',
                            text: 'Принести информацию о редких травах',
                            response: 'Ой, милая! Ты нашла травника и узнала про редкие растения! Теперь я смогу приготовить то особое зелье. Спасибо тебе большое!',
                            requires: 'has_herb_info',
                            quest_action: 'complete_helper_quest',
                            unlocks: 'herb_quest_completed'
                        }
                    ]
                }
            },
            noble: {
                initial: {
                    greeting: 'О, ваше высочество... *нервно кланяется и вытирает руки о фартук* Чем могу служить?',
                    choices: [
                        {
                            id: 'ask_for_food',
                            text: 'Попросить поесть',
                            response: 'Конечно, ваше высочество. Сейчас приготовлю что-нибудь... особенное. *нервничает*',
                            unlocks: 'served_noble'
                        },
                        {
                            id: 'chat_casually',
                            text: 'Поговорить по-дружески',
                            response: 'Я... я не смею, ваше высочество. Простите. *отступает*',
                            unlocks: 'tried_friendship'
                        }
                    ]
                },
                return: {
                    greeting: 'Ваше высочество! *ещё больше нервничает* Снова изволите меня посетить?',
                    choices: [
                        {
                            id: 'reassure',
                            text: 'Успокоить её',
                            response: 'В-вы очень добры, ваше высочество... но я простая повариха.',
                            requires: 'tried_friendship'
                        },
                        {
                            id: 'order_food_again',
                            text: 'Заказать ещё еды',
                            response: 'Конечно! Что изволите отведать на этот раз?',
                            requires: 'served_noble'
                        }
                    ]
                }
            }
        }
    },
    
    'gardener': {
        id: 'gardener',
        name: 'Садовник Томас',
        description: 'Пожилой человек с добрыми глазами и руками в земле',
        likesNoble: false, // Больше доверяет простым людям
        dialogue: {
            noble: {
                initial: {
                    greeting: 'Простите, ваше высочество, но этот сад закрыт для посещений по приказу короля.',
                    choices: [
                        {
                            id: 'insist_entry',
                            text: 'Настоять на входе',
                            response: 'Приказ есть приказ, ваше высочество. Даже для вас. *упрямо качает головой*',
                            unlocks: 'insisted_entry'
                        },
                        {
                            id: 'ask_why_closed',
                            text: 'Спросить причину',
                            response: 'Его величество не объясняет причин. Просто велел никого не пускать.',
                            unlocks: 'asked_reason'
                        }
                    ]
                },
                return: {
                    greeting: 'Ваше высочество, я уже говорил - сад закрыт.',
                    choices: [
                        {
                            id: 'appeal_to_reason',
                            text: 'Обратиться к разуму',
                            response: 'Понимаю ваше желание, но я не могу ослушаться короля.',
                            requires: 'asked_reason'
                        }
                    ]
                }
            },
            common: {
                initial: {
                    greeting: 'Ах, деточка! Давно не видел молодых лиц в саду. Хочешь посмотреть на мои розы? Они как раз расцвели!',
                    choices: [
                        {
                            id: 'see_roses',
                            text: 'Посмотреть розы',
                            response: 'Вот, смотри! Эту красавицу я вывел сам. А вот тут у меня целебные травы растут.',
                            effects: { item: 'healing_herbs' },
                            unlocks: 'saw_garden',
                            next_choices: [
                                {
                                    id: 'learn_about_herbs',
                                    text: 'Узнать о травах',
                                    response: 'Эта ромашка лечит головную боль, а это зверобой - от тоски!'
                                }
                            ]
                        },
                        {
                            id: 'help_garden',
                            text: 'Предложить помощь',
                            response: 'Правда поможешь? Вот умница! Давай покажу тебе особое место - там растут волшебные цветы!',
                            unlocks: 'helped_garden'
                        }
                    ]
                },
                return: {
                    greeting: 'А вот и моя юная помощница! Как дела, деточка?',
                    choices: [
                        {
                            id: 'continue_helping',
                            text: 'Продолжить помогать',
                            response: 'Отлично! Сегодня научу тебя ухаживать за магическими растениями.',
                            requires: 'helped_garden'
                        },
                        {
                            id: 'ask_about_magic_flowers',
                            text: 'Спросить о волшебных цветах',
                            response: 'Это лунная фиалка - она светится по ночам! А это солнечный мак - его семена дают энергию.',
                            requires: 'saw_garden'
                        }
                    ]
                }
            }
        }
    },
    
    'guard_captain': {
        id: 'guard_captain',
        name: 'Капитан стражи Рейнальд',
        description: 'Суровый воин в сияющих доспехах с мечом на поясе',
        likesNoble: true,
        dialogue: {
            noble: {
                initial: {
                    greeting: 'Ваше высочество! *отдаёт честь* Замок под надёжной охраной!',
                    choices: [
                        {
                            id: 'ask_about_security',
                            text: 'Спросить об охране',
                            response: 'Все входы перекрыты, на стенах дозор. Можете не волноваться, ваше высочество.',
                            unlocks: 'discussed_security',
                            next_choices: [
                                {
                                    id: 'ask_about_guards',
                                    text: 'Спросить о стражниках',
                                    response: 'У меня верные люди, ваше высочество. Все прошли строгий отбор.'
                                }
                            ]
                        }
                    ]
                },
                return: {
                    greeting: 'Ваше высочество! Всё спокойно, как и обещал.',
                    choices: [
                        {
                            id: 'detailed_report',
                            text: 'Подробный доклад',
                            response: 'За последние часы никаких происшествий. Стража бдительна.',
                            requires: 'discussed_security'
                        }
                    ]
                }
            },
            common: {
                initial: {
                    greeting: 'Стой! Что ты здесь делаешь? Покажи разрешение!',
                    choices: [
                        {
                            id: 'no_permission',
                            text: 'У меня нет разрешения...',
                            response: 'Тогда проваливай! И не попадайся мне больше на глаза!',
                            unlocks: 'caught_once'
                        }
                    ]
                },
                return: {
                    greeting: 'Ты опять здесь?! Я же сказал убираться!',
                    choices: [
                        {
                            id: 'try_to_explain',
                            text: 'Попытаться объяснить',
                            response: 'Никаких объяснений! В следующий раз брошу в темницу!',
                            requires: 'caught_once'
                        }
                    ]
                }
            }
        }
    },
    
    'weaponsmith': {
        id: 'weaponsmith',
        name: 'Оружейник Бьорн',
        description: 'Мускулистый мужчина с молотом у наковальни',
        likesNoble: false,
        dialogue: {
            noble: {
                initial: {
                    greeting: '*продолжает работать, не поднимая головы* Чего надо?',
                    choices: [
                        {
                            id: 'order_weapon',
                            text: 'Заказать оружие',
                            response: 'Для вас? Ха! Вы и меч-то держать не умеете.',
                            unlocks: 'tried_to_order'
                        }
                    ]
                },
                return: {
                    greeting: '*хмыкает* Опять вы. Всё ещё думаете о мече?',
                    choices: [
                        {
                            id: 'insist_on_weapon',
                            text: 'Настаивать на заказе',
                            response: 'Упрямая, ничего не скажешь. Может, и правда стоит сделать вам что-то...',
                            requires: 'tried_to_order'
                        }
                    ]
                }
            },
            common: {
                initial: {
                    greeting: 'О, привет! Редко вижу новые лица. Хочешь посмотреть на мою работу?',
                    choices: [
                        {
                            id: 'watch_work',
                            text: 'Посмотреть на работу',
                            response: 'Смотри - это будет лучший меч в королевстве! Хочешь, научу основам?',
                            unlocks: 'interested_in_craft',
                            next_choices: [
                                {
                                    id: 'learn_smithing',
                                    text: 'Научиться кузнечному делу',
                                    response: 'Хорошо! Начнём с простого - как правильно держать молот.'
                                }
                            ]
                        }
                    ]
                },
                return: {
                    greeting: 'А, вернулась! Готова к урокам кузнечного дела?',
                    choices: [
                        {
                            id: 'continue_learning',
                            text: 'Продолжить обучение',
                            response: 'Отлично! Сегодня научу тебя выбирать металл.',
                            requires: 'interested_in_craft'
                        }
                    ]
                }
            }
        }
    },
    
    'priest': {
        id: 'priest',
        name: 'Священник отец Мартин',
        description: 'Человек в чёрной рясе с молитвенником',
        likesNoble: true,
        dialogue: {
            noble: {
                initial: {
                    greeting: 'Благословение вам, ваше высочество. Пришли помолиться?',
                    choices: [
                        {
                            id: 'pray',
                            text: 'Помолиться',
                            response: 'Пусть боги хранят вас и вашу семью, дитя моё.',
                            unlocks: 'prayed_together'
                        }
                    ]
                },
                return: {
                    greeting: 'Снова приветствую вас в доме божьем, ваше высочество.',
                    choices: [
                        {
                            id: 'confession',
                            text: 'Исповедаться',
                            response: 'Говорите, дитя моё. Здесь ваши слова в безопасности.',
                            requires: 'prayed_together'
                        }
                    ]
                }
            },
            common: {
                initial: {
                    greeting: 'Добро пожаловать в дом божий, дитя. Все равны перед богами.',
                    choices: [
                        {
                            id: 'seek_blessing',
                            text: 'Попросить благословения',
                            response: 'Иди с миром, дитя. Пусть твой путь будет светлым.',
                            unlocks: 'blessed'
                        }
                    ]
                },
                return: {
                    greeting: 'Снова вижу вас, дитя. Как идёт ваш путь?',
                    choices: [
                        {
                            id: 'ask_guidance',
                            text: 'Попросить совета',
                            response: 'Слушай своё сердце, дитя. Оно знает правильный путь.',
                            requires: 'blessed'
                        }
                    ]
                }
            }
        }
    },

    'librarian': {
        id: 'librarian',
        name: 'Библиотекарь Марк',
        description: 'Пожилой учёный в очках с множеством книг',
        likesNoble: true,
        dialogue: {
            noble: {
                initial: {
                    greeting: 'Ваше высочество! Добро пожаловать в библиотеку. Ищете что-то конкретное?',
                    choices: [
                        {
                            id: 'start_quest',
                            text: 'Спросить о древних реликвиях',
                            response: 'Ах, древности! Да, я помню советник говорил о пропавшем амулете. Позвольте мне проверить наши записи... Ага! Есть упоминание в старых хрониках. Мне нужно найти более подробную информацию в секретном архиве. Встретимся там!',
                            unlocks: 'librarian_consulted',
                            quest_action: 'progress_quest'
                        },
                        {
                            id: 'general_books',
                            text: 'Просто посмотреть книги',
                            response: 'Конечно! У нас прекрасная коллекция. Берите любую книгу.'
                        }
                    ]
                },
                return: {
                    greeting: 'Ваше высочество! Я уже перешёл в секретный архив. Встретимся там!',
                    requires: 'librarian_consulted',
                    choices: [
                        {
                            id: 'reminder',
                            text: 'Напомнить о встрече',
                            response: 'Да-да, секретный архив! Там все древние записи о реликвиях.'
                        }
                    ]
                },
                archive: {
                    greeting: 'Ваше высочество! Отлично, что вы пришли! Я нашёл древние записи о пропавшем амулете!',
                    choices: [
                        {
                            id: 'start_quest',
                            text: 'Узнать подробности о древних записях',
                            response: 'Согласно хроникам, амулет был спрятан в особом месте. Теперь я знаю где искать! Возвращайтесь к советнику с этой информацией.',
                            unlocks: 'archive_consulted',
                            quest_action: 'complete_archive_step'
                        }
                    ]
                }
            },
            common: {
                initial: {
                    greeting: 'Добро пожаловать в библиотеку. Но только аккуратно с книгами, пожалуйста.',
                    choices: [
                        {
                            id: 'look_around',
                            text: 'Осмотреться',
                            response: 'Да, здесь много книг. Но самые ценные хранятся в закрытых разделах.'
                        }
                    ]
                },
                return: {
                    greeting: 'Вы снова здесь. Не забывайте быть аккуратными с книгами.',
                    choices: [
                        {
                            id: 'browse_books',
                            text: 'Посмотреть доступные книги',
                            response: 'Вот эти книги вы можете взять. Остальные только для знати.'
                        }
                    ]
                }
            }
        }
    },

    'herbalist': {
        id: 'herbalist',
        name: 'Травник Элиас',
        description: 'Молодой человек в зелёной одежде с корзиной трав',
        likesNoble: false,
        dialogue: {
            common: {
                initial: {
                    greeting: 'Привет! Я собираю лечебные травы. Тебе нужно что-то особенное?',
                    choices: [
                        {
                            id: 'start_quest',
                            text: 'Спросить о редких травах для зелья',
                            response: 'О, для зелья исцеления! Мне нужно собрать особые растения в теплице. Встретимся там!',
                            unlocks: 'quest_started',
                            quest_action: 'progress_herb_quest',
                            requires: 'has_herb_quest'
                        },
                        {
                            id: 'common_herbs',
                            text: 'Просто интересуюсь травами',
                            response: 'Вот базилик, мята, ромашка. Обычные, но полезные травы.'
                        }
                    ]
                },
                return: {
                    greeting: 'Ты помнишь про теплицу? Там я покажу тебе редкие растения!',
                    requires: 'quest_started',
                    choices: [
                        {
                            id: 'reminder',
                            text: 'Напомнить о встрече',
                            response: 'Да, теплица! Там растут самые мощные лечебные травы.'
                        }
                    ]
                },
                greenhouse: {
                    greeting: 'Отлично! Вот мы и в теплице. Смотри - это редчайшие травы для зелья!',
                    choices: [
                        {
                            id: 'collect_herbs',
                            text: 'Взять нужные травы',
                            response: 'Вот, возьми эти травы. Теперь возвращайся к Марте, она будет рада!',
                            unlocks: 'herbs_collected',
                            quest_action: 'complete_herb_collection'
                        }
                    ]
                }
            },
            noble: {
                initial: {
                    greeting: 'Ваше высочество... Я простой травник, не знаю, чем могу помочь.',
                    choices: [
                        {
                            id: 'polite_inquiry',
                            text: 'Вежливо спросить о травах',
                            response: 'Я... я могу показать некоторые обычные растения, если желаете.'
                        }
                    ]
                },
                return: {
                    greeting: 'Снова приветствую, ваше высочество. Всё ещё интересуетесь травами?',
                    choices: [
                        {
                            id: 'ask_herbs',
                            text: 'Да, покажите что есть',
                            response: 'Вот несколько безопасных растений для ваших покоев.'
                        }
                    ]
                }
            }
        }
    }
};

// Базовые локации NPC (где они находятся изначально)
const BaseNPCLocations = {
    'princess_chamber': [],
    'throne_room': ['royal_advisor', 'guard_captain'],
    'kitchen': ['cook'],
    'garden': ['herbalist'],
    'secret_garden': ['gardener'],
    'armory': ['weaponsmith'],
    'great_hall': [],
    'corridor_upper': [],
    'corridor_lower': [],
    'stairs_main': [],
    'private_quarters': [],
    'chapel': ['priest'],
    'pantry': [],
    'library': ['librarian'],
    'secret_archive': [],
    'greenhouse': []
};

// Правила перемещения NPC в зависимости от состояния квеста
const NPCMovementRules = {
    'librarian': {
        baseLocation: 'library',
        conditions: [
            {
                // Если игрок поговорил с библиотекарем о квесте, он перемещается в архив
                condition: (questState, npcMemory) => {
                    return npcMemory.noble && npcMemory.noble.librarian_consulted;
                },
                location: 'secret_archive'
            }
        ]
    },
    'herbalist': {
        baseLocation: 'garden',
        conditions: [
            {
                // Если игрок поговорил с травником о квесте, он перемещается в теплицу
                condition: (questState, npcMemory) => {
                    return npcMemory.common && npcMemory.common.quest_started;
                },
                location: 'greenhouse'
            }
        ]
    }
};

class NPCData {
    static getNPC(npcId) {
        return NPCs[npcId];
    }
    
    static getNPCsForLocation(location, gameState = null, character = null) {
        // Начинаем с базового списка NPC для локации
        const baseNpcIds = BaseNPCLocations[location] || [];
        let finalNpcIds = [...baseNpcIds];
        
        // Если есть состояние игры, проверяем динамические перемещения
        if (gameState && character) {
            const questState = gameState.quests[character];
            const characterMemory = gameState.npcMemory[character] || {};
            
            // Проверяем каждого NPC который может перемещаться
            Object.keys(NPCMovementRules).forEach(npcId => {
                const rules = NPCMovementRules[npcId];
                const npcMemory = characterMemory[npcId] || {};
                
                // Определяем текущую локацию NPC для этого игрока
                let currentNpcLocation = rules.baseLocation;
                
                // Проверяем условия перемещения (берём последнее выполненное условие)
                for (const rule of rules.conditions) {
                    if (rule.condition(questState, npcMemory)) {
                        currentNpcLocation = rule.location;
                    }
                }
                
                // Если NPC находится в этой локации, добавляем его
                if (currentNpcLocation === location && !finalNpcIds.includes(npcId)) {
                    finalNpcIds.push(npcId);
                }
                
                // Если NPC переместился из этой локации, убираем его
                if (currentNpcLocation !== location && finalNpcIds.includes(npcId)) {
                    finalNpcIds = finalNpcIds.filter(id => id !== npcId);
                }
            });
        }
        
        return finalNpcIds.map(id => NPCs[id]).filter(npc => npc);
    }
    
    static getNPCDialogue(npcId, playerOutfit, npcMemory = {}, currentLocation = null, questState = null, globalQuestMemory = null) {
        const npc = NPCs[npcId];
        if (!npc) return null;
        
        // Определяем, какой тип диалога использовать
        const isNobleOutfit = playerOutfit === 'princess_dress' || playerOutfit === 'court_dress';
        const outfitType = isNobleOutfit ? 'noble' : 'common';
        
        const dialogueTree = npc.dialogue[outfitType];
        if (!dialogueTree) return null;
        
        // Определяем, это первая встреча или возвращение
        const hasMetBefore = npcMemory[outfitType] && Object.keys(npcMemory[outfitType]).length > 0;
        
        let currentDialogue;
        
        // Специальная логика для NPC в особых локациях (приоритет над return диалогом)
        if (npcId === 'librarian' && currentLocation === 'secret_archive' && dialogueTree.archive) {
            currentDialogue = dialogueTree.archive;
        } else if (npcId === 'herbalist' && currentLocation === 'greenhouse' && dialogueTree.greenhouse) {
            currentDialogue = dialogueTree.greenhouse;
        } else if (hasMetBefore && dialogueTree.return) {
            // Проверяем, выполнены ли условия для диалога "return"
            if (dialogueTree.return.requires) {
                const requirementMet = npcMemory[outfitType] && npcMemory[outfitType][dialogueTree.return.requires];
                currentDialogue = requirementMet ? dialogueTree.return : dialogueTree.initial;
            } else {
                currentDialogue = dialogueTree.return;
            }
        } else {
            currentDialogue = dialogueTree.initial;
        }
        
        // Фильтруем выборы на основе памяти NPC и состояния квеста
        const availableChoices = currentDialogue.choices.filter(choice => {
            // Проверяем глобальную память квестов (приоритет)
            if (choice.quest_action && globalQuestMemory) {
                if (choice.quest_action === 'start_noble_quest' && globalQuestMemory.princess_lost_relic) return false;
                if (choice.quest_action === 'start_common_quest' && globalQuestMemory.helper_secret_potion) return false;
            }
            
            // Проверяем негативные требования (не должно быть выполнено)
            if (choice.requires_not) {
                const hasNegativeCondition = npcMemory[outfitType] && npcMemory[outfitType][choice.requires_not];
                if (hasNegativeCondition) return false;
            }
            
            // Проверяем позитивные требования
            if (choice.requires) {
                // Специальная логика для квестовых требований - проверяем конкретную информацию
                if (choice.requires === 'has_archive_info' && questState) {
                    // Проверяем, что игрок получил информацию из архива (завершил разговор с библиотекарем)
                    const quest = questState.active;
                    if (quest && quest.id === 'princess_lost_relic') {
                        const archiveStep = quest.steps.find(step => step.id === 'talk_to_librarian');
                        return archiveStep && archiveStep.completed;
                    }
                    return false;
                }
                
                if (choice.requires === 'has_herb_info' && questState) {
                    // Проверяем, что игрок получил информацию о травах (завершил разговор с травником)
                    const quest = questState.active;
                    if (quest && quest.id === 'helper_secret_potion') {
                        const herbStep = quest.steps.find(step => step.id === 'find_herbalist');
                        return herbStep && herbStep.completed;
                    }
                    return false;
                }
                
                if (choice.requires === 'has_herb_quest' && questState) {
                    // Проверяем, что у игрока есть активный квест зелья
                    const quest = questState.active;
                    return quest && quest.id === 'helper_secret_potion';
                }
                
                // Обычная проверка по памяти NPC
                return npcMemory[outfitType] && npcMemory[outfitType][choice.requires];
            }
            
            // Если нет требований, выбор доступен
            return true;
        });
        
        return {
            greeting: currentDialogue.greeting,
            choices: availableChoices
        };
    }
    
    static processDialogueChoice(npcId, choiceId, playerOutfit, npcMemory = {}, isFollowUp = false, currentChoices = [], currentLocation = null) {
        const npc = NPCs[npcId];
        if (!npc) return null;
        
        const isNobleOutfit = playerOutfit === 'princess_dress' || playerOutfit === 'court_dress';
        const outfitType = isNobleOutfit ? 'noble' : 'common';
        
        let choice;
        
        if (isFollowUp && currentChoices.length > 0) {
            // Ищем выбор в текущих доп. выборах
            choice = currentChoices.find(c => c.id === choiceId);
        } else {
            // Ищем выбор в основном диалоге
            const dialogueTree = npc.dialogue[outfitType];
            if (!dialogueTree) return null;
            
            // Используем ту же логику что и в getNPCDialogue для определения текущего диалога
            let currentDialogue;
            const hasMetBefore = npcMemory[outfitType] && Object.keys(npcMemory[outfitType]).length > 0;
            
            // Специальная логика для NPC в особых локациях (приоритет над return диалогом)
            if (npcId === 'librarian' && currentLocation === 'secret_archive' && dialogueTree.archive) {
                currentDialogue = dialogueTree.archive;
            // Специальная логика для травницы в теплице
            } else if (npcId === 'herbalist' && currentLocation === 'greenhouse' && dialogueTree.greenhouse) {
                currentDialogue = dialogueTree.greenhouse;
            } else if (hasMetBefore && dialogueTree.return) {
                // Проверяем, выполнены ли условия для диалога "return"
                if (dialogueTree.return.requires) {
                    const requirementMet = npcMemory[outfitType] && npcMemory[outfitType][dialogueTree.return.requires];
                    currentDialogue = requirementMet ? dialogueTree.return : dialogueTree.initial;
                } else {
                    currentDialogue = dialogueTree.return;
                }
            } else {
                currentDialogue = dialogueTree.initial;
            }
            
            choice = currentDialogue.choices.find(c => c.id === choiceId);
        }
        
        if (!choice) return null;
        
        // Инициализируем память для этого типа наряда если её нет
        if (!npcMemory[outfitType]) {
            npcMemory[outfitType] = {};
        }
        
        // Сохраняем в память, что был сделан этот выбор
        if (choice.unlocks) {
            npcMemory[outfitType][choice.unlocks] = true;
        }
        
        return {
            response: choice.response,
            effects: choice.effects,
            next_choices: choice.next_choices || [],
            quest_action: choice.quest_action,
            updatedMemory: npcMemory
        };
    }
    
    static getNPCAttitude(npcId, playerOutfit) {
        const npc = NPCs[npcId];
        if (!npc) return 'neutral';
        
        const isNobleOutfit = playerOutfit === 'princess_dress' || playerOutfit === 'court_dress';
        
        // Если NPC любит знатных и игрок в знатном наряде, или NPC не любит знатных и игрок НЕ в знатном наряде
        if ((npc.likesNoble && isNobleOutfit) || (!npc.likesNoble && !isNobleOutfit)) {
            return 'friendly';
        } else {
            return 'hostile';
        }
    }
}

module.exports = NPCData;