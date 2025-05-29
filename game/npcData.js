// NPC data with outfit preferences
const NPCs = {
    'royal_advisor': {
        id: 'royal_advisor',
        name: 'Королевский советник Эдвард',
        description: 'Важный вельможа в богатых одеждах с золотой цепью',
        likesNoble: true, // Хорошо относится к княжескому наряду
        dialogue: {
            noble: {
                greeting: 'Ваше высочество! Рад видеть вас в добром здравии. Надеюсь, утро выдалось приятным?',
                choices: [
                    {
                        id: 'ask_about_kingdom',
                        text: 'Спросить о делах королевства',
                        response: 'Дела идут прекрасно, ваше высочество. Казна полна, народ доволен.'
                    },
                    {
                        id: 'ask_about_parents',
                        text: 'Узнать о родителях',
                        response: 'Их величества сейчас на совете. Обсуждают важные государственные дела.'
                    }
                ]
            },
            common: {
                greeting: 'Эй ты! Что простолюдинка делает в тронном зале? Стража!',
                choices: [
                    {
                        id: 'explain_presence',
                        text: 'Объяснить своё присутствие',
                        response: 'Не важно! Убирайся отсюда, пока я не вызвал стражу!'
                    },
                    {
                        id: 'run_away',
                        text: 'Быстро уйти',
                        response: 'И не смей больше показываться здесь!'
                    }
                ]
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
                greeting: 'Ох, милая! Проголодалась? Садись, садись! Сейчас накормлю досыта!',
                choices: [
                    {
                        id: 'accept_food',
                        text: 'С радостью поесть',
                        response: 'Вот свежий хлеб, сыр и мой фирменный пирог! Ешь, не стесняйся!',
                        effects: { item: 'marta_pie' }
                    },
                    {
                        id: 'ask_about_castle',
                        text: 'Расспросить о замке',
                        response: 'Ох, деточка, я тут уже 20 лет работаю! Многое повидала. Хочешь, расскажу пару секретов? *подмигивает*',
                        effects: { info: 'castle_secrets' }
                    }
                ]
            },
            noble: {
                greeting: 'О, ваше высочество... *нервно кланяется и вытирает руки о фартук* Чем могу служить?',
                choices: [
                    {
                        id: 'ask_for_food',
                        text: 'Попросить поесть',
                        response: 'Конечно, ваше высочество. Сейчас приготовлю что-нибудь... особенное. *нервничает*'
                    },
                    {
                        id: 'chat_casually',
                        text: 'Поговорить по-дружески',
                        response: 'Я... я не смею, ваше высочество. Простите. *отступает*'
                    }
                ]
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
                greeting: 'Простите, ваше высочество, но этот сад закрыт для посещений по приказу короля.',
                choices: [
                    {
                        id: 'insist_entry',
                        text: 'Настоять на входе',
                        response: 'Приказ есть приказ, ваше высочество. Даже для вас. *упрямо качает головой*'
                    },
                    {
                        id: 'ask_why_closed',
                        text: 'Спросить причину',
                        response: 'Его величество не объясняет причин. Просто велел никого не пускать.'
                    }
                ]
            },
            common: {
                greeting: 'Ах, деточка! Давно не видел молодых лиц в саду. Хочешь посмотреть на мои розы? Они как раз расцвели!',
                choices: [
                    {
                        id: 'see_roses',
                        text: 'Посмотреть розы',
                        response: 'Вот, смотри! Эту красавицу я вывел сам. А вот тут у меня целебные травы растут.',
                        effects: { item: 'healing_herbs' }
                    },
                    {
                        id: 'help_garden',
                        text: 'Предложить помощь',
                        response: 'Правда поможешь? Вот умница! Давай покажу тебе особое место - там растут волшебные цветы!'
                    }
                ]
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
                greeting: 'Ваше высочество! *отдаёт честь* Замок под надёжной охраной!',
                choices: [
                    {
                        id: 'ask_about_security',
                        text: 'Спросить об охране',
                        response: 'Все входы перекрыты, на стенах дозор. Можете не волноваться, ваше высочество.'
                    }
                ]
            },
            common: {
                greeting: 'Стой! Что ты здесь делаешь? Покажи разрешение!',
                choices: [
                    {
                        id: 'no_permission',
                        text: 'У меня нет разрешения...',
                        response: 'Тогда проваливай! И не попадайся мне больше на глаза!'
                    }
                ]
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
                greeting: '*продолжает работать, не поднимая головы* Чего надо?',
                choices: [
                    {
                        id: 'order_weapon',
                        text: 'Заказать оружие',
                        response: 'Для вас? Ха! Вы и меч-то держать не умеете.'
                    }
                ]
            },
            common: {
                greeting: 'О, привет! Редко вижу новые лица. Хочешь посмотреть на мою работу?',
                choices: [
                    {
                        id: 'watch_work',
                        text: 'Посмотреть на работу',
                        response: 'Смотри - это будет лучший меч в королевстве! Хочешь, научу основам?'
                    }
                ]
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
                greeting: 'Благословение вам, ваше высочество. Пришли помолиться?',
                choices: [
                    {
                        id: 'pray',
                        text: 'Помолиться',
                        response: 'Пусть боги хранят вас и вашу семью, дитя моё.'
                    }
                ]
            },
            common: {
                greeting: 'Добро пожаловать в дом божий, дитя. Все равны перед богами.',
                choices: [
                    {
                        id: 'seek_blessing',
                        text: 'Попросить благословения',
                        response: 'Иди с миром, дитя. Пусть твой путь будет светлым.'
                    }
                ]
            }
        }
    }
};

// Привязка NPC к локациям
const NPCLocations = {
    'princess_chamber': [],
    'throne_room': ['royal_advisor', 'guard_captain'],
    'kitchen': ['cook'],
    'garden': [],
    'secret_garden': ['gardener'],
    'armory': ['weaponsmith'],
    'great_hall': [],
    'corridor_upper': [],
    'corridor_lower': [],
    'stairs_main': [],
    'private_quarters': [],
    'chapel': ['priest'],
    'pantry': []
};

class NPCData {
    static getNPC(npcId) {
        return NPCs[npcId];
    }
    
    static getNPCsForLocation(location) {
        const npcIds = NPCLocations[location] || [];
        return npcIds.map(id => NPCs[id]).filter(npc => npc);
    }
    
    static getNPCDialogue(npcId, playerOutfit) {
        const npc = NPCs[npcId];
        if (!npc) return null;
        
        // Определяем, какой тип диалога использовать
        const isNobleOutfit = playerOutfit === 'princess_dress' || playerOutfit === 'court_dress';
        
        // Если игрок в знатном наряде, используем диалог для знатных, иначе для простолюдинов
        if (isNobleOutfit) {
            return npc.dialogue.noble;
        } else {
            return npc.dialogue.common;
        }
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