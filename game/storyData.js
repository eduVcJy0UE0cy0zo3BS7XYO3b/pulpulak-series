// Данные сюжета с множественными ветвлениями
const storyScenes = {
    "awakening": {
        title: "Пробуждение",
        text: `Утренний свет пробивается сквозь тяжелые шторы. Вы медленно просыпаетесь в своей роскошной спальне, но что-то кажется необычным...
        
        Рядом с кроватью стоит девушка, очень похожая на вас - такие же русые волосы, такое же лицо. Она держит поднос с завтраком и смотрит на вас с заботливой улыбкой.
        
        "Доброе утро, сестрица!" - говорит она. "Я приготовила тебе завтрак!"`,
        choices: [
            {
                id: "greet_warmly",
                text: "Тепло поприветствовать",
                description: "Улыбнуться и поблагодарить за завтрак",
                consequence: "Повысит доверие 'сестры'",
                effects: { loyalty: +10 }
            },
            {
                id: "ask_suspicious",
                text: "Подозрительно спросить",
                description: "Кто вы? У меня нет сестры!",
                consequence: "Повысит понимание ситуации",
                effects: { awareness: +15, loyalty: -5 }
            },
            {
                id: "stay_silent",
                text: "Молчать и наблюдать",
                description: "Не выдавать своих мыслей",
                consequence: "Нейтральный подход",
                effects: { awareness: +5 }
            }
        ]
    },

    "sister_explanation": {
        title: "Объяснение 'сестры'",
        text: `"Я твоя младшая сестра, неужели забыла?" - девушка выглядит слегка растерянной. "Родители уехали на войну защищать наши земли от захватчиков. Тетя велела нам оставаться здесь и ждать писем."
        
        Она протягивает вам письмо с королевской печатью. "Вот, пришло новое послание от них!"
        
        Воспоминания кажутся туманными, но ее слова звучат... правдоподобно?`,
        choices: [
            {
                id: "read_letter",
                text: "Прочитать письмо",
                description: "Узнать, что пишут 'родители'",
                effects: { awareness: +10 }
            },
            {
                id: "ask_about_war",
                text: "Спросить о войне",
                description: "Подробнее узнать о ситуации",
                effects: { awareness: +15 }
            },
            {
                id: "trust_sister",
                text: "Довериться 'сестре'",
                description: "Принять объяснение на веру",
                effects: { loyalty: +15, awareness: -5 }
            },
            {
                id: "change_outfit",
                text: "Попросить одежду",
                description: "Нужно переодеться из ночной рубашки",
                next_scene: "outfit_selection"
            }
        ]
    },

    "outfit_selection": {
        title: "Выбор наряда",
        text: `"Конечно, нужно одеться!" - 'сестра' открывает ваш гардероб. "У нас сегодня много дел. Хочешь, поменяемся одеждой? Иногда забавно побыть в роли друг друга!"
        
        В гардеробе висят ваши роскошные княжеские платья, а 'сестра' носит простую крестьянскую одежду.`,
        choices: [
            {
                id: "wear_princess_dress",
                text: "Надеть княжеское платье",
                description: "Одеться подобающе своему статусу",
                effects: { outfit: "princess_dress" },
                consequence: "Привлечет внимание как знатная особа"
            },
            {
                id: "switch_clothes",
                text: "Поменяться одеждой",
                description: "Надеть простое платье 'сестры'",
                effects: { outfit: "common_dress", loyalty: +10 },
                consequence: "Поможет незаметно перемещаться"
            },
            {
                id: "wear_court_dress",
                text: "Выбрать придворное платье",
                description: "Элегантный компромисс",
                effects: { outfit: "court_dress" },
                consequence: "Подойдет для визитов к придворным"
            }
        ]
    },

    "castle_exploration": {
        title: "Исследование замка",
        text: `Одевшись, вы выходите в коридоры замка. Удивительно тихо для обычного дня - где все слуги? Где стража?
        
        'Сестра' идет рядом, напевая мелодию. "Куда пойдем?" - спрашивает она с улыбкой.`,
        choices: [
            {
                id: "visit_throne_room",
                text: "Пойти в тронный зал",
                description: "Навестить родителей",
                next_scene: "tragic_discovery",
                consequence: "Может изменить все!"
            },
            {
                id: "explore_kitchen",
                text: "Заглянуть на кухню",
                description: "Поговорить с поварами",
                next_scene: "kitchen_scene"
            },
            {
                id: "visit_garden",
                text: "Пройтись по саду",
                description: "Подышать свежим воздухом",
                next_scene: "garden_scene"
            },
            {
                id: "check_armory",
                text: "Посетить арсенал",
                description: "Посмотреть на оружие",
                next_scene: "armory_scene",
                requirements: [
                    { type: "outfit", value: "common_dress" }
                ]
            }
        ]
    },

    "tragic_discovery": {
        title: "Ужасное открытие",
        text: `Подходя к тронному залу, вы слышите приглушенные голоса. Заглянув в щель между створками дверей, вы видите ужасную картину...
        
        Ваши родители лежат в лужах крови, а над ними стоят люди в незнакомых доспехах. Один из них говорит: "Княжна должна быть где-то здесь. Найдите ее!"
        
        Сердце замирает. 'Сестра' хватает вас за руку: "Быстро, нужно бежать!"`,
        choices: [
            {
                id: "flee_immediately",
                text: "Бежать немедленно",
                description: "Последовать за 'сестрой'",
                effects: { awareness: +25, loyalty: +5 },
                next_scene: "escape_route"
            },
            {
                id: "try_to_help",
                text: "Попытаться помочь",
                description: "Вбежать в зал",
                effects: { awareness: +30, loyalty: -10 },
                next_scene: "failed_rescue"
            },
            {
                id: "hide_and_watch",
                text: "Спрятаться и слушать",
                description: "Узнать больше о врагах",
                effects: { awareness: +35 },
                next_scene: "gather_intel"
            }
        ]
    }

    // ... Здесь будут еще много сцен с разветвлениями
};

class StoryData {
    static getScene(sceneId, gameState = null) {
        const scene = storyScenes[sceneId];
        if (!scene) {
            return {
                title: "Неизвестная сцена",
                text: "Произошла ошибка в сюжете.",
                choices: [
                    {
                        id: "return_to_start",
                        text: "Вернуться к началу",
                        next_scene: "awakening"
                    }
                ]
            };
        }

        // Можем модифицировать сцену в зависимости от состояния игры
        if (gameState) {
            return this.adaptSceneToGameState(scene, gameState);
        }

        return scene;
    }

    static adaptSceneToGameState(scene, gameState) {
        // Адаптируем текст и выборы в зависимости от состояния игры
        const adaptedScene = { ...scene };
        
        // Пример: изменение текста в зависимости от наряда
        if (gameState.princess.currentOutfit === "common_dress") {
            adaptedScene.text = adaptedScene.text.replace(
                "роскошных княжеских платьев",
                "простой крестьянской одежды"
            );
        }

        // Фильтруем выборы в зависимости от состояния
        adaptedScene.choices = scene.choices.filter(choice => {
            if (choice.requirements) {
                return this.checkRequirements(choice.requirements, gameState);
            }
            return true;
        });

        return adaptedScene;
    }

    static checkRequirements(requirements, gameState) {
        return requirements.every(req => {
            switch (req.type) {
            case "outfit":
                return gameState.princess.currentOutfit === req.value;
            case "loyalty":
                return gameState.loyalty >= req.value;
            case "awareness":
                return gameState.awareness >= req.value;
            case "flag":
                return gameState.flags[req.value];
            default:
                return true;
            }
        });
    }

    static getAllScenes() {
        return Object.keys(storyScenes);
    }
}

module.exports = StoryData;
