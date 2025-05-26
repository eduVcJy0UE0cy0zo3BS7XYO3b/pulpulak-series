const coopScenes = {
    "coop_awakening": {
        title: "Утреннее пробуждение",
        text: `Утренний свет пробивается сквозь тяжелые шторы княжеской спальни. 
        
        🎭 <strong>Ситуация:</strong> Княжна просыпается и видит рядом незнакомую девушку, очень похожую на неё. 
        Девушка представляется сестрой и предлагает завтрак.
        
        <div style="background: rgba(255,255,0,0.1); padding: 10px; border-radius: 5px; margin: 10px 0;">
        💡 <strong>Подсказка:</strong> В комнате никого нет кроме вас двоих! Любой из игроков может предложить поменяться одеждой в любой момент.
        </div>`,

        location: 'princess_chamber',
        
        choices: {
            princess: [
                {
                    id: "princess_greet_warmly",
                    text: "Тепло поприветствовать",
                    description: "Улыбнуться и поблагодарить за завтрак",
                    resultText: "Ваша теплота располагает к себе 'сестру'"
                },
                {
                    id: "princess_ask_suspicious", 
                    text: "Задать подозрительные вопросы",
                    description: "Кто вы? У меня нет сестры!",
                    resultText: "Ваши вопросы смущают 'сестру'"
                }
            ],
            helper: [
                {
                    id: "helper_explain_calmly",
                    text: "Спокойно объяснить ситуацию",
                    description: "Рассказать про родителей и войну",
                    resultText: "Ваше объяснение звучит убедительно"
                },
                {
                    id: "helper_use_magic",
                    text: "Активировать магию серег",
                    description: "Усилить магическое воздействие",
                    effects: { flag: "magic_used" },
                    resultText: "Магия делает ваши слова более убедительными"
                }
            ]
        }
    },

    "outfit_discussion": {
        title: "Обсуждение нарядов", 
        text: `'Сестра' открывает гардероб и предлагает различные варианты одежды.
        
        🎭 <strong>Динамика:</strong> Княжна может выбрать, как одеться, а помощница - влиять на её решение.
        Выбор наряда повлияет на то, как персонажей будут воспринимать в замке.`,
        
        choices: {
            princess: [
                {
                    id: "princess_choose_royal",
                    text: "Выбрать княжеское платье",
                    description: "Одеться соответственно статусу",
                    effects: { outfit: "princess_dress" },
                    resultText: "Вы выглядите как настоящая княжна"
                },
                {
                    id: "princess_choose_simple",
                    text: "Выбрать простое платье", 
                    description: "Предпочесть скромность",
                    effects: { outfit: "common_dress" },
                    resultText: "Простая одежда поможет не привлекать внимания"
                }
            ],
            helper: [
                {
                    id: "helper_suggest_switch",
                    text: "Предложить поменяться",
                    description: "Забавно побыть в роли друг друга!",
                    resultText: "Ваше предложение заинтриговало княжну"
                },
                {
                    id: "helper_stay_quiet",
                    text: "Молчать и наблюдать",
                    description: "Позволить княжне самой решить",
                    resultText: "Вы решили не вмешиваться в выбор"
                }
            ]
        }
    },

    "castle_exploration": {
        title: "Исследование замка",
        text: `Одевшись, вы выходите в коридоры замка. Удивительно тихо для обычного дня.
        
        🎭 <strong>Выбор локации:</strong> Куда направиться? В зависимости от места могут быть разные NPC.`,
        
        choices: {
            princess: [
                {
                    id: "go_to_throne_room",
                    text: "Пойти в тронный зал",
                    description: "Навестить родителей",
                    effects: { location: "throne_room" },
                    resultText: "Направляетесь к тронному залу"
                },
                {
                    id: "go_to_kitchen",
                    text: "Пойти на кухню",
                    description: "Поговорить с поварами",
                    effects: { location: "kitchen" },
                    resultText: "Идете на кухню"
                }
            ],
            helper: [
                {
                    id: "helper_suggest_garden",
                    text: "Предложить сад",
                    description: "Там можно поговорить спокойно",
                    effects: { location: "garden" },
                    resultText: "Предложили прогуляться в саду"
                },
                {
                    id: "helper_follow",
                    text: "Следовать за княжной",
                    description: "Пусть она сама выберет направление",
                    resultText: "Решили довериться выбору княжны"
                }
            ]
        }
    }
};

class CoopStoryData {
    static getScene(sceneId) {
        return coopScenes[sceneId] || {
            title: "Неизвестная сцена",
            text: "Произошла ошибка.",
            choices: { princess: [], helper: [] }
        };
    }

    static getAllScenes() {
        return Object.keys(coopScenes);
    }
}

module.exports = CoopStoryData;
