const coopScenes = {
    "coop_awakening": {
        title: "Утреннее пробуждение",
        text: `Утренний свет пробивается сквозь тяжелые шторы княжеской спальни. 
        
        🎭 <strong>Ситуация:</strong> Княжна просыпается и видит рядом незнакомую девушку, очень похожую на неё. 
        Девушка представляется сестрой и предлагает завтрак.
        
        <em>Каждый игрок знает то, что знает его персонаж. У помощницы ведьмы есть секретная информация!</em>`,
        
        choices: {
            princess: [
                {
                    id: "princess_greet_warmly",
                    text: "Тепло поприветствовать",
                    description: "Улыбнуться и поблагодарить за завтрак",
                    effects: { loyalty: 10 },
                    resultText: "Ваша теплота располагает к себе 'сестру'"
                },
                {
                    id: "princess_ask_suspicious", 
                    text: "Задать подозрительные вопросы",
                    description: "Кто вы? У меня нет сестры!",
                    effects: { awareness: 15 },
                    resultText: "Вы начинаете понимать, что что-то не так"
                }
            ],
            helper: [
                {
                    id: "helper_explain_calmly",
                    text: "Спокойно объяснить ситуацию",
                    description: "Рассказать про родителей и войну",
                    effects: { influence: 5 },
                    resultText: "Ваше объяснение звучит убедительно"
                },
                {
                    id: "helper_use_magic",
                    text: "Активировать магию серег",
                    description: "Усилить магическое воздействие",
                    effects: { influence: 15, flag: "magic_used" },
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
                    effects: { outfit: "Княжеское платье" },
                    resultText: "Вы выглядите как настоящая княжна"
                },
                {
                    id: "princess_choose_simple",
                    text: "Выбрать простое платье", 
                    description: "Предпочесть скромность",
                    effects: { outfit: "Простое платье", loyalty: 5 },
                    resultText: "Простая одежда поможет не привлекать внимания"
                }
            ],
            helper: [
                {
                    id: "helper_suggest_switch",
                    text: "Предложить поменяться",
                    description: "Забавно побыть в роли друг друга!",
                    effects: { influence: 10 },
                    resultText: "Ваше предложение заинтриговало княжну"
                },
                {
                    id: "helper_stay_quiet",
                    text: "Молчать и наблюдать",
                    description: "Позволить княжне самой решить",
                    effects: { },
                    resultText: "Вы решили не вмешиваться в выбор"
                }
            ]
        }
    }

    // Здесь будут еще сцены с интересными взаимодействиями между персонажами
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
