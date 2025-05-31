// Система квестов для принцессы и помощницы
const Quests = {
    // Квест для княжны: найти потерянную реликвию
    'princess_lost_relic': {
        id: 'princess_lost_relic',
        character: 'princess',
        title: 'Потерянная королевская реликвия',
        description: 'Найти древний амулет, который пропал из сокровищницы',
        steps: [
            {
                id: 'get_quest',
                description: 'Поговорить с королевским советником о пропаже',
                location: 'throne_room',
                npc: 'royal_advisor',
                completed: false
            },
            {
                id: 'search_library',
                description: 'Найти библиотекаря для поиска подсказок',
                location: 'library',
                npc: 'librarian',
                completed: false
            },
            {
                id: 'talk_to_librarian',
                description: 'Поговорить с библиотекарем о древних записях',
                location: 'secret_archive',
                npc: 'librarian',
                completed: false
            },
            {
                id: 'return_to_advisor',
                description: 'Вернуться к советнику с информацией',
                location: 'throne_room',
                npc: 'royal_advisor',
                completed: false
            }
        ],
        currentStep: 0,
        rewards: ['ancient_amulet', 'royal_gratitude']
    },

    // Квест для помощницы: секретное зелье
    'helper_secret_potion': {
        id: 'helper_secret_potion',
        character: 'helper',
        title: 'Секретное зелье исцеления',
        description: 'Найти ингредиенты для мощного зелья исцеления',
        steps: [
            {
                id: 'get_quest',
                description: 'Поговорить с поваром о лечебных травах',
                location: 'kitchen',
                npc: 'cook',
                completed: false
            },
            {
                id: 'find_herbalist',
                description: 'Найти травника в саду',
                location: 'garden',
                npc: 'herbalist',
                completed: false
            },
            {
                id: 'talk_to_herbalist',
                description: 'Поговорить с травником о редких растениях',
                location: 'greenhouse',
                npc: 'herbalist',
                completed: false
            },
            {
                id: 'return_to_cook',
                description: 'Вернуться к повару с ингредиентами',
                location: 'kitchen',
                npc: 'cook',
                completed: false
            }
        ],
        currentStep: 0,
        rewards: ['healing_potion', 'herbal_knowledge']
    }
};

class QuestData {
    static getQuest(questId) {
        return Quests[questId];
    }

    static getQuestForCharacter(character) {
        const questKey = character === 'princess' ? 'princess_lost_relic' : 'helper_secret_potion';
        return this.getQuest(questKey);
    }

    static getAllQuests() {
        return Quests;
    }

    static createQuestInstance(questId) {
        const questTemplate = this.getQuest(questId);
        if (!questTemplate) return null;

        // Создаем копию квеста для экземпляра игры
        return JSON.parse(JSON.stringify(questTemplate));
    }
}

module.exports = QuestData;