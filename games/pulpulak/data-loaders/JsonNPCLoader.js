/**
 * Загрузчик данных NPC из JSON файлов
 */

const fs = require('fs');
const path = require('path');

class JsonNPCLoader {
    constructor(dataPath) {
        this.dataPath = dataPath || path.join(__dirname, '../data-json/npcData.json');
        this._data = null;
    }

    /**
     * Загружает данные из JSON файла
     */
    async _loadData() {
        if (this._data !== null) {
            return this._data;
        }

        try {
            const data = await fs.promises.readFile(this.dataPath, 'utf8');
            this._data = JSON.parse(data);
            return this._data;
        } catch (error) {
            console.error('Failed to load NPC data from JSON:', error);
            throw new Error(`Cannot load NPC data: ${error.message}`);
        }
    }

    /**
     * Получить NPC по ID
     */
    async getNPC(npcId) {
        const data = await this._loadData();
        return data.npcs[npcId] || null;
    }

    /**
     * Получить NPC для локации
     */
    async getNPCsForLocation(location, gameState = null, character = null) {
        const data = await this._loadData();
        
        // Начинаем с базового списка NPC для локации
        const baseNpcIds = data.baseNPCLocations[location] || [];
        let finalNpcIds = [...baseNpcIds];
        
        // Если есть состояние игры, проверяем динамические перемещения
        if (gameState && character) {
            const questState = gameState.quests[character];
            const characterMemory = gameState.npcMemory[character] || {};
            
            // Проверяем каждого NPC который может перемещаться
            Object.keys(data.npcMovementRules).forEach(npcId => {
                const rules = data.npcMovementRules[npcId];
                const npcMemory = characterMemory[npcId] || {};
                
                // Определяем текущую локацию NPC для этого игрока
                let currentNpcLocation = rules.baseLocation;
                
                // Проверяем условия перемещения (берём последнее выполненное условие)
                for (const rule of rules.conditions) {
                    // Простая проверка условий - в реальном коде нужна более сложная логика
                    if (this._evaluateCondition(rule.condition, questState, npcMemory)) {
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
        
        return finalNpcIds.map(id => data.npcs[id]).filter(npc => npc);
    }

    /**
     * Простая оценка условий (заглушка)
     */
    _evaluateCondition(condition, questState, npcMemory) {
        // В реальной реализации здесь должна быть более сложная логика
        // Пока возвращаем false для простоты
        return false;
    }

    /**
     * Получить диалог NPC
     */
    async getNPCDialogue(npcId, playerOutfit, npcMemory = {}, currentLocation = null, questState = null, globalQuestMemory = null) {
        const npc = await this.getNPC(npcId);
        if (!npc) return null;
        
        // Определяем, какой тип диалога использовать
        const isNobleOutfit = playerOutfit === 'princess_dress' || playerOutfit === 'court_dress';
        const outfitType = isNobleOutfit ? 'noble' : 'common';
        
        const dialogueTree = npc.dialogue[outfitType];
        if (!dialogueTree) return null;
        
        // Определяем, это первая встреча или возвращение
        const hasMetBefore = npcMemory[outfitType] && Object.keys(npcMemory[outfitType]).length > 0;
        
        let currentDialogue;
        
        // Специальная логика для NPC в особых локациях
        if (npcId === 'librarian' && currentLocation === 'secret_archive' && dialogueTree.archive) {
            currentDialogue = dialogueTree.archive;
        } else if (npcId === 'herbalist' && currentLocation === 'greenhouse' && dialogueTree.greenhouse) {
            currentDialogue = dialogueTree.greenhouse;
        } else if (hasMetBefore && dialogueTree.return) {
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
            // Простая логика фильтрации - в реальной реализации сложнее
            return this._evaluateChoiceConditions(choice, npcMemory, outfitType, questState, globalQuestMemory);
        });
        
        return {
            greeting: currentDialogue.greeting,
            choices: availableChoices
        };
    }

    /**
     * Простая оценка условий выбора (заглушка)
     */
    _evaluateChoiceConditions(choice, npcMemory, outfitType, questState, globalQuestMemory) {
        // В реальной реализации здесь должна быть более сложная логика
        return true;
    }

    /**
     * Обработать выбор диалога
     */
    async processDialogueChoice(npcId, choiceId, playerOutfit, npcMemory = {}, isFollowUp = false, currentChoices = [], currentLocation = null) {
        const npc = await this.getNPC(npcId);
        if (!npc) return null;
        
        // Реализация аналогична оригинальному NPCData.processDialogueChoice
        // Упрощена для демонстрации
        
        return {
            response: "Диалог обработан (JSON версия)",
            effects: {},
            next_choices: [],
            quest_action: null,
            updatedMemory: npcMemory
        };
    }

    /**
     * Получить отношение NPC к игроку
     */
    async getNPCAttitude(npcId, playerOutfit) {
        const npc = await this.getNPC(npcId);
        if (!npc) return 'neutral';
        
        const isNobleOutfit = playerOutfit === 'princess_dress' || playerOutfit === 'court_dress';
        
        if ((npc.likesNoble && isNobleOutfit) || (!npc.likesNoble && !isNobleOutfit)) {
            return 'friendly';
        } else {
            return 'hostile';
        }
    }

    /**
     * Валидация структуры NPC
     */
    static validateNPC(npc) {
        const required = ['id', 'name', 'description', 'likesNoble', 'dialogue'];
        
        for (const field of required) {
            if (!(field in npc)) {
                return { valid: false, error: `Missing required field: ${field}` };
            }
        }

        if (typeof npc.likesNoble !== 'boolean') {
            return { valid: false, error: 'likesNoble must be a boolean' };
        }

        if (typeof npc.dialogue !== 'object') {
            return { valid: false, error: 'dialogue must be an object' };
        }

        return { valid: true };
    }

    /**
     * Валидация всех загруженных данных
     */
    async validateAllData() {
        const data = await this._loadData();
        const errors = [];

        // Проверяем структуру данных
        if (!data.npcs || !data.baseNPCLocations || !data.npcMovementRules) {
            errors.push('Missing required data sections: npcs, baseNPCLocations, or npcMovementRules');
            return { valid: false, errors };
        }

        // Валидируем каждого NPC
        for (const [npcId, npc] of Object.entries(data.npcs)) {
            const validation = JsonNPCLoader.validateNPC(npc);
            if (!validation.valid) {
                errors.push(`NPC ${npcId}: ${validation.error}`);
            }
        }

        return {
            valid: errors.length === 0,
            errors
        };
    }
}

module.exports = JsonNPCLoader;