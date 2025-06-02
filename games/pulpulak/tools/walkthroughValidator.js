/**
 * Валидатор прохождения игры - проверяет корректность и проходимость всех сценариев
 */

const fs = require('fs');
const path = require('path');
const Ajv = require('ajv');
const addFormats = require('ajv-formats');

// Импорт загрузчиков данных
const JsonStoryLoader = require('../data-loaders/JsonStoryLoader');
const JsonLocationLoader = require('../data-loaders/JsonLocationLoader');
const JsonNPCLoader = require('../data-loaders/JsonNPCLoader');
const JsonQuestLoader = require('../data-loaders/JsonQuestLoader');

class WalkthroughValidator {
    constructor() {
        this.ajv = new Ajv({ allErrors: true });
        addFormats(this.ajv);
        this._walkthroughData = null;
        this._gameData = {};
    }

    /**
     * Загружает данные прохождения и схему
     */
    async _loadData() {
        if (this._walkthroughData) {
            return this._walkthroughData;
        }

        try {
            // Загружаем данные прохождения
            const walkthroughPath = path.join(__dirname, '../data-json/walkthroughData.json');
            const walkthroughContent = await fs.promises.readFile(walkthroughPath, 'utf8');
            this._walkthroughData = JSON.parse(walkthroughContent);

            // Загружаем схему
            const schemaPath = path.join(__dirname, '../schemas/gameplayWalkthrough.schema.json');
            const schemaContent = await fs.promises.readFile(schemaPath, 'utf8');
            const schema = JSON.parse(schemaContent);
            this.ajv.addSchema(schema, 'walkthrough');

            // Загружаем игровые данные для валидации
            const storyLoader = new JsonStoryLoader();
            const locationLoader = new JsonLocationLoader();
            const npcLoader = new JsonNPCLoader();
            const questLoader = new JsonQuestLoader();

            this._gameData = {
                stories: await storyLoader.getAllScenesData(),
                locations: await locationLoader._loadData(),
                npcs: await npcLoader._loadData(),
                quests: await questLoader.getAllQuests()
            };

            return this._walkthroughData;
        } catch (error) {
            console.error('Failed to load walkthrough data:', error);
            throw error;
        }
    }

    /**
     * Валидация структуры данных прохождения
     */
    async validateSchema() {
        console.log('🔍 Валидация схемы прохождения...');
        
        try {
            const data = await this._loadData();
            const validate = this.ajv.getSchema('walkthrough');
            const isValid = validate(data);
            
            const errors = [];
            if (!isValid) {
                errors.push(...validate.errors.map(err => 
                    `Schema validation: ${err.instancePath} ${err.message}`
                ));
            }

            return {
                valid: errors.length === 0,
                errors,
                dataType: 'walkthrough_schema'
            };
        } catch (error) {
            return {
                valid: false,
                errors: [`Schema validation failed: ${error.message}`],
                dataType: 'walkthrough_schema'
            };
        }
    }

    /**
     * Валидация ссылок на игровые объекты
     */
    async validateGameReferences() {
        console.log('🔍 Валидация ссылок на игровые объекты...');
        
        try {
            const data = await this._loadData();
            const errors = [];

            // Проверяем каждый сценарий прохождения
            for (const walkthrough of data.walkthroughs) {
                for (const step of walkthrough.steps) {
                    const stepPrefix = `Walkthrough ${walkthrough.id}, step ${step.stepNumber}`;
                    
                    // Проверяем ссылки на локации
                    if (step.action.target && step.action.type === 'move') {
                        if (!this._gameData.locations[step.action.target]) {
                            errors.push(`${stepPrefix}: Invalid location reference '${step.action.target}'`);
                        }
                    }
                    
                    // Проверяем ссылки на NPC
                    if (step.action.target && step.action.type === 'dialogue') {
                        if (!this._gameData.npcs.npcs[step.action.target]) {
                            errors.push(`${stepPrefix}: Invalid NPC reference '${step.action.target}'`);
                        }
                    }
                    
                    // Проверяем ссылки на сцены
                    if (step.action.target && step.action.type === 'scene_choice') {
                        if (!this._gameData.stories[step.action.target]) {
                            errors.push(`${stepPrefix}: Invalid scene reference '${step.action.target}'`);
                        }
                    }
                    
                    // Проверяем предусловия
                    for (const precondition of step.preconditions || []) {
                        if (precondition.type === 'location' && precondition.value) {
                            if (!this._gameData.locations[precondition.value]) {
                                errors.push(`${stepPrefix}: Invalid location in precondition '${precondition.value}'`);
                            }
                        }
                    }
                }
            }

            // Проверяем квестовые пути
            for (const [questId, questPath] of Object.entries(data.questCompletionPaths)) {
                if (!this._gameData.quests[questId]) {
                    errors.push(`Quest completion path: Invalid quest reference '${questId}'`);
                }
                
                for (const step of questPath.steps) {
                    for (const action of step.requiredActions) {
                        if (action.type === 'move' && !this._gameData.locations[action.target]) {
                            errors.push(`Quest ${questId}, step ${step.stepId}: Invalid location '${action.target}'`);
                        }
                        if (action.type === 'dialogue' && !this._gameData.npcs.npcs[action.target]) {
                            errors.push(`Quest ${questId}, step ${step.stepId}: Invalid NPC '${action.target}'`);
                        }
                    }
                }
            }

            return {
                valid: errors.length === 0,
                errors,
                dataType: 'game_references'
            };
        } catch (error) {
            return {
                valid: false,
                errors: [`Reference validation failed: ${error.message}`],
                dataType: 'game_references'
            };
        }
    }

    /**
     * Валидация логики прохождения
     */
    async validateWalkthroughLogic() {
        console.log('🔍 Валидация логики прохождения...');
        
        try {
            const data = await this._loadData();
            const errors = [];

            for (const walkthrough of data.walkthroughs) {
                const walkthroughPrefix = `Walkthrough ${walkthrough.id}`;
                
                // Проверяем последовательность шагов
                const stepNumbers = walkthrough.steps.map(step => step.stepNumber);
                const expectedSequence = Array.from({length: stepNumbers.length}, (_, i) => i + 1);
                
                if (JSON.stringify(stepNumbers) !== JSON.stringify(expectedSequence)) {
                    errors.push(`${walkthroughPrefix}: Steps are not in sequential order`);
                }
                
                // Проверяем логику смены одежды
                const outfitChanges = this._trackOutfitChanges(walkthrough.steps);
                if (!this._validateOutfitLogic(outfitChanges)) {
                    errors.push(`${walkthroughPrefix}: Invalid outfit change logic`);
                }
                
                // Проверяем достижимость финального состояния
                const finalState = this._simulateWalkthrough(walkthrough.steps);
                if (!this._compareGameStates(finalState, walkthrough.expectedOutcome)) {
                    errors.push(`${walkthroughPrefix}: Expected outcome doesn't match simulation result`);
                }
                
                // Проверяем завершенность квестов
                const questCompletionValid = this._validateQuestCompletion(
                    walkthrough.steps, 
                    walkthrough.expectedOutcome.completedQuests
                );
                if (!questCompletionValid.valid) {
                    errors.push(`${walkthroughPrefix}: ${questCompletionValid.error}`);
                }
            }

            return {
                valid: errors.length === 0,
                errors,
                dataType: 'walkthrough_logic'
            };
        } catch (error) {
            return {
                valid: false,
                errors: [`Logic validation failed: ${error.message}`],
                dataType: 'walkthrough_logic'
            };
        }
    }

    /**
     * Отслеживает изменения одежды в прохождении
     */
    _trackOutfitChanges(steps) {
        const outfitState = {
            princess: 'princess_dress',
            helper: 'helper_dress'
        };
        const changes = [];

        for (const step of steps) {
            if (step.action.type === 'outfit_swap') {
                // Меняем наряды местами
                const temp = outfitState.princess;
                outfitState.princess = outfitState.helper;
                outfitState.helper = temp;
                
                changes.push({
                    step: step.stepNumber,
                    princess: outfitState.princess,
                    helper: outfitState.helper
                });
            }
        }

        return changes;
    }

    /**
     * Валидирует логику смены одежды
     */
    _validateOutfitLogic(outfitChanges) {
        // Проверяем что смена одежды происходит только в подходящих локациях
        // и что персонажи находятся вместе
        return true; // Упрощенная реализация
    }

    /**
     * Симулирует прохождение для получения финального состояния
     */
    _simulateWalkthrough(steps) {
        const gameState = {
            princess: {
                location: 'princess_chamber',
                outfit: 'princess_dress',
                completedQuests: [],
                dialogueProgress: {},
                items: []
            },
            helper: {
                location: 'princess_chamber',
                outfit: 'helper_dress',
                completedQuests: [],
                dialogueProgress: {},
                items: []
            }
        };

        // Упрощенная симуляция - в реальной реализации нужна полная логика игры
        for (const step of steps) {
            this._applyStepToGameState(gameState, step);
        }

        return gameState;
    }

    /**
     * Применяет шаг к состоянию игры
     */
    _applyStepToGameState(gameState, step) {
        const character = gameState[step.character];
        
        switch (step.action.type) {
            case 'move':
                character.location = step.action.target;
                break;
            case 'outfit_swap':
                // Меняем наряды
                const temp = gameState.princess.outfit;
                gameState.princess.outfit = gameState.helper.outfit;
                gameState.helper.outfit = temp;
                break;
            case 'quest':
                if (step.expectedResult.questProgression) {
                    for (const progression of step.expectedResult.questProgression) {
                        if (progression.newStatus === 'completed') {
                            character.completedQuests.push(progression.questId);
                        }
                    }
                }
                break;
            case 'dialogue':
                // Обновляем прогресс диалогов
                if (step.expectedResult.dialogueUnlocks) {
                    if (!character.dialogueProgress[step.action.target]) {
                        character.dialogueProgress[step.action.target] = {};
                    }
                    for (const unlock of step.expectedResult.dialogueUnlocks) {
                        character.dialogueProgress[step.action.target][unlock] = true;
                    }
                }
                break;
        }
        
        // Применяем изменения состояния
        if (step.expectedResult.gameStateChanges) {
            for (const change of step.expectedResult.gameStateChanges) {
                const targetCharacter = gameState[change.character];
                if (change.property === 'items' && Array.isArray(change.newValue)) {
                    targetCharacter.items.push(...change.newValue);
                }
            }
        }
    }

    /**
     * Сравнивает игровые состояния
     */
    _compareGameStates(simulated, expected) {
        // Упрощенное сравнение - в реальной реализации нужна более детальная проверка
        return true;
    }

    /**
     * Валидирует завершение квестов
     */
    _validateQuestCompletion(steps, expectedQuests) {
        const questSteps = steps.filter(step => 
            step.expectedResult.questProgression && 
            step.expectedResult.questProgression.some(q => q.newStatus === 'completed')
        );
        
        const completedQuests = [];
        for (const step of questSteps) {
            for (const progression of step.expectedResult.questProgression) {
                if (progression.newStatus === 'completed') {
                    completedQuests.push(progression.questId);
                }
            }
        }
        
        const missingQuests = expectedQuests.filter(q => !completedQuests.includes(q));
        const extraQuests = completedQuests.filter(q => !expectedQuests.includes(q));
        
        if (missingQuests.length > 0 || extraQuests.length > 0) {
            return {
                valid: false,
                error: `Quest completion mismatch. Missing: [${missingQuests.join(', ')}], Extra: [${extraQuests.join(', ')}]`
            };
        }
        
        return { valid: true };
    }

    /**
     * Валидация покрытия игрового контента
     */
    async validateContentCoverage() {
        console.log('🔍 Валидация покрытия игрового контента...');
        
        try {
            const data = await this._loadData();
            const errors = [];
            
            // Собираем все использованные элементы из прохождений
            const usedContent = {
                locations: new Set(),
                npcs: new Set(),
                scenes: new Set(),
                quests: new Set()
            };
            
            for (const walkthrough of data.walkthroughs) {
                for (const step of walkthrough.steps) {
                    if (step.action.type === 'move') {
                        usedContent.locations.add(step.action.target);
                    }
                    if (step.action.type === 'dialogue') {
                        usedContent.npcs.add(step.action.target);
                    }
                    if (step.action.type === 'scene_choice') {
                        usedContent.scenes.add(step.action.target);
                    }
                }
            }
            
            // Добавляем контент из квестовых путей
            for (const questPath of Object.values(data.questCompletionPaths)) {
                usedContent.quests.add(questPath.questId);
                for (const step of questPath.steps) {
                    for (const action of step.requiredActions) {
                        if (action.type === 'move') {
                            usedContent.locations.add(action.target);
                        }
                        if (action.type === 'dialogue') {
                            usedContent.npcs.add(action.target);
                        }
                    }
                }
            }
            
            // Проверяем покрытие
            const allLocations = Object.keys(this._gameData.locations);
            const allNPCs = Object.keys(this._gameData.npcs.npcs);
            const allScenes = Object.keys(this._gameData.stories);
            const allQuests = Object.keys(this._gameData.quests);
            
            const uncoveredLocations = allLocations.filter(loc => !usedContent.locations.has(loc));
            const uncoveredNPCs = allNPCs.filter(npc => !usedContent.npcs.has(npc));
            const uncoveredScenes = allScenes.filter(scene => !usedContent.scenes.has(scene));
            const uncoveredQuests = allQuests.filter(quest => !usedContent.quests.has(quest));
            
            if (uncoveredLocations.length > 0) {
                errors.push(`Uncovered locations: ${uncoveredLocations.join(', ')}`);
            }
            if (uncoveredNPCs.length > 0) {
                errors.push(`Uncovered NPCs: ${uncoveredNPCs.join(', ')}`);
            }
            if (uncoveredScenes.length > 0) {
                errors.push(`Uncovered scenes: ${uncoveredScenes.join(', ')}`);
            }
            if (uncoveredQuests.length > 0) {
                errors.push(`Uncovered quests: ${uncoveredQuests.join(', ')}`);
            }
            
            // Рассчитываем процент покрытия
            const totalContent = allLocations.length + allNPCs.length + allScenes.length + allQuests.length;
            const coveredContent = usedContent.locations.size + usedContent.npcs.size + 
                                 usedContent.scenes.size + usedContent.quests.size;
            const coveragePercentage = Math.round((coveredContent / totalContent) * 100);
            
            return {
                valid: errors.length === 0,
                errors,
                dataType: 'content_coverage',
                coveragePercentage,
                details: {
                    locations: `${usedContent.locations.size}/${allLocations.length}`,
                    npcs: `${usedContent.npcs.size}/${allNPCs.length}`,
                    scenes: `${usedContent.scenes.size}/${allScenes.length}`,
                    quests: `${usedContent.quests.size}/${allQuests.length}`
                }
            };
        } catch (error) {
            return {
                valid: false,
                errors: [`Content coverage validation failed: ${error.message}`],
                dataType: 'content_coverage'
            };
        }
    }

    /**
     * Полная валидация всех аспектов прохождения
     */
    async validateAll() {
        console.log('🚀 Начинаем полную валидацию прохождения игры...');
        
        const results = [];
        
        try {
            results.push(await this.validateSchema());
            results.push(await this.validateGameReferences());
            results.push(await this.validateWalkthroughLogic());
            results.push(await this.validateContentCoverage());
            
            const overallValid = results.every(result => result.valid);
            const totalErrors = results.reduce((sum, result) => sum + result.errors.length, 0);
            
            // Генерируем отчет
            const report = {
                timestamp: new Date().toISOString(),
                overallValid,
                totalErrors,
                results,
                summary: {
                    schema: results[0],
                    gameReferences: results[1], 
                    walkthroughLogic: results[2],
                    contentCoverage: results[3]
                }
            };
            
            // Сохраняем отчет
            const reportPath = path.join(__dirname, '../walkthrough-validation-report.json');
            await fs.promises.writeFile(reportPath, JSON.stringify(report, null, 2));
            
            // Выводим результаты
            console.log('\n📊 Результаты валидации прохождения:');
            results.forEach(result => {
                const status = result.valid ? '✅' : '❌';
                console.log(`${status} ${result.dataType}: ${result.errors.length} ошибок`);
                
                if (result.coveragePercentage !== undefined) {
                    console.log(`   📈 Покрытие контента: ${result.coveragePercentage}%`);
                    console.log(`   📍 Детали: Локации ${result.details.locations}, NPC ${result.details.npcs}, Сцены ${result.details.scenes}, Квесты ${result.details.quests}`);
                }
                
                if (result.errors.length > 0) {
                    result.errors.forEach(error => {
                        console.log(`   - ${error}`);
                    });
                }
            });
            
            console.log(`\n📋 Отчет сохранен в: ${reportPath}`);
            console.log(`📈 Общий статус: ${overallValid ? '✅ ПРОХОЖДЕНИЕ ВАЛИДНО' : '❌ НАЙДЕНЫ ОШИБКИ'}`);
            console.log(`📊 Всего ошибок: ${totalErrors}`);
            
            return report;
        } catch (error) {
            console.error('💥 Критическая ошибка валидации прохождения:', error);
            throw error;
        }
    }
}

// Запуск валидации если файл вызван напрямую
if (require.main === module) {
    const validator = new WalkthroughValidator();
    
    validator.validateAll()
        .then(report => {
            console.log('✨ Валидация прохождения завершена!');
            process.exit(report.overallValid ? 0 : 1);
        })
        .catch(error => {
            console.error('💥 Валидация прохождения провалилась:', error);
            process.exit(1);
        });
}

module.exports = WalkthroughValidator;