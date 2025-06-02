/**
 * Валидатор данных игры в JSON формате
 */

const fs = require('fs');
const path = require('path');
const Ajv = require('ajv');

// Импорт загрузчиков
const JsonStoryLoader = require('../data-loaders/JsonStoryLoader');
const JsonLocationLoader = require('../data-loaders/JsonLocationLoader');
const JsonNPCLoader = require('../data-loaders/JsonNPCLoader');
const JsonQuestLoader = require('../data-loaders/JsonQuestLoader');

class DataValidator {
    constructor() {
        this.ajv = new Ajv({ allErrors: true });
        this._schemas = {};
    }

    /**
     * Загружает JSON схемы
     */
    async _loadSchemas() {
        if (Object.keys(this._schemas).length > 0) {
            return; // Схемы уже загружены
        }

        const schemaFiles = {
            story: '../schemas/story.schema.json',
            location: '../schemas/location.schema.json',
            npc: '../schemas/npc.schema.json',
            quest: '../schemas/quest.schema.json'
        };

        for (const [name, filePath] of Object.entries(schemaFiles)) {
            try {
                const schemaPath = path.join(__dirname, filePath);
                const schemaData = await fs.promises.readFile(schemaPath, 'utf8');
                this._schemas[name] = JSON.parse(schemaData);
                
                // Проверяем, не добавлена ли уже схема
                if (!this.ajv.getSchema(name)) {
                    this.ajv.addSchema(this._schemas[name], name);
                }
            } catch (error) {
                console.error(`Failed to load schema ${name}:`, error);
                throw error;
            }
        }
    }

    /**
     * Валидация данных историй
     */
    async validateStoryData() {
        console.log('🔍 Валидация данных историй...');
        
        try {
            await this._loadSchemas();
            const loader = new JsonStoryLoader();
            const scenes = await loader.getAllScenesData();
            
            const validate = this.ajv.getSchema('story');
            const isValid = validate(scenes);
            
            const errors = [];
            if (!isValid) {
                errors.push(...validate.errors.map(err => 
                    `Schema validation: ${err.instancePath} ${err.message}`
                ));
            }

            // Дополнительная валидация через загрузчик
            const loaderValidation = await loader.validateAllData();
            if (!loaderValidation.valid) {
                errors.push(...loaderValidation.errors);
            }

            return {
                dataType: 'stories',
                valid: errors.length === 0,
                errors,
                itemCount: Object.keys(scenes).length
            };
        } catch (error) {
            return {
                dataType: 'stories',
                valid: false,
                errors: [`Failed to validate story data: ${error.message}`],
                itemCount: 0
            };
        }
    }

    /**
     * Валидация данных локаций
     */
    async validateLocationData() {
        console.log('🔍 Валидация данных локаций...');
        
        try {
            await this._loadSchemas();
            const loader = new JsonLocationLoader();
            const locations = await loader._loadData();
            
            const validate = this.ajv.getSchema('location');
            const isValid = validate(locations);
            
            const errors = [];
            if (!isValid) {
                errors.push(...validate.errors.map(err => 
                    `Schema validation: ${err.instancePath} ${err.message}`
                ));
            }

            // Дополнительная валидация через загрузчик
            const loaderValidation = await loader.validateAllData();
            if (!loaderValidation.valid) {
                errors.push(...loaderValidation.errors);
            }

            return {
                dataType: 'locations',
                valid: errors.length === 0,
                errors,
                itemCount: Object.keys(locations).length
            };
        } catch (error) {
            return {
                dataType: 'locations',
                valid: false,
                errors: [`Failed to validate location data: ${error.message}`],
                itemCount: 0
            };
        }
    }

    /**
     * Валидация данных NPC
     */
    async validateNPCData() {
        console.log('🔍 Валидация данных NPC...');
        
        try {
            await this._loadSchemas();
            const loader = new JsonNPCLoader();
            const npcData = await loader._loadData();
            
            const validate = this.ajv.getSchema('npc');
            const isValid = validate(npcData);
            
            const errors = [];
            if (!isValid) {
                errors.push(...validate.errors.map(err => 
                    `Schema validation: ${err.instancePath} ${err.message}`
                ));
            }

            // Дополнительная валидация через загрузчик
            const loaderValidation = await loader.validateAllData();
            if (!loaderValidation.valid) {
                errors.push(...loaderValidation.errors);
            }

            return {
                dataType: 'npcs',
                valid: errors.length === 0,
                errors,
                itemCount: Object.keys(npcData.npcs || {}).length
            };
        } catch (error) {
            return {
                dataType: 'npcs',
                valid: false,
                errors: [`Failed to validate NPC data: ${error.message}`],
                itemCount: 0
            };
        }
    }

    /**
     * Валидация данных квестов
     */
    async validateQuestData() {
        console.log('🔍 Валидация данных квестов...');
        
        try {
            await this._loadSchemas();
            const loader = new JsonQuestLoader();
            const quests = await loader.getAllQuests();
            
            const validate = this.ajv.getSchema('quest');
            const isValid = validate(quests);
            
            const errors = [];
            if (!isValid) {
                errors.push(...validate.errors.map(err => 
                    `Schema validation: ${err.instancePath} ${err.message}`
                ));
            }

            // Дополнительная валидация через загрузчик
            const loaderValidation = await loader.validateAllData();
            if (!loaderValidation.valid) {
                errors.push(...loaderValidation.errors);
            }

            return {
                dataType: 'quests',
                valid: errors.length === 0,
                errors,
                itemCount: Object.keys(quests).length
            };
        } catch (error) {
            return {
                dataType: 'quests',
                valid: false,
                errors: [`Failed to validate quest data: ${error.message}`],
                itemCount: 0
            };
        }
    }

    /**
     * Кросс-валидация между типами данных
     */
    async validateCrossReferences() {
        console.log('🔍 Кросс-валидация ссылок между данными...');
        
        try {
            const storyLoader = new JsonStoryLoader();
            const locationLoader = new JsonLocationLoader();
            const npcLoader = new JsonNPCLoader();
            const questLoader = new JsonQuestLoader();

            const [scenes, locations, npcData, quests] = await Promise.all([
                storyLoader.getAllScenesData(),
                locationLoader._loadData(),
                npcLoader._loadData(),
                questLoader.getAllQuests()
            ]);

            const errors = [];

            // Проверяем ссылки из сцен на локации
            for (const [sceneId, scene] of Object.entries(scenes)) {
                if (scene.location && !locations[scene.location]) {
                    errors.push(`Scene ${sceneId}: references non-existent location ${scene.location}`);
                }
            }

            // Проверяем ссылки из квестов на локации и NPC
            for (const [questId, quest] of Object.entries(quests)) {
                for (const step of quest.steps || []) {
                    if (step.location && !locations[step.location]) {
                        errors.push(`Quest ${questId}, step ${step.id}: references non-existent location ${step.location}`);
                    }
                    if (step.npc && !npcData.npcs[step.npc]) {
                        errors.push(`Quest ${questId}, step ${step.id}: references non-existent NPC ${step.npc}`);
                    }
                }
            }

            // Проверяем базовые локации NPC
            for (const [locationId, npcIds] of Object.entries(npcData.baseNPCLocations || {})) {
                if (!locations[locationId]) {
                    errors.push(`NPC base locations: references non-existent location ${locationId}`);
                }
                for (const npcId of npcIds) {
                    if (!npcData.npcs[npcId]) {
                        errors.push(`NPC base locations: references non-existent NPC ${npcId} in location ${locationId}`);
                    }
                }
            }

            return {
                dataType: 'cross-references',
                valid: errors.length === 0,
                errors,
                itemCount: errors.length
            };
        } catch (error) {
            return {
                dataType: 'cross-references',
                valid: false,
                errors: [`Failed to validate cross-references: ${error.message}`],
                itemCount: 0
            };
        }
    }

    /**
     * Полная валидация всех данных
     */
    async validateAllData() {
        console.log('🚀 Начинаем полную валидацию JSON данных...');
        
        const results = [];
        
        try {
            // Валидируем каждый тип данных
            results.push(await this.validateStoryData());
            results.push(await this.validateLocationData());
            results.push(await this.validateNPCData());
            results.push(await this.validateQuestData());
            results.push(await this.validateCrossReferences());

            const overallValid = results.every(result => result.valid);
            const totalErrors = results.reduce((sum, result) => sum + result.errors.length, 0);
            const totalItems = results.reduce((sum, result) => sum + result.itemCount, 0);

            // Генерируем отчет
            const report = {
                timestamp: new Date().toISOString(),
                overallValid,
                totalErrors,
                totalItems,
                results,
                summary: {
                    stories: results[0],
                    locations: results[1],
                    npcs: results[2],
                    quests: results[3],
                    crossReferences: results[4]
                }
            };

            // Сохраняем отчет
            const reportPath = path.join(__dirname, '../validation-report.json');
            await fs.promises.writeFile(reportPath, JSON.stringify(report, null, 2));

            // Выводим результаты
            console.log('\n📊 Результаты валидации:');
            results.forEach(result => {
                const status = result.valid ? '✅' : '❌';
                console.log(`${status} ${result.dataType}: ${result.itemCount} элементов, ${result.errors.length} ошибок`);
                
                if (result.errors.length > 0) {
                    result.errors.forEach(error => {
                        console.log(`   - ${error}`);
                    });
                }
            });

            console.log(`\n📋 Отчет о валидации сохранен в: ${reportPath}`);
            console.log(`📈 Общий статус: ${overallValid ? '✅ ВСЕ ДАННЫЕ ВАЛИДНЫ' : '❌ НАЙДЕНЫ ОШИБКИ'}`);
            console.log(`📊 Всего элементов: ${totalItems}, ошибок: ${totalErrors}`);

            return report;
        } catch (error) {
            console.error('💥 Критическая ошибка валидации:', error);
            throw error;
        }
    }

    /**
     * Быстрая проверка целостности
     */
    async quickValidation() {
        console.log('⚡ Быстрая проверка целостности данных...');
        
        try {
            const checks = [
                { name: 'Story files', check: () => fs.promises.access(path.join(__dirname, '../data-json/storyData.json')) },
                { name: 'Location files', check: () => fs.promises.access(path.join(__dirname, '../data-json/locationData.json')) },
                { name: 'NPC files', check: () => fs.promises.access(path.join(__dirname, '../data-json/npcData.json')) },
                { name: 'Quest files', check: () => fs.promises.access(path.join(__dirname, '../data-json/questData.json')) }
            ];

            const results = [];
            for (const { name, check } of checks) {
                try {
                    await check();
                    results.push({ name, status: '✅', error: null });
                } catch (error) {
                    results.push({ name, status: '❌', error: error.message });
                }
            }

            results.forEach(result => {
                console.log(`${result.status} ${result.name}${result.error ? ': ' + result.error : ''}`);
            });

            return results.every(result => result.status === '✅');
        } catch (error) {
            console.error('Ошибка быстрой проверки:', error);
            return false;
        }
    }
}

// Запуск валидации если файл вызван напрямую
if (require.main === module) {
    const validator = new DataValidator();
    
    const command = process.argv[2];
    
    if (command === 'quick') {
        validator.quickValidation()
            .then(success => {
                process.exit(success ? 0 : 1);
            })
            .catch(error => {
                console.error('💥 Ошибка:', error);
                process.exit(1);
            });
    } else {
        validator.validateAllData()
            .then(report => {
                console.log('✨ Валидация завершена!');
                process.exit(report.overallValid ? 0 : 1);
            })
            .catch(error => {
                console.error('💥 Валидация провалилась:', error);
                process.exit(1);
            });
    }
}

module.exports = DataValidator;