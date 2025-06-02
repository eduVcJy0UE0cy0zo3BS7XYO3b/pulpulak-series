/**
 * Утилита для конвертации данных игры из JS модулей в JSON файлы
 */

const fs = require('fs');
const path = require('path');

// Импорт исходных данных
const CoopStoryData = require('../data/coopStoryData');
const LocationData = require('../data/locationData');
const NPCData = require('../data/npcData');
const QuestData = require('../data/questData');

class DataConverter {
    /**
     * Конвертирует истории в JSON
     */
    static async convertStoryData() {
        console.log('🔄 Конвертация данных историй...');
        
        try {
            // Получаем все сцены
            const sceneIds = CoopStoryData.getAllScenes();
            const scenes = {};
            
            sceneIds.forEach(sceneId => {
                const scene = CoopStoryData.getScene(sceneId);
                if (scene) {
                    scenes[sceneId] = scene;
                }
            });
            
            const outputPath = path.join(__dirname, '../data-json/storyData.json');
            await fs.promises.mkdir(path.dirname(outputPath), { recursive: true });
            await fs.promises.writeFile(outputPath, JSON.stringify(scenes, null, 2));
            
            console.log('✅ История сконвертирована в:', outputPath);
            return scenes;
        } catch (error) {
            console.error('❌ Ошибка конвертации историй:', error);
            throw error;
        }
    }
    
    /**
     * Конвертирует локации в JSON
     */
    static async convertLocationData() {
        console.log('🔄 Конвертация данных локаций...');
        
        try {
            const locationIds = LocationData.getAllLocations();
            const locations = {};
            
            locationIds.forEach(locationId => {
                const location = LocationData.getLocation(locationId);
                if (location) {
                    locations[locationId] = location;
                }
            });
            
            const outputPath = path.join(__dirname, '../data-json/locationData.json');
            await fs.promises.mkdir(path.dirname(outputPath), { recursive: true });
            await fs.promises.writeFile(outputPath, JSON.stringify(locations, null, 2));
            
            console.log('✅ Локации сконвертированы в:', outputPath);
            return locations;
        } catch (error) {
            console.error('❌ Ошибка конвертации локаций:', error);
            throw error;
        }
    }
    
    /**
     * Конвертирует NPC данные в JSON
     */
    static async convertNPCData() {
        console.log('🔄 Конвертация данных NPC...');
        
        try {
            // Для NPC нужно получить приватные данные из модуля
            const NPCModule = require('../data/npcData');
            const npcDataPath = path.join(__dirname, '../data/npcData.js');
            const npcContent = await fs.promises.readFile(npcDataPath, 'utf8');
            
            // Извлекаем NPCs объект из кода (простой способ)
            const npcsMatch = npcContent.match(/const NPCs = ({[\s\S]*?});/);
            const baseLocationsMatch = npcContent.match(/const BaseNPCLocations = ({[\s\S]*?});/);
            const movementRulesMatch = npcContent.match(/const NPCMovementRules = ({[\s\S]*?});/);
            
            if (!npcsMatch || !baseLocationsMatch || !movementRulesMatch) {
                throw new Error('Не удалось извлечь данные NPC из исходного файла');
            }
            
            // Используем eval для извлечения объектов (не рекомендуется в продакшене)
            const NPCs = eval(`(${npcsMatch[1]})`);
            const BaseNPCLocations = eval(`(${baseLocationsMatch[1]})`);
            const NPCMovementRules = eval(`(${movementRulesMatch[1]})`);
            
            const npcData = {
                npcs: NPCs,
                baseNPCLocations: BaseNPCLocations,
                npcMovementRules: NPCMovementRules
            };
            
            const outputPath = path.join(__dirname, '../data-json/npcData.json');
            await fs.promises.mkdir(path.dirname(outputPath), { recursive: true });
            await fs.promises.writeFile(outputPath, JSON.stringify(npcData, null, 2));
            
            console.log('✅ NPC данные сконвертированы в:', outputPath);
            return npcData;
        } catch (error) {
            console.error('❌ Ошибка конвертации NPC:', error);
            throw error;
        }
    }
    
    /**
     * Конвертирует квесты в JSON
     */
    static async convertQuestData() {
        console.log('🔄 Конвертация данных квестов...');
        
        try {
            const allQuests = QuestData.getAllQuests();
            
            const outputPath = path.join(__dirname, '../data-json/questData.json');
            await fs.promises.mkdir(path.dirname(outputPath), { recursive: true });
            await fs.promises.writeFile(outputPath, JSON.stringify(allQuests, null, 2));
            
            console.log('✅ Квесты сконвертированы в:', outputPath);
            return allQuests;
        } catch (error) {
            console.error('❌ Ошибка конвертации квестов:', error);
            throw error;
        }
    }
    
    /**
     * Конвертирует все данные
     */
    static async convertAllData() {
        console.log('🚀 Начинаем полную конвертацию данных...');
        
        const results = {};
        
        try {
            results.stories = await this.convertStoryData();
            results.locations = await this.convertLocationData();
            results.npcs = await this.convertNPCData();
            results.quests = await this.convertQuestData();
            
            console.log('🎉 Все данные успешно сконвертированы!');
            return results;
        } catch (error) {
            console.error('❌ Ошибка общей конвертации:', error);
            throw error;
        }
    }
    
    /**
     * Создает сводный отчет о конвертации
     */
    static async generateConversionReport() {
        console.log('📊 Генерация отчета о конвертации...');
        
        try {
            const results = await this.convertAllData();
            
            const report = {
                timestamp: new Date().toISOString(),
                conversionSummary: {
                    stories: Object.keys(results.stories || {}).length,
                    locations: Object.keys(results.locations || {}).length,
                    npcs: Object.keys(results.npcs?.npcs || {}).length,
                    quests: Object.keys(results.quests || {}).length
                },
                files: [
                    '../data-json/storyData.json',
                    '../data-json/locationData.json', 
                    '../data-json/npcData.json',
                    '../data-json/questData.json'
                ],
                schemas: [
                    '../schemas/story.schema.json',
                    '../schemas/location.schema.json',
                    '../schemas/npc.schema.json',
                    '../schemas/quest.schema.json'
                ]
            };
            
            const reportPath = path.join(__dirname, '../conversion-report.json');
            await fs.promises.writeFile(reportPath, JSON.stringify(report, null, 2));
            
            console.log('📋 Отчет о конвертации сохранен в:', reportPath);
            console.log('📈 Сводка:');
            console.log(`   - Историй: ${report.conversionSummary.stories}`);
            console.log(`   - Локаций: ${report.conversionSummary.locations}`);
            console.log(`   - NPC: ${report.conversionSummary.npcs}`);
            console.log(`   - Квестов: ${report.conversionSummary.quests}`);
            
            return report;
        } catch (error) {
            console.error('❌ Ошибка генерации отчета:', error);
            throw error;
        }
    }
}

// Запуск конвертации если файл вызван напрямую
if (require.main === module) {
    DataConverter.generateConversionReport()
        .then(() => {
            console.log('✨ Конвертация завершена успешно!');
            process.exit(0);
        })
        .catch(error => {
            console.error('💥 Конвертация провалилась:', error);
            process.exit(1);
        });
}

module.exports = DataConverter;