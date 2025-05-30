const CoopStoryData = require('../data/coopStoryDataSCM');

describe('CoopStoryData', () => {
    describe('getScene', () => {
        test('должна вернуть сцену пробуждения', () => {
            const scene = CoopStoryData.getScene('coop_awakening');

            expect(scene).toBeDefined();
            expect(scene.title).toBe('Утреннее пробуждение');
            expect(scene.location).toBe('princess_chamber');
            expect(scene.choices.princess).toBeDefined();
            expect(scene.choices.helper).toBeDefined();
        });

        test('должна вернуть undefined для несуществующей сцены', () => {
            const scene = CoopStoryData.getScene('non_existent_scene');
            expect(scene).toBeUndefined();
        });

        test('должна содержать выборы для обоих персонажей', () => {
            const scene = CoopStoryData.getScene('coop_awakening');

            expect(scene.choices.princess.length).toBeGreaterThan(0);
            expect(scene.choices.helper.length).toBeGreaterThan(0);
            
            // Проверяем структуру выборов
            scene.choices.princess.forEach(choice => {
                expect(choice).toHaveProperty('id');
                expect(choice).toHaveProperty('text');
                expect(choice).toHaveProperty('description');
            });
        });
    });

    describe('Scene Structure', () => {
        test('каждая сцена должна иметь обязательные поля', () => {
            const sceneIds = ['coop_awakening', 'outfit_discussion'];
            
            sceneIds.forEach(sceneId => {
                const scene = CoopStoryData.getScene(sceneId);
                
                expect(scene).toHaveProperty('title');
                expect(scene).toHaveProperty('text');
                expect(scene).toHaveProperty('location');
                expect(scene).toHaveProperty('choices');
                expect(scene.choices).toHaveProperty('princess');
                expect(scene.choices).toHaveProperty('helper');
            });
        });

        test('выборы должны иметь правильную структуру', () => {
            const scene = CoopStoryData.getScene('coop_awakening');
            const allChoices = [...scene.choices.princess, ...scene.choices.helper];

            allChoices.forEach(choice => {
                expect(typeof choice.id).toBe('string');
                expect(typeof choice.text).toBe('string');
                expect(typeof choice.description).toBe('string');
                expect(typeof choice.resultText).toBe('string');
                
                // Опциональные поля
                if (choice.effects) {
                    expect(typeof choice.effects).toBe('object');
                }
                if (choice.nextScene) {
                    expect(typeof choice.nextScene).toBe('string');
                }
            });
        });
    });

    describe('Story Consistency', () => {
        test('все ссылки на следующие сцены должны существовать', () => {
            const allScenes = ['coop_awakening', 'outfit_discussion'];
            
            allScenes.forEach(sceneId => {
                const scene = CoopStoryData.getScene(sceneId);
                const allChoices = [...scene.choices.princess, ...scene.choices.helper];
                
                allChoices.forEach(choice => {
                    if (choice.nextScene) {
                        const nextScene = CoopStoryData.getScene(choice.nextScene);
                        expect(nextScene).toBeDefined();
                    }
                });
            });
        });

        test('локации должны быть валидными', () => {
            const validLocations = [
                'princess_chamber',
                'throne_room',
                'kitchen',
                'garden',
                'armory',
                'private_quarters',
                'secret_passage'
            ];
            
            const scene = CoopStoryData.getScene('coop_awakening');
            expect(validLocations).toContain(scene.location);
        });
    });
});