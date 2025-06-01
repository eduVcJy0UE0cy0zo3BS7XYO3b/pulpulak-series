const DetectiveGameConfig = require('../DetectiveGameConfig');

describe('DetectiveGameConfig', () => {
    let gameConfig;

    beforeEach(() => {
        gameConfig = new DetectiveGameConfig();
    });

    describe('Basic Configuration', () => {
        test('should have correct game metadata', () => {
            expect(gameConfig.gameId).toBe('detective');
            expect(gameConfig.gameName).toBe('Detective Mystery: The Missing Jewels');
            expect(gameConfig.gameVersion).toBe('1.0.0');
        });

        test('should return correct game metadata', () => {
            const metadata = gameConfig.getGameMetadata();
            expect(metadata).toEqual({
                id: 'detective',
                name: 'Detective Mystery: The Missing Jewels',
                version: '1.0.0',
                description: 'Кооперативная детективная игра о расследовании кражи драгоценностей',
                minPlayers: 2,
                maxPlayers: 2,
                estimatedPlayTime: '45-75 minutes',
                tags: ['mystery', 'investigation', 'cooperative', 'detective']
            });
        });

        test('should have required interface methods', () => {
            expect(typeof gameConfig.getCharacters).toBe('function');
            expect(typeof gameConfig.getCharacterNames).toBe('function');
            expect(typeof gameConfig.getCharacterRoles).toBe('function');
            expect(typeof gameConfig.getInitialLocation).toBe('function');
            expect(typeof gameConfig.getInitialOutfit).toBe('function');
            expect(typeof gameConfig.validateGameRules).toBe('function');
        });
    });

    describe('Character System', () => {
        test('should return correct characters', () => {
            const characters = gameConfig.getCharacters();
            expect(characters).toEqual(['detective', 'journalist']);
        });

        test('should return correct character names', () => {
            const names = gameConfig.getCharacterNames();
            expect(names).toEqual({
                detective: 'Детектив',
                journalist: 'Журналист'
            });
        });

        test('should return correct character roles', () => {
            const roles = gameConfig.getCharacterRoles();
            expect(roles).toEqual({
                detective: 'detective',
                journalist: 'journalist'
            });
        });

        test('should return correct initial locations', () => {
            expect(gameConfig.getInitialLocation('detective')).toBe('crime_scene');
            expect(gameConfig.getInitialLocation('journalist')).toBe('street');
        });

        test('should return correct initial outfits', () => {
            expect(gameConfig.getInitialOutfit('detective')).toBe('detective_coat');
            expect(gameConfig.getInitialOutfit('journalist')).toBe('casual_clothes');
        });
    });

    describe('Outfit System', () => {
        test('should return available outfits for detective', () => {
            const outfits = gameConfig.getAvailableOutfits('detective');
            expect(outfits).toEqual(['detective_coat', 'disguise']);
        });

        test('should return available outfits for journalist', () => {
            const outfits = gameConfig.getAvailableOutfits('journalist');
            expect(outfits).toEqual(['casual_clothes', 'disguise']);
        });

        test('should enable outfit swapping', () => {
            expect(gameConfig.isOutfitSwappingEnabled()).toBe(true);
        });

        test('should check outfit switching availability based on location', () => {
            const gameState = {
                stats: {
                    detective: { location: 'crime_scene' }
                }
            };

            // Mock location data
            gameConfig.getLocationData = jest.fn().mockReturnValue({
                crime_scene: { canChangeOutfit: true }
            });

            const canSwitch = gameConfig.canSwitchOutfits(gameState, 'detective');
            expect(canSwitch).toBe(true);
        });

        test('should prevent outfit switching in public locations', () => {
            const gameState = {
                stats: {
                    detective: { location: 'public_square' }
                }
            };

            gameConfig.getLocationData = jest.fn().mockReturnValue({
                public_square: { canChangeOutfit: false }
            });

            const canSwitch = gameConfig.canSwitchOutfits(gameState, 'detective');
            expect(canSwitch).toBe(false);
        });
    });

    describe('Dynamic Choices', () => {
        test('should generate outfit swap choice when allowed', () => {
            const gameState = {
                stats: {
                    detective: { location: 'private_room' }
                }
            };

            gameConfig.canSwitchOutfits = jest.fn().mockReturnValue(true);

            const choices = gameConfig.getDynamicChoices(gameState, 'detective');
            expect(choices).toHaveLength(1);
            expect(choices[0]).toEqual({
                id: 'request_outfit_swap',
                text: '🥸 Предложить поменяться одеждой',
                description: 'Предложить журналисту поменяться для маскировки',
                isOutfitRequest: true
            });
        });

        test('should not generate choices when outfit swap not allowed', () => {
            const gameState = {
                stats: {
                    detective: { location: 'public_square' }
                }
            };

            gameConfig.canSwitchOutfits = jest.fn().mockReturnValue(false);

            const choices = gameConfig.getDynamicChoices(gameState, 'detective');
            expect(choices).toHaveLength(0);
        });

        test('should create correct outfit swap choice for journalist', () => {
            gameConfig.canSwitchOutfits = jest.fn().mockReturnValue(true);

            const choices = gameConfig.getDynamicChoices({}, 'journalist');
            expect(choices[0].description).toBe('Предложить детективу поменяться для маскировки');
        });
    });

    describe('Request System', () => {
        test('should identify outfit swap requests', () => {
            expect(gameConfig.isRequestChoice('request_outfit_swap')).toBe(true);
            expect(gameConfig.isRequestChoice('other_choice')).toBe(false);
        });

        test('should return correct request type', () => {
            expect(gameConfig.getRequestTypeFromChoice('request_outfit_swap')).toBe('outfit_swap');
            expect(gameConfig.getRequestTypeFromChoice('unknown')).toBe(null);
        });

        test('should validate outfit swap requests', () => {
            const gameState = { stats: { detective: { location: 'private_room' } } };
            gameConfig.canSwitchOutfits = jest.fn().mockReturnValue(true);

            const result = gameConfig.canCreateRequest(gameState, 'outfit_swap', 'detective', {});
            expect(result.allowed).toBe(true);
            expect(result.reason).toBe(null);
        });

        test('should reject invalid outfit swap requests', () => {
            const gameState = { stats: { detective: { location: 'public_square' } } };
            gameConfig.canSwitchOutfits = jest.fn().mockReturnValue(false);

            const result = gameConfig.canCreateRequest(gameState, 'outfit_swap', 'detective', {});
            expect(result.allowed).toBe(false);
            expect(result.reason).toBe('Cannot swap outfits here');
        });

        test('should reject unknown request types', () => {
            const result = gameConfig.canCreateRequest({}, 'unknown_request', 'detective', {});
            expect(result.allowed).toBe(false);
            expect(result.reason).toBe('Unknown request type: unknown_request');
        });
    });

    describe('Request Execution', () => {
        test('should execute outfit swap successfully', () => {
            const gameState = {
                stats: {
                    detective: { outfit: 'detective_coat' },
                    journalist: { outfit: 'casual_clothes' }
                }
            };

            const request = {
                type: 'outfit_swap',
                fromCharacter: 'detective',
                toCharacter: 'journalist'
            };

            const result = gameConfig.executeRequest(gameState, request, {});

            expect(result.success).toBe(true);
            expect(result.message).toBe('Outfits swapped successfully for disguise');
            expect(gameState.stats.detective.outfit).toBe('casual_clothes');
            expect(gameState.stats.journalist.outfit).toBe('detective_coat');
        });

        test('should reject unknown request types in execution', () => {
            const request = { type: 'unknown_request' };
            const result = gameConfig.executeRequest({}, request, {});

            expect(result.success).toBe(false);
            expect(result.message).toBe('Unknown request type: unknown_request');
        });
    });

    describe('Game Validation', () => {
        test('should validate complete game state', () => {
            const validGameState = {
                stats: {
                    detective: { location: 'crime_scene', outfit: 'detective_coat' },
                    journalist: { location: 'street', outfit: 'casual_clothes' }
                },
                currentScene: 'intro'
            };

            gameConfig.scenes = { intro: {} }; // Mock scenes

            const result = gameConfig.validateGameRules(validGameState);
            expect(result.valid).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        test('should detect missing character stats', () => {
            const invalidGameState = {
                stats: {
                    detective: { location: 'crime_scene', outfit: 'detective_coat' }
                    // Missing journalist
                },
                currentScene: 'intro'
            };

            const result = gameConfig.validateGameRules(invalidGameState);
            expect(result.valid).toBe(false);
            expect(result.errors).toContain('Missing stats for character: journalist');
        });

        test('should detect missing character properties', () => {
            const invalidGameState = {
                stats: {
                    detective: { location: 'crime_scene' }, // Missing outfit
                    journalist: { outfit: 'casual_clothes' } // Missing location
                },
                currentScene: 'intro'
            };

            const result = gameConfig.validateGameRules(invalidGameState);
            expect(result.valid).toBe(false);
            expect(result.errors).toContain('Missing outfit for detective');
            expect(result.errors).toContain('Missing location for journalist');
        });

        test('should detect missing current scene', () => {
            const invalidGameState = {
                stats: {
                    detective: { location: 'crime_scene', outfit: 'detective_coat' },
                    journalist: { location: 'street', outfit: 'casual_clothes' }
                }
                // Missing currentScene
            };

            const result = gameConfig.validateGameRules(invalidGameState);
            expect(result.valid).toBe(false);
            expect(result.errors).toContain('Missing current scene');
        });

        test('should detect invalid current scene', () => {
            const invalidGameState = {
                stats: {
                    detective: { location: 'crime_scene', outfit: 'detective_coat' },
                    journalist: { location: 'street', outfit: 'casual_clothes' }
                },
                currentScene: 'nonexistent_scene'
            };

            gameConfig.scenes = { intro: {} }; // Mock scenes

            const result = gameConfig.validateGameRules(invalidGameState);
            expect(result.valid).toBe(false);
            expect(result.errors).toContain('Invalid current scene: nonexistent_scene');
        });
    });

    describe('Game Constants', () => {
        test('should return correct game constants', () => {
            gameConfig.outfits = {
                detective_coat: { name: 'Пальто детектива' },
                disguise: { name: 'Маскировка' }
            };

            const constants = gameConfig.getGameConstants();
            expect(constants).toEqual({
                OUTFIT_NAMES: {
                    detective_coat: 'Пальто детектива',
                    disguise: 'Маскировка'
                },
                CHARACTER_NAMES: {
                    detective: 'Детектив',
                    journalist: 'Журналист'
                },
                CHARACTER_ROLES: {
                    detective: 'detective',
                    journalist: 'journalist'
                }
            });
        });
    });

    describe('Handler Methods', () => {
        test('should return null for request handlers initially', () => {
            expect(gameConfig.getRequestHandlers()).toBe(null);
        });

        test('should return null for quest action handlers initially', () => {
            expect(gameConfig.getQuestActionHandlers()).toBe(null);
        });
    });
});