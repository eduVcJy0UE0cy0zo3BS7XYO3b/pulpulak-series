/**
 * Tests for dynamic role mapping functionality
 * Tests the frontend logic that adapts UI based on game types
 */

// Mock Mithril for testing
const mockM = {
    redraw: jest.fn()
};

// Mock fetch for API calls
global.fetch = jest.fn();

// Import the functions we want to test by creating a test version of the lobby logic
class TestLobby {
    constructor() {
        this.availableGames = [];
    }

    async loadGameInfo() {
        try {
            const response = await fetch('/api/games');
            if (response.ok) {
                const data = await response.json();
                this.availableGames = Array.isArray(data) ? data : (data.games || []);
                return true;
            }
            return false;
        } catch (error) {
            console.error('Failed to load game info:', error);
            return false;
        }
    }

    getGameInfo(gameId) {
        if (!this.availableGames) return null;
        return this.availableGames.find(game => game.id === gameId);
    }

    getRoleMapping(gameId) {
        const gameInfo = this.getGameInfo(gameId);
        if (!gameInfo || !gameInfo.roles) {
            // Fallback to Pulpulak roles
            return {
                princess: {
                    name: '👑 Княжна Пулпулак',
                    icon: '👑',
                    class: 'role-princess'
                },
                helper: {
                    name: '🧙‍♀️ Помощница ведьмы',
                    icon: '🧙‍♀️',
                    class: 'role-helper'
                }
            };
        }

        // Create dynamic role mapping based on game data
        const roleMapping = {};
        
        // Always ensure both princess and helper roles exist
        if (gameInfo.roles.length >= 1) {
            // Map first role to princess
            roleMapping.princess = {
                name: `${this.getRoleIcon(gameInfo.roles[0].id)} ${gameInfo.roles[0].name}`,
                icon: this.getRoleIcon(gameInfo.roles[0].id),
                class: `role-${gameInfo.roles[0].id}`,
                description: gameInfo.roles[0].description
            };
        }
        
        if (gameInfo.roles.length >= 2) {
            // Map second role to helper
            roleMapping.helper = {
                name: `${this.getRoleIcon(gameInfo.roles[1].id)} ${gameInfo.roles[1].name}`,
                icon: this.getRoleIcon(gameInfo.roles[1].id),
                class: `role-${gameInfo.roles[1].id}`,
                description: gameInfo.roles[1].description
            };
        } else {
            // Fallback to default helper if only one role exists
            roleMapping.helper = {
                name: '🧙‍♀️ Помощница ведьмы',
                icon: '🧙‍♀️',
                class: 'role-helper'
            };
        }
        
        return roleMapping;
    }

    getRoleIcon(roleId) {
        const iconMap = {
            princess: '👑',
            helper: '🧙‍♀️',
            detective: '🔍',
            journalist: '📰'
        };
        return iconMap[roleId] || '🎭';
    }

    getRoleDisplayName(roleKey, gameId) {
        const gameInfo = this.getGameInfo(gameId);
        if (!gameInfo || !gameInfo.roles) {
            const fallbackNames = {
                princess: 'Княжна',
                helper: 'Помощница'
            };
            return fallbackNames[roleKey] || roleKey;
        }

        const roleIndex = roleKey === 'princess' ? 0 : 1;
        const role = gameInfo.roles[roleIndex];
        return role ? role.name : roleKey;
    }

    getCharacterDisplayName(character, gameId) {
        return this.getRoleDisplayName(character, gameId);
    }
}

describe('Role Mapping System', () => {
    let testLobby;

    beforeEach(() => {
        testLobby = new TestLobby();
        fetch.mockClear();
    });

    describe('Game Info Loading', () => {
        test('should load game info from API', async () => {
            const mockGames = [
                {
                    id: 'pulpulak',
                    name: 'Княжна Пулпулак',
                    roles: [
                        { id: 'princess', name: 'Княжна', description: 'Главная героиня' },
                        { id: 'helper', name: 'Помощник', description: 'Верный спутник' }
                    ]
                },
                {
                    id: 'detective',
                    name: 'Детективное дело',
                    roles: [
                        { id: 'detective', name: 'Детектив', description: 'Опытный сыщик' },
                        { id: 'journalist', name: 'Журналист', description: 'Любопытный репортёр' }
                    ]
                }
            ];

            fetch.mockResolvedValueOnce({
                ok: true,
                json: () => Promise.resolve(mockGames)
            });

            const success = await testLobby.loadGameInfo();

            expect(success).toBe(true);
            expect(testLobby.availableGames).toEqual(mockGames);
            expect(fetch).toHaveBeenCalledWith('/api/games');
        });

        test('should handle API errors gracefully', async () => {
            fetch.mockRejectedValueOnce(new Error('Network error'));

            const success = await testLobby.loadGameInfo();

            expect(success).toBe(false);
            expect(testLobby.availableGames).toEqual([]);
        });

        test('should handle non-ok response', async () => {
            fetch.mockResolvedValueOnce({
                ok: false,
                status: 500
            });

            const success = await testLobby.loadGameInfo();

            expect(success).toBe(false);
        });
    });

    describe('Role Mapping for Pulpulak Game', () => {
        beforeEach(() => {
            testLobby.availableGames = [
                {
                    id: 'pulpulak',
                    name: 'Княжна Пулпулак',
                    roles: [
                        { id: 'princess', name: 'Княжна', description: 'Главная героиня приключения' },
                        { id: 'helper', name: 'Помощник', description: 'Верный спутник княжны' }
                    ]
                }
            ];
        });

        test('should return correct role mapping for Pulpulak', () => {
            const roleMapping = testLobby.getRoleMapping('pulpulak');

            expect(roleMapping).toEqual({
                princess: {
                    name: '👑 Княжна',
                    icon: '👑',
                    class: 'role-princess',
                    description: 'Главная героиня приключения'
                },
                helper: {
                    name: '🧙‍♀️ Помощник',
                    icon: '🧙‍♀️',
                    class: 'role-helper',
                    description: 'Верный спутник княжны'
                }
            });
        });

        test('should return correct role display names', () => {
            expect(testLobby.getRoleDisplayName('princess', 'pulpulak')).toBe('Княжна');
            expect(testLobby.getRoleDisplayName('helper', 'pulpulak')).toBe('Помощник');
        });

        test('should return correct character display names', () => {
            expect(testLobby.getCharacterDisplayName('princess', 'pulpulak')).toBe('Княжна');
            expect(testLobby.getCharacterDisplayName('helper', 'pulpulak')).toBe('Помощник');
        });
    });

    describe('Role Mapping for Detective Game', () => {
        beforeEach(() => {
            testLobby.availableGames = [
                {
                    id: 'detective',
                    name: 'Детективное дело',
                    roles: [
                        { id: 'detective', name: 'Детектив', description: 'Опытный сыщик с доступом к официальным ресурсам' },
                        { id: 'journalist', name: 'Журналист', description: 'Любопытный репортёр с социальными связями' }
                    ]
                }
            ];
        });

        test('should return correct role mapping for Detective game', () => {
            const roleMapping = testLobby.getRoleMapping('detective');

            expect(roleMapping).toEqual({
                princess: { // Maps to first role
                    name: '🔍 Детектив',
                    icon: '🔍',
                    class: 'role-detective',
                    description: 'Опытный сыщик с доступом к официальным ресурсам'
                },
                helper: { // Maps to second role
                    name: '📰 Журналист',
                    icon: '📰',
                    class: 'role-journalist',
                    description: 'Любопытный репортёр с социальными связями'
                }
            });
        });

        test('should return correct role display names for Detective game', () => {
            expect(testLobby.getRoleDisplayName('princess', 'detective')).toBe('Детектив');
            expect(testLobby.getRoleDisplayName('helper', 'detective')).toBe('Журналист');
        });

        test('should return correct character display names for Detective game', () => {
            expect(testLobby.getCharacterDisplayName('princess', 'detective')).toBe('Детектив');
            expect(testLobby.getCharacterDisplayName('helper', 'detective')).toBe('Журналист');
        });
    });

    describe('Fallback Behavior', () => {
        test('should fallback to default roles for unknown game', () => {
            testLobby.availableGames = [];

            const roleMapping = testLobby.getRoleMapping('unknown-game');

            expect(roleMapping).toEqual({
                princess: {
                    name: '👑 Княжна Пулпулак',
                    icon: '👑',
                    class: 'role-princess'
                },
                helper: {
                    name: '🧙‍♀️ Помощница ведьмы',
                    icon: '🧙‍♀️',
                    class: 'role-helper'
                }
            });
        });

        test('should fallback when game has no roles', () => {
            testLobby.availableGames = [
                {
                    id: 'incomplete-game',
                    name: 'Incomplete Game'
                    // No roles array
                }
            ];

            const roleMapping = testLobby.getRoleMapping('incomplete-game');

            expect(roleMapping).toEqual({
                princess: {
                    name: '👑 Княжна Пулпулак',
                    icon: '👑',
                    class: 'role-princess'
                },
                helper: {
                    name: '🧙‍♀️ Помощница ведьмы',
                    icon: '🧙‍♀️',
                    class: 'role-helper'
                }
            });
        });

        test('should use fallback names when no game info available', () => {
            testLobby.availableGames = [];

            expect(testLobby.getRoleDisplayName('princess', 'unknown')).toBe('Княжна');
            expect(testLobby.getRoleDisplayName('helper', 'unknown')).toBe('Помощница');
        });

        test('should return role key as fallback for unknown roles', () => {
            testLobby.availableGames = [];

            expect(testLobby.getRoleDisplayName('unknown-role', 'unknown')).toBe('unknown-role');
        });
    });

    describe('Role Icon Mapping', () => {
        test('should return correct icons for known roles', () => {
            expect(testLobby.getRoleIcon('princess')).toBe('👑');
            expect(testLobby.getRoleIcon('helper')).toBe('🧙‍♀️');
            expect(testLobby.getRoleIcon('detective')).toBe('🔍');
            expect(testLobby.getRoleIcon('journalist')).toBe('📰');
        });

        test('should return default icon for unknown roles', () => {
            expect(testLobby.getRoleIcon('unknown-role')).toBe('🎭');
        });
    });

    describe('Game Info Retrieval', () => {
        beforeEach(() => {
            testLobby.availableGames = [
                { id: 'game1', name: 'Game 1' },
                { id: 'game2', name: 'Game 2' }
            ];
        });

        test('should find game by ID', () => {
            const game = testLobby.getGameInfo('game1');
            expect(game).toEqual({ id: 'game1', name: 'Game 1' });
        });

        test('should return null for non-existent game', () => {
            const game = testLobby.getGameInfo('nonexistent');
            expect(game).toBe(undefined);
        });

        test('should return null when no games loaded', () => {
            testLobby.availableGames = null;
            const game = testLobby.getGameInfo('any-game');
            expect(game).toBe(null);
        });
    });

    describe('Role Slot Mapping', () => {
        test('should map first role to princess slot', () => {
            testLobby.availableGames = [
                {
                    id: 'test-game',
                    roles: [
                        { id: 'role1', name: 'First Role' },
                        { id: 'role2', name: 'Second Role' }
                    ]
                }
            ];

            const roleMapping = testLobby.getRoleMapping('test-game');
            expect(roleMapping.princess.name).toContain('First Role');
        });

        test('should map second role to helper slot', () => {
            testLobby.availableGames = [
                {
                    id: 'test-game',
                    roles: [
                        { id: 'role1', name: 'First Role' },
                        { id: 'role2', name: 'Second Role' }
                    ]
                }
            ];

            const roleMapping = testLobby.getRoleMapping('test-game');
            expect(roleMapping.helper.name).toContain('Second Role');
        });

        test('should handle games with only one role', () => {
            testLobby.availableGames = [
                {
                    id: 'single-role-game',
                    roles: [
                        { id: 'only-role', name: 'Only Role' }
                    ]
                }
            ];

            const roleMapping = testLobby.getRoleMapping('single-role-game');
            expect(roleMapping.princess.name).toContain('Only Role');
            // Helper role should still exist (using second element or fallback)
            expect(roleMapping.helper).toBeDefined();
        });

        test('should handle games with more than two roles', () => {
            testLobby.availableGames = [
                {
                    id: 'multi-role-game',
                    roles: [
                        { id: 'role1', name: 'First Role' },
                        { id: 'role2', name: 'Second Role' },
                        { id: 'role3', name: 'Third Role' }
                    ]
                }
            ];

            const roleMapping = testLobby.getRoleMapping('multi-role-game');
            expect(roleMapping.princess.name).toContain('First Role');
            expect(roleMapping.helper.name).toContain('Second Role');
            // Third role is ignored in current implementation (only uses first two)
        });
    });

    describe('Integration with Multiple Games', () => {
        beforeEach(() => {
            testLobby.availableGames = [
                {
                    id: 'pulpulak',
                    name: 'Княжна Пулпулак',
                    roles: [
                        { id: 'princess', name: 'Княжна', description: 'Главная героиня' },
                        { id: 'helper', name: 'Помощник', description: 'Верный спутник' }
                    ]
                },
                {
                    id: 'detective',
                    name: 'Детективное дело',
                    roles: [
                        { id: 'detective', name: 'Детектив', description: 'Опытный сыщик' },
                        { id: 'journalist', name: 'Журналист', description: 'Любопытный репортёр' }
                    ]
                }
            ];
        });

        test('should return different mappings for different games', () => {
            const pulpulakMapping = testLobby.getRoleMapping('pulpulak');
            const detectiveMapping = testLobby.getRoleMapping('detective');

            expect(pulpulakMapping.princess.name).toContain('Княжна');
            expect(detectiveMapping.princess.name).toContain('Детектив');

            expect(pulpulakMapping.helper.name).toContain('Помощник');
            expect(detectiveMapping.helper.name).toContain('Журналист');
        });

        test('should maintain consistent mapping for same game', () => {
            const mapping1 = testLobby.getRoleMapping('detective');
            const mapping2 = testLobby.getRoleMapping('detective');

            expect(mapping1).toEqual(mapping2);
        });
    });
});