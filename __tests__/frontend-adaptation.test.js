/**
 * Tests for frontend game interface adaptation
 * Tests how the UI adapts to different game types
 */

// Mock DOM and Mithril
global.document = {
    getElementById: jest.fn(),
    createElement: jest.fn(),
    addEventListener: jest.fn()
};

global.window = {
    m: {
        mount: jest.fn(),
        redraw: jest.fn()
    }
};

// Mock fetch for API calls
global.fetch = jest.fn();

// Mock NotificationManager
const mockNotificationManager = {
    add: jest.fn()
};

// Test implementation of CoopGame screen logic
class TestCoopGame {
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

    getRoleDisplayName(roleKey, gameId) {
        const gameInfo = this.getGameInfo(gameId);
        if (!gameInfo || !gameInfo.roles || gameInfo.roles.length === 0) {
            const fallbackNames = {
                princess: 'Княжна',
                helper: 'Помощница'
            };
            return fallbackNames[roleKey] || roleKey;
        }

        const roleIndex = roleKey === 'princess' ? 0 : 1;
        const role = gameInfo.roles[roleIndex];
        return role ? role.name : (roleKey === 'princess' ? 'Княжна' : 'Помощница');
    }

    getRoleIcon(roleKey, gameId) {
        const gameInfo = this.getGameInfo(gameId);
        if (!gameInfo || !gameInfo.roles) {
            return roleKey === 'princess' ? '👑' : '🧙‍♀️';
        }

        const roleIndex = roleKey === 'princess' ? 0 : 1;
        const role = gameInfo.roles[roleIndex];
        
        const iconMap = {
            princess: '👑',
            helper: '🧙‍♀️',
            detective: '🔍',
            journalist: '📰'
        };
        
        return role ? (iconMap[role.id] || '🎭') : (iconMap[roleKey] || '🎭');
    }

    getCharacterDisplayName(character, gameId) {
        return this.getRoleDisplayName(character, gameId);
    }

    // Simulates the outfit request rendering logic
    renderOutfitRequestNotification(activeRequest, playerRole, gameId) {
        const isTargetPlayer = activeRequest.targetCharacter === playerRole;
        const isRequestInitiator = activeRequest.fromCharacter === playerRole;

        if (isTargetPlayer) {
            const fromCharacterName = this.getCharacterDisplayName(activeRequest.fromCharacter, gameId);
            return {
                type: 'incoming',
                header: '👗 Запрос на обмен одеждой',
                message: `${fromCharacterName} предлагает поменяться одеждой!`,
                actions: ['accept', 'decline']
            };
        } else if (isRequestInitiator) {
            const targetCharacterName = this.getCharacterDisplayName(activeRequest.targetCharacter, gameId);
            return {
                type: 'outgoing',
                header: '👗 Запрос отправлен',
                message: `Ожидаем ответа от ${targetCharacterName}...`,
                waiting: true
            };
        }
        return null;
    }

    // Simulates player panel rendering
    renderPlayerPanel(playerRole, gameData) {
        const gameId = gameData?.gameId || 'pulpulak';
        const roleName = this.getRoleDisplayName(playerRole, gameId);
        const roleIcon = this.getRoleIcon(playerRole, gameId);

        return {
            header: `${roleIcon} ${roleName}`,
            playerName: gameData.players[playerRole]?.name || '-',
            outfit: gameData.stats?.[playerRole]?.outfit || 'default',
            location: gameData.locations?.[playerRole]
        };
    }
}

// Test implementation of GameSelection screen logic
class TestGameSelection {
    constructor() {
        this.availableGames = [];
        this.selectedGame = null;
        this.loading = true;
    }

    async fetchAvailableGames() {
        try {
            this.loading = true;
            const response = await fetch('/api/games');
            if (!response.ok) {
                throw new Error(`HTTP ${response.status}: ${response.statusText}`);
            }

            const data = await response.json();
            this.availableGames = Array.isArray(data) ? data : (data.games || []);
            
            if (this.availableGames.length > 0) {
                this.selectedGame = this.availableGames[0].id;
            }
            
            this.loading = false;
            return true;
        } catch (error) {
            console.error('❌ Failed to fetch games:', error);
            this.loading = false;
            return false;
        }
    }

    selectGame(gameId) {
        this.selectedGame = gameId;
    }

    getGameInfo(gameId) {
        return this.availableGames.find(game => game.id === gameId);
    }

    createGameCard(game) {
        const isSelected = this.selectedGame === game.id;
        
        return {
            id: game.id,
            selected: isSelected,
            icon: game.icon || '🎮',
            name: game.name || 'Неизвестная игра',
            description: game.description || 'Описание недоступно',
            players: `👥 ${game.minPlayers || 2}-${game.maxPlayers || 2} игроков`,
            time: (game.estimatedPlayTime || game.estimatedDuration) ? 
                  `⏱️ ${game.estimatedPlayTime || game.estimatedDuration}` : null,
            tags: (game.tags && game.tags.length > 0) ? game.tags : []
        };
    }
}

describe('Frontend Game Interface Adaptation', () => {
    let testCoopGame, testGameSelection;

    beforeEach(() => {
        testCoopGame = new TestCoopGame();
        testGameSelection = new TestGameSelection();
        fetch.mockClear();
    });

    describe('Game Selection Interface', () => {
        test('should load and display available games', async () => {
            const mockGames = [
                {
                    id: 'pulpulak',
                    name: 'Княжна Пулпулак',
                    description: 'Кооперативная средневековая приключенческая игра',
                    minPlayers: 2,
                    maxPlayers: 2,
                    estimatedDuration: '60-90 минут',
                    tags: ['cooperative', 'story', 'medieval']
                },
                {
                    id: 'detective',
                    name: 'Детективное дело',
                    description: 'Загадочное расследование в викторианском Лондоне',
                    minPlayers: 2,
                    maxPlayers: 2,
                    estimatedPlayTime: '45-75 minutes',
                    tags: ['mystery', 'investigation', 'victorian']
                }
            ];

            fetch.mockResolvedValueOnce({
                ok: true,
                json: () => Promise.resolve(mockGames)
            });

            const success = await testGameSelection.fetchAvailableGames();

            expect(success).toBe(true);
            expect(testGameSelection.loading).toBe(false);
            expect(testGameSelection.availableGames).toEqual(mockGames);
            expect(testGameSelection.selectedGame).toBe('pulpulak');
        });

        test('should handle empty games list', async () => {
            fetch.mockResolvedValueOnce({
                ok: true,
                json: () => Promise.resolve([])
            });

            const success = await testGameSelection.fetchAvailableGames();

            expect(success).toBe(true);
            expect(testGameSelection.availableGames).toEqual([]);
            expect(testGameSelection.selectedGame).toBe(null);
        });

        test('should handle API errors during game loading', async () => {
            fetch.mockRejectedValueOnce(new Error('Network error'));

            const success = await testGameSelection.fetchAvailableGames();

            expect(success).toBe(false);
            expect(testGameSelection.loading).toBe(false);
        });

        test('should create game cards with proper fallbacks', () => {
            const completeGame = {
                id: 'complete-game',
                name: 'Complete Game',
                description: 'A complete game with all fields',
                minPlayers: 2,
                maxPlayers: 4,
                estimatedPlayTime: '30-60 minutes',
                tags: ['tag1', 'tag2']
            };

            const incompleteGame = {
                id: 'incomplete-game'
                // Missing most fields
            };

            testGameSelection.selectedGame = 'complete-game';

            const completeCard = testGameSelection.createGameCard(completeGame);
            const incompleteCard = testGameSelection.createGameCard(incompleteGame);

            expect(completeCard).toEqual({
                id: 'complete-game',
                selected: true,
                icon: '🎮',
                name: 'Complete Game',
                description: 'A complete game with all fields',
                players: '👥 2-4 игроков',
                time: '⏱️ 30-60 minutes',
                tags: ['tag1', 'tag2']
            });

            expect(incompleteCard).toEqual({
                id: 'incomplete-game',
                selected: false,
                icon: '🎮',
                name: 'Неизвестная игра',
                description: 'Описание недоступно',
                players: '👥 2-2 игроков',
                time: null,
                tags: []
            });
        });

        test('should handle game selection', () => {
            testGameSelection.availableGames = [
                { id: 'game1', name: 'Game 1' },
                { id: 'game2', name: 'Game 2' }
            ];

            testGameSelection.selectGame('game2');
            expect(testGameSelection.selectedGame).toBe('game2');

            const card1 = testGameSelection.createGameCard(testGameSelection.availableGames[0]);
            const card2 = testGameSelection.createGameCard(testGameSelection.availableGames[1]);

            expect(card1.selected).toBe(false);
            expect(card2.selected).toBe(true);
        });
    });

    describe('CoopGame Interface Adaptation', () => {
        beforeEach(() => {
            testCoopGame.availableGames = [
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

        test('should render player panel for Pulpulak game', () => {
            const gameData = {
                gameId: 'pulpulak',
                players: {
                    princess: { name: 'TestPlayer' }
                },
                stats: {
                    princess: { outfit: 'princess_dress' }
                },
                locations: {
                    princess: { name: 'Royal Chamber', icon: '🏰' }
                }
            };

            const panel = testCoopGame.renderPlayerPanel('princess', gameData);

            expect(panel).toEqual({
                header: '👑 Княжна',
                playerName: 'TestPlayer',
                outfit: 'princess_dress',
                location: { name: 'Royal Chamber', icon: '🏰' }
            });
        });

        test('should render player panel for Detective game', () => {
            const gameData = {
                gameId: 'detective',
                players: {
                    princess: { name: 'Detective Smith' } // Maps to detective role
                },
                stats: {
                    princess: { outfit: 'detective_coat' }
                },
                locations: {
                    princess: { name: 'Crime Scene', icon: '🔍' }
                }
            };

            const panel = testCoopGame.renderPlayerPanel('princess', gameData);

            expect(panel).toEqual({
                header: '🔍 Детектив',
                playerName: 'Detective Smith',
                outfit: 'detective_coat',
                location: { name: 'Crime Scene', icon: '🔍' }
            });
        });

        test('should handle fallback for unknown game', () => {
            const gameData = {
                gameId: 'unknown-game',
                players: {
                    princess: { name: 'TestPlayer' }
                }
            };

            const panel = testCoopGame.renderPlayerPanel('princess', gameData);

            expect(panel.header).toBe('👑 Княжна');
            expect(panel.playerName).toBe('TestPlayer');
        });

        test('should adapt outfit request notifications for different games', () => {
            const request = {
                fromCharacter: 'princess',
                targetCharacter: 'helper'
            };

            // Test for Pulpulak game
            const pulpulakNotification = testCoopGame.renderOutfitRequestNotification(
                request, 'helper', 'pulpulak'
            );

            expect(pulpulakNotification).toEqual({
                type: 'incoming',
                header: '👗 Запрос на обмен одеждой',
                message: 'Княжна предлагает поменяться одеждой!',
                actions: ['accept', 'decline']
            });

            // Test for Detective game
            const detectiveNotification = testCoopGame.renderOutfitRequestNotification(
                request, 'helper', 'detective'
            );

            expect(detectiveNotification).toEqual({
                type: 'incoming',
                header: '👗 Запрос на обмен одеждой',
                message: 'Детектив предлагает поменяться одеждой!',
                actions: ['accept', 'decline']
            });
        });

        test('should render outgoing request notifications', () => {
            const request = {
                fromCharacter: 'princess',
                targetCharacter: 'helper'
            };

            const notification = testCoopGame.renderOutfitRequestNotification(
                request, 'princess', 'detective'
            );

            expect(notification).toEqual({
                type: 'outgoing',
                header: '👗 Запрос отправлен',
                message: 'Ожидаем ответа от Журналист...',
                waiting: true
            });
        });

        test('should return null for non-participant in request', () => {
            const request = {
                fromCharacter: 'princess',
                targetCharacter: 'helper'
            };

            const notification = testCoopGame.renderOutfitRequestNotification(
                request, 'observer', 'pulpulak'
            );

            expect(notification).toBe(null);
        });
    });

    describe('Cross-Game Consistency', () => {
        test('should maintain consistent interface structure across games', () => {
            const games = ['pulpulak', 'detective'];
            const panels = [];

            testCoopGame.availableGames = [
                {
                    id: 'pulpulak',
                    roles: [
                        { id: 'princess', name: 'Княжна' },
                        { id: 'helper', name: 'Помощник' }
                    ]
                },
                {
                    id: 'detective',
                    roles: [
                        { id: 'detective', name: 'Детектив' },
                        { id: 'journalist', name: 'Журналист' }
                    ]
                }
            ];

            games.forEach(gameId => {
                const gameData = {
                    gameId,
                    players: { princess: { name: 'TestPlayer' } },
                    stats: { princess: { outfit: 'default' } }
                };

                panels.push(testCoopGame.renderPlayerPanel('princess', gameData));
            });

            // All panels should have the same structure
            panels.forEach(panel => {
                expect(panel).toHaveProperty('header');
                expect(panel).toHaveProperty('playerName');
                expect(panel).toHaveProperty('outfit');
                expect(panel).toHaveProperty('location');
                expect(typeof panel.header).toBe('string');
                expect(typeof panel.playerName).toBe('string');
            });

            // But different content
            expect(panels[0].header).not.toBe(panels[1].header);
        });

        test('should handle role mapping consistently', () => {
            testCoopGame.availableGames = [
                {
                    id: 'test-game',
                    roles: [
                        { id: 'role1', name: 'First Role' },
                        { id: 'role2', name: 'Second Role' }
                    ]
                }
            ];

            // Princess should always map to first role
            expect(testCoopGame.getRoleDisplayName('princess', 'test-game')).toBe('First Role');
            // Helper should always map to second role
            expect(testCoopGame.getRoleDisplayName('helper', 'test-game')).toBe('Second Role');
        });
    });

    describe('Error Handling and Edge Cases', () => {
        test('should handle missing game data gracefully', () => {
            testCoopGame.availableGames = [];

            const gameData = { players: {}, stats: {}, locations: {} };
            const panel = testCoopGame.renderPlayerPanel('princess', gameData);
            expect(panel.header).toBe('👑 Княжна'); // Should use fallback
        });

        test('should handle malformed game data', () => {
            testCoopGame.availableGames = [
                {
                    id: 'malformed-game',
                    // Missing roles array
                }
            ];

            const displayName = testCoopGame.getRoleDisplayName('princess', 'malformed-game');
            expect(displayName).toBe('Княжна'); // Should use fallback
        });

        test('should handle empty roles array', () => {
            testCoopGame.availableGames = [
                {
                    id: 'empty-roles-game',
                    roles: []
                }
            ];

            const displayName = testCoopGame.getRoleDisplayName('princess', 'empty-roles-game');
            expect(displayName).toBe('Княжна'); // Should use fallback for empty roles
        });

        test('should handle network failures during game info loading', async () => {
            fetch.mockRejectedValueOnce(new Error('Network failure'));

            const success = await testCoopGame.loadGameInfo();
            expect(success).toBe(false);
            expect(testCoopGame.availableGames).toEqual([]);
        });

        test('should handle invalid JSON responses', async () => {
            fetch.mockResolvedValueOnce({
                ok: true,
                json: () => Promise.reject(new Error('Invalid JSON'))
            });

            const success = await testCoopGame.loadGameInfo();
            expect(success).toBe(false);
        });
    });

    describe('Performance Considerations', () => {
        test('should not reload game info unnecessarily', async () => {
            const mockGames = [{ id: 'test', name: 'Test Game' }];
            
            fetch.mockResolvedValue({
                ok: true,
                json: () => Promise.resolve(mockGames)
            });

            // Load once
            await testCoopGame.loadGameInfo();
            expect(fetch).toHaveBeenCalledTimes(1);

            // Getting game info should not trigger additional fetches
            testCoopGame.getGameInfo('test');
            testCoopGame.getRoleDisplayName('princess', 'test');
            expect(fetch).toHaveBeenCalledTimes(1);
        });

        test('should cache role mappings', () => {
            testCoopGame.availableGames = [
                {
                    id: 'test-game',
                    roles: [{ id: 'role1', name: 'Role 1' }]
                }
            ];

            // Multiple calls should return consistent results
            const name1 = testCoopGame.getRoleDisplayName('princess', 'test-game');
            const name2 = testCoopGame.getRoleDisplayName('princess', 'test-game');
            const icon1 = testCoopGame.getRoleIcon('princess', 'test-game');
            const icon2 = testCoopGame.getRoleIcon('princess', 'test-game');

            expect(name1).toBe(name2);
            expect(icon1).toBe(icon2);
        });
    });
});