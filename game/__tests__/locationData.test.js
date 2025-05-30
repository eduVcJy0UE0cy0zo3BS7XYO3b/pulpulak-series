const LocationData = require('../data/locationDataSCM');

describe('LocationData', () => {
    describe('getLocation', () => {
        test('должна вернуть информацию о локации', () => {
            const location = LocationData.getLocation('princess_chamber');
            
            expect(location).toBeDefined();
            expect(location.name).toBe('Спальня княжны');
            expect(location.connections).toContain('corridor_upper');
            expect(location.canChangeOutfit).toBe(true);
            expect(location.icon).toBe('🛏️');
        });

        test('должна вернуть undefined для несуществующей локации', () => {
            const location = LocationData.getLocation('non_existent');
            expect(location).toBeUndefined();
        });
    });

    describe('getConnections', () => {
        test('должна вернуть список соединений для локации', () => {
            const connections = LocationData.getConnections('throne_room');
            
            expect(connections).toEqual(['corridor_upper', 'great_hall']);
        });

        test('должна вернуть пустой массив для несуществующей локации', () => {
            const connections = LocationData.getConnections('non_existent');
            expect(connections).toEqual([]);
        });
    });

    describe('canChangeOutfit', () => {
        test('должна разрешить переодевание в спальне', () => {
            expect(LocationData.canChangeOutfit('princess_chamber')).toBe(true);
            expect(LocationData.canChangeOutfit('secret_garden')).toBe(true);
            expect(LocationData.canChangeOutfit('pantry')).toBe(true);
        });

        test('не должна разрешить переодевание в публичных местах', () => {
            expect(LocationData.canChangeOutfit('throne_room')).toBe(false);
            expect(LocationData.canChangeOutfit('great_hall')).toBe(false);
            expect(LocationData.canChangeOutfit('corridor_upper')).toBe(false);
        });
    });

    describe('getLocationInfo', () => {
        test('должна вернуть полную информацию о локации с соединениями', () => {
            const info = LocationData.getLocationInfo('kitchen');
            
            expect(info.id).toBe('kitchen');
            expect(info.name).toBe('Кухня');
            expect(info.description).toContain('Большая кухня');
            expect(info.icon).toBe('🍳');
            expect(info.canChangeOutfit).toBe(false);
            
            // Проверяем соединения
            expect(info.connections).toHaveLength(3);
            const connectionIds = info.connections.map(c => c.id);
            expect(connectionIds).toContain('great_hall');
            expect(connectionIds).toContain('corridor_lower');
            expect(connectionIds).toContain('pantry');
            
            // Проверяем, что у соединений есть имена и иконки
            info.connections.forEach(conn => {
                expect(conn).toHaveProperty('name');
                expect(conn).toHaveProperty('icon');
            });
        });
    });

    describe('Location graph integrity', () => {
        test('все соединения должны быть двусторонними', () => {
            const allLocations = LocationData.getAllLocations();
            
            allLocations.forEach(locationId => {
                const connections = LocationData.getConnections(locationId);
                
                connections.forEach(connectedId => {
                    const reverseConnections = LocationData.getConnections(connectedId);
                    expect(reverseConnections).toContain(locationId);
                });
            });
        });

        test('все локации должны быть достижимы из спальни княжны', () => {
            const visited = new Set();
            const queue = ['princess_chamber'];
            
            while (queue.length > 0) {
                const current = queue.shift();
                if (visited.has(current)) continue;
                
                visited.add(current);
                const connections = LocationData.getConnections(current);
                connections.forEach(conn => {
                    if (!visited.has(conn)) {
                        queue.push(conn);
                    }
                });
            }
            
            const allLocations = LocationData.getAllLocations();
            expect(visited.size).toBe(allLocations.length);
        });
    });
});