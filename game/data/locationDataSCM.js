/**
 * Location Data loaded from S-expression files
 */

const path = require('path');
const DataLoader = require('./dataLoader');

class LocationDataSCM {
    constructor() {
        this.loader = new DataLoader();
        this.locations = null;
        this.loadData();
    }

    loadData() {
        const locationsFile = path.join(__dirname, 'locations.scm');
        this.locations = this.loader.loadSExpFile(locationsFile);
    }

    static getLocation(locationId) {
        if (!this.instance) {
            this.instance = new LocationDataSCM();
        }
        return this.instance.locations[locationId];
    }
    
    static getAllLocations() {
        if (!this.instance) {
            this.instance = new LocationDataSCM();
        }
        return Object.keys(this.instance.locations);
    }
    
    static getConnections(locationId) {
        const location = this.getLocation(locationId);
        return location ? location.connections : [];
    }
    
    static canChangeOutfit(locationId) {
        const location = this.getLocation(locationId);
        return location ? location.canChangeOutfit : false;
    }
    
    static getLocationInfo(locationId) {
        const location = this.getLocation(locationId);
        if (!location) return null;
        
        const allLocations = this.instance.locations;
        
        return {
            id: locationId,
            name: location.name,
            description: location.description,
            icon: location.icon,
            canChangeOutfit: location.canChangeOutfit,
            connections: location.connections.map(connId => ({
                id: connId,
                name: allLocations[connId]?.name || connId,
                icon: allLocations[connId]?.icon || '❓'
            }))
        };
    }
}

module.exports = LocationDataSCM;