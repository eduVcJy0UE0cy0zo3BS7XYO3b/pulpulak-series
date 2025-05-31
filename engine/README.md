# Game Engine Documentation

This directory contains a generic, data-driven game engine that can power multiple different games by simply changing the configuration.

## Architecture

### Core Components

1. **GameEngine.js** - The main engine that handles game mechanics
2. **interfaces/GameConfig.js** - Interface defining what data structure games need
3. **GameEngineFactory.js** - Factory for creating engines with different game configs

### Game Configurations

Games are defined entirely through configuration objects that extend `GameConfigInterface`:

- **games/pulpulak/PulpulakGameConfig.js** - The original cooperative adventure
- **games/detective/DetectiveGameConfig.js** - A detective mystery game

## Features

The engine supports:
- **Turn-based gameplay** with multiple players
- **Location system** with movement between areas
- **NPC interaction system** with dynamic dialogues
- **Outfit/disguise system** for different social interactions
- **Quest system** for structured objectives
- **Dynamic scene progression**
- **State management** with immutable updates

## Usage

### Creating a New Game Engine

```javascript
const GameEngineFactory = require('./engine/GameEngineFactory');

// Create an engine for the pulpulak game
const pulpulakEngine = GameEngineFactory.createEngine('pulpulak');

// Create an engine for the detective game
const detectiveEngine = GameEngineFactory.createEngine('detective');

// Start a game
const gameData = pulpulakEngine.startGame('room123', {
    princess: { id: 'player1', name: 'Alice' },
    helper: { id: 'player2', name: 'Bob' }
});
```

### Creating a New Game

To create a new game, extend the `GameConfigInterface`:

```javascript
const GameConfigInterface = require('./engine/interfaces/GameConfig');

class MyGameConfig extends GameConfigInterface {
    constructor() {
        super();
        
        this.gameId = 'my_game';
        this.gameName = 'My Awesome Game';
        
        // Define characters
        this.characters = {
            hero: { name: 'Hero', description: 'The main character' },
            sidekick: { name: 'Sidekick', description: 'The helper' }
        };
        
        // Define locations
        this.locations = {
            start: {
                name: 'Starting Location',
                description: 'Where the adventure begins',
                connections: ['forest'],
                canChangeOutfit: true,
                icon: '🏠',
                npcs: ['villager']
            }
        };
        
        // Define NPCs, scenes, quests...
    }
}
```

Then register it with the factory:

```javascript
GameEngineFactory.registerGame('my_game', MyGameConfig);
```

## Game Configuration Structure

### Required Properties

- `gameId` - Unique identifier
- `gameName` - Display name
- `characters` - Object defining player characters
- `locations` - Object defining game world
- `scenes` - Object defining story scenes
- `npcs` - Object defining non-player characters
- `startingScene` - ID of first scene
- `initialState` - Starting game state

### Optional Features

- `outfits` - Clothing/disguise system
- `quests` - Structured objectives
- `features` - Which engine features to enable

## Extending the Engine

The engine is designed to be extensible. You can:

1. **Add new features** by extending the `GameEngine` class
2. **Create custom dialogue systems** by overriding NPC interaction methods
3. **Implement game-specific mechanics** in your configuration
4. **Add new choice types** by extending the choice processing system

## Examples

### Pulpulak Game
A cooperative medieval adventure where players can swap outfits to interact differently with NPCs, complete quests, and explore a castle.

### Detective Game  
A modern mystery where a detective and journalist investigate a theft, using different approaches (formal vs casual) to gather information.

Both games use the exact same engine but feel completely different due to their data configuration.

## Benefits of This Architecture

1. **Reusability** - Same engine powers multiple games
2. **Maintainability** - Bug fixes benefit all games
3. **Rapid Development** - New games are mostly data configuration
4. **Consistency** - All games share the same reliable mechanics
5. **Testability** - Engine can be tested independently of content