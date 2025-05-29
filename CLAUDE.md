# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

### Development
```bash
# Install dependencies
npm install

# Run the main game server
npm start
# or
npm run dev

# Run the hide-and-seek mini-game server (from hide_and_seek directory)
cd hide_and_seek && npm start
```

The main server runs on http://localhost:3000

## Architecture Overview

### Project Structure
This is a cooperative multiplayer text adventure game built with Node.js/Express backend and Socket.IO for real-time communication. The game supports exactly 2 players who experience an interactive story together.

### Key Components

**Backend Architecture:**
- `server.js` - Express server setup and static file serving
- `network/socketHandler.js` - Socket.IO event handling, manages player connections and game state synchronization
- `game/coopGameLogic.js` - Core game mechanics including:
  - Player role assignment (princess/helper)
  - Outfit swapping system
  - Loyalty tracking
  - Scene progression logic
- `game/coopStoryData.js` - Story content, scenes, choices, and narrative data

**Frontend Architecture:**
- Single-page application with component-based screens
- `public/js/main.js` - Main entry point, handles screen transitions
- `public/js/socketManager.js` - Client-side Socket.IO connection management
- Screen components:
  - `mainMenu.js` - Initial menu with room creation/joining
  - `lobby.js` - Pre-game lobby for player synchronization
  - `coopGame.js` - Main game interface with story rendering and choice handling

### Game Mechanics
- **Outfit System**: Players can swap clothes when alone, affecting gameplay and NPC reactions
- **Loyalty System**: Tracks relationships with villagers based on player choices
- **Dual Narrative**: Princess and helper see different perspectives of the same story
- **Synchronous Choices**: Some decisions require both players to agree

### Data Flow
1. Players connect via Socket.IO with room codes
2. Game state is maintained server-side in `gameState.js`
3. Scene transitions and choices are processed in `coopGameLogic.js`
4. Updates are broadcast to both players simultaneously
5. Client renders scenes based on player role and current outfit