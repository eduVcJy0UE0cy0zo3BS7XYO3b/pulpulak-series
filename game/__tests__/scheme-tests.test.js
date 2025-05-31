// Jest интеграция для Scheme тестов
// game/__tests__/scheme-tests.test.js

const { SimpleSchemeTestRunner } = require('../simple-test-runner');

describe('Pulpulak Game - Scheme Architecture Tests', () => {
  let runner;
  
  beforeAll(() => {
    runner = new SimpleSchemeTestRunner();
  });

  describe('Core Functions', () => {
    test('Game State Creation', () => {
      const state = runner.makeGameState('test-room');
      expect(state).toBeTruthy();
      expect(state.type).toBe('game-state');
      expect(state.scene).toBe('coop_awakening');
      expect(state['current-turn']).toBe('princess');
    });

    test('Player Join Functionality', () => {
      const state = runner.makeGameState('test-room');
      const [status, role] = runner.joinGame(state, 'player1', 'princess');
      
      expect(status).toBe('success');
      expect(role).toBe('princess');
    });

    test('Two Players Join', () => {
      const state = runner.makeGameState('test-room');
      const [status1, role1, newState] = runner.joinGame(state, 'player1', 'princess');
      const [status2, role2] = runner.joinGame(newState, 'player2', 'helper');
      
      expect(status1).toBe('success');
      expect(role1).toBe('princess');
      expect(status2).toBe('success');
      expect(role2).toBe('helper');
    });

    test('Game Full Rejection', () => {
      const state = runner.makeGameState('test-room');
      const [, , state1] = runner.joinGame(state, 'player1', 'princess');
      const [, , state2] = runner.joinGame(state1, 'player2', 'helper');
      const [status3] = runner.joinGame(state2, 'player3', 'princess');
      
      expect(status3).toBe('error');
    });
  });

  describe('Character Movement', () => {
    test('Character Movement', () => {
      const state = runner.makeGameState('test-room');
      const [, , gameState] = runner.joinGame(state, 'player1', 'princess');
      const [, , fullState] = runner.joinGame(gameState, 'player2', 'helper');
      
      const [status, , newState] = runner.makeChoice(fullState, 'player1', 'move_to_throne_room', 'princess');
      
      expect(status).toBe('success');
      expect(runner.getCharacterLocation(newState, 'princess')).toBe('throne_room');
    });

    test('Invalid Movement Rejected', () => {
      const state = runner.makeGameState('test-room');
      const [, , gameState] = runner.joinGame(state, 'player1', 'princess');
      const [, , fullState] = runner.joinGame(gameState, 'player2', 'helper');
      
      const [status] = runner.makeChoice(fullState, 'player1', 'move_to_invalid_location', 'princess');
      
      expect(status).toBe('error');
    });
  });

  describe('Outfit System', () => {
    test('Outfit Swap When Together', () => {
      const state = runner.makeGameState('test-room');
      const [, , gameState] = runner.joinGame(state, 'player1', 'princess');
      const [, , fullState] = runner.joinGame(gameState, 'player2', 'helper');
      
      const initialPrincessOutfit = runner.getCharacterOutfit(fullState, 'princess');
      const initialHelperOutfit = runner.getCharacterOutfit(fullState, 'helper');
      
      const [status, , newState] = runner.makeChoice(fullState, 'player1', 'swap_outfits', 'princess');
      
      expect(status).toBe('success');
      expect(runner.getCharacterOutfit(newState, 'princess')).toBe(initialHelperOutfit);
      expect(runner.getCharacterOutfit(newState, 'helper')).toBe(initialPrincessOutfit);
    });

    test('Initial Outfits Correct', () => {
      const state = runner.makeGameState('test-room');
      const [, , gameState] = runner.joinGame(state, 'player1', 'princess');
      const [, , fullState] = runner.joinGame(gameState, 'player2', 'helper');
      
      expect(runner.getCharacterOutfit(fullState, 'princess')).toBe('princess_dress');
      expect(runner.getCharacterOutfit(fullState, 'helper')).toBe('common_dress');
    });
  });

  describe('Basic Actions', () => {
    test('Explore Action', () => {
      const state = runner.makeGameState('test-room');
      const [, , gameState] = runner.joinGame(state, 'player1', 'princess');
      const [, , fullState] = runner.joinGame(gameState, 'player2', 'helper');
      
      const [status] = runner.makeChoice(fullState, 'player1', 'explore', 'princess');
      
      expect(status).toBe('success');
    });

    test('Rest Action', () => {
      const state = runner.makeGameState('test-room');
      const [, , gameState] = runner.joinGame(state, 'player1', 'princess');
      const [, , fullState] = runner.joinGame(gameState, 'player2', 'helper');
      
      const [status] = runner.makeChoice(fullState, 'player2', 'rest', 'helper');
      
      expect(status).toBe('success');
    });

    test('Invalid Action Rejected', () => {
      const state = runner.makeGameState('test-room');
      const [, , gameState] = runner.joinGame(state, 'player1', 'princess');
      const [, , fullState] = runner.joinGame(gameState, 'player2', 'helper');
      
      const [status] = runner.makeChoice(fullState, 'player1', 'invalid_action', 'princess');
      
      expect(status).toBe('error');
    });
  });

  describe('Integration Tests', () => {
    test('Complete Game Flow', () => {
      let state = runner.makeGameState('integration-test');
      
      // Join players
      let [status1, , newState] = runner.joinGame(state, 'player1', 'princess');
      expect(status1).toBe('success');
      
      let [status2, , fullState] = runner.joinGame(newState, 'player2', 'helper');
      expect(status2).toBe('success');
      
      // Move characters
      let [moveStatus1, , movedState] = runner.makeChoice(fullState, 'player1', 'move_to_library', 'princess');
      expect(moveStatus1).toBe('success');
      
      let [moveStatus2, , reuniteState] = runner.makeChoice(movedState, 'player2', 'move_to_library', 'helper');
      expect(moveStatus2).toBe('success');
      
      // Check they're together
      expect(runner.getCharacterLocation(reuniteState, 'princess')).toBe('library');
      expect(runner.getCharacterLocation(reuniteState, 'helper')).toBe('library');
      
      // Swap outfits
      let [swapStatus, , finalState] = runner.makeChoice(reuniteState, 'player1', 'swap_outfits', 'princess');
      expect(swapStatus).toBe('success');
      
      // Verify final state
      expect(runner.getCharacterOutfit(finalState, 'princess')).toBe('common_dress');
      expect(runner.getCharacterOutfit(finalState, 'helper')).toBe('princess_dress');
    });
  });

  describe('Full Test Suite Execution', () => {
    test('Run All Tests Via Runner', () => {
      const exitCode = runner.runAllTests();
      expect(exitCode).toBe(0); // Success
    }, 30000);

    test('Run Quick Tests Via Runner', () => {
      const exitCode = runner.runQuickTests();
      expect(exitCode).toBe(0); // Success
    });
  });
});