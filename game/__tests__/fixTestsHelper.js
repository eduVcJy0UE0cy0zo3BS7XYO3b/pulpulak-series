// Временный скрипт для автоматического исправления тестов после миграции на Immer
const fs = require('fs');
const path = require('path');

function fixTestFile(filePath) {
    let content = fs.readFileSync(filePath, 'utf8');
    
    // Заменяем const gameState на let gameState
    content = content.replace(/const gameState = gameLogic\.games\.get\(roomId\);/g, 
                             'let gameState = gameLogic.games.get(roomId);');
    
    // Добавляем импорт refreshGameState если его нет
    if (!content.includes('refreshGameState')) {
        content = content.replace(
            /const CoopGameLogic = require\('\.\.\/coopGameLogic'\);/,
            "const CoopGameLogic = require('../coopGameLogic');\nconst { refreshGameState } = require('./testHelpers');"
        );
    }
    
    // Добавляем refreshGameState после операций изменения состояния
    const operationsToFix = [
        'gameLogic.processNPCInteraction',
        'gameLogic.processNPCDialogueChoice', 
        'gameLogic.closeNPCDialogue',
        'gameLogic.processMovement',
        'gameLogic.makeChoice'
    ];
    
    operationsToFix.forEach(operation => {
        const regex = new RegExp(`(${operation}\\([^;]+;)`, 'g');
        content = content.replace(regex, '$1\n            gameState = refreshGameState(gameLogic, roomId);');
    });
    
    fs.writeFileSync(filePath, content);
    console.log(`Fixed: ${path.basename(filePath)}`);
}

// Исправляем все тестовые файлы
const testDir = __dirname;
const testFiles = fs.readdirSync(testDir)
    .filter(file => file.endsWith('.test.js'))
    .map(file => path.join(testDir, file));

testFiles.forEach(fixTestFile);

console.log('All test files have been fixed!');