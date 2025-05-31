#!/usr/bin/env node

// Массовое исправление тестов для работы с Immer
const fs = require('fs');
const path = require('path');

const testFiles = [
    'questSystem.test.js',
    'fullQuestFlow.test.js', 
    'dialogueBranching.test.js',
    'independentDialogues.test.js'
];

function fixTestFile(filePath) {
    console.log(`Fixing ${path.basename(filePath)}...`);
    
    let content = fs.readFileSync(filePath, 'utf8');
    
    // Заменяем const gameState на let gameState в тестах
    content = content.replace(
        /(\s+)(const gameState = gameLogic\.games\.get\(roomId\);)/g,
        '$1let gameState = gameLogic.games.get(roomId);'
    );
    
    // Добавляем импорт refreshGameState если его нет
    if (!content.includes('refreshGameState')) {
        content = content.replace(
            /(const CoopGameLogic = require\('\.\.\/coopGameLogic'\);)/,
            '$1\nconst { refreshGameState } = require(\'./testHelpers\');'
        );
    }
    
    // Добавляем refreshGameState после операций, которые изменяют состояние
    const operationsToFix = [
        { pattern: /(gameLogic\.processNPCInteraction\([^;]+;)/, replacement: '$1\n            gameState = refreshGameState(gameLogic, roomId);' },
        { pattern: /(gameLogic\.processNPCDialogueChoice\([^;]+;)/, replacement: '$1\n            gameState = refreshGameState(gameLogic, roomId);' },
        { pattern: /(gameLogic\.closeNPCDialogue\([^;]+;)/, replacement: '$1\n            gameState = refreshGameState(gameLogic, roomId);' },
        { pattern: /(gameLogic\.startQuest\([^;]+;)/, replacement: '$1\n            gameState = refreshGameState(gameLogic, roomId);' },
        { pattern: /(gameLogic\.updateQuestProgress\([^;]+;)/, replacement: '$1\n            gameState = refreshGameState(gameLogic, roomId);' }
    ];
    
    operationsToFix.forEach(({ pattern, replacement }) => {
        content = content.replace(pattern, replacement);
    });
    
    // Исправляем специфичные проблемы с тестами квестов
    content = content.replace(
        /(const result = gameLogic\.startQuest\(gameState, [^;]+;\s+)([^}]+expect\(gameState\.)/g,
        '$1\n            // Получаем обновленное состояние\n            const updatedGameState = gameLogic.games.get(roomId);\n            $2expect(updatedGameState.'
    );
    
    // Заменяем прямые обращения к gameState в ожиданиях после startQuest на updatedGameState
    content = content.replace(
        /(const updatedGameState = gameLogic\.games\.get\(roomId\);\s+[^}]*?)gameState\.(quests\.[^.]+\.active)/g,
        '$1updatedGameState.$2'
    );
    
    fs.writeFileSync(filePath, content);
    console.log(`✅ Fixed ${path.basename(filePath)}`);
}

// Исправляем все файлы
testFiles.forEach(filename => {
    const filePath = path.join(__dirname, filename);
    if (fs.existsSync(filePath)) {
        fixTestFile(filePath);
    } else {
        console.log(`❌ File not found: ${filename}`);
    }
});

console.log('\n🎉 All test files have been processed!');