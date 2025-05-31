module.exports = {
  testEnvironment: 'node',
  testMatch: ['**/__tests__/**/*.test.js'],
  collectCoverageFrom: [
    'game/**/*.js',
    '!game/**/*.test.js',
    '!game/test-runner.js' // Исключаем test runner из покрытия
  ],
  coverageDirectory: 'coverage',
  coverageReporters: ['text', 'lcov', 'html'],
  testTimeout: 60000, // Увеличиваем таймаут для Scheme тестов
  setupFilesAfterEnv: ['<rootDir>/test-setup.js']
};