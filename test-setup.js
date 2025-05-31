// Настройка для Jest тестов
// test-setup.js

// Увеличиваем таймауты для Scheme тестов
jest.setTimeout(60000);

// Подавляем некоторые предупреждения BiwaScheme в тестах
const originalConsoleWarn = console.warn;
console.warn = (message) => {
  // Подавляем предупреждения BiwaScheme которые не критичны для тестов
  if (typeof message === 'string' && 
      (message.includes('BiwaScheme') || 
       message.includes('deprecation') ||
       message.includes('experimental'))) {
    return;
  }
  originalConsoleWarn(message);
};

// Глобальная настройка для обработки ошибок в Scheme тестах
process.on('unhandledRejection', (reason, promise) => {
  console.error('Unhandled Rejection at:', promise, 'reason:', reason);
});

// Настройка окружения для тестов
global.testConfig = {
  schemeTimeout: 30000,
  verboseLogging: process.env.NODE_ENV === 'test' && process.env.VERBOSE === 'true'
};