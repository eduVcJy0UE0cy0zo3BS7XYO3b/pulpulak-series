// Интерфейс для запуска Scheme тестов из Node.js
// game/test-runner.js

const fs = require('fs');
const path = require('path');
const BiwaScheme = require('biwascheme');

class SchemeTestRunner {
  constructor() {
    this.interpreter = new BiwaScheme.Interpreter();
    this.setupJavaScriptBridge();
  }

  setupJavaScriptBridge() {
    // Мост для логирования из Scheme в JavaScript
    this.interpreter.define('js-log', (message) => {
      console.log(message);
      return message;
    });

    // Мост для конвертации символов в строки
    this.interpreter.define('js-symbol->string', (sym) => {
      return sym.toString();
    });

    // Мост для конкатенации строк
    this.interpreter.define('js-string-append', (...strings) => {
      return strings.join('');
    });

    // Мост для конвертации чисел в строки
    this.interpreter.define('js-number->string', (num) => {
      return num.toString();
    });

    // Мост для отображения (display)
    this.interpreter.define('display', (message) => {
      process.stdout.write(message.toString());
    });

    // Мост для новой строки
    this.interpreter.define('newline', () => {
      process.stdout.write('\n');
    });

    // Мост для проверки типов
    this.interpreter.define('string?', (obj) => {
      return typeof obj === 'string';
    });

    this.interpreter.define('symbol?', (obj) => {
      return obj && obj.constructor && obj.constructor.name === 'BiwaSymbol';
    });

    this.interpreter.define('number?', (obj) => {
      return typeof obj === 'number';
    });

    this.interpreter.define('boolean?', (obj) => {
      return typeof obj === 'boolean';
    });

    this.interpreter.define('list?', (obj) => {
      return Array.isArray(obj);
    });

    this.interpreter.define('null?', (obj) => {
      return obj === null || obj === undefined || (Array.isArray(obj) && obj.length === 0);
    });

    // Мост для создания hash-table (используем Map)
    this.interpreter.define('make-hash-table', () => {
      return new Map();
    });

    this.interpreter.define('hash-table-set!', (table, key, value) => {
      table.set(key, value);
      return value;
    });

    this.interpreter.define('hash-table-ref/default', (table, key, defaultValue) => {
      return table.has(key) ? table.get(key) : defaultValue;
    });

    // Мост для работы со временем
    this.interpreter.define('current-time', () => {
      return Date.now();
    });
  }

  loadSchemeFile(filePath) {
    try {
      const content = fs.readFileSync(filePath, 'utf8');
      this.interpreter.evaluate(content);
      return true;
    } catch (error) {
      console.error(`Ошибка загрузки файла ${filePath}:`, error.message);
      return false;
    }
  }

  loadTestFramework() {
    const frameworkPath = path.join(__dirname, 'test-framework.scm');
    return this.loadSchemeFile(frameworkPath);
  }

  loadGameLogic() {
    const gamePath = path.join(__dirname, 'pulpulak-game.scm');
    return this.loadSchemeFile(gamePath);
  }

  loadTestSuite(suiteName) {
    const testPath = path.join(__dirname, 'tests', `${suiteName}.test.scm`);
    return this.loadSchemeFile(testPath);
  }

  runSchemeFunction(functionName, ...args) {
    try {
      const result = this.interpreter.evaluate(`(${functionName}${args.length > 0 ? ' ' + args.map(arg => JSON.stringify(arg)).join(' ') : ''})`);
      return result;
    } catch (error) {
      console.error(`Ошибка выполнения функции ${functionName}:`, error.message);
      return null;
    }
  }

  async runAllTests() {
    console.log('🧪 Запуск тестов Scheme архитектуры...\n');

    // Загружаем все необходимые файлы
    console.log('📁 Загрузка компонентов...');
    
    if (!this.loadGameLogic()) {
      throw new Error('Не удалось загрузить игровую логику');
    }
    console.log('✅ Игровая логика загружена');

    if (!this.loadTestFramework()) {
      throw new Error('Не удалось загрузить тестовый фреймворк');
    }
    console.log('✅ Тестовый фреймворк загружен');

    // Загружаем тестовые наборы
    const testSuites = ['core-functions', 'game-logic', 'integration'];
    let allLoaded = true;

    for (const suite of testSuites) {
      if (!this.loadTestSuite(suite)) {
        console.error(`❌ Не удалось загрузить тесты: ${suite}`);
        allLoaded = false;
      } else {
        console.log(`✅ Тесты ${suite} загружены`);
      }
    }

    if (!allLoaded) {
      throw new Error('Не удалось загрузить все тестовые наборы');
    }

    // Загружаем главный файл запуска тестов
    const runTestsPath = path.join(__dirname, 'tests', 'run-tests.scm');
    if (!this.loadSchemeFile(runTestsPath)) {
      throw new Error('Не удалось загрузить файл запуска тестов');
    }
    console.log('✅ Система запуска тестов загружена\n');

    // Запускаем тесты
    console.log('🚀 Выполнение тестов...\n');
    
    try {
      const result = this.runSchemeFunction('run-all-tests');
      
      // Анализируем результаты для возврата правильного кода выхода
      if (result && Array.isArray(result)) {
        let totalFailed = 0;
        for (const suiteResult of result) {
          if (Array.isArray(suiteResult) && suiteResult.length >= 4) {
            totalFailed += suiteResult[3] || 0; // Количество неудачных тестов
          }
        }
        
        if (totalFailed > 0) {
          console.log(`\n❌ Тесты завершены с ошибками: ${totalFailed} неудачных`);
          process.exit(1);
        } else {
          console.log('\n✅ Все тесты прошли успешно!');
          process.exit(0);
        }
      } else {
        console.log('\n✅ Тесты выполнены');
        process.exit(0);
      }
    } catch (error) {
      console.error('\n❌ Ошибка при выполнении тестов:', error.message);
      process.exit(1);
    }
  }

  async runQuickTests() {
    console.log('⚡ Запуск быстрых тестов...\n');
    
    if (!this.loadGameLogic() || !this.loadTestFramework()) {
      throw new Error('Не удалось загрузить основные компоненты');
    }

    this.loadTestSuite('core-functions');
    
    const runTestsPath = path.join(__dirname, 'tests', 'run-tests.scm');
    this.loadSchemeFile(runTestsPath);

    try {
      this.runSchemeFunction('run-quick-tests');
      console.log('\n✅ Быстрые тесты завершены');
    } catch (error) {
      console.error('\n❌ Ошибка в быстрых тестах:', error.message);
      process.exit(1);
    }
  }

  async runSpecificTest(testType) {
    console.log(`🎯 Запуск тестов: ${testType}\n`);
    
    if (!this.loadGameLogic() || !this.loadTestFramework()) {
      throw new Error('Не удалось загрузить основные компоненты');
    }

    this.loadTestSuite(testType);
    
    const runTestsPath = path.join(__dirname, 'tests', 'run-tests.scm');
    this.loadSchemeFile(runTestsPath);

    try {
      let functionName;
      switch (testType) {
        case 'core-functions':
          functionName = 'run-all-core-tests';
          break;
        case 'game-logic':
          functionName = 'run-all-game-logic-tests';
          break;
        case 'integration':
          functionName = 'run-all-integration-tests';
          break;
        default:
          throw new Error(`Неизвестный тип тестов: ${testType}`);
      }
      
      this.runSchemeFunction(functionName);
      console.log(`\n✅ Тесты ${testType} завершены`);
    } catch (error) {
      console.error(`\n❌ Ошибка в тестах ${testType}:`, error.message);
      process.exit(1);
    }
  }
}

// Функция для использования в Jest или прямого запуска
async function runSchemeTests(testType = 'all') {
  const runner = new SchemeTestRunner();
  
  try {
    switch (testType) {
      case 'quick':
        await runner.runQuickTests();
        break;
      case 'core':
        await runner.runSpecificTest('core-functions');
        break;
      case 'logic':
        await runner.runSpecificTest('game-logic');
        break;
      case 'integration':
        await runner.runSpecificTest('integration');
        break;
      case 'all':
      default:
        await runner.runAllTests();
        break;
    }
  } catch (error) {
    console.error('💥 Критическая ошибка:', error.message);
    process.exit(1);
  }
}

// Экспорт для использования в тестах
module.exports = {
  SchemeTestRunner,
  runSchemeTests
};

// Прямой запуск если файл вызван напрямую
if (require.main === module) {
  const testType = process.argv[2] || 'all';
  runSchemeTests(testType);
}