## Курсовая работа. Часть первая.

Первая часть курсовой работы представляет собой разработку синтаксического анализатора (парсера), разбирающего строки, прочитанные из текстового файла .txt. Файл должен содержать значения и бинарные операции. 

- **Значения:** целые числа в десятичной системе счисления.
- **Бинарные операции:** сложение, вычитание, умножение, деление.

Пример строки в файле: 234 * 43. Вычислить проанализированное выражение, вывести его и результат вычисления на экран. Пользователь вводит название файла. 

## Структура текущей директории:
```
├── app/           
│   ├── Main.hs           # Главная точка входа программы  
├── src/               
│   ├── Lib.hs            # Основная реализация программы
├── test/                 # Тесты для программы (отсутстуют)
│   ├── Spec.hs    
├── Report/               # Отчет по обеим частям курсовой работы  
├── coursework-p2.cabal   # Конфигурация Haskell-проекта  
├── Setup.hs              # Скрипт сборки проекта  
├── stack.yaml            # Конфигурация для Stack  
├── LICENSE               # Лицензия проекта  
├── README.md             # Документация (вы читаете её!) 
├── package.yaml          # Альтернативная конфигурация проекта 
└── example.txt           # Текст, который нужно распарсить и вычислить значения выражений
```