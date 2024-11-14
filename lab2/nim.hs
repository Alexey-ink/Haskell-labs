import Data.List (intercalate)
import Data.Bits (xor)
import Control.Monad (when)

type GameState = [Int]  --  список камней в кучах
type MoveHistory = [(String, Int, Int)]  -- (игрок, номер кучи, количество камней)

-- Функция для инициализации начального состояния игры
initializeGame :: IO GameState
initializeGame = do
    putStrLn "Введите количество куч:"
    nPiles <- read <$> getLine
    putStrLn "Введите количество камней в каждой куче (через пробел):"
    stones <- map read . words <$> getLine
    if length stones == nPiles
       then return stones
       else do
           putStrLn "Ошибка: количество куч не совпадает с введенными значениями."
           initializeGame


isGameOver :: GameState -> Bool
isGameOver = all (== 0)

-- Функция для печати текущего состояния кучек
printGameState :: GameState -> IO ()
printGameState piles = putStrLn $ "Кучки: " ++ intercalate ", " (zipWith (\i stones -> show i ++ ": " ++ show stones) [1..] piles)

-- Функция для хода игрока
playerMove :: GameState -> IO (Int, Int)
playerMove piles = do
    printGameState piles
    putStrLn "Выберите номер кучи (начиная с 1):"
    pile <- read <$> getLine
    putStrLn "Сколько камней вы хотите взять?"
    stones <- read <$> getLine
    if pile > 0 && pile <= length piles && stones > 0 && stones <= piles !! (pile - 1)
       then return (pile - 1, stones)
       else do
           putStrLn "Неверный ход. Попробуйте снова."
           playerMove piles

-- Функция для хода компьютера с использованием стратегии Нэша
computerMove :: GameState -> IO (Int, Int)
computerMove piles = do
    let nimSum = foldr xor 0 piles  -- Вычисляем Nim-сумму
    let moves = [(i, 1) | (i, s) <- zip [0..] piles, s > 0]
    let winningMoves = [(i, s - (s `xor` nimSum)) | (i, s) <- zip [0..] piles, s `xor` nimSum < s]
    
    let move = if nimSum == 0 
               then case moves of 
                        (x:_) -> x -- Выбираем первый элемент из списка
                        []    -> error "Нет возможных ходов" 
               else case winningMoves of 
                        (x:_) -> x
                        []    -> error "Нет выигрышных ходов" 

    let (pile, stones) = move
    putStrLn $ "Компьютер выбрал кучу " ++ show (pile + 1) ++ " и взял " ++ show stones ++ " камней."
    return move

-- Функция для обновления состояния игры
makeMove :: GameState -> (Int, Int) -> GameState
makeMove piles (pile, stones) = take pile piles ++ [piles !! pile - stones] ++ drop (pile + 1) piles

-- Функция для вывода истории ходов
printMoveHistory :: MoveHistory -> IO ()
printMoveHistory moves = do
    putStrLn "Ходы в игре:"
    mapM_ (\(player, pile, stones) -> putStrLn $ player ++ " взяли " ++ show stones ++ " камней из кучи " ++ show (pile + 1)) moves

-- Основная логика игры
playNim :: GameState -> MoveHistory -> IO ()
playNim piles moves
  | isGameOver piles = do
      putStrLn "Игра окончена!"
      printMoveHistory moves
  | otherwise = do
      (pile, stones) <- playerMove piles
      let newPiles = makeMove piles (pile, stones)
      let newMoves = moves ++ [("Вы", pile, stones)]
      if isGameOver newPiles
         then do
           putStrLn "Поздравляем, вы выиграли!"
           printMoveHistory newMoves
         else do
           (pileComp, stonesComp) <- computerMove newPiles
           let finalPiles = makeMove newPiles (pileComp, stonesComp)
           let finalMoves = newMoves ++ [("Компьютер", pileComp, stonesComp)]
           if isGameOver finalPiles
              then do
                putStrLn "Компьютер выиграл!!!!!!!"
                printMoveHistory finalMoves
              else playNim finalPiles finalMoves

-- Главная функция
main :: IO ()
main = do
    putStrLn "Добро пожаловать в игру Ним!"
    initialPiles <- initializeGame
    playNim initialPiles []