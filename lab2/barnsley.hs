import System.Random (randomR, mkStdGen, RandomGen, StdGen)


type Point = (Double, Double)

-- Преобразования для фрактала
transformation1 :: Point -> Point
transformation1 (_, y) = (0, 0.16 * y)

transformation2 :: Point -> Point
transformation2 (x, y) = (0.85 * x + 0.04 * y, -0.04 * x + 0.85 * y + 1.6)

transformation3 :: Point -> Point
transformation3 (x, y) = (0.2 * x - 0.26 * y, 0.23 * x + 0.22 * y + 1.6)

transformation4 :: Point -> Point
transformation4 (x, y) = (-0.15 * x + 0.28 * y, 0.26 * x + 0.24 * y + 0.44)

-- Применение случайного преобразования
chooseTransformation :: Point -> Double -> Point
chooseTransformation point randValue
  | randValue < 0.01 = transformation1 point
  | randValue < 0.86 = transformation2 point
  | randValue < 0.93 = transformation3 point
  | otherwise = transformation4 point

-- Генерация новой точки фрактала, с использованием генератора случайных чисел
generateNextPoint :: RandomGen g => Point -> g -> (Point, g)
generateNextPoint point gen = 
  let (randValue, newGen) = randomR (0.0, 1.0) gen  -- генерируем случайное число
  in (chooseTransformation point randValue, newGen)

-- Рекурсивное построение фрактала
buildFern :: RandomGen g => Point -> Int -> g -> [Point]
buildFern _ 0 _ = []
buildFern currentPoint steps gen =
  let (nextPoint, newGen) = generateNextPoint currentPoint gen
      remainingPoints = buildFern nextPoint (steps - 1) newGen
  in currentPoint : remainingPoints

-- Главная программа
main :: IO ()
main = do
  putStrLn "Введите количество шагов для генерации фрактала:"
  input <- getLine
  let steps = read input :: Int
  let initialGen = mkStdGen 42  -- Используем начальное зерно для генератора случайных чисел
  let fernPoints = buildFern (0, 0) steps initialGen
  print fernPoints
