module Main where


import TheLens ()

-- Вывести на экран список list
-- в cols столбцов (например,
-- после каждого числа вывести
-- знак табуляции) и вернуть
-- сумму элементов списка
--
-- > sumAndTabulate 3 [1..10]
-- >
-- >    1       2       3
-- >    4       5       6
-- >    7       8       9
-- >    10
-- >
-- > 55

sumAndTabulate :: Int -> [Int] -> IO Int
sumAndTabulate cols list =
  helper 0 cols 0 list
  where
    helper :: Int -> Int -> Int -> [Int] -> IO Int
    helper sum cols curCol list
      | cols == curCol = do
          putStrLn ""
          helper sum cols 0 list
      | null list = do
          putStrLn ""
          return sum
      | otherwise = do
          putStr (show x ++ "\t")
          helper (sum + x) cols (curCol + 1) xs
      where (x,xs) = (head list, tail list)

main :: IO Int
main = sumAndTabulate 3 [1..10]
