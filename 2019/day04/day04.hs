module Main where

-- Day 4: Secure Container
-- Part 1
digits :: Int -> [Int]
digits 0 = [0]
digits n = f n
  where
    f 0 = []
    f m = f (div m 10) ++ [mod m 10]

containsEqualAdjacents :: Int -> [Int] -> Bool
containsEqualAdjacents numberOfDigits = f 0
  where
    d = numberOfDigits - 1
    f :: Int -> [Int] -> Bool
    f found [] = found >= d
    f found (_:[]) = found >= d
    f found (x:xs)
      | found >= d = True
      | x == (head xs) = f (found + 1) xs
      | otherwise = f found xs

isIncreasing :: [Int] -> Bool
isIncreasing [] = True
isIncreasing (_:[]) = True
isIncreasing (x:xs) =
  if head xs >= x
    then isIncreasing xs
    else False

calculateCodes :: Int -> Int -> [Int]
calculateCodes start end =
  [ x
  | x <- [start .. end]
  , let d = digits x
  , containsEqualAdjacents 2 d
  , isIncreasing d
  ]

part01 :: [Int] -> Int
part01 = length

-- Part 2
containsDouble :: [Int] -> Bool
containsDouble [] = False
containsDouble (_:[]) = False
containsDouble (a:(b:[])) = a == b
containsDouble (a:(b:(c:[]))) = (a == b && b /= c) || (a /= b && b == c)
containsDouble (a:(b:(c:cs))) =
  if a == b && b /= c
    then True
    else f (a : (b : (c : cs)))
  where
    f :: [Int] -> Bool
    f [] = False
    f (_:[]) = False
    f (x:(y:[])) = x == y
    f (x:(y:(z:[]))) = x /= y && y == z
    f (x:(y:(z:zs)))
      | x /= y && y == z && z /= (head zs) = True
      | otherwise = f (y : (z : zs))

part02 :: [Int] -> Int
part02 = length . filter (containsDouble . digits)

showAnswer :: ([Int] -> Int) -> [Int] -> IO ()
showAnswer = ((putStrLn . show) .)

main :: IO ()
main = do
  let codes = calculateCodes 245318 765747
  showAnswer part01 $ codes
  showAnswer part02 $ codes
