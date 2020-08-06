import Data.Maybe (catMaybes)
import System.IO (IOMode(ReadMode), hClose, hGetContents, openFile)
import Text.Read (readMaybe)

-- Day 1: The Tyranny of the Rocket Equation
-- Part 1
calculateFuel :: Int -> Int
calculateFuel = (flip (-) 2) . (flip div 3)

part01 :: [Int] -> Int
part01 = sum . map calculateFuel

-- Part 2
calculateTotalFuel :: Int -> Int
calculateTotalFuel mass = f (calculateFuel mass) 0
  where
    f :: Int -> Int -> Int
    f currentMass fuel =
      if currentMass <= 0
        then fuel
        else f (calculateFuel currentMass) (fuel + currentMass)

part02 :: [Int] -> Int
part02 = sum . map calculateTotalFuel

showAnswer :: Int -> IO ()
showAnswer = putStrLn . show

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let input =
        catMaybes . map (readMaybe :: String -> Maybe Int) . lines $ contents
  showAnswer . part01 $ input
  showAnswer . part02 $ input
  hClose handle
