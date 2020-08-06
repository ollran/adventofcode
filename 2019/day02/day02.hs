module Main where

-- Day 2: 1202 Program Alarm
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (catMaybes, fromJust)
import System.IO (IOMode(ReadMode), hClose, hGetContents, openFile)
import Text.Read (readMaybe)

-- Part 1
type IntCodes = IntMap (Int)

split :: (Char -> Bool) -> String -> [String]
split p s =
  case dropWhile p s of
    "" -> []
    s' -> w : split p s''
  where
    (w, s'') = break p s'

getOperation :: Int -> IntCodes -> Maybe (Int -> Int -> Int)
getOperation index codes =
  case IntMap.lookup index codes of
    1 -> Just (+)
    2 -> Just (*)
    _ -> Nothing

getInputValues :: Int -> IntCodes -> Maybe (Int, Int)
getInputValues index codes = do
  firstInputIndex <- IntMap.lookup (index + 1) codes
  secondInputIndex <- IntMap.lookup (index + 2) codes
  firstInput <- IntMap.lookup firstInputIndex codes
  secondInput <- IntMap.lookup secondInputIndex codes
  return (firstInput, secondInput)

getTargetIndex :: Int -> IntCodes -> Maybe Int
getTargetIndex index codes = IntMap.lookup (index + 3) codes

applyOperation :: Int -> IntCodes -> Maybe IntCodes
applyOperation index codes = do
  operation <- getOperation index codes
  (firstInput, secondInput) <- getInputValues index codes
  target <- getTargetIndex index codes
  return (IntMap.insert target (operation firstInput secondInput) codes)

processIntCodes :: IntCodes -> IntCodes
processIntCodes = f 0
  where
    f :: Int -> IntCodes -> IntCodes
    f index codes =
      case applyOperation index codes of
        Just newCodes -> f (index + 4) newCodes
        _ -> codes

setProgramToState :: Int -> Int -> IntCodes -> IntCodes
setProgramToState verb noun codes =
  IntMap.insert 1 verb . IntMap.insert 2 noun $ codes

part01 :: IntCodes -> Int
part01 = fromJust . IntMap.lookup 0 . processIntCodes . setProgramToState 12 2

-- Part 2
findProgramState :: Int -> IntCodes -> (Int, Int)
findProgramState output codes = f 0 0
  where
    f :: Int -> Int -> (Int, Int)
    f verb noun
      | noun > 99 = f (verb + 1) 0
      | verb > 99 = undefined
      | (fromJust . IntMap.lookup 0 $
         processIntCodes (setProgramToState verb noun codes)) ==
          output = (verb, noun)
      | otherwise = f verb (noun + 1)

part02 :: Int -> IntCodes -> Int
part02 =
  ((\(verb, noun) -> (read :: String -> Int) $ (show verb) ++ (show noun)) .) .
  findProgramState

showAnswer :: Int -> IO ()
showAnswer = putStrLn . show

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let input =
        IntMap.fromList .
        zip [0 ..] .
        catMaybes .
        map (readMaybe :: String -> Maybe Int) .
        concat . map (split (',' ==)) . lines $
        contents
  showAnswer . part01 $ input
  showAnswer $ part02 19690720 input
  hClose handle
