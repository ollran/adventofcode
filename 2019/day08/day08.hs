module Main where

-- Day 8: Space Image Format
import Data.Char (digitToInt)
import Data.List (intercalate, sortBy)
import System.IO (IOMode(ReadMode), hClose, hGetContents, openFile)

-- Part 1
type Image = [Int]

type Layer = [[Int]]

type Layers = [Layer]

data Color
  = Black
  | White
  | Transparent
  deriving (Show)

type DecodedImageLayer = [Color]

type DecodedImage = [[Color]]

type ZippedImage = [[(Color, Int)]]

imageWidth :: Int
imageWidth = 25

imageHeight :: Int
imageHeight = 6

imageArea :: Int
imageArea = imageWidth * imageHeight

split :: (Char -> Bool) -> String -> [String]
split p s =
  case dropWhile p s of
    "" -> []
    s' -> w : split p s''
      where (w, s'') = break p s'

imageToLayer :: Image -> Layer
imageToLayer = f []
  where
    f :: Layer -> Image -> Layer
    f layer _
      | length layer >= imageHeight = layer
    f layer image = f (layer ++ [take imageWidth image]) (drop imageWidth image)

imageToLayers :: Image -> Layers
imageToLayers = f []
  where
    f :: Layers -> Image -> Layers
    f layers image
      | length image < imageArea = layers
    f layers image =
      f
        (layers ++ [imageToLayer . take imageArea $ image])
        (drop imageArea image)

countDigits :: Int -> [Int] -> Int
countDigits = (length .) . filter . (==)

countLayerDigits :: Int -> Layer -> Int
countLayerDigits digit layer = sum . map (countDigits digit) $ layer

findLayerWithFewestDigits :: Int -> Layers -> Layer
findLayerWithFewestDigits digit layers =
  head .
  map snd .
  sortBy (\(a, _) (b, _) -> compare a b) . map ((,) =<< countLayerDigits digit) $
  layers

part01 :: Layers -> IO ()
part01 layers =
  let targetLayer = findLayerWithFewestDigits 0 layers
   in putStrLn . show $
      (countLayerDigits 1 targetLayer) * (countLayerDigits 2 targetLayer)

-- Part 2
initialDecodedImage :: DecodedImage
initialDecodedImage = replicate imageHeight $ replicate imageWidth Transparent

applyValueToColor :: (Color, Int) -> Color
applyValueToColor (color, value) =
  case (color, value) of
    (Black, _) -> Black
    (White, _) -> White
    (_, 0) -> Black
    (_, 1) -> White
    (c, _) -> c

zipImageAndLayer :: DecodedImage -> Layer -> ZippedImage
zipImageAndLayer decodedImage layer = map (uncurry zip) $ zip decodedImage layer

applyLayer :: DecodedImage -> Layer -> DecodedImage
applyLayer decodedImage layer =
  map (map applyValueToColor) $ zipImageAndLayer decodedImage layer

decodeLayers :: Layers -> DecodedImage
decodeLayers = f initialDecodedImage
  where
    f :: DecodedImage -> Layers -> DecodedImage
    f decodedImage [] = decodedImage
    f decodedImage (x:xs) = f (applyLayer decodedImage x) xs

renderColor :: Color -> Char
renderColor Black = 'X'
renderColor White = ' '
renderColor _ = '_'

renderImage :: DecodedImage -> String
renderImage decodedImage = intercalate "\n" $ map (f []) decodedImage
  where
    f :: String -> DecodedImageLayer -> String
    f renderedImage [] = renderedImage
    f renderedImage (x:xs) = f (renderedImage ++ [renderColor x]) xs

part02 :: Layers -> IO ()
part02 = putStrLn . renderImage . decodeLayers

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let input = imageToLayers . map digitToInt . head . lines $ contents
  part01 input
  part02 input
  hClose handle
