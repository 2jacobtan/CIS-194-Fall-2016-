{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-unused-binds #-}
import CodeWorld

main :: IO ()
main = exercise3

-- Fill in the blanks! (When I say blanks, I mean undefineds)

-- Exercise 1

botCircle, topCircle, midCircle :: Color -> Picture
botCircle c = colored c (translated 0 (-3) (solidCircle 1))
topCircle c = colored c (translated 0   3  (solidCircle 1))
midCircle c = colored c (translated 0   0  (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 8.5

trafficLight :: Int -> Picture
trafficLight 2 = botCircle green & topCircle black & frame & midCircle black
trafficLight 1 = botCircle black & topCircle black & frame & midCircle yellow
trafficLight 0 = botCircle black & topCircle red   & frame & midCircle black
trafficLight 3 = botCircle black & topCircle red   & frame & midCircle yellow

trafficController :: Double -> Picture
trafficController t
  | floor (t/1) `mod` 6 `elem` [0,1] = trafficLight 2
  | floor (t/1) `mod` 6   ==    2    = trafficLight 1
  | floor (t/1) `mod` 6 `elem` [3,4] = trafficLight 0
  | otherwise                        = trafficLight 3

trafficLightAnimation :: Double -> Picture
trafficLightAnimation = trafficController

exercise1 :: IO ()
exercise1 = animationOf trafficLightAnimation

-- Exercise 2

blossom :: Double -> Picture
blossom t
  | t > 10    = colored yellow (solidCircle 0.3)
  | otherwise = colored yellow (solidCircle (0.3 * t / 5))

tree :: Integer -> Double -> Picture
tree 0 t = blossom t
tree n t = path [(0,0),(0,1)] & translated 0 1 (
  rotated (pi/10) (tree (n-1) t) & rotated (- pi/10) (tree (n-1) t))
  
exercise2 :: IO ()
exercise2 = animationOf (tree 8 . id)

-- Exercise 3

wall, ground, storage, box :: Picture
wall =    colored gray (solidRectangle 1 1)
ground =  colored yellow (solidRectangle 1 1)
storage = colored black (solidCircle 0.3 ) & colored yellow (solidRectangle 1 1)
box =     colored brown (solidRectangle 1 1)

drawTile :: Integer -> Picture
drawTile i
  | i == 0 = blank
  | i == 1 = wall
  | i == 2 = ground
  | i == 3 = storage
  | i == 4 = box

drawRows :: Int -> Int -> Picture
drawRows start end
  | start > end+1 = colored red (solidCircle 3)
  | start == end+1 = blank
  | otherwise = drawCols start (-4) 4 & drawRows (start+1) end

drawCols :: Int -> Int -> Int -> Picture
drawCols row start end
  | start > end+1 = colored orange (solidCircle 3)
  | start == end+1 = blank
  | otherwise = translated (fromIntegral start) (fromIntegral row) (drawItem row start) & drawCols row (start+1) end

drawItem :: Int -> Int -> Picture
drawItem y x = drawTile (maze (toInteger x) (toInteger y) )

pictureOfMaze :: Picture
pictureOfMaze = (drawRows (-4) 4)

exercise3 :: IO ()
exercise3 = drawingOf pictureOfMaze
         
maze :: Integer -> Integer -> Integer 
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2
 