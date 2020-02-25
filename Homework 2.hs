{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

main :: IO ()
--main = interactionOf initialCoord handleTime handleEvent drawState
main = exercise3

wall, ground, storage, box :: Picture
wall =    colored grey (solidRectangle 1 1)
ground =  colored yellow     (solidRectangle 1 1)
storage = solidCircle 0.4 & ground
box =     colored brown      (solidRectangle 1 1)

data Tile = Wall | Ground | Storage | Box | Blank deriving Eq

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

pictureOfMaze :: Picture
pictureOfMaze = draw21times (\r -> draw21times (\c -> drawTileAt (C c r)))

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
  where
    go :: Integer -> Picture
    go 11 = blank
    go n  = something n & go (n+1)

drawTileAt :: Coord -> Picture
drawTileAt (C x y) = translated (fromIntegral x) (fromIntegral y) (drawTile (maze (C x y) ))
         
maze :: Coord -> Tile 
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

data Direction = R | U | L | D | N

data Coord = C Integer Integer

data State = S Coord Direction

initialCoord :: Coord
initialCoord = C 0 0

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

handleTime :: Double -> Coord -> Coord
handleTime _ c = c

handleTime1 :: Double -> State -> State
handleTime1 _ s = s


handleEvent :: Event -> Coord -> Coord
handleEvent (KeyPress key) c
    | key == "Right" = adjacentCoord R c
    | key == "Up"    = adjacentCoord U c
    | key == "Left"  = adjacentCoord L c
    | key == "Down"  = adjacentCoord D c
    | otherwise      = c
handleEvent _ c      = c

handleEvent1 :: Event -> State -> State
handleEvent1 (KeyPress key) (S from dir)
    | key == "Right" = validatedMove from R
    | key == "Up"    = validatedMove from U
    | key == "Left"  = validatedMove from L
    | key == "Down"  = validatedMove from D
handleEvent1 _ s     = s


drawState :: Coord -> Picture
drawState c = atCoord c pictureOfMaze


-- Exercise 2
player :: Direction -> Picture
player R = colored blue (solidCircle 0.2) & colored green (polyline [(0,0),(1,0)])
player U = colored blue (solidCircle 0.2) & colored green (polyline [(0,0),(0,1)])
player D = colored blue (solidCircle 0.2) & colored green (polyline [(0,0),(0,(-1))])
player L = colored blue (solidCircle 0.2) & colored green (polyline [(0,0),((-1),0)])
player N = colored blue (solidCircle 0.2)

drawPlayer :: State -> Picture
drawPlayer (S c dir) = atCoord c (player dir)

validatedMove :: Coord -> Direction -> State
validatedMove from dir
  | maze to == Ground || maze to == Storage = S to dir
  | otherwise = S from dir
  where to = adjacentCoord dir from

drawState1 :: State -> Picture
drawState1 s = drawPlayer s & pictureOfMaze

exercise2 :: IO ()
exercise2 = interactionOf (S (C 1 1) N) handleTime1 handleEvent1 drawState1

-- Exercise 3
resetableInteractionOf ::
    world ->
    (Double -> world -> world) ->
    (Event -> world -> world) ->
    (world -> Picture) ->
    IO ()
resetableInteractionOf state0 step handle draw
  = interactionOf state0 step handle3 draw
  where
    handle3 (KeyPress key) s
      | key == "Esc" = state0
    handle3 e s = handle e s

exercise3 :: IO ()
exercise3 = resetableInteractionOf (S (C 1 1) N) handleTime1 handleEvent1 drawState1