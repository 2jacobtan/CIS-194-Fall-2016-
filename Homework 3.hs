{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

-- Lists

data List a = Empty | Entry a (List a)
    
mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

combine :: List Picture -> Picture
combine Empty = blank
combine (Entry p ps) = p & combine ps

appendList :: List a -> List a -> List a
appendList first Empty = first
appendList Empty second = second
appendList (Entry b bs) second = Entry b (appendList bs second)

-- Coordinates

data Coord = C Integer Integer

data Direction = R | U | L | D | N

eqCoord :: Coord -> Coord -> Bool
eqCoord (C a b) (C x y)
  | a == x && b == y = True
  | otherwise        = False

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

moveFromTo :: Coord -> Coord -> Coord -> Coord
moveFromTo from to original
  | eqCoord from original = to
  | otherwise = original

-- The maze

data Tile = Wall | Ground | Storage | Box | Blank deriving Eq
       
maze :: Coord -> Tile 
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

noBoxMaze :: Coord -> Tile
noBoxMaze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | otherwise                = Ground

mazeWithBoxes :: List Coord -> (Coord -> Tile)
mazeWithBoxes Empty c = noBoxMaze c
mazeWithBoxes cList c
  | inList cList c = Box
  | otherwise = noBoxMaze c
  where
    inList :: List Coord -> Coord -> Bool
    inList Empty _ = False
    inList (Entry a as) c
      | eqCoord a c = True
      | otherwise = inList as c


-- The state

data State = State Coord Direction (List Coord) -- FIXME! -- done

{- 
-- 0 version 1
initialBoxes :: List Coord
initialBoxes = collect21times (\r -> collect21times (\c -> screen (C r c)))
  where
    screen :: Coord -> List Coord
    screen c = case maze c of
      Box -> Entry c Empty
      _ -> Empty

collect21times :: (Integer -> List a) -> List a
collect21times mapping = go (-10)
  where
    go 11 = Empty
    go n = mapping n `appendList` go (n+1)
 -}

-- 0 version 2
initialBoxes :: List Coord
initialBoxes = go (-10) (-10) Empty
  where
    go :: Integer -> Integer -> List Coord -> List Coord
    go 11 _ aList = aList
    go r 11 aList = go (r+1) (-10) aList
    go r c aList = case maze (C r c) of
      Box -> go r (c+1) (Entry (C r c) aList)
      _ -> go r (c+1) aList


initialState :: State
initialState = State (C 1 1) N initialBoxes -- FIXME! -- done

-- Event handling

handleEvent :: Event -> State -> State
handleEvent _ s
  | isWon s = s
handleEvent (KeyPress key) (State from dir boxList) -- FIXME! -- done
  | key == "Right" = validatedMove (State from R boxList)
  | key == "Up"    = validatedMove (State from U boxList)
  | key == "Left"  = validatedMove (State from L boxList)
  | key == "Down"  = validatedMove (State from D boxList)
handleEvent _ s     = s

validatedMove :: State -> State
validatedMove (State from dir boxList)
  | maze2 to == Ground || maze2 to == Storage = State to dir boxList
  | maze2 to == Box && (maze2 to2 == Ground || maze2 to2 == Storage) = boxMove (State from dir boxList)
  | otherwise = State from dir boxList
  where
    maze2 = mazeWithBoxes boxList
    to = adjacentCoord dir from
    to2 = adjacentCoord dir to
    moveBox :: Coord -> Coord
    moveBox = moveFromTo to to2
    boxMove :: State -> State
    boxMove (State from dir boxList)= (State to dir (mapList moveBox boxList))

-- Drawing

wall, ground, storage, box :: Picture
wall =    colored grey (solidRectangle 1 1)
ground =  colored yellow     (solidRectangle 1 1)
storage = colored white (solidCircle 0.3) & ground
box =     colored brown      (solidRectangle 1 1)

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

drawTileAt :: Coord -> Picture
drawTileAt c = atCoord c (drawTile (noBoxMaze c))

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
  where
    go :: Integer -> Picture
    go 11 = blank
    go n  = something n & go (n+1)

drawState :: State -> Picture
drawState (State c d boxList) = stateOverlay (State c d boxList) & pictureOfPlayer c d & pictureOfBoxes boxList & pictureOfMaze

pictureOfMaze :: Picture
pictureOfMaze = draw21times (\r -> draw21times (\c -> drawTileAt (C r c)))

pictureOfBoxes :: List Coord -> Picture
pictureOfBoxes cs = combine (mapList (\c -> atCoord c (drawTile Box)) cs)

stateOverlay :: State -> Picture
stateOverlay s
  | isWon s = scaled 3 3 (text "You won!")
  | otherwise = blank

-- player (custom)
player :: Direction -> Picture
player R = colored blue (solidCircle 0.2) & colored green (polyline [(0,0),(1,0)])
player U = colored blue (solidCircle 0.2) & colored green (polyline [(0,0),(0,1)])
player D = colored blue (solidCircle 0.2) & colored green (polyline [(0,0),(0,(-1))])
player L = colored blue (solidCircle 0.2) & colored green (polyline [(0,0),((-1),0)])
player N = colored blue (solidCircle 0.2)

pictureOfPlayer :: Coord -> Direction -> Picture 
pictureOfPlayer c d = atCoord c (player d)

-- The complete interaction

sokoban :: Interaction State
sokoban = Interaction initialState (\_ s -> s) handleEvent drawState

sokoban2 = undefined

-- The general interaction type

data Interaction world = Interaction
        world
        (Double -> world -> world)
        (Event -> world -> world)
        (world -> Picture)

runInteraction :: Interaction s -> IO ()
runInteraction (Interaction state0 step handle draw)
  = interactionOf state0 step handle draw

-- Resetable interactions

resetable :: Interaction s -> Interaction s
resetable (Interaction state0 step handle draw)
  = Interaction state0 step handle' draw
  where handle' (KeyPress key) _ | key == "Esc" = state0
        handle' e s = handle e s

-- Start screen

startScreen :: Picture
startScreen = scaled 3 3 (text "Sokoban!")

data SSState world = StartScreen | Running world

withStartScreen :: Interaction s  -> Interaction (SSState s)
withStartScreen (Interaction state0 step handle draw)
  = Interaction state0' step' handle' draw'
  where
    state0' = StartScreen
    
    step' _ StartScreen = StartScreen
    step' t (Running s) = Running (step t s)
    
    handle' (KeyPress key) StartScreen | key == " " = Running state0
    handle' _              StartScreen              = StartScreen
    handle' e              (Running s)              = Running (handle e s)
    
    draw' StartScreen = startScreen
    draw' (Running s) = draw s

-- Winning
isWon :: State -> Bool
isWon (State c dir boxList) = allList (mapList isOnStorage boxList)
  where
    isOnStorage :: Coord -> Bool
    isOnStorage c
      | maze c == Storage = True
      | otherwise = False
    allList :: List Bool -> Bool
    allList Empty = True
    allList (Entry b bs)
      | b == False = False
      | otherwise = allList bs

-- The main function

main :: IO ()
--main = runInteraction sokoban
--main = drawingOf (pictureOfBoxes initialBoxes)
--main = drawingOf (draw initialState)
--main = runInteraction sokoban --3.5
main = runInteraction (withStartScreen (resetable sokoban))