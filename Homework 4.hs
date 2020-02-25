{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

-- Lists

data List a = Empty | Entry a (List a) deriving (Eq,Show)
    
mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

combine :: List Picture -> Picture
combine Empty = blank
combine (Entry p ps) = p & combine ps

elemList :: Eq a => a -> List a -> Bool -- hw 4
elemList _ Empty = False
elemList x (Entry y ys)
  | x == y = True
  | otherwise = elemList x ys

appendList :: List a -> List a -> List a -- hw 4 + hw 3 ?
appendList first Empty = first
appendList Empty second = second
appendList (Entry b bs) second = Entry b (appendList bs second)

append :: List a -> a -> List a
append xs x = appendList xs (Entry x Empty)

listLength :: List a -> Integer -- hw 4
listLength Empty = 0
listLength (Entry _ xs) = 1 + listLength xs

filterList :: (a -> Bool) -> List a -> List a -- hw 4
filterList indicator xs = go xs
  where
    go Empty = Empty
    go (Entry y ys)
      | indicator y = Entry y (go ys)
      | otherwise = go ys

nth :: List a -> Integer -> a -- hw 4
nth (Entry x _) 1 = x
nth (Entry _ xs) n = nth xs (n-1)
nth Empty _ = error "list is too short"

i'thMaze :: Integer -> (Coord -> Tile) -- hw 4
i'thMaze i = maze
  where
    (Maze _ maze) = nth mazes i

-- Coordinates

data Coord = C Integer Integer
instance Eq Coord where
  C x1 y1 == C x2 y2 = x1 == x2 && y1 == y2

data Direction = R | U | L | D | N deriving Eq

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
{- -- deprecated since hw 4
maze :: Coord -> Tile 
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground
 -}
noBoxMaze :: Integer -> Coord -> Tile
noBoxMaze i c
  | tile == Box = Ground
  | otherwise = tile
  where
    tile = i'thMaze i c
{- -- deprecated since hw 4
noBoxMaze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | otherwise                = Ground
 -}
mazeWithBoxes :: Integer -> List Coord -> (Coord -> Tile)
mazeWithBoxes i Empty c = noBoxMaze i c
mazeWithBoxes i cList c
  | elemList c cList = Box
  | otherwise = noBoxMaze i c

-- The state

data State = State Integer Coord Direction (List Coord) deriving Eq

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

-- 0 version 2 -- augmented during hw 4 ex 4
initialBoxes :: Integer -> List Coord
initialBoxes i = go (-10) (-10) Empty
  where
    go :: Integer -> Integer -> List Coord -> List Coord
    go 11 _ aList = aList
    go r 11 aList = go (r+1) (-10) aList
    go r c aList = case maze (C r c) of
      Box -> go r (c+1) (Entry (C r c) aList)
      _ -> go r (c+1) aList
      where
        maze = i'thMaze i

initialState :: Integer -> State
initialState i = State i initialCoord N (initialBoxes i) -- hw 4 ex 4
  where
    initialCoord = go (nth mazes i)
      where go (Maze initialCoord maze) = initialCoord

-- Event handling

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) s
  | key == "C" = next s
  | isWon s && key == " " = next s
  | isWon s = s
  where
    next s
      | progress s == listLength mazes = s
      where
        progress (State i _ _ _) = i
    next (State i c d boxList)
      | otherwise = initialState (i+1)
handleEvent (KeyPress key) (State i from dir boxList)
  | key == "Right" = validatedMove (State i from R boxList)
  | key == "Up"    = validatedMove (State i from U boxList)
  | key == "Left"  = validatedMove (State i from L boxList)
  | key == "Down"  = validatedMove (State i from D boxList)
handleEvent _ s    = s

validatedMove :: State -> State
validatedMove (State i from dir boxList)
  | maze2 to == Ground || maze2 to == Storage = State i to dir boxList
  | maze2 to == Box && (maze2 to2 == Ground || maze2 to2 == Storage) = boxMove (State i from dir boxList)
  | otherwise = State i from dir boxList
  where
    maze2 = mazeWithBoxes i boxList
    to = adjacentCoord dir from
    to2 = adjacentCoord dir to
    moveBox :: Coord -> Coord
    moveBox = moveFromTo to to2
    boxMove :: State -> State
    boxMove (State i from dir boxList)= (State i to dir (mapList moveBox boxList))

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

drawTileAt :: Integer -> Coord -> Picture
drawTileAt i c = atCoord c (drawTile (noBoxMaze i c))

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
  where
    go :: Integer -> Picture
    go 11 = blank
    go n  = something n & go (n+1)

drawState :: State -> Picture
drawState (State i c d boxList) = stateOverlay (State i c d boxList) & pictureOfPlayer c d & pictureOfBoxes boxList & pictureOfMaze i

pictureOfMaze :: Integer -> Picture
pictureOfMaze i = draw21times (\r -> draw21times (\c -> drawTileAt i (C r c)))

pictureOfBoxes :: List Coord -> Picture
pictureOfBoxes cs = combine (mapList (\c -> atCoord c (drawTile Box)) cs)

stateOverlay :: State -> Picture
stateOverlay s = stateOverlayWin s & stateOverlayStage s

stateOverlayStage :: State -> Picture
stateOverlayStage (State i _ _ _) = translated (-5) 5 (lettering $ iText $ i)
  where
    iText 1 = "1"
    iText 2 = "2"
    iText 3 = "3"
    iText 4 = "4"
    iText 5 = "5"
    iText 6 = "6"
    iText 7 = "7"
    iText 8 = "8"
    iText 9 = "9"
    iText 10 = "10"
    iText 11 = "11"
    iText _ = ">11"

stateOverlayWin :: State -> Picture
stateOverlayWin s
  | isWon s && progress s == listLength mazes = scaled 3 3 (lettering "All done!")
  | isWon s = scaled 3 3 (lettering "You won!")
  | otherwise = blank
  where
    progress (State i _ _ _) = i

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
sokoban = Interaction (initialState 1) (\_ s -> s) handleEvent drawState

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

resetable :: Interaction (WithUndo State) -> Interaction (WithUndo State)
resetable (Interaction (WithUndo state0 pastStates) step handle draw)
  = Interaction (WithUndo state0 pastStates) step handle' draw
  where
    handle' (KeyPress key) (WithUndo (State i c dir boxes) pastStates)
      | key == "Esc" = WithUndo (initialState i) pastStates
    handle' e s = handle e s

-- Start screen

startScreen :: Picture
startScreen = scaled 0.7 0.7 (go lines 4)
  where
    go Empty _ = blank
    go (Entry l ls) n = translated 0 n (lettering l) & go ls (n-1)
    lines =
      Entry "Sokoban!" $
      Entry "" $
      -- Entry "Instructions:" $
      -- Entry "" $
      Entry "1. Press <spacebar> to begin. Arrows to move." $
      Entry "2. Push all boxes into white circles to win." $
      Entry "3. Press <u> to undo. <Escape> to reset the current stage." $
      Entry "4. When \"You win!\", press <spacebar> to move to next stage." $
      Entry "5. Press <c> (cheat) to skip to the next stage." $
      Empty

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

-- Undoable interactions
-- We need to remember the current state, and all past states:
data WithUndo a = WithUndo a (List a)

withUndo :: Eq a => Interaction a -> Interaction (WithUndo a)
withUndo (Interaction state0 step handle draw)
  = Interaction state0' step' handle' draw'
  where
    state0' = WithUndo state0 Empty
    
    step' t (WithUndo s stack) = WithUndo (step t s) stack
    
    handle' (KeyPress key) (WithUndo s stack) | key == "U"
      = case stack of Entry s' stack' -> WithUndo s' stack'
                      Empty           -> WithUndo s Empty
    handle' e              (WithUndo s stack)
       | s' == s = WithUndo s stack
       | otherwise = WithUndo s' (Entry s stack)
      where s' = handle e s
    
    draw' (WithUndo s _) = draw s


-- Winning
isWon :: State -> Bool
isWon (State i c dir boxList) = allList (mapList isOnStorage boxList)
  where
    isOnStorage :: Coord -> Bool
    isOnStorage c
      | i'thMaze i c == Storage = True
      | otherwise = False
    allList :: List Bool -> Bool
    allList Empty = True
    allList (Entry b bs)
      | b == False = False
      | otherwise = allList bs

-- hw 4 ex 2: graph search
isGraphClosed :: Eq a => a -> (a -> List a) -> (a -> Bool) -> Bool
isGraphClosed initial adjacent isOk = go Empty (adjacent initial)
  where
    go _ Empty = True
    go seen (Entry x xs)
      | x `elemList` seen = go seen xs
      | isOk x = go (append seen x) (appendList xs (adjacent x))
      | otherwise = False


adjacent :: Maze -> Coord -> List Coord
adjacent (Maze _ maze) c = filterList notWall cs
  where
    cs = mapList (\d -> adjacentCoord d c) allDirections
    notWall :: Coord -> Bool
    notWall c = case maze c of
      Wall -> False
      _ -> True

isOk :: Maze -> Coord -> Bool
isOk (Maze _ maze) c
  | maze c == Blank = False
  | otherwise = True

allDirections :: List Direction
allDirections =
  Entry R $ Entry U $ Entry L $ Entry D $ Empty

-- hw 4 ex 3: check closedness of mazes

isClosed :: Maze -> Bool
isClosed (Maze initial maze)
  | maze initial `elemList` (Entry Ground $ Entry Storage $ Empty)
    && isGraphClosed initial (adjacent (Maze initial maze)) (isOk (Maze initial maze))
    = True
  | otherwise = False

exercise3 :: IO ()
exercise3 = drawingOf (pictureOfBools (mapList isClosed extraMazes))

-- copy-pasta from hw 4 ex 3
pictureOfBools :: List Bool -> Picture
pictureOfBools xs = translated (-fromIntegral k /2) (fromIntegral k) (go 0 xs)
  where n = listLength xs
        k = findK 0 -- k is the integer square of n
        findK i | i * i >= n = i
                | otherwise  = findK (i+1)
        go _ Empty = blank
        go i (Entry b bs) =
          translated (fromIntegral (i `mod` k))
                     (-fromIntegral (i `div` k))
                     (pictureOfBool b)
          & go (i+1) bs

        pictureOfBool True =  colored green (solidCircle 0.4)
        pictureOfBool False = colored red   (solidCircle 0.4)

-- The main function

main :: IO ()
main = runInteraction (withStartScreen $ resetable $ withUndo $ sokoban)

-- copy-pasta from Mazes.hs

data Maze = Maze Coord (Coord -> Tile) 

mazes :: List Maze
mazes =
  Entry (Maze (C 1 1)       maze9) $
  Entry (Maze (C 0 0)       maze8) $
  Entry (Maze (C (-3) 3)    maze7) $
  Entry (Maze (C (-2) 4)    maze6) $
  Entry (Maze (C 0 1)       maze5) $
  Entry (Maze (C 1 (-3))    maze4) $
  Entry (Maze (C (-4) 3)    maze3) $
  Entry (Maze (C 0 1)       maze1) $
  Empty
  
extraMazes :: List Maze
extraMazes =
  Entry (Maze (C 1 (-3))    maze4') $
  Entry (Maze (C 1 (-3))    maze4'') $
  Entry (Maze (C 1 1)       maze9') $
  mazes

maze1 :: Coord -> Tile 
maze1 (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

maze3 :: Coord -> Tile
maze3 (C (-5) (-5)) = Wall
maze3 (C (-5) (-4)) = Wall
maze3 (C (-5) (-3)) = Wall
maze3 (C (-5) (-2)) = Wall
maze3 (C (-5) (-1)) = Wall
maze3 (C (-5)   0 ) = Wall
maze3 (C (-5)   1 ) = Wall
maze3 (C (-5)   2 ) = Wall
maze3 (C (-5)   3 ) = Wall
maze3 (C (-5)   4 ) = Wall

maze3 (C (-4) (-5)) = Wall
maze3 (C (-4) (-4)) = Ground
maze3 (C (-4) (-3)) = Ground
maze3 (C (-4) (-2)) = Ground
maze3 (C (-4) (-1)) = Ground
maze3 (C (-4)   0 ) = Ground
maze3 (C (-4)   1 ) = Ground
maze3 (C (-4)   2 ) = Ground
maze3 (C (-4)   3 ) = Ground
maze3 (C (-4)   4 ) = Wall

maze3 (C (-3) (-5)) = Wall
maze3 (C (-3) (-4)) = Ground
maze3 (C (-3) (-3)) = Wall
maze3 (C (-3) (-2)) = Wall
maze3 (C (-3) (-1)) = Wall
maze3 (C (-3)   0 ) = Wall
maze3 (C (-3)   1 ) = Ground
maze3 (C (-3)   2 ) = Wall
maze3 (C (-3)   3 ) = Ground
maze3 (C (-3)   4 ) = Wall
maze3 (C (-3)   5 ) = Wall

maze3 (C (-2) (-5)) = Wall
maze3 (C (-2) (-4)) = Box
maze3 (C (-2) (-3)) = Ground
maze3 (C (-2) (-2)) = Ground
maze3 (C (-2) (-1)) = Ground
maze3 (C (-2)   0 ) = Wall
maze3 (C (-2)   1 ) = Ground
maze3 (C (-2)   2 ) = Box
maze3 (C (-2)   3 ) = Box
maze3 (C (-2)   4 ) = Ground
maze3 (C (-2)   5 ) = Wall

maze3 (C (-1) (-6)) = Wall
maze3 (C (-1) (-5)) = Wall
maze3 (C (-1) (-4)) = Ground
maze3 (C (-1) (-3)) = Ground
maze3 (C (-1) (-2)) = Ground
maze3 (C (-1) (-1)) = Ground
maze3 (C (-1)   0 ) = Wall
maze3 (C (-1)   1 ) = Ground
maze3 (C (-1)   2 ) = Ground
maze3 (C (-1)   3 ) = Box
maze3 (C (-1)   4 ) = Ground
maze3 (C (-1)   5 ) = Wall
maze3 (C (-1)   6 ) = Wall

maze3 (C   0  (-6)) = Wall
maze3 (C   0  (-5)) = Ground
maze3 (C   0  (-4)) = Ground
maze3 (C   0  (-3)) = Ground
maze3 (C   0  (-2)) = Ground
maze3 (C   0  (-1)) = Ground
maze3 (C   0    0 ) = Wall
maze3 (C   0    1 ) = Wall
maze3 (C   0    2 ) = Wall
maze3 (C   0    3 ) = Wall
maze3 (C   0    4 ) = Ground
maze3 (C   0    5 ) = Ground
maze3 (C   0    6 ) = Wall

maze3 (C   1  (-6)) = Wall
maze3 (C   1  (-5)) = Ground
maze3 (C   1  (-4)) = Ground
maze3 (C   1  (-3)) = Ground
maze3 (C   1  (-2)) = Ground
maze3 (C   1  (-1)) = Ground
maze3 (C   1    0 ) = Wall
maze3 (C   1    1 ) = Storage
maze3 (C   1    2 ) = Storage
maze3 (C   1    3 ) = Storage
maze3 (C   1    4 ) = Ground
maze3 (C   1    5 ) = Ground
maze3 (C   1    6 ) = Wall

maze3 (C   2  (-6)) = Wall
maze3 (C   2  (-5)) = Wall
maze3 (C   2  (-4)) = Ground
maze3 (C   2  (-3)) = Ground
maze3 (C   2  (-2)) = Ground
maze3 (C   2  (-1)) = Ground
maze3 (C   2    0 ) = Wall
maze3 (C   2    1 ) = Wall
maze3 (C   2    2 ) = Wall
maze3 (C   2    3 ) = Wall
maze3 (C   2    4 ) = Wall
maze3 (C   2    5 ) = Wall
maze3 (C   2    6 ) = Wall

maze3 (C   3  (-5)) = Wall
maze3 (C   3  (-4)) = Ground
maze3 (C   3  (-3)) = Ground
maze3 (C   3  (-2)) = Storage
maze3 (C   3  (-1)) = Ground
maze3 (C   3    0 ) = Wall

maze3 (C   4  (-5)) = Wall
maze3 (C   4  (-4)) = Wall
maze3 (C   4  (-3)) = Wall
maze3 (C   4  (-2)) = Wall
maze3 (C   4  (-1)) = Wall
maze3 (C   4    0 ) = Wall

maze3 _ = Blank

maze4 :: Coord -> Tile
maze4 (C x y)
  | abs x > 4  || abs y > 4      = Blank
  | abs x == 4 || abs y == 4     = Wall
  | x ==  2 && y <   0           = Wall
  | x >= -1 && y ==  1 && x <= 2 = Wall
  | x == -3 && y ==  1           = Wall
  | x ==  0 && y ==  3           = Wall
  | x ==  0 && y ==  0           = Wall
  | x ==  3 && y == -3           = Storage
  | x ==  1 && y ==  2           = Storage
  | x == -3 && y ==  2           = Storage
  | x ==  1 && y == -1           = Storage
  | x == -2 && y ==  1           = Box
  | x ==  2 && y ==  2           = Box
  | x <=  1 && y == -2 && x >= 0 = Box
  | otherwise                    = Ground

maze5 :: Coord -> Tile 
maze5 (C x y)
  | abs x >  4 || abs y >  4           = Blank
  | abs x == 4 || abs y == 4           = Wall
  | x ==     1 && y <      0           = Wall
  | x ==    -3 && y ==    -2           = Wall
  | x <=     1 && x >     -2 && y == 0 = Wall
  | x >     -3 && x <      3 && y == 2 = Wall
  | x ==     3 && y >      1           = Storage
  | y ==    -2 && x <      0           = Box
  | y ==    -2 && x ==     2           = Box
  | y ==    0  && x ==     3           = Box
  | y == -1    && x > 1      && x < 4  = Storage
  | otherwise                          = Ground

maze6 :: Coord -> Tile 
maze6 (C x y)
  | abs x > 3  || abs y > 5                 = Blank
  | abs x == 3 || (abs y == 5 && abs x < 4) = Wall
  | x == 0 && abs y < 4                     = Storage
  | x == -1 && (y == 0 || abs y == 2)       = Box
  | x == 1 && (abs y == 1 || abs y == 3)    = Box
  | x == (-2) &&  y == 1                    = Wall
  | otherwise                               = Ground

maze7 :: Coord -> Tile
maze7 (C x y)
  | abs x > 4  || abs y > 4   = Blank
  | abs x == 4 || abs y == 4  = Wall
  | not (x == 2)  && y == 2   = Wall
  | not (x == -2)  && y == -1 = Wall
  | x ==  3 && y == -3        = Storage
  | x == 2 && y == 2          = Box
  | otherwise                 = Ground
  
maze8 :: Coord -> Tile
maze8 (C x y)
  | abs x > 10 || abs y > 10    = Blank
  | x == 0 && y == 0            = Ground
  | abs x == 9 && abs y == 9    = Wall
  | abs x == 10 || abs y == 10  = Wall
  | x == y                      = Storage
  | abs x == abs y              = Box
  | x < 0 && x > (-9) && y == 0 = Box
  | x > 0 && x < 9 && y == 0    = Storage
  | otherwise                   = Ground

maze9 :: Coord -> Tile 
maze9 (C x y)
  | abs x > 4  || abs y > 4                  = Blank
  | abs x == 4 || abs y == 4 || x == -3      = Wall
  | x == -2 && (y == 3 || y == 0)            = Wall
  | x == -1 &&  y == -1                      = Wall
  | x == -0 &&  y == 1                       = Wall
  | x ==  3 &&  y == 0                       = Wall
  | x <   0 && (y == 2 || y == -3)           = Storage
  | x == -1 &&  y == 1                       = Storage
  | x ==  0 && (y == 2 || y == 0 || y == -1) = Box
  | x ==  1 &&  y == -2                      = Box
  | x ==  2 &&  y == -3                      = Box
  | otherwise                                = Ground

maze4'' :: Coord -> Tile
maze4'' (C 1 (-3)) = Box
maze4'' c = maze4 c

maze4' :: Coord -> Tile
maze4' (C 0 1) = Blank
maze4' c = maze4 c

maze9' :: Coord -> Tile
maze9' (C 3 0) = Box
maze9' (C 4 0) = Box
maze9'  c      = maze9 c
