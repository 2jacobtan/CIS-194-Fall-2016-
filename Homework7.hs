{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import System.Random

-- Exercise 1
fib :: Integer -> Integer
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = [1..] >>= return . fib

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 2
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList = streamToList1
-- streamToList = streamToList2
streamToList1 (Cons a s) = [a] ++ streamToList s
streamToList2 s = go [] s -- tail-recursive version; but ++ is not tail-recursive?
  where go l (Cons a s') = go (l ++ [a]) s'

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat a = Cons a $ streamRepeat a

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a s) = Cons (f a) $ streamMap f s

streamIterate :: (a -> a) -> a -> Stream a
streamIterate f a = Cons a $ streamIterate f $ f a

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons a s) s' = Cons a $ streamInterleave s' s

nats :: Stream Integer
nats = streamIterate (+1) 1
-- nats = streamIterate (+1) 0

ruler :: Stream Integer
ruler = go $ streamRepeat 0
  where go s = streamInterleave s $ go $ streamMap (+1) s

main :: IO ()
-- main = print $ take 5 fibs1
-- main = print fibs1
-- main = print $ take 30 fibs2
-- main = print ruler

-- Exercise 3
data Supply s a = S (Stream s -> (a, Stream s))

get :: Supply s s
get = get2
get1 = S go
  where
    go (Cons x s) = (x, s)
get2 = S $ \(Cons x s) -> (x, s)

pureSupply :: a -> Supply s a
pureSupply a = S $ \s -> (a,s)

mapSupply :: (a -> b) -> Supply s a -> Supply s b
mapSupply f (S sup) = S $ \s ->
  let (a, s') = (sup s)
  in (f a, s')

mapSupply2 :: (a -> b -> c) -> Supply s a -> Supply s b -> Supply s c
mapSupply2 f (S sup1) (S sup2) = S go
  where
    go s = (f a b, s'')
      where
        (a, s') = sup1 s
        (b, s'') = sup2 s'

bindSupply :: Supply s a -> (a -> Supply s b) -> Supply s b
bindSupply (S sup1) f = S $ \s ->
  let (a, s') = sup1 s; (S sup2) = f a
  in sup2 s'

runSupply :: Stream s -> Supply s a -> a
runSupply s (S sup) = fst $ sup s

instance Functor (Supply s) where
  fmap = mapSupply

instance Applicative (Supply s) where
  pure = pureSupply
  (<*>) = mapSupply2 id

instance Monad (Supply s) where
  return = pureSupply
  (>>=) = bindSupply

data Tree a = Node (Tree a) (Tree a) | Leaf a deriving Show

labelTree :: Tree a -> Tree Integer
labelTree t = runSupply nats $ go t
  where
    go :: Tree a -> Supply s (Tree s)
    go (Leaf _) = Leaf <$> get
    go (Node t1 t2) = Node <$> go t1 <*> go t2  

t1 = let l = Leaf () ; n = Node in n (n (n l l) l) (n l l)

t2 = labelTree t1


-- Non-Exercise: dice throws
type Rand a = Supply Integer a

randomDice :: RandomGen g => g -> Stream Integer
randomDice gen =
    let (roll, gen') = randomR (1,6) gen
    in Cons roll (randomDice gen')

runRand :: Rand a -> IO a
runRand r = do
    stdGen <- getStdGen
    let diceRolls = randomDice stdGen
    return $ runSupply diceRolls r

averageOfTwo :: Rand Double
averageOfTwo = do
    d1 <- get
    d2 <- get
    return $ fromIntegral (d1 + d2) / 2

bestOutOfTwo :: Rand Double
bestOutOfTwo = do
    d1 <- get
    d2 <- get
    return $ fromIntegral $ if (d1 > d2) then d1 else d2

-- Look, ma, I’m recursive!
sumUntilOne :: Rand Double
sumUntilOne = do
    d <- get
    if (d == 1) then return 0
                else do
                  s <- sumUntilOne
                  return (s + fromIntegral d)

sample :: Int -> Rand Double -> Rand (Double, Double)
sample n what = do
    samples <- replicateM n what
    return (maximum samples, sum samples / fromIntegral n)

diceThrows = mapM_ go [ ("average of two", averageOfTwo)
          , ("bestOutOfTwo",   bestOutOfTwo)
          , ("sumUntilOne",    sumUntilOne)
          ]
  where
    n = 10000
    go (name, what) = do
      (max, avg) <- runRand (sample n what)
      putStrLn $ "Playing \"" ++ name ++ "\" " ++ show n ++ " times " ++
                 "yields a max of " ++ show max ++ " and an average of " ++
                 show avg ++ "."

main = diceThrows