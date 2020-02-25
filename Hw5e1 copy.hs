import Data.Char (isControl, isAscii)
import Data.List (foldl1')

-- main :: IO ()
-- main = 

--e1
halveEvens :: [Integer] -> [Integer]
halveEvens xs = concatMap mapping xs
  where
    mapping :: Integer -> [Integer]
    mapping x = case x `mod` 2 of
      0 -> [x `div` 2]
      1 -> []

ex_halveEvens =
    [ halveEvens [] == []
    , halveEvens [1,2,3,4,5] == [1,2]
    , halveEvens [6,6,6,3,3,3,2,2,2] == [3,3,3,1,1,1]
    ]

q1 = print ex_halveEvens

--e2
safeString :: String -> String
safeString cs = map mapping cs
  where
    mapping c = case isUnwanted c of
      True -> '_'
      False -> c
    isUnwanted c = isControl c || (not . isAscii $ c)

ex_safeString =
    [ safeString [] == []
    , safeString "Hello World!" == "Hello World!"
    , safeString "That’s your line:\n" == "That_s your line:_"
    , safeString "🙋.o(“Me Me Me”)" == "_.o(_Me Me Me_)"
    ]

q2 = print ex_safeString

--e3
holes :: [a] -> [[a]]
holes l = map mapping [1..n]
  where
    n = length l
    mapping i = take (i-1) l ++ drop i l

ex_holes =
   [ holes "" == []
   , holes "Hello" == ["ello", "Hllo", "Helo", "Helo", "Hell"]
   ]
q3 = print ex_holes

--e4
longestText :: Show a => [a] -> a
longestText l = foldl1' longerShow l
  where
    longerShow a b
      | length (show a) > length (show b) = a
      | otherwise = b

ex_longestText =
   [ longestText [True,False] == False
   , longestText [2,4,16,32] == (32::Int)
   , longestText (words "Hello World") == "World"
   , longestText (words "Olá mundo") ==  "Olá"
   ]
q4 = ex_longestText