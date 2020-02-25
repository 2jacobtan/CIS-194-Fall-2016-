import Data.Char (isControl, isAscii, isDigit)
import Data.List (foldl1', intercalate, foldl')

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

--e5
adjacents:: [a] -> [(a,a)]
adjacents [] = []
adjacents l = zip (init l) (tail l)

ex_adjacents =
   [ adjacents "" == []
   , adjacents [True] == []
   , adjacents "Hello" == [('H','e'),('e','l'),('l','l'),('l','o')]
   ]
q5 = ex_adjacents

--e6
commas :: [String] -> String
commas l = intercalate ", " l

ex_commas =
   [ commas [] == ""
   , commas ["Hello"] == "Hello"
   , commas ["Hello", "World"] == "Hello, World"
   , commas ["Hello", "", "World"] == "Hello, , World"
   , commas ["Hello", "new", "World"] == "Hello, new, World"
   ]
q6 = ex_commas

--e7
addPolynomials :: [[Integer]] -> [Integer]
addPolynomials ll = foldl1' (zipWith (+)) ll

ex_addPolynomials =
   [ addPolynomials [[]] == []
   , addPolynomials [[0, 1], [1, 1]] == [1, 2]
   , addPolynomials [[0, 1, 5], [7, 0, 0], [-2, -1, 5]] == [5, 0, 10]
   ]
q7 = ex_addPolynomials

--e8
sumNumbers :: String -> Integer
sumNumbers s = sum $ foldl' f [] s
  where
    f :: [Integer] -> Char -> [Integer]
    f l c = case (isDigit c) of
      True -> init0 l ++ [read (showLast0 l ++ [c]) :: Integer]
      False ->
        if l == [] then []
        else if last l == 0 then l
        else l ++ [0]
    init0 [] = []
    init0 xs = init xs
    showLast0 [] = ""
    showLast0 xs = show $ last xs

ex_sumNumbers =
   [ sumNumbers "" == 0
   , sumNumbers "Hello world!" == 0
   , sumNumbers "a1bc222d3f44" == 270
   , sumNumbers "words0are1234separated12by3integers45678" == 46927
   , sumNumbers "000a." == 0
   , sumNumbers "0.00a." == 0
   ]
q8 = ex_sumNumbers