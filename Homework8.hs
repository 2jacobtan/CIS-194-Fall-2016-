
-- import Control.Applicative (liftA2)
import Data.Char (isAlphaNum)

import System.Environment
import System.IO
import System.Exit


-- Exercise 3

-- (*>) :: Applicative f => f a -> f b -> f b
-- a1 *> a2 = id <$ a1 <*> a2
-- (<*) :: Applicative f => f a -> f b -> f a
-- -- a1 <* a2 = const <$> a1 <*> a2
-- (<*) = liftA2 const

data Parser a = P { runParser :: ( String -> Maybe (a,String) ) }

parse :: Parser a -> String -> Maybe a
parse p s = case runParser p s of
  Just (x,"") -> Just x
  _ -> Nothing -- this line replaces the 2 lines below
  -- Just _ -> Nothing
  -- Nothing -> Nothing

-- for testing
parse2 :: Parser a -> String -> Maybe (a,String)
parse2 p s = runParser p s

noParser :: Parser a
noParser = P f
  where f _ = Nothing

pureParser :: a -> Parser a
pureParser x = P f
  where f s = Just (x,s)

instance Functor Parser where
  fmap f p = P f'
    where
      f' s = case runParser p s of
        Nothing -> Nothing
        Just (x,s') -> Just (f x, s')
{- 
instance Applicative Parser where
  pure = pureParser
  fp <*> fx = P g
    where
      g s = case runParser fp s of
        Nothing -> Nothing
        Just (f,s) -> case runParser fx s of
          Nothing -> Nothing
          Just (x,s') -> Just (f x,s')
 -}
instance Applicative Parser where
  pure = pureParser
  p1 <*> p2 = P $ \input -> do
    (f,rest1) <- runParser p1 input
    (x,rest2) <- runParser p2 rest1
    return (f x, rest2)

instance Monad Parser where
  return = pureParser
  ma >>= k = P f'
    where
      f' s = case runParser ma s of
        Nothing -> Nothing
        Just (x,s') -> runParser (k x) s'

anyChar :: Parser Char
anyChar = P (\s -> if s == "" then Nothing else let x:xs = s in Just (x,xs))

char :: Char -> Parser ()
char c = do
  x <- anyChar
  if x == c then return () else noParser

anyCharBut :: Char -> Parser Char
anyCharBut c = do
  x <- anyChar
  if x == c then noParser else return x

orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 = P go
  where go s = case runParser p1 s of
          Just xl -> Just xl
          Nothing -> runParser p2 s

-- ? try to use orElse
-- ? try to use "do" notation
many :: Parser a -> Parser [a]
many p = P $ Just . go []
  where
    go l s = case runParser p s of
      Nothing -> (l,s)
      Just (x,s') -> go (l++[x]) s'

many' :: Parser a -> Parser [a]
many' p = ( (:) <$> p <*> many' p ) `orElse` return []

-- ? try to use "do" notation
sepBy :: Parser a -> Parser () -> Parser [a]
sepBy p1 p2 = P $ Just . go
  where
    go s = case runParser p1 s of
      Nothing -> ([], s)
      Just (x,s') ->
        let Just (l,s'') = runParser (many $ p2 *> p1) s'
        in ([x] ++ l,s'')

sepBy' :: Parser a -> Parser () -> Parser [a]
sepBy' p1 p2 = ( (:) <$> p1 <*>  many (p2 *> p1) ) `orElse` return []

--copy-pasta from Homework 8
parseCSV :: Parser [[String]]
parseCSV = many parseLine
  where
    parseLine = parseCell `sepBy` char ',' <* char '\n'
    parseCell = do
        char '"'
        content <- many (anyCharBut '"')
        char '"'
        return content


-- Exercise 4

many1 :: Parser a -> Parser [a]
many1 p = P go1
  where
    go1 s = case runParser p s of
      Nothing -> Nothing
      Just (x,s') -> Just $ go [x] s'
    go l s = case runParser p s of
      Nothing -> (l,s)
      Just (x,s') -> go (l++[x]) s'

many1' :: Parser a -> Parser [a]
many1' p = do
  x <- p
  xs <- many p
  return (x:xs)

letterOrDigit :: Parser Char
letterOrDigit = P go
  where
    go s = case s of
      (x:xs) | isAlphaNum x -> Just (x,xs)
      _ -> Nothing

letterOrDigit' :: Parser Char
letterOrDigit' = do
  x <- anyChar
  if isAlphaNum x then return x
  else noParser

commentLine :: Parser ()
commentLine = do
  char '#'
  many $ anyCharBut '\n'
  char '\n'
  return ()

emptyLine :: Parser ()
emptyLine = do
  many $ char ' '
  char '\n'
  return ()

ignoredLine :: Parser ()
ignoredLine = commentLine `orElse` emptyLine

{- nextLineINI :: Parser a -> Parser a
nextLineINI = do
  many $ orElse commentLine emptyLine
  nextLine <- 
 -}  

--copy-pasta from Homework 8
type Identifier = String
type Declaration = (Identifier, String)
type Section = (Identifier, [Declaration])
type INIFile = [Section]
parseINI :: Parser INIFile
parseINI = many parseSection

parseIndentifier :: Parser Identifier 
parseIndentifier = many1' letterOrDigit'

parseDeclaration :: Parser Declaration
parseDeclaration = do
  key <- parseIndentifier
  many $ char ' '
  char '='
  many $ char ' '
  value <- many $ anyCharBut '\n'
  char '\n'
  many ignoredLine
  return (key,value)

parseSection :: Parser Section
parseSection = do
  char '['
  title <- parseIndentifier
  char ']'
  char '\n'
  many ignoredLine
  declarationList <- many parseDeclaration
  -- many ignoredLine
  return (title,declarationList)

example = unlines
  ["[requests]",
  "desiredFood = cookies",
  "desiredQuantity = 20",
  "",
  "[supply]",
  "flour = 20 ounzes",
  "",
  "sugar = none!",
  "[conclusion]",
  "# none!"]

main :: IO ()
main = do
    args <- getArgs
    input <- case args of
        [] -> getContents
        [fileName] -> readFile fileName
        _ -> hPutStrLn stderr "Too many arguments given" >> exitFailure
    case parse parseINI input of
        Just i -> print i
        Nothing -> do
            hPutStrLn stderr "Failed to parse INI file."
            exitFailure