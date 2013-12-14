#!/usr/bin/env runhaskell
module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import qualified Data.Map as Map
import Numeric (readFloat)
import Data.Ratio (Rational,(%))

main = do
  exprs <- readFile "test.scm"
  sequence_ $ map (putStrLn.readExpr) $ lines exprs
  

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Integer Integer
             | Float {getLispFloat::Float}
             | Rational Rational 
             | Bool Bool
             | String String
             | Char Char
             deriving (Show,Eq)


listExpr = liftM List $ sepBy expr spaces


dottedListExpr = do
  h <- endBy expr spaces
  t <- char '.' >> spaces >> expr
  return $ DottedList h t


stringExpr = do
  char '"'
  s <- strRemain
  char '"'
  return $ String $ unescape s


strRemain :: Parser String
strRemain = (many $ noneOf "\"\\") >>= (\s ->
  (char '\\' >> (
    (oneOf "nrt\\\"")>>= (\q->
    strRemain >>= \rs -> 
    return $ s ++ ('\\':q:rs)
  ))) <|> return s)

atomExpr = do
  first <- letter <|> symbol
  rest <- many $ letter <|> symbol <|> digit
  let atom = first:rest
  return $ Atom atom

boolExpr = oneOf "tf" >>= return . Bool . (== 't')

charExpr = do
  char '\\'
  n <- (many1 letter) <|> (anyToken>>=return.(: []))
  return $ Char $ charFromName n

radixNumExpr = exactNumExpr <|> (oneOf "id" >> decimalExpr) <|> hexExpr <|> binaryExpr <|> octalExpr

decimalExpr = many digit>>=(\i ->
                              if null i then floatPart i
                              else floatPart i <|> powerPart i <|> fractPart i <|> intResult i)

intResult :: String -> Parser LispVal
intResult = return . Integer . read 

floatPart :: String -> Parser LispVal
floatPart i = char '.' >> 
  many digit >>= \a -> 
    if null (i++a) then fail "Single '.' cant be treated as float!"
    else 
      let n = '0':i ++ '.':a ++ "0" in powerPart n <|> floatResult n

floatResult :: String -> Parser LispVal
floatResult = return . Float . read

powerPart :: String -> Parser LispVal
powerPart i = char 'e' >> many1 digit >>=(\p->return . Float . read $ i ++"e" ++ p)
  
fractPart :: String -> Parser LispVal
fractPart numerator = char '/' >> many1 digit >>= \denominator -> 
                        if denominator == "0" then fail $ "Division by zero:"++numerator++"/0"
                        else
                          let n = read numerator :: Integer
                              d = read denominator :: Integer
                              nf = fromIntegral n
                              df = fromIntegral d
                              r = Rational (n % d) in
                          (powerPart "1" >>=(\p->
                             return . Float $ nf/df*(getLispFloat p))) <|> return r

exactNumExpr = char 'e' >> (liftM (Rational .fst.head.readFloat) $ many1 (digit <|> oneOf ".e"))

hexExpr =  char 'x' >> (liftM (Integer . read . ("0x" ++)) $ many1 hexDigit)
binaryExpr = char 'b' >> (liftM (Integer . readBinary) $ many1 (oneOf "01"))
octalExpr = char 'o' >> (liftM (Integer . read . ("0o" ++)) $ many1 octDigit)

sharpTokenExpr = char '#' >> (boolExpr <|> charExpr <|> radixNumExpr)

expr :: Parser LispVal
expr = stringExpr <|> sharpTokenExpr <|> atomExpr <|> decimalExpr

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse expr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val



charNameMap = Map.fromList [
  ("newline",'\n'),
  ("tab",'\t'),
  ("space",' ')]

charFromName :: String -> Char
charFromName [a] = a
charFromName name = case Map.lookup name charNameMap of
                        Just c -> c
                        Nothing -> error $ "Bad char name:" ++ show name

escapeList = [
  ("\\\\","\\"),
  ("\\\"","\""),
  ("\\n","\n"),
  ("\\t","\t"),
  ("\\r","\r")]

unescape :: String -> String
unescape s= foldr (\(k,v) s->replace k v s) s escapeList

replace :: String -> String -> String -> String
replace search new haystack | n > (length haystack) = haystack
                            | otherwise =  if take n haystack  == search then
                                             new ++ replace search new (drop n haystack)
                                           else
                                             (head haystack):(replace search new (tail haystack))
                            where n = length search

readBinary :: String -> Integer
readBinary =  snd . foldr (\t (r,a)->(r+1,(if t then 2^r else 0)+a)) (0,0) . map (== '1')
