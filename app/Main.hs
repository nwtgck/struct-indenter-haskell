{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Maybe
import Data.List
import System.IO

import           Text.Parsec      (Parsec, (<|>))
import qualified Text.Parsec      as Parsec
import qualified Text.Parsec.Char as ParsecChar

data Term =
   StopTerm {name :: String}
 | ContTerm {name :: String, terms :: [Term]}
 deriving (Show, Eq)

nameP = Parsec.many1 (ParsecChar.noneOf ['(', ')', ','])

termP :: Parsec String u Term
termP = Parsec.try contTermP <|> stopTermP
  where
    contTermP = do
      name  <- nameP
      start <- ParsecChar.char '('
      terms <- Parsec.sepBy termP (ParsecChar.char ',')
      end   <- ParsecChar.char ')'
      return ContTerm{name=name, terms=terms}

    stopTermP = (StopTerm <$> stringP) <|> do
      name  <- nameP
      return StopTerm{name=name}



-- (from: https://stackoverflow.com/a/24106749/2885946)
stringP :: Parsec String u String
stringP = do
    ParsecChar.char '"'
    strings <- Parsec.many character
    ParsecChar.char '"'
    return $ concat strings
 where
  escape :: Parsec String u String
  escape = do
      d <- ParsecChar.char '\\'
      c <- ParsecChar.oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
      return [d, c]

  nonEscape :: Parsec String u Char
  nonEscape = ParsecChar.noneOf "\\\"\0\n\r\v\t\b\f"

  character :: Parsec String u String
  character = fmap return nonEscape <|> escape


showWithIndent :: Int -> Term -> String
showWithIndent indent StopTerm{name}        = (replicate indent ' ') ++ name
showWithIndent indent ContTerm{name, terms} = (replicate indent ' ') ++ name ++ "(\n" ++  (intercalate ",\n" (fmap (\t -> showWithIndent (indent + 2) t ) terms )) ++ "\n" ++  ((replicate indent ' ') ++ ")")

main :: IO ()
main = do
  codeStr <- hGetContents stdin
  case Parsec.parse termP "" codeStr of
    Right term ->
      putStrLn (showWithIndent 0 term)
    Left e ->
      print e
