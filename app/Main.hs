{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Maybe
import Data.List

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
  let str = "List(Defn.Object(Nil, Term.Name(\"Main\"), Template(Nil, Nil, Self(Name(\"\"), None), List(Term.For(List(Enumerator.Generator(Pat.Var(Term.Name(\"i\")), Term.ApplyInfix(Lit.Int(0), Term.Name(\"to\"), Nil, List(Lit.Int(10))))), Term.Block(List(Term.Apply(Term.Name(\"println\"), List(Term.Interpolate(Term.Name(\"s\"), List(Lit.String(\"i: \"), Lit.String(\"\")), List(Term.Name(\"i\")))))))), Defn.Val(Nil, List(Pat.Var(Term.Name(\"arr\"))), None, Term.Apply(Term.Select(Term.Name(\"np\"), Term.Name(\"array\")), List(Lit.Int(1), Lit.Int(2), Lit.Int(3), Lit.Int(4)))), Defn.Val(Nil, List(Pat.Var(Term.Name(\"iOrStr\"))), Some(Type.Or(Type.Name(\"Int\"), Type.Name(\"String\"))), Lit.Int(10)), Term.Apply(Term.Name(\"println\"), List(Lit.String(\"hello, world\")))))))"
  let str2 = "A(1,2,3)"
  let str3 = "A(B(1,2,3),C(B,D,4),Z(B(1),C(2)))"
  putStrLn str
  case Parsec.parse termP "" str of
    Right term ->
      putStrLn (showWithIndent 0 term)
    Left e ->
      print e
