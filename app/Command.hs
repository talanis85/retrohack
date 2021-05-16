module Command
  ( Command
  , parseCommand
  , commandP
  , symbolP
  , stringP
  , addressP
  , integerP
  , typeP

  -- * re-exports
  , choice
  ) where

import Text.Parsec
import Text.Parsec.Error
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

import AppM
import Retrohack.Memory

type Command = Parsec String () (AppM ())

parseCommand :: [Parsec String () (AppM ())] -> String -> AppM ()
parseCommand _ "" = return ()
parseCommand commands s = do
  let result = runParser (choice commands) () "<command>" s
  case result of
    Left err -> case errorMessages err of
      [] -> output $ "Error parsing command"
      xs -> output $ "Error parsing command: " ++ (messageString (last (errorMessages err)))
    Right x -> x

lexer = P.makeTokenParser haskellDef

symbolP = try . P.symbol lexer
stringP = P.stringLiteral lexer
addressP = P.natural lexer
integerP = P.integer lexer
typeP = choice
  [ symbolP "i8" >> return I8
  , symbolP "i16" >> return I16
  , symbolP "i32" >> return I32
  , symbolP "u8" >> return U8
  , symbolP "u16" >> return U16
  , symbolP "u32" >> return U32
  ]

commandP name doc p = do
  symbolP name
  try p <|> fail ("Syntax: " ++ name ++ " " ++ doc)
