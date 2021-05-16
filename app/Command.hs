module Command
  ( Command
  , parseCommand
  , commandP
  , symbolP
  , stringP
  , addressP
  , valueP

  -- * re-exports
  , choice
  ) where

import Text.Parsec
import Text.Parsec.Error
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

import AppM

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
valueP = P.natural lexer

commandP name doc p = do
  symbolP name
  try p <|> fail ("Syntax: " ++ name ++ " " ++ doc)
