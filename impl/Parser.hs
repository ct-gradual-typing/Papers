{-# LANGUAGE NoMonomorphismRestriction, PackageImports, TemplateHaskell #-}

module Parser (module Text.Parsec, expr, Vnm, letParser, lineParser, REPLExpr(..), parseLine) where

import Prelude
import Data.List
import Data.Char 
import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language
import Control.Monad -- For debugging messages.
import Data.Functor.Identity

import Syntax
    
------------------------------------------------------------------------
-- We first setup the lexer.                                          --
------------------------------------------------------------------------
lexer = haskellStyle {
  Token.reservedOpNames = ["x", "->", "0", "succ", "?", "triv", "\\", "proj1", "proj2", ":t", ":type", ":s", ":show"]
}
tokenizer = Token.makeTokenParser lexer

ident      = Token.identifier tokenizer
reserved   = Token.reserved tokenizer
reservedOp = Token.reservedOp tokenizer
parens     = Token.parens tokenizer
angles     = Token.angles tokenizer
brackets   = Token.brackets tokenizer
braces     = Token.braces tokenizer
ws         = Token.whiteSpace tokenizer
natural    = Token.natural tokenizer
dot        = Token.dot tokenizer
comma      = Token.comma tokenizer
colon      = Token.colon tokenizer
symbol     = Token.symbol tokenizer             

unexpColon msg = unexpected msg -- Used for error handing.

------------------------------------------------------------------------
-- First, we implement the parser for types called typeParser.        --
------------------------------------------------------------------------
var' p c = do 
  var_name <- p
  return (c var_name)  

varName' p msg = do
  n <- many1 alphaNum
  ws
  when ((length n) > 0) $
    let h = head n in 
      when (p h || isNumber h) $ unexpColon (n++" : "++msg)
  return . s2n $ n

parseConst s c = do
  reservedOp s
  return c
         
tyNat = parseConst "Nat" Nat
tyU = parseConst "U" U
tyUnit = parseConst "1" Unit         
        
-- The initial expression parsing table for types.
table = [[binOp AssocRight "->" (\d r -> Arr d r), binOp AssocLeft "x" (\d r -> Prod d r)]]
binOp assoc op f = Text.Parsec.Expr.Infix (do{ reservedOp op;ws;return f}) assoc
typeParser = buildExpressionParser table typeParser'
typeParser' = parens typeParser <|> tyNat <|> tyU <|> tyUnit

------------------------------------------------------------------------
-- Next the term parsers.                                             --
------------------------------------------------------------------------
aterm = try (parens pairParse) <|> parens expr <|> zeroParse <|> trivParse <|> var
expr = funParse <|> succParse <|> fstParse <|> sndParse <|> appParse <|> parens expr <?> "parse error" 
              
varName = varName' isUpper "Term variables must begin with a lowercase letter."
var = var' varName Var

zeroParse = parseConst "0" Zero
trivParse = parseConst "triv" Triv

succParse = do
  reservedOp "succ"
  ws
  t <- expr
  return $ Succ t

pairParse = do
  t1 <- expr
  comma
  t2 <- expr
  return $ Pair t1 t2

fstParse = do
  reservedOp "fst"
  t <- expr
  return $ Fst t

sndParse = do
  reservedOp "snd"
  t <- expr
  return $ Snd t
         
funParse = do
  reservedOp "\\"
  symbol "("
  name <- varName
  colon
  ty <- typeParser
  symbol ")"
  dot
  body <- expr
  return . Fun ty . bind name $ body

appParse = do
  l <- many aterm
  return $ foldl1 App l         

------------------------------------------------------------------------                 
-- Parsers for the REPL                                               --
------------------------------------------------------------------------        

data REPLExpr =
   Let Vnm Term                 -- Toplevel let-expression: for the REPL
 | TypeCheck Term               -- Typecheck a term
 | ShowAST Term                 -- Show a terms AST
 | DumpState                    -- Trigger to dump the state for debugging.
 | Unfold Term                  -- Unfold the definitions in a term for debugging.
 deriving Show
                    
letParser = do
  reservedOp "let"
  n <- varName
  symbol "="
  t <- expr
  eof
  return $ Let n t         

replTermCmdParser short long c p = do
  colon
  cmd <- many lower
  ws
  t <- p
  eof
  if (cmd == long || cmd == short)
  then return $ c t
  else fail $ "Command \":"++cmd++"\" is unrecognized."

replIntCmdParser short long c = do
  colon
  cmd <- many lower
  eof
  if (cmd == long || cmd == short)
  then return c
  else fail $ "Command \":"++cmd++"\" is unrecognized."       
                 
typeCheckParser = replTermCmdParser "t" "type" TypeCheck expr

showASTParser = replTermCmdParser "s" "show" ShowAST expr

unfoldTermParser = replTermCmdParser "u" "unfold" Unfold expr                

dumpStateParser = replIntCmdParser "d" "dump" DumpState
                 
lineParser = letParser <|> try typeCheckParser <|> try showASTParser <|> try unfoldTermParser <|> dumpStateParser

parseLine :: String -> Either String REPLExpr
parseLine s = case (parse lineParser "" s) of
                Left msg -> Left $ show msg
                Right l -> Right l