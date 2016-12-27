{-# LANGUAGE NoMonomorphismRestriction, PackageImports, TemplateHaskell, FlexibleContexts #-}

module Parser (module Text.Parsec, expr, 
               Vnm, 
               letParser, 
               lineParser, 
               REPLExpr(..), 
               parseLine, 
               runParse, 
               GFile, 
               Prog(..)) where

import Prelude
import Data.List
import Data.Char 
import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language
import Control.Monad -- For debugging messages.
import Data.Functor.Identity
import Text.Parsec.Extra

import Syntax
import Queue
import Pretty

------------------------------------------------------------------------
-- We first setup the lexer.                                          --
------------------------------------------------------------------------
lexer = haskellStyle {
  Token.reservedOpNames = ["x", "->", "0", "succ", "?", "triv", "\\", "proj1", "proj2", ":t", ":type", ":s", ":show", "Nat", "Triv",
                           "box", "unbox", ":l", "sqsh", "split"]
}
tokenizer = Token.makeTokenParser lexer

ident      = Token.identifier tokenizer
reserved   = Token.reserved tokenizer
reservedOp = Token.reservedOp tokenizer
parens     = Token.parens tokenizer
ws         = Token.whiteSpace tokenizer
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
  when ((length n) > 0) $
    let h = head n in 
      when (p h || isNumber h) $ unexpColon (n++" : "++msg)
  return . s2n $ n

parseConst s c = symbol s >> return c
         
tyNat = parseConst "Nat" Nat
tyU = parseConst "?" U
tyUnit = parseConst "1" Unit         
        
-- The initial expression parsing table for types.
table = [[binOp AssocRight "->" (\d r -> Arr d r), binOp AssocLeft "x" (\d r -> Prod d r)]]
binOp assoc op f = Text.Parsec.Expr.Infix (do{ reservedOp op;ws;return f}) assoc
typeParser = buildExpressionParser table typeParser'
typeParser' = parens typeParser <|> tyNat <|> tyU <|> tyUnit

------------------------------------------------------------------------
-- Next the term parsers.                                             --
------------------------------------------------------------------------
aterm = try (parens pairParse) <|> parens expr    <|> zeroParse 
                               <|> try trivParse  <|> try squash
                               <|> try split      <|> try boxParse
                               <|> try unboxParse <|> var
expr = funParse <|> succParse <|> fstParse <|> sndParse <|> appParse <|> parens expr <?> "parse error"
              
varName = varName' isUpper "Term variables must begin with a lowercase letter."
var = var' varName Var

zeroParse = parseConst "0" Zero
trivParse = parseConst "triv" Triv

boxParse = do
  symbol "box"
  ty <- between (symbol "<") (symbol ">") typeParser
  return $ Box ty

unboxParse = do
  symbol "unbox"
  symbol "<"
  ty <- typeParser
  symbol ">"
  return $ Unbox ty

succParse = do
  reservedOp "succ"
  t <- expr
  return $ Succ t
         
pairParse = do
  t1 <- expr
  symbol ","
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
  ws
  symbol ":"
  ty <- typeParser
  symbol ")"
  symbol "."
  body <- expr
  return . Fun ty . bind name $ body

appParse = do
  l <- many (ws *> aterm)
  case l of
    [] -> fail "A term must be supplied"
    _ -> return $ foldl1 App l

squash = do
  reservedOp "squash"
  return $ Squash
  
split = do
  reservedOp "split"
  return $ Split

------------------------------------------------------------------------                 
-- Parsers for the Files                                              --
------------------------------------------------------------------------        

type TypeDef = (Vnm, Type)   
type ExpDef = (Vnm, Term)

data Prog =
  Def Vnm Type Term
  deriving (Show)

type GFile = Queue Prog      -- Grady file

parseTypeDef = do
  n <- varName
  ws
  symbol ":"    
  ty <- (typeParser) 
  return (n,ty)

parseExpDef = do
  n <- varName
  ws
  symbol "=" 
  t <- expr 
  return (n,t)   

parseDef = do
  (n, ty) <- ws *> parseTypeDef
  (m,t) <- ws *> parseExpDef
  symbol ";"
  if( m == n )
  then return $ Def n ty t
  else error "Definition name and expression name do not match"
            
parseFile = ws *> many parseDef

runParseFile :: String -> Either String GFile
runParseFile s = case (parse parseFile "" s) of
                Left msg -> Left $ show msg
                Right l -> return $ (fromList l)      

runParse :: FilePath -> IO(Either String GFile)
runParse path = do
    s <- readFile path
    return $ runParseFile s

------------------------------------------------------------------------                 
--                  Parsers for the REPL                              --
------------------------------------------------------------------------        

data REPLExpr =
   Let Vnm Term                 -- Toplevel let-expression: for the REPL
 | TypeCheck Term               -- Typecheck a term
 | ShowAST Term                 -- Show a terms AST
 | DumpState                    -- Trigger to dump the state for debugging.
 | Unfold Term                  -- Unfold the definitions in a term for debugging.
 | LoadFile String
 deriving Show
                    
letParser = do
  reservedOp "let"
  ws
  n <- varName
  ws
  symbol "="
  ws
  t <- expr <?> "Failed expr"
  eof <?> "Failed eof "++(runPrettyTerm t)
  return $ Let n t        

fileParser = do
  reservedOp ":l"
  path <- many1 anyChar
  eof  
  return $ LoadFile path
  
replTermCmdParser short long c p = do
  symbol ":"
  cmd <- many lower
  ws
  t <- p       
  eof
  if (cmd == long || cmd == short)
  then return $ c t
  else fail $ "Command \":"++cmd++"\" is unrecognized."

replIntCmdParser short long c = do
  symbol ":"
  cmd <- many lower
  eof
  if (cmd == long || cmd == short)
  then return c
  else fail $ "Command \":"++cmd++"\" is unrecognized."       
                 
typeCheckParser = replTermCmdParser "t" "type" TypeCheck expr

showASTParser = replTermCmdParser "s" "show" ShowAST expr

unfoldTermParser = replTermCmdParser "u" "unfold" Unfold expr                

dumpStateParser = replIntCmdParser "d" "dump" DumpState
               
lineParser = try letParser <|> try typeCheckParser <|> try showASTParser <|> try unfoldTermParser <|> try dumpStateParser

parseLine :: String -> Either String REPLExpr
parseLine s = case (parse lineParser "" s) of
                Left msg -> Left $ show msg
                Right l -> Right l