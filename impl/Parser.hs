{-# 
LANGUAGE 
  NoMonomorphismRestriction, 
  PackageImports, 
  TemplateHaskell, 
  FlexibleContexts 
#-}

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
  Token.reservedOpNames = ["x", "->", "0", "succ", "?", "triv",
                           "\\", "proj1", "proj2", "Nat", "Triv",
                           "box", "unbox", "sqsh", "split", "forall"]
}
tokenizer = Token.makeTokenParser lexer

ident      = Token.identifier tokenizer
reserved   = Token.reserved tokenizer
reservedOp = Token.reservedOp tokenizer
parens     = Token.parens tokenizer
ws         = Token.whiteSpace tokenizer
symbol     = Token.symbol tokenizer             

unexp msg = unexpected msg -- Used for error handing.

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
      when (p h || isNumber h) $ unexp (n++" : "++msg)
  return . s2n $ n

parseConst s c = symbol s >> return c

typeVarName = varName' isLower "Type variables must begin with an uppercase letter."
tvar = ws *> var' typeVarName TVar <* ws
         
tyNat = parseConst "Nat" Nat
tyU = parseConst "?" U
tyUnit = parseConst "Unit" Unit         
tyTop = parseConst "*" Top
        
prod = do
  symbol "("
  t1 <- typeParser
  symbol ","
  t2 <- typeParser
  symbol ")"
  return $ Prod t1 t2

forall = do
  reservedOp "forall"
  ws
  symbol "("
  v <- typeVarName
  ws
  symbol ":>"
  t1 <- typeParser
  ws
  symbol ")"
  symbol "."
  t2 <- typeParser
  return $ Forall t1 (bind v t2)

-- The initial expression parsing table for types.
table = [[binOp AssocRight "->" (\d r -> Arr d r)]]
binOp assoc op f = Text.Parsec.Expr.Infix (do{ ws;reservedOp op;ws;return f}) assoc
typeParser = ws *> buildExpressionParser table (ws *> typeParser')
typeParser' = try (parens typeParser) <|> tyNat <|> tyU <|> tyUnit <|> try tyTop
                                      <|> try forall <|> try prod <|> tvar

------------------------------------------------------------------------
-- Next the term parsers.                                             --
------------------------------------------------------------------------
aterm = try (parens pairParse) <|> parens expr    <|> try zeroParse 
                               <|> try trivParse  <|> try squash
                               <|> try split      <|> try boxParse
                               <|> try unboxParse <|> var
expr = ws *> (try funParse <|> tfunParse <|> succParse <|> fstParse    <|> sndParse
                           <|> tappParse <|> appParse  <|> parens expr <?> "parse error")

varName = varName' isUpper "Term variables must begin with a lowercase letter."
var = ws *> var' varName Var <* ws

zeroParse = parseConst "0" Zero
trivParse = parseConst "triv" Triv

tfunParse = do
  reservedOp "\\"
  symbol "("  
  v <- typeVarName
  ws
  symbol ":>"
  ty <- typeParser
  ws
  symbol ")"
  symbol "."
  t <- expr
  return $ TFun ty $ bind v t

tappParse = do
  symbol "["
  ty <- typeParser
  ws
  symbol "]"
  t <- expr
  return $ TApp ty t

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
  ws
  name <- varName
  ws
  symbol ":"
  ty <- typeParser
  ws
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
  ws
  ty <- typeParser
  return $ (Squash ty)
  
split = do
  reservedOp "split"
  ws
  ty <- typeParser
  return $ (Split ty)

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

replFileCmdParser short long c = do
  symbol ":"
  cmd <- many lower
  ws
  path <- many1 anyChar
  eof
  if(cmd == long || cmd == short)
  then return $ c path
  else fail $ "Command \":"++cmd++"\" is unrecognized."
  
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

loadFileParser = replFileCmdParser "l" "load" LoadFile
               
lineParser = try letParser <|> try loadFileParser <|> try typeCheckParser <|> try showASTParser <|> try unfoldTermParser <|> try dumpStateParser

parseLine :: String -> Either String REPLExpr
parseLine s = case (parse lineParser "" s) of
                Left msg -> Left $ show msg
                Right l -> Right l