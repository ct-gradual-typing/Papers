{-# 
LANGUAGE 
  NoMonomorphismRestriction, 
  PackageImports, 
  TemplateHaskell, 
  FlexibleContexts 
#-}

module Surface.Parser (module Text.Parsec, expr, 
               Vnm, 
               letParser, 
               lineParser, 
               REPLExpr(..), 
               parseLine, 
               runFileParser, 
               GFile, 
               Prog(..)) where

import Prelude
import Data.List
import Data.Char 
import qualified Data.Text as T
import Text.Parsec hiding (Empty)
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language
import Control.Monad -- For debugging messages.
import Data.Functor.Identity
import Text.Parsec.Extra
import System.FilePath
import System.Directory

import Surface.Syntax
import Queue
import Surface.Pretty

------------------------------------------------------------------------
-- We first setup the lexer.                                          --
------------------------------------------------------------------------
lexer = haskellStyle {
  Token.reservedNames   = ["of","0","?","triv","proj1","proj2","forall",
                           "ncase","box","unbox","Nat","Unit", "||", "[]", ":", "lcase"],
  Token.reservedOpNames = ["->", "succ", "\\","box","unbox", "proj1", "proj2", "forall", "ncase", ":", "lcase"]
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
  n <- ident  
  return . s2n $ n

parseConst s c = symbol s >> return c

typeVarName = varName' isLower "Type variables must begin with an uppercase letter."
tvar = ws *> var' typeVarName TVar <* ws
         
tyNat = parseConst "Nat" Nat
tyU = parseConst "?" U
tyUnit = parseConst "Unit" Unit         
        
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
  symbol "<:"
  t1 <- typeParser
  ws
  symbol ")"
  symbol "."
  t2 <- typeParser
  return $ Forall t1 (bind v t2)

list = do
  symbol "["
  ty <- typeParser  
  symbol "]"
  return $ List ty

tySimple = parseConst "Simple" Simple

-- The initial expression parsing table for types.
table = [[binOp AssocRight "->" (\d r -> Arr d r)]]
binOp assoc op f = Text.Parsec.Expr.Infix (do{ ws;reservedOp op;ws;return f}) assoc
typeParser = ws *> buildExpressionParser table (ws *> typeParser')
typeParser' = try (parens typeParser) <|> tyNat <|> tyU <|> tyUnit <|> try tySimple
                                      <|> try forall <|> try prod <|> try list <|> tvar

parseType :: String -> Either String Type
parseType s = case (parse typeParser "" s) of
                Left msg -> Left $ show msg
                Right l -> Right l

------------------------------------------------------------------------
-- Next the term parsers.                                             --
------------------------------------------------------------------------

int2term :: Integer -> Term
int2term 0 = Zero
int2term n = Succ $ int2term $ n-1

aterm = try (parens pairParse) <|> parens expr    <|> try intParse
                               <|> try trivParse  <|> try boxParse
                               <|> try unboxParse <|> try emptyListParse
                               <|> try listNParse <|> var                                
expr = ws *> (try funParse <|> tfunParse  <|> succParse <|> fstParse  <|> sndParse
                           <|> try caseParse <|> tappParse <|> try listParse <|> appParse <|> parens expr <?> "parse error")

varName = varName' isUpper "Term variables must begin with a lowercase letter."
var = ws *> var' varName Var <* ws

intParse = integer >>= return.int2term

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

tfunParse = do
  reservedOp "\\"
  symbol "("  
  v <- typeVarName
  ws
  symbol "<:"
  ty <- typeParser
  ws
  symbol ")"
  symbol "->"
  t <- expr
  return $ TFun ty $ bind v t

tappParse = try $ do
  symbol "["
  ty <- typeParser
  ws
  symbol "]"
  t <- expr
  return $ TApp ty t

succParse = do
  reservedOp "succ"
  t <- expr
  return $ Succ t
         
caseParse = do
  symbol "case"
  t <- expr
  ws
  reserved "of"
  ws
  try (ncaseParse t) <|> lcaseParse t  

ncaseParse t = do  
  symbol "0"
  symbol "->"  
  t1 <- expr 
  ws  
  symbol ","
  symbol "("
  symbol "succ"
  x <- varName 
  ws
  symbol ")"         
  symbol "->" 
  t2 <- expr
  return $ NCase t t1 (bind x t2)  

lcaseParse t = do
  symbol "[]"
  symbol "->"
  t1 <- expr 
  ws
  symbol ","
  symbol "("
  hv <- varName
  ws
  symbol "::"
  tv <- varName
  ws
  symbol ")"
  symbol "->"
  t2 <- expr
  return $ LCase t t1 (bind hv (bind tv t2))

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
  symbol "->"
  body <- expr
  return . Fun ty . bind name $ body

appParse = do
  l <- many (ws *> aterm)
  case l of
    [] -> fail "A term must be supplied"
    _ -> return $ foldl1 App l

getPos = do
  p <- getPosition
  return (sourceLine p, sourceColumn p, sourceName p)

listNParse = do
  symbol "["
  l <- aterm `sepBy1` (symbol ",")
  symbol "]"
  return $ case l of
    [] -> Empty
    _ -> foldr Cons Empty l

emptyListParse = do
  symbol "[]"
  return Empty

consParse = do
  lookAhead $ (aterm >> ws >> (symbol "::"))
  l <- aterm `sepBy1` (ws >> symbol "::")
  return $ case l of
    [] -> error "empty list"
    _ -> foldr1 Cons l

listParse = (try listNParse) <|> consParse

parseTerm :: String -> Either String Term
parseTerm s = case (parse expr "" s) of
                Left msg -> Left $ show msg
                Right l -> Right l
         
------------------------------------------------------------------------                 
-- Parsers for the Files                                              --
------------------------------------------------------------------------        

type TypeDef = (Vnm, Type)   
type ExpDef = (Vnm, Term)

data Prog = Def Vnm Type Term
  deriving Show

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

imports = try $ do
  symbol "import"
  fns <- many alphaNum
  symbol ";"
  return $ fns++".gry"

parseImports = ws *> many imports

parseFile = ws *> do
  _ <- many imports
  dfs <- many parseDef
  return dfs

runParseImports :: String -> Either String [String]
runParseImports s = case (parse parseImports "" s) of
                Left msg -> Left $ show msg
                Right is -> Right is

runParseFile :: String -> IO (Either String [Prog])
runParseFile s = case (parse parseFile "" s) of
                Left msg -> return $ Left $ show msg
                Right ds -> return $ Right ds

getImports :: FilePath -> FilePath -> IO(Either String [String])
getImports path wdir = do
    let file = wdir </> path
    b <- doesFileExist file
    if b
    then do
      s <- readFile file
      let is = runParseImports s
       in case is of
           Left msg -> return $ Left $ show msg
           Right ims -> let imswdir = map (wdir </>)  ims
                            x = map (\f -> getImports f wdir) ims
                         in do y <- gatherImports x
                               case y of
                                 Left m -> return $ Left $ show m
                                 Right rims -> return $ Right $ rims ++ ims
    else return $ Left $ file ++ " does not exist."
                                              
gatherImports :: [IO (Either String [String])] -> IO (Either String [String])
gatherImports [] = return $ Right []
gatherImports (x:xs) = do
  mi <- x
  rest <- gatherImports xs
  case (mi,rest) of
    (Left m1, Left m2) -> return $ Left $ m1 ++ "\n"++m2
    (Left m, _) -> return $ Left m
    (_, Left m) -> return $ Left m
    (Right im1, Right im2) -> return $ Right $ im1 ++ im2

runFileParser :: FilePath -> FilePath -> IO (Either String GFile)
runFileParser file wdir = do  
    mis <- getImports file wdir
    case mis of
      Left m -> return $ Left m
      Right is' -> do
               let is = is' ++ [file]
               let iswdir = map (wdir </>)  is
               let mds1 = map runFileParser' iswdir
               let mds2 = gatherDefs mds1
               mds <- mds2
               case mds of
                 Left m -> return $ Left m
                 Right ds -> return $ Right $ fromList ds

gatherDefs :: [IO (Either String [Prog])] -> IO (Either String [Prog])
gatherDefs [] = return $ Right []
gatherDefs (x:xs) = do
  d <- x
  rest <- gatherDefs xs
  case (d,rest) of
    (Left m1, Left m2) -> return $ Left $ m1 ++ m2
    (Left m, _) -> return $ Left m
    (_, Left m) -> return $ Left m
    (Right d1, Right d2) -> return $ Right $ d1 ++ d2

runFileParser' :: FilePath -> IO (Either String [Prog])
runFileParser' path = do
    b <- doesFileExist path
    if b
    then do
      s <- readFile path
      runParseFile s
    else return $ Left $ path ++ " does not exist."

------------------------------------------------------------------------                 
--                  Parsers for the REPL                              --
------------------------------------------------------------------------        

data REPLExpr =
   Let Vnm Term                 -- Toplevel let-expression: for the REPL
 | TypeCheck Term               -- Typecheck a term
 | ShowAST Term                 -- Show a terms AST
 | DumpState                    -- Trigger to dump the state for debugging.
 | Unfold Term                  -- Unfold the definitions in a term for debugging.
 | LoadFile String              -- Loading an external file into the context
 | Eval Term                    -- The defualt is to evaluate.
 deriving Show
                    
letParser = do
  reservedOp "let"
  ws
  n <- varName
  ws
  symbol "="
  ws
  t <- expr 
  eof
  return $ Let n t        

replFileCmdParser short long c = do
  symbol ":"
  cmd <- many lower
  ws
  pathUntrimmed <- many1 anyChar
  eof
  if(cmd == long || cmd == short)
  then do
    -- Trim whiteSpace from path
    let path = T.unpack . T.strip . T.pack $ pathUntrimmed
    return $ c path
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

evalParser = do
  t <- expr
  return $ Eval t

typeCheckParser = replTermCmdParser "t" "type" TypeCheck expr

showASTParser = replTermCmdParser "s" "show" ShowAST expr

unfoldTermParser = replTermCmdParser "u" "unfold" Unfold expr                

dumpStateParser = replIntCmdParser "d" "dump" DumpState

loadFileParser = replFileCmdParser "l" "load" LoadFile
              
lineParser = try letParser <|> try loadFileParser <|> try typeCheckParser <|> try showASTParser <|> try unfoldTermParser <|> try dumpStateParser <|> evalParser

parseLine :: String -> Either String REPLExpr
parseLine s = case (parse lineParser "" s) of
                Left msg -> Left $ show msg
                Right l -> Right l