{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Tiger.Parser where


import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Megaparsec.Debug (dbg)
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Data.Text (Text)
import Data.Bifunctor (bimap)
import qualified Data.Text as T
import Data.Void (Void)
import Debug.Trace

type Parser = Parsec Void Text


newtype Identifier = Identifier Text
  deriving (Eq, Show)


data Var
  = SimpleVar Identifier SourcePos
  | FieldVar Var Identifier SourcePos
  | SubscriptVar Var Exp SourcePos
  deriving (Eq, Show)


data Exp
  = VarExp Var
  | NilExp
  | IntExp Int
  | NegExp Exp
  | StringExp Text SourcePos
  | CallExp { func :: Identifier
            , args :: [ Exp ]
            , pos :: SourcePos
            }
  | OpExp { left :: Exp
          , oper :: Oper
          , right :: Exp
          , pos :: SourcePos
          }
  | RecordExp { fields :: [ ( Identifier, Exp, SourcePos ) ]
              , typ :: Identifier
              , pos :: SourcePos
              }
  | SeqExp [ ( Exp, SourcePos ) ]
  | AssignExp { var :: Var
              , exp :: Exp
              , pos :: SourcePos
              }
  | IfExp { test :: Exp
          , then' :: Exp
          , else' :: Maybe Exp
          , pos :: SourcePos
          }
  | WhileExp { test :: Exp
             , body :: Exp
             , pos :: SourcePos
             }
  | ForExp { vari :: Identifier
           , escape :: Bool -- ?
           , lo :: Exp
           , hi :: Exp
           , body :: Exp
           , pos :: SourcePos
           }
  | BreakExp SourcePos
  | LetExp { decs :: [ Dec ]
           , body :: Exp
           , pos :: SourcePos
           }
  | ArrayExp { typ :: Identifier
             , size :: Exp
             , init :: Exp
             , pos :: SourcePos
             }
  deriving (Eq, Show)


data Dec
  = FunctionDec [ FunDec ]
  | VarDec { name :: Identifier
           , escape :: Bool -- ?
           , typ :: Maybe ( Identifier, SourcePos )
           , init :: Exp
           , pos :: SourcePos
           }
  | TypeDec { name :: Identifier
            , ty :: Ty
            , pos :: SourcePos
            }
  deriving (Eq, Show)


data Ty
  = NameTy Identifier SourcePos
  | RecordTy [ Field ]
  | ArrayTy Identifier SourcePos
  deriving (Eq, Show)


data Field = Field { name :: Identifier
                   , escape :: Bool -- ?
                   , typ :: Identifier
                   , pos :: SourcePos
                   }
  deriving (Eq, Show)


data FunDec = FunDec { name :: Identifier
                     , params :: [ Field ]
                     , result :: Maybe ( Identifier, SourcePos )
                     , body :: Exp
                     , pos :: SourcePos
                     }
  deriving (Eq, Show)


data Oper
  = PlusOp
  | MinusOp
  | TimesOp
  | DivideOp
  | EqOp
  | NeqOp
  | LtOp
  | LeOp
  | GtOp
  | GeOp
  deriving (Eq, Show)

data AExpr
  = AVar Var
  | AConst Int
  | ANeg AExpr
  | ABinary Oper AExpr AExpr


spaceconsumer :: Parser ()
spaceconsumer = L.space
     space1
     (L.skipLineComment "//")
     (L.skipBlockCommentNested "/*" "*/")

lexeme = L.lexeme spaceconsumer


symbol :: Text -> Parser Text
symbol = L.symbol spaceconsumer

underscore :: Parser Char
underscore = single '_'

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser Text
stringLiteral = T.pack <$> ( lexeme $ char '\"' *> manyTill L.charLiteral (char '\"') )

integerLiteral :: Parser Int
integerLiteral = lexeme L.decimal <?> "integer"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "{") (symbol "}")

squareBrackets :: Parser a -> Parser a
squareBrackets = between (symbol "[") (symbol "]")


-- | Reserved words

rword :: Text -> Parser ()
rword w = ( lexeme . try ) ( string w *> notFollowedBy alphaNumChar )


rws :: [Text]
rws = [ "while", "for", "to", "break", "let", "in", "end"
      , "function", "var", "type", "array", "if", "then"
      , "else", "do", "of", "nil"
      ]


-- | Start with a letter and then follow with a sequence of letters, digets and underscores
identifier :: Parser Identifier
identifier =
  (lexeme . try) ( p >>= check )

  where
    p = do
      start <- T.singleton <$> letterChar
      rest <- T.pack <$> many (alphaNumChar <|> underscore)
      return $ Identifier $ start <> rest 

    check :: Identifier -> Parser Identifier
    check (Identifier x) = if x `elem` rws
                           then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                           else return $ Identifier x


-- | Parse the expression, whatever kind of expression it is.
parseExp :: Parser Exp
parseExp =  ( parseNilExp <?> "nil" ) <|>
            ( parseForExp <?> "for" ) <|>
            ( parseIfExp <?> "if" ) <|>
            ( parseWhileExp <?> "while" ) <|>
            ( parseBreakExp <?> "break" ) <|>
            ( parseSeqExp <?> "seq" ) <|>
            ( parseLetExp <?> "let" ) <|>
            ( try parseIntExp <?> "int" ) <|>
            ( try parseVar <?> "var" ) <|>
            ( try parseStringExp <?> "string" ) <|>
            ( try parseRecordExp <?> "record" ) <|>
            ( try parseArrayExp <?> "array" ) <|>
            ( try parseCallExp <?> "call" ) <|>
            ( try parseAssignExp <?> "assign" ) <|>
            ( try parseOpExp <?> "op" )  

tyFieldParser :: Parser Field
tyFieldParser = do
  pos <- getSourcePos
  name <- identifier <?> "name"
  symbol ":"
  typ <- identifier <?> "type"

  return $ Field { name, escape = False, typ, pos }



-- | A declaration.
-- | Can be a function, a variable or a type
parseDec :: Parser Dec
parseDec = ( FunctionDec <$> some parseFunctionDec ) <|>
           ( parseVarDec ) <|>
           ( parseTypeDec )

  where
    parseType = do
      symbol ":"
      pos <- getSourcePos
      result <- identifier

      return (result, pos)
      
    parseFunctionDec :: Parser FunDec
    parseFunctionDec = do
      pos <- getSourcePos
      rword "function"
      name <- lexeme identifier <?> "function name"
      params <- parens $ sepBy tyFieldParser ( symbol "," )
      result <- optional parseType
      symbol "="
      body <- parseExp

      return $ FunDec { name, params, result, body, pos }
      
    parseVarDec :: Parser Dec
    parseVarDec = do
      pos <- getSourcePos
      rword "var"
      name <- identifier <?> "variable name"
      typ <- optional $ try ( parseType <?> "type" )
      symbol ":="
      init <- parseExp

      return $ VarDec { name, escape = True, typ, init, pos }

    parseRecordTy :: Parser Ty
    parseRecordTy = brackets $ do
      fields <- sepBy tyFieldParser ( symbol "," )
      return $ RecordTy fields

    parseArrayTy :: Parser Ty
    parseArrayTy = do
      rword "array"
      rword "of"
      pos <- getSourcePos
      ty <- identifier <?> "array type"

      return $ ArrayTy ty pos
      
    parseNameTy :: Parser Ty
    parseNameTy = do
      pos <- getSourcePos
      name <- identifier
      return $ NameTy name pos
      
    parseTypeDec :: Parser Dec
    parseTypeDec = do
      pos <- getSourcePos
      rword "type"
      name <- identifier
      symbol "="
      ty <- parseRecordTy <|> parseArrayTy <|> parseNameTy

      return $ TypeDec { name, ty, pos }
      
      
-- | A variable
-- x.y[3].z[2][9]
variableParser :: Parser Var
variableParser = do
  pos <- getSourcePos
  var <- parseSimpleVar pos <?> "Simple var"
  return var
  -- go var

  where
    
    go :: Var -> Parser Var
    go var = do
      pos <- getSourcePos
      next <- optional $
              try ( parseFieldVar var pos <?> "field var" ) <|>
              try ( parseSubscriptVar var pos <?> "subscript var" )
      case next of
        Nothing -> return var -- There is nothing more to this var
        Just var' -> go var'  -- There is more, continue parsing

    parseFieldVar :: Var -> SourcePos -> Parser Var
    parseFieldVar var pos = do
      symbol "."
      name <- identifier  
      return $ FieldVar var name pos

    parseSubscriptVar :: Var -> SourcePos -> Parser Var
    parseSubscriptVar var pos = do
      exp <- squareBrackets $ parseExp
      return $ SubscriptVar var exp pos

    parseSimpleVar :: SourcePos -> Parser Var
    parseSimpleVar pos = do
      name <- identifier

      return $ SimpleVar name pos


parseVar :: Parser Exp
parseVar = VarExp <$> variableParser

-- | nil
parseNilExp :: Parser Exp
parseNilExp = NilExp <$ rword "nil"


-- | An integer literal
-- 334
parseIntExp :: Parser Exp
parseIntExp = IntExp <$> integerLiteral


-- | Parse a string literal
-- "onk wonk \"shnork\" flook"
parseStringExp :: Parser Exp
parseStringExp = do
  pos <- getSourcePos
  str <- stringLiteral
  return $ StringExp str pos


-- | A function call
-- onk ( 43, (2 + x) )
parseCallExp :: Parser Exp
parseCallExp = do
  pos <- getSourcePos
  func <- identifier <?> "function name"
  args <- parens $ sepBy parseExp (char ',')

  return $ CallExp { func, args, pos }
  

parseOp :: Oper -> SourcePos -> Exp -> Exp -> Exp
parseOp oper pos left right = 
  OpExp { left, oper, right, pos }


binary :: Text -> (SourcePos -> a -> a -> a) -> Operator Parser a
binary name f = InfixL $ do
  pos <- getSourcePos
  f pos <$ symbol name
  

-- | Set up the precedence table for parsing operators.
arithmeticOperators :: [[Operator Parser Exp]]
arithmeticOperators =
  [ [ Prefix (NegExp <$ symbol "-") ]
  , [ binary "*" (parseOp TimesOp)
    , binary "/" (parseOp DivideOp)
    ]
  , [ binary "+" (parseOp PlusOp)
    , binary "-" (parseOp MinusOp)
    ]
  , [ binary "=" (parseOp EqOp)
    , binary "/=" (parseOp NeqOp)
    , binary "<" (parseOp LtOp)
    , binary "<=" (parseOp LeOp)
    , binary ">" (parseOp GtOp)
    , binary ">=" (parseOp GeOp)
    ]
  ]

arithmeticTerm :: Parser Exp
arithmeticTerm = parens parseOpExp
                 <|> VarExp <$> ( variableParser <?> "variable" )
                 <|> IntExp <$> ( integerLiteral <?> "integer" )


parseOpExp :: Parser Exp
parseOpExp = makeExprParser arithmeticTerm arithmeticOperators


-- | Create a record
-- ook { ponk = 3, onk = "flork }
parseRecordExp :: Parser Exp
parseRecordExp = brackets $ do
  pos <- getSourcePos
  typ <- identifier <?> "record name"
  fields <- sepBy parseAssignment (symbol ",")

  return $ RecordExp { fields, typ, pos }

  where
    parseAssignment = do
      pos <- getSourcePos
      id <- identifier <?> "record field"
      symbol "="
      exp <- parseExp <?> "record field value"

      return ( id, exp, pos )

-- | Parse array creation
-- mylist [3] of int
parseArrayExp :: Parser Exp
parseArrayExp = do
  pos <- getSourcePos
  typ <- identifier <?> "array name"
  size <- squareBrackets parseExp
  rword "of"
  init <- parseExp <?> "array init"

  return $ ArrayExp { typ, size, init, pos }


-- | A sequence of expressions
-- ( x := 4; print "ook" )
parseSeqExp :: Parser Exp
parseSeqExp = parens $ do
  traceM "Seq"
  pos <- getSourcePos
  exps <- sepBy expParser' $ symbol ";"

  return $ SeqExp exps

  where
    expParser' = do
      pos <- getSourcePos
      exp <- parseExp
      return (exp, pos)


-- | Assignment
parseAssignExp :: Parser Exp
parseAssignExp = do
  pos <- getSourcePos

  var <- variableParser <?> "variable"
  symbol ":="
  exp <- parseExp

  return $ AssignExp { var, exp, pos }
  

-- | if blah then blah else blah
parseIfExp :: Parser Exp
parseIfExp = do
  pos <- getSourcePos
  rword "if"
  test <- parseExp <?> "test"
  rword "then"
  then' <- parseExp <?> "then"
  else' <- optional $ rword "else" >> ( parseExp <?> "else" )

  return $ IfExp { test, then', else', pos }
                         
  
-- | while blah do blah
parseWhileExp :: Parser Exp
parseWhileExp = do
  pos <- getSourcePos
  rword "while"
  test <- parseExp
  rword "do"
  body <- parseExp

  return $ WhileExp { test, body, pos }


parseForExp :: Parser Exp
parseForExp = do
  pos <- getSourcePos
  rword "for"
  vari <- identifier <?> "for variable"
  symbol ":="
  lo <- parseExp <?> "lo"
  rword "to"
  hi <- parseExp <?> "hi"
  rword "do"
  body <- parseExp

  return $ ForExp { vari, escape = True, lo, hi, body, pos }


parseBreakExp :: Parser Exp
parseBreakExp =
  rword "break" *> ( BreakExp <$> getSourcePos ) 


-- | let stuff in blah
parseLetExp  :: Parser Exp
parseLetExp = do
  pos <- getSourcePos
  rword "let"
  decs <- many parseDec 
  rword "in"
  body <- parseExp
  rword "end"

  return $ LetExp { decs, body, pos }
  
  
programParser :: Parser Exp
programParser = between spaceconsumer eof parseExp
  

parseText :: Text -> String -> Either T.Text Exp
parseText contents filename =
  bimap (T.pack . errorBundlePretty) id $ parse programParser filename contents


ook = parseTest ((parseExp <?> "var") <* eof ) "oogle [ 42 ] of 0"

onk = parseTest ((parseVar <?> "var") <* eof ) "oogle [ 42 ] of 0"
