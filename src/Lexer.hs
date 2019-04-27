{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error (errorBundlePretty)
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
-- import Control.Applicative.Combinators
import Data.Text (Text)
import Data.Bifunctor (bimap)
import qualified Data.Text as T
import Data.Void (Void)

type Parser = Parsec Void Text

underscore :: Parser Char
underscore = single '_'


newtype Identifier = Identifier Text
  deriving Show

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


spaceconsumer :: Parser ()
spaceconsumer = L.space
     space1
     (L.skipLineComment "//")
     (L.skipBlockCommentNested "/*" "*/")

lexeme = L.lexeme spaceconsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceconsumer

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser Text
stringLiteral = T.pack <$> ( char '\"' *> manyTill L.charLiteral (char '\"') )

integerLiteral :: Parser Int
integerLiteral = lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")


-- |   Expressions

data Variable = Variable Identifier ( Maybe TypeName ) Expr
  deriving Show

data TyDec = TyDec Identifier Ty
  deriving Show

data Declaration
  = DVariable Variable
  | DTyDec TyDec
  deriving Show


data Expr
  = Function Identifier [ TyField ] ( Maybe TypeName ) Expr
  | Nil
  | StringLiteral T.Text
  | IntLiteral Int
  | Sequence [ Expr ]
  | FunctionCall Identifier [ Expr ]
  | Arithmetic AExpr
  | Comparison Expr CompOp Expr
  | Let [ Declaration ] Expr
  | IfElse Expr Expr Expr
  | Assignment LValue Expr
  | While Expr Expr
  | For Identifier Expr Expr Expr
  | LValueE LValue
  deriving Show



-- |     Type definitions

newtype TypeName = TypeName Identifier 
  deriving Show

data TyField
  = TyField Identifier TypeName
  deriving Show

data Ty
  = TypeId TypeName
  | TypeRecord [ TyField ]
  | TypeArray TypeName
  deriving Show


parseBuiltIn :: Parser Ty
parseBuiltIn = TypeId . TypeName . Identifier <$> ( string "int" <|> string "string" )


parseRecord :: Parser Ty
parseRecord = TypeRecord <$> between (char '{') (char '}') (sepBy1 tyField (lexeme $ char ','))


tyField :: Parser TyField
tyField = do
  id <- lexeme identifier
  lexeme $ char ':'
  type' <- lexeme typeId
  return $ TyField id type'
      

parseArray :: Parser Ty
parseArray = do
  lexeme "array"
  lexeme "of"
  
  TypeArray <$> typeId


typeId :: Parser TypeName
typeId = TypeName <$> identifier


ty :: Parser Ty
ty = choice [ parseBuiltIn, parseRecord, parseArray ]


tydecParser :: Parser TyDec
tydecParser = do
  _ <- string "type"
  spaceconsumer
  id <- identifier
  spaceconsumer
  _ <- string "="
  spaceconsumer
  ty' <- ty

  return $ TyDec id ty'


-- | Variables

variableParser :: Parser Variable
variableParser = do
  _ <- lexeme $ string "var"
  id <- lexeme identifier
  type' <- optional $ try $ ( lexeme $ string ":" ) *> (lexeme typeId)
  _ <- lexeme $ string ":="
  expr <- exprParser
  return $ Variable id type' expr



-- | Functions

paramParser :: Parser [ TyField ]
paramParser = parens (sepBy1 tyField (lexeme $ char ','))


functionParser :: Parser Expr
functionParser = do
  _ <- lexeme $ string "function"
  id <- lexeme identifier
  params <- paramParser
  expr <- exprParser

  return $ Function id params Nothing expr
  


-- |

data LValue
  = Value Identifier
  | RecordLookup Identifier LValue 
  | ArrElem Identifier Expr
  deriving Show

-- | Not quite there
-- Cant do a.x.e[3].s
lvalueParser :: Parser LValue
lvalueParser =  choice [ try arrElem
                       , try recordLookup
                       , value
                       ]
  where
    arrElem = do
      lvalue <- lexeme identifier
      _ <- lexeme $ char '['
      expr <- exprParser
      _ <- lexeme $ char ']'

      return $ ArrElem lvalue expr

    recordLookup = do
      lvalue <- lexeme identifier
      char '.'
      id <- lvalueParser
      return $ RecordLookup lvalue id

    value = do
      id <- lexeme identifier
      return $ Value id
      

-- | Nil

nilParser :: Parser Expr
nilParser =
  lexeme $ string "nil" *> return Nil


-- | Sequence

sequenceParser :: Parser Expr
sequenceParser =
  Sequence <$> parens ( sepBy1 exprParser (lexeme $ char ';'))


-- | Arithmetic

data ArithOp
  = Plus
  | Minus
  | Times
  | Divide
  deriving Show

data AExpr
  = Var LValue
  | IntConst Int
  | Neg AExpr
  | ABinary ArithOp AExpr AExpr
  deriving (Show)

arithmeticOperators :: [[Operator Parser AExpr]]
arithmeticOperators =
  [ [Prefix (Neg <$ symbol "-") ]
  , [ InfixL (ABinary Times  <$ symbol "*")
    , InfixL (ABinary Divide <$ symbol "/") ]
  , [ InfixL (ABinary Plus   <$ symbol "+")
    , InfixL (ABinary Minus  <$ symbol "-") ]
  ]

arithmeticTerm :: Parser AExpr
arithmeticTerm = parens aExpr
                 <|> Var      <$> lvalueParser
                 <|> IntConst <$> integerLiteral



aExpr :: Parser AExpr
aExpr = makeExprParser arithmeticTerm arithmeticOperators

arithmeticParser :: Parser Expr
arithmeticParser = 
  Arithmetic <$> aExpr

  
-- | Comparison

data CompOp
  = EQ_
  | NE_
  | LT_
  | GT_
  | LE_
  | GE_
  deriving Show


comparisonParser :: Parser Expr
comparisonParser =
  Comparison <$> exprParser <*> opParser <*> exprParser
  where
    opParser = choice [ EQ_ <$ ( lexeme $ string "=" )
                      , NE_ <$ ( lexeme $ string "<>" )
                      , LT_ <$ ( lexeme $ string "<" )
                      , GT_ <$ ( lexeme $ string ">" )
                      , LE_ <$ ( lexeme $ string "<=" )
                      , GE_ <$ ( lexeme $ string ">=" )
                      ]


-- | Function call

functionCallParser :: Parser Expr
functionCallParser = do
  id <- lexeme identifier
  params <- parens $ sepBy exprParser (char ',')

  return $ FunctionCall id params


-- | Let

letParser :: Parser Expr
letParser = do
  rword "let"
  decs <- many $ (DVariable <$> variableParser) <|> (DTyDec <$> tydecParser)
  rword "in"
  expr <- exprParser
  rword "end"

  return $ Let decs expr


-- | If then else
  
ifParser :: Parser Expr
ifParser = do
  rword "if"
  exprCond <- exprParser
  rword "then"
  exprThen <- exprParser
  rword "else"
  exprElse <- exprParser

  return $ IfElse exprCond exprThen exprElse
  

-- | Assignment

assignmentParser :: Parser Expr
assignmentParser = do
  lvalue <- lvalueParser
  lexeme ":="
  expr <- exprParser

  return $ Assignment lvalue expr


-- | While

whileParser :: Parser Expr
whileParser = do
  rword "while"
  cond <- exprParser
  rword "do"
  expr <- exprParser

  return $ While cond expr


-- | For
forParser :: Parser Expr
forParser = do
  rword "for"
  id <- identifier
  lexeme ":="
  from <- exprParser
  rword "to"
  to <- exprParser
  rword "do"
  expr <- exprParser

  return $ For id from to expr

-- | Literals

literalParser :: Parser Expr
literalParser = 
  ( StringLiteral <$> stringLiteral ) <|> ( IntLiteral <$> integerLiteral )

exprParser :: Parser Expr
exprParser = choice [ try functionParser <?> "function"
                    , try nilParser <?> "nil"
                    , try sequenceParser <?> "sequence"
                    , try functionCallParser <?> "function call"
                    , try arithmeticParser <?> "arithmetic"
                    -- , try comparisonParser
                    , try letParser <?> "let"
                    , try ifParser <?> "if"
                    , try assignmentParser <?> "assignment"
                    , try whileParser <?> "while"
                    , try forParser <?> "for"
                    , try literalParser <?> "literal"
                    ]

programParser :: Parser Expr
programParser = between spaceconsumer eof exprParser
  
parseText :: Text -> String -> Either T.Text Expr
parseText contents filename =
  bimap (T.pack . errorBundlePretty) id $ parse programParser filename contents
