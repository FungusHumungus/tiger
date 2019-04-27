-- | Chapter 1

module Lib
    ( run
    ) where

import qualified Data.Map as Map
import Control.Monad (void)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Lexer as L
import System.Environment (getArgs)

type Id = String

data BinOp = Plus
           | Minus
           | Times
           | Div


data Stm = CompoundStm Stm Stm
         | AssignStm Id Exp
         | PrintStm [Exp]


data Exp = IdExp Id
         | NumExp Int
         | OpExp Exp BinOp Exp
         | EseqExp Stm Exp
         
prog :: Stm
prog = CompoundStm (AssignStm "a" (OpExp (NumExp 5) Plus (NumExp 3)))
                   (CompoundStm
                     (AssignStm "b" (EseqExp (PrintStm [ IdExp "a"
                                                       , OpExp (IdExp "a") Minus (NumExp 1)
                                                       ])
                                      (OpExp (NumExp 10) Times (IdExp "a"))))
                     (PrintStm [ IdExp "b" ]))

maxArgsExp :: Exp -> Int
maxArgsExp ( OpExp a _ b ) = max (maxArgsExp a) (maxArgsExp b)
maxArgsExp ( EseqExp a b) = max (maxArgs a) (maxArgsExp b)
maxArgsExp _ = 0

maxArgs :: Stm -> Int
maxArgs ( CompoundStm a b ) = max (maxArgs a) (maxArgs b)
maxArgs ( AssignStm _ exp ) = maxArgsExp exp
maxArgs ( PrintStm args ) = max (length args) ( foldl max 0 $ maxArgsExp <$> args)
  

type Table = Map.Map Id Int

interpStm :: Table -> Stm -> IO Table
interpStm table ( CompoundStm a b ) = do
  table' <- interpStm table a
  interpStm table' b

interpStm table ( AssignStm id exp ) = do
  ( table', res ) <- interpExp table exp
  return (Map.insert id res table') 
  
interpStm table ( PrintStm exps ) =
  go table exps
  where
    go table [] = do
      putStr "\n"
      return table
      
    go table (x: xs) = do
      (table', res) <- interpExp table x
      putStr $ show res <> " "
      go table' xs
  

interpExp :: Table -> Exp -> IO (Table, Int)
interpExp table (IdExp i) = return (table, case Map.lookup i table of
                                             Nothing -> error $ "Invalid variable " <> i
                                             Just val -> val)
interpExp table (NumExp i) = return (table, i)
interpExp table (OpExp a op b) = do
  (table', a') <- interpExp table a
  (table'', b') <- interpExp table' b
  case op of
    Plus -> return (table'', a' + b')
    Minus -> return (table'', a' - b')
    Times -> return (table'', a' * b')
    Div -> return (table'', a' `div` b')

interpExp table (EseqExp stm exp) = do
  table' <- interpStm table stm
  interpExp table' exp


interp :: Stm -> IO ()  
interp stm = 
  void $ interpStm Map.empty stm 
                                             
run :: IO ()
run = do
  args <- getArgs
  let file = case args of
               [] -> "test.tig"
               (x:_) -> x

  contents <- TIO.readFile file

  putStrLn $ T.unpack contents

  case L.parseText contents file of
    Left err -> putStrLn $ T.unpack err
    Right expr -> putStrLn $ show expr
  
  

