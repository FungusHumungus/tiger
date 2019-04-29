{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec
import Text.Megaparsec.Pos
import Text.Megaparsec.Debug
import Tiger.Parser


testParser parser msg text res =
  case parse (parser <* eof) "test" text of
    Left err -> assertFailure $ "Failed to parse " <> msg <> " -- " <> ( errorBundlePretty err )
    Right parsed -> assertEqual msg res parsed


testDbgParser parser msg text res =
  case parse (dbg "test" $ parser <* eof) "test" text of
    Left err -> assertFailure $ "Failed to parse " <> msg <> " -- " <> ( errorBundlePretty err )
    Right parsed -> assertEqual msg res parsed

  
sourcePos :: Int -> Int -> SourcePos
sourcePos col line = SourcePos { sourceName = "test"
                         , sourceColumn = mkPos col
                         , sourceLine = mkPos line
                         } 


main :: IO ()
main = defaultMain $ testGroup "parsing"
  [ testCase "Test integer literal" $
      testParser parseExp "integer literal" "42" $ IntExp 42

  , testCase "Test string literal" $
      testParser parseExp "string literal" "\"ooogleshnork\"" $
        StringExp "ooogleshnork" ( sourcePos 1 1 )


  , testCase "Parse variables" $ do
      testParser parseExp "simple variable" "a" $
        VarExp $ SimpleVar (Identifier "a") (sourcePos 1 1)

      testParser parseExp "field variable" "onk.oog" $
        VarExp $ FieldVar (SimpleVar (Identifier "onk") (sourcePos 1 1)) (Identifier "oog") (sourcePos 4 1)

      testParser parseExp "subscript variable" "a[42]" $
        VarExp $ SubscriptVar (SimpleVar (Identifier "a") (sourcePos 1 1)) (IntExp 42) (sourcePos 2 1)

      testParser parseExp "multi variable" "a.x[42]" $
        VarExp (SubscriptVar
                (FieldVar
                 (SimpleVar (Identifier "a") (sourcePos 1 1))
                 (Identifier "x")
                 (sourcePos 2 1))
                (IntExp 42)
                (sourcePos 4 1))

  , testCase "Parse array creation" $ do
      testParser parseExp "array creation" "oogle [ 42 ] of 0" $
        ArrayExp { typ = Identifier "oogle"
                 , size = IntExp 42
                 , init = IntExp 0
                 , pos = sourcePos 1 1
                 }

  , testCase "Parse calls" $ do
      testParser parseExp "calls" "print (\"onk\")" $
        CallExp { func = Identifier "print"
                , args = [ StringExp "onk" (sourcePos 8 1) ]
                , pos = sourcePos 1 1
                }

  , testCase "Parse sequence expressions" $ do
      testParser parseExp "sequences" "( 4; print(\"ook\") )" $
        SeqExp [ ( IntExp 4, sourcePos 3 1 )
               , ( CallExp { func = Identifier "print"
                           , args = [ StringExp "ook" (sourcePos 12 1) ]
                           , pos = sourcePos 6 1
                           }, sourcePos 6 1 )
               ]

      testParser parseExp "sequences" "(4)" $
        SeqExp [ ( IntExp 4, sourcePos 2 1 ) ]

  , testCase "Parse assignment" $ do
      testParser parseExp "assignment" "x := 32" $
        AssignExp { var = SimpleVar (Identifier "x") (sourcePos 1 1)
                  , exp = IntExp 32
                  , pos = sourcePos 1 1
                  }

      testParser parseExp "array assignment" "x[23] := ook.onk" $
        AssignExp { var = SubscriptVar
                          (SimpleVar (Identifier "x") (sourcePos 1 1))
                          (IntExp 23)
                          (sourcePos 2 1)
                  , exp = VarExp $ (FieldVar
                                    (SimpleVar (Identifier "ook") (sourcePos 10 1))
                                    (Identifier "onk")
                                    (sourcePos 13 1))
                  , pos = sourcePos 1 1
                  }


  , testCase "Parse arithmetic" $ do
      testParser parseExp "arithmetic" "x + y - 3" $
        OpExp { left = OpExp { left = VarExp ( SimpleVar ( Identifier "x" ) ( sourcePos 1 1))
                             , oper = PlusOp
                             , right = VarExp ( SimpleVar ( Identifier "y" ) ( sourcePos 5 1 ))
                             , pos = sourcePos 3 1
                             }
              , oper = MinusOp
              , right = IntExp 3
              , pos = sourcePos 7 1
              }

  , testCase "Parse if then else " $ do
      testParser parseExp "if" "if a > b then 4 else 2" $
        IfExp { test = OpExp { left = VarExp ( SimpleVar ( Identifier "a" ) ( sourcePos 4 1 ) )
                             , oper = GtOp
                             , right = VarExp ( SimpleVar ( Identifier "b" ) ( sourcePos 8 1 ) )
                             , pos = sourcePos 6 1
                             }
              , then' = IntExp 4
              , else' = Just $ IntExp 2
              , pos = sourcePos 1 1
              }

  , testCase "Parse for" $ do
      testParser parseExp "for" "for i := 0 to N do print (i) " $
        ForExp { vari = Identifier "i"
               , escape = True
               , lo = IntExp 0
               , hi = VarExp $ SimpleVar (Identifier "N") (sourcePos 15 1)
               , body = CallExp { func = Identifier "print"
                                , args = [ VarExp $ SimpleVar (Identifier "i") (sourcePos 27 1) ]
                                , pos = sourcePos 20 1
                                }
               , pos = sourcePos 1 1
               }
    
  ]
