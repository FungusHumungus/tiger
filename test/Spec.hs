{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec
import Text.Megaparsec.Pos
import Tiger.Parser


-- testParser :: Text -> 
testParser parser msg text res =
  case parse (parser <* eof) "test" text of
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
      testParser parseVar "simple variable" "a" $ SimpleVar (Identifier "a") (sourcePos 1 1)

      testParser parseVar "field variable" "onk.oog" $
        FieldVar (SimpleVar (Identifier "onk") (sourcePos 1 1)) (Identifier "oog") (sourcePos 4 1)

      testParser parseVar "subscript variable" "a[42]" $
        SubscriptVar (SimpleVar (Identifier "a") (sourcePos 1 1)) (IntExp 42) (sourcePos 2 1)

      testParser parseVar "multi variable" "a.x[42]" $
        (SubscriptVar
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


  ]
