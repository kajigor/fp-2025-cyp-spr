{-# LANGUAGE OverloadedStrings #-}

module StatementParserSpec where

import Data.Text (Text)
import Data.Void
import Expression
import Parser
import Statement
import Test.Hspec
import Test.Hspec.Megaparsec
import qualified Text.Megaparsec as MP

-- Parsing helper function for tests
parseTest :: Text -> Either (MP.ParseErrorBundle Text Void) Stmt
parseTest = MP.parse parseProgram ""

spec :: Spec
spec = do
  describe "Statement parser" $ do
    it "parses an assignment statement" $ do
      parseTest "x = 42;" `shouldParse` Assign "x" (IntLit 42)
      parseTest "foo = 100;" `shouldParse` Assign "foo" (IntLit 100)
      parseTest "bar = x;" `shouldParse` Assign "bar" (VarLit "x")

    it "parses an expression assignment" $ do
      parseTest "x = 5 + 3;" `shouldParse` Assign "x" (Sum (IntLit 5) (IntLit 3))
      parseTest "y = a - b;" `shouldParse` Assign "y" (Sub (VarLit "a") (VarLit "b"))
      parseTest "z = 10 * 20;" `shouldParse` Assign "z" (Prod (IntLit 10) (IntLit 20))

    it "parses a read statement" $ do
      parseTest "x = readInt();" `shouldParse` Read "x"
      parseTest "input = readInt();" `shouldParse` Read "input"

    it "parses a write statement" $ do
      parseTest "print(42);" `shouldParse` Write (IntLit 42)
      parseTest "print(x);" `shouldParse` Write (VarLit "x")
      parseTest "print(a + b);" `shouldParse` Write (Sum (VarLit "a") (VarLit "b"))

    it "parses a while statement" $ do
      parseTest "while (x) { print(x); };"
        `shouldParse` While (VarLit "x") (Write (VarLit "x"))

      parseTest "while (i - n) { i = i + 1; };"
        `shouldParse` While
          (Sub (VarLit "i") (VarLit "n"))
          (Assign "i" (Sum (VarLit "i") (IntLit 1)))

    it "parses sequence of statements" $ do
      parseTest "a = 1; b = 2;"
        `shouldParse` Seq (Assign "a" (IntLit 1)) (Assign "b" (IntLit 2))

    it "parses nested while loops" $ do
      parseTest "while (x) { while (y) { print(z); }; };"
        `shouldParse` While
          (VarLit "x")
          ( While
              (VarLit "y")
              (Write (VarLit "z"))
          )

    it "parses complex nested statements" $ do
      parseTest "x = 0; while (x - 10) { print(x); x = x + 1; };"
        `shouldParse` Seq
          (Assign "x" (IntLit 0))
          ( While
              (Sub (VarLit "x") (IntLit 10))
              ( Seq
                  (Write (VarLit "x"))
                  (Assign "x" (Sum (VarLit "x") (IntLit 1)))
              )
          )

    it "parses the fibonacci example program" $ do
      let fibProgram =
            "n = readInt(); \
            \a = 0; \
            \b = 1; \
            \i = 1; \
            \while ((n - i)) { \
            \  next = (a + b); \
            \  a = b; \
            \  b = next; \
            \  i = (i + 1); \
            \}; \
            \print(b);"

      parseTest fibProgram
        `shouldParse` Seq
          (Read "n")
          ( Seq
              (Assign "a" (IntLit 0))
              ( Seq
                  (Assign "b" (IntLit 1))
                  ( Seq
                      (Assign "i" (IntLit 1))
                      ( Seq
                          ( While
                              (Sub (VarLit "n") (VarLit "i"))
                              ( Seq
                                  (Assign "next" (Sum (VarLit "a") (VarLit "b")))
                                  ( Seq
                                      (Assign "a" (VarLit "b"))
                                      ( Seq
                                          (Assign "b" (VarLit "next"))
                                          (Assign "i" (Sum (VarLit "i") (IntLit 1)))
                                      )
                                  )
                              )
                          )
                          (Write (VarLit "b"))
                      )
                  )
              )
          )

    it "parses fibonacci program with newlines" $ do
      let fibProgramWithNewlines =
            "n = readInt();\n\
            \a = 0;\n\
            \b = 1;\n\
            \i = 1;\n\
            \while ((n - i)) {\n\
            \  next = (a + b);\n\
            \  a = b;\n\
            \  b = next;\n\
            \  i = (i + 1);\n\
            \};\n\
            \print(b);"

      parseTest fibProgramWithNewlines
        `shouldParse` Seq
          (Read "n")
          ( Seq
              (Assign "a" (IntLit 0))
              ( Seq
                  (Assign "b" (IntLit 1))
                  ( Seq
                      (Assign "i" (IntLit 1))
                      ( Seq
                          ( While
                              (Sub (VarLit "n") (VarLit "i"))
                              ( Seq
                                  (Assign "next" (Sum (VarLit "a") (VarLit "b")))
                                  ( Seq
                                      (Assign "a" (VarLit "b"))
                                      ( Seq
                                          (Assign "b" (VarLit "next"))
                                          (Assign "i" (Sum (VarLit "i") (IntLit 1)))
                                      )
                                  )
                              )
                          )
                          (Write (VarLit "b"))
                      )
                  )
              )
          )

    it "handles newlines after semicolons" $ do
      let programWithNewlines = "x = 1;\ny = 2;\nz = 3;"

      parseTest programWithNewlines
        `shouldParse` Seq
          (Assign "x" (IntLit 1))
          ( Seq
              (Assign "y" (IntLit 2))
              (Assign "z" (IntLit 3))
          )

    it "handles mixed formatting with multiple newlines" $ do
      let mixedProgram = "a = 10;\n\nb = 20;\n\n\nc = 30;"

      parseTest mixedProgram
        `shouldParse` Seq
          (Assign "a" (IntLit 10))
          ( Seq
              (Assign "b" (IntLit 20))
              (Assign "c" (IntLit 30))
          )

    it "fails for incomplete statements" $ do
      parseTest `shouldFailOn` "x = 42" -- missing semicolon
      parseTest `shouldFailOn` "print(42" -- missing closing paren and semicolon
    it "fails for invalid statements" $ do
      parseTest `shouldFailOn` "x + y = 42;" -- left side must be a variable
      parseTest `shouldFailOn` "while x { a = 1; };" -- missing parentheses around condition

main :: IO ()
main = hspec spec
