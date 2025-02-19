import LambdaTerms
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "LambdaTerm AlphaEq" $ do
    it "simple variable equality" $ do
      alphaEq (Var "x") (Var "x") `shouldBe` True
      alphaEq (Var "x") (Var "y") `shouldBe` False

    it "alpha equivalence of abstractions" $ do
      alphaEq (Abstraction "x" (Var "x")) (Abstraction "y" (Var "y")) `shouldBe` True
      alphaEq (Abstraction "x" (Var "y")) (Abstraction "z" (Var "y")) `shouldBe` True
      alphaEq (Abstraction "x" (Var "x")) (Abstraction "y" (Var "x")) `shouldBe` False

    it "application equivalence" $ do
      let xy = Application (Var "x") (Var "y")
      let xz = Application (Var "x") (Var "z")
      alphaEq xy xy `shouldBe` True
      alphaEq xy xz `shouldBe` False

    it "complex expressions" $ do
      alphaEq
        (Abstraction "x" (Application (Var "x") (Var "y")))
        (Abstraction "z" (Application (Var "z") (Var "y")))
        `shouldBe` True
