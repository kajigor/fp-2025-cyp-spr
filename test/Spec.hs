import Test.Tasty ( defaultMain, testGroup )

import Test.List
import Test.RAList
import Test.Binary 

main :: IO ()
main =
  defaultMain $ testGroup "tests" 
    [ testList 
    , testBinary 
    , testRAList 
    ] 