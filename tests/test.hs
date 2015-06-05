import qualified Test.Basic as Basic
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Haskell-igraph Tests"
    [ Basic.tests
    ]
