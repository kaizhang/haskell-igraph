import qualified Test.Algorithms as Algorithms
import qualified Test.Attributes as Attributes
import qualified Test.Basic      as Basic
import           Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Haskell-igraph Tests"
    [ Basic.tests
    , Algorithms.tests
    , Attributes.tests
    ]
