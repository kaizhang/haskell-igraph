import qualified Test.Basic     as Basic
import qualified Test.Motif     as Motif
import qualified Test.Structure as Structure
import           Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Haskell-igraph Tests"
    [ Basic.tests
    , Structure.tests
    , Motif.tests
    ]
