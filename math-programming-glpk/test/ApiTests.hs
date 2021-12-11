module ApiTests where

import Math.Programming.Glpk
import Math.Programming.Tests
import Test.Tasty

test_tree :: TestTree
test_tree = makeAllTests "GLPK" runGlpk
