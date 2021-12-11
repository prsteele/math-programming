module ApiSpec where

import Math.Programming.Glpk
import Math.Programming.Tests
import Test.Hspec

spec :: Spec
spec = makeAllTests "GLPK" runGlpk
