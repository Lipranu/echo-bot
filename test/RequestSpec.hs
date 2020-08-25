module RequestSpec ( spec ) where

-- IMPORTS ---------------------------------------------------------------------

import Request

import Test.Hspec               ( Spec, context, describe, it, shouldBe )
import Test.Hspec.QuickCheck    ( prop )
import Test.QuickCheck          ( Arbitrary (..), elements )

-- TYPES AND INSTANCES ---------------------------------------------------------



-- FUNCTIONS -------------------------------------------------------------------



-- TESTS -----------------------------------------------------------------------

spec :: Spec
spec = describe "init" $ it "test" $
  1 + 1 `shouldBe` 2
