{-# LANGUAGE TemplateHaskell #-}

module TestLib (testLib) where

import Types
    ( User(..)
    , Group(..)
    )

import Util
    ( GroupNameWrapper(..)
    , UserWrapper(..)
    , GroupWrapper(..)
    , GroupListWrapper(..)
    )

import Lib
    ( objFnGroup
    , objFnAll
    )

import Test.QuickCheck (Positive(..), Property, (==>))
import Test.QuickCheck.All (quickCheckAll)

prop_objFnGroupHasResultOfZeroForSingletonGroup :: GroupNameWrapper -> UserWrapper -> Bool
prop_objFnGroupHasResultOfZeroForSingletonGroup gnw uw = objFnGroup g == 0
    where g = Group (unwrapGroupName gnw) [unwrapUser uw]

prop_objFnGroupHasResultOfZeroForHomogeneousGroup :: Positive Int -> GroupNameWrapper -> UserWrapper -> Bool
prop_objFnGroupHasResultOfZeroForHomogeneousGroup n gnw uw = objFnGroup g == 0
    where u = unwrapUser uw
          g = Group (unwrapGroupName gnw) $ replicate (getPositive n) u

prop_objFnGroupHasResultOf3ForMaximallyDiverseUsers :: UserWrapper -> UserWrapper -> GroupNameWrapper -> Property
prop_objFnGroupHasResultOf3ForMaximallyDiverseUsers (UserWrapper u1@(User _ g1 ce1 co1)) (UserWrapper u2@(User _ g2 ce2 co2)) gnw =
    u1 /= u2 && g1 /= g2 && ce1 /= ce2 && co1 /= co2 ==> objFnGroup g == 3
    where g = Group (unwrapGroupName gnw) [u1, u2]

prop_objFnGroupHasNonNegativeResult :: GroupWrapper -> Bool
prop_objFnGroupHasNonNegativeResult gw = objFnGroup (unwrapGroup gw) >= 0

prop_objFnAllSumsOverGroup :: GroupListWrapper -> Bool
prop_objFnAllSumsOverGroup glw = objFnAll xs == sum (map objFnGroup xs)
    where xs = unwrapGroupList glw

return []

testLib :: IO Bool
testLib = $quickCheckAll
