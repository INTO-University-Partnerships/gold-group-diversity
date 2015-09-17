{-# LANGUAGE TemplateHaskell #-}

module TestLib (testLib, prop_splitCourseIntoGroupsOfSizeHasSameUsers) where

import Types
    ( User(..)
    , Group(..)
    )

import Util
    ( GroupNameWrapper(..)
    , UserWrapper(..)
    , GroupWrapper(..)
    , CourseWrapper(..)
    , GroupListWrapper(..)
    )

import Lib
    ( groupNames
    , objFnGroup
    , objFnAll
    , splitCourseIntoGroupsOfSize
    )

import Data.List (nub, sort)
import Test.QuickCheck (Positive(..), Property, (==>))
import Test.QuickCheck.All (quickCheckAll)

maxGroupSize :: Int
maxGroupSize = 100

prop_objFnGroupHasResultOfZeroForSingletonGroup :: GroupNameWrapper -> UserWrapper -> Bool
prop_objFnGroupHasResultOfZeroForSingletonGroup gnw uw = objFnGroup g == 0
    where g = Group (unwrapGroupName gnw) [unwrapUser uw]

prop_objFnGroupHasResultOfZeroForHomogeneousGroup :: Positive Int -> GroupNameWrapper -> UserWrapper -> Bool
prop_objFnGroupHasResultOfZeroForHomogeneousGroup n gnw (UserWrapper u) = objFnGroup g == 0
    where g = Group (unwrapGroupName gnw) $ replicate (getPositive n) u

prop_objFnGroupHasResultOf3ForMaximallyDiverseUsers :: GroupNameWrapper -> UserWrapper -> UserWrapper -> Property
prop_objFnGroupHasResultOf3ForMaximallyDiverseUsers gnw (UserWrapper u1@(User _ g1 ce1 co1)) (UserWrapper u2@(User _ g2 ce2 co2)) =
    constraints ==> objFnGroup group == 3
    where constraints = u1 /= u2 && g1 /= g2 && ce1 /= ce2 && co1 /= co2
          group       = Group (unwrapGroupName gnw) [u1, u2]

prop_objFnGroupHasNonNegativeResult :: GroupWrapper -> Bool
prop_objFnGroupHasNonNegativeResult gw = objFnGroup (unwrapGroup gw) >= 0

prop_objFnAllSumsOverGroup :: GroupListWrapper -> Bool
prop_objFnAllSumsOverGroup (GroupListWrapper xs) = objFnAll xs == sum (map objFnGroup xs)

prop_splitCourseIntoGroupsOfSizeGeneratesGroupNames :: CourseWrapper -> Positive Int -> Property
prop_splitCourseIntoGroupsOfSizeGeneratesGroupNames (CourseWrapper xs) (Positive groupSize) =
    constraints ==> names == take (length groups) groupNames
    where constraints = groupSize <= length xs && groupSize <= maxGroupSize
          groups      = splitCourseIntoGroupsOfSize groupSize xs
          names       = map (\(Group groupName _) -> groupName) groups

prop_splitCourseIntoGroupsOfSizeHasFixedSizedGroups :: CourseWrapper -> Positive Int -> Property
prop_splitCourseIntoGroupsOfSizeHasFixedSizedGroups (CourseWrapper xs) (Positive groupSize) =
    constraints ==> nub lengths == [groupSize]
    where constraints = groupSize < length xs && groupSize <= maxGroupSize
          groups      = splitCourseIntoGroupsOfSize groupSize xs
          lengths     = init $ map (\(Group _ xs') -> length xs') groups

prop_splitCourseIntoGroupsOfSizeHasLeftOverGroup :: CourseWrapper -> Positive Int -> Property
prop_splitCourseIntoGroupsOfSizeHasLeftOverGroup (CourseWrapper xs) (Positive groupSize) =
    constraints ==> length xs' > 0 && length xs' <= groupSize && length xs' == (length xs `mod` groupSize)
    where constraints   = groupSize <= length xs && groupSize <= maxGroupSize && (length xs `mod` groupSize > 0)
          (Group _ xs') = last $ splitCourseIntoGroupsOfSize groupSize xs

prop_splitCourseIntoGroupsOfSizeHasSameUsers :: CourseWrapper -> Positive Int -> Property
prop_splitCourseIntoGroupsOfSizeHasSameUsers (CourseWrapper xs) (Positive groupSize) =
    constraints ==> sort xs == sort (concatMap (\(Group _ xs') -> xs') groups)
    where constraints = groupSize <= length xs && groupSize <= maxGroupSize
          groups      = splitCourseIntoGroupsOfSize groupSize xs

return []

testLib :: IO Bool
testLib = $quickCheckAll
