{-# LANGUAGE TemplateHaskell #-}

module TestLib (testLib) where

import Types
    ( Gender(..)
    , User(..)
    , Group(..)
    , Switch(..)
    )

import Util
    ( GroupNameWrapper(..)
    , UserWrapper(..)
    , GroupWrapper(..)
    , HomoGroupWrapper(..)
    , CourseWrapper(..)
    , GroupListWrapper(..)
    )

import Lib
    ( diversifyElements
    , groupNames
    , objectiveFunction
    , anySwitches
    , switchPairOfElements
    , applySwitch
    , distributeElementsIntoGroups
    , getElementGroup
    , getGroupsExcept
    , objectiveFunctionDelta
    , swapElements
    , swapElementsHelper
    )

import Data.List (nub, sort)
import Test.QuickCheck (Positive(..), Property, (==>), once)
import Test.QuickCheck.All (quickCheckAll)

maxGroupSize :: Int
maxGroupSize = 100

homogeneousUsers :: ([User], [User])
homogeneousUsers = (concat xs, map head xs) where
    xs =
        [ map (\i -> User (show i) Female "NCL" "GB") ([1..8]   :: [Int])
        , map (\i -> User (show i) Female "NCL" "FR") ([9..16]  :: [Int])
        , map (\i -> User (show i) Female "CIT" "GB") ([17..24] :: [Int])
        , map (\i -> User (show i) Female "CIT" "FR") ([25..32] :: [Int])
        , map (\i -> User (show i) Male   "NCL" "GB") ([33..40] :: [Int])
        , map (\i -> User (show i) Male   "NCL" "FR") ([41..48] :: [Int])
        , map (\i -> User (show i) Male   "CIT" "GB") ([49..56] :: [Int])
        , map (\i -> User (show i) Male   "CIT" "FR") ([57..64] :: [Int])
        ]

prop_diversifyElements :: Property
prop_diversifyElements = once $
    map objectiveFunction diversifiedGroups == replicate (length diversifiedGroups) (objectiveFunction $ Group "A" $ snd homogeneousUsers)
    where diversifiedGroups = diversifyElements (length $ snd homogeneousUsers) (fst homogeneousUsers)

prop_objectiveFunctionHasResultOfZeroForSingletonGroup :: GroupNameWrapper -> UserWrapper -> Bool
prop_objectiveFunctionHasResultOfZeroForSingletonGroup gnw uw = objectiveFunction g == 0
    where g = Group (unwrapGroupName gnw) [unwrapUser uw]

prop_objectiveFunctionHasResultOfZeroForHomogeneousGroup :: Positive Int -> GroupNameWrapper -> UserWrapper -> Bool
prop_objectiveFunctionHasResultOfZeroForHomogeneousGroup n gnw (UserWrapper u) = objectiveFunction g == 0
    where g = Group (unwrapGroupName gnw) $ replicate (getPositive n) u

prop_objectiveFunctionHasResultOf3ForMaximallyDiverseUsers :: GroupNameWrapper -> UserWrapper -> UserWrapper -> Property
prop_objectiveFunctionHasResultOf3ForMaximallyDiverseUsers gnw (UserWrapper u1@(User _ g1 ce1 co1)) (UserWrapper u2@(User _ g2 ce2 co2)) =
    constraints ==> objectiveFunction group == 3
    where constraints = and [u1 /= u2, g1 /= g2, ce1 /= ce2, co1 /= co2]
          group       = Group (unwrapGroupName gnw) [u1, u2]

prop_objectiveFunctionHasNonNegativeResult :: GroupWrapper -> Bool
prop_objectiveFunctionHasNonNegativeResult gw = objectiveFunction (unwrapGroup gw) >= 0

prop_anySwitches :: HomoGroupWrapper -> HomoGroupWrapper -> Property
prop_anySwitches (HomoGroupWrapper g1@(Group _ xs)) (HomoGroupWrapper g2@(Group _ ys)) =
    constraints ==> case anySwitches [g1, g2] of
        (True,  gs) -> (sum . map objectiveFunction) gs > 0
        (False, _)  -> False
    where constraints          = g1 /= g2 && or [ge1 /= ge2, ce1 /= ce2, co1 /= co2]
          (User _ ge1 ce1 co1) = head xs
          (User _ ge2 ce2 co2) = head ys

prop_switchPairOfElements :: HomoGroupWrapper -> HomoGroupWrapper -> Property
prop_switchPairOfElements (HomoGroupWrapper g1@(Group _ xs)) (HomoGroupWrapper g2@(Group _ ys)) =
    constraints ==> case switchPairOfElements [g1, g2] u1 of
        (True,  gs) -> (sum . map objectiveFunction) gs > 0
        (False, _)  -> False
    where constraints             = g1 /= g2 && or [ge1 /= ge2, ce1 /= ce2, co1 /= co2]
          u1@(User _ ge1 ce1 co1) = head xs
          (User    _ ge2 ce2 co2) = head ys

prop_applySwitch :: GroupListWrapper -> Bool
prop_applySwitch (GroupListWrapper gs) =
    case applySwitch gs' (Switch u1 u2) of
        (g1'@(Group _ xs'):g2'@(Group _ ys'):rest') -> and
            [ g1' == g1
            , g2' == g2
            , u1 `notElem` xs'
            , u1 `elem` ys'
            , u2 `notElem` ys'
            , u2 `elem` xs'
            , rest' == rest
            ]
        _ -> False
    where gs'             = nub gs
          g1@(Group _ xs) = head gs'
          g2@(Group _ ys) = head $ tail gs'
          u1              = head xs
          u2              = head ys
          rest            = getGroupsExcept g1 (getGroupsExcept g2 gs')

prop_splitElementsIntoGroupsOfSizeGeneratesGroupNames :: CourseWrapper -> Positive Int -> Property
prop_splitElementsIntoGroupsOfSizeGeneratesGroupNames (CourseWrapper xs) (Positive groupSize) =
    constraints ==> names == take (length groups) groupNames
    where constraints = groupSize <= length xs && groupSize <= maxGroupSize
          groups      = distributeElementsIntoGroups groupSize xs
          names       = map (\(Group groupName _) -> groupName) groups

prop_distributeElementsIntoGroupsHasFixedSizedGroups :: CourseWrapper -> Positive Int -> Property
prop_distributeElementsIntoGroupsHasFixedSizedGroups (CourseWrapper xs) (Positive groupSize) =
    constraints ==> nub lengths == [groupSize]
    where constraints = groupSize < length xs && groupSize <= maxGroupSize
          groups      = distributeElementsIntoGroups groupSize xs
          lengths     = init $ map (\(Group _ xs') -> length xs') groups

prop_distributeElementsIntoGroupsHasLeftOverGroup :: CourseWrapper -> Positive Int -> Property
prop_distributeElementsIntoGroupsHasLeftOverGroup (CourseWrapper xs) (Positive groupSize) =
    constraints ==> length xs' > 0 && length xs' <= groupSize && length xs' == (length xs `mod` groupSize)
    where constraints   = and [groupSize <= length xs, groupSize <= maxGroupSize, length xs `mod` groupSize > 0]
          (Group _ xs') = last $ distributeElementsIntoGroups groupSize xs

prop_distributeElementsIntoGroupsHasSameUsers :: CourseWrapper -> Positive Int -> Property
prop_distributeElementsIntoGroupsHasSameUsers (CourseWrapper xs) (Positive groupSize) =
    constraints ==> sort xs == sort (concatMap (\(Group _ xs') -> xs') groups)
    where constraints = groupSize <= length xs && groupSize <= maxGroupSize
          groups      = distributeElementsIntoGroups groupSize xs

prop_getElementGroupSuccess :: GroupListWrapper -> Bool
prop_getElementGroupSuccess (GroupListWrapper gs) =
    case getElementGroup gs user of
        Just g' -> g' == g
        Nothing -> False
    where g@(Group _ xs) = head gs
          user           = head xs

prop_getElementGroupFail :: GroupListWrapper -> UserWrapper -> Property
prop_getElementGroupFail (GroupListWrapper gs) (UserWrapper user) =
    constraints ==> getElementGroup gs user == Nothing
    where constraints = and $ map (\(Group _ gs') -> user `notElem` gs') gs

prop_getGroupsExcept :: GroupListWrapper -> Bool
prop_getGroupsExcept (GroupListWrapper gs) = getGroupsExcept (head gs') gs' == tail gs'
    where gs' = nub gs

prop_objectiveFunctionDeltaReturnsNothingIfInSameGroup :: GroupListWrapper -> Property
prop_objectiveFunctionDeltaReturnsNothingIfInSameGroup (GroupListWrapper gs) =
    constraints ==> case objectiveFunctionDelta gs u1 u2 of
        Nothing -> True
        _       -> False
    where constraints  = length xs > 1
          (Group _ xs) = head gs
          u1           = head xs
          u2           = head $ drop 1 xs

prop_objectiveFunctionDeltaReturnsSwitch :: GroupListWrapper -> Bool
prop_objectiveFunctionDeltaReturnsSwitch (GroupListWrapper gs) =
    case objectiveFunctionDelta gs u1 u2 of
        Just (_, Switch u1' u2') -> u1' == u1 && u2' == u2
        Nothing                  -> False
    where gs'          = nub gs
          (Group _ xs) = head gs'
          (Group _ ys) = head $ tail gs'
          u1           = head xs
          u2           = head ys

prop_swapElements :: GroupListWrapper -> Property
prop_swapElements (GroupListWrapper gs) =
    constraints ==> case swapElements gs' u1 u2 of
        Just (gi, gi'@(Group _ xs'), gj, gj'@(Group _ ys')) -> and
            [ gi  == g1
            , gi' == g1
            , gj  == g2
            , gj' == g2
            , u1 `notElem` xs'
            , u1 `elem` ys'
            , u2 `notElem` ys'
            , u2 `elem` xs'
            ]
        Nothing -> False
    where constraints     = g1 /= g2 && u1 /= u2
          gs'             = nub gs
          g1@(Group _ xs) = head gs'
          g2@(Group _ ys) = head $ tail gs'
          u1              = head xs
          u2              = head ys

prop_swapElementsInSameGroup :: GroupListWrapper -> Property
prop_swapElementsInSameGroup (GroupListWrapper gs) =
    constraints ==> case swapElements gs' u1 u2 of
        Just _  -> False
        Nothing -> True
    where constraints  = u1 /= u2
          gs'          = nub gs
          (Group _ xs) = head gs'
          u1           = head xs
          u2           = head $ tail xs

prop_swapElementsHelper :: GroupWrapper -> GroupWrapper -> Property
prop_swapElementsHelper (GroupWrapper g1@(Group _ xs)) (GroupWrapper g2@(Group _ ys)) =
    constraints ==> and
        [ u1 `notElem` xs'
        , u1 `elem` ys'
        , u2 `notElem` ys'
        , u2 `elem` xs'
        ]
    where constraints = g1 /= g2 && u1 /= u2
          u1 = head xs
          u2 = head ys
          (Group _ xs', Group _ ys') = swapElementsHelper (u1, g1) (u2, g2)

return []

testLib :: IO Bool
testLib = $quickCheckAll
