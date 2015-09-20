{-# LANGUAGE ScopedTypeVariables #-}

module Lib
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
    , groupsToElements
    ) where

import Types
    ( GroupName
    , Group(..)
    , Switch(..)
    , Element
    , distance
    )

import Control.Monad (guard)
import Data.List (sort, sortBy)
import Data.Maybe (fromJust)

diversifyElements :: forall a. Element a => Int -> [a] -> [Group a]
diversifyElements groupSize es = f gs'
    where
        f :: [Group a] -> [Group a]
        f xs = case anySwitches xs of
            (True,  xs') -> f xs'
            (False, xs') -> sort xs' ++ last'
        gs       = distributeElementsIntoGroups groupSize es
        leftOver = getGroupSize (last gs) /= getGroupSize (head gs)
        gs'      = if leftOver then init gs   else gs
        last'    = if leftOver then [last gs] else []

groupNames :: [GroupName]
groupNames = map ((++) "Group ") $ single ++ double ++ triple
    where alphabet = ['A'..'Z']
          single   = map (flip (:) []) alphabet
          double   = [[a, b]    | a <- alphabet, b <- alphabet]
          triple   = [[a, b, c] | a <- alphabet, b <- alphabet, c <- alphabet]

objectiveFunction :: forall a. Element a => Group a -> Int
objectiveFunction (Group _ []) = 0
objectiveFunction (Group _ xs) = sum [fromJust $ d xs' i j | i <- [1..(m-1)], j <- [(i+1)..m]]
    where
        m   = length xs
        xs' = zip [1..] xs
        d :: [(Int, a)] -> Int -> Int -> Maybe Int
        d l i j = do
            e1 <- lookup i l
            e2 <- lookup j l
            return $ distance e1 e2

anySwitches :: forall a. Element a => [Group a] -> (Bool, [Group a])
anySwitches [] = (False, [])
anySwitches gs = f (groupsToElements gs) (False, gs)
    where
        f :: [a] -> (Bool, [Group a]) -> (Bool, [Group a])
        f [] p = p
        f (x:xs) (b, gs') =
            case switchPairOfElements gs' x of
                (True,  gs'') -> f xs (True, gs'')
                (False, _)    -> f xs (b,    gs')

-- 1. Find an Element j in any Group (except the given Element i's Group) for which a switch of Group assignments
--    between Element i and Element j results in the largest positive delta in the objective function value.
-- 2. If there is at least one switch, apply the switch.
switchPairOfElements :: forall a. Element a => [Group a] -> a -> (Bool, [Group a])
switchPairOfElements [] _ = (False, [])
switchPairOfElements gs i =
    case f of
        Just s  -> (True,  applySwitch gs s)
        Nothing -> (False, gs)
    where
        f :: Maybe (Switch a)
        f = do
            gi <- getElementGroup gs i
            xs <- mapM (objectiveFunctionDelta gs i) $ groupsToElements $ getGroupsExcept gi gs
            let xs'  = filter (\(Switch d  _  _) -> d > 0) xs
            let xs'' = sortBy (\(Switch d1 _  _) (Switch d2 _ _) -> compare d2 d1) xs'
            safeHead xs''

applySwitch :: forall a. Element a => [Group a] -> Switch a -> [Group a]
applySwitch [] _ = []
applySwitch gs (Switch _ i j) =
    case f of
        Just gs' -> gs'
        Nothing  -> gs
    where
        f :: Maybe [Group a]
        f = do
            (gi, gi', gj, gj') <- swapElements gs i j
            return $ gi' : gj' : getGroupsExcept gj (getGroupsExcept gi gs)

distributeElementsIntoGroups :: Int -> [a] -> [Group a]
distributeElementsIntoGroups groupSize course =
    zipWith (\groupName c -> Group groupName c) groupNames $ f course
    where
        f :: [a] -> [[a]]
        f [] = []
        f xs = take groupSize xs : f (drop groupSize xs)

getElementGroup :: Eq a => [Group a] -> a -> Maybe (Group a)
getElementGroup [] _ = Nothing
getElementGroup gs e = safeHead gs'
    where gs' = dropWhile (\(Group _ xs) -> e `notElem` xs) gs

getGroupsExcept :: Element a => Group a -> [Group a] -> [Group a]
getGroupsExcept _ [] = []
getGroupsExcept g gs = filter ((/=) g) gs

objectiveFunctionDelta :: Element a => [Group a] -> a -> a -> Maybe (Switch a)
objectiveFunctionDelta [] _ _ = Nothing
objectiveFunctionDelta gs i j = do
    (gi, gi', gj, gj') <- swapElements gs i j
    let old = objectiveFunction gi  + objectiveFunction gj
    let new = objectiveFunction gi' + objectiveFunction gj'
    return $ Switch (new - old) i j

swapElements :: Element a => [Group a] -> a -> a -> Maybe (Group a, Group a, Group a, Group a)
swapElements gs i j = do
    gi <- getElementGroup gs i
    gj <- getElementGroup gs j
    guard $ gi /= gj
    let (gi', gj') = swapElementsHelper (i, gi) (j, gj)
    return (gi, gi', gj, gj')

swapElementsHelper :: Eq a => (a, Group a) -> (a, Group a) -> (Group a, Group a)
swapElementsHelper (i, Group ni gi) (j, Group nj gj) =
    (Group ni $ j : gi', Group nj $ i : gj')
    where gi' = filter ((/=) i) gi
          gj' = filter ((/=) j) gj

groupsToElements :: [Group a] -> [a]
groupsToElements = concatMap (\(Group _ xs) -> xs)

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

getGroupSize :: Group a -> Int
getGroupSize (Group _ xs) = length xs
