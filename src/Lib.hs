module Lib
    ( diversifyCourse
    , groupNames
    , objectiveFunction
    , anySwitches
    , switchOneUser
    , makeSwitch
    , splitCourseIntoGroupsOfSize
    , getUserGroup
    , getGroupsExcept
    , objectiveFunctionDelta
    , swapUsers
    , swapUsersBetweenGroups
    ) where

import Types
    ( GroupName
    , User(..)
    , Group(..)
    , Course
    , Switch
    )

import Control.Monad (guard)
import Data.List (sort, sortBy)
import Data.Maybe (fromJust)

diversifyCourse :: Int -> Course -> [Group]
diversifyCourse groupSize course = f gs'
    where
        f xs = case anySwitches xs of
            (True,  xs') -> f xs'
            (False, xs') -> sort xs'
        gs  = splitCourseIntoGroupsOfSize groupSize course
        gs' = if getGroupSize (last gs) /= getGroupSize (head gs) then init gs else gs

groupNames :: [GroupName]
groupNames = map ((++) "Group ") $ single ++ double ++ triple
    where alphabet = ['A'..'Z']
          single   = map (flip (:) []) alphabet
          double   = [[a, b]    | a <- alphabet, b <- alphabet]
          triple   = [[a, b, c] | a <- alphabet, b <- alphabet, c <- alphabet]

objectiveFunction :: Group -> Int
objectiveFunction (Group _ []) = 0
objectiveFunction (Group _ xs) = sum [fromJust $ d xs' i j | i <- [1..(m-1)], j <- [(i+1)..m]]
    where
        m   = length xs
        xs' = zip [1..] xs
        d :: [(Int, User)] -> Int -> Int -> Maybe Int
        d l i j = do
            User _ g1 ce1 co1 <- lookup i l
            User _ g2 ce2 co2 <- lookup j l
            return $ s g1 g2 + s ce1 ce2 + s co1 co2
            where
                s :: Eq a => a -> a -> Int
                s x y = if x == y then 0 else 1

anySwitches :: [Group] -> (Bool, [Group])
anySwitches [] = (False, [])
anySwitches gs = f (groupsToUsers gs) (False, gs)
    where
        f :: [User] -> (Bool, [Group]) -> (Bool, [Group])
        f [] p = p
        f (x:xs) (b, gs') =
            case switchOneUser gs' x of
                (True,  gs'') -> f xs (True, gs'')
                (False, _)    -> f xs (b,    gs')

-- 1. Find a User j in any Group (except the given User i's Group) for which a switch of Group assignments
--    between User i and User j results in the largest positive delta in the objective function value.
-- 2. If there is at least one switch, make the switch.
switchOneUser :: [Group] -> User -> (Bool, [Group])
switchOneUser [] _ = (False, [])
switchOneUser gs i =
    case f of
        Just (_, _, j) -> (True,  makeSwitch gs i j)
        Nothing        -> (False, gs)
    where
        f :: Maybe Switch
        f = do
            gi <- getUserGroup gs i
            xs <- mapM (objectiveFunctionDelta gs i) $ groupsToUsers $ getGroupsExcept gi gs
            let xs'  = filter (\(d, _, _) -> d > 0) xs
            let xs'' = sortBy (\(d1, _, _) (d2, _, _) -> compare d2 d1) xs'
            safeHead xs''

makeSwitch :: [Group] -> User -> User -> [Group]
makeSwitch [] _ _ = []
makeSwitch gs i j =
    case f of
        Just gs' -> gs'
        Nothing  -> gs
    where
        f :: Maybe [Group]
        f = do
            (gi, gi', gj, gj') <- swapUsers gs i j
            return $ gi' : gj' : getGroupsExcept gj (getGroupsExcept gi gs)

splitCourseIntoGroupsOfSize :: Int -> Course -> [Group]
splitCourseIntoGroupsOfSize groupSize course = zipWith (\groupName c -> Group groupName c) groupNames $ f course
    where
        f :: Course -> [Course]
        f [] = []
        f xs = take groupSize xs : f (drop groupSize xs)

getUserGroup :: [Group] -> User -> Maybe Group
getUserGroup [] _ = Nothing
getUserGroup gs u =
    case length gs' of
        0 -> Nothing
        _ -> Just $ head gs'
    where gs' = dropWhile (\(Group _ xs) -> u `notElem` xs) gs

getGroupsExcept :: Group -> [Group] -> [Group]
getGroupsExcept _ [] = []
getGroupsExcept g gs = filter ((/=) g) gs

objectiveFunctionDelta :: [Group] -> User -> User -> Maybe Switch
objectiveFunctionDelta [] _ _ = Nothing
objectiveFunctionDelta gs i j = do
    (gi, gi', gj, gj') <- swapUsers gs i j
    let old = objectiveFunction gi  + objectiveFunction gj
    let new = objectiveFunction gi' + objectiveFunction gj'
    return (new - old, i, j)

swapUsers :: [Group] -> User -> User -> Maybe (Group, Group, Group, Group)
swapUsers gs i j = do
    gi <- getUserGroup gs i
    gj <- getUserGroup gs j
    guard $ gi /= gj
    let (gi', gj') = swapUsersBetweenGroups (i, gi) (j, gj)
    return (gi, gi', gj, gj')

swapUsersBetweenGroups :: (User, Group) -> (User, Group) -> (Group, Group)
swapUsersBetweenGroups (i, Group ni gi) (j, Group nj gj) = (Group ni $ j : gi', Group nj $ i : gj')
    where gi' = filter ((/=) i) gi
          gj' = filter ((/=) j) gj

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

groupsToUsers :: [Group] -> [User]
groupsToUsers = concatMap (\(Group _ xs) -> xs)

getGroupSize :: Group -> Int
getGroupSize (Group _ xs) = length xs
