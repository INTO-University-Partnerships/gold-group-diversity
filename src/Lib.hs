module Lib
    ( groupNames
    , objFnGroup
    , objFnAll
    , diversifyCourse
    , getSwitch
    , splitCourseIntoGroupsOfSize
    , getUserGroup
    , getGroupsExcept
    , getObjFnDelta
    , swapElementsBetweenGroups
    , safeHead
    ) where

import Types
    ( GroupName
    , User(..)
    , Group(..)
    , Course
    , Switch
    )

import Control.Monad (guard)
import Data.List (sortBy)
import Data.Maybe (fromJust)

groupNames :: [GroupName]
groupNames = map ((++) "Group ") $ single ++ double ++ triple
    where alphabet = ['A'..'Z']
          single   = map (flip (:) []) alphabet
          double   = [[a, b]    | a <- alphabet, b <- alphabet]
          triple   = [[a, b, c] | a <- alphabet, b <- alphabet, c <- alphabet]

objFnGroup :: Group -> Int
objFnGroup (Group _ []) = 0
objFnGroup (Group _ xs) = sum [fromJust $ d xs' i j | i <- [1..(m-1)], j <- [(i+1)..m]]
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

objFnAll :: [Group] -> Int
objFnAll = sum . map objFnGroup

diversifyCourse :: Course -> [Group]
diversifyCourse = undefined

-- Find a User in any Group (except the given User's Group) for which a switch of Group assignments
-- between the given User and the User found results in the largest increase in the objective function value.
getSwitch :: User -> [Group] -> Maybe Switch
getSwitch _ [] = Nothing
getSwitch u gs = do
    ug <- getUserGroup u gs
    xs <- mapM (getObjFnDelta gs u) $ concatMap (\(Group _ xs) -> xs) $ getGroupsExcept ug gs
    let xs'  = filter (\(d, _, _) -> d > 0) xs
    let xs'' = sortBy (\(d1, _, _) (d2, _, _) -> compare d2 d1) xs'
    safeHead xs''

splitCourseIntoGroupsOfSize :: Int -> Course -> [Group]
splitCourseIntoGroupsOfSize groupSize course = zipWith (\groupName c -> Group groupName c) groupNames $ f course
    where
        f :: Course -> [Course]
        f [] = []
        f xs = take groupSize xs : f (drop groupSize xs)

getUserGroup :: User -> [Group] -> Maybe Group
getUserGroup _ [] = Nothing
getUserGroup u gs =
    case length gs' of
        0 -> Nothing
        _ -> Just $ head gs'
    where gs' = dropWhile (\(Group _ xs) -> u `notElem` xs) gs

getGroupsExcept :: Group -> [Group] -> [Group]
getGroupsExcept _ [] = []
getGroupsExcept g gs = filter ((/=) g) gs

getObjFnDelta :: [Group] -> User -> User -> Maybe Switch
getObjFnDelta [] _ _ = Nothing
getObjFnDelta gs i j = do
    gi <- getUserGroup i gs
    gj <- getUserGroup j gs
    guard $ gi /= gj
    let (gi', gj') = swapElementsBetweenGroups (i, gi) (j, gj)
    let old = objFnGroup gi  + objFnGroup gj
    let new = objFnGroup gi' + objFnGroup gj'
    return (new - old, i, j)

swapElementsBetweenGroups :: (User, Group) -> (User, Group) -> (Group, Group)
swapElementsBetweenGroups (i, Group ni gi) (j, Group nj gj) = (Group ni $ j : gi', Group nj $ i : gj')
    where gi' = filter ((/=) i) gi
          gj' = filter ((/=) j) gj

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x
