module Lib
    ( groupNames
    , objFnGroup
    , objFnAll
    , diversifyCourse
    , getSwitch
    , splitCourseIntoGroupsOfSize
    , getUserGroup
    , getGroupsExcept
    , getObjFnValueAfterSwitch
    , swapElementsBetweenGroups
    ) where

import Types
    ( GroupName
    , User(..)
    , Group(..)
    , Course
    )

import Control.Monad (guard)
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

-- Find the element (i.e. User) in any group for which a switch of group assignments between
-- elements and results in the largest increase in the objective function value
getSwitch :: User -> [Group] -> Maybe User
getSwitch _ [] = Nothing
getSwitch u gs = do
    userGroup   <- getUserGroup u gs
    _           <- mapM (getObjFnValueAfterSwitch gs u) $ concatMap (\(Group _ xs) -> xs) $ getGroupsExcept userGroup gs
    return u -- TODO

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

getObjFnValueAfterSwitch :: [Group] -> User -> User -> Maybe (Int, User, User)
getObjFnValueAfterSwitch [] _ _ = Nothing
getObjFnValueAfterSwitch gs i j = do
    gi <- getUserGroup i gs
    gj <- getUserGroup j gs
    guard (gi /= gj)
    let gs'        = getGroupsExcept gi gs
    let gs''       = getGroupsExcept gj gs'
    let (gi', gj') = swapElementsBetweenGroups (i, gi) (j, gj)
    let objFnValue = objFnAll $ gs'' ++ [gi', gj']
    return $ (,,) objFnValue i j

swapElementsBetweenGroups :: (User, Group) -> (User, Group) -> (Group, Group)
swapElementsBetweenGroups (i, Group ni gi) (j, Group nj gj) = (Group ni $ j : gi', Group nj $ i : gj')
    where gi' = filter ((/=) i) gi
          gj' = filter ((/=) j) gj
