module Lib
    ( groupSize
    , objFnGroup
    , objFnAll
    ) where

import Types
    ( Group(..)
    , User(..)
    )

groupSize :: Int
groupSize = 20

objFnGroup :: Group -> Int
objFnGroup (Group _ []) = 0
objFnGroup (Group _ xs) = sum [d i j | i <- xs, j <- xs, i /= j]
    where
        d :: User -> User -> Int
        d (User _ gender1 centre1 country1) (User _ gender2 centre2 country2) =
            s gender1 gender2 + s centre1 centre2 + s country1 country2
            where
                s :: Eq a => a -> a -> Int
                s x y = if x == y then 0 else 1

objFnAll :: [Group] -> Int
objFnAll = sum . map objFnGroup
