module Lib
    ( groupSize
    , objFnGroup
    , objFnAll
    ) where

import Types
    ( Group(..)
    , User(..)
    )

import Data.Maybe (fromJust)

groupSize :: Int
groupSize = 20

objFnGroup :: Group -> Int
objFnGroup (Group _ []) = 0
objFnGroup (Group _ xs) = sum [fromJust $ d xs' i j | i <- [1..(m-1)], j <- [(i+1)..m]]
    where
        m = length xs
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
