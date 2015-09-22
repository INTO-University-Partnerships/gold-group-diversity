module Parse
    ( collectCSVRecords
    , toUserWithGroup
    ) where

import Types (User, UserWithGroup(..), Group)
import Lib (groupsToElements, getElementGroup)

import Control.Monad.Writer (Writer, writer, runWriter, tell)
import Data.List (intersperse)
import Data.Maybe (fromJust)

import qualified Data.Csv.Streaming as CS
import qualified Data.Vector as V

collectCSVRecords :: CS.Records a -> Either String (V.Vector a)
collectCSVRecords rs = if length s == 0 then Right v' else Left s
    where
        (v', xs) = runWriter $ f rs 1 V.empty
        s = concat $ intersperse "\r\n" xs
        f :: CS.Records a -> Int -> V.Vector a -> Writer [String] (V.Vector a)
        f (CS.Nil _ _)             _   v = writer (v, [])
        f (CS.Cons (Right r) more) row v = f more (row + 1) $ V.snoc v r
        f (CS.Cons (Left e)  more) row v = do
            tell ["Row " ++ show row ++ " has error \"" ++ e ++ "\""]
            f more (row + 1) v

toUserWithGroup :: [Group User] -> [UserWithGroup]
toUserWithGroup gs = map (\u -> UserWithGroup u $ fromJust $ getElementGroup gs u) users
    where users = groupsToElements gs
