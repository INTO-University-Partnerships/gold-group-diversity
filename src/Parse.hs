module Parse
    ( collectCSVUserData
    , collectCSVUserErrors
    , toUserWithGroup
    ) where

import Types (User, UserWithGroup(..), Group)
import Lib (groupsToUsers, getUserGroup)

import Data.Csv.Streaming (Records(..))
import Data.Maybe (fromJust)

import qualified Data.Csv.Streaming as CS
import qualified Data.Vector as V

collectCSVUserData :: V.Vector User -> Records User -> V.Vector User
collectCSVUserData v (Nil _ _) = v
collectCSVUserData v (Cons r moreRecords) =
    case r of
        Right i -> collectCSVUserData (V.snoc v i) moreRecords
        Left  _ -> collectCSVUserData v moreRecords

collectCSVUserErrors :: Int -> V.Vector String -> CS.Records User -> Either String Bool
collectCSVUserErrors _ v (CS.Nil _ _)
    | l == 0    = Right True
    | otherwise = Left $ V.foldl1 (\acc x -> acc ++ "\r\n" ++ x) v
    where l     = V.length v
collectCSVUserErrors row v (CS.Cons r moreRecords) =
    case r of
        Right _ -> collectCSVUserErrors (row + 1) v moreRecords
        Left  e -> collectCSVUserErrors (row + 1) (V.snoc v $ "Row " ++ show row ++ " has error \"" ++ e ++ "\"") moreRecords

toUserWithGroup :: [Group] -> [UserWithGroup]
toUserWithGroup gs = map (\u -> UserWithGroup u $ fromJust $ getUserGroup gs u) users
    where users = groupsToUsers gs
