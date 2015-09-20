{-# LANGUAGE OverloadedStrings #-}

module Types
    ( GroupName
    , StudentID
    , Centre
    , Country
    , Gender(..)
    , User(..)
    , UserWithGroup(..)
    , Group(..)
    , Course
    , Switch
    , DiversifyOpts(..)
    ) where

import Control.Monad (mzero)
import Data.Csv (FromField, parseField, FromRecord, parseRecord, ToRecord, toRecord, record, (.!))
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import qualified Data.ByteString.Internal as BI
import qualified Data.Text as T
import qualified Data.Vector as V

enc :: String -> BI.ByteString
enc = encodeUtf8 . T.pack

type GroupName = String
type StudentID = String
type Centre    = String
type Country   = String
data Gender    = Female | Male deriving (Eq, Ord, Show)
data User      = User StudentID Gender Centre Country deriving Show
data Group     = Group GroupName [User] deriving Show
type Course    = [User]
type Switch    = (Int, User, User)

data UserWithGroup = UserWithGroup User Group deriving Show
data DiversifyOpts = DiversifyOpts Int deriving Show

instance Eq User where
    (User id1 _ _ _) == (User id2 _ _ _) = id1 == id2

instance Ord User where
    compare (User id1 _ _ _) (User id2 _ _ _) = compare id1 id2

instance Eq Group where
    (Group name1 _) == (Group name2 _) = name1 == name2

instance Ord Group where
    compare (Group name1 _) (Group name2 _) =
        case c of
            EQ -> compare name1 name2
            _  -> c
        where c = compare (length name1) (length name2)

instance Eq UserWithGroup where
    (UserWithGroup u1 _) == (UserWithGroup u2 _) = u1 == u2

instance FromField Gender where
    parseField f
        | f' == "f"      = pure Female
        | f' == "female" = pure Female
        | f' == "m"      = pure Male
        | f' == "male"   = pure Male
        | otherwise      = fail $ "\"" ++ unpacked ++ "\" is not one of ['F', 'M']"
        where decoded  = decodeUtf8 f
              unpacked = T.unpack decoded
              f'       = T.toLower decoded

instance FromRecord User where
    parseRecord v
        | l == 4 = User
                   <$> v .! 0
                   <*> v .! 1
                   <*> v .! 2
                   <*> v .! 3
        | otherwise = mzero
        where l = length v

instance ToRecord User where
    toRecord (User i g ce co) = record l
        where
            l :: [BI.ByteString]
            l = map enc [i, if g == Female then "F" else "M", ce, co]

instance ToRecord UserWithGroup where
    toRecord (UserWithGroup user (Group n _)) = V.snoc (toRecord user) (enc n)
