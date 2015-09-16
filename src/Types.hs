module Types
    ( GroupName
    , StudentID
    , Centre
    , Country
    , Gender(..)
    , User(..)
    , Group(..)
    , Course
    ) where

type GroupName = String
type StudentID = String
type Centre    = String
type Country   = String
data Gender    = Female | Male deriving Eq
data User      = User StudentID Gender Centre Country deriving Show
data Group     = Group GroupName [User] deriving Show
type Course    = [User]

instance Eq User where
    (User id1 _ _ _) == (User id2 _ _ _) = id1 == id2

instance Show Gender where
    show Female = "F"
    show Male   = "M"
