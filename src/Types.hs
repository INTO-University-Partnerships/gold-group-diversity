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
data Gender    = Female | Male deriving (Eq, Show)
data User      = User StudentID Gender Centre Country deriving Show
data Group     = Group GroupName [User] deriving Show
type Course    = [User]

instance Eq User where
    (User id1 _ _ _) == (User id2 _ _ _) = id1 == id2

instance Ord User where
    compare (User id1 _ _ _) (User id2 _ _ _) = compare id1 id2

instance Eq Group where
    (Group name1 _) == (Group name2 _) = name1 == name2

instance Ord Group where
    compare (Group name1 _) (Group name2 _) = compare name1 name2
