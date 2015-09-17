module Util
    ( GroupNameWrapper(..)
    , StudentIDWrapper(..)
    , CentreWrapper(..)
    , CountryWrapper(..)
    , GenderWrapper(..)
    , UserWrapper(..)
    , GroupWrapper(..)
    , CourseWrapper(..)
    , GroupListWrapper(..)
    ) where

import Types
    ( GroupName
    , StudentID
    , Centre
    , Country
    , Gender(..)
    , User(..)
    , Group(..)
    , Course
    )

import Lib
    ( groupNames
    )

import Test.QuickCheck
    ( Arbitrary(..)
    , elements
    , choose
    , vectorOf
    , Gen
    )

groupSize :: Int
groupSize = 20

allCountries :: [Country]
allCountries =
    [ "AF", "AX", "AL", "DZ", "AS", "AD", "AO", "AI", "AQ", "AG", "AR", "AM", "AW", "AU", "AT", "AZ", "BS", "BH", "BD"
    , "BB", "BY", "BE", "BZ", "BJ", "BM", "BT", "BO", "BQ", "BA", "BW", "BV", "BR", "IO", "BN", "BG", "BF", "BI", "KH"
    , "CM", "CA", "CV", "KY", "CF", "TD", "CL", "CN", "CX", "CC", "CO", "KM", "CG", "CD", "CK", "CR", "CI", "HR", "CU"
    , "CW", "CY", "CZ", "DK", "DJ", "DM", "DO", "EC", "EG", "SV", "GQ", "ER", "EE", "ET", "FK", "FO", "FJ", "FI", "FR"
    , "GF", "PF", "TF", "GA", "GM", "GE", "DE", "GH", "GI", "GR", "GL", "GD", "GP", "GU", "GT", "GG", "GN", "GW", "GY"
    , "HT", "HM", "VA", "HN", "HK", "HU", "IS", "IN", "ID", "IR", "IQ", "IE", "IM", "IL", "IT", "JM", "JP", "JE", "JO"
    , "KZ", "KE", "KI", "KP", "KR", "KW", "KG", "LA", "LV", "LB", "LS", "LR", "LY", "LI", "LT", "LU", "MO", "MK", "MG"
    , "MW", "MY", "MV", "ML", "MT", "MH", "MQ", "MR", "MU", "YT", "MX", "FM", "MD", "MC", "MN", "ME", "MS", "MA", "MZ"
    , "MM", "NA", "NR", "NP", "NL", "NC", "NZ", "NI", "NE", "NG", "NU", "NF", "MP", "NO", "OM", "PK", "PW", "PS", "PA"
    , "PG", "PY", "PE", "PH", "PN", "PL", "PT", "PR", "QA", "RE", "RO", "RU", "RW", "BL", "SH", "KN", "LC", "MF", "PM"
    , "VC", "WS", "SM", "ST", "SA", "SN", "RS", "SC", "SL", "SG", "SX", "SK", "SI", "SB", "SO", "ZA", "GS", "SS", "ES"
    , "LK", "SD", "SR", "SJ", "SZ", "SE", "CH", "SY", "TW", "TJ", "TZ", "TH", "TL", "TG", "TK", "TO", "TT", "TN", "TR"
    , "TM", "TC", "TV", "UG", "UA", "AE", "GB", "US", "UM", "UY", "UZ", "VU", "VE", "VN", "VG", "VI", "WF", "EH", "YE"
    , "ZM", "ZW"
    ]

newtype GroupNameWrapper = GroupNameWrapper { unwrapGroupName :: GroupName } deriving Show
newtype StudentIDWrapper = StudentIDWrapper { unwrapStudentID :: StudentID } deriving Show
newtype CentreWrapper    = CentreWrapper    { unwrapCentre    :: Centre    } deriving Show
newtype CountryWrapper   = CountryWrapper   { unwrapCountry   :: Country   } deriving Show
newtype GenderWrapper    = GenderWrapper    { unwrapGender    :: Gender    } deriving Show
newtype UserWrapper      = UserWrapper      { unwrapUser      :: User      } deriving Show
newtype GroupWrapper     = GroupWrapper     { unwrapGroup     :: Group     } deriving Show
newtype CourseWrapper    = CourseWrapper    { unwrapCourse    :: Course    } deriving Show
newtype GroupListWrapper = GroupListWrapper { unwrapGroupList :: [Group]   } deriving Show

instance Arbitrary GroupNameWrapper where
    arbitrary = do
        names <- elements groupNames
        return $ GroupNameWrapper names

instance Arbitrary StudentIDWrapper where
    arbitrary = do
        xs <- vectorOf 10 $ elements ['0'..'9']
        return $ StudentIDWrapper xs

instance Arbitrary CentreWrapper where
    arbitrary = do
        centre <- elements ["CIT", "EAL", "EXE", "MAN", "MDX", "MER", "NCL", "QUB", "SCO", "SGL", "STI", "UEA", "UOG"]
        return $ CentreWrapper centre

instance Arbitrary CountryWrapper where
    arbitrary = do
        country <- elements $ take 15 allCountries
        return $ CountryWrapper country

instance Arbitrary GenderWrapper where
    arbitrary = do
        b <- arbitrary :: Gen Bool
        return $ GenderWrapper $ if b then Female else Male

instance Arbitrary UserWrapper where
    arbitrary = do
        studentID <- arbitrary
        gender    <- arbitrary
        centre    <- arbitrary
        country   <- arbitrary
        return $ UserWrapper $ User (unwrapStudentID studentID) (unwrapGender gender) (unwrapCentre centre) (unwrapCountry country)

instance Arbitrary GroupWrapper where
    arbitrary = do
        groupName <- arbitrary
        xs        <- vectorOf groupSize (arbitrary :: Gen UserWrapper)
        return $ GroupWrapper $ Group (unwrapGroupName groupName) $ map unwrapUser xs

instance Arbitrary CourseWrapper where
    arbitrary = do
        l  <- choose (50, 1000)
        xs <- vectorOf l (arbitrary :: Gen UserWrapper)
        return $ CourseWrapper $ map unwrapUser xs

instance Arbitrary GroupListWrapper where
    arbitrary = do
        l  <- choose (5, 50)
        xs <- vectorOf l (arbitrary :: Gen GroupWrapper)
        return $ GroupListWrapper $ map unwrapGroup xs
