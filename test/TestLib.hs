{-# LANGUAGE TemplateHaskell #-}

module TestLib (testLib) where

import Util ()
import Lib ()

import Test.QuickCheck.All (quickCheckAll)

return []

testLib :: IO Bool
testLib = $quickCheckAll
