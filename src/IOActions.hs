module IOActions
  ( runDiversification
  ) where

import Types (User, Group(..), DiversifyOpts(..))
import Parse (collectCSVRecords, toUserWithGroup)
import Lib (diversifyElements, objectiveFunction)

import Data.Csv (encodeWith, defaultEncodeOptions, EncodeOptions(..), Quoting(..), HasHeader(..))
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv.Streaming as CS
import qualified Data.Text as T
import qualified Data.Vector as V

runDiversification :: DiversifyOpts -> IO ()
runDiversification (DiversifyOpts groupSize values) = do
  csvData <- getContents
  case h csvData of
    Left  e -> putStrLn e
    Right v -> (if values then outputValues else outputCSV) $ diversifyElements groupSize $ V.toList v
  where
  h :: String -> Either String (V.Vector User)
  h csvData = do
    let lazyByteString = BL.fromStrict . encodeUtf8 . T.pack $ csvData
    let csvUserData  = CS.decode NoHeader lazyByteString
    collectCSVRecords csvUserData

outputValues :: [Group User] -> IO ()
outputValues xs = do
  let lines' = map (\g@(Group n _) -> let v = objectiveFunction g in n ++ " = " ++ show v) xs
  mapM_ putStrLn lines'

outputCSV :: [Group User] -> IO ()
outputCSV xs = do
  let encodeOpts = defaultEncodeOptions { encQuoting = QuoteAll }
  let encoded  = encodeWith encodeOpts $ toUserWithGroup xs
  putStrLn . T.unpack . decodeUtf8 . BL.toStrict $ encoded
