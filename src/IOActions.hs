module IOActions
    ( runDiversification
    ) where

import Types (User, DiversifyOpts(..))
import Parse (collectCSVUserData, collectCSVUserErrors, toUserWithGroup)
import Lib (diversifyElements)

import Data.Csv (encodeWith, defaultEncodeOptions, EncodeOptions(..), Quoting(..), HasHeader(..))
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv.Streaming as CS
import qualified Data.Text as T
import qualified Data.Vector as V

runDiversification :: DiversifyOpts -> IO ()
runDiversification (DiversifyOpts groupSize) = do
    csvData <- getContents
    case h csvData of
        Left  e -> putStrLn e
        Right v -> do
            let encoded = encodeWith encodeOpts $ toUserWithGroup $ diversifyElements groupSize $ V.toList v
            putStrLn $ T.unpack $ decodeUtf8 $ BL.toStrict encoded
            where encodeOpts = defaultEncodeOptions { encQuoting = QuoteAll }
    where
        h :: String -> Either String (V.Vector User)
        h csvData = do
            let lazyByteString = BL.fromStrict . encodeUtf8 . T.pack $ csvData
            let csvUserData    = CS.decode NoHeader lazyByteString
            _ <- collectCSVUserErrors 1 V.empty csvUserData
            return $ collectCSVUserData V.empty csvUserData
