module Main where

import Types (DiversifyOpts(..))
import IOActions (runDiversification)

import Options.Applicative
  ( Parser
  , ReadM
  , readerError
  , execParser
  , option
  , switch
  , str
  , info
  , helper
  , progDesc
  , fullDesc
  , header
  , long
  , short
  , value
  , help
  , (<>)
  )

optParseGroupSize :: String -> ReadM Int
optParseGroupSize f
  | s > 1 && s <= 100 = return s
  | otherwise         = readerError "Please specify a sensible group size in the range [2..100]"
  where
  s = read f

diversifyOpts :: Parser DiversifyOpts
diversifyOpts = DiversifyOpts
                <$> option (str >>= optParseGroupSize) (long "size"   <> short 's' <> help "Group size (20 by default)" <> value 20)
                <*> switch                             (long "values" <> short 'v' <> help "Whether to output objective function values")

main :: IO ()
main = execParser opts >>= runDiversification
  where
  ourHeader   = "GOLD group diversifier"
  ourProgDesc = "Given a group size, creates diverse groups of users"
  opts        = info (helper <*> diversifyOpts) (fullDesc <> progDesc ourProgDesc <> header ourHeader)
