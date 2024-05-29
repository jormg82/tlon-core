-----------------------------------------
-- Params.hs
-----------------------------------------

module Params.Params
  ( Params (..),
    cmdLineParser,
  )
where

import Options.Applicative

newtype Params = Params {file :: FilePath}
  deriving (Show)

makeParams :: Parser Params
makeParams =
  Params <$> strArgument (metavar "FILE" <> help "input file")

cmdLineParser :: IO Params
cmdLineParser =
  execParser $
    info
      (makeParams <**> helper)
      (fullDesc <> progDesc "The Tlön Language")
