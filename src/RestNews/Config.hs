{-# LANGUAGE OverloadedStrings #-}

module RestNews.Config
  ( Config (..),
  parseConfig
  ) where

import Data.Ini.Config
import qualified Data.Text.IO as T


data Config = Config {
    _runAtPort :: Int,
    _dbHost :: String,
    _dbPort :: Int,
    _dbUser :: String,
    _dbPassword :: String,
    _dbName :: String
} deriving (Eq, Show)


configParser :: IniParser Config
configParser =
  section "RestNews" $ do
    runAtPort <- fieldOf "runAtPort" number
    dbHost <- fieldOf "dbHost" string
    dbPort <- fieldOf "dbPort" number
    dbUser <- fieldOf "dbUser" string
    dbPassword <- fieldOf "dbPassword" string
    dbName <- fieldOf "dbName" string
    return $ Config runAtPort dbHost dbPort dbUser dbPassword dbName


parseConfig :: IO (Either String Config)
parseConfig = (`parseIniFile` configParser) <$> T.readFile "config.ini"

