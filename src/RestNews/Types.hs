{-# LANGUAGE DeriveGeneric #-}

module RestNews.Types (
    Error (..)
) where


import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude (Show)

newtype Error =
    Error {error :: Text}
        deriving (Generic, Show)

instance ToJSON Error
