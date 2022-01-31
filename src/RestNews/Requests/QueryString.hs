{-# LANGUAGE OverloadedStrings #-}

module RestNews.Requests.QueryString where

import Data.Int (Int32)
import Data.Text (Text, append, unpack)
import Data.Time.Calendar (Day)
import Network.HTTP.Types.URI (QueryText)
import Text.Read (readMaybe)


requiredFold :: QueryText -> Either String [(Text, Text)] -> Text -> Either String [(Text, Text)]
requiredFold query eitherAcc requiredFieldName =
    case eitherAcc of
    Right accList ->
        case lookup requiredFieldName query of 
        Just (Just fieldValue) -> Right $ (requiredFieldName, fieldValue) : accList
        _ -> Left $ "Query string has no required field or value for it: " ++ show requiredFieldName
    leftErrorString -> leftErrorString

collectRequiredFields :: [Text] -> QueryText -> Either String [(Text, Text)]
collectRequiredFields requiredFieldNames query =
    foldl (requiredFold query) (Right []) requiredFieldNames
        

parseRequiredValue :: Read a => Text -> Text -> Either String a
parseRequiredValue key value =
    case readMaybe $ unpack value of
        Just parsedValue -> Right parsedValue
        Nothing -> Left $ "Cannot parse value for field " ++ show key ++ ": " ++ show value

parseTextOrStringValue :: Read a => Text -> Text -> Either String a
parseTextOrStringValue key value =
    case readMaybe . (++) "\"" . (++ "\"") $ unpack value of
        Just parsedValue -> Right parsedValue
        Nothing -> Left $ "Cannot parse value for field " ++ show key ++ ": " ++ show value

-- https://discord.com/channels/280033776820813825/505367988166197268/937674122262085672

class FromQuery a where
    parseParams :: QueryText -> Either String a

newtype AuthorIdRequest = AuthorIdRequest {
    author_id :: Int32
} deriving (Show)

--instance FromQuery AuthorIdRequest where
--    parseParams query =
--        let requiredFields = ["author_id"]


data ArticlesByCreationDateRequest = ArticlesByCreationDateRequest {
    day :: Day,
    offset :: Maybe Int32
} deriving (Show)

--instance FromQuery ArticlesByCreationDateRequest where
--    parseParams query =

--instance FromQuery ArticlesByCreationDateRequest where
--    parseParams query =
--        let requiredFields = ["day"]
----        let fieldsToCollect = "[offset"]
--        in case  of
