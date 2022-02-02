{-# LANGUAGE OverloadedStrings #-}

module RestNews.Requests.QueryString where

import Data.Int (Int32)
import Data.Text (Text, append, unpack)
import Data.Time.Calendar (Day)
import Network.HTTP.Types.URI (QueryText)
import Text.Read (readMaybe)

-- we do not care for fields without values ("?pluh")
-- we use first encountered field-value pair in query string: "?pluh&pluh=123" will fail on parsing pluh field.

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


optionalFold :: QueryText -> Either String [(Text, Text)] -> Text -> Either String [(Text, Text)]
optionalFold query eitherAcc requiredFieldName =
    case eitherAcc of
    Right accList ->
        case lookup requiredFieldName query of 
        Just (Just fieldValue) -> Right $ (requiredFieldName, fieldValue) : accList
        _ -> Right accList
    leftErrorString -> leftErrorString

collectOptionalFields :: [Text] -> QueryText -> Either String [(Text, Text)]
collectOptionalFields optionalFieldNames query =
    foldl (optionalFold query) (Right []) optionalFieldNames
        

parseOptionalValue :: Read a => Text -> Text -> Either String (Maybe a)
parseOptionalValue key value =
    case readMaybe $ unpack value of
        Just parsedValue -> Right $ Just parsedValue
        Nothing -> Right Nothing

parseOptionalTextOrStringValue :: Read a => Text -> Text -> Either String (Maybe a)
parseOptionalTextOrStringValue key value =
    case readMaybe . (++) "\"" . (++ "\"") $ unpack value of
        Just parsedValue -> Right $ Just parsedValue
        Nothing -> Right $ Nothing

-- https://discord.com/channels/280033776820813825/505367988166197268/937674122262085672

class FromQuery a where
    parseParams :: QueryText -> Either String a


--newtype AuthorIdRequest = AuthorIdRequest {
--    author_id :: Int32
--} deriving (Show)
--
----instance FromQuery AuthorIdRequest where
----    parseParams query =
----        let requiredFields = ["author_id"]
--
--
--newtype CategoryIdRequest = CategoryIdRequest {
--    category_id :: Int32
--} deriving (Show)
--
----instance FromQuery CategoryIdRequest where
--
--newtype TagIdRequest = TagIdRequest {
--    tag_id :: Int32
--} deriving (Show)
--
--
--data ArticleCommentsRequest = ArticleCommentsRequest {
--    article_id :: Int32,
--    offset :: Maybe Int32
--} deriving (Show)
--
----instance FromQuery ArticleCommentsRequest
--
--
--newtype ArticleDraftIdRequest = ArticleDraftIdRequest {
--    article_id :: Int32
--} deriving (Show)
--
----instance FromQuery ArticleDraftIdRequest
--
--
--data ArticlesByCategoryIdRequest = ArticlesByCategoryIdRequest {
--    category_id :: Int32,
--    offset :: Maybe Int32
--} deriving (Show)
--
----instance FromQuery ArticlesByCategoryIdRequest
--
--
--data TagIdRequestWithOffset = TagIdRequestWithOffset {
--    tag_id :: Int32,
--    offset :: Maybe Int32
--} deriving (Show)
--
----instance FromQuery TagIdRequestWithOffset
--
--
--data ArticlesByTagIdListRequest = ArticlesByTagIdListRequest {
--    tags_ids :: Vector Int32,
--    offset :: Maybe Int32
--} deriving (Show)
--
----instance FromQuery ArticlesByTagIdListRequest
--
--
--data ArticlesByTitlePartRequest = ArticlesByTitlePartRequest {
--    title_substring :: Text,
--    offset :: Maybe Int32
--} deriving (Show)
--
----instance FromQuery ArticlesByTitlePartRequest
--
--
--data ArticlesByContentPartRequest = ArticlesByContentPartRequest {
--    content_substring :: Text,
--    offset :: Maybe Int32
--} deriving (Show)
--
--
--data ArticlesByAuthorNamePartRequest = ArticlesByAuthorNamePartRequest {
--    author_name_substring :: Text,
--    offset :: Maybe Int32
--} deriving (Show)
--
----instance FromQuery ArticlesByAuthorNamePartRequest
--
--
--data ArticlesByTextContentRequest = ArticlesByTextContentRequest {
--    substring :: Text,
--    offset :: Maybe Int32
--} deriving (Show)
--
----instance FromQuery ArticlesByTextContentRequest
--
--
--newtype OffsetRequest = OffsetRequest {
--    offset :: Int32
--} deriving (Show)
--
----instance FromQuery OffsetRequest
--
--
--data ArticlesByCreationDateRequest = ArticlesByCreationDateRequest {
--    day :: Day,
--    offset :: Maybe Int32
--} deriving (Show)
--
----instance FromQuery ArticlesByCreationDateRequest where
----    parseParams query =
----        let requiredFields = ["day"]
------        let fieldsToCollect = "[offset"]
----        in case  of
