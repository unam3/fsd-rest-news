{-# LANGUAGE OverloadedStrings #-}

module RestNews.Requests.QueryString where

import Data.Int (Int32)
import Data.Text (Text, append, unpack)
import Data.Time.Calendar (Day)
import Network.HTTP.Types.URI (QueryText)
import Text.Read (readMaybe)

collectFields' ::
    [(Text, Maybe Text)]
    -> [Text]
    -> [(Text, Maybe Text)]
    -> Either Text [(Text, Maybe Text)]
collectFields' _ [] [] = Left "Collect no fields"
collectFields' _ [] collected = Right collected
collectFields' query (fieldName:otherFieldsToCollect) collected =
    case lookup fieldName query of
    Just fieldValue ->
        let newQuery = filter ((/=) fieldName . fst) query
            newCollected = (fieldName, fieldValue) : collected
        in collectFields' newQuery otherFieldsToCollect newCollected
    Nothing -> Left $ append "Query has no required parameter: " fieldName

collectFields ::
    [(Text, Maybe Text)]
    -> [Text]
    -> Either Text [(Text, Maybe Text)]
collectFields query fieldsToCollect = collectFields' query fieldsToCollect []

filterOutNothing :: [(Text, Maybe Text)] -> Either Text [(Text, Maybe Text)]
filterOutNothing fields =
    Right $ filter ((/=) Nothing . snd) fields
    --case filter ((/=) Nothing . snd) fields of
    --[] -> Left "All fields with Nothing were filtered. No fields with value to return."
    --nonEmptyList -> Right nonEmptyList

class FromQuery a where
    parseParams :: QueryText -> Either String a

newtype AuthorIdRequest = AuthorIdRequest {
    author_id :: Int32
} deriving (Show)

instance FromQuery AuthorIdRequest where
    parseParams query =
        let fieldsToCollect = ["author_id"]
        in case ((Right . lookup "author_id") =<< filterOutNothing =<< collectFields query fieldsToCollect) of
            Right Nothing -> Left "Query has no required parameter with a value: author_id"
            Right (Just (Just fieldValue)) -> case readMaybe $ unpack fieldValue of
                Just author_id' -> Right $ AuthorIdRequest author_id'
                Nothing -> Left $ "Wrong author_id value: " ++ show fieldValue
            Left _ -> error "just can't be"


data ArticlesByCreationDateRequest = ArticlesByCreationDateRequest {
    day :: Day,
    offset :: Maybe Int32
} deriving (Show)

parseDay ::  Text -> [(Text, Maybe Text)] -> Either String (Maybe Int32 -> ArticlesByCreationDateRequest)
parseDay key query = case lookup key query of
    Nothing ->  Left $ "Query string has no required parameter with a value: " ++ unpack key
    Just Nothing -> Left $ "Wrong value for key " ++ unpack key
    (Just (Just value)) -> 
        case readMaybe $ unpack value of
            Just parsedDay -> Right $ ArticlesByCreationDateRequest parsedDay
            Nothing -> Left $ "Cannot parse value for field " ++ show key

--instance FromQuery ArticlesByCreationDateRequest where
--    parseParams query =
--        let fieldsToCollect = ["day", "offset"]
--        in case ((Right . lookup "day") =<< filterOutNothing =<< collectFields query fieldsToCollect) of
--            Right Nothing -> Left "Query has no required parameter with a value: author_id"
--            Right (Just (Just fieldValue)) -> case readMaybe $ unpack fieldValue of
--                Just author_id' -> Right $ ArticlesByCreationDateRequest author_id'
--                Nothing -> Left $ "Wrong author_id value: " ++ show fieldValue
--            Left _ -> error "just can't be"
