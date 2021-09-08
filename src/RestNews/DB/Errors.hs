{-# LANGUAGE OverloadedStrings  #-}

module RestNews.DB.Errors (
    SessionError(..)
    , eNegativeOffset
    , getError
    , getErrorCode
    ) where

import Data.Aeson (ToJSON, (.=), object, toJSON)
import Data.ByteString.Internal (unpackChars)
import Data.Text (Text, pack)

import qualified Hasql.Session as Session


data SessionError = PSQL_STRING_DATA_RIGHT_TRUNCATION
    | PSQL_INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE
    | PSQL_FOREIGN_KEY_VIOLATION
    | PSQL_UNIQUE_VIOLATION
    | UnexpectedAmountOfRowsOrUnexpectedNull deriving (Show, Eq)

-- https://www.postgresql.org/docs/12/errcodes-appendix.html
getError :: Session.QueryError -> Maybe (SessionError, Maybe String)
getError (Session.QueryError _ _ (Session.ResultError (Session.ServerError "22001" _ details _))) =
    Just (PSQL_STRING_DATA_RIGHT_TRUNCATION, fmap unpackChars details)

getError (Session.QueryError _ _ (Session.ResultError (Session.ServerError "2201X" msg _ _))) =
    Just (PSQL_INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE, Just $ unpackChars msg)

getError (Session.QueryError _ _ (Session.ResultError (Session.ServerError "23503" _ details _))) =
    Just (PSQL_FOREIGN_KEY_VIOLATION, fmap unpackChars details)

getError (Session.QueryError _ _ (Session.ResultError (Session.ServerError "23505" _ details _))) =
    Just (PSQL_UNIQUE_VIOLATION, fmap unpackChars details)

getError (Session.QueryError _ _ (Session.ResultError (Session.UnexpectedAmountOfRows 0))) =
    Just (UnexpectedAmountOfRowsOrUnexpectedNull, Nothing)

getError (Session.QueryError _ _ (Session.ResultError (Session.RowError 0 Session.UnexpectedNull))) =
    Just (UnexpectedAmountOfRowsOrUnexpectedNull, Nothing)

getError _ = Nothing

getErrorCode :: Session.QueryError -> Maybe SessionError
getErrorCode = fmap fst . getError


-- "\\\"offset\\\" must not be negative\"}"

newtype MustNotBeNegative =
    MustNotBeNegative {error :: Text}
        deriving (Show)

instance ToJSON MustNotBeNegative where
    toJSON (MustNotBeNegative error)
        = object ["name" .= error]

eNegativeOffset = MustNotBeNegative "\\\"offset\\\" must not be negative"


--MaxBoundOverflow a
--"article_title length must be 80 characters at most\"}"
--"name and surname field length must be 80 characters at most\"}"
--"category name length must be 80 characters at most\"}"
--"tag_name length must be 80 characters at most\"}"
--
--DoesNotExist a
--"such author does not exist\"}"
--"such user does not exist\"}"
--"parent category does not exist\"}"
--
--AlreadyExists 
--"tag with such name already exists\"}"
--"such user is already an author\"}"
--"user with this username already exists\"}"
--
--(Already ?) in use
--"category is in use\"}"
--
--NoSuchThing a
--"no such article\"}"
--"no such category\"}"
--"no such comment\"}"
--"no such tag\"}"
--
--"tag is referenced by an article\"}"
--
