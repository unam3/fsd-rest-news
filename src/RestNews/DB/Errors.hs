{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module RestNews.DB.Errors (
    SessionError(..)
    , eArticleTitleMaxBoundOverflow
    , eCategoryInUse
    , eCategoryNameMaxBoundOverflow
    , eNameAndSurnameMaxBoundOverflow
    , eNegativeOffset
    , eNoSuchArticle
    , makeNoSuchCategory
    , parentIdDescendant
    , eNoSuchComment
    , eNoSuchEndpoint
    , eNoSuchTag
    , eSuchAuthorDoesNotExist
    , eSuchUserAlreadyAuthor
    , eSuchUserDoesNotExist
    , eTagNameMaxBoundOverflow
    , eTagReferencedByArticle
    , eTagWithSuchNameAlreadyExist
    , eUserWithSuchUsernameAlreadyExist
    , eWrongUsernameOrPassword
    , getError
    , getErrorCode
    ) where

import Data.ByteString.Internal (unpackChars)
import Data.Text (Text, append)

import qualified Hasql.Session as Session

import RestNews.Types (Error (..))

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


eNoSuchEndpoint :: Error
eNoSuchEndpoint = Error "No such endpoint"


eNegativeOffset :: Error
eNegativeOffset = Error "\\\"offset\\\" must not be negative"


makeFieldMaxBoundOverflow :: Text -> Error
makeFieldMaxBoundOverflow fieldName = Error
    $ append fieldName " length must be 80 characters at most\"}"

eArticleTitleMaxBoundOverflow :: Error
eArticleTitleMaxBoundOverflow = makeFieldMaxBoundOverflow ("article_title" :: Text)

eNameAndSurnameMaxBoundOverflow :: Error
eNameAndSurnameMaxBoundOverflow = makeFieldMaxBoundOverflow "name and surname"

eCategoryNameMaxBoundOverflow :: Error
eCategoryNameMaxBoundOverflow = makeFieldMaxBoundOverflow "category name"

eTagNameMaxBoundOverflow :: Error
eTagNameMaxBoundOverflow = makeFieldMaxBoundOverflow "tag_name"


makeDoesNotExist :: Text -> Error
makeDoesNotExist thing = Error
    $ append thing " does not exist"

eSuchAuthorDoesNotExist :: Error
eSuchAuthorDoesNotExist = makeDoesNotExist "such author"

eSuchUserDoesNotExist :: Error
eSuchUserDoesNotExist = makeDoesNotExist "such user"


makeAlreadyExist :: Text -> Error
makeAlreadyExist thing = Error
    $ append thing " already exist"

eTagWithSuchNameAlreadyExist :: Error
eTagWithSuchNameAlreadyExist = makeAlreadyExist "tag with such name"

eUserWithSuchUsernameAlreadyExist :: Error
eUserWithSuchUsernameAlreadyExist = makeAlreadyExist "user with such username"


eSuchUserAlreadyAuthor :: Error
eSuchUserAlreadyAuthor = Error "such user is already an author"


eCategoryInUse :: Error
eCategoryInUse = Error "category is in use"


makeNoSuchThing :: Text -> Error
makeNoSuchThing thing = Error
    $ append "no such " thing

eNoSuchArticle :: Error
eNoSuchArticle = makeNoSuchThing "article"

makeNoSuchCategory :: Text -> Error
makeNoSuchCategory = makeNoSuchThing . append "category with id="

parentIdDescendant :: Error
parentIdDescendant = Error "parent_id is in descenants"

eNoSuchComment :: Error
eNoSuchComment = makeNoSuchThing "comment"

eNoSuchTag :: Error
eNoSuchTag = makeNoSuchThing "tag"


eTagReferencedByArticle :: Error
eTagReferencedByArticle = Error "tag is referenced by an article"


eWrongUsernameOrPassword :: Error
eWrongUsernameOrPassword = Error "wrong username or password"
