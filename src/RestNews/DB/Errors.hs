{-# LANGUAGE DerivingVia #-}
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
    , eNoSuchCategory
    , eNoSuchComment
    , eNoSuchTag
    , eParentCategoryDoesNotExist
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

import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.ByteString.Internal (unpackChars)
import Data.Text (Text, append)
import Prelude hiding (error)

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


newtype MustNotBeNegative =
    MustNotBeNegative {error :: Text}
        deriving (Show)

instance ToJSON MustNotBeNegative where
    toJSON (MustNotBeNegative error')
        = object ["error" .= error']

eNegativeOffset :: MustNotBeNegative
eNegativeOffset = MustNotBeNegative "\\\"offset\\\" must not be negative"


newtype FieldMaxBoundOverflow =
    FieldMaxBoundOverflow {error :: Text}
        deriving (Show)
        deriving (ToJSON) via MustNotBeNegative

makeFieldMaxBoundOverflow :: Text -> FieldMaxBoundOverflow
makeFieldMaxBoundOverflow fieldName = FieldMaxBoundOverflow
    $ append fieldName " length must be 80 characters at most\"}"

eArticleTitleMaxBoundOverflow :: FieldMaxBoundOverflow
eArticleTitleMaxBoundOverflow = makeFieldMaxBoundOverflow ("article_title" :: Text)

eNameAndSurnameMaxBoundOverflow :: FieldMaxBoundOverflow
eNameAndSurnameMaxBoundOverflow = makeFieldMaxBoundOverflow "name and surname"

eCategoryNameMaxBoundOverflow :: FieldMaxBoundOverflow
eCategoryNameMaxBoundOverflow = makeFieldMaxBoundOverflow "category name"

eTagNameMaxBoundOverflow :: FieldMaxBoundOverflow
eTagNameMaxBoundOverflow = makeFieldMaxBoundOverflow "tag_name"


newtype DoesNotExist =
    DoesNotExist {error :: Text}
        deriving (Show)
        deriving (ToJSON) via MustNotBeNegative

makeDoesNotExist :: Text -> DoesNotExist
makeDoesNotExist thing = DoesNotExist
    $ append thing " does not exist"

eSuchAuthorDoesNotExist :: DoesNotExist
eSuchAuthorDoesNotExist = makeDoesNotExist "such author"

eSuchUserDoesNotExist :: DoesNotExist
eSuchUserDoesNotExist = makeDoesNotExist "such user"

eParentCategoryDoesNotExist :: DoesNotExist
eParentCategoryDoesNotExist = makeDoesNotExist "parent category"


newtype AlreadyExist =
    AlreadyExist {error :: Text}
        deriving (Show)
        deriving (ToJSON) via MustNotBeNegative

makeAlreadyExist :: Text -> AlreadyExist
makeAlreadyExist thing = AlreadyExist
    $ append thing " already exist"

eTagWithSuchNameAlreadyExist :: AlreadyExist
eTagWithSuchNameAlreadyExist = makeAlreadyExist "tag with such name"

eUserWithSuchUsernameAlreadyExist :: AlreadyExist
eUserWithSuchUsernameAlreadyExist = makeAlreadyExist "user with such username"


newtype UserAlreadyAuthor =
    UserAlreadyAuthor {error :: Text}
        deriving (Show)
        deriving (ToJSON) via MustNotBeNegative

eSuchUserAlreadyAuthor :: UserAlreadyAuthor
eSuchUserAlreadyAuthor = UserAlreadyAuthor "such user is already an author"


newtype CategoryInUse =
    CategoryInUse {error :: Text}
        deriving (Show)
        deriving (ToJSON) via MustNotBeNegative

eCategoryInUse :: CategoryInUse
eCategoryInUse = CategoryInUse "category is in use"


newtype NoSuchThing =
    NoSuchThing {error :: Text}
        deriving (Show)
        deriving (ToJSON) via MustNotBeNegative


makeNoSuchThing :: Text -> NoSuchThing
makeNoSuchThing thing = NoSuchThing
    $ append "no such " thing

eNoSuchArticle :: NoSuchThing
eNoSuchArticle = makeNoSuchThing "article"

eNoSuchCategory :: NoSuchThing
eNoSuchCategory = makeNoSuchThing "category"

eNoSuchComment :: NoSuchThing
eNoSuchComment = makeNoSuchThing "comment"

eNoSuchTag :: NoSuchThing
eNoSuchTag = makeNoSuchThing "tag"


newtype TagReferencedByArticle =
    TagReferencedByArticle {error :: Text}
        deriving (Show)
        deriving (ToJSON) via MustNotBeNegative


eTagReferencedByArticle :: TagReferencedByArticle
eTagReferencedByArticle = TagReferencedByArticle "tag is referenced by an article"


newtype WrongUsernameOrPassword =
    WrongUsernameOrPassword {error :: Text}
        deriving (Show)
        deriving (ToJSON) via MustNotBeNegative

eWrongUsernameOrPassword :: WrongUsernameOrPassword
eWrongUsernameOrPassword = WrongUsernameOrPassword "wrong username or password"
