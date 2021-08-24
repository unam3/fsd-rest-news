{-# LANGUAGE OverloadedStrings #-}

module RestNews.Requests.PrerequisitesCheck
  ( Params(..)
  , getPrerequisitesCheck
  , noSuchEndpoint
  ) where

import qualified Data.ByteString as BS
import Data.Text (Text)

noSuchEndpointS :: String
noSuchEndpointS = "No such endpoint"

noSuchEndpoint :: Either String String
noSuchEndpoint = Left noSuchEndpointS

passSessionNameIf :: String -> Bool -> Either String String
passSessionNameIf sessionName condition =
  if condition
    then Right sessionName
    else noSuchEndpoint

data Params =
  Params
    { isAdmin :: Bool
    , hasUserId :: Bool
    , hasAuthorId :: Bool
    }
  deriving (Show)

class CheckMethods a where
  passSessionNameIfHasUserId :: a -> String -> Either String String
  passSessionNameIfHasAuthorId :: a -> String -> Either String String
  passSessionNameIfAdmin :: a -> String -> Either String String

instance CheckMethods Params where
  passSessionNameIfHasUserId reqAndParams sessionName =
    passSessionNameIf sessionName $ hasUserId reqAndParams
  passSessionNameIfHasAuthorId reqAndParams sessionName =
    passSessionNameIf sessionName $ hasAuthorId reqAndParams
  passSessionNameIfAdmin reqAndParams sessionName =
    passSessionNameIf sessionName $ isAdmin reqAndParams

getPrerequisitesCheck ::
     ([Text], BS.ByteString) -> Maybe (Params -> Either String String)
getPrerequisitesCheck pathAndMethod =
  case pathAndMethod of
    (["auth"], "POST") -> Just . const $ Right "auth"
    (["users"], "POST") -> Just . const $ Right "createUser"
    (["users"], "GET") -> Just (`passSessionNameIfHasUserId` "getUser")
    (["users"], "DELETE") -> Just (`passSessionNameIfAdmin` "deleteUser")
    (["articles"], "POST") ->
      Just (`passSessionNameIfHasAuthorId` "createArticleDraft")
    (["articles"], "PATCH") ->
      Just (`passSessionNameIfHasAuthorId` "editArticleDraft")
    (["articles"], "GET") ->
      Just (`passSessionNameIfHasAuthorId` "getArticleDraft")
    (["articles"], "DELETE") ->
      Just (`passSessionNameIfHasAuthorId` "deleteArticleDraft")
    (["articles", "publish"], "POST") ->
      Just (`passSessionNameIfHasAuthorId` "publishArticleDraft")
    (["authors"], "POST") ->
      Just (`passSessionNameIfAdmin` "promoteUserToAuthor")
    (["authors"], "PATCH") -> Just (`passSessionNameIfAdmin` "editAuthor")
    (["authors"], "GET") -> Just (`passSessionNameIfAdmin` "getAuthor")
    (["authors"], "DELETE") ->
      Just (`passSessionNameIfAdmin` "deleteAuthorRole")
    (["categories"], "POST") -> Just (`passSessionNameIfAdmin` "createCategory")
    (["categories"], "PATCH") ->
      Just (`passSessionNameIfAdmin` "updateCategory")
    (["categories"], "GET") -> Just . const $ Right "getCategory"
    (["categories"], "DELETE") ->
      Just (`passSessionNameIfAdmin` "deleteCategory")
    (["tags"], "POST") -> Just (`passSessionNameIfAdmin` "createTag")
    (["tags"], "PATCH") -> Just (`passSessionNameIfAdmin` "editTag")
    (["tags"], "GET") -> Just . const $ Right "getTag"
    (["tags"], "DELETE") -> Just (`passSessionNameIfAdmin` "deleteTag")
    (["comments"], "POST") ->
      Just (`passSessionNameIfHasUserId` "createComment")
    (["comments"], "GET") -> Just . const $ Right "getArticleComments"
    (["comments"], "DELETE") ->
      Just (`passSessionNameIfHasUserId` "deleteComment")
    (["articles", "category"], "GET") ->
      Just . const $ Right "getArticlesByCategoryId"
    (["articles", "tag"], "GET") -> Just . const $ Right "getArticlesByTagId"
    (["articles", "tags__any"], "GET") ->
      Just . const $ Right "getArticlesByAnyTagId"
    (["articles", "tags__all"], "GET") ->
      Just . const $ Right "getArticlesByAllTagId"
    (["articles", "in__title"], "GET") ->
      Just . const $ Right "getArticlesByTitlePart"
    (["articles", "in__content"], "GET") ->
      Just . const $ Right "getArticlesByContentPart"
    (["articles", "in__author_name"], "GET") ->
      Just . const $ Right "getArticlesByAuthorNamePart"
    (["articles", "byPhotosNumber"], "GET") ->
      Just . const $ Right "getArticlesSortedByPhotosNumber"
    (["articles", "byCreationDate"], "GET") ->
      Just . const $ Right "getArticlesSortedByCreationDate"
    (["articles", "sortByAuthor"], "GET") ->
      Just . const $ Right "getArticlesSortedByAuthor"
    (["articles", "sortByCategory"], "GET") ->
      Just . const $ Right "getArticlesSortedByCategory"
    (["articles", "createdAt"], "GET") ->
      Just . const $ Right "getArticlesFilteredByCreationDate"
    (["articles", "createdBefore"], "GET") ->
      Just . const $ Right "getArticlesCreatedBeforeDate"
    (["articles", "createdAfter"], "GET") ->
      Just . const $ Right "getArticlesCreatedAfterDate"
    _ -> Nothing
