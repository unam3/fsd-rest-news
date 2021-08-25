{-# LANGUAGE OverloadedStrings  #-}

module RestNews.Requests.PrerequisitesCheck
    (
    Params(..),
    getPrerequisitesCheck,
    noSuchEndpoint,
    ) where

import qualified Data.ByteString as BS
import Data.Text (Text)


noSuchEndpointS :: String
noSuchEndpointS = "No such endpoint"

noSuchEndpoint :: Either String String
noSuchEndpoint = Left noSuchEndpointS

passSessionNameIf :: String -> Bool -> Either String String
passSessionNameIf sessionName condition = if condition
    then Right sessionName
    else noSuchEndpoint


data Params = Params {
    isAdmin :: Bool,
    hasUserId :: Bool,
    hasAuthorId :: Bool
} deriving Show

class CheckMethods a where
    passSessionNameIfHasUserId :: a -> String -> Either String String
    passSessionNameIfHasAuthorId :: a -> String -> Either String String
    passSessionNameIfAdmin :: a -> String -> Either String String

instance CheckMethods Params where
    passSessionNameIfHasUserId reqAndParams sessionName = passSessionNameIf sessionName $ hasUserId reqAndParams
    passSessionNameIfHasAuthorId reqAndParams sessionName = passSessionNameIf sessionName $ hasAuthorId reqAndParams
    passSessionNameIfAdmin reqAndParams sessionName = passSessionNameIf sessionName $ isAdmin reqAndParams


getPrerequisitesCheck :: ([Text], BS.ByteString) -> Either String (Params -> Either String String)
getPrerequisitesCheck pathAndMethod = case pathAndMethod of
    (["auth"], "POST") -> Right . const $ Right "auth"
    (["users"], "POST") -> Right . const $ Right "createUser"
    (["users"], "GET") -> Right (`passSessionNameIfHasUserId` "getUser")
    (["users"], "DELETE") -> Right (`passSessionNameIfAdmin` "deleteUser")
    (["articles"], "POST") -> Right (`passSessionNameIfHasAuthorId` "createArticleDraft")
    (["articles"], "PATCH") -> Right (`passSessionNameIfHasAuthorId` "editArticleDraft")
    (["articles"], "GET") -> Right (`passSessionNameIfHasAuthorId` "getArticleDraft")
    (["articles"], "DELETE") -> Right (`passSessionNameIfHasAuthorId` "deleteArticleDraft")
    (["articles", "publish"], "POST") -> Right (`passSessionNameIfHasAuthorId` "publishArticleDraft")
    (["authors"], "POST") -> Right (`passSessionNameIfAdmin` "promoteUserToAuthor")
    (["authors"], "PATCH") -> Right (`passSessionNameIfAdmin` "editAuthor")
    (["authors"], "GET") -> Right (`passSessionNameIfAdmin` "getAuthor")
    (["authors"], "DELETE") -> Right (`passSessionNameIfAdmin` "deleteAuthorRole")
    (["categories"], "POST") -> Right (`passSessionNameIfAdmin` "createCategory")
    (["categories"], "PATCH") -> Right (`passSessionNameIfAdmin` "updateCategory")
    (["categories"], "GET") -> Right . const $ Right "getCategory"
    (["categories"], "DELETE") -> Right (`passSessionNameIfAdmin` "deleteCategory")
    (["tags"], "POST") -> Right (`passSessionNameIfAdmin` "createTag")
    (["tags"], "PATCH") -> Right (`passSessionNameIfAdmin` "editTag")
    (["tags"], "GET") -> Right . const $ Right "getTag"
    (["tags"], "DELETE") -> Right (`passSessionNameIfAdmin` "deleteTag")
    (["comments"], "POST") -> Right (`passSessionNameIfHasUserId` "createComment")
    (["comments"], "GET") -> Right . const $ Right "getArticleComments"
    (["comments"], "DELETE") -> Right (`passSessionNameIfHasUserId` "deleteComment")
    (["articles", "category"], "GET") -> Right . const $ Right "getArticlesByCategoryId"
    (["articles", "tag"], "GET") -> Right . const $ Right "getArticlesByTagId"
    (["articles", "tags__any"], "GET") -> Right . const $ Right "getArticlesByAnyTagId"
    (["articles", "tags__all"], "GET") -> Right . const $ Right "getArticlesByAllTagId"
    (["articles", "in__title"], "GET") -> Right . const $ Right "getArticlesByTitlePart"
    (["articles", "in__content"], "GET") -> Right . const $ Right "getArticlesByContentPart"
    (["articles", "in__author_name"], "GET") -> Right . const $ Right "getArticlesByAuthorNamePart"
    (["articles", "byPhotosNumber"], "GET") -> Right . const $ Right "getArticlesSortedByPhotosNumber"
    (["articles", "byCreationDate"], "GET") -> Right . const $ Right "getArticlesSortedByCreationDate"
    (["articles", "sortByAuthor"], "GET") -> Right . const $ Right "getArticlesSortedByAuthor"
    (["articles", "sortByCategory"], "GET") -> Right . const $ Right "getArticlesSortedByCategory"
    (["articles", "createdAt"], "GET") -> Right . const $ Right "getArticlesFilteredByCreationDate"
    (["articles", "createdBefore"], "GET") -> Right . const $ Right "getArticlesCreatedBeforeDate"
    (["articles", "createdAfter"], "GET") -> Right . const $ Right "getArticlesCreatedAfterDate"
    _ -> Left noSuchEndpointS
