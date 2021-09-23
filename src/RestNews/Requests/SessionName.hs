{-# LANGUAGE OverloadedStrings #-}

module RestNews.Requests.SessionName
    (
    getSessionName
    , noSuchEndpointS
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text)

noSuchEndpointS :: String
noSuchEndpointS = "No such endpoint"

getSessionName :: ([Text], ByteString) -> Either String String
getSessionName pathAndMethod = case pathAndMethod of
    (["auth"], "POST") -> Right "auth"
    (["users"], "POST") -> Right "createUser"
    (["users"], "GET") -> Right "getUser"
    (["users"], "DELETE") -> Right "deleteUser"
    (["authors"], "POST") -> Right "promoteUserToAuthor"
    (["authors"], "PATCH") -> Right "editAuthor"
    (["authors"], "GET") -> Right "getAuthor"
    (["authors"], "DELETE") -> Right "deleteAuthorRole"
    (["categories"], "POST") -> Right "createCategory"
    (["categories"], "PATCH") -> Right "updateCategory"
    (["categories"], "GET") -> Right "getCategory"
    (["categories"], "DELETE") -> Right "deleteCategory"
    (["tags"], "POST") -> Right "createTag"
    (["tags"], "PATCH") -> Right "editTag"
    (["tags"], "GET") -> Right "getTag"
    (["tags"], "DELETE") -> Right "deleteTag"
    (["comments"], "POST") -> Right "createComment"
    (["comments"], "GET") -> Right "getArticleComments"
    (["comments"], "DELETE") -> Right "deleteComment"
    (["articles"], "POST") -> Right "createArticleDraft"
    (["articles"], "PATCH") -> Right "editArticleDraft"
    (["articles"], "GET") -> Right "getArticleDraft"
    (["articles"], "DELETE") -> Right "deleteArticleDraft"
    (["articles", "publish"], "POST") -> Right "publishArticleDraft"
    (["articles", "category"], "GET") -> Right "getArticlesByCategoryId"
    (["articles", "tag"], "GET") -> Right "getArticlesByTagId"
    (["articles", "tags__any"], "GET") -> Right "getArticlesByAnyTagId"
    (["articles", "tags__all"], "GET") -> Right "getArticlesByAllTagId"
    (["articles", "in__title"], "GET") -> Right "getArticlesByTitlePart"
    (["articles", "in__content"], "GET") -> Right "getArticlesByContentPart"
    (["articles", "in__author_name"], "GET") -> Right "getArticlesByAuthorNamePart"
    (["articles", "byPhotosNumber"], "GET") -> Right "getArticlesSortedByPhotosNumber"
    (["articles", "byCreationDate"], "GET") -> Right "getArticlesSortedByCreationDate"
    (["articles", "sortByAuthor"], "GET") -> Right "getArticlesSortedByAuthor"
    (["articles", "sortByCategory"], "GET") -> Right "getArticlesSortedByCategory"
    (["articles", "createdAt"], "GET") -> Right "getArticlesFilteredByCreationDate"
    (["articles", "createdBefore"], "GET") -> Right "getArticlesCreatedBeforeDate"
    (["articles", "createdAfter"], "GET") -> Right "getArticlesCreatedAfterDate"
    _ -> Left noSuchEndpointS
