{-# LANGUAGE OverloadedStrings  #-}

module HasqlSessionsRunner
    ( runSession
    ) where

import qualified HasqlSessions as HSS

import Data.Aeson (decode)
import Data.Bifunctor (first)
import Data.ByteString.Lazy.UTF8 (ByteString)
import Data.Int (Int32)
import Data.Maybe (fromJust)
import Hasql.Connection (Connection)


runSession ::
    Connection
    -> ByteString
    -> ((Either String (Int32, Bool, Int32), Maybe ByteString)
        -> IO (Either String (Int32, Bool, Int32), Maybe ByteString))
    -> Int32
    -> Int32
    -> String
    -> IO (Either String ByteString, Maybe ByteString)
runSession
    connection
    requestBody
    processCredentialsPartial
    sessionUserId
    sessionAuthorId
    sessionName = let {
        runSessionWithJSON session = session connection . fromJust $ decode requestBody;
    } in case sessionName of
        "auth" -> runSessionWithJSON HSS.getCredentials
            >>= processCredentialsPartial
            >>= pure . first (fmap $ const "cookies are baked")
        "createUser" -> runSessionWithJSON HSS.createUser;
        "getUser" -> HSS.getUser connection sessionUserId
        "deleteUser" -> runSessionWithJSON HSS.deleteUser
        "promoteUserToAuthor" -> runSessionWithJSON HSS.promoteUserToAuthor;
        "editAuthor" -> runSessionWithJSON HSS.editAuthor
        "getAuthor" -> runSessionWithJSON HSS.getAuthor
        "deleteAuthorRole" -> runSessionWithJSON HSS.deleteAuthorRole
        "createCategory" -> runSessionWithJSON HSS.createCategory
        "updateCategory" -> runSessionWithJSON HSS.updateCategory
        "getCategory" -> runSessionWithJSON HSS.getCategory
        "deleteCategory" -> runSessionWithJSON HSS.deleteCategory
        "createTag" -> runSessionWithJSON HSS.createTag
        "editTag" -> runSessionWithJSON HSS.editTag
        "getTag" -> runSessionWithJSON HSS.getTag
        "deleteTag" -> runSessionWithJSON HSS.deleteTag
        "createComment" -> runSessionWithJSON HSS.createComment sessionUserId
        "deleteComment" -> runSessionWithJSON HSS.deleteComment sessionUserId
        "getArticleComments" -> runSessionWithJSON HSS.getArticleComments
        "createArticleDraft" -> runSessionWithJSON HSS.createArticleDraft sessionAuthorId
        "editArticleDraft" -> runSessionWithJSON HSS.editArticleDraft sessionAuthorId
        "publishArticleDraft" -> runSessionWithJSON HSS.publishArticleDraft sessionAuthorId
        "getArticleDraft" -> runSessionWithJSON HSS.getArticleDraft sessionAuthorId
        "deleteArticleDraft" -> runSessionWithJSON HSS.deleteArticleDraft sessionAuthorId
        "getArticlesByCategoryId" -> runSessionWithJSON HSS.getArticlesByCategoryId
        "getArticlesByTagId" -> runSessionWithJSON HSS.getArticlesByTagId
        "getArticlesByAnyTagId" -> runSessionWithJSON HSS.getArticlesByAnyTagId
        "getArticlesByAllTagId" -> runSessionWithJSON HSS.getArticlesByAllTagId
        "getArticlesByTitlePart" -> runSessionWithJSON HSS.getArticlesByTitlePart
        "getArticlesByContentPart" -> runSessionWithJSON HSS.getArticlesByContentPart
        "getArticlesByAuthorNamePart" -> runSessionWithJSON HSS.getArticlesByAuthorNamePart
        "getArticlesSortedByPhotosNumber" -> runSessionWithJSON HSS.getArticlesSortedByPhotosNumber
        "getArticlesSortedByCreationDate" -> runSessionWithJSON HSS.getArticlesSortedByCreationDate
        "getArticlesSortedByAuthor" -> runSessionWithJSON HSS.getArticlesSortedByAuthor
        "getArticlesSortedByCategory" -> runSessionWithJSON HSS.getArticlesSortedByCategory
        "getArticlesFilteredByCreationDate" -> runSessionWithJSON HSS.getArticlesFilteredByCreationDate
        "getArticlesCreatedBeforeDate" -> runSessionWithJSON HSS.getArticlesCreatedBeforeDate
        "getArticlesCreatedAfterDate" -> runSessionWithJSON HSS.getArticlesCreatedAfterDate
        nonMatched -> pure (
            Left nonMatched,
            Nothing
            )
