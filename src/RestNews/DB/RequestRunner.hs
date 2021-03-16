{-# LANGUAGE OverloadedStrings  #-}

module RestNews.DB.RequestRunner
    ( runSession
    ) where

import qualified RestNews.DB.ProcessRequest as PR

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode)
import Data.Bifunctor (first)
import Data.ByteString.Lazy.UTF8 (ByteString)
import Data.Int (Int32)
import Data.Maybe (fromJust)
import Hasql.Connection (Connection)
import qualified Hasql.Session as Session
import qualified Util


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
        -- (Util.∘∘) == (.).(.)
        sessionRun = (Util.∘∘) liftIO  Session.run;
        runSessionWithJSON session = session sessionRun connection . fromJust $ decode requestBody;
    } in case sessionName of
        "auth" -> runSessionWithJSON PR.getCredentials
            >>= processCredentialsPartial
            >>= pure . first (fmap $ const "cookies are baked")
        "createUser" -> runSessionWithJSON PR.createUser;
        "getUser" -> PR.getUser sessionRun connection sessionUserId
        "deleteUser" -> runSessionWithJSON PR.deleteUser
        "promoteUserToAuthor" -> runSessionWithJSON PR.promoteUserToAuthor;
        "editAuthor" -> runSessionWithJSON PR.editAuthor
        "getAuthor" -> runSessionWithJSON PR.getAuthor
        "deleteAuthorRole" -> runSessionWithJSON PR.deleteAuthorRole
        "createCategory" -> runSessionWithJSON PR.createCategory
        "updateCategory" -> runSessionWithJSON PR.updateCategory
        "getCategory" -> runSessionWithJSON PR.getCategory
        "deleteCategory" -> runSessionWithJSON PR.deleteCategory
        "createTag" -> runSessionWithJSON PR.createTag
        "editTag" -> runSessionWithJSON PR.editTag
        "getTag" -> runSessionWithJSON PR.getTag
        "deleteTag" -> runSessionWithJSON PR.deleteTag
        "createComment" -> runSessionWithJSON PR.createComment sessionUserId
        "deleteComment" -> runSessionWithJSON PR.deleteComment sessionUserId
        "getArticleComments" -> runSessionWithJSON PR.getArticleComments
        "createArticleDraft" -> runSessionWithJSON PR.createArticleDraft sessionAuthorId
        "editArticleDraft" -> runSessionWithJSON PR.editArticleDraft sessionAuthorId
        "publishArticleDraft" -> runSessionWithJSON PR.publishArticleDraft sessionAuthorId
        "getArticleDraft" -> runSessionWithJSON PR.getArticleDraft sessionAuthorId
        "deleteArticleDraft" -> runSessionWithJSON PR.deleteArticleDraft sessionAuthorId
        "getArticlesByCategoryId" -> runSessionWithJSON PR.getArticlesByCategoryId
        "getArticlesByTagId" -> runSessionWithJSON PR.getArticlesByTagId
        "getArticlesByAnyTagId" -> runSessionWithJSON PR.getArticlesByAnyTagId
        "getArticlesByAllTagId" -> runSessionWithJSON PR.getArticlesByAllTagId
        "getArticlesByTitlePart" -> runSessionWithJSON PR.getArticlesByTitlePart
        "getArticlesByContentPart" -> runSessionWithJSON PR.getArticlesByContentPart
        "getArticlesByAuthorNamePart" -> runSessionWithJSON PR.getArticlesByAuthorNamePart
        "getArticlesSortedByPhotosNumber" -> runSessionWithJSON PR.getArticlesSortedByPhotosNumber
        "getArticlesSortedByCreationDate" -> runSessionWithJSON PR.getArticlesSortedByCreationDate
        "getArticlesSortedByAuthor" -> runSessionWithJSON PR.getArticlesSortedByAuthor
        "getArticlesSortedByCategory" -> runSessionWithJSON PR.getArticlesSortedByCategory
        "getArticlesFilteredByCreationDate" -> runSessionWithJSON PR.getArticlesFilteredByCreationDate
        "getArticlesCreatedBeforeDate" -> runSessionWithJSON PR.getArticlesCreatedBeforeDate
        "getArticlesCreatedAfterDate" -> runSessionWithJSON PR.getArticlesCreatedAfterDate
        nonMatched -> pure (
            Left nonMatched,
            Nothing
            )
