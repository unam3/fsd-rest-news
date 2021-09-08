{-# LANGUAGE OverloadedStrings  #-}


module RestNews.DB.RequestRunner
    ( cantDecode
    , runSession
    ) where

import qualified RestNews.DB.ProcessRequest as PR

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode)
import Data.Bifunctor (first)
import Data.ByteString.Lazy.UTF8 (ByteString)
import Data.Functor ((<&>))
import Data.Int (Int32)
import Hasql.Connection (Connection)
import qualified Hasql.Session as Session
import qualified Util

cantDecode :: PR.HasqlSessionResults a
cantDecode = PR.H $ Left $ Right "Wrong parameters/parameters values"

runSession ::
    Connection
    -> ByteString
    -> (PR.HasqlSessionResults (Int32, Bool, Int32)
        -> IO (PR.HasqlSessionResults (Int32, Bool, Int32)))
    -> Int32
    -> Int32
    -> String
    -- -> IO (PR.HasqlSessionResults a)
    -> IO (PR.HasqlSessionResults ByteString)
runSession
    connection
    requestBody
    processCredentialsPartial
    sessionUserId
    sessionAuthorId
    sessionName = let {
        -- (Util.∘∘) == (.).(.)
        sessionRun = (Util.∘∘) liftIO  Session.run;
        runSessionWithJSON session = maybe (pure cantDecode) (session sessionRun connection) (decode requestBody);
        runSessionWithJSONAndArg session arg =
            let partiallyAppliedSession = session sessionRun connection
            in maybe (pure cantDecode) (`partiallyAppliedSession` arg) (decode requestBody);
    } in case sessionName of
        "auth" -> runSessionWithJSON PR.getCredentials
            >>= processCredentialsPartial
                <&> \(PR.H wrappedEither) -> PR.H $ fmap (const "cookies are baked") wrappedEither
        "createUser" -> runSessionWithJSON PR.createUser
        "getUser" -> PR.getUser sessionRun connection sessionUserId
        "deleteUser" -> runSessionWithJSON PR.deleteUser
        "promoteUserToAuthor" -> runSessionWithJSON PR.promoteUserToAuthor
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
        "createComment" -> runSessionWithJSONAndArg PR.createComment sessionUserId
        "deleteComment" -> runSessionWithJSONAndArg PR.deleteComment sessionUserId
        "getArticleComments" -> runSessionWithJSON PR.getArticleComments
        "createArticleDraft" -> runSessionWithJSONAndArg PR.createArticleDraft sessionAuthorId
        "editArticleDraft" -> runSessionWithJSONAndArg PR.editArticleDraft sessionAuthorId
        "publishArticleDraft" -> runSessionWithJSONAndArg PR.publishArticleDraft sessionAuthorId
        "getArticleDraft" -> runSessionWithJSONAndArg PR.getArticleDraft sessionAuthorId
        "deleteArticleDraft" -> runSessionWithJSONAndArg PR.deleteArticleDraft sessionAuthorId
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
        nonMatched -> pure $ PR.H $ Left $ Left nonMatched
