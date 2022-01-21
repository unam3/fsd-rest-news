{-# LANGUAGE OverloadedStrings #-}


module RestNews.DB.RequestRunner
    ( cantDecode
    , cantDecodeS
    , cantDecodeBS
    , runSession
    --, filterOutNothing, collectFields, testQuery, testFieldNames, parseParams, AuthorIdRequest (..)
    ) where

import qualified RestNews.DB.ProcessRequest as PR

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode)
import qualified Data.ByteString as B
import Data.ByteString.Lazy.UTF8 (ByteString, fromString)
import Data.Functor ((<&>))
import Data.Int (Int32)
import Hasql.Connection (Connection)
import qualified Hasql.Session as Session
import Network.HTTP.Types.URI (Query)
import Text.Read (readMaybe)
import qualified Util


cantDecodeS :: String
cantDecodeS = "Wrong parameters/parameters values"

cantDecodeBS :: ByteString
cantDecodeBS = fromString cantDecodeS

cantDecode :: PR.HasqlSessionResults a
cantDecode = PR.H . Right $ Left cantDecodeBS


collectFields' ::
    [(B.ByteString, Maybe B.ByteString)]
    -> [B.ByteString]
    -> [(B.ByteString, Maybe B.ByteString)]
    -> Either B.ByteString [(B.ByteString, Maybe B.ByteString)]
collectFields' _ [] [] = Left "Collect no fields"
collectFields' _ [] collected = Right collected
collectFields' query (fieldName:otherFieldsToCollect) collected =
    case lookup fieldName query of
    Just fieldValue ->
        let newQuery = filter ((/=) fieldName . fst) query
            newCollected = (fieldName, fieldValue) : collected
        in collectFields' newQuery otherFieldsToCollect newCollected
    Nothing -> Left $ B.append "Query has no required parameter: " fieldName

collectFields ::
    [(B.ByteString, Maybe B.ByteString)]
    -> [B.ByteString]
    -> Either B.ByteString [(B.ByteString, Maybe B.ByteString)]
collectFields query fieldsToCollect = collectFields' query fieldsToCollect []

filterOutNothing :: [(B.ByteString, Maybe B.ByteString)] -> Either B.ByteString [(B.ByteString, Maybe B.ByteString)]
filterOutNothing fields = case filter ((/=) Nothing . snd) fields of
    [] -> Left "All fields with Nothing were filtered. No fields with value to return."
    nonEmptyList -> Right nonEmptyList

class FromQuery a where
    parseParams :: Query -> Either String a

newtype AuthorIdRequest = AuthorIdRequest {
    author_id :: Int32
} deriving (Show)

testQuery :: [(B.ByteString, Maybe B.ByteString)]
testQuery = ([("author_id", Just "12"), ("abyr", Nothing), ("ad", Nothing), ("abyr", Just "Valg3"), ("meh", Nothing)])

testFieldNames :: [B.ByteString]
testFieldNames = ["meh", "abyr"]

-- (Right . lookup (last testFieldNames)) =<< filterOutNothing =<< collectFields testQuery testFieldNames

instance FromQuery AuthorIdRequest where
    parseParams query =
        let fieldsToCollect = ["author_id"]
        in case ((Right . lookup "author_id") =<< filterOutNothing =<< collectFields query fieldsToCollect) of
            Right Nothing -> Left "Query has no required parameter with a value: author_id"
            Right (Just (Just fieldValue)) -> case readMaybe $ show fieldValue of
                Just author_id -> Right $ AuthorIdRequest author_id
                Nothing -> Left $ "Wrong author_id value: " ++ show fieldValue


runSession ::
    Connection
    -> ByteString
    -> Query
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
    query
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
                <&> \(PR.H wrappedEither) -> PR.H $ (fmap $ fmap (const "cookies are baked")) wrappedEither
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
        "getArticlesBySubstring" -> runSessionWithJSON PR.getArticlesBySubstring
        "getArticlesSortedByPhotosNumber" -> runSessionWithJSON PR.getArticlesSortedByPhotosNumber
        "getArticlesSortedByCreationDate" -> runSessionWithJSON PR.getArticlesSortedByCreationDate
        "getArticlesSortedByAuthor" -> runSessionWithJSON PR.getArticlesSortedByAuthor
        "getArticlesSortedByCategory" -> runSessionWithJSON PR.getArticlesSortedByCategory
        "getArticlesFilteredByCreationDate" -> runSessionWithJSON PR.getArticlesFilteredByCreationDate
        "getArticlesCreatedBeforeDate" -> runSessionWithJSON PR.getArticlesCreatedBeforeDate
        "getArticlesCreatedAfterDate" -> runSessionWithJSON PR.getArticlesCreatedAfterDate
        nonMatched -> pure $ PR.H $ Left nonMatched
