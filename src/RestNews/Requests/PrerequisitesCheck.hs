{-# LANGUAGE OverloadedStrings #-}

module RestNews.Requests.PrerequisitesCheck
    (
    Params(..),
    prerequisitesCheck,
    noSuchEndpoint,
    ) where

import Data.Aeson (encode)
import Data.ByteString.Lazy.UTF8 (toString)

import RestNews.DB.Errors (eNoSuchEndpoint)


noSuchEndpoint :: Either String String
noSuchEndpoint = Left . toString $ encode eNoSuchEndpoint

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


prerequisitesCheck :: Params -> String -> Either String String
prerequisitesCheck params sessionName =
    let passWithoutCheck = Right sessionName
        passSessionNameIfHasUserId' = passSessionNameIfHasUserId params sessionName
        passSessionNameIfAdmin' = passSessionNameIfAdmin params sessionName
        passSessionNameIfHasAuthorId' = passSessionNameIfHasAuthorId params sessionName
    in case sessionName of
    "auth"                              -> passWithoutCheck
    "createUser"                        -> passWithoutCheck
    "getUser"                           -> passSessionNameIfHasUserId'
    "deleteUser"                        -> passSessionNameIfAdmin'
    "createArticleDraft"                -> passSessionNameIfHasAuthorId'
    "editArticleDraft"                  -> passSessionNameIfHasAuthorId'
    "getArticleDraft"                   -> passSessionNameIfHasAuthorId'
    "deleteArticleDraft"                -> passSessionNameIfHasAuthorId'
    "publishArticleDraft"               -> passSessionNameIfHasAuthorId'
    "promoteUserToAuthor"               -> passSessionNameIfAdmin'
    "editAuthor"                        -> passSessionNameIfAdmin'
    "getAuthor"                         -> passSessionNameIfAdmin'
    "deleteAuthorRole"                  -> passSessionNameIfAdmin'
    "createCategory"                    -> passSessionNameIfAdmin'
    "updateCategory"                    -> passSessionNameIfAdmin'
    "getCategory"                       -> passWithoutCheck
    "deleteCategory"                    -> passSessionNameIfAdmin'
    "createTag"                         -> passSessionNameIfAdmin'
    "editTag"                           -> passSessionNameIfAdmin'
    "getTag"                            -> passWithoutCheck
    "deleteTag"                         -> passSessionNameIfAdmin'
    "createComment"                     -> passSessionNameIfHasUserId'
    "getArticleComments"                -> passWithoutCheck
    "deleteComment"                     -> passSessionNameIfHasUserId'
    "getArticlesByCategoryId"           -> passWithoutCheck
    "getArticlesByTagId"                -> passWithoutCheck
    "getArticlesByAnyTagId"             -> passWithoutCheck
    "getArticlesByAllTagId"             -> passWithoutCheck
    "getArticlesByTitlePart"            -> passWithoutCheck
    "getArticlesByContentPart"          -> passWithoutCheck
    "getArticlesByAuthorNamePart"       -> passWithoutCheck
    "getArticlesBySubstring"            -> passWithoutCheck
    "getArticlesSortedByPhotosNumber"   -> passWithoutCheck
    "getArticlesSortedByCreationDate"   -> passWithoutCheck
    "getArticlesSortedByAuthor"         -> passWithoutCheck
    "getArticlesSortedByCategory"       -> passWithoutCheck
    "getArticlesFilteredByCreationDate" -> passWithoutCheck
    "getArticlesCreatedBeforeDate"      -> passWithoutCheck
    "getArticlesCreatedAfterDate"       -> passWithoutCheck
    _ -> noSuchEndpoint
