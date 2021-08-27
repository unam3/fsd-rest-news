{-# LANGUAGE OverloadedStrings  #-}

module RestNews.Requests.PrerequisitesCheck
    (
    Params(..),
    prerequisitesCheck,
    noSuchEndpoint,
    ) where

import RestNews.Requests.SessionName (noSuchEndpointS)

import qualified Data.ByteString as BS
import Data.Text (Text)


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


prerequisitesCheck :: Params -> String -> Either String String
prerequisitesCheck params sessionName =
    let passWithoutCheck = Right sessionName
        passSessionNameIfHasUserId' = Right $ passSessionNameIfHasUserId params sessionName
        passSessionNameIfAdmin' = Right $ passSessionNameIfAdmin params sessionName
        passSessionNameIfHasAuthorId' = Right $ passSessionNameIfHasAuthorId params sessionName
    in case sessionName of
    "auth"                              -> passWithoutCheck
    "createUser"                        -> passWithoutCheck
    "getArticlesByCategoryId"           -> passWithoutCheck
    "getArticlesByTagId"                -> passWithoutCheck
    "getArticlesByAnyTagId"             -> passWithoutCheck
    "getArticlesByAllTagId"             -> passWithoutCheck
    "getArticlesByTitlePart"            -> passWithoutCheck
    "getArticlesByContentPart"          -> passWithoutCheck
    "getArticlesByAuthorNamePart"       -> passWithoutCheck
    "getArticlesSortedByPhotosNumber"   -> passWithoutCheck
    "getArticlesSortedByCreationDate"   -> passWithoutCheck
    "getArticlesSortedByAuthor"         -> passWithoutCheck
    "getArticlesSortedByCategory"       -> passWithoutCheck
    "getArticlesFilteredByCreationDate" -> passWithoutCheck
    "getArticlesCreatedBeforeDate"      -> passWithoutCheck
    "getArticlesCreatedAfterDate"       -> passWithoutCheck
    _ -> Left noSuchEndpointS -- mb just noSuchEndpoint?
