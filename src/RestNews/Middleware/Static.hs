{-# LANGUAGE OverloadedStrings  #-}

module RestNews.Middleware.Static
    ( router
    ) where

import Network.HTTP.Types (RequestHeaders)
import Network.Wai (Application, Request, pathInfo)
import Network.Wai.Application.Static (defaultWebAppSettings, staticApp)
import Network.Wai.Middleware.Rewrite (PathsAndQueries, rewritePureWithQueries)

isRequestToStatic :: Request -> Bool
isRequestToStatic request =
    let pathTextChunks = pathInfo request
        isRequestPathNotEmpty = not $ null pathTextChunks
    in isRequestPathNotEmpty && head pathTextChunks == "static"

removeStaticFromURI :: PathsAndQueries -> RequestHeaders -> PathsAndQueries
removeStaticFromURI ("static":otherPathPieces, queries) _ = (otherPathPieces, queries)
removeStaticFromURI pathsAndQueries _ = pathsAndQueries

rewrite :: Application -> Application
rewrite = rewritePureWithQueries removeStaticFromURI

router :: Application -> Application
router app request respond =
    if isRequestToStatic request
        then rewrite (staticApp (defaultWebAppSettings "static")) request respond
        else app request respond
