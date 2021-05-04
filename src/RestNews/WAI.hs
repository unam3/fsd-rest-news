module RestNews.WAI
    ( Config(..)
    , Handle(..)
    , withWAI
    ) where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Network.HTTP.Types.Method (Method)
import Network.Wai (Application, Request)


data Config a = Config {
    cRequestMethod :: Request -> Method,
    cPathInfo :: Request -> [Text],
    cStrictRequestBody :: Request -> IO ByteString
}

data Handle a = Handle {
    hRequestMethod :: Request -> Method,
    hPathInfo :: Request -> [Text],
    hStrictRequestBody :: Request -> IO ByteString
}

withWAI :: Config a -> (Handle a -> Application) -> Application
withWAI config f =
    f
        $ Handle
            (cRequestMethod config)
            (cPathInfo config)
            (cStrictRequestBody config)
