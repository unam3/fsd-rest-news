{-# LANGUAGE OverloadedStrings  #-}

module RestNewsSpec where

import Control.Exception
import Control.Monad (void)
--import Network.Wai (responseLBS)
--import Network.Wai.Internal (ResponseReceived(..))
import Network.Wai.Handler.Warp (run)
import qualified Network.HTTP.Types as H
import System.Log.Logger (Priority (DEBUG, ERROR), debugM, errorM, setLevel, traplogging, updateGlobalLogger)
import System.Process (readProcess)
import System.Exit
import Test.Hspec

import RestNews (runWarp)
import qualified RestNews.Logger as L

getSession :: IO String
getSession =
    let {
        ioResponse = readProcess
            "curl"
            [
                "-s",
                "-i",
                "-X", "POST",
                "-d", "{\"username\": \"username\", \"password\": \"12345\"}",
                "http://0.0.0.0:8081/auth"
                ]
            [];
        session = ioResponse
            >>= pure
            . init
            . drop 20
            -- "Set-Cookie: SESSION=736eaf44239adb2e1e0f1cf52db8f3e4db4362fed5dc9ba\r"
            . (!! 4)
            . lines;
    } in session

curl :: String -> String -> String -> String -> IO String
curl method session dashDData url =
    let {
        initialData = 
            [
                "-s",
                "-X", method,
                url
                ];
        dataWithCookies = 
            (case session of
                [] -> initialData
                _ -> "-H" : ("Cookie: SESSION=" ++ session) : initialData);
        options =
            (case dashDData of
                [] -> dataWithCookies
                _ -> "-d" : dashDData : dataWithCookies);
        ioResponse = readProcess
            "curl"
            options
            [];
    } in ioResponse

auth :: IO String
auth = curl
    "POST"
    []
    "{\"username\": \"username\", \"password\": \"12345\"}"
    "http://0.0.0.0:8081/auth"

authMustFail :: IO String
authMustFail = curl
    "POST"
    []
    "{\"username\": \"non-existent-username\", \"password\": \"12345\"}"
    "http://0.0.0.0:8081/auth"

createUser :: IO String
createUser = curl
    "POST"
    []
    "{\"username\": \"createUserTest2\", \"password\": \"check, indeed\", \"name\": \"name\", \"surname\": \"surname\", \"avatar\": \"asd\"}"
    "http://0.0.0.0:8081/users"

getUser :: String -> IO String
getUser session = curl
    "GET"
    session
    []
    "http://0.0.0.0:8081/users"

replaceComasWithNewlines :: String -> String
replaceComasWithNewlines = map (
    \ char -> if char == ','
        then '\n'
        else char
        )

promoteUserToAuthor :: String -> String -> IO String
promoteUserToAuthor params session = curl
    "POST"
    session
    params
    "http://0.0.0.0:8081/authors"

getAuthor :: String -> String -> IO String
getAuthor params session = curl
    "GET"
    session
    params
    "http://0.0.0.0:8081/authors"

editAuthor :: String -> String -> IO String
editAuthor params session = curl
    "PATCH"
    session
    params
    "http://0.0.0.0:8081/authors"

deleteAuthorRole :: String -> String -> IO String
deleteAuthorRole params session = curl
    "DELETE"
    session
    params
    "http://0.0.0.0:8081/authors"

deleteUser :: String -> String -> IO String
deleteUser params session = curl
    "DELETE"
    session
    params
    "http://0.0.0.0:8081/users"

createCategory :: String -> String -> IO String
createCategory params session = curl
    "POST"
    session
    params
    "http://0.0.0.0:8081/categories"

getCategory :: String -> IO String
getCategory params = curl
    "GET"
    []
    params
    "http://0.0.0.0:8081/categories"

updateCategory :: String -> String -> IO String
updateCategory params session = curl
    "PATCH"
    session
    params
    "http://0.0.0.0:8081/categories"

deleteCategory :: String -> String -> IO String
deleteCategory params session = curl
    "DELETE"
    session
    params
    "http://0.0.0.0:8081/categories"

createTag :: String -> String -> IO String
createTag params session = curl
    "POST"
    session
    params
    "http://0.0.0.0:8081/tags"

editTag :: String -> String -> IO String
editTag params session = curl
    "PATCH"
    session
    params
    "http://0.0.0.0:8081/tags"

getTag :: String -> IO String
getTag params = curl
    "GET"
    []
    params
    "http://0.0.0.0:8081/tags"

deleteTag :: String -> String -> IO String
deleteTag params session = curl
    "DELETE"
    session
    params
    "http://0.0.0.0:8081/tags"

createArticleDraft :: String -> String -> IO String
createArticleDraft params session = curl
    "POST"
    session
    params
    "http://0.0.0.0:8081/articles"

editArticleDraft :: String -> String -> IO String
editArticleDraft params session = curl
    "PATCH"
    session
    params
    "http://0.0.0.0:8081/articles"

getArticleDraft :: String -> String -> IO String
getArticleDraft params session = curl
    "GET"
    session
    params
    "http://0.0.0.0:8081/articles"

publishArticleDraft :: String -> String -> IO String
publishArticleDraft params session = curl
    "POST"
    session
    params
    "http://0.0.0.0:8081/articles"

deleteArticleDraft :: String -> String -> IO String
deleteArticleDraft params session = curl
    "DELETE"
    session
    params
    "http://0.0.0.0:8081/articles"

createComment :: String -> String -> IO String
createComment params session = curl
    "POST"
    session
    params
    "http://0.0.0.0:8081/comments"

getArticleComments :: String -> IO String
getArticleComments params = curl
    "GET"
    []
    params
    "http://0.0.0.0:8081/comments"

deleteComment :: String -> String -> IO String
deleteComment params session = curl
    "DELETE"
    session
    params
    "http://0.0.0.0:8081/comments"


runStub _ _ = pure ()

spec :: Spec
spec = do
    --describe "restAPI" $ do
    describe "runWarp" $ do
        it "exit with success if arguments are ok"
            $ do 
                eitherExitCode <- try 
                    (L.withLogger 
                        (L.Config
                            DEBUG
                            (\ _ -> return ())
                            (\ _ -> return ())
                            (\ _ -> return ())
                        )
                        (\ loggerH ->
                            runWarp
                                loggerH
                                runStub
                                ["8081", "localhost", "5432", "rest-news-user", "rest", "rest-news-test"]
                        )
                    ) :: IO (Either SomeException ())
                shouldBe
                    (show eitherExitCode)
                    (show (Left $ toException ExitSuccess :: Either SomeException ()))

        it "exit with failure if wrong number of arguments"
            $ do 
                eitherExitCode <- try 
                    (L.withLogger 
                        (L.Config
                            DEBUG
                            (\ _ -> return ())
                            (\ _ -> return ())
                            (\ _ -> return ())
                        )
                        (\ loggerH ->
                            runWarp
                                loggerH
                                runStub
                                ["8081", "localhost", "5432", "rest-news-user", "rest"]
                        )
                    ) :: IO (Either SomeException ())
                shouldBe
                    (show eitherExitCode)
                    (show (Left $ toException (ExitFailure 1) :: Either SomeException ()))
