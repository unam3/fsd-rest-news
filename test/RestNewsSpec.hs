{-# LANGUAGE OverloadedStrings #-}

module RestNewsSpec where

import Control.Exception (try)
import Control.Monad (void)
import Data.Aeson (encode)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Functor ((<&>))
import Data.List (find, isPrefixOf, stripPrefix, uncons)
import Data.Maybe (fromMaybe)
import Database.PostgreSQL.Simple (ConnectInfo)
import Hasql.Connection (Connection, ConnectionError, Settings)
import Network.Wai (Application, Middleware, Request, pathInfo, requestMethod, strictRequestBody)
import Network.Wai.Handler.Warp (Port, testWithApplication, withApplication)
import Network.Wai.Internal (ResponseReceived (..))
import System.Log.Logger (Priority (DEBUG))
import System.Process (readProcess)
import Test.Hspec (Spec, afterAll, beforeAll, describe, it, runIO, shouldBe, shouldContain, shouldStartWith)

import RestNews
import qualified RestNews.Config as C
import RestNews.DB.Errors
import RestNews.DB.RequestRunner (cantDecodeBS)
import qualified RestNews.DBConnection as DBC
import qualified RestNews.Logger as L
import qualified RestNews.Middleware.Sessions as S
import qualified RestNews.WAI as WAI


getStringStartingWith :: String -> String -> Maybe String
getStringStartingWith stringToFind stringWithNewLines =  find (isPrefixOf stringToFind) $ lines stringWithNewLines

getCookieSession :: String -> Maybe String
getCookieSession response = getStringStartingWith "Set-Cookie:" response
    >>= stripPrefix "Set-Cookie: SESSION="
       <&> takeWhile (/= '\r')

getSession :: Int -> IO String
getSession port = do
    mbSession <- getCookieSession
            <$> readProcess
                "curl"
                [
                    "-s",
                    "-i",
                    "-X", "POST",
                    "-d", "{\"username\": \"username\", \"password\": \"12345\"}",
                    "http://0.0.0.0:" ++ show port ++ "/auth"
                    ]
                []
    case mbSession of
        Nothing -> error "response has no cookie session"
        Just session -> pure session

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


wrongPathRequest :: Int -> IO String
wrongPathRequest port = curl
    "GET"
    []
    []
    ("http://0.0.0.0:" ++ show port ++ "/wrongPath")

wrongCredsRequest :: Int -> IO String
wrongCredsRequest port = curl
    "GET"
    []
    []
    ("http://0.0.0.0:" ++ show port ++ "/users")

dbSessionErrorRequest :: Int -> IO String
dbSessionErrorRequest port = curl
    "POST"
    []
    "{\"username\": \"asdqasdqasdqasdqasdqasdqasdqasdqasdqasdqasdqasdqasdqasdqasdqasdqasdqasdqasdqasdqa0\", \"surname\": \"surname\", \"avatar\": \"asd\"}"
    ("http://0.0.0.0:" ++ show port ++ "/users")

auth :: Int -> IO String
auth port = curl
    "POST"
    []
    "{\"username\": \"username\", \"password\": \"12345\"}"
    ("http://0.0.0.0:" ++ show port ++ "/auth")

authMustFail :: Int -> IO String
authMustFail port = curl
    "POST"
    []
    "{\"username\": \"non-existent-username\", \"password\": \"12345\"}"
    ("http://0.0.0.0:" ++ show port ++ "/auth")

createUser :: Int -> IO String
createUser port = curl
    "POST"
    []
    "{\"username\": \"createUserTest2\", \"password\": \"check, indeed\", \"name\": \"name\", \"surname\": \"surname\", \"avatar\": \"asd\"}"
    ("http://0.0.0.0:" ++ show port ++ "/users")

getUser :: String -> Int -> IO String
getUser session port = curl
    "GET"
    session
    []
    ("http://0.0.0.0:" ++ show port ++ "/users")

replaceComasWithNewlines :: String -> String
replaceComasWithNewlines = map (
    \ char -> if char == ','
        then '\n'
        else char
        )

promoteUserToAuthor :: String -> String -> Int -> IO String
promoteUserToAuthor params session port = curl
    "POST"
    session
    params
    ("http://0.0.0.0:" ++ show port ++ "/authors")

getAuthor :: String -> String -> Int -> IO String
getAuthor params session port = curl
    "GET"
    session
    params
    ("http://0.0.0.0:" ++ show port ++ "/authors")

editAuthor :: String -> String -> Int -> IO String
editAuthor params session port = curl
    "PATCH"
    session
    params
    ("http://0.0.0.0:" ++ show port ++ "/authors")

deleteAuthorRole :: String -> String -> Int -> IO String
deleteAuthorRole params session port = curl
    "DELETE"
    session
    params
    ("http://0.0.0.0:" ++ show port ++ "/authors")

deleteUser :: String -> String -> Int -> IO String
deleteUser params session port = curl
    "DELETE"
    session
    params
    ("http://0.0.0.0:" ++ show port ++ "/users")

createCategory :: String -> String -> Int -> IO String
createCategory params session port = curl
    "POST"
    session
    params
    ("http://0.0.0.0:" ++ show port ++ "/categories")

getCategory :: String -> Int -> IO String
getCategory params port = curl
    "GET"
    []
    params
    ("http://0.0.0.0:" ++ show port ++ "/categories")

updateCategory :: String -> String -> Int -> IO String
updateCategory params session port = curl
    "PATCH"
    session
    params
    ("http://0.0.0.0:" ++ show port ++ "/categories")

deleteCategory :: String -> String -> Int -> IO String
deleteCategory params session port = curl
    "DELETE"
    session
    params
    ("http://0.0.0.0:" ++ show port ++ "/categories")

createTag :: String -> String -> Int -> IO String
createTag params session port = curl
    "POST"
    session
    params
    ("http://0.0.0.0:" ++ show port ++ "/tags")

editTag :: String -> String -> Int -> IO String
editTag params session port = curl
    "PATCH"
    session
    params
    ("http://0.0.0.0:" ++ show port ++ "/tags")

getTag :: String -> Int -> IO String
getTag params port = curl
    "GET"
    []
    params
    ("http://0.0.0.0:" ++ show port ++ "/tags")

deleteTag :: String -> String -> Int -> IO String
deleteTag params session port = curl
    "DELETE"
    session
    params
    ("http://0.0.0.0:" ++ show port ++ "/tags")

createArticleDraft :: String -> String -> Int -> IO String
createArticleDraft params session port = curl
    "POST"
    session
    params
    ("http://0.0.0.0:" ++ show port ++ "/articles")

editArticleDraft :: String -> String -> Int -> IO String
editArticleDraft params session port = curl
    "PATCH"
    session
    params
    ("http://0.0.0.0:" ++ show port ++ "/articles")

getArticleDraft :: String -> String -> Int -> IO String
getArticleDraft params session port = curl
    "GET"
    session
    params
    ("http://0.0.0.0:" ++ show port ++ "/articles")

publishArticleDraft :: String -> String -> Int -> IO String
publishArticleDraft params session port = curl
    "POST"
    session
    params
    ("http://0.0.0.0:" ++ show port ++ "/articles/publish")

deleteArticleDraft :: String -> String -> Int -> IO String
deleteArticleDraft params session port = curl
    "DELETE"
    session
    params
    ("http://0.0.0.0:" ++ show port ++ "/articles")

createComment :: String -> String -> Int -> IO String
createComment params session port = curl
    "POST"
    session
    params
    ("http://0.0.0.0:" ++ show port ++ "/comments")

getArticleComments :: String -> Int -> IO String
getArticleComments params port = curl
    "GET"
    []
    params
    ("http://0.0.0.0:" ++ show port ++ "/comments")

deleteComment :: String -> String -> Int -> IO String
deleteComment params session port = curl
    "DELETE"
    session
    params
    ("http://0.0.0.0:" ++ show port ++ "/comments")


withSessionStub :: Middleware
--withSessionStub app request respond = app request respond
withSessionStub app = app

maybeSessionMethodsStub :: Request -> Maybe (String -> IO (Maybe String), String -> String -> IO ())
maybeSessionMethodsStub _ = Nothing

clearSessionStub :: Request -> IO ()
clearSessionStub _ = pure ()

waiH :: WAI.Handle a
waiH =
    WAI.Handle
        requestMethod
        pathInfo
        strictRequestBody

sessionsH :: S.Handle
sessionsH =
    S.Handle
        withSessionStub
        maybeSessionMethodsStub
        clearSessionStub

acquireStub :: IO (Either ConnectionError Connection)
acquireStub = pure $ Left Nothing

dbH :: DBC.Handle
dbH = DBC.Handle acquireStub

withLogger' :: (L.Handle () -> IO a) -> IO a
withLogger' = L.withLogger
    (L.Config
        DEBUG
        (\ _ -> return ())
        (\ _ -> return ())
        (\ _ -> return ())
        (\ _ -> return ()))

processedConfig :: IO (Port, Settings, ConnectInfo)
processedConfig = do
    Right config <- C.parseConfig "tests-config.ini"
    pure $ processConfig config


makeApplication' :: L.Handle () -> Settings -> ConnectInfo -> IO Application
makeApplication' loggerH _ _ =
    pure
        . S.hWithSession
            sessionsH
            $ restAPI loggerH sessionsH dbH waiH

runApllicationWith :: (Port -> IO a) -> IO a
runApllicationWith f = do
    (_, dbConnectionSettings, connectInfo) <- processedConfig
    withApplication
        (withLogger' (\loggerH -> makeApplication loggerH dbConnectionSettings connectInfo))
        f


spec :: Spec
spec = do
    describe "restAPI" $ do
        it "exit with failure if no vault"
            $ do
                (_, dbConnectionSettings, connectInfo) <- processedConfig
                eitherException <- try
                    -- Right ResponseReceived -> Right ()
                    (void $
                        testWithApplication
                            (withLogger'
                                (\loggerH -> makeApplication' loggerH dbConnectionSettings connectInfo)
                            )
                            (\ port -> auth port >> pure ResponseReceived)
                        ) :: IO (Either SessionErrorThatNeverOccured ())

                shouldBe
                    (show eitherException)
                    (show (Left SessionErrorThatNeverOccured :: Either SessionErrorThatNeverOccured ()))

        it "message when wrong path was requested"
            $ runApllicationWith wrongPathRequest
                >>= (`shouldBe` (toString $ encode eNoSuchEndpoint))

        it "message when wrong credentials"
            $ runApllicationWith wrongCredsRequest
                >>= (`shouldBe` (toString $ encode eNoSuchEndpoint))

        it "message when DB session error"
            $ runApllicationWith dbSessionErrorRequest
                >>= (`shouldBe` toString cantDecodeBS)


    describe "auth" $ do
        it "recognize existing user credentials"
            $ runApllicationWith auth
                >>= (`shouldBe` "cookies are baked")

        it "return error when wrong creds"
            $ runApllicationWith authMustFail
                >>= (`shouldBe` (toString $ encode eWrongUsernameOrPassword))


    createUserResult <- runIO (runApllicationWith createUser)

    describe "createUser" $ do
        it "create user"
            -- {"name":"name","is_admin":false,"creation_date":"2021-01-19T14:30:26.911449","surname":"surname","user_id":35,"avatar":"asd"}
            $ shouldStartWith createUserResult "{\"name"

        it "create user will return error if such user already exists"
            $ runApllicationWith createUser
            >>= (`shouldBe` (toString $ encode eUserWithSuchUsernameAlreadyExist))


    session <- runIO (runApllicationWith getSession)

    describe "getUser" $
        it "get user information"
            $ runApllicationWith (getUser session)
            >>= (`shouldStartWith` "{\"name")


    let maybeUserIdJSONSection = getStringStartingWith "\"user_id\"" $ replaceComasWithNewlines createUserResult

        userIdJSONSection = case maybeUserIdJSONSection of
            Nothing -> error "response has no user_id"
            Just section -> section

        userIdJSON = concat ["{", userIdJSONSection, "}"]

    promoteUserToAuthorResult <- runIO (
        runApllicationWith $
            promoteUserToAuthor (concat ["{", userIdJSONSection, ", \"description\": \"blob deccas\"}"]) session
        )

    describe "promoteUserToAuthor" $ do
        it "successfully makes author"
            $ shouldStartWith
                promoteUserToAuthorResult
                "{\"author_id\":"

        it "call with same params returns error"
            $ runApllicationWith
                (promoteUserToAuthor (concat ["{", userIdJSONSection, ", \"description\": \"blob deccas\"}"]) session)
                    >>= (`shouldBe` (toString $ encode eSuchUserAlreadyAuthor))

        it "call with non-existent user_id"
             $ runApllicationWith
                (promoteUserToAuthor "{\"user_id\": 123456, \"description\": \"blob deccas\"}" session)
                     >>= (`shouldBe` (toString $ encode eSuchUserDoesNotExist))


    let authorIdJSONSection = maybe
            (error "let authorIdJSONSection error")
            fst
            (uncons . lines $ replaceComasWithNewlines promoteUserToAuthorResult)

    describe "getAuthor" $ do
        it "successfully get author"
            $ runApllicationWith
                (getAuthor (authorIdJSONSection ++ "}") session)
                    >>= (`shouldStartWith` "{\"author_id\":")

        it "return error message if no such author"
            $ runApllicationWith
                (getAuthor "{\"author_id\": 123456, \"description\": \"asd\"}" session)
                    >>= (`shouldBe` (toString $ encode eSuchAuthorDoesNotExist))


    describe "editAuthor" $ do
        it "successfully edit author description"
            $ runApllicationWith
                (editAuthor (authorIdJSONSection ++ ", \"description\": \"asd\"}") session)
                    >>= (`shouldStartWith` "{\"author_id\":")

        it "return error message if no such author"
            $ runApllicationWith
                (editAuthor "{\"author_id\": 123456, \"description\": \"asd\"}" session)
                    >>= (`shouldBe` (toString $ encode eSuchAuthorDoesNotExist))


    describe "deleteAuthorRole" $ do
        it "successfully deletes author role"
            $ runApllicationWith
                (deleteAuthorRole (authorIdJSONSection ++ "}") session)
                    >>= (`shouldBe` "{\"results\":\"ook\"}")

        it "return error message if no such author"
            $ runApllicationWith
                (deleteAuthorRole "{\"author_id\":123456}" session)
                    >>= (`shouldBe` (toString $ encode eSuchAuthorDoesNotExist))


    describe "deleteUser" $ do
        it "successfully delete user"
            $ runApllicationWith
                (deleteUser userIdJSON session)
                    >>= (`shouldBe` "{\"results\":\"ook\"}")

        it "returns error on non-existent user"
            $ runApllicationWith
                (deleteUser userIdJSON session)
                    >>= (`shouldBe` (toString $ encode eSuchUserDoesNotExist))


    createCategoryResult <- runIO (
        runApllicationWith
            $ createCategory "{\"name\": \"pluh\", \"parent_id\": null}" session
        )

    let categoryIdJSONSection = maybe
            (error "let categoryIdJSONSection error")
            fst
            (uncons . lines $ replaceComasWithNewlines createCategoryResult)

    describe "createCategory" $ do
        it "creates category"
            $ shouldStartWith createCategoryResult "{\"category_id\":"

        it "returns error if non-existent parent category"
            $ runApllicationWith
                (createCategory "{\"name\": \"pluh\", \"parent_id\": 12345}" session)
                    >>= (`shouldBe` (toString . encode $ makeNoSuchCategory "12345"))


    describe "getCategory" $ do
        it "get category"
            $ runApllicationWith
                (getCategory (categoryIdJSONSection ++ "}"))
                    >>= (`shouldStartWith` "{\"category_id\":")

        it "returns error on non-existent category"
            $ runApllicationWith
                (getCategory "{\"category_id\": 123456}")
                    >>= (`shouldBe` (toString . encode $ makeNoSuchCategory "123456"))


    describe "updateCategory" $ do
        it "updates category"
            $ runApllicationWith
                (updateCategory
                    (categoryIdJSONSection ++ ", \"name\": \"pluh_pattched\", \"parent_id\": null}")
                    session
                    )
                    >>= (`shouldStartWith` "{\"category_id\":")

        it "returns error on non-existent category"
            $ runApllicationWith
                (updateCategory "{\"category_id\": 123456, \"name\": \"pluh_pattched\", \"parent_id\": null}" session)
                    >>= (`shouldBe` (toString . encode $ makeNoSuchCategory "123456"))

        it "returns error on non-existent parent category"
            $ runApllicationWith
                (updateCategory "{\"category_id\": 9, \"name\": \"plusdh\", \"parent_id\": 12345}" session)
                    >>= (`shouldBe` (toString . encode $ makeNoSuchCategory "12345"))

        it "returns error if parent is set to itself"
            $ runApllicationWith
                (updateCategory "{\"category_id\": 9, \"name\": \"pluh_pattched\", \"parent_id\": 9}" session)
                    >>= (`shouldBe` (toString $ encode eSameParentId))

        it "returns error if parent is set to category descendant"
            $ runApllicationWith
                (updateCategory "{\"category_id\": 9, \"name\": \"pluh_pattched\", \"parent_id\": 10}" session)
                    >>= (`shouldBe` (toString $ encode eParentIdIsDescendant))

        


    describe "deleteCategory" $ do
        it "deletes category"
            $ runApllicationWith
                (deleteCategory (categoryIdJSONSection ++ "}") session)
                    >>= (`shouldBe` "{\"results\":\"ook\"}")

        it "returns error on non-existent category"
            $ runApllicationWith
                (deleteCategory "{\"category_id\": 123456}" session)
                    >>= (`shouldBe` (toString . encode $ makeNoSuchCategory "123456"))

        it "returns error if category is referenced in DB"
            $ runApllicationWith
                (deleteCategory "{\"category_id\": 1}" session)
                    >>= (`shouldBe` (toString $ encode eCategoryInUse))


    createArticleDraftResult <- runIO
            (runApllicationWith
                $ createArticleDraft
                    "{\"article_title\": \"they dont beleive their eyes…\", \"category_id\": 1, \"article_content\": \"article is long enough\", \"tags\": [], \"main_photo\": \"http://pl.uh/main\", \"additional_photos\": [\"1\", \"2\", \"3\"]}"
                    session
            )

    let maybeArticleIdJSONSection = getStringStartingWith "\"article_id\""
            $ replaceComasWithNewlines createArticleDraftResult

        articleIdJSONSection = case maybeArticleIdJSONSection of
            Nothing -> error "response has no article_id"
            Just section -> section

    describe "createArticleDraft" $ do
        it "creates article draft"
            $ shouldStartWith createArticleDraftResult "{\"article_content\""

        it "returns error if no such tag"
            $ runApllicationWith
                (createArticleDraft
                    "{\"article_title\": \"they dont beleive their eyes…\", \"category_id\": 1, \"article_content\": \"article is long enough\", \"tags\": [123344], \"main_photo\": \"http://pl.uh/main\", \"additional_photos\": [\"1\", \"2\", \"3\"]}"
                    session)
                    >>= (`shouldBe` (toString $ encode eNoSuchTag))

        it "returns error if no such category"
            $ runApllicationWith
                (createArticleDraft
                    "{\"article_title\": \"they dont beleive their eyes…\", \"category_id\": 1123, \"article_content\": \"article is long enough\", \"tags\": [], \"main_photo\": \"http://pl.uh/main\", \"additional_photos\": [\"1\", \"2\", \"3\"]}"
                    session)
                    >>= (`shouldBe` (toString . encode $ makeNoSuchCategory "1123"))


    describe "editArticleDraft" $ do
        it "edits article draft"
            $ runApllicationWith
                (editArticleDraft
                    ("{\"article_title\": \"they dont beleive their eyes…\", \"category_id\": 2, \"article_content\": \"article is long enough\", \"tags\": [1], \"main_photo\": \"http://pl.uh/main\", \"additional_photos\": [\"1\", \"2\", \"3\"], " ++ articleIdJSONSection ++ "}")
                    session)
                    >>= (`shouldStartWith` "{\"article_content")

        it "returns error if no such article"
            $ runApllicationWith
                (editArticleDraft
                    "{\"article_title\": \"they dont beleive their eyes…\", \"category_id\": 1, \"article_content\": \"article is long enough\", \"tags\": [1], \"main_photo\": \"http://pl.uh/main\", \"additional_photos\": [\"1\", \"2\", \"3\"], \"article_id\": 123445}"
                    session)
                    >>= (`shouldBe` (toString $ encode eNoSuchArticle))

        it "returns error if no such tags"
            $ runApllicationWith
                (editArticleDraft
                    ("{\"article_title\": \"they dont beleive their eyes…\", \"category_id\": 1, \"article_content\": \"article is long enough\", \"tags\": [11234], \"main_photo\": \"http://pl.uh/main\", \"additional_photos\": [\"1\", \"2\", \"3\"], " ++ articleIdJSONSection ++ "}")
                    session)
                    >>= (`shouldBe` (toString $ encode eNoSuchTag))

        it "returns error if no such category"
            $ runApllicationWith
                (editArticleDraft
                    ("{\"article_title\": \"they dont beleive their eyes…\", \"category_id\": 11234, \"article_content\": \"article is long enough\", \"tags\": [1], \"main_photo\": \"http://pl.uh/main\", \"additional_photos\": [\"1\", \"2\", \"3\"], " ++ articleIdJSONSection ++ "}")
                    session)
                        >>= (`shouldBe` (toString . encode $ makeNoSuchCategory "11234"))


    describe "getArticleDraft" $ do
        it "get article"
            $ runApllicationWith
                (getArticleDraft ("{" ++ articleIdJSONSection ++ "}") session)
                    >>= (`shouldContain` articleIdJSONSection)

        it "returns error if no such article"
            $ runApllicationWith
                (getArticleDraft "{\"article_id\":123456}" session)
                    >>= (`shouldStartWith` (toString $ encode eNoSuchArticle))


    describe "publishArticleDraft" $ do
        it "publish article"
            $ runApllicationWith
                (publishArticleDraft ("{" ++ articleIdJSONSection ++ "}") session)
                    >>= (`shouldBe` "{\"results\":\"ook\"}")

        it "returns error if no such article"
            $ runApllicationWith
                (publishArticleDraft "{\"article_id\":123456}" session)
                    >>= (`shouldBe` (toString $ encode eNoSuchArticle))


    createArticleDraftResult1 <- runIO
        (runApllicationWith
            $ createArticleDraft
                "{\"article_title\": \"they dont beleive their eyes…\", \"category_id\": 1, \"article_content\": \"article is long enough\", \"tags\": [], \"main_photo\": \"http://pl.uh/main\", \"additional_photos\": [\"1\", \"2\", \"3\"]}"
                session
            )

    let maybeArticleIdJSONSection1 = getStringStartingWith "\"article_id\""
            $ replaceComasWithNewlines createArticleDraftResult1

        articleIdJSONSection1 = case maybeArticleIdJSONSection1 of
            Nothing -> error "response has no article_id"
            Just section -> section

    describe "deleteArticleDraft" $ do
        it "delete article draft"
            $ runApllicationWith
                (deleteArticleDraft ("{" ++ articleIdJSONSection1 ++ "}") session)
                    >>= (`shouldBe` "{\"results\":\"ook\"}")

        it "published article can't be deleted"
            $ runApllicationWith
                (deleteArticleDraft ("{" ++ articleIdJSONSection ++ "}") session)
                    >>= (`shouldBe` (toString $ encode eNoSuchArticle))

        it "returns error if no such article"
            $ runApllicationWith
                (deleteArticleDraft "{\"article_id\":123456}" session)
                    >>= (`shouldBe` (toString $ encode eNoSuchArticle))


    createTagResult <- runIO
        (runApllicationWith
            $ createTag "{\"tag_name\": \"test tag\"}" session
        )

    let tagIdJSONSection = fromMaybe
            (error "let tagIdJSONSection error")
            --["{\"tag_name\":\"test tag\"","\"tag_id\":22}"]
            (stripPrefix "{\"tag_name\":\"test tag\"" $ replaceComasWithNewlines createTagResult)

    describe "createTag" $ do
        it "creates tag"
            $ shouldStartWith createTagResult "{\"tag_name\":"

        it "returns error if tag already exists"
            $ runApllicationWith
                (createTag "{\"tag_name\": \"test tag\"}" session)
                    >>= (`shouldBe` (toString $ encode eTagWithSuchNameAlreadyExist))


    beforeAll
        (void . runApllicationWith $ createTag "{\"tag_name\": \"test tag1\"}" session)

        $ describe "editTag" $ do
            it "edit"
                $ runApllicationWith
                    (editTag ("{\"tag_name\": \"test tasd\"," ++ tagIdJSONSection) session)
                        >>= (`shouldStartWith` "{\"tag_name\":")

            it "returns error if tag with such name already exist"
                $ runApllicationWith
                    (editTag ("{\"tag_name\": \"test tag1\"," ++ tagIdJSONSection) session)
                        >>= (`shouldBe` (toString $ encode eTagWithSuchNameAlreadyExist))

            it "returns error if tag does not exists"
                $ runApllicationWith
                    (editTag "{\"tag_id\": 12345,\"tag_name\": \"test ta\"}" session)
                        >>= (`shouldBe` (toString $ encode eNoSuchTag))


    describe "getTag" $ do
        it "get"
            $ runApllicationWith
                (getTag ("{" ++ tagIdJSONSection))
                    >>= (`shouldStartWith`  "{\"tag_name\":")

        it "returns error if tag does not exists"
            $ runApllicationWith
                (getTag "{\"tag_id\": 12345}")
                    >>= (`shouldBe` (toString $ encode eNoSuchTag))


    createTagResult1 <- runIO
        (runApllicationWith
            $ createTag "{\"tag_name\": \"test tag pluh\"}" session
        )

    let tagId =  maybe
            (error "let tagId error: " ++ createTagResult1)
            (takeWhile (/= '}'))
            (stripPrefix "{\"tag_name\":\"test tag pluh\",\"tag_id\":" createTagResult1)


    beforeAll
        (runApllicationWith
            (createArticleDraft
                ("{\"article_title\": \"they dont beleive their eyes…\", \"category_id\": 1, \"article_content\": \"article is long enough\", \"main_photo\": \"http://pl.uh/main\", \"additional_photos\": [\"1\", \"2\", \"3\"], \"tags\": ["
                    ++ tagId
                    ++ "]}")
                session))

            . afterAll (\createdArticleDraftResponse -> ( do
                    let maybeCreatedArticleDraftIdSection = takeWhile (/= '\r')
                            <$> getStringStartingWith
                                "\"article_id"
                                (replaceComasWithNewlines createdArticleDraftResponse)
                    void $ case maybeCreatedArticleDraftIdSection of
                        Nothing -> error "response has no comment_id"
                        Just section ->
                            runApllicationWith $ deleteArticleDraft ("{" ++ section ++ "}") session
                    void . runApllicationWith $ deleteTag ("{\"tag_id\": " ++ tagId ++ "}") session
                )
            )

            $ describe "deleteTag" $ do
                it "delete"
                    $ \_ -> runApllicationWith
                        (deleteTag ("{" ++ tagIdJSONSection) session)
                            >>= (`shouldBe` "{\"results\":\"ook\"}")

                it "returns error if tag does not exists"
                    $ \_ -> runApllicationWith
                        (deleteTag "{\"tag_id\": 12345}" session)
                            >>= (`shouldBe` (toString $ encode eNoSuchTag))

                it "returns error if tag is referenced by an article"
                    $ \_ -> runApllicationWith
                        (deleteTag ("{\"tag_id\": " ++ tagId ++ "}") session)
                            >>= (`shouldBe` (toString $ encode eTagReferencedByArticle))


    createCommentResult <- runIO
        (runApllicationWith
            $ createComment "{\"article_id\": 1, \"comment_text\": \"bluasd!\"}" session)

    -- {"comment_id":12,"user_id":1,"article_id":1,"comment_text":"bluasd!"}
    let maybeCommentIdJSONSection = getStringStartingWith "{\"comment_id\""
            $ replaceComasWithNewlines createCommentResult

        commentIdJSONSection = case maybeCommentIdJSONSection of
            Nothing -> error "response has no comment_id"
            Just section -> section


    describe "createComment" $ do
        it "creates article comment"
            $ shouldStartWith createCommentResult "{\"comment_id\""

        it "returns error if no such article"
            $ runApllicationWith
                (createComment
                    "{\"article_id\": 12345, \"comment_text\": \"bluasd!\"}"
                    session)
                    >>= (`shouldBe` (toString $ encode eNoSuchArticle))


    describe "getArticleComments" $ do
        it "get article comments"
            $ runApllicationWith
                (getArticleComments "{\"article_id\": 1}")
                    >>= (`shouldStartWith` "[{\"comment_id\"")

        it "returns empty array if no such article"
            $ runApllicationWith
                (getArticleComments "{\"article_id\": 12345}")
                    >>= (`shouldBe` "[]")


    describe "deleteComment" $ do
        it "delete article comment"
            $ runApllicationWith
                (deleteComment
                    (commentIdJSONSection ++ "}")
                    session)
                    >>= (`shouldBe` "{\"results\":\"ook\"}")

        it "returns error if no such comment"
            $ runApllicationWith
                (deleteComment
                    "{\"comment_id\": 12345}"
                    session)
                    >>= (`shouldBe` (toString $ encode eNoSuchComment))
