{-# LANGUAGE OverloadedStrings  #-}

module RestNewsSpec where

import Control.Exception
import Control.Monad (void)
import Database.PostgreSQL.Simple (ConnectInfo)
import Hasql.Connection (Connection, ConnectionError, Settings)
import Network.Wai (Application, Middleware, Request, pathInfo, requestMethod, strictRequestBody)
import Network.Wai.Internal (ResponseReceived(..))
import Network.Wai.Handler.Warp (Port, testWithApplication, withApplication)
import System.Log.Logger (Priority (DEBUG))
import System.Process (readProcess)
import System.Exit
import Test.Hspec

import RestNews
import qualified RestNews.Config as C
import qualified RestNews.DBConnection as DBC
import qualified RestNews.Logger as L
import qualified RestNews.Middleware.Sessions as S
import qualified RestNews.WAI as WAI


getSession :: Int -> IO String
getSession port =
    init
    . drop 20
    -- "Set-Cookie: SESSION=736eaf44239adb2e1e0f1cf52db8f3e4db4362fed5dc9ba\r"
    . (!! 4)
    . lines
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
    ("http://0.0.0.0:" ++ show port ++ "/articles")

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


runStub :: Port -> Application -> IO ()
runStub _ _ = pure ()

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

dbConnectionSettings :: Settings
connectInfo :: ConnectInfo
(_, dbConnectionSettings, connectInfo) = processConfig $
    C.Config {
        C._runAtPort = 8081,
        C._dbHost = "localhost",
        C._dbPort = 5432,
        C._dbUser = "rest-news-user",
        C._dbPassword = "rest",
        C._dbName = "rest-news-test"
    }

makeApplication' :: L.Handle () -> Settings -> ConnectInfo -> IO Application
makeApplication' loggerH _ _ =  
    pure
        . S.hWithSession
            sessionsH
            $ restAPI loggerH sessionsH dbH waiH

runApllicationWith :: (Port -> IO a) -> IO a
runApllicationWith = withApplication
    (withLogger'
        (\loggerH -> makeApplication loggerH dbConnectionSettings connectInfo)
    )


spec :: Spec
spec = do
    describe "restAPI" $
        it "exit with failure if no vault"
            $ do 
                eitherExitCode <- try
                    -- Right ResponseReceived -> Right ()
                    (void $
                        testWithApplication
                            (withLogger'
                                (\loggerH -> makeApplication' loggerH dbConnectionSettings connectInfo)
                            )
                            (\ port -> auth port >> pure ResponseReceived)
                        ) :: IO (Either SomeException ())

                shouldBe
                    (show eitherExitCode)
                    (show (Left $ toException (ExitFailure 1) :: Either SomeException ()))


    describe "auth" $ do
        it "recognize existing user credentials"
            -- $ do
            --     response <- runApllicationWith
            --         auth

            --     shouldBe
            --         response
            --         "cookies are baked"
            $ runApllicationWith auth    
                >>= (`shouldBe` "cookies are baked")

        it "return error when wrong creds"
            $ runApllicationWith authMustFail
                >>= (`shouldBe` "wrong username/password")


    createUserResult <- runIO (runApllicationWith createUser)

    describe "createUser" $ do
        it "create user"
            -- {"name":"name","is_admin":false,"creation_date":"2021-01-19T14:30:26.911449","surname":"surname","user_id":35,"avatar":"asd"}
            $ shouldStartWith createUserResult "{\"name"

        it "create user will return error if such user already exists"
            $ runApllicationWith createUser
            >>= (`shouldBe` "{\"error\": \"user with this username already exists\"}")


    session <- runIO (runApllicationWith getSession)

    describe "getUser" $
        it "get user information"
            $ runApllicationWith (getUser session)
            >>= (`shouldStartWith` "{\"name")


    let userIdJSONSection = (!! 4) . lines $ replaceComasWithNewlines createUserResult;

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
                    >>= (`shouldBe` "{\"error\": \"such user is already an author\"}")

        it "call with non-existent user_id"
             $ runApllicationWith
                (promoteUserToAuthor "{\"user_id\": 123456, \"description\": \"blob deccas\"}" session)
                     >>= (`shouldBe` "{\"error\": \"such user does not exist\"}")


    let authorIdJSONSection = head . lines $ replaceComasWithNewlines promoteUserToAuthorResult;

    describe "getAuthor" $ do
        it "successfully get author"
            $ runApllicationWith
                (getAuthor (authorIdJSONSection ++ "}") session)
                    >>= (`shouldStartWith` "{\"author_id\":")

        it "return error message if no such author"
            $ runApllicationWith
                (getAuthor "{\"author_id\": 123456, \"description\": \"asd\"}" session)
                    >>= (`shouldBe` "{\"error\": \"such author does not exist\"}")


    describe "editAuthor" $ do
        it "successfully edit author description"
            $ runApllicationWith
                (editAuthor (authorIdJSONSection ++ ", \"description\": \"asd\"}") session)
                    >>= (`shouldStartWith` "{\"author_id\":")

        it "return error message if no such author"
            $ runApllicationWith
                (editAuthor "{\"author_id\": 123456, \"description\": \"asd\"}" session)
                    >>= (`shouldBe` "{\"error\": \"such author does not exist\"}")

    
    describe "deleteAuthorRole" $ do
        it "successfully deletes author role"
            $ runApllicationWith
                (deleteAuthorRole (authorIdJSONSection ++ "}") session)
                    >>= (`shouldBe` "{\"results\":\"ook\"}")

        it "return error message if no such author"
            $ runApllicationWith
                (deleteAuthorRole "{\"author_id\":123456}" session)
                    >>= (`shouldBe` "{\"error\": \"such author does not exist\"}")


    describe "deleteUser" $ do
        it "successfully delete user"
            $ runApllicationWith
                (deleteUser userIdJSON session)
                    >>= (`shouldBe` "{\"results\":\"ook\"}")

        it "returns error on non-existent user"
            $ runApllicationWith
                (deleteUser userIdJSON session)
                    >>= (`shouldBe` "{\"error\": \"such user does not exist\"}")


    createCategoryResult <- runIO (
        runApllicationWith
            $ createCategory "{\"name\": \"pluh\", \"parent_id\": null}" session
        )

    let categoryIdJSONSection = head . lines $ replaceComasWithNewlines createCategoryResult;

    describe "createCategory" $ do
        it "creates category"
            $ shouldStartWith createCategoryResult "{\"category_id\":"

        it "returns error if non-existent parent category"
            $ runApllicationWith
                (createCategory "{\"name\": \"pluh\", \"parent_id\": 12345}" session)
                    >>= (`shouldBe` "{\"error\": \"parent category does not exist\"}")


    describe "getCategory" $ do
        it "get category"
            $ runApllicationWith
                (getCategory (categoryIdJSONSection ++ "}"))
                    >>= (`shouldStartWith` "{\"category_id\":")

        it "returns error on non-existent category"
            $ runApllicationWith
                (getCategory "{\"category_id\": 123456}")
                    >>= (`shouldBe` "{\"error\": \"no such category\"}")


    describe "updateCategory" $ do
        it "update category"
            $ runApllicationWith
                (updateCategory
                    (categoryIdJSONSection ++ ", \"name\": \"pluh_pattched\", \"parent_id\": null}")
                    session
                    )
                    >>= (`shouldStartWith` "{\"category_id\":")

        it "returns error on non-existent category"
            $ runApllicationWith
                (updateCategory "{\"category_id\": 123456, \"name\": \"pluh_pattched\", \"parent_id\": null}" session)
                    >>= (`shouldBe` "{\"error\": \"no such category\"}")

        it "returns error if non-existent parent category"
            $ runApllicationWith
                (updateCategory (categoryIdJSONSection ++ ", \"name\": \"plusdh\", \"parent_id\": 12345}") session)
                    >>= (`shouldBe` "{\"error\": \"parent category does not exist\"}")


    describe "deleteCategory" $ do
        it "deletes category"
            $ runApllicationWith
                (deleteCategory (categoryIdJSONSection ++ "}") session)
                    >>= (`shouldBe` "{\"results\":\"ook\"}")

        it "returns error on non-existent category"
            $ runApllicationWith
                (deleteCategory "{\"category_id\": 123456}" session)
                    >>= (`shouldBe` "{\"error\": \"no such category\"}")

        it "returns error if category is referenced in DB"
            $ runApllicationWith
                (deleteCategory "{\"category_id\": 1}" session)
                    >>= (`shouldBe` "{\"error\": \"category is in use\"}")


    createArticleDraftResult <- runIO
            (runApllicationWith
                $ createArticleDraft
                    "{\"article_title\": \"they dont beleive their eyes…\", \"category_id\": 1, \"article_content\": \"article is long enough\", \"tags\": [], \"main_photo\": \"http://pl.uh/main\", \"additional_photos\": [\"1\", \"2\", \"3\"]}"
                    session
            )

    let articleIdJSONSection = (!! 6) . lines $ replaceComasWithNewlines createArticleDraftResult;

    describe "createArticleDraft" $ do
        it "creates article draft"
            $ shouldStartWith createArticleDraftResult "{\"article_content\""

        it "returns error if no such tag"
            $ runApllicationWith
                (createArticleDraft
                    "{\"article_title\": \"they dont beleive their eyes…\", \"category_id\": 1, \"article_content\": \"article is long enough\", \"tags\": [123344], \"main_photo\": \"http://pl.uh/main\", \"additional_photos\": [\"1\", \"2\", \"3\"]}"
                    session)
                    >>= (`shouldBe` "{\"error\": \"no such tag\"}")

        it "returns error if no such category"
            $ runApllicationWith
                (createArticleDraft
                    "{\"article_title\": \"they dont beleive their eyes…\", \"category_id\": 1123, \"article_content\": \"article is long enough\", \"tags\": [], \"main_photo\": \"http://pl.uh/main\", \"additional_photos\": [\"1\", \"2\", \"3\"]}"
                    session)
                    >>= (`shouldBe` "{\"error\": \"no such category\"}")


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
                    >>= (`shouldBe` "{\"error\": \"no such article\"}")

        it "returns error if no such tags"
            $ runApllicationWith
                (editArticleDraft
                    ("{\"article_title\": \"they dont beleive their eyes…\", \"category_id\": 1, \"article_content\": \"article is long enough\", \"tags\": [11234], \"main_photo\": \"http://pl.uh/main\", \"additional_photos\": [\"1\", \"2\", \"3\"], " ++ articleIdJSONSection ++ "}")
                    session)
                    >>= (`shouldBe` "{\"error\": \"no such tag\"}")

        it "returns error if no such category"
            $ runApllicationWith
                (editArticleDraft
                    ("{\"article_title\": \"they dont beleive their eyes…\", \"category_id\": 11234, \"article_content\": \"article is long enough\", \"tags\": [1], \"main_photo\": \"http://pl.uh/main\", \"additional_photos\": [\"1\", \"2\", \"3\"], " ++ articleIdJSONSection ++ "}")
                    session)
                        >>= (`shouldBe` "{\"error\": \"no such category\"}")


    describe "getArticleDraft" $ do
        it "get article"
            $ runApllicationWith
                (getArticleDraft ("{" ++ articleIdJSONSection ++ "}") session)
                    >>= (`shouldStartWith` "{\"article_content")

        it "returns error if no such article"
            $ runApllicationWith
                (getArticleDraft "{\"article_id\":123456}" session)
                    >>= (`shouldStartWith` "{\"error\": \"no such article\"}")


    describe "publishArticleDraft" $ do
        it "publish article"
            $ runApllicationWith
                (publishArticleDraft ("{" ++ articleIdJSONSection ++ "}") session)
                    >>= (`shouldBe` "{\"results\":\"ook\"}")

        it "returns error if no such article"
            $ runApllicationWith
                (publishArticleDraft "{\"article_id\":123456}" session)
                    >>= (`shouldBe` "{\"error\": \"no such article\"}")


    createArticleDraftResult1 <- runIO
        (runApllicationWith
            $ createArticleDraft
                "{\"article_title\": \"they dont beleive their eyes…\", \"category_id\": 1, \"article_content\": \"article is long enough\", \"tags\": [], \"main_photo\": \"http://pl.uh/main\", \"additional_photos\": [\"1\", \"2\", \"3\"]}" 
                session
            )

    let articleIdJSONSection1 = (!! 6) . lines $ replaceComasWithNewlines createArticleDraftResult1;

    describe "deleteArticleDraft" $ do
        it "delete article draft"
            $ runApllicationWith
                (deleteArticleDraft ("{" ++ articleIdJSONSection1 ++ "}") session)
                    >>= (`shouldBe` "{\"results\":\"ook\"}")

        it "published article can't be deleted"
            $ runApllicationWith
                (deleteArticleDraft ("{" ++ articleIdJSONSection ++ "}") session)
                    >>= (`shouldBe` "{\"error\": \"no such article\"}")

        it "returns error if no such article"
            $ runApllicationWith
                (deleteArticleDraft "{\"article_id\":123456}" session)
                    >>= (`shouldBe` "{\"error\": \"no such article\"}")


    createTagResult <- runIO
        (runApllicationWith
            $ createTag "{\"tag_name\": \"test tag\"}" session
        )

    let tagIdJSONSection = last . lines $ replaceComasWithNewlines createTagResult;

    describe "createTag" $ do
        it "creates tag"
            $ shouldStartWith createTagResult "{\"tag_name\":"

        it "returns error if tag already exists"
            $ runApllicationWith
                (createTag "{\"tag_name\": \"test tag\"}" session)
                    >>= (`shouldBe` "{\"error\": \"tag with such name already exists\"}")


    before_
        (runApllicationWith (createTag "{\"tag_name\": \"test tag1\"}" session)
            >> pure ()
        )

        $ describe "editTag" $ do
            it "edit"
                $ runApllicationWith
                    (editTag ("{\"tag_name\": \"test tasd\"," ++ tagIdJSONSection) session)
                        >>= (`shouldStartWith` "{\"tag_name\":")

            it "returns error if tag with such name already exist"
                $ runApllicationWith
                    (editTag ("{\"tag_name\": \"test tag1\"," ++ tagIdJSONSection) session)
                        >>= (`shouldBe` "{\"error\": \"tag with such name already exists\"}")

            it "returns error if tag does not exists"
                $ runApllicationWith
                    (editTag "{\"tag_id\": 12345,\"tag_name\": \"test ta\"}" session)
                        >>= (`shouldBe` "{\"error\": \"no such tag\"}")


    describe "getTag" $ do
        it "get"
            $ runApllicationWith
                (getTag ("{" ++ tagIdJSONSection))
                    >>= (`shouldStartWith`  "{\"tag_name\":")

        it "returns error if tag does not exists"
            $ runApllicationWith
                (getTag "{\"tag_id\": 12345}")
                    >>= (`shouldBe` "{\"error\": \"no such tag\"}")


    createTagResult1 <- runIO
        (runApllicationWith
            $ createTag "{\"tag_name\": \"test tag pluh\"}" session
        )

    let tagId = init . drop (length ("\"tag_id\":" :: String)) . last . lines $ replaceComasWithNewlines createTagResult1;

    before_
        (runApllicationWith
            (createArticleDraft
                ("{\"article_title\": \"they dont beleive their eyes…\", \"category_id\": 1, \"article_content\": \"article is long enough\", \"main_photo\": \"http://pl.uh/main\", \"additional_photos\": [\"1\", \"2\", \"3\"], \"tags\": ["
                    ++ tagId
                    ++ "]}")
                session)
                >> pure ())

        $ describe "deleteTag" $ do
            it "delete"
                $ runApllicationWith
                    (deleteTag ("{" ++ tagIdJSONSection) session)
                        >>= (`shouldBe` "{\"results\":\"ook\"}")

            it "returns error if tag does not exists"
                $ runApllicationWith
                    (deleteTag "{\"tag_id\": 12345}" session)
                        >>= (`shouldBe` "{\"error\": \"no such tag\"}")

            it "returns error if tag is referenced by an article"
                $ runApllicationWith
                    (deleteTag ("{\"tag_id\": " ++ tagId ++ "}") session)
                        >>= (`shouldBe` "{\"error\": \"tag is referenced by an article\"}")

        
    createCommentResult <- runIO
        (runApllicationWith
            $ createComment "{\"article_id\": 1, \"comment_text\": \"bluasd!\"}" session)

    -- {"comment_id":12,"user_id":1,"article_id":1,"comment_text":"bluasd!"}
    let commentIdJSONSection = (!! 0) . lines $ replaceComasWithNewlines createCommentResult;

    describe "createComment" $ do
        it "creates article comment"
            $ shouldStartWith createCommentResult "{\"comment_id\""

        it "returns error if no such article"
            $ runApllicationWith
                (createComment
                    "{\"article_id\": 12345, \"comment_text\": \"bluasd!\"}"
                    session)
                    >>= (`shouldBe` "{\"error\": \"no such article\"}")


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
                    >>= (`shouldBe` "{\"error\": \"no such comment\"}")
