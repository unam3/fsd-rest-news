{-# LANGUAGE OverloadedStrings  #-}

module RestNewsSpec where

import System.Process (readProcess)
--import RestNews (runWarpWithLogger)
import Test.Hspec

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


spec :: Spec
spec = do
    describe "auth" $ do
        it "recognize existing user credentials"
            $ auth
            >>= (`shouldBe` "cookies are baked")

        it "return error when wrong creds"
            $ authMustFail
            >>= (`shouldBe` "wrong username/password")


    createUserResult <- runIO createUser

    describe "createUser" $ do
        it "create user"
            -- {"name":"name","is_admin":false,"creation_date":"2021-01-19T14:30:26.911449","surname":"surname","user_id":35,"avatar":"asd"}
            $ shouldStartWith createUserResult "{\"name"

        it "create user will return error if such user already exists"
            $ createUser
            >>= (`shouldBe` "{\"error\": \"user with this username already exists\"}")

    session <- runIO getSession

    describe "getUser" $
        it "get user information"
            $ getUser session
            >>= (`shouldStartWith` "{\"name")


    let userIdJSONSection = (!! 4) . lines $ replaceComasWithNewlines createUserResult;

    let userIdJSON = concat ["{", userIdJSONSection, "}"]

    promoteUserToAuthorResult <- runIO
        $ promoteUserToAuthor (concat ["{", userIdJSONSection, ", \"description\": \"blob deccas\"}"]) session

    describe "promoteUserToAuthor" $ do
        it "successfully makes author"
            $ shouldStartWith promoteUserToAuthorResult "{\"author_id\":"

        it "call with same params returns error"
            $ promoteUserToAuthor (concat ["{", userIdJSONSection, ", \"description\": \"blob deccas\"}"]) session
            >>= (`shouldBe` "{\"error\": \"such user is already an author\"}")

        it "call with non-existent user_id"
             $ promoteUserToAuthor "{\"user_id\": 123456, \"description\": \"blob deccas\"}" session
             >>= (`shouldBe` "{\"error\": \"such user does not exist\"}")


    let authorIdJSONSection = head . lines $ replaceComasWithNewlines promoteUserToAuthorResult;

    describe "getAuthor" $ do
        it "successfully get author"
            $ getAuthor (authorIdJSONSection ++ "}") session
            >>= (`shouldStartWith` "{\"author_id\":")

        it "return error message if no such author"
            $ getAuthor "{\"author_id\": 123456, \"description\": \"asd\"}" session
            >>= (`shouldBe` "{\"error\": \"such author does not exist\"}")

    
    describe "editAuthor" $ do
        it "successfully edit author description"
            $ editAuthor (authorIdJSONSection ++ ", \"description\": \"asd\"}") session
            >>= (`shouldStartWith` "{\"author_id\":")

        it "return error message if no such author"
            $ editAuthor "{\"author_id\": 123456, \"description\": \"asd\"}" session
            >>= (`shouldBe` "{\"error\": \"such author does not exist\"}")

    
    describe "deleteAuthorRole" $ do
        it "successfully deletes author role"
            $ deleteAuthorRole (authorIdJSONSection ++ "}") session
            >>= (`shouldBe` "{\"results\":\"ook\"}")

        it "return error message if no such author"
            $ deleteAuthorRole "{\"author_id\":123456}" session
            >>= (`shouldBe` "{\"error\": \"such author does not exist\"}")


    describe "deleteUser" $ do
        it "successfully delete user"
            $ deleteUser userIdJSON session
            >>= (`shouldBe` "{\"results\":\"ook\"}")

        it "returns error on non-existent user"
            $ deleteUser userIdJSON session
            >>= (`shouldBe` "{\"error\": \"such user does not exist\"}")


    createCategoryResult <- runIO
        $ createCategory "{\"name\": \"pluh\", \"parent_id\": null}" session

    let categoryIdJSONSection = head . lines $ replaceComasWithNewlines createCategoryResult;

    describe "createCategory" $ do
        it "creates category"
            $ shouldStartWith createCategoryResult "{\"category_id\":"

        it "returns error if non-existent parent category"
            $ createCategory "{\"name\": \"pluh\", \"parent_id\": 12345}" session
            >>= (`shouldBe` "{\"error\": \"parent category does not exist\"}")


    describe "getCategory" $ do
        it "get category"
            $ getCategory (categoryIdJSONSection ++ "}")
            >>= (`shouldStartWith` "{\"category_id\":")

        it "returns error on non-existent category"
            $ getCategory "{\"category_id\": 123456}"
            >>= (`shouldBe` "{\"error\": \"no such category\"}")


    describe "updateCategory" $ do
        it "update category"
            $ updateCategory (categoryIdJSONSection ++ ", \"name\": \"pluh_pattched\", \"parent_id\": null}") session
            >>= (`shouldStartWith` "{\"category_id\":")

        it "returns error on non-existent category"
            $ updateCategory "{\"category_id\": 123456, \"name\": \"pluh_pattched\", \"parent_id\": null}" session
            >>= (`shouldBe` "{\"error\": \"no such category\"}")

        it "returns error if non-existent parent category"
            $ updateCategory (categoryIdJSONSection ++ ", \"name\": \"plusdh\", \"parent_id\": 12345}") session
            >>= (`shouldBe` "{\"error\": \"parent category does not exist\"}")


    describe "deleteCategory" $ do
        it "deletes category"
            $ deleteCategory (categoryIdJSONSection ++ "}") session
            >>= (`shouldBe` "{\"results\":\"ook\"}")

        it "returns error on non-existent category"
            $ deleteCategory "{\"category_id\": 123456}" session
            >>= (`shouldBe` "{\"error\": \"no such category\"}")

        it "returns error if category is referenced in DB"
            $ deleteCategory "{\"category_id\": 1}" session
            >>= (`shouldBe` "{\"error\": \"category is in use\"}")


    createArticleDraftResult <- runIO
        $ createArticleDraft "{\"article_title\": \"they dont beleive their eyes…\", \"category_id\": 1, \"article_content\": \"article is long enough\", \"tags\": [], \"main_photo\": \"http://pl.uh/main\", \"additional_photos\": [\"1\", \"2\", \"3\"]}" session

    let articleIdJSONSection = (!! 6) . lines $ replaceComasWithNewlines createArticleDraftResult;

    describe "createArticleDraft" $ do
        it "creates article draft"
            $ shouldStartWith createArticleDraftResult "{\"article_content\""

        it "returns error if no such tag"
            $ createArticleDraft
                "{\"article_title\": \"they dont beleive their eyes…\", \"category_id\": 1, \"article_content\": \"article is long enough\", \"tags\": [123344], \"main_photo\": \"http://pl.uh/main\", \"additional_photos\": [\"1\", \"2\", \"3\"]}"
                session
            >>= (`shouldBe` "{\"error\": \"no such tag\"}")

        it "returns error if no such category"
            $ createArticleDraft
                "{\"article_title\": \"they dont beleive their eyes…\", \"category_id\": 1123, \"article_content\": \"article is long enough\", \"tags\": [], \"main_photo\": \"http://pl.uh/main\", \"additional_photos\": [\"1\", \"2\", \"3\"]}"
                session
            >>= (`shouldBe` "{\"error\": \"no such category\"}")

    describe "editArticleDraft" $ do
        it "edits article draft"
            $ editArticleDraft
                ("{\"article_title\": \"they dont beleive their eyes…\", \"category_id\": 2, \"article_content\": \"article is long enough\", \"tags\": [1], \"main_photo\": \"http://pl.uh/main\", \"additional_photos\": [\"1\", \"2\", \"3\"], " ++ articleIdJSONSection ++ "}")
                session
            >>= (`shouldStartWith` "{\"article_content")

        it "returns error if no such article"
            $ editArticleDraft
                "{\"article_title\": \"they dont beleive their eyes…\", \"category_id\": 1, \"article_content\": \"article is long enough\", \"tags\": [1], \"main_photo\": \"http://pl.uh/main\", \"additional_photos\": [\"1\", \"2\", \"3\"], \"article_id\": 123445}"
                session
            >>= (`shouldBe` "{\"error\": \"no such article\"}")

        it "returns error if no such tags"
            $ editArticleDraft
                ("{\"article_title\": \"they dont beleive their eyes…\", \"category_id\": 1, \"article_content\": \"article is long enough\", \"tags\": [11234], \"main_photo\": \"http://pl.uh/main\", \"additional_photos\": [\"1\", \"2\", \"3\"], " ++ articleIdJSONSection ++ "}")
                session
            >>= (`shouldBe` "{\"error\": \"no such tag\"}")

        it "returns error if no such category"
            $ editArticleDraft
                ("{\"article_title\": \"they dont beleive their eyes…\", \"category_id\": 11234, \"article_content\": \"article is long enough\", \"tags\": [1], \"main_photo\": \"http://pl.uh/main\", \"additional_photos\": [\"1\", \"2\", \"3\"], " ++ articleIdJSONSection ++ "}")
                session
            >>= (`shouldBe` "{\"error\": \"no such category\"}")


    describe "getArticleDraft" $ do
        it "get article"
            $ getArticleDraft ("{" ++ articleIdJSONSection ++ "}") session
            >>= (`shouldStartWith` "{\"article_content")

        it "returns error if no such article"
            $ getArticleDraft "{\"article_id\":123456}" session
            >>= (`shouldStartWith` "{\"error\": \"no such article\"}")


    describe "publishArticleDraft" $ do
        it "publish article"
            $ publishArticleDraft ("{" ++ articleIdJSONSection ++ "}") session
            >>= (`shouldBe` "{\"results\":\"ook\"}")

        it "returns error if no such article"
            $ publishArticleDraft "{\"article_id\":123456}" session
            >>= (`shouldBe` "{\"error\": \"no such article\"}")


    createArticleDraftResult1 <- runIO
        $ createArticleDraft "{\"article_title\": \"they dont beleive their eyes…\", \"category_id\": 1, \"article_content\": \"article is long enough\", \"tags\": [], \"main_photo\": \"http://pl.uh/main\", \"additional_photos\": [\"1\", \"2\", \"3\"]}" session

    let articleIdJSONSection1 = (!! 6) . lines $ replaceComasWithNewlines createArticleDraftResult1;

    describe "deleteArticleDraft" $ do
        it "delete article draft"
            $ deleteArticleDraft ("{" ++ articleIdJSONSection1 ++ "}") session
            >>= (`shouldBe` "{\"results\":\"ook\"}")

        it "published article can't be deleted"
            $ deleteArticleDraft ("{" ++ articleIdJSONSection ++ "}") session
            >>= (`shouldBe` "{\"error\": \"no such article\"}")

        it "returns error if no such article"
            $ deleteArticleDraft "{\"article_id\":123456}" session
            >>= (`shouldBe` "{\"error\": \"no such article\"}")


    createTagResult <- runIO
        $ createTag "{\"tag_name\": \"test tag\"}" session

    let tagIdJSONSection = last . lines $ replaceComasWithNewlines createTagResult;

    describe "createTag" $ do
        it "creates tag"
            $ shouldStartWith createTagResult "{\"tag_name\":"

        it "returns error if tag already exists"
            $ createTag "{\"tag_name\": \"test tag\"}" session
            >>= (`shouldBe` "{\"error\": \"tag with such name already exists\"}")


    before_
        (createTag "{\"tag_name\": \"test tag1\"}" session >> pure ())

        $ describe "editTag" $ do
            it "edit"
                $ editTag ("{\"tag_name\": \"test tasd\"," ++ tagIdJSONSection) session
                >>= (`shouldStartWith` "{\"tag_name\":")

            it "returns error if tag with such name already exist"
                $ editTag ("{\"tag_name\": \"test tag1\"," ++ tagIdJSONSection) session
                >>= (`shouldBe` "{\"error\": \"tag with such name already exists\"}")

            it "returns error if tag does not exists"
                $ editTag "{\"tag_id\": 12345,\"tag_name\": \"test ta\"}" session
                >>= (`shouldBe` "{\"error\": \"no such tag\"}")


    describe "getTag" $ do
        it "get"
            $ getTag ("{" ++ tagIdJSONSection)
            >>= (`shouldStartWith`  "{\"tag_name\":")

        it "returns error if tag does not exists"
            $ getTag "{\"tag_id\": 12345}"
            >>= (`shouldBe` "{\"error\": \"no such tag\"}")


    createTagResult1 <- runIO
        $ createTag "{\"tag_name\": \"test tag pluh\"}" session

    let tagId = init . drop (length ("\"tag_id\":" :: String)) . last . lines $ replaceComasWithNewlines createTagResult1;

    before_
        (createArticleDraft
            ("{\"article_title\": \"they dont beleive their eyes…\", \"category_id\": 1, \"article_content\": \"article is long enough\", \"main_photo\": \"http://pl.uh/main\", \"additional_photos\": [\"1\", \"2\", \"3\"], \"tags\": ["
                ++ tagId
                ++ "]}")
            session
        >> pure ())

        $ describe "deleteTag" $ do
            it "delete"
                $ deleteTag ("{" ++ tagIdJSONSection) session
                >>= (`shouldBe` "{\"results\":\"ook\"}")

            it "returns error if tag does not exists"
                $ deleteTag "{\"tag_id\": 12345}" session
                >>= (`shouldBe` "{\"error\": \"no such tag\"}")

            it "returns error if tag is referenced by an article"
                $ deleteTag ("{\"tag_id\": " ++ tagId ++ "}") session
                >>= (`shouldBe` "{\"error\": \"tag is referenced by an article\"}")

        
    createCommentResult <- runIO
        $ createComment "{\"article_id\": 1, \"comment_text\": \"bluasd!\"}" session

    -- {"comment_id":12,"user_id":1,"article_id":1,"comment_text":"bluasd!"}
    let commentIdJSONSection = (!! 0) . lines $ replaceComasWithNewlines createCommentResult;

    describe "createComment" $ do
        it "creates article comment"
            $ shouldStartWith createCommentResult "{\"comment_id\""

        it "returns error if no such article"
            $ createComment
                "{\"article_id\": 12345, \"comment_text\": \"bluasd!\"}"
                session
            >>= (`shouldBe` "{\"error\": \"no such article\"}")

    describe "getArticleComments" $ do
        it "get article comments"
            $ getArticleComments "{\"article_id\": 1}"
            >>= (`shouldStartWith` "[{\"comment_id\"")

        it "returns empty array if no such article"
            $ getArticleComments "{\"article_id\": 12345}"
            >>= (`shouldBe` "[]")

    describe "deleteComment" $ do
        it "delete article comment"
            $ deleteComment
                (commentIdJSONSection ++ "}")
                session
            >>= (`shouldBe` "{\"results\":\"ook\"}")

        it "returns error if no such comment"
            $ deleteComment
                "{\"comment_id\": 12345}"
                session
            >>= (`shouldBe` "{\"error\": \"no such comment\"}")
