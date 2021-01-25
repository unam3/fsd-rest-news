{-# LANGUAGE OverloadedStrings  #-}

module RestNewsSpec where

import System.Process (readProcess)
import RestNews (runWarpWithLogger)
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


spec :: Spec
spec = do
    -- beforeAll_ runWarpWithLogger $ do
    describe "auth" $ do
        it "recognize existing user credentials"
            $ auth
            >>= (`shouldBe` "cookies are baked")

        it "return error when wrong creds"
            $ authMustFail
            >>= (`shouldBe` "wrong username/password")


    createUserResult <- runIO createUser
    -- runIO $ print createUserResult

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

    describe "createCategory" $
        it "creates category"
            $ shouldStartWith createCategoryResult "{\"category_id\":"


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


    describe "deleteTag" $ do
        it "delete"
            $ deleteTag ("{" ++ tagIdJSONSection) session
            >>= (`shouldBe` "{\"results\":\"ook\"}")

        it "returns error if tag does not exists"
            $ deleteTag "{\"tag_id\": 12345}" session
            >>= (`shouldBe` "{\"error\": \"no such tag\"}")

        it "returns error if tag is referenced by an article"
            $ pendingWith "createArticle with such tag, use it's atricle_id here"
            -- $ deleteTag "{\"tag_id\": 12345}" session
            -- >>= (`shouldBe` "{\"error\": \"tag is referenced by an article\"}")
