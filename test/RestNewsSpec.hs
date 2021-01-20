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
                _ -> "-H" : concat ["Cookie: SESSION=", session] : initialData);
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

deleteUser :: String -> String -> IO String
deleteUser params session = curl
    "DELETE"
    session
    params
    "http://0.0.0.0:8081/users"


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

    describe "getUser" $ do
        it "get user information"
            $ getUser session
            >>= (`shouldBe` "{\"name\":\"Scott\",\"is_admin\":true,\"creation_date\":\"2021-01-08T19:05:24.751993\",\"surname\":\"Adams\",\"user_id\":1,\"avatar\":\"http://pluh/meh.jpg\"}")


    let userIdJSONSection = (!! 4) . lines $ replaceComasWithNewlines createUserResult;

    let userIdJSON = concat ["{", userIdJSONSection, "}"]


    describe "promoteUserToAuthor" $ do
        it "successfully makes author"
            $ do pendingWith "remove author resolution: 23503 update or delete on table \"users\" violates foreign key constraint \"authors_user_id_fkey\" on table \"authors\" (Just \"Key (user_id)=(124) is still referenced from table \"authors\"."
            -- $ promoteUserToAuthor (concat ["{", userIdJSONSection, ", \"description\": \"blob deccas\"}"]) session
            -- >>= (`shouldStartWith` "{\"author_id\":")


    describe "deleteUser" $ do
        it "successfully delete user"
            $ deleteUser userIdJSON session
            >>= (`shouldBe` "{\"results\":\"ook\"}")

        it "returns error on non-existent user"
            $ deleteUser userIdJSON session
            >>= (`shouldBe` "{\"error\": \"such user does not exist\"}")
