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
    "{\"username\": \"createUserTest1\", \"password\": \"check, indeed\", \"name\": \"name\", \"surname\": \"surname\", \"avatar\": \"asd\"}"
    "http://0.0.0.0:8081/users"

deleteUser :: String -> IO String
deleteUser session = curl
    "DELETE"
    session
    "{\"user_id\": 12345}"
    "http://0.0.0.0:8081/users"

spec :: Spec
spec = do
    --runWarpWithLogger

    describe "auth user" .
         it "recognize existing user credentials"
         $ auth
         >>= (`shouldBe` "cookies are baked")

    describe "auth with wrong creds" .
         it "return error"
         $ authMustFail
         >>= (`shouldBe` "wrong username/password")

    describe "createUser" .
         it "create user"
         -- {"name":"name","is_admin":false,"creation_date":"2021-01-19T14:30:26.911449","surname":"surname","user_id":35,"avatar":"asd"}
         $ createUser
         >>= (`shouldBe` "{\"name")
            . take 6

    describe "createUser with already existed user data" .
         it "create user will return error"
         $ createUser
         >>= (`shouldBe` "{\"error\": \"user with this username already exists\"}")

    -- getUser

    describe "delete existing user" .
         it "successful user deletion"
         $ getSession
         >>= deleteUser
         >>= (`shouldBe` "{\"results\": \"ook\"}")

    describe "delete non-existent user" .
         it "successful user deletion"
         $ getSession
         >>= deleteUser
         >>= (`shouldBe` "{\"error\": \"such user does not exist\"}")
