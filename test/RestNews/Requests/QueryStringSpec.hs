{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module RestNews.Requests.QueryStringSpec where

import Data.Text (Text)
import Data.Vector (Vector, fromList)
import Test.Hspec (Spec, describe, it, shouldBe)

import RestNews.Requests.QueryString


queryString :: [(Text, Maybe Text)]
queryString = [("author_id", Just "12"), ("author_id", Nothing), ("abyr", Nothing), ("ad", Nothing), ("abyr", Just "Valg3"), ("meh", Nothing), ("tags_ids", Just "[1,2,3]"), ("pluh", Just "12"), ("mah", Just "as\\\"d")]

data TestQueryStringRequest = TestQueryStringRequest {
    pluh :: Int,
    mah :: String,
    tags_ids :: Vector Int
} deriving (Eq, Show)

instance FromQuery TestQueryStringRequest where

    parseParams query = do
    
        let requiredFieldNames = ["pluh", "mah", "tags_ids"]

        ---- Right [("tags_ids","[1,2,3]"),("mah","asd"),("pluh","12")]
        -- Right . error . show $ collectRequiredFields requiredFieldNames queryString

        nameValueTuples <- collectRequiredFields requiredFieldNames queryString

        parsedPluh <- parseRequiredValue "pluh" (snd $ (!!) nameValueTuples 2)
        
        paresedMah <- parseTextOrStringValue "mah" (snd $ (!!) nameValueTuples 1)            

        parsedTagsIds <- parseRequiredValue "tags_ids" (snd $ (!!) nameValueTuples 0)

        Right $ TestQueryStringRequest parsedPluh paresedMah parsedTagsIds


queryString1 :: [(Text, Maybe Text)]
queryString1 = [("pluh", Just "12")]

queryString2 :: [(Text, Maybe Text)]
queryString2 = [("pluh", Just "12"), ("mah", Just "as\\\"d")]

queryString3 :: [(Text, Maybe Text)]
queryString3 = [("tags_ids", Just "[1,2,3]")]

data TestQueryStringOptionalRequest = TestQueryStringOptionalRequest {
    pluh :: Int,
    mah :: Maybe String,
    tags_ids :: Maybe (Vector Int)
} deriving (Eq, Show)

instance FromQuery TestQueryStringOptionalRequest where

    parseParams query = do

        let requiredFieldNames = ["pluh"]

        requiredNameValueTuples <- collectRequiredFields requiredFieldNames queryString

        parsedPluh <- parseRequiredValue "pluh" (snd $ (!!) requiredNameValueTuples 2)


        let optionalFieldNames = ["mah", "tags_ids"]

        optionalNameValueTuples <- collectOptionalFields optionalFieldNames queryString
        
        paresedMah <- parseOptionalTextOrStringValue "mah" (snd $ (!!) optionalNameValueTuples 1)            

        parsedTagsIds <- parseOptionalValue "tags_ids" (snd $ (!!) optionalNameValueTuples 0)


        Right $ TestQueryStringOptionalRequest parsedPluh paresedMah parsedTagsIds


spec :: Spec
spec = do
    describe "requiredFold" $ do
        it "process first encountered required field"
            $ shouldBe
                (requiredFold
                    queryString
                    (Right [])
                    "author_id"
                )
                (Right [("author_id", "12")])

        it "return error if field is without a value (?abyr)"
            $ shouldBe
                (requiredFold
                    queryString
                    (Right [])
                    "abyr"
                )
                (Left "Query string has no required field or value for it: \"abyr\"")

        it "return error if query string has no required field"
            $ shouldBe
                (requiredFold
                    queryString
                    (Right [])
                    "field_which_we_dont_have_in_querystring"
                )
                (Left "Query string has no required field or value for it: \"field_which_we_dont_have_in_querystring\"")

    describe "parseParams implementation" $ do
        it "workds for TestQueryStringRequest"
            $ shouldBe
                (parseParams queryString)
                (Right $ TestQueryStringRequest 12 "a\\\"sd" (fromList [1,2,3]))

        it "workds for TestQueryStringOptionalRequest with optional fields"
            $ shouldBe
                (parseParams queryString1)
                (Right $ TestQueryStringOptionalRequest 12 Nothing Nothing)
