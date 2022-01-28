{-# LANGUAGE OverloadedStrings #-}

module RestNews.Requests.QueryStringSpec where

import Data.Text (Text)
import Data.Vector (Vector, fromList)
import Test.Hspec (Spec, describe, it, shouldBe)

import RestNews.Requests.QueryString


queryString :: [(Text, Maybe Text)]
queryString = [("author_id", Just "12"), ("author_id", Nothing), ("abyr", Nothing), ("ad", Nothing), ("abyr", Just "Valg3"), ("meh", Nothing), ("tags_ids", Just "[1,2,3]"), ("pluh", Just "12"), ("mah", Just "12")]

data TestQueryStringRequest = TestQueryStringRequest {
    pluh :: Int,
    mah :: String,
    tags_ids :: Vector Int
} deriving (Eq, Show)

instance FromQuery TestQueryStringRequest where
    parseParams query =
        let requiredFieldNames = ["pluh", "mah", "tags_ids"]
            ---- Right [("tags_ids","[1,2,3]"),("mah","12"),("pluh","12")]
            -- in error . show $ collectRequiredFields requiredFieldNames queryString
        in (\ nameValueTuples ->
                (\ partiallyAppliedT ->
                    (Right . partiallyAppliedT)
                        =<< parseRequiredValue "tags_ids" (snd $ (!!) nameValueTuples 2))
                            =<< (\ partiallyAppliedT' -> (Right . partiallyAppliedT')
                                =<< parseRequiredValue "mah" (snd $ (!!) nameValueTuples 1))
                                    =<< (Right . TestQueryStringRequest)
                                        =<< parseRequiredValue "pluh" (snd $ (!!) nameValueTuples 0)
            ) =<< collectRequiredFields requiredFieldNames queryString

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
                (Right $ TestQueryStringRequest 12 "asd" (fromList [1,2,3]))
