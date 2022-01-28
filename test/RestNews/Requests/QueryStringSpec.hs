{-# LANGUAGE OverloadedStrings #-}

module RestNews.Requests.QueryStringSpec where

import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)

import RestNews.Requests.QueryString


queryString :: [(Text, Maybe Text)]
queryString = ([("author_id", Just "12"), ("author_id", Nothing), ("abyr", Nothing), ("ad", Nothing), ("abyr", Just "Valg3"), ("meh", Nothing)])

fieldNames :: [Text]
fieldNames = ["meh", "abyr"]

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
