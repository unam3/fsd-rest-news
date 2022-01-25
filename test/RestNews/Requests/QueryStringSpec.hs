{-# LANGUAGE OverloadedStrings #-}

module RestNews.Requests.QueryStringSpec where

import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)

import RestNews.Requests.QueryString


queryString :: [(Text, Maybe Text)]
queryString = ([("author_id", Just "12"), ("abyr", Nothing), ("ad", Nothing), ("abyr", Just "Valg3"), ("meh", Nothing)])

fieldNames :: [Text]
fieldNames = ["meh", "abyr"]

spec :: Spec
spec = do
    describe "collectFields" $ do
        it "collects fields"
            $ shouldBe
                (collectFields queryString fieldNames)
                (Right [("abyr", Nothing), ("meh", Nothing)])

        it "return error if query string has no required field"
            $ shouldBe
                (collectFields queryString ["name-of-non-existent-field"])
                (Left "Query has no required parameter: name-of-non-existent-field")

    describe "filterOutNothing" $ do
        it "filters parameters without values"
            $ shouldBe
                (filterOutNothing queryString)
                (Right [("author_id", Just "12"), ("abyr", Just "Valg3")])
