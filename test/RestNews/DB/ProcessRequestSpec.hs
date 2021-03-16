{-# LANGUAGE OverloadedStrings  #-}

module RestNews.DB.ProcessRequestSpec where

import RestNews.DB.ProcessRequest (getError)

import Data.ByteString.Internal (unpackChars)
import Hasql.Session
import Test.Hspec

e22001 :: Either QueryError resultsType
e22001 =
    Left (
        QueryError
            "SELECT edit_article_draft($1 :: int4, $2 :: int4, $3 :: int4, $4 :: text, $5 :: text, $6 :: text, $7 :: text[], $8 :: int4[]) :: json"
            ["1","9","2","\"PATCHEDttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt\"","\"PATCHED\"","\"fs\"","[\"9\", \"2\"]","[1, 2]"]
            (
                ResultError
                    (
                        ServerError
                            "22001"
                            "value too long for type character varying(80)"
                            Nothing
                            Nothing
                    )
            )
    )

e23503 :: Either QueryError resultsType
e23503 = 
    Left (
        QueryError
            "WITH delete_results AS (DELETE FROM categories WHERE category_id = $1 :: int4 RETURNING *) SELECT CASE WHEN count(delete_results) = 0 THEN json_build_object('error', 'no such category') ELSE json_build_object('results', 'ook') END :: json FROM delete_results"
            ["1"]
            (
                ResultError (
                    ServerError
                        "23503"
                        "update or delete on table \"categories\" violates foreign key constraint \"categories_parent_id_fkey\" on table \"categories\""
                        (Just "Key (category_id)=(1) is still referenced from table \"categories\".")
                        Nothing
                )
            )
    )

e23505 :: Either QueryError resultsType
e23505 = 
    Left (
        QueryError
            "INSERT INTO users (username, password, name, surname, avatar, is_admin) VALUES ($1 :: text, crypt($2 :: text, gen_salt('bf', 8)), $3 :: text, $4 :: text, $5 :: text, FALSE) RETURNING json_build_object('user_id', user_id, 'name', name, 'surname', surname, 'avatar', avatar, 'creation_date', creation_date, 'is_admin', is_admin) :: json"
            ["\"asdq\"","\"check, indeed\"","\"name\"","\"surname\"","\"asd\""]
            (
                ResultError (
                    ServerError
                        "23505"
                        "duplicate key value violates unique constraint \"users_username_key\""
                        (Just "Key (username)=(asdq) already exists.")
                        Nothing
                )
            )
    )


eUnexpectedAmountOfRows0 :: Either QueryError resultsType
eUnexpectedAmountOfRows0 = 
    Left (
        QueryError
            "SELECT users.user_id :: int4, users.is_admin :: bool, COALESCE (authors.author_id, 0) :: int4 FROM users LEFT JOIN authors ON authors.user_id = users.user_id, (SELECT user_id FROM users WHERE username = lower($1 :: text) AND password = crypt($2 :: text, password)) AS matched_user WHERE users.user_id = matched_user.user_id"
            ["\"usasdername5\"","\"12345\""]
            (
                ResultError (
                    UnexpectedAmountOfRows 0
                )
            )
    )

eUnexpectedNull :: Either QueryError resultsType
eUnexpectedNull =
    Left (
        QueryError
            "SELECT json_agg(users.*) :: json FROM users WHERE user_id = $1 :: int4"
            ["20"]
            (
                ResultError (
                    RowError
                    0
                    UnexpectedNull
                )
            )
    )

spec :: Spec
spec =
    describe "getError"
        $ do
            it "process 22001"
                $ shouldBe (getError e22001) $ Just ("22001", Nothing)

            it "process 23503"
                $ shouldBe (getError e23503)
                    $ Just ("23503", fmap unpackChars (Just "Key (category_id)=(1) is still referenced from table \"categories\"."))

            it "process UnexpectedAmountOfRows 0"
                $ shouldBe (getError eUnexpectedAmountOfRows0) $ Just ("0", Nothing)

            it "process UnexpectedNull"
                $ shouldBe (getError eUnexpectedNull) $ Just ("0", Nothing)
