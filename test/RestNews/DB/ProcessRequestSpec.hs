{-# LANGUAGE OverloadedStrings #-}

module RestNews.DB.ProcessRequestSpec where

import RestNews.DB.Errors (getError)
import RestNews.DB.ProcessRequest (SessionError (..))


import Data.ByteString.Internal (unpackChars)
import Hasql.Session
import Test.Hspec (Spec, describe, it, shouldBe)


ePSQL_INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE :: QueryError
ePSQL_INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE =
    QueryError
        "SELECT CASE WHEN count(ordered) = 0 THEN to_json(ARRAY [] :: INT[]) ELSE json_agg(ordered.*) END :: json FROM (SELECT comment_id, comment_text FROM articles_comments WHERE article_id = $1 :: int4 ORDER BY comment_id LIMIT 20 OFFSET $2 :: int4) AS ordered"
        ["2","-1"]
        (
            ResultError (
                ServerError "2201X" "OFFSET must not be negative" Nothing Nothing
            )
        )


ePSQL_STRING_DATA_RIGHT_TRUNCATION :: QueryError
ePSQL_STRING_DATA_RIGHT_TRUNCATION =
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

ePSQL_FOREIGN_KEY_VIOLATION :: QueryError
ePSQL_FOREIGN_KEY_VIOLATION =
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

ePSQL_UNIQUE_VIOLATION :: QueryError
ePSQL_UNIQUE_VIOLATION =
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


eUnexpectedAmountOfRows0 :: QueryError
eUnexpectedAmountOfRows0 =
    QueryError
        "SELECT users.user_id :: int4, users.is_admin :: bool, COALESCE (authors.author_id, 0) :: int4 FROM users LEFT JOIN authors ON authors.user_id = users.user_id, (SELECT user_id FROM users WHERE username = lower($1 :: text) AND password = crypt($2 :: text, password)) AS matched_user WHERE users.user_id = matched_user.user_id"
        ["\"usasdername5\"","\"12345\""]
        (
            ResultError (
                UnexpectedAmountOfRows 0
            )
        )

eUnexpectedNull :: QueryError
eUnexpectedNull =
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

spec :: Spec
spec =
    describe "getError"
        $ do
            it "process psql_string_data_right_truncation"
                $ shouldBe (getError ePSQL_STRING_DATA_RIGHT_TRUNCATION)
                    $ Just (PSQL_STRING_DATA_RIGHT_TRUNCATION, Nothing)

            it "process psql_invalid_row_count_in_result_offset_clause"
                $ shouldBe (getError ePSQL_INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE) $ Just (
                    PSQL_INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE,
                    Just "OFFSET must not be negative"
                    )

            it "process psql_foreign_key_violation"
                $ shouldBe (getError ePSQL_FOREIGN_KEY_VIOLATION)
                    $ Just (
                        PSQL_FOREIGN_KEY_VIOLATION,
                        fmap unpackChars (Just "Key (category_id)=(1) is still referenced from table \"categories\".")
                        )

            it "process psql_unique_violation"
                $ shouldBe (getError ePSQL_UNIQUE_VIOLATION)
                    $ Just (
                        PSQL_UNIQUE_VIOLATION,
                        fmap unpackChars (Just "Key (username)=(asdq) already exists.")
                        )

            it "process UnexpectedAmountOfRows 0"
                $ shouldBe (getError eUnexpectedAmountOfRows0) $ Just (UnexpectedAmountOfRowsOrUnexpectedNull, Nothing)

            it "process UnexpectedNull"
                $ shouldBe (getError eUnexpectedNull) $ Just (UnexpectedAmountOfRowsOrUnexpectedNull, Nothing)
