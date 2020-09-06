{-# LANGUAGE QuasiQuotes        #-}

module HasqlStatements (
    createUser,
    deleteUser,
    getUser,
    createCategory,
    updateCategory,
    getCategory,
    deleteCategory,
    createTag,
    editTag,
    getTag,
    deleteTag
    ) where

import Data.Int (Int16)
import Data.Text (Text)
import qualified Hasql.TH as TH
import Hasql.Statement (Statement(..))

createUser :: Statement (Text, Text, Text, Bool) ()
createUser =
    [TH.singletonStatement|
        insert
        into users (name, surname, avatar, is_admin)
            values (
                $1 :: text,
                $2 :: text,
                $3 :: text,
                $4 :: bool
                )
        |]

deleteUser :: Statement Int16 ()
deleteUser =
    [TH.singletonStatement|
        delete
        from users
        where user_id = $1 :: int2
        |]

getUser :: Statement Int16 (Text, Text, Text, Text, Bool)
getUser =
    [TH.singletonStatement|
        select name :: text, surname :: text, avatar :: text, creation_date :: text, is_admin :: bool
        from users
        where user_id = $1 :: int2
        |]


-- resquest's parent_id may be omitted or set to "null"
createCategory :: Statement (Text, Maybe Int16) ()
createCategory =
    [TH.singletonStatement|
        insert
        into categories (name, parent_id)
            values (
                $1 :: text,
                $2 :: int2?
                )
        |]

updateCategory :: Statement (Int16, Text, Maybe Int16) ()
updateCategory =
    [TH.singletonStatement|
        update categories
        set name = $2 :: text, parent_id = $3 :: int2?
        where category_id = $1 :: int2
        |]

getCategory :: Statement Int16 (Text, Maybe Int16)
getCategory =
    [TH.singletonStatement|
        select name :: text, parent_id :: int2?
        from categories
        where category_id = $1 :: int2
        |]

deleteCategory :: Statement (Int16) ()
deleteCategory =
    [TH.singletonStatement|
        delete
        from categories
        where category_id = $1 :: int2
        |]



createTag :: Statement Text ()
createTag =
    [TH.singletonStatement|
        insert
        into tags (tag_name)
            values (
                $1 :: text
                )
        |]

editTag :: Statement (Int16, Text) ()
editTag =
    [TH.singletonStatement|
        update tags
        set tag_name = $2 :: text
        where tag_id = $1 :: int2
        |]

deleteTag :: Statement Int16 ()
deleteTag =
    [TH.singletonStatement|
        delete
        from tags
        where tag_id = $1 :: int2
        |]

getTag :: Statement Int16 Text
getTag =
    [TH.singletonStatement|
        select tag_name :: text
        from tags
        where tag_id = $1 :: int2
        |]
