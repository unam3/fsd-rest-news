{-# LANGUAGE QuasiQuotes        #-}

module HasqlStatements (
    createUser,
    deleteUser,
    getUser,
    promoteUserToAuthor,
    editAuthor,
    getAuthor,
    deleteAuthorRole,
    createCategory,
    updateCategory,
    getCategory,
    deleteCategory,
    createTag,
    editTag,
    getTag,
    deleteTag,
    createComment,
    deleteComment,
    getArticleComments,
    createArticleDraft,
    publishArticleDraft
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
    [TH.resultlessStatement|
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


getAuthor :: Statement Int16 (Int16, Int16, Text)
getAuthor =
    [TH.singletonStatement|
        select author_id :: int2, user_id :: int2, description :: text
        from authors
        where author_id = $1 :: int2
        |]

deleteAuthorRole :: Statement Int16 ()
deleteAuthorRole =
    [TH.resultlessStatement|
        delete
        from authors
        where author_id = $1 :: int2
        |]

promoteUserToAuthor :: Statement (Int16, Text) Int16
promoteUserToAuthor =
    [TH.singletonStatement|
        insert
        into authors (user_id, description)
            values (
                $1 :: int2,
                $2 :: text
                )
            returning (author_id :: int2)
        |]

editAuthor :: Statement (Int16, Int16, Text) ()
editAuthor =
    [TH.resultlessStatement|
        update authors
        set user_id = $2 :: int2, description = $3 :: Text
        where author_id = $1 :: int2
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

deleteCategory :: Statement Int16 ()
deleteCategory =
    [TH.resultlessStatement|
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
    [TH.resultlessStatement|
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



createComment :: Statement (Int16, Text) ()
createComment =
    [TH.singletonStatement|
        insert
        into articles_comments (article_id, comment_text)
            values (
                $1 :: int2,
                $2 :: text
                )
        |]

deleteComment :: Statement Int16 ()
deleteComment =
    [TH.resultlessStatement|
        delete
        from articles_comments
        where comment_id = $1 :: int2
        |]

getArticleComments :: Statement Int16 (Int16, Text)
getArticleComments =
    [TH.singletonStatement|
        select comment_id :: int2, comment_text :: text
        from articles_comments
        where article_id = $1 :: int2
        |]


createArticleDraft :: Statement (Int16, Int16, Text, Text) Int16
createArticleDraft =
    [TH.singletonStatement|
        insert
        into articles (author, category_id, article_title, article_content, is_published)
            values (
                $1 :: int2,
                $2 :: int2,
                $3 :: text,
                $4 :: text,
                False :: bool
                )
        returning article_id :: int2
        |]

publishArticleDraft :: Statement Int16 ()
publishArticleDraft =
    [TH.resultlessStatement|
        update
        articles
        set is_published = True :: bool
        where article_id = $1 :: int2
        |]
