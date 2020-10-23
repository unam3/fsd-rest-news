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
    publishArticleDraft,
    getArticleDraft,
    getArticlesByCategoryId,
    getArticlesByTagId,
    getArticlesByAnyTagId,
    getArticlesByAllTagId,
    getArticlesByTitlePart,
    getArticlesByContentPart,
    getArticlesByAuthorNamePart
    ) where

import Data.Aeson (Value)
import qualified Data.ByteString.Lazy.UTF8 as UTFLBS
import Data.Int (Int32)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Hasql.TH as TH

import Hasql.Statement (Statement(..))

createUser :: Statement (Text, Text, Text, Bool) Int32
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
        returning user_id :: int4
        |]

deleteUser :: Statement Int32 ()
deleteUser =
    [TH.resultlessStatement|
        delete
        from users
        where user_id = $1 :: int4
        |]

getUser :: Statement Int32 (Text, Text, Text, Text, Bool)
getUser =
    [TH.singletonStatement|
        select name :: text, surname :: text, avatar :: text, creation_date :: text, is_admin :: bool
        from users
        where user_id = $1 :: int4
        |]


getAuthor :: Statement Int32 (Int32, Int32, Text)
getAuthor =
    [TH.singletonStatement|
        select author_id :: int4, user_id :: int4, description :: text
        from authors
        where author_id = $1 :: int4
        |]

deleteAuthorRole :: Statement Int32 ()
deleteAuthorRole =
    [TH.resultlessStatement|
        delete
        from authors
        where author_id = $1 :: int4
        |]

promoteUserToAuthor :: Statement (Int32, Text) Int32
promoteUserToAuthor =
    [TH.singletonStatement|
        insert
        into authors (user_id, description)
            values (
                $1 :: int4,
                $2 :: text
                )
            returning (author_id :: int4)
        |]

editAuthor :: Statement (Int32, Int32, Text) ()
editAuthor =
    [TH.resultlessStatement|
        update authors
        set user_id = $2 :: int4, description = $3 :: Text
        where author_id = $1 :: int4
        |]


-- resquest's parent_id may be omitted or set to "null"
createCategory :: Statement (Text, Maybe Int32) ()
createCategory =
    [TH.singletonStatement|
        insert
        into categories (name, parent_id)
            values (
                $1 :: text,
                $2 :: int4?
                )
        |]

updateCategory :: Statement (Int32, Text, Maybe Int32) ()
updateCategory =
    [TH.singletonStatement|
        update categories
        set name = $2 :: text, parent_id = $3 :: int4?
        where category_id = $1 :: int4
        |]

getCategory :: Statement Int32 (Text, Maybe Int32)
getCategory =
    [TH.singletonStatement|
        select name :: text, parent_id :: int4?
        from categories
        where category_id = $1 :: int4
        |]

deleteCategory :: Statement Int32 ()
deleteCategory =
    [TH.resultlessStatement|
        delete
        from categories
        where category_id = $1 :: int4
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

editTag :: Statement (Int32, Text) ()
editTag =
    [TH.singletonStatement|
        update tags
        set tag_name = $2 :: text
        where tag_id = $1 :: int4
        |]

deleteTag :: Statement Int32 ()
deleteTag =
    [TH.resultlessStatement|
        delete
        from tags
        where tag_id = $1 :: int4
        |]

getTag :: Statement Int32 Text
getTag =
    [TH.singletonStatement|
        select tag_name :: text
        from tags
        where tag_id = $1 :: int4
        |]



createComment :: Statement (Int32, Text) ()
createComment =
    [TH.singletonStatement|
        insert
        into articles_comments (article_id, comment_text)
            values (
                $1 :: int4,
                $2 :: text
                )
        |]

deleteComment :: Statement Int32 ()
deleteComment =
    [TH.resultlessStatement|
        delete
        from articles_comments
        where comment_id = $1 :: int4
        |]

getArticleComments :: Statement Int32 (Int32, Text)
getArticleComments =
    [TH.singletonStatement|
        select comment_id :: int4, comment_text :: text
        from articles_comments
        where article_id = $1 :: int4
        |]


createArticleDraft :: Statement (Int32, Int32, Text, Text) Int32
createArticleDraft =
    [TH.singletonStatement|
        insert
        into articles (author, category_id, article_title, article_content, is_published)
            values (
                $1 :: int4,
                $2 :: int4,
                $3 :: text,
                $4 :: text,
                False :: bool
                )
        returning article_id :: int4
        |]

publishArticleDraft :: Statement Int32 ()
publishArticleDraft =
    [TH.resultlessStatement|
        update
        articles
        set is_published = True :: bool
        where article_id = $1 :: int4
        |]

getArticleDraft :: Statement Int32 (Maybe Value)
getArticleDraft =
    [TH.maybeStatement|
        select get_article($1 :: int4) :: json
        |]


getArticlesByCategoryId :: Statement Int32 (Maybe Value)
getArticlesByCategoryId =
    [TH.maybeStatement|
        select json_agg(aid.get_article) :: json from 
            (select get_article(article_id) from articles where category_id = $1 :: int4) as aid
        |]


--select article_ids.* from (select json_agg(get_article(article_id)) from articles_tags where tag_id = 2) as article_ids;

-- doesn't work
--select (articles_by_tag_id.*) :: json from
--    (select json_agg(get_article(article_id)) from articles_tags where tag_id = $1 :: int4) as articles_by_tag_id
getArticlesByTagId :: Statement Int32 (Maybe Value)
getArticlesByTagId =
    [TH.maybeStatement|
        select json_agg(articles_by_tag_id.get_article) :: json from
            (select get_article(article_id) from articles_tags where tag_id = $1 :: int4) as articles_by_tag_id
        |]

-- select get_article(article_id) from articles_tags where article_id = any (array[4,1]::int[]) group by article_id;
getArticlesByAnyTagId :: Statement (Vector Int32) (Maybe Value)
getArticlesByAnyTagId =
    [TH.maybeStatement|
        select json_agg(articles_ids.get_article) :: json from
            (select get_article(article_id) from articles_tags where tag_id = any ($1 :: int4[]) group by article_id) as articles_ids
        |]

-- select get_article(article_id) from (select article_id, array_agg(tag_id) as id_array from articles_tags group by article_id) as articles_tags_agg where id_array @> (array[2,1]::int[]);
getArticlesByAllTagId :: Statement (Vector Int32) (Maybe Value)
getArticlesByAllTagId =
    [TH.maybeStatement|
        select json_agg(get_article) :: json from (
            select get_article(article_id) from
                (select article_id, array_agg(tag_id) as id_array from
                    articles_tags group by article_id) as articles_tags
                where id_array @> ($1 :: int4[])) as articles_tags_agg
        |]

-- select * from articles where article_title like '%ve%';
-- select json_agg(get_article(article_id)) as articles from (select article_id  from articles where article_title like '%ve%') as foo;
getArticlesByTitlePart :: Statement Text (Maybe Value)
getArticlesByTitlePart =
    [TH.maybeStatement|
        select json_agg(get_article(article_id)) :: json from
            (select article_id from articles where article_title ilike
                '%' || regexp_replace(($1 :: text), '(%|_)', '', 'g') || '%') as articles_by_title_part
        |]

getArticlesByContentPart :: Statement Text (Maybe Value)
getArticlesByContentPart =
    [TH.maybeStatement|
        select json_agg(get_article(article_id)) :: json from
            (select article_id from articles where article_content ilike
                '%' || regexp_replace(($1 :: text), '(%|_)', '', 'g') || '%') as articles_by_content_part
        |]

{-
select get_article(article_id) from articles, (
    select authors.author_id from authors, (
        select user_id from users where name ilike '%ph%'
    ) as s_users where authors.user_id = s_users.user_id
) as s_author_ids
where author = s_author_ids.author_id;
-}
getArticlesByAuthorNamePart :: Statement Text (Maybe Value)
getArticlesByAuthorNamePart =
    [TH.maybeStatement|
        select json_agg(get_article(article_id)) :: json from articles, (
            select authors.author_id from authors, (
                select user_id from users where name ilike
                    '%' || regexp_replace(($1 :: text), '(%|_)', '', 'g') || '%'
            ) as s_users where authors.user_id = s_users.user_id
        ) as s_author_ids
        where author = s_author_ids.author_id
        |]
