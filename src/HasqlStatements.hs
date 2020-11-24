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
    editArticleDraft,
    getArticleDraft,
    deleteArticleDraft,
    getArticlesByCategoryId,
    getArticlesByTagId,
    getArticlesByAnyTagId,
    getArticlesByAllTagId,
    getArticlesByTitlePart,
    getArticlesByContentPart,
    getArticlesByAuthorNamePart,
    getCredentials
    ) where

import Data.Aeson (Value)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Hasql.TH as TH

import Hasql.Statement (Statement(..))

createUser :: Statement (Text, Text, Text, Bool) Value
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
        returning json_build_object(
            'user_id', user_id,
            'name', name,
            'surname', surname,
            'avatar', avatar,
            'creation_date', creation_date,
            'is_admin', is_admin
            )::json
        |]

deleteUser :: Statement Int32 Value
deleteUser =
    [TH.singletonStatement|
        delete
        from users
        where user_id = $1 :: int4
        returning json_build_object( 
            'results', 'ook'
            )::json
        |]

getUser :: Statement Int32 Value
getUser =
    [TH.singletonStatement|
        select json_agg(users.*) :: json
        from users
        where user_id = $1 :: int4
        |]


getAuthor :: Statement Int32 Value
getAuthor =
    [TH.singletonStatement|
        select json_agg(authors.*) :: json
        from authors
        where author_id = $1 :: int4
        |]

deleteAuthorRole :: Statement Int32 Value
deleteAuthorRole =
    [TH.singletonStatement|
        delete
        from authors
        where author_id = $1 :: int4
        returning json_build_object( 
            'results', 'ook'
            )::json
        |]

promoteUserToAuthor :: Statement (Int32, Text) Value
promoteUserToAuthor =
    [TH.singletonStatement|
        insert
        into authors (user_id, description)
            values (
                $1 :: int4,
                $2 :: text
                )
        returning json_build_object(
            'author_id', author_id,
            'user_id', user_id,
            'description', description
            )::json
        |]

editAuthor :: Statement (Int32, Int32, Text) Value
editAuthor =
    [TH.singletonStatement|
        update authors
        set user_id = $2 :: int4, description = $3 :: Text
        where author_id = $1 :: int4
        returning json_build_object(
            'author_id', author_id,
            'user_id', user_id,
            'description', description
            )::json
        |]


-- resquest's parent_id may be omitted or set to "null"
createCategory :: Statement (Text, Maybe Int32) Value
createCategory =
    [TH.singletonStatement|
        insert
        into categories (name, parent_id)
            values (
                $1 :: text,
                $2 :: int4?
                )
        returning json_build_object(
            'name', name,
            'category_id', category_id,
            'parent_id', parent_id
            )::json
        |]

updateCategory :: Statement (Int32, Text, Maybe Int32) Value
updateCategory =
    [TH.singletonStatement|
        update categories
        set name = $2 :: text, parent_id = $3 :: int4?
        where category_id = $1 :: int4
        returning json_build_object(
            'name', name,
            'category_id', category_id,
            'parent_id', parent_id
            )::json
        |]

getCategory :: Statement Int32 Value
getCategory =
    [TH.singletonStatement|
        select json_agg(categories.*)::json
        from categories
        where category_id = $1 :: int4
        |]

deleteCategory :: Statement Int32 Value
deleteCategory =
    [TH.singletonStatement|
        delete
        from categories
        where category_id = $1 :: int4
        returning json_build_object( 
            'results', 'ook'
            )::json
        |]



createTag :: Statement Text Value
createTag =
    [TH.singletonStatement|
        insert
        into tags (tag_name)
            values (
                $1 :: text
                )
        returning json_build_object(
            'tag_name', tag_name,
            'tag_id', tag_id
            )::json
        |]

editTag :: Statement (Int32, Text) Value
editTag =
    [TH.singletonStatement|
        update tags
        set tag_name = $2 :: text
        where tag_id = $1 :: int4
        returning json_build_object(
            'tag_name', tag_name,
            'tag_id', tag_id
            )::json
        |]

deleteTag :: Statement Int32 Value
deleteTag =
    [TH.singletonStatement|
        delete
        from tags
        where tag_id = $1 :: int4
        returning json_build_object( 
            'results', 'ook'
            )::json
        |]

getTag :: Statement Int32 Value
getTag =
    [TH.singletonStatement|
        select json_agg(tags.*)::json
        from tags
        where tag_id = $1 :: int4
        |]



createComment :: Statement (Int32, Text, Int32) Value
createComment =
    [TH.singletonStatement|
        insert
        into articles_comments (article_id, comment_text, author)
            values (
                $1 :: int4,
                $2 :: text,
                $3 :: int4
                )
        returning json_build_object(
            'comment_id', comment_id,
            'author_id', author,
            'article_id', article_id,
            'comment_text', comment_text
            )::json
        |]

deleteComment :: Statement (Int32, Int32) Value
deleteComment =
    [TH.singletonStatement|
        delete
        from articles_comments
        where comment_id = $1 :: int4
            and author = $2 :: int4
        returning json_build_object( 
            'results', 'ook'
            )::json
        |]

getArticleComments :: Statement Int32 Value
getArticleComments =
    [TH.singletonStatement|
        select json_agg(json_build_object('article_id', article_id, 'comment_text', comment_text))::json
        from articles_comments
        where article_id = $1 :: int4
        |]


createArticleDraft :: Statement (Int32, Int32, Text, Text) Value
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
        returning json_build_object(
            'article_id', article_id,
            'author', author,
            'category_id', category_id,
            'article_title', article_title,
            'article_content', article_content,
            'is_published', is_published
            )::json
        |]

publishArticleDraft :: Statement (Int32, Int32) Value
publishArticleDraft =
    [TH.singletonStatement|
        update articles
        set is_published = true
        where
            author = $1 :: int4
            and article_id = $2 :: int4
        returning json_build_object(
            'article_id', article_id,
            'author', author,
            'category_id', category_id,
            'article_title', article_title,
            'article_content', article_content,
            'is_published', is_published
            )::json
        |]

editArticleDraft :: Statement (Int32, Int32, Int32, Text, Text) Value
editArticleDraft =
    [TH.singletonStatement|
        update articles
        set category_id = $3 :: int4,
            article_title = $4 :: text,
            article_content = $5 :: text
        where
            author = $1 :: int4
            and article_id = $2 :: int4
        returning json_build_object(
            'article_id', article_id,
            'author', author,
            'category_id', category_id,
            'article_title', article_title,
            'article_content', article_content,
            'is_published', is_published
            )::json
        |]

getArticleDraft :: Statement (Int32, Int32) Value
getArticleDraft =
    [TH.singletonStatement|
        select get_article($1 :: int4) :: json
        where (
            select *
            from articles
            where article_id = $1 :: int4
                and author = $2 :: int4
        ) is not null
        |]

deleteArticleDraft :: Statement (Int32, Int32) Value
deleteArticleDraft =
    [TH.singletonStatement|
        delete
        from articles
        where article_id = $1 :: int4
            and author = $2 :: int4
            and is_published = false
        returning json_build_object( 
            'results', 'ook'
            )::json
        |]


getArticlesByCategoryId :: Statement Int32 Value
getArticlesByCategoryId =
    [TH.singletonStatement|
        select json_agg(aid.get_article) :: json
        from (
            select get_article(article_id)
            from articles
            where category_id = $1 :: int4
                and is_published = true
        ) as aid
        |]


--select article_ids.* from (select json_agg(get_article(article_id)) from articles_tags where tag_id = 2) as article_ids;

-- doesn't work
--select (articles_by_tag_id.*) :: json from
--    (select json_agg(get_article(article_id)) from articles_tags where tag_id = $1 :: int4) as articles_by_tag_id
getArticlesByTagId :: Statement Int32 Value
getArticlesByTagId =
    [TH.singletonStatement|
        select json_agg(articles_by_tag_id.get_article) :: json
        from (select get_article(articles_ids_filtered.article_id)
            from (select articles.article_id
                from articles
                inner join articles_tags
                on articles.article_id = articles_tags.article_id
                where tag_id = $1 :: int4
                    and is_published = true
            ) as articles_ids_filtered
        ) as articles_by_tag_id
        |]

-- select get_article(article_id) from articles_tags where article_id = any (array[4,1]::int[]) group by article_id;
getArticlesByAnyTagId :: Statement (Vector Int32) Value
getArticlesByAnyTagId =
    [TH.singletonStatement|
        select json_agg(articles_filtered.get_article) :: json
            from (select get_article(articles_ids_filtered.article_id)
                from (select articles.article_id
                    from articles
                    inner join articles_tags
                    on articles.article_id = articles_tags.article_id
                    where tag_id = any ($1 :: int4[])
                        and is_published = true
                    group by articles.article_id
                ) as articles_ids_filtered
            ) as articles_filtered
        |]

-- select get_article(article_id) from (select article_id, array_agg(tag_id) as id_array from articles_tags group by article_id) as articles_tags_agg where id_array @> (array[2,1]::int[]);
getArticlesByAllTagId :: Statement (Vector Int32) Value
getArticlesByAllTagId =
    [TH.singletonStatement|
        select json_agg(get_article) :: json
            from (select get_article(articles_tags.article_id)
                from articles
                inner join (select article_id, array_agg(tag_id) as id_array
                    from articles_tags
                    group by article_id
                ) as articles_tags
                on articles.article_id = articles_tags.article_id
                where id_array @> ($1 :: int4[])
                    and is_published = true
            ) as articles_tags_agg
        |]

-- select * from articles where article_title like '%ve%';
-- select json_agg(get_article(article_id)) as articles from (select article_id  from articles where article_title like '%ve%') as foo;
getArticlesByTitlePart :: Statement Text Value
getArticlesByTitlePart =
    [TH.singletonStatement|
        select json_agg(get_article(article_id)) :: json
            from (select article_id
                from articles
                where article_title ilike
                        '%' || regexp_replace(($1 :: text), '(%|_)', '', 'g') || '%'
                    and is_published = true
            ) as articles_by_title_part
        |]

getArticlesByContentPart :: Statement Text Value
getArticlesByContentPart =
    [TH.singletonStatement|
        select json_agg(get_article(article_id)) :: json
            from (select article_id
                from articles
                where article_content ilike
                        '%' || regexp_replace(($1 :: text), '(%|_)', '', 'g') || '%'
                    and is_published = true
            ) as articles_by_content_part
        |]

{-
select get_article(article_id) from articles, (
    select authors.author_id from authors, (
        select user_id from users where name ilike '%ph%'
    ) as s_users where authors.user_id = s_users.user_id
) as s_author_ids
where author = s_author_ids.author_id;
-}
getArticlesByAuthorNamePart :: Statement Text Value
getArticlesByAuthorNamePart =
    [TH.singletonStatement|
        select json_agg(get_article(article_id)) :: json
            from articles
            inner join (select authors.author_id
                from authors
                inner join (select user_id
                    from users
                    where name ilike
                        '%' || regexp_replace(($1 :: text), '(%|_)', '', 'g') || '%'
                ) as filtered_user_ids
                on authors.user_id = filtered_user_ids.user_id
            ) as filtered_author_ids
            on author = filtered_author_ids.author_id
                and is_published = true
        |]


--select users.user_id, users.is_admin, authors.author_id from users left join authors on authors.user_id = users.user_id where users.user_id = 6;
getCredentials :: Statement () (Int32, Bool, Int32)
getCredentials =
    [TH.singletonStatement|
        select
            users.user_id :: int4,
            users.is_admin :: bool,
            coalesce(authors.author_id, 0) :: int4
        from
            users
            left join authors on authors.user_id = users.user_id
        where
            users.user_id = 1
        |]
