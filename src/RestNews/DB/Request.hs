{-# LANGUAGE QuasiQuotes #-}

module RestNews.DB.Request (
    createUser,
    deleteUser,
    getUser,
    promoteUserToAuthor,
    editAuthor,
    getAuthor,
    deleteAuthorRole,
    createCategory,
    isCategoryExist,
    getCategoryDescendants,
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
    getArticlesBySubstring,
    getArticlesSortedByPhotosNumber,
    getArticlesSortedByCreationDate,
    getArticlesSortedByAuthor,
    getArticlesSortedByCategory,
    getArticlesFilteredByCreationDate,
    getArticlesCreatedBeforeDate,
    getArticlesCreatedAfterDate,
    getCredentials
    ) where

import Data.Aeson (Value)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Hasql.TH as TH

import Hasql.Statement (Statement (..))

createUser :: Statement (Text, Text, Text, Text, Text) Value
createUser =
    [TH.singletonStatement|
        insert
        into users (
            username,
            password,
            name,
            surname,
            avatar,
            is_admin
            )
        values (
            $1 :: text,
            crypt($2 :: text, gen_salt('bf', 8)),
            $3 :: text,
            $4 :: text,
            $5 :: text,
            false
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
        select
            json_build_object(
                'user_id', user_id,
                'name', name,
                'surname', surname,
                'avatar', avatar,
                'creation_date', creation_date,
                'is_admin', is_admin
                )::json
        from users
        where user_id = $1 :: int4
        |]

getAuthor :: Statement Int32 Value
getAuthor =
    [TH.singletonStatement|
        select to_json(authors.*) :: json
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

editAuthor :: Statement (Int32, Text) Value
editAuthor =
    [TH.singletonStatement|
        update authors
        set description = $2 :: Text
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

isCategoryExist :: Statement Int32 Bool
isCategoryExist =
    [TH.singletonStatement|
        with isCategory as (
            select count(true) != 0 as exist from categories where category_id = $1 :: int4
        ) select case when isCategory.exist
            then true
            else false
            end :: bool
        from isCategory
        |]

getCategoryDescendants :: Statement Int32 Value
getCategoryDescendants =
    [TH.singletonStatement|
        with isCategory as (
            select count(true) != 0 as exist from categories where category_id = $1 :: int4
        ) select case when isCategory.exist
            then json_build_object('descendants', get_category_descendants($1 :: int4) :: int[])
            else json_build_object('error', 'no such category')
            end :: json
        from isCategory
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
        select to_json(categories.*)::json
        from categories
        where category_id = $1 :: int4
        |]

deleteCategory :: Statement Int32 Value
deleteCategory =
    [TH.singletonStatement|
        delete
        from categories
        where category_id = $1 :: int4
        returning json_build_object('results', 'ook') :: json
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
            ) :: json
        |]

getTag :: Statement Int32 Value
getTag =
    [TH.singletonStatement|
        select to_json(tags.*)::json
        from tags
        where tag_id = $1 :: int4
        |]



createComment :: Statement (Int32, Text, Int32) Value
createComment =
    [TH.singletonStatement|
        insert
        into articles_comments (article_id, comment_text, user_id)
            values (
                $1 :: int4,
                $2 :: text,
                $3 :: int4
                )
        returning json_build_object(
            'comment_id', comment_id,
            'user_id', user_id,
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
            and user_id = $2 :: int4
        returning json_build_object(
            'results', 'ook'
            ) :: json
        |]

-- by sql error we can't distinguish between no comments and no such article, so we return empty array
getArticleComments :: Statement (Int32, Maybe Int32) Value
getArticleComments =
    [TH.singletonStatement|
        select case when count(ordered) = 0
            then to_json(array[] :: int[])
            else json_agg(ordered.*)
            end :: json
        from (
            select
                comment_id,
                comment_text
            from articles_comments
            where article_id = $1 :: int4
            order by comment_id
            limit 20
            offset $2 :: int4?
        ) as ordered
        |]

{-
with created_draft as (
    insert into articles
    (author, category_id, article_content, article_title, is_published) values
    (1, 4, 'asd', 'asd', false)
    returning *
) insert into articles_tags
(tag_id, article_id)
(select unnest(array[2,1]::int[], article_id) from created_draft) returning article_id; -- returns as many rows as tags

-}
createArticleDraft :: Statement (Int32, Int32, Text, Text, Vector Int32, Text, Vector Text) Value
createArticleDraft =
    [TH.singletonStatement|
            select create_article_draft(
                $1 :: int4,
                $2 :: int4,
                $3 :: text,
                $4 :: text,
                $5 :: int4[],
                $6 :: text,
                $7 :: text[]
            ) :: json
        |]

publishArticleDraft :: Statement (Int32, Int32) Value
publishArticleDraft =
    [TH.singletonStatement|
        update articles
        set is_published = true
        where
            author = $1 :: int4
            and article_id = $2 :: int4
        returning json_build_object('results', 'ook') :: json
        |]

editArticleDraft :: Statement (Int32, Int32, Int32, Text, Text, Text, Vector Text, Vector Int32) Value
editArticleDraft =
    [TH.singletonStatement|
        select edit_article_draft(
            $1 :: int4,
            $2 :: int4,
            $3 :: int4,
            $4 :: text,
            $5 :: text,
            $6 :: text,
            $7 :: text[],
            $8 :: int4[]
        ) :: json
        |]

getArticleDraft :: Statement (Int32, Int32) Value
getArticleDraft =
    [TH.singletonStatement|
        select get_article(article_id) :: json
        from articles
        where article_id = $1 :: int4
            and author = $2 :: int4
        |]

deleteArticleDraft :: Statement (Int32, Int32) Value
deleteArticleDraft =
    [TH.singletonStatement|
        delete
        from articles
        where article_id = $1 :: int4
            and author = $2 :: int4
            and is_published = false
        returning json_build_object('results', 'ook') :: json
        |]


-- we can't easily distinguish nonexistent category_id and no articles with category_id
getArticlesByCategoryId :: Statement (Int32, Maybe Int32) Value
getArticlesByCategoryId =
    [TH.singletonStatement|
        select
            case when count(select_results) = 0
            then to_json(array[] :: int[])
            else json_agg(get_article(select_results.article_id))
            end :: json
        from (
            select article_id
            from articles
            where category_id = $1 :: int4
                and is_published = true
            order by article_id
            limit 20
            offset $2 :: int4?
        ) as select_results
        |]


--select article_ids.* from (select json_agg(get_article(article_id)) from articles_tags where tag_id = 2) as article_ids;

-- doesn't work
--select (articles_by_tag_id.*) :: json from
--    (select json_agg(get_article(article_id)) from articles_tags where tag_id = $1 :: int4) as articles_by_tag_id
getArticlesByTagId :: Statement (Int32, Maybe Int32) Value
getArticlesByTagId =
    [TH.singletonStatement|
        select
            case when count(select_results) = 0
            then to_json(array[] :: int[])
            else json_agg(get_article(select_results.article_id))
            end :: json
        from (
            select articles.article_id
            from articles
            inner join articles_tags
            on articles.article_id = articles_tags.article_id
            where tag_id = $1 :: int4
                and is_published = true
            order by article_id
            limit 20
            offset $2 :: int4?
        ) as select_results
        |]

-- select get_article(article_id) from articles_tags where article_id = any (array[4,1]::int[]) group by article_id;
getArticlesByAnyTagId :: Statement (Vector Int32, Maybe Int32) Value
getArticlesByAnyTagId =
    [TH.singletonStatement|
        select
            case when count(select_results) = 0
            then to_json(array[] :: int[])
            else json_agg(get_article(select_results.article_id))
            end :: json
        from (
            select articles.article_id
            from articles
            inner join articles_tags
            on articles.article_id = articles_tags.article_id
            where tag_id = any ($1 :: int4[])
                and is_published = true
            group by articles.article_id
            order by articles.article_id
            limit 20
            offset $2 :: int4?
        ) as select_results
        |]

-- select get_article(article_id) from (select article_id, array_agg(tag_id) as id_array from articles_tags group by article_id) as articles_tags_agg where id_array @> (array[2,1]::int[]);
getArticlesByAllTagId :: Statement (Vector Int32, Maybe Int32) Value
getArticlesByAllTagId =
    [TH.singletonStatement|
        select
            case when count(select_results) = 0
            then to_json(array[] :: int[])
            else json_agg(get_article(select_results.article_id))
            end :: json
        from (
            select articles_tags.article_id
            from articles
            inner join (select article_id, array_agg(tag_id) as id_array
                from articles_tags
                group by article_id
            ) as articles_tags
            on articles.article_id = articles_tags.article_id
            where id_array @> ($1 :: int4[])
                and is_published = true
            order by articles.article_id
            limit 20
            offset $2 :: int4?
        ) as select_results
        |]

-- select * from articles where article_title like '%ve%';
-- select json_agg(get_article(article_id)) as articles from (select article_id  from articles where article_title like '%ve%') as foo;
getArticlesByTitlePart :: Statement (Text, Maybe Int32) Value
getArticlesByTitlePart =
    [TH.singletonStatement|
        select
            case when count(select_results) = 0
            then to_json(array[] :: int[])
            else json_agg(get_article(select_results.article_id))
            end :: json
        from (
            select article_id
            from articles
            where article_title ilike
                    '%' || regexp_replace(($1 :: text), '(%|_)', '', 'g') || '%'
                and is_published = true
            order by articles.article_id
            limit 20
            offset $2 :: int4?
        ) as select_results
        |]

getArticlesByContentPart :: Statement (Text, Maybe Int32) Value
getArticlesByContentPart =
    [TH.singletonStatement|
        select
            case when count(select_results) = 0
            then to_json(array[] :: int[])
            else json_agg(get_article(select_results.article_id))
            end :: json
        from (
            select article_id
            from articles
            where article_content ilike
                    '%' || regexp_replace(($1 :: text), '(%|_)', '', 'g') || '%'
                and is_published = true
            order by articles.article_id
            limit 20
            offset $2 :: int4?
        ) as select_results
        |]

{-
select get_article(article_id) from articles, (
    select authors.author_id from authors, (
        select user_id from users where name ilike '%ph%'
    ) as s_users where authors.user_id = s_users.user_id
) as s_author_ids
where author = s_author_ids.author_id;
-}
getArticlesByAuthorNamePart :: Statement (Text, Maybe Int32) Value
getArticlesByAuthorNamePart =
    [TH.singletonStatement|
        select
            case when count(select_results) = 0
            then to_json(array[] :: int[])
            else json_agg(get_article(select_results.article_id))
            end :: json
        from (
            select article_id
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
            order by articles.article_id
            limit 20
            offset $2 :: int4?
        ) as select_results
        |]


getArticlesBySubstring :: Statement (Text, Maybe Int32) Value
getArticlesBySubstring =
    [TH.singletonStatement|
            with pattern as (
                select '%' || regexp_replace(($1 :: text), '(%|_)', '', 'g') || '%' as string
            )
            select
                case when count(select_results) = 0
                then to_json(array[] :: int[])
                else json_agg(get_article(select_results.article_id))
                end :: json
            from (
                select union_results.article_id from (
                    (
                        select article_id
                        from articles
                        inner join (
                            select authors.author_id
                            from authors
                            inner join (
                                select user_id
                                from users, pattern
                                where name ilike pattern.string
                                    or surname ilike pattern.string
                            ) as filtered_user_ids
                        on authors.user_id = filtered_user_ids.user_id
                        ) as filtered_author_ids
                        on author = filtered_author_ids.author_id
                            and is_published = true
                    union
                        select article_id
                        from articles, pattern
                        where articles.article_title ilike pattern.string
                            or articles.article_content ilike pattern.string
                            and is_published = true
                    union
                        select article_id
                        from articles, (
                            select category_id
                            from categories, pattern
                            where name ilike pattern.string
                        ) as categories_with_substring
                        where articles.category_id = categories_with_substring.category_id
                            and is_published = true
                    union
                        select articles.article_id
                        from articles
                        inner join (
                            select article_id
                            from articles_tags,(
                                select tag_id
                                from tags, pattern
                                where tag_name ilike pattern.string
                            ) as tags_with_substring
                            where articles_tags.tag_id = tags_with_substring.tag_id
                        ) as atricles_ids_with_substring_in_tag
                        on articles.article_id = atricles_ids_with_substring_in_tag.article_id
                        where articles.is_published = true
                    )
                ) as union_results
                order by union_results.article_id
                limit 20
                offset $2 :: int4?
            ) as select_results
        |]

{-
select article_id from articles order by coalesce(array_length(additional_photos, 1), 0) asc, main_photo = '' desc;
 main_photo | additional_photos
------------+-------------------
 asdjf      | {}
            | {"one link"}
            | {two,links}
 main ph    | {two,links}
            | {three,eee,links}
 main       | {thr,e,e}
            | {f,o,u,r}
 main       | {f,o,u,r}
-}
getArticlesSortedByPhotosNumber :: Statement Int32 Value
getArticlesSortedByPhotosNumber =
    [TH.singletonStatement|
        select
            case when count(select_results) = 0
            then to_json(array[] :: int[])
            else json_agg(get_article(select_results.article_id))
            end :: json
        from (
            select article_id
            from articles
            where is_published = true
            order by
                coalesce(array_length(additional_photos, 1), 0) asc,
                main_photo = '' desc
            limit 20
            offset $1 :: int4
        ) as select_results
        |]

getArticlesSortedByCreationDate :: Statement Int32 Value
getArticlesSortedByCreationDate =
    [TH.singletonStatement|
        select
            case when count(select_results) = 0
            then to_json(array[] :: int[])
            else json_agg(get_article(select_results.article_id))
            end :: json
        from (
            select article_id
            from articles
            where is_published = true
            order by creation_date asc
            limit 20
            offset $1 :: int4
        ) as select_results
        |]

-- select article_id from users inner join authors on users.user_id = authors.user_id inner join articles on authors.author_id = articles.author where is_published = true order by surname asc;
getArticlesSortedByAuthor :: Statement Int32 Value
getArticlesSortedByAuthor =
    [TH.singletonStatement|
        select
            case when count(select_results) = 0
            then to_json(array[] :: int[])
            else json_agg(get_article(select_results.article_id))
            end :: json
        from (
            select article_id
            from users
            inner join authors
            on users.user_id = authors.user_id
            inner join articles
            on authors.author_id = articles.author
            where is_published = true
            order by surname asc
            limit 20
            offset $1 :: int4
        ) as select_results
        |]

getArticlesSortedByCategory :: Statement Int32 Value
getArticlesSortedByCategory =
    [TH.singletonStatement|
        select
            case when count(select_results) = 0
            then to_json(array[] :: int[])
            else json_agg(get_article(select_results.article_id))
            end :: json
        from (
            select article_id
            from articles
            inner join categories
            on articles.category_id = categories.category_id
            where is_published = true
            order by name asc
            limit 20
            offset $1 :: int4
        ) as select_results
        |]

getArticlesFilteredByCreationDate :: Statement (Text, Maybe Int32) Value
getArticlesFilteredByCreationDate =
    [TH.singletonStatement|
        select
            case when count(select_results) = 0
            then to_json(array[] :: int[])
            else json_agg(get_article(select_results.article_id))
            end :: json
        from (
            select article_id
            from articles
            where is_published = true
                and creation_date :: date = ($1 :: text) :: date
            order by article_id
            limit 20
            offset $2 :: int4?
        ) as select_results
        |]

getArticlesCreatedBeforeDate :: Statement (Text, Maybe Int32) Value
getArticlesCreatedBeforeDate =
    [TH.singletonStatement|
        select
            case when count(select_results) = 0
            then to_json(array[] :: int[])
            else json_agg(get_article(select_results.article_id))
            end :: json
        from (
            select article_id
            from articles
            where is_published = true
                and creation_date :: date < ($1 :: text) :: date
            order by article_id
            limit 20
            offset $2 :: int4?
        ) as select_results
        |]

getArticlesCreatedAfterDate :: Statement (Text, Maybe Int32) Value
getArticlesCreatedAfterDate =
    [TH.singletonStatement|
        select
            case when count(select_results) = 0
            then to_json(array[] :: int[])
            else json_agg(get_article(select_results.article_id))
            end :: json
        from (
            select article_id
            from articles
            where is_published = true
                and creation_date :: date > ($1 :: text) :: date
            order by article_id
            limit 20
            offset $2 :: int4?
        ) as select_results
        |]

{-
select users.user_id, users.is_admin, authors.author_id from users left join authors on authors.user_id = users.user_id where users.user_id = 6;

select
    users.user_id,
    users.is_admin,
    authors.author_id
from
    users
    left join authors
    on authors.user_id = users.user_id,
    (SELECT user_id FROM users WHERE username = lower('username4') AND password = crypt('12345', password)) as matched_user
    where users.user_id = matched_user.user_id;
-}
getCredentials :: Statement (Text, Text) (Int32, Bool, Int32)
getCredentials =
    [TH.singletonStatement|
        select
            users.user_id :: int4,
            users.is_admin :: bool,
            coalesce(authors.author_id, 0) :: int4
        from
            users
            left join authors on authors.user_id = users.user_id,
            (select user_id
                from users
                where
                    username = lower($1 :: text)
                    and password = crypt($2 :: text, password)
            ) as matched_user
        where
            users.user_id = matched_user.user_id
        |]
