curl -i -X POST -d '{"name": "5.110", "surname": "2ba49e2dfc0d", "avatar": "asd", "is_admin": false}' http://0.0.0.0:8081/users

curl -i -X GET -d '{"user_id": 2}' http://0.0.0.0:8081/users

curl -i -X DELETE -d '{"user_id": 3}' http://0.0.0.0:8081/users


curl -i -X POST -d '{"user_id": 3, "description": "blob deccas"}' http://0.0.0.0:8081/authors

curl -i -X PATCH -d '{"author_id": 4, "user_id": 1, "description": "patched"}' http://0.0.0.0:8081/authors

curl -i -X GET -d '{"author_id": 1}' http://0.0.0.0:8081/authors

curl -i -X DELETE -d '{"author_id": 2}' http://0.0.0.0:8081/authors


curl -i -X POST -d '{"name": "pluh", "parent_id": null}' http://0.0.0.0:8081/categories

curl -i -X PATCH -d '{"category_id": 1, "name": "pluh_pattched", "parent_id": null}' http://0.0.0.0:8081/categories

curl -i -X GET -d '{"category_id": 1}' http://0.0.0.0:8081/categories

curl -i -X DELETE -d '{"category_id": 10}' http://0.0.0.0:8081/categories


curl -i -X POST -d '{"article_id": 2, "comment_text": "bluasd!"}' http://0.0.0.0:8081/comments

curl -i -X GET -d '{"article_id": 2}' http://0.0.0.0:8081/comments

curl -i -X DELETE -d '{"article_id": 4}' http://0.0.0.0:8081/comments


curl -i -X POST -d '{"tag_name": "pluh"}' http://0.0.0.0:8081/tags

curl -i -X PATCH -d '{"tag_id": 2, "tag_name": "pluh_patched"}' http://0.0.0.0:8081/tags

curl -i -X GET -d '{"tag_id": 2}' http://0.0.0.0:8081/tags

curl -i -X DELETE -d '{"tag_id": 1}' http://0.0.0.0:8081/tags


curl -i -X POST -d '{"article_title": "they dont beleive their eyes…", "author": 1, "category_id": 1, "article_content": "article is long enough"}' http://0.0.0.0:8081/articles

# "publish" article draft
curl -i -X POST -d '{"article_id": 3}' http://0.0.0.0:8081/articles

curl -i -X GET -d '{"article_id": 2}' http://0.0.0.0:8081/articles

curl -i -X DELETE -d '{"article_id": 2}' http://0.0.0.0:8081/articles

#filtering
curl -i -X GET -d '{"category_id": 2}' http://0.0.0.0:8081/articles/category

curl -i -X GET -d '{"tag_id": 2}' http://0.0.0.0:8081/articles/tag

curl -i -X GET -d '{"tags_ids": [2, 1]}' http://0.0.0.0:8081/articles/tags__any

curl -i -X GET -d '{"tags_ids": [2, 1]}' http://0.0.0.0:8081/articles/tags__all

curl -i -X GET -d '{"title_substring": "ve"}' http://0.0.0.0:8081/articles/in__title

curl -i -X GET -d '{"content_substring": "is"}' http://0.0.0.0:8081/articles/in__content

curl -i -X GET -d '{"author_name_substring": "ph"}' http://0.0.0.0:8081/articles/in__author_name
# "If you’re
curl -i -X GET -d '{"author_name_substring": "s"}' http://0.0.0.0:8081/articles/in__author_name
