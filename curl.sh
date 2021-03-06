# not an author
NOT_AN_AUTHOR_SESSION=`curl -i -X POST -d '{"username": "username5", "password": "12345"}' http://0.0.0.0:8081/auth|grep SESSION|cut -d = -f 2 -|tr -d "\r\n"`

# author
AUTHOR_SESSION=`curl -i -X POST -d '{"username": "username2", "password": "12345"}' http://0.0.0.0:8081/auth|grep SESSION|cut -d = -f 2 -|tr -d "\r\n"`

# author and admin
AUTHOR_ADMIN_SESSION=`curl -i -X POST -d '{"username": "username", "password": "12345"}' http://0.0.0.0:8081/auth|grep SESSION|cut -d = -f 2 -|tr -d "\r\n"`

# session invalidation test
SESSION=`curl -i -X POST -d '{"username": "username5", "password": "12345"}' http://0.0.0.0:8081/auth|grep SESSION|cut -d = -f 2 -|tr -d "\r\n"`
SESSION=`curl -H 'Cookie: SESSION='${SESSION} -i -X POST -d '{"username": "username5", "password": "12345"}' http://0.0.0.0:8081/auth|grep SESSION|cut -d = -f 2 -|tr -d "\r\n"`


curl -i -X POST -d '{"username": "asdq", "password": "check, indeed", "name": "name2", "surname": "surname", "avatar": "asd"}' http://0.0.0.0:8081/users

curl -i -H 'Cookie: SESSION='${NOT_AN_AUTHOR_SESSION} -X GET http://0.0.0.0:8081/users

curl -i -H 'Cookie: SESSION='${AUTHOR_ADMIN_SESSION} -X DELETE -d '{"user_id": 7}' http://localhost:8081/users


curl -i -H 'Cookie: SESSION='${AUTHOR_ADMIN_SESSION} -X POST -d '{"user_id": 6, "description": "blob deccas"}' http://0.0.0.0:8081/authors

curl -i -H 'Cookie: SESSION='${AUTHOR_ADMIN_SESSION} -X PATCH -d '{"author_id": 5, "description": "edited"}' http://0.0.0.0:8081/authors

curl -i -H 'Cookie: SESSION='${AUTHOR_ADMIN_SESSION} -X GET -d '{"author_id": 5}' http://0.0.0.0:8081/authors

curl -i -H 'Cookie: SESSION='${AUTHOR_ADMIN_SESSION} -X DELETE -d '{"author_id": 111}' http://0.0.0.0:8081/authors


curl -i -H 'Cookie: SESSION='${AUTHOR_ADMIN_SESSION} -X POST -d '{"name": "create cat test", "parent_id": null}' http://0.0.0.0:8081/categories

curl -i -H 'Cookie: SESSION='${AUTHOR_ADMIN_SESSION} -X PATCH -d '{"category_id": 8, "name": "pluh_pattched", "parent_id": null}' http://0.0.0.0:8081/categories

curl -i -X GET -d '{"category_id": 8}' http://0.0.0.0:8081/categories

curl -i -H 'Cookie: SESSION='${AUTHOR_ADMIN_SESSION} -X DELETE -d '{"category_id": 8}' http://0.0.0.0:8081/categories


curl -i -H 'Cookie: SESSION='${NOT_AN_AUTHOR_SESSION} -X POST -d '{"article_id": 2, "comment_text": "bluasd!"}' http://0.0.0.0:8081/comments

curl -i -X GET -d '{"article_id": 2}' http://0.0.0.0:8081/comments
curl -i -X GET -d '{"article_id": 2, "offset": 1}' http://0.0.0.0:8081/comments

curl -i -H 'Cookie: SESSION='${NOT_AN_AUTHOR_SESSION} -X DELETE -d '{"comment_id": 4}' http://0.0.0.0:8081/comments


curl -i -H 'Cookie: SESSION='${AUTHOR_ADMIN_SESSION} -X POST -d '{"tag_name": "pluh1"}' http://0.0.0.0:8081/tags

curl -i -H 'Cookie: SESSION='${AUTHOR_ADMIN_SESSION} -X PATCH -d '{"tag_id": 4, "tag_name": "pluh_patched"}' http://0.0.0.0:8081/tags

curl -i -X GET -d '{"tag_id": 4}' http://0.0.0.0:8081/tags

curl -i -H 'Cookie: SESSION='${AUTHOR_ADMIN_SESSION} -X DELETE -d '{"tag_id": 4}' http://0.0.0.0:8081/tags


curl -i -H 'Cookie: SESSION='${AUTHOR_SESSION} -X POST -d '{"article_title": "they dont beleive their eyes…", "category_id": 1, "article_content": "article is long enough", "tags": [1,2], "main_photo": "http://pl.uh/main", "additional_photos": ["1", "2", "3"]}' http://0.0.0.0:8081/articles

curl -i -H 'Cookie: SESSION='${AUTHOR_SESSION} -X PATCH -d '{"article_title": "PATCHED", "category_id": 2, "article_content": "PATCHED", "article_id": 9, "main_photo": "fs", "additional_photos": ["9", "2"], "tags": [1,2]}' http://0.0.0.0:8081/articles

# publish article draft
curl -i -H 'Cookie: SESSION='${AUTHOR_SESSION} -X POST -d '{"article_id": 9}' http://0.0.0.0:8081/articles

curl -i -H 'Cookie: SESSION='${AUTHOR_SESSION} -X GET -d '{"article_id": 8}' http://0.0.0.0:8081/articles

curl -i -H 'Cookie: SESSION='${AUTHOR_SESSION} -X DELETE -d '{"article_id": 10}' http://0.0.0.0:8081/articles

#filtering
curl -i -X GET -d '{"category_id": 2}' http://0.0.0.0:8081/articles/category
curl -i -X GET -d '{"category_id": 2, "offset": 1}' http://0.0.0.0:8081/articles/category

curl -i -X GET -d '{"tag_id": 2}' http://0.0.0.0:8081/articles/tag
curl -i -X GET -d '{"tag_id": 2, "offset": 1}' http://0.0.0.0:8081/articles/tag

curl -i -X GET -d '{"tags_ids": [2, 1]}' http://0.0.0.0:8081/articles/tags__any
curl -i -X GET -d '{"tags_ids": [2, 1], "offset": 1}' http://0.0.0.0:8081/articles/tags__any

curl -i -X GET -d '{"tags_ids": [2, 1]}' http://0.0.0.0:8081/articles/tags__all
curl -i -X GET -d '{"tags_ids": [2, 1], "offset": 1}' http://0.0.0.0:8081/articles/tags__all

curl -i -X GET -d '{"title_substring": "o"}' http://0.0.0.0:8081/articles/in__title
curl -i -X GET -d '{"title_substring": "o", "offset": 1}' http://0.0.0.0:8081/articles/in__title

curl -i -X GET -d '{"content_substring": "a"}' http://0.0.0.0:8081/articles/in__content
curl -i -X GET -d '{"content_substring": "a", "offset": 1}' http://0.0.0.0:8081/articles/in__content

curl -i -X GET -d '{"author_name_substring": "s"}' http://0.0.0.0:8081/articles/in__author_name

curl -i -X GET -d '{"day": "2020-12-09", "offset": 1}' http://0.0.0.0:8081/articles/createdAt

curl -i -X GET -d '{"day": "2020-12-10", "offset": 1}' http://0.0.0.0:8081/articles/createdBefore

curl -i -X GET -d '{"day": "2020-12-08", "offset": 1}' http://0.0.0.0:8081/articles/createdAfter

# sorting
curl -i -X GET -d '{"offset": 0}' http://0.0.0.0:8081/articles/byPhotosNumber

curl -i -X GET -d '{"offset": 1}' http://0.0.0.0:8081/articles/byCreationDate

curl -i -X GET -d '{"offset": 1}' http://0.0.0.0:8081/articles/sortByAuthor

curl -i -X GET -d '{"offset": 1}' http://0.0.0.0:8081/articles/sortByCategory
