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


curl -i -X POST -d '{"arcticle_name": "pluh"}' http://0.0.0.0:8081/arcticles

curl -i -X PATCH -d '{"arcticle_id": 2, "arcticle_name": "pluh_patched"}' http://0.0.0.0:8081/arcticles

curl -i -X GET -d '{"arcticle_id": 2}' http://0.0.0.0:8081/arcticles

curl -i -X DELETE -d '{"arcticle_id": 1}' http://0.0.0.0:8081/arcticles
