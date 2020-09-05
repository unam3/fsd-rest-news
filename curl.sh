curl -i -X POST -d '{"name": "5.110", "surname": "2ba49e2dfc0d", "avatar": "asd", "is_admin": false}' http://0.0.0.0:8081/authors

curl -i -X GET -d '{"user_id": 2}' http://0.0.0.0:8081/authors

curl -i -X DELETE -d '{"user_id": 3}' http://0.0.0.0:8081/authors


curl -i -X POST -d '{"name": "pluh", "parent_id": null}' http://0.0.0.0:8081/categories


curl -i -X POST -d '{"tag_name": "pluh"}' http://0.0.0.0:8081/tags

curl -i -X PATCH -d '{"tag_id": 2, "tag_name": "pluh_patched"}' http://0.0.0.0:8081/tags

curl -i -X GET -d '{"tag_id": 2}' http://0.0.0.0:8081/tags

curl -i -X DELETE -d '{"tag_id": 3}' http://0.0.0.0:8081/tags
