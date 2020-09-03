curl -i -X POST -d '{"name": "5.110", "surname": "2ba49e2dfc0d", "avatar": "asd", "is_admin": false}' http://0.0.0.0:8081/authors

curl -i -X GET -d '{"user_id": 2}' http://0.0.0.0:8081/authors

curl -i -X DELETE -d '{"user_id": 2}' http://0.0.0.0:8081/authors
