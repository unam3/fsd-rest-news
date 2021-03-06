# rest-news

## Installation and Usage

Install:

- PostgresSQL 12.3 and `libpq-dev` (`postgresql-libpq-0.9.4.2` dependency):
```apt install postgresql-12 libpq-dev```
- [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)

Run from the terminal from project directory: 

```
su postgres
createuser -d -P rest-news-user
# enter password "rest" twice
createdb rest-news-db
psql rest-news-db -c "CREATE EXTENSION pgcrypto;"
psql -h 0.0.0.0 -W -f schema.psql rest-news-db rest-news-user

# add user with admin privileges: substitute LOGIN and PASSWORD with desired values
psql rest-news-db -c "INSERT INTO users (username, password, name, surname, is_admin) VALUES ('LOGIN', crypt('PASSWORD', gen_salt('bf', 8)), '~', '~', true)"

^D

stack build

# arguments: port to run rest-news, db hostname, db port, db user, db password, db name
stack exec rest-news-exe 8081 "localhost" 5432 "rest-news-user" "rest" "rest-news-db"
```

As a definitive guide to parameter values for requests one may use combination of `src/RestNews.hs`, `src/HasqlSessions.hs` and `src/AesonDefinitions.hs`.

## Shell wrapped curl-requests

With hardcoded test data are located at `sh-curl` directory. To run them execute next in terminal from project directory:

```
cd sh-curl

# set environment variables with sessions
. getCreds
```
… and then run any wrapped request.


## Tests


To run tests you need to (1) create test db and do migrations, (2) run rest news, (3) create proper db-structure before each tests run and (4) run stack tests:

```
su
su postgres
createdb rest-news-test
psql rest-news-test -c "CREATE EXTENSION pgcrypto;"

stack build
stack exec rest-news-exe 8081 "localhost" 5432 "rest-news-user" "rest" "rest-news-test"

psql -h 0.0.0.0 -W -f schema.psql rest-news-test rest-news-user
psql -h 0.0.0.0 -W -f tests-fixtures.psql rest-news-test rest-news-user

stack test
```

## Known limitations

- Deletion of inexistent entity will return response with 200 HTTP status code.

- Unused fields of json requests will be ignored silently.

- `%` and `_` characters will be stripped from `title_substring` parameter of `articles/in__title` request.

- `offset` parameter is required for next methods and must be `'{"offset": 0}'` if you won't use any offset at all:
    - articles/byPhotosNumber
    - articles/byCreationDate
    - articles/sortByAuthor
    - articles/sortByCategory
