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

# build package
stack build

# run it
stack exec rest-news-exe
```

## Tests


To run tests you need to (1) run rest news, (2) before each tests run create proper db-structure and (3) run stack tests:

```
stack build

stack exec rest-news-exe

psql -h 0.0.0.0 -W -f schema.psql rest-news-db rest-news-user && psql -h 0.0.0.0 -W -f tests-fixtures.psql rest-news-db rest-news-user

stack test
```

## Known limitations

- Deletion of inexistent entity will return response with 200 HTTP status code.

- Unused fields of json requests will be ignored silently.

- `%` and `_` characters will be stripped from `title_substring` parameter of `articles/in__title` request.
