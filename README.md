# rest-news

## Installation and usage

Install:

- PostgresSQL 12.3 and `libpq-dev` (`postgresql-libpq-0.9.4.2` dependency):
```apt install postgresql-12 libpq-dev```
- [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)

All configuration is done by `config.ini` file. Annotated example of which you may find in `_config.ini` file.

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

# to build rest-news
stack build

# to run
stack exec rest-news-exe

# to stop: ^C (Control and "c" keys simultaneously)
```


As a definitive guide to parameter values for requests one may use combination of `src/RestNews.hs`, `src/HasqlSessions.hs` and `src/AesonDefinitions.hs`.

## Shell wrapped curl-requests

With hardcoded test data are located at `sh-curl` directory. To run them one need to:

1) if test db does not exist — create it:
```
su
su postgres
createdb rest-news-test
psql rest-news-test -c "CREATE EXTENSION pgcrypto;"
```

2) do migrations:
```
psql -h 0.0.0.0 -W -f schema.psql rest-news-test rest-news-user
psql -h 0.0.0.0 -W -f tests-fixtures.psql rest-news-test rest-news-user

```

3) change `config.ini` (and when tests are done back to initial value if you want to work with production db):
```
dbName = rest-news-db
```
to
```
dbName = rest-news-test
```

4) run server:
```
stack build && stack exec rest-news-exe
```

5) execute in terminal from project directory:

```
cd sh-curl

# set environment variables with sessions
. getCreds
```
… and then run any wrapped request; for example:
```
./users_getUser
```


## Tests


To run tests you need to (1) create test db, (2) copy tests config file,  (3) do migrations: **create proper db-structure before each failed tests run** and (4) run stack tests:

```
#1
su
su postgres
createdb rest-news-test
psql rest-news-test -c "CREATE EXTENSION pgcrypto;"

#2
cp _tests-config.ini tests-config.ini

#3
psql -h 0.0.0.0 -W -f schema.psql rest-news-test rest-news-user
psql -h 0.0.0.0 -W -f tests-fixtures.psql rest-news-test rest-news-user

#4
stack test
```

## Known limitations

- Deletion of inexistent entity will return response with 200 HTTP status code.

- Unused fields of json requests/query strings will be ignored silently.

- Only first encountered field of the same name from query string will be processed.

- `%` and `_` characters will be stripped from `title_substring` parameter of `articles/in__title` request.

- `offset` parameter is required for next methods and must be `'{"offset": 0}'` if you won't use any offset at all:
    - articles/byPhotosNumber
    - articles/byCreationDate
    - articles/sortByAuthor
    - articles/sortByCategory


## Debug

To enable debug messages change logging level: add `DEBUG` to imports and change `INFO` to `DEBUG` in `src/RestNews.hs` `runWarpWithLogger` function.


## Project structure

One may find it out in old and simple fashion by `git ls-files | grep hs` command and browsing listed source files. Some usage of [the Handle Pattern](https://jaspervdj.be/posts/2018-03-08-handle-pattern.html) may be found.

