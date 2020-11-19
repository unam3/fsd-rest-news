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
psql -h 0.0.0.0 -W -f schema.psql rest-news-db rest-news-user

^D

# build package
stack build

# run it
stack exec rest-news-exe
```

## Known limitations

- Deletion of inexistent entity will return response with 200 HTTP status code.

- Unused fields of request json will be ignored silently.

- `%` and `_` characters will be stripped from `title_substring` parameter of `articles/in__title` request.
