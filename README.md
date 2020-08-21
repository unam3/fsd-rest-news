# rest-news

## Installation and Usage

Install:

- PostgresSQL 12.3 and `libpq-dev` (`postgresql-libpq-0.9.4.2` dependency): `apt install postgresql-12 libpq-dev`
- [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)

Run from the terminal from project directory: 

```
su postgres
createuser -d -P rest-news-user
# enter password "rest" twice
createdb rest-news-db
^D

psql -d rest-news-db -f schema.psql

# build package
stack build

# run it
stack exec rest-news-exe
```
