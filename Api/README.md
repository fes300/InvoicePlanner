# Api Documentation
a simple api for simple requests

## To setup you dev env

1. start the `postgres` and `adminer` instances
  ```sh
    $ docker-compose up
  ```

2. run the migrations
   ?

3. start the api:
  ```sh
    $> stack build
    $> stack exec Api-exe
  ```
## Log-in into `adminer`
  Server: `db` *-- the name of the postgres service in `docker-copose.yml`*
  Username: `user`
  Password: `password`
  Database: `api`

## Run the tests
to run the tests simply execute the command:
```sh
$> stack test
```
