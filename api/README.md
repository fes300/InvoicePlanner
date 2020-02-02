# Api Documentation
a simple api for simple requests

## To setup you dev env

1. start the `postgres` and `adminer` instances
  ```sh
    $> docker-compose up
  ```

2. run the migrations
   ```sh
    $> cd ./migrations
    $> flyway migrate
   ```

3. start a compilation errors loop
  ```sh
    $> ghcid
  ```

4. build the project:
  ```sh
    $> stack build
    $> stack exec api
  ```

5. start the api:
  ```sh
    $> stack exec api
  ```

6. generate Elm api:
  ```sh
    $> stack exec generate-api
  ```

## Log-in into `adminer`
  - Server: `db` *-- this is actually the name of the postgres service in `docker-copose.yml`*
  - Username: `user`
  - Password: `password`
  - Database: `api`

## Run the tests
```sh
  $> stack test
```
