
基于 https://github.com/postgis/docker-postgis

https://github.com/postgis/docker-postgis/blob/master/15-3.3/Dockerfile

```
docker network create imboy-network

# Server container
docker run --name imboy-postgis --network imboy-network -e POSTGRES_USER=imboy_user -e POSTGRES_PASSWORD=abc123456 -e POSTGRES_DB=imboy_v1 -e PGDATA=/www/server/pgsql/data -p 5432:5432 -d postgis/postgis:15-3.3

ln -s /www/server/pgsql/data/ ~/workspace/imboy/pgsql_data


docker exec -ti some-postgis psql -U postgres
```
