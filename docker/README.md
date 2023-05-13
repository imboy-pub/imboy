

```
docker network create imboy-network
```


## Dockerfile

```
cd docker
docker build --file "./postgis_15-3.3_Dockerfile" -t imboy:0.1_postgis15-3.3_pgroonga3.0.2 .
```


## postgresql
https://github.com/postgis/postgis Star 1.4K
基于 https://github.com/postgis/docker-postgis

https://github.com/postgis/docker-postgis/blob/master/15-3.3/Dockerfile

```
mkdir -p /data/docker/pgsql15data

# Server container
docker run --name imboy_postgis --network imboy-network -e POSTGRES_USER=imboy_user -e POSTGRES_PASSWORD=abc54321 -e POSTGRES_DB=imboy_v1 -v /data/docker/pgsql15data:/var/lib/postgresql/data  -p 4321:5432 -d postgis/postgis:15-3.3
```

## go-fastdfs
https://github.com/sjqzhang/go-fastdfs Star 3.5K

https://sjqzhang.github.io/go-fastdfs/#character

```


docker rm -f imboy_fastdfs && docker run --network=imboy-network -d --name imboy_fastdfs -v /data/docker/img_fastdfs_data:/data -p 8080:8080 -e GO_FASTDFS_DIR=/data sjqzhang/go-fastdfs
```
