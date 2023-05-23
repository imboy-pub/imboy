

```
docker network create imboy-network
```


## Dockerfile
from  https://github.com/postgis/docker-postgis/blob/master/15-3.3/Dockerfile

dev
```
cd docker
docker build --file "./postgis_15-3.3_Dockerfile_dev" -t imboy:pg15_dev .

docker run --name imboy_postgis_dev --network imboy-network -e POSTGRES_USER=imboy_user -e POSTGRES_PASSWORD=abc54321 -e POSTGRES_DB=imboy_v1 -v "pgsql15data":/var/lib/postgresql/data  -p 4322:5432 -d imboy:pg15_dev
```

Install the project...
-- Install configuration: ""
-- Installing: /usr/lib/postgresql/15/lib/pg_jieba.so
-- Installing: /usr/share/postgresql/15/extension/pg_jieba.control
-- Installing: /usr/share/postgresql/15/extension/pg_jieba--1.1.1.sql
-- Installing: /usr/share/postgresql/15/tsearch_data/jieba_base.dict
-- Installing: /usr/share/postgresql/15/tsearch_data/jieba_hmm.model
-- Installing: /usr/share/postgresql/15/tsearch_data/jieba_user.dict
-- Installing: /usr/share/postgresql/15/tsearch_data/jieba.stop
-- Installing: /usr/share/postgresql/15/tsearch_data/jieba.idf

从容器里面拷文件到宿主机
> docker cp goiissy:/root/idex.html /opt
```
docker cp imboy_postgis_dev:/usr/lib/postgresql/15/lib/pg_jieba.so ./pg_jieba/pg_jieba.so && \
docker cp imboy_postgis_dev:/usr/share/postgresql/15/extension/pg_jieba.control ./pg_jieba/pg_jieba.control && \
docker cp "imboy_postgis_dev:/usr/share/postgresql/15/extension/pg_jieba--1.1.1.sql" "./pg_jieba/pg_jieba--1.1.1.sql" && \
docker cp imboy_postgis_dev:/usr/share/postgresql/15/tsearch_data/jieba_base.dict ./pg_jieba/jieba_base.dict && \
docker cp imboy_postgis_dev:/usr/share/postgresql/15/tsearch_data/jieba_hmm.model ./pg_jieba/jieba_hmm.model && \
docker cp imboy_postgis_dev:/usr/share/postgresql/15/tsearch_data/jieba_user.dict ./pg_jieba/jieba_user.dict && \
docker cp imboy_postgis_dev:/usr/share/postgresql/15/tsearch_data/jieba.stop ./pg_jieba/jieba.stop && \
docker cp imboy_postgis_dev:/usr/share/postgresql/15/tsearch_data/jieba.idf ./pg_jieba/jieba.idf
```
/usr/share/postgresql/15
从宿主机拷文件到容器里面
> docker cp /opt/test.js goiissy:/root
```
docker cp ./pg_jieba/pg_jieba.so imboy_pg15:/usr/lib/postgresql/15/lib/pg_jieba.so && \
docker cp ./pg_jieba/pg_jieba.control imboy_pg15:/usr/share/postgresql/15/extension/pg_jieba.control && \
docker cp ./pg_jieba/pg_jieba--1.1.1.sql "imboy_pg15:/usr/share/postgresql/15/extension/pg_jieba--1.1.1.sql" && \
docker cp ./pg_jieba/jieba_base.dict imboy_pg15:/usr/share/postgresql/15/tsearch_data/jieba_base.dict && \
docker cp ./pg_jieba/jieba_hmm.model imboy_pg15:/usr/share/postgresql/15/tsearch_data/jieba_hmm.model && \
docker cp ./pg_jieba/jieba_user.dict imboy_pg15:/usr/share/postgresql/15/tsearch_data/jieba_user.dict && \
docker cp ./pg_jieba/jieba.stop imboy_pg15:/usr/share/postgresql/15/tsearch_data/jieba.stop && \
docker cp ./pg_jieba/jieba.idf imboy_pg15:/usr/share/postgresql/15/tsearch_data/jieba.idf

create extension pg_jieba;
```

pro
```
删除所有未使用的卷
docker volume prune

cd docker
docker build --file "./postgis_15-3.3_Dockerfile" -t imboy-pg:pg15.3 .

docker save -o imboy-pg-pg15.3.tar imboy-pg:pg15.3

from https://github.com/docker-library/docs/blob/master/postgres/README.md
docker run -d \
    --name imboy_pg15 \
    --network imboy-network \
    -e POSTGRES_USER=imboy_user \
    -e POSTGRES_PASSWORD=abc54321 \
    -e POSTGRES_DB=imboy_v1 \
    -e PGDATA=/var/lib/postgresql/data/pgdata \
    -v /data/docker/imboy_pg15:/var/lib/postgresql/data \
    -p 127.0.0.1:4321:5432 \
    imboy-pg:pg15.3



CREATE EXTENSION IF NOT EXISTS pg_jieba
    SCHEMA public
    VERSION "1.1.1";


select * from to_tsquery('jiebacfg', '是拖拉机学院手扶拖拉机专业的。不用多久，我就会升职加薪，当上CEO，走上人生巅峰。');


DROP SCHEMA public CASCADE;
CREATE SCHEMA public;

GRANT ALL ON SCHEMA public TO imboy_user;
GRANT ALL ON SCHEMA public TO public;

psql -h 127.0.0.1 -d imboy_v1 -U imboy_user -p 4321 -f /Users/leeyi/Downloads/imboy_v1.sql



```

## postgresql
https://github.com/postgis/postgis Star 1.4K
基于 https://github.com/postgis/docker-postgis

https://github.com/postgis/docker-postgis/blob/master/15-3.3/Dockerfile

```
mkdir -p /data/ && mkdir -p /data/docker/ && mkdir -p /data/docker/pgsql15data

# Server container
docker run --name imboy_postgis --network imboy-network -e POSTGRES_USER=imboy_user -e POSTGRES_PASSWORD=abc54321 -e POSTGRES_DB=imboy_v1 -v /data/docker/pgsql15data:/var/lib/postgresql/data  -p 4321:5432 -d postgis/postgis:15-3.3
```

## go-fastdfs
https://github.com/sjqzhang/go-fastdfs Star 3.5K

https://sjqzhang.github.io/go-fastdfs/#character

```

docker rm -f imboy_fastdfs && docker run --network=imboy-network -d --name imboy_fastdfs -v /data/docker/img_fastdfs_data:/data -p 8080:8080 -e GO_FASTDFS_DIR=/data sjqzhang/go-fastdfs

// 统计api
http://127.0.0.1:8080/stat

文件列表 dir : 要查看文件列表的目录名
http://127.0.0.1:8080/list_dir
http://127.0.0.1:8080/list_dir?dir=default
curl http://127.0.0.1:8080/list_dir?dir=location/20235
```

##

https://blog.wu-boy.com/2018/03/nginx-reverse-proxy-image-resizing/
