

```
docker network create imboy-network
```

# docker 云沙箱中的 Erlang

* https://github.com/oltarasenko/erlang_distribution_in_docker
* https://blog.erlware.org/epmdlessless/
* https://github.com/tsloughter/epmdless

## Dockerfile
from  https://github.com/postgis/docker-postgis/blob/master/15-3.3/Dockerfile

dev
```
cd docker
docker build --file "./postgis_15-3.3_Dockerfile_dev" -t imboy_pg15_3_dev:0.1.1 .

docker run --name imboy_postgis_dev_0.1.1 --network imboy-network -e POSTGRES_USER=imboy_user -e POSTGRES_PASSWORD=abc54321 -e POSTGRES_DB=imboy_v1 -v "pgsql15data":/var/lib/postgresql/data  -p 4322:5432 -d imboy_pg15_3_dev:0.1.1

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
docker cp imboy_postgis_dev_0.1.1:/usr/lib/postgresql/15/lib/pg_jieba.so ./pg_jieba/pg_jieba.so && \
docker cp imboy_postgis_dev_0.1.1:/usr/share/postgresql/15/extension/pg_jieba.control ./pg_jieba/pg_jieba.control && \
docker cp "imboy_postgis_dev_0.1.1:/usr/share/postgresql/15/extension/pg_jieba--1.1.1.sql" "./pg_jieba/pg_jieba--1.1.1.sql" && \
docker cp imboy_postgis_dev_0.1.1:/usr/share/postgresql/15/tsearch_data/jieba_base.dict ./pg_jieba/jieba_base.dict && \
docker cp imboy_postgis_dev_0.1.1:/usr/share/postgresql/15/tsearch_data/jieba_hmm.model ./pg_jieba/jieba_hmm.model && \
docker cp imboy_postgis_dev_0.1.1:/usr/share/postgresql/15/tsearch_data/jieba_user.dict ./pg_jieba/jieba_user.dict && \
docker cp imboy_postgis_dev_0.1.1:/usr/share/postgresql/15/tsearch_data/jieba.stop ./pg_jieba/jieba.stop && \
docker cp imboy_postgis_dev_0.1.1:/usr/share/postgresql/15/tsearch_data/jieba.idf ./pg_jieba/jieba.idf


docker cp imboy_postgis_dev_0.1.1:/usr/lib/postgresql/15/lib/timescaledb.so ./timescaledb/timescaledb.so && \
docker cp imboy_postgis_dev_0.1.1:/usr/lib/postgresql/15/lib/timescaledb-2.12.0-dev.so ./timescaledb/timescaledb-2.12.0-dev.so && \
docker cp imboy_postgis_dev_0.1.1:/usr/lib/postgresql/15/lib/timescaledb-tsl-2.12.0-dev.so ./timescaledb/timescaledb-tsl-2.12.0-dev.so && \
docker cp imboy_postgis_dev_0.1.1:/usr/share/postgresql/15/extension/timescaledb.control ./timescaledb/timescaledb.control && \
docker cp "imboy_postgis_dev_0.1.1:/usr/share/postgresql/15/extension/timescaledb--2.12.0-dev.sql" "./timescaledb/timescaledb--2.12.0-dev.sql"

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

docker rename imboy-pg15 imboy_pg15

docker cp ./timescaledb/timescaledb.so imboy_pg15:/usr/lib/postgresql/15/lib/timescaledb.so && \
docker cp ./timescaledb/timescaledb-2.12.0-dev.so imboy_pg15:/usr/lib/postgresql/15/lib/timescaledb-2.12.0-dev.so && \
docker cp ./timescaledb/timescaledb-tsl-2.11.0-dev.so imboy_pg15:/usr/lib/postgresql/15/lib/timescaledb-tsl-2.11.0-dev.so && \
docker cp ./timescaledb/timescaledb.control imboy_pg15:/usr/share/postgresql/15/extension/timescaledb.control && \
docker cp "./timescaledb/timescaledb--2.11.0.sql" "imboy_pg15:/usr/share/postgresql/15/extension/timescaledb--2.11.0.sql"

echo "shared_preload_libraries = 'timescaledb'" >> /var/lib/postgresql/data/pgdata/postgresql.conf
CREATE EXTENSION IF NOT EXISTS timescaledb;
```

pro
```
删除所有未使用的卷
docker volume prune

cd docker
docker build --file "./postgis_15-3.3_Dockerfile" -t imboy_pg15_3:0.1.1 .

from https://github.com/docker-library/docs/blob/master/postgres/README.md
 docker rm -f imboy_pg15 && docker run -d \
    --name imboy_pg15 \
    --network imboy-network \
    -e POSTGRES_USER=imboy_user \
    -e POSTGRES_PASSWORD=abc54321 \
    -e POSTGRES_DB=imboy_v1 \
    -e PGDATA=/var/lib/postgresql/data/pgdata \
    -v /data/docker/imboy_pg15:/var/lib/postgresql/data \
    -p 127.0.0.1:4321:5432 \
    imboy_pg15_3:0.1.1


ALTER EXTENSION postgis UPDATE TO '3.3.3';
ALTER EXTENSION postgis_tiger_geocoder UPDATE TO '3.3.3';
ALTER EXTENSION postgis_topology UPDATE TO '3.3.3';
ALTER EXTENSION pgroonga UPDATE TO '3.0.6';



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


### a.imboy.pub
```

docker rm -f imboy_fastdfs && docker run --network=imboy-network -d --name imboy_fastdfs -v /data/docker/img_fastdfs_data:/data -p 8080:8080 -e GO_FASTDFS_DIR=/data sjqzhang/go-fastdfs


docker rm -f imboy_fastdfs && \
docker run -d \
    --name imboy_fastdfs \
    --network imboy-network \
    -v /data/docker/img_fastdfs_data:/data \
    -p 8080:8080 \
    -e GO_FASTDFS_DIR=/data \
    sjqzhang/go-fastdfs

// 统计api
http://127.0.0.1:8080/stat

文件列表 dir : 要查看文件列表的目录名
http://127.0.0.1:8080/list_dir
http://127.0.0.1:8080/list_dir?dir=default
curl http://127.0.0.1:8080/list_dir?dir=location/20235
curl 'http://127.0.0.1:8080/delete?path=/collect/107/temp.jpg&s=dev&a=xxx&v=xxx'
curl http://127.0.0.1:8080/repair_fileinfo?s=dev&a=xxx&v=xxx&width=375
```


### c.imboy.pub
```
docker rm -f imboy_fastdfs_collect && \
docker run -d \
    --name imboy_fastdfs_collect \
    --network imboy-network \
    -v /data/docker/imboy_fastdfs_collect:/data \
    -p 8081:8080 \
    -e GO_FASTDFS_DIR=/data \
    sjqzhang/go-fastdfs

// 统计api
http://127.0.0.1:8081/stat

imboy_uri:check_auth("http://c.imboy.pub/collect/image/chk7efp0poqbagho741g.HEIC?").

文件列表 dir : 要查看文件列表的目录名
http://127.0.0.1:8081/list_dir
http://127.0.0.1:8081/list_dir?dir=default
curl http://127.0.0.1:8081/list_dir?dir=location/20235
curl 'http://127.0.0.1:8081/delete?path=/collect/107/temp.jpg&s=dev&a=xxx&v=xxx'
curl 'http://127.0.0.1:8081/delete?md5=3176156f892b3509f3b39751ee50b58b6f16a3a5&s=dev&a=xxx&v=xxx'
curl http://127.0.0.1:8081/repair_fileinfo?s=dev&a=xxx&v=xxx&width=375


```

## WebRTCServer

### webrtc-streamer
```
docker run -p 9801:8000 -it mpromonet/webrtc-streamer -n raspicam -u rtsp://pi2.local:8554/unicast

```

### membrane_videoroom
https://github.com/membraneframework/membrane_videoroom
```
git clone https://github.com/membraneframework/membrane_videoroom
docker build -t membrane_videoroom .

docker rm -f imboy_rtc1 && \
docker run -d \
    --name imboy_rtc1 \
    --network imboy-network \
    -p 50000-50050:50000-50050/udp \
    -p 4000:4000/tcp \
    -e INTEGRATED_TURN_PORT_RANGE=50000-50050 \
    -e EXTERNAL_IP=192.168.0.144 \
    -e VIRTUAL_HOST=localhost \
     membrane_videoroom:latest

Finally, go to http://localhost:4000/
EXTERNAL_IP=192.168.0.144 mix phx.server
```

### SRS
```

docker rm -f imboy_srs && \
docker run -d \
    --name imboy_srs \
    --network imboy-network \
    -p 1935:1935 \
    -p 1985:1985 \
    -p 9080:8080 \
    -p 1990:1990 \
    -p 8088:8088 \
    -p 8000:8000/udp \
    -e CANDIDATE="turn:dev.imboy.pub:34780?transport=udp" \
    registry.cn-hangzhou.aliyuncs.com/ossrs/srs:5 \
    ./objs/srs -c conf/https.docker.conf


docker rm -f imboy_srs && \
docker run -d \
    --name imboy_srs \
    --network imboy-network \
    -p 1935:1935 \
    -p 1985:1985 \
    -p 9080:8080 \
    -p 1990:1990 \
    -p 8088:8088 \
    -p 8000:8000/udp \
    -e CANDIDATE="81.68.209.56" \
    registry.cn-hangzhou.aliyuncs.com/ossrs/srs:6 \
    ./objs/srs -c conf/rtc2rtmp.conf

rtmp://81.68.209.56/live/livestream

rtmp://81.68.209.56/live/livestream
live
/root/srs/trunk/conf/https.docker.conf

git clone -b develop https://gitee.com/ossrs/srs.git
cd srs/trunk
./configure
make

// 启动服务器：
CANDIDATE="192.168.0.144"
./objs/srs -c conf/srs.conf
```

https://81.68.209.56:9080/players/rtc_publisher.html?autostart=true&stream=livestream&port=9080&schema=http

本机推拉流（即浏览器和SRS都在本机），使用WebRTC推流到SRS：WebRTC: Publish

https://192.168.0.144:9080/players/rtc_publisher.html?autostart=true&stream=livestream&port=9080&schema=http

https://192.168.0.144:8088/players/rtc_publisher.html?autostart=true&stream=livestream&port=9080&schema=https

打开页面观看WebRTC流
https://192.168.0.144:8088/players/rtc_player.html?autostart=true&stream=livestream&port=9080&schema=https

##

https://blog.wu-boy.com/2018/03/nginx-reverse-proxy-image-resizing/
