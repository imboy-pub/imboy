

```
docker network create imboy-network

docker network inspect -f '{{range .IPAM.Config}}{{.Subnet}}{{end}}' imboy-network


docker exec imboy_postgis cat /etc/hosts
docker exec imboy_fastdfs cat /etc/hosts


docker exec -it imboy_api bash
docker exec -it imboy_fastdfs bash
    ping imboy_postgis
```

# docker 云沙箱中的 Erlang

* https://github.com/oltarasenko/erlang_distribution_in_docker
* https://blog.erlware.org/epmdlessless/
* https://github.com/tsloughter/epmdless

## docker-compose.yml

```
docker-compose -f docker-compose.yml up

# 如果你想在后台执行该服务可以加上 -d 参数：
docker-compose -f docker-compose.yml up -d



shasum -a 256 ~/Downloads/otp-OTP-26.1.2.tar.gz
56042d53b30863d4e720ebf463d777f0502f8c986957fc3a9e63dae870bbafe0

openssl sha256 ~/Downloads/otp-OTP-26.1.2.tar.gz
56042d53b30863d4e720ebf463d777f0502f8c986957fc3a9e63dae870bbafe0

openssl sha256 ~/Downloads/otp-OTP-25.3.2.6.tar.gz
SHA256(/Users/leeyi/Downloads/otp-OTP-25.3.2.6.tar.gz)= 67e0f5c209a335cfc216a57b1f016072a69eb9683d36d6d101bf2f60a2e45926

zsh
OTP_DOWNLOAD_SHA256="56042d53b30863d4e720ebf463d777f0502f8c986957fc3a9e63dae870bbafe0"
echo "$OTP_DOWNLOAD_SHA256  otp-src.tar.gz" | sha256sum -c
echo "$OTP_DOWNLOAD_SHA256  /Users/leeyi/Downloads/otp-OTP-26.1.2.tar.gz" | sha256sum -c



echo "$OTP_DOWNLOAD_SHA256  /Users/leeyi/Downloads/otp-OTP-26.1.2.tar.gz" | sha256sum -c

REBAR3_DOWNLOAD_SHA256="2855b5784300865d2e43cb7a135cb2bba144cf15214c619065b918afc8cc6eb9"
echo "$REBAR3_DOWNLOAD_SHA256  /Users/leeyi/Downloads/rebar3-3.22.1.tar.gz" | sha256sum -c
```

## Dockerfile

### Erlang 26

```
docker build --file "docker/imboy_Dockerfile_dev" -t imboy/imboy-api:dev .
docker push imboy/imboy-api:dev

docker run -it imboy-api:dev

docker build --file "docker/imboy_Dockerfile_dev" -t imboy/imboy-api:0.1.3 .
docker push imboy/imboy-api:0.1.3



mkdir -p /www /www/wwwroot && git clone https://gitee.com/imboy-pub/imboy.git imboy-api && cd imboy-api
git fetch origin dev && git checkout dev
make deps

```

### Erlang 25
```
https://github.com/erlang/otp/archive/refs/tags/OTP-25.3.2.6.tar.gz

https://github.com/erlang/otp/archive/refs/tags/otp_src_25.3.2.6.tar.gz
```


### PG16
```
cd docker

wget https://raw.githubusercontent.com/postgis/docker-postgis/master/16-3.4/Dockerfile -O 'postgis_16-3.4_Dockerfile'
```

### PG15
from  https://github.com/postgis/docker-postgis/blob/master/15-3.4/Dockerfile

dev
```
docker build --file "./docker/postgis15_Dockerfile_dev" -t imboy/imboy-pg:15.3.4.1.dev .

docker run --name imboy_postgis_dev_0.1.2 --network imboy-network -e POSTGRES_USER=imboy_user -e POSTGRES_PASSWORD=abc54321 -e POSTGRES_DB=imboy_v1 -v "pgsql15data":/var/lib/postgresql/data  -p 4321:5432 -d imboy-pg:15.3.4.1.dev

docker build --file "./docker/postgis15_Dockerfile" -t imboy-pg:15.3.4.1 .
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
docker cp imboy_postgis_dev_0.1.2:/usr/lib/postgresql/15/lib/pg_jieba.so ./docker/pg_jieba/pg_jieba.so && \
...
```
/usr/share/postgresql/15
从宿主机拷文件到容器里面
> docker cp /opt/test.js goiissy:/root
```
docker cp ./docker/pg_jieba/pg_jieba.so imboy-pg:15./usr/lib/postgresql/15/lib/pg_jieba.so
...

create extension pg_jieba;


echo "shared_preload_libraries = 'timescaledb'" >> /var/lib/postgresql/data/pgdata/postgresql.conf
CREATE EXTENSION IF NOT EXISTS timescaledb;
```

pro
```
删除所有未使用的卷
docker volume prune

cd docker
docker build --file "./docker/postgis15_Dockerfile" -t imboy-pg:15.3.4.1 .

from https://github.com/docker-library/docs/blob/master/postgres/README.md
 docker rm -f imboy-pg 15.&& docker run -d \
    --name imboy-pg 15.\
    --network imboy-network \
    -e POSTGRES_USER=imboy_user \
    -e POSTGRES_PASSWORD=abc54321 \
    -e POSTGRES_DB=imboy_v1 \
    -e PGDATA=/var/lib/postgresql/data/pgdata \
    -v /data/docker/imboy-pg:15./var/lib/postgresql/data \
    -p 127.0.0.1:4321:5432 \
    imboy-pg:15.3.4.1


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

curl http://127.0.0.1:8080/get_file_info?md5=0505831cf4baf0e84a421e96cdb064e963dcca1a
curl 'http://127.0.0.1:8080/get_file_info?path=/img/20238/21_12/cjhe7e988ho9tm77og6g.jpg'

curl http://127.0.0.1:8080/get_file_info?md5=f3fd20e8010f94bc8fd3e49701a7ef83370b31fd
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

### ZLMediaKit

```
#国内用户推荐从同步镜像网站gitee下载
git clone --depth 1 https://gitee.com/xia-chu/ZLMediaKit
cd ZLMediaKit
#千万不要忘记执行这句命令
git submodule update --init

mkdir build && cd build
cmake .. -DENABLE_WEBRTC=true  -DOPENSSL_ROOT_DIR=/usr/local/openssl  -DOPENSSL_LIBRARIES=/opt/homebrew/Cellar/openssl@1.1/1.1.1v/lib
cmake --build . --target MediaServer
```

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
