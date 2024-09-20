

Docker Hub 镜像加速器 https://gist.github.com/y0ngb1n/7e8f16af3242c7815e7ca2f0833d3ea6

```
docker network create imboy-network

docker network inspect -f '{{range .IPAM.Config}}{{.Subnet}}{{end}}' imboy-network

docker run -it --network imboy-network imboy/imboy-api:dev bash
docker run -it --network imboy-network imboy/imboy-pg:15.3.4.1.dev bash

inet:getaddr("imboy_fastdfs", inet).

docker exec imboy_postgis cat /etc/hosts
docker exec imboy_fastdfs cat /etc/hosts



docker exec -it imboy_eturnal sh
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

openssl sha256 ~/Downloads/otp-OTP-25.3.2.6.tar.gz
SHA256(/Users/leeyi/Downloads/otp-OTP-25.3.2.6.tar.gz)= 67e0f5c209a335cfc216a57b1f016072a69eb9683d36d6d101bf2f60a2e45926



echo "$OTP_DOWNLOAD_SHA256  /Users/leeyi/Downloads/otp-OTP-26.1.2.tar.gz" | sha256sum -c

REBAR3_DOWNLOAD_SHA256="2855b5784300865d2e43cb7a135cb2bba144cf15214c619065b918afc8cc6eb9"
echo "$REBAR3_DOWNLOAD_SHA256  /Users/leeyi/Downloads/rebar3-3.22.1.tar.gz" | sha256sum -c
```

## Dockerfile

### Erlang 26

```
docker build --file "docker/imboy_Dockerfile_dev" -t imboy/otp26:0.5.0 .

docker run -it imboy/imboy-api:dev_arm64 bash --link=imboy_postgis

docker build --file "docker/imboy_Dockerfile_dev" -t imboy/imboy-api:0.1.5 .

docker buildx build --platform linux/amd64 --file "docker/imboy_Dockerfile_dev" -t imboy/imboy-api-linux-arm64:0.1.5 .

docker push imboy/imboy-api-linux-arm64:0.1.5 .


mkdir -p /www /www/wwwroot && git clone https://gitee.com/imboy-pub/imboy.git imboy-api && cd imboy-api
git fetch origin dev && git checkout dev
make deps

```

### PG15 / PG16
from  https://github.com/postgis/docker-postgis

dev
```
docker build --file "./docker/pg16_Dockerfile_dev" -t imboy/pg16:3.4.2.dev.1 .


docker build --file "./docker/pg15_Dockerfile_dev" -t imboy/imboy-pg:15.3.4.2.dev.7 .
```

pro
```
删除所有未使用的卷
docker volume prune

```

## postgresql
https://github.com/postgis/postgis Star 1.4K
基于 https://github.com/postgis/docker-postgis

https://github.com/postgis/docker-postgis/blob/master/15-3.4/Dockerfile

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
    -p 8080:8080 \
    -e GO_FASTDFS_DIR=/data \
    sjqzhang/go-fastdfs

// 统计api
http://127.0.0.1:8080/stat

imboy_uri:check_auth("http://c.imboy.pub/collect/image/chk7efp0poqbagho741g.HEIC?").

文件列表 dir : 要查看文件列表的目录名
http://127.0.0.1:8080/list_dir
http://127.0.0.1:8080/list_dir?dir=default
curl http://127.0.0.1:8080/list_dir?dir=location/20235
curl 'http://127.0.0.1:8080/delete?path=/collect/107/temp.jpg&s=dev&a=xxx&v=xxx'

curl 'http://127.0.0.1:8080/delete?md5=3176156f892b3509f3b39751ee50b58b6f16a3a5&s=dev&a=xxx&v=xxx'
curl http://127.0.0.1:8080/repair_fileinfo?s=dev&a=xxx&v=xxx&width=375


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

### jenkins
docker rm -f imboy_jenkins && \
docker run -d \
    --name imboy_jenkins \
    --user root \
    --network imboy-network \
    -p 7080:8080 \
    -p 50000:50000 \
    -v /data/docker/jenkins_home:/var/jenkins_home \
    -v /var/run/docker.sock:/var/run/docker.sock \
    -v /usr/bin/docker:/usr/bin/docker \
    -v /etc/docker:/etc/docker \
    jenkins/jenkins:latest-jdk17

将 /data/docker/jenkins_home 的所有权更改为UID 1000：

    chown -R 1000:1000 /data/docker/jenkins_home && chmod -R 777 /data/docker/jenkins_home
##

https://blog.wu-boy.com/2018/03/nginx-reverse-proxy-image-resizing/
