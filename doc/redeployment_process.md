
重新部署流程

1. 安装erlang

【有道云笔记】CentOS 源码安装 Erlang.md
https://note.youdao.com/s/7hvcSteY


2. 安装docker
```
mkdir -p /data /data/imboy
```

3. 安装postgresql15

```
cd /data/imboy/imboyapi
docker build --file "./docker/pg15_Dockerfile_dev" -t imboy/imboy-pg:15.3.4.2.dev .

docker rm -f imboy_pgsql && docker run -d --name imboy_pgsql --network imboy-network -e POSTGRES_USER=imboy_user -e POSTGRES_PASSWORD=abc54321 -e POSTGRES_DB=imboy_v1 -e PGDATA=/var/lib/postgresql/data/pgdata -v /data/imboy/imboy_pgsql:/var/lib/postgresql/data -p 127.0.0.1:4321:5432 imboy/imboy-pg:15.3.4.2.dev

docker rm -f imboy_pgsql && docker run -d --name imboy_pgsql --network imboy-network -e POSTGRES_USER=imboy_user -e POSTGRES_PASSWORD=abc54321 -e POSTGRES_DB=imboy_v1 -e PGDATA=/var/lib/postgresql/data/pgdata -v /data/imboy/imboy_pgsql:/var/lib/postgresql/data -p 127.0.0.1:4321:5432 imboy-pg:15.3.4.1.dev
```

4. sjqzhang/go-fastdfs

```
cd /data/imboy/

docker rm -f imboy_fastdfs && docker run --network=imboy-network -d --name imboy_fastdfs  -v /data/imboy/img_fastdfs_data:/data -p 8080:8080 -e GO_FASTDFS_DIR=/data sjqzhang/go-fastdfs
```

修改配置如下：
/data/imboy/img_fastdfs_data/conf/cfg.json
```
    "support_group_manage": false,
    "download_domain": "124.222.102.13:8080",
    "scenes": ["dev", "prod"],
    "show_dir": false,
    "file_sum_arithmetic": "sha1",
    "admin_ips": ["127.0.0.1"],
    "auth_url": "http://124.222.102.13:9800/auth/assets",
    "enable_download_auth": true,
    "default_download": false,
```

6. ghcr.io/processone/eturnal:latest

https://eturnal.net/doc/container.html
```
docker rm -f imboy_eturnal &&  docker run -d --rm \
    --name imboy_eturnal \
    --network=imboy-network \
    --read-only \
    --cap-drop=ALL \
    --security-opt no-new-privileges \
    -p 3478:3478 \
    -p 3478:3478/udp \
    -p 50000-50500:50000-50500/udp \
    -e ETURNAL_RELAY_MIN_PORT=49152 \
    -e ETURNAL_RELAY_MAX_PORT=65535 \
    -v /data/imboy/eturnal.yml:/etc/eturnal.yml:ro \
  ghcr.io/processone/eturnal:latest

docker exec imboy_eturnal eturnalctl info
docker exec imboy_eturnal eturnalctl credentials
```

hrvUm#>vK9/c@'a`>reT4

测试 eturnal 安装是否成功
```
nc -zv 124.222.102.13 3478
Connection to 124.222.102.13 port 3478 [tcp/nat-stun-port] succeeded!


telnet 124.222.102.13 3478
Trying 124.222.102.13...
Connected to 124.222.102.13.
Escape character is '^]'.
```


STUN_URL=stun:124.222.102.13:3478
TURN_URL=turn:124.222.102.13:3478?transport=udp
