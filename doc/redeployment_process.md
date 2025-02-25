
# 重新部署流程

共有1-10个注意事项：

pro.imboy.pub
i.imboy.puub
turn.imboy.pub

* 使用 docker容器启动 eturnal 服务，会非常占用内存，直接源码安装或者用依赖包在宿主机安装很省内存；
* 按下面流程配置好各个依赖后，通过 docker-compose-pro.yml 管理 postgresql17 服务

```
cd imboy
docker-compose -f docker-compose-pro.yml up -d
```


1. 安装erlang

./doc/erlang_install.md

【有道云笔记】CentOS 源码安装 Erlang.md
https://note.youdao.com/s/7hvcSteY


2. 安装 docker
```
mkdir -p /data /data/imboy
docker network create imboy-network
```

3. 安装 postgresql17

```
cd /www/wwwroot/imboy-api
docker pull postgres:15-bullseye
docker build --file "./docker/pg17_Dockerfile_dev" -t imboy/pg17:3.5.1.dev.1 .

// 解决升级 timescaledb 后加载报错的问题
psql -U imboy_user -d imboy_v1
DROP EXTENSION IF EXISTS timescaledb CASCADE;
CREATE EXTENSION IF NOT EXISTS timescaledb CASCADE;
```

注意设置pgsql 内网访问

设置只允许内网访问，docker网关地址为 172.19.0.1
```
# - Connection Settings -

listen_addresses = 'localhost, 172.19.0.0/16'
                    # comma-separated list of addresses;
                    # defaults to 'localhost'; use '*' for all
                    # (change requires restart)
#port = 5432                # (change requires restart)
```

4. sjqzhang/go-fastdfs

```
cd /data/imboy/

docker rm -f imboy_fastdfs && docker run -d \
    --network=imboy-network \
    --name imboy_fastdfs  \
    --memory 800M \
    --memory-swap 2g \
    -v /data/docker/img_fastdfs_data:/data \
    -p 8080:8080 \
    -e GO_FASTDFS_DIR=/data \
    sjqzhang/go-fastdfs
```

修改配置如下：
/data/docker/img_fastdfs_data/conf/cfg.json
```
    "support_group_manage": false,
    "download_domain": "124.222.102.13:8080",
    "scenes": ["dev", "pro"],
    "show_dir": false,
    "file_sum_arithmetic": "sha1",
    "admin_ips": ["127.0.0.1"],
    "auth_url": "https://dev.imboy.pub/auth/assets",
    "enable_download_auth": true,
    "default_download": false,
```

6. ghcr.io/processone/eturnal:latest

https://eturnal.net/doc/container.html
```
ghcr.io/processone/eturnal:latest
mkdir -p /data/docker/eturnal && cd /data/docker/eturnal
wget https://raw.githubusercontent.com/processone/eturnal/master/config/eturnal.yml

docker rm -f imboy_eturnal &&  docker run -d --rm \
    --name imboy_eturnal \
    --network=imboy-network \
    --read-only \
    --cap-drop=ALL \
    --security-opt no-new-privileges \
    --memory 800M \
    --memory-swap 2g \
    -p 3478:3478 \
    -p 3478:3478/udp \
    -p 50000-50500:50000-50500/udp \
    -e ETURNAL_RELAY_MIN_PORT=50000 \
    -e ETURNAL_RELAY_MAX_PORT=50500 \
    -v /data/docker/eturnal/eturnal.yml:/etc/eturnal.yml:ro \
    docker.io/eturnal/eturnal:latest

docker exec imboy_eturnal eturnalctl info
docker exec imboy_eturnal eturnalctl credentials

```


测试 eturnal 安装是否成功
```
brew install stuntman
ll /usr/local/Cellar/stuntman/1.2.16/bin/stunclient

stunclient dev.imboy.pub 3478
stunclient -u 1710902602 -p jx9u3FeQ6YGcISGwOq7lkyyuGpU= dev.imboy.pub 3478

nc -zv 124.222.102.13 3478
Connection to 124.222.102.13 port 3478 [tcp/nat-stun-port] succeeded!


telnet 124.222.102.13 3478
Trying 124.222.102.13...
Connected to 124.222.102.13.
Escape character is '^]'.
```


STUN_URL=stun:124.222.102.13:3478
TURN_URL=turn:124.222.102.13:3478?transport=udp

7. 数据库初始化配置

./project/imboy.pub/doc/keystore/imboy_init_config_dev.md
./project/imboy.pub/doc/keystore/imboy_init_config_pro.md

```

config_ds:set(<<"api_auth_switch">>, on). % on | off

config_ds:set(<<"hashids_salt">>, <<>>).
config_ds:set(<<"jwt_key">>, <<>>).
config_ds:set(<<"password_salt">>, <<>>).


config_ds:set(<<"login_pwd_rsa_encrypt">>, 1, <<"登录密码使用RSA算法加密"/utf8>>, <<"系统登录是否开启RSA加密 1 是； 0 否"/utf8>>).
config_ds:set(<<"site_name">>, <<"IMBoy"/utf8>>, <<"前端站点名称"/utf8>>, <<>>).

config_ds:set(<<"login_rsa_pub_key">>, <<"">>, <<"登录RSA算法加密公钥"/utf8>>, <<"pem文件内容，换行用\n"/utf8>>).

config_ds:set(<<"login_rsa_priv_key">>, <<"">>, <<"登录RSA算法加密私钥"/utf8>>, <<"pem文件内容，换行用\n"/utf8>>).



config_ds:set(<<"upload_url">>, <<"https://a.imboy.pub">>).
config_ds:set(<<"upload_key">>, <<"">>).
config_ds:set(<<"upload_scene">>, <<"dev">>).
config_ds:set(<<"ws_url">>, <<"wss://dev.imboy.pub/ws/">>).
config_ds:set(<<"ws_url">>, <<"ws://http://192.168.2.226:9800/ws/">>).

config_ds:set(<<"eturnal_secret">>, "").
config_ds:set(<<"turn_urls">>, [<<"turn:dev.imboy.pub:34780?transport=udp">>]).
config_ds:set(<<"stun_urls">>, [<<"stun:dev.imboy.pub:34780">>]).

config_ds:set(<<"solidified_key">>, <<"">>, <<"接口默认签名秘钥"/utf8>>, <<"如果没有自动获取到sign_key 就用这个值来签名"/utf8>>).
config_ds:set(<<"solidified_key_iv">>, <<"">>, <<"秘钥IV"/utf8>>, <<"IV 必须都为128比特，也就是16字节"/utf8>>).

config_ds:get(<<"upload_url">>).
config_ds:get(<<"ws_url">>).
config_ds:get(<<"turn_urls">>).

```


8. nginx 配置

./nginx_dev.imboy.pub.conf
```

cp -Rf /www/wwwroot/imboy-api/apps/imadm/priv/static /www/wwwroot/dev.imboy.pub/

cp -Rf /www/wwwroot/imboy-api/apps/imadm/priv/static /www/wwwroot/pro.imboy.pub/

    # 解决静态资源404问题
    location ~ .*\.(js|css|ttf|woff|woff2)?$
    {
        expires      12h;
        error_log /dev/null;
        access_log /dev/null;
    }
```

9. adm
Captcha

https://www.cnblogs.com/ziyouchutuwenwu/p/4424499.html
sudo apt-get install imagemagick

10. 定时备份数据


在宿主机上面：

```

cp -rf /www/wwwroot/imboy-api/doc/postgresql/cron_backup_pgsql.sh /data/docker/imboy_pg15 && chmod +x /data/docker/imboy_pg15

[root@imboy ~]# crontab -e
0 3 * * * /usr/bin/docker exec imboy_pg15 /bin/sh /var/lib/postgresql/data/cron_backup_pgsql.sh
```
