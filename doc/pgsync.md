
https://github.com/ankane/pgsync star 3K Ruby

```
ssh -p 32 -L 4321:127.0.0.1:4321 root@81.68.209.56 -N -f

pgsync --init
pgsync --schemas public --schema-first
```

##  生成 SQL 差异文件
```
```

# install
```
brew install pgsync
```

## 错误1
```
From: imboy_v1 on localhost:4321
To: imboy_v1 on localhost:9820
pg_restore not found
2024-05-28 20:50:50
```
这个错误信息表明 pgsync 在执行过程中找不到 pg_restore 工具。 pg_restore 是 PostgreSQL 的一部分，它用于恢复数据库备份。确保 pg_restore 工具已安装并可被系统路径访问。

```
        sudo apt-get update
        sudo apt-get install postgresql-client

        sudo yum install postgresql

        brew install postgresql
```

## 错误2

```
pgsync --schemas public --schema-first
connection to server at "localhost" (127.0.0.1), port 4321 failed: timeout expired
```

这个错误表明 pgsync 在试图连接到运行在 localhost 端口 4321 的 PostgreSQL 服务器时超时。这个问题可能与多种因素有关，包括网络配置、服务未运行或者端口阻塞等。以下是一些解决这个问题的步骤：
```
# 检查现有的 SSH 隧道
ps aux | grep ssh

# 如果隧道没有正确建立，可以再次运行以下命令
ssh -p 99 -L 5433:localhost:5432 yourusername@remote.server.com -N -f
````

## 错误3

```
pgsync --schemas public --schema-first
From: imboy_v1 on 127.0.0.1:4321
To: imboy_v1 on 127.0.0.1:9820
✖ Syncing schema
pg_dump: error: server version: 15.6 (Debian 15.6-1.pgdg110+2); pg_dump version: 14.12 (Homebrew)
pg_dump: error: aborting because of server version mismatch
pg_restore: error: input file is too short (read 0, expected 5)
```

这个错误是由于 pg_dump 和 PostgreSQL 服务器的版本不匹配导致的。即 pg_dump 版本是 14.12，而服务器版本是 15.6。这种不匹配导致 pg_dump 不能正确执行，从而导致同步失败。

```
brew install postgresql@15
    postgresql-15.7
```
