

```
#psql -d template1
#template1=# \l

// 创建用户
CREATE USER imboy_user WITH PASSWORD 'abc12345';

CREATE DATABASE imboy_v1 WITH OWNER imboy_user ENCODING 'UTF8';

// 将所有权限赋给postgres
GRANT ALL PRIVILEGES ON DATABASE imboy_v1 to imboy_user;

// 删除默认生成的postgresql
DROP DATABASE postgres;
DROP USER postgres;
DROP schema "config"

\dx postgis*


https://blog.51cto.com/suncj/5038850

例：从ip为xxx的数据库mon导出所有的表结构到文件dump2022.sql:
pg_dump -h 127.0.0.1 mon -U postgres -p 5432  -f dump2022.sql

pg_dump -h 127.0.0.1 --inserts -d imboy_v1 -U imboy_user -p 5432  -f imboy_v1.sql

mv imboy_v1.sql /var/lib/postgresql/data/

/data/docker/imboy_pg15

导入到本地数据库
psql -h 127.0.0.1 -d imboy_v1 -U imboy_user -p 5432 -f imboy_v1.sql

DROP SCHEMA public CASCADE;
CREATE SCHEMA public;

GRANT ALL ON SCHEMA public TO imboy_user;
GRANT ALL ON SCHEMA public TO public;

psql -h 127.0.0.1 -d imboy_v1 -U imboy_user -p 5432 -f /var/lib/postgresql/data/user_data.sql

```

https://www.runoob.com/postgresql/postgresql-index.html

# postgresql 扩展

```
select name from pg_available_extensions;
```

## postgis


## pgroonga

## pg_jieba
在macos m1 上面做的 pg_jieba.so 等8个文件copy到CentOS8 里面的docker里面，没有使用；

之后我在CentOS8上面弄了一个 imboy:pg15_dev ，从里面copy出 pg_jieba扩展，在 docker cp 到 imboy_pg15 才可用

详细步骤参考 ./docker/README.md

## timescaledb

https://packagecloud.io/timescale/timescaledb/install
https://docs.timescale.com/self-hosted/latest/install/installation-linux/


```
curl -s https://packagecloud.io/install/repositories/timescale/timescaledb/script.deb.sh | sudo bash
apt install timescaledb-2-postgresql-15

CREATE EXTENSION IF NOT EXISTS timescaledb;

```
上面的代码在 imboy_postgis_dev docker容器里面执行成功

# FAQ

PostgreSQL 如何存储 emoji 表情 https://www.oschina.net/question/2286362_2190411
