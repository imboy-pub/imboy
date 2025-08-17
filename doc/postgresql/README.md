

# PostgreSQL规约（PG16）


# 使用 pure-migrations 管理数据库版本


https://github.com/bearmug/erlang-pure-migrations


* 数据库版本脚本文件使用8位数做前缀 00000000_xxx.sql
* 为了数据库安全，添加 super_account 数据库账户，和api的账号分开；
```
imboy_db:migrate().

import_chinese_poetry:start().


// 下面的命令以后添加 TODO
./imboy migrate:run


```


```
cat ./doc/postgresql/migrations/*.sql > merged.sql

find /Users/leeyi/project/imboy.pub/chinese-poetry -type f -exec cat {} + > output.txt

```

* 精准备份 public schema 的命令：
```
pg_dump -h 127.0.0.1 \
  --inserts \                  # 生成 INSERT 语句（兼容性更好）
  -d imboy_v1 \                # 数据库名
  -U imboy_user \              # 用户名
  -p 5432 \                    # 端口
  -n 'public' \                # 限定备份 public schema
  --no-acl \                   # 跳过权限信息（可选）
  -f imboy_v1.sql              # 输出文件

pg_dump -h 127.0.0.1 --inserts -d imboy_v1 -U imboy_user -p 5432 -n 'public'  -f imboy_v1.sql

```

* DBeaver 可以对比数据结构

```
#psql -d template1
#template1=# \l

// 创建用户
CREATE USER imboy_user WITH PASSWORD 'abc12345';

CREATE DATABASE imboy_v1 WITH OWNER imboy_user ENCODING 'UTF8';

// 将所有权限赋给 imboy_user
GRANT ALL PRIVILEGES ON DATABASE imboy_v1 to imboy_user;

// 删除默认生成的postgresql
DROP DATABASE postgres;
DROP USER postgres;
DROP schema "config"

\dx postgis*


https://blog.51cto.com/suncj/5038850

例：从ip为xxx的数据库mon导出所有的表结构到文件dump2022.sql:
pg_dump -h 127.0.0.1 mon -U postgres -p 5432  -f dump2022.sql


pg_dump 只导出 public 模式数据，不导出数据结构的SQL


pg_dump -h 127.0.0.1 -U imboy_user -p 5432 -d imboy_v1 --data-only --column-inserts --schema=public --exclude-table=public.fts_user --exclude-table=public.database_migrations_history --exclude-table=public.spatial_ref_sys > imboy_v1_data.sql

psql -h 127.0.0.1 -p 5432 -U imboy_user -d imboy_v1 -f imboy_v1_data.sql


如果你只想导出数据库的结构而不包含数据，可以使用以下命令：
pg_dump -h 127.0.0.1 -p 5432 -U imboy_user -d imboy_v1 -s -f imboy_v1_dev.sql

psql -h 127.0.0.1 -p 5432 -U imboy_user -d imboy_v1 -f imboy_v1_dev.sql


mv imboy_v1.sql /var/lib/postgresql/data/

/data/docker/imboy_pg15

导入到本地数据库
psql -h 127.0.0.1 -d imboy_v1 -U imboy_user -p 5432 -f imboy_v1.sql

psql -h 127.0.0.1 -d imboy_v1 -U imboy_user -p 4322 -f ./docker/imboy_v1.sql

psql -h 127.0.0.1 -d imboy_v1 -U imboy_user -p 9820 -f docker/imboy_v1.sql

DROP SCHEMA public CASCADE;
CREATE SCHEMA public;

GRANT ALL ON SCHEMA public TO imboy_user;
GRANT ALL ON SCHEMA public TO public;

select * from to_tsquery('jiebacfg', '是拖拉机学院手扶拖拉机专业的。不用多久，我就会升职加薪，当上CEO，走上人生巅峰。');


DROP SCHEMA public CASCADE;
CREATE SCHEMA public;

GRANT ALL ON SCHEMA public TO imboy_user;
GRANT ALL ON SCHEMA public TO public;

psql -h 127.0.0.1 -d imboy_v1 -U imboy_user -p 5432 -f /var/lib/postgresql/data/user_data.sql

```

https://www.runoob.com/postgresql/postgresql-index.html

# postgresql 扩展


* how to set multiple shared_preload_libraries

https://www.postgresql.org/docs/current/runtime-config-client.html
```
# Add settings for extensions here
# 它包含以逗号分隔的库名称列表； 条目之间的空格被忽略；如果需要在名称中包含空格或逗号，请用双引号将库名称括起来。
shared_preload_libraries='pg_stat_statements,timescaledb'
pg_stat_statements.max=10000
pg_stat_statements.track=all
```
* PostgreSQL实用技巧 https://tonydong.blog.csdn.net/article/details/128259591

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

## pgcrypto

* https://tonydong.blog.csdn.net/article/details/109073000 PostgreSQL 数据加密之 pgcrypto



## pg_stat_statements

查找占用资源最多的查询和进程
```
# CREATE EXTENSION IF NOT EXISTS pg_stat_statements;

SELECT
    total_exec_time,
    mean_exec_time as avg_ms,
    calls,
    query
FROM pg_stat_statements
ORDER BY mean_exec_time DESC
LIMIT 10;

```

## Posttgresql 日志功能

https://tonydong.blog.csdn.net/article/details/128259591

开启慢查询记录功能，执行时间等于或者大于 log_min_duration_statement 设置值的语句会被记录：
```
ALTER database imboy_v1 SET log_min_duration_statement = '250ms';
```

使用 log_statement 选项设置日志记录的语句类型：
```
ALTER DATABASE imboy_v1 SET log_statement = 'all';
```
有效的取值包括 all、ddl、none 以及 mod。

当数据库出现锁等待事件时记录日志:
```
ALTER DATABASE imboy_v1 SET log_lock_waits = 'on';

```

## 清空 标签系统相关数据
```

truncate public.user_tag_relation;
truncate public.user_tag;

update public.user_friend set tag = '' where 1=1;
update public.user_collect set tag = '' where 1=1;


select * from public.user_tag
select * from public.user_tag_relation
select id,tag from public.user_friend where (from_user_id = 62913 and to_user_id = 513242) or (to_user_id = 62913 and from_user_id = 513242)
```

# FAQ

PostgreSQL 如何存储 emoji 表情 https://www.oschina.net/question/2286362_2190411




ALTER TABLE public.adm_user ADD COLUMN created_at_ts timestamp with time zone DEFAULT CURRENT_TIMESTAMP;

UPDATE public.adm_user SET created_at_ts = to_timestamp(created_at/1000.0);

select created_at_ts from public.adm_user
