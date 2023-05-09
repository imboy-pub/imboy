

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

pg_dump -h 127.0.0.1 -d imboy_v1 -U imboy_user -p 5432  -f imboy_v1.sql

导入到本地数据库
psql -h 127.0.0.1 -d imboy_v1 -U imboy_user -p 5432 -f imboy_v1.sql

psql -h 127.0.0.1 -d imboy_v1 -U imboy_user -p 5432 -f /var/lib/postgresql/data/user_data.sql

```

https://www.runoob.com/postgresql/postgresql-index.html


# FAQ

PostgreSQL 如何存储 emoji 表情 https://www.oschina.net/question/2286362_2190411
