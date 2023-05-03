

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
```

https://www.runoob.com/postgresql/postgresql-index.html


# FAQ

PostgreSQL 如何存储 emoji 表情 https://www.oschina.net/question/2286362_2190411
