# Redis Stack
* RediSearch：一个功能齐全的搜索引擎；
* RedisJSON：对JSON类型的原生支持；
* RedisTimeSeries：时序数据库支持；

https://www.cnblogs.com/virtulreal/p/16676256.html

## RediSearch
```
docker rm -f RediSearch  && docker run -d -p 63798:6379 --name=RediSearch -e REDIS_ARGS="--requirepass imboy123456" redis/redis-stack-server:latest

f(C).
f(Options).
Options = [{password, "imboy123456"}, {database, 0}, {host, "127.0.0.1"}, {port, 63798}].
{ok, C} = eredis:start_link(Options).

HashObj = ["id", "objectId", "message", "message", "receiver", "receiver", "status", "read"].
{ok, <<"OK">>} = eredis:q(C, ["HMSET", "key" | HashObj]).
{ok, Values} = eredis:q(C, ["HGETALL", "key"]).

```

https://redis.io/docs/stack/search/quick_start/
```
// Create an index
127.0.0.1:6379> FT.CREATE myIdx ON HASH PREFIX 1 doc: SCHEMA title TEXT WEIGHT 5.0 body TEXT url TEXT
eredis:q(C, ["FT.CREATE", "myIdx", "ON", "HASH", "PREFIX", "1", "doc:", "SCHEMA", "title", "TEXT", "WEIGHT", "5.0", "body", "TEXT", "url", "TEXT"]).


// add doc
127.0.0.1:6379> HSET doc:1 title "hello world" body "lorem ipsum" url "http://redis.io"
eredis:q(C, ["HSET", "doc:1", "title", "hello world", "body", "lorem ipsum", "url", "http://redis.io"]).
eredis:q(C, ["HSET", "doc:2", "title", "hello imboy", "body", "imboy", "url", "http://www.imboy.pub"]).

// Search the index
127.0.0.1:6379> FT.SEARCH myIdx "hello world" LIMIT 0 10
eredis:q(C, ["FT.SEARCH", "myIdx", "hello world", "LIMIT", 0, 10]).

```
