# [Redis Stack](https://redis.com/blog/introducing-redis-stack/)

Redis Stack ， 它将多个领先的 Redis 模块集成为单个产品， 使得用户可以更轻松地利用基于 Redis 构建的搜索、文档存储、图数据和时间序列能力！

Redis Stack 是一个由三个组件组成的套件：

* Redis Stack 服务器将开源的 Redis 和 RedisSearch 、 RedisJSON 、 RedisGraph 、 RedisTimeSeries 和 RedisBloom 结合起来。
* RedisInsight 是一个强大的工具，用于可视化和优化 Redis 数据，使实时应用开发比以前更容易、更有趣。
* Redis Stack 客户端 SDK 包含领先的 Java 、 JavaScript 和 Python 官方客户端以及我们新推出的对象映射库套件 Redis OM（支持 .Net 、 Node.js 、Java 和 Python），后者提供了对开发者友好的抽象，使用户只需要几行代码就能够提高工作效率，并且能够更容易地集成 Spring 、 ASP.NET Core 、 FastAPI 和 Express 等主要应用框架。

* RediSearch：一个功能齐全的搜索引擎；
* RedisJSON：对JSON类型的原生支持；
* RedisTimeSeries：时序数据库支持；

https://www.cnblogs.com/virtulreal/p/16676256.html

https://hub.docker.com/r/redis/redis-stack-server
```
docker rm -f imboy.pub-redisstack  && docker run -d -p 63798:6379 --name=imboy.pub-redisstack -e REDIS_ARGS="--requirepass imboy123456" redis/redis-stack-server:latest

f(C).
f(Options).
Options = [{password, "imboy123456"}, {database, 0}, {host, "127.0.0.1"}, {port, 63798}].
{ok, C} = eredis:start_link(Options).

```

## RediSearch

https://redis.io/docs/stack/search/quick_start/
```
f(C), f(Options), Options = [{password, "imboy123456"}, {database, 0}, {host, "127.0.0.1"}, {port, 63798}], {ok, C} = eredis:start_link(Options).

// Create an index
127.0.0.1:6379> FT.CREATE myIdx ON HASH PREFIX 1 doc: SCHEMA title TEXT WEIGHT 5.0 body TEXT url TEXT
eredis:q(C, ["FT.CREATE", "myIdx", "ON", "HASH", "PREFIX", "1", "doc:", "SCHEMA", "title", "TEXT", "WEIGHT", "5.0", "body", "TEXT", "url", "TEXT"]).


// add doc
127.0.0.1:6379> HSET doc:1 title "hello world" body "lorem ipsum" url "http://redis.io"
eredis:q(C, ["HSET", "doc:1", "title", "hello world", "body", "lorem ipsum", "url", "http://redis.io"]).
eredis:q(C, ["HSET", "doc:2", "title", "hello imboy", "body", "imboy", "url", "http://www.imboy.pub"]).

eredis:q(C, ["HSET", "doc:3", "title", "hello abc", "body", "dddd eeee", "url", "http://www.imboy.pub"]).
eredis:q(C, ["HSET", "doc:4", "language", "chinese", "title", "hello 2 abc", "body", "dddd eeee", "url", "http://www.imboy.pub"]).


// Search the index
127.0.0.1:6379> FT.SEARCH myIdx "hello world" LIMIT 0 10
eredis:q(C, ["FT.SEARCH", "myIdx", "hello world", "LIMIT", 0, 10]).


127.0.0.1:6379> FT.DROPINDEX myIdx

FT.CREATE idxCn SCHEMA txt TEXT
FT.ADD idxCn docCn 1.0 LANGUAGE chinese FIELDS txt "Redis支持主从同步。数据可以从主服务器向任意数量的从服务器上同步，从服务器可以是关联其他从服务器的主服务器。这使得Redis可执行单层树复制。从盘可以有意无意的对数据进行写操作。由于完全实现了发布/订阅机制，使得从数据库在任何地方同步树时，可订阅一个频道并接收主服务器完整的消息发布记录。同步对读取操作的可扩展性和数据冗余很有帮助。[8]"
FT.SEARCH idxCn "数据" LANGUAGE chinese HIGHLIGHT SUMMARIZE

f(C), f(Options), Options = [{password, "imboy123456"}, {database, 0}, {host, "127.0.0.1"}, {port, 63798}], {ok, C} = eredis:start_link(Options).
eredis:q(C, ["FT.SEARCH", "idxCn", unicode:characters_to_binary("数据"), "LANGUAGE", "chinese", "HIGHLIGHT", "SUMMARIZE"]).

```

## RedisJSON
https://redis.io/docs/stack/json/

Add JSON documents
```

127.0.0.1:6379> FT.DROPINDEX itemIdx

127.0.0.1:6379> FT.CREATE itemIdx ON JSON PREFIX 1 item: SCHEMA $.name AS name TEXT $.description as description TEXT $.price AS price NUMERIC $.embedding AS embedding VECTOR FLAT 6 DIM 4 DISTANCE_METRIC L2 TYPE FLOAT32

127.0.0.1:6379> JSON.SET item:1 $ '{"name":"Noise-cancelling Bluetooth headphones","description":"Wireless Bluetooth headphones with noise-cancelling technology","connection":{"wireless":true,"type":"Bluetooth"},"price":99.98,"stock":25,"colors":["black","silver"],"embedding":[0.87,-0.15,0.55,0.03]}'
"OK"
127.0.0.1:6379> JSON.SET item:2 $ '{"name":"Wireless earbuds","description":"Wireless Bluetooth in-ear headphones","connection":{"wireless":true,"type":"Bluetooth"},"price":64.99,"stock":17,"colors":["black","white"],"embedding":[-0.7,-0.51,0.88,0.14]}'
"OK"

127.0.0.1:6379> FT.SEARCH itemIdx '@name:(earbuds)'

f(C), f(Options), Options = [{password, "imboy123456"}, {database, 0}, {host, "127.0.0.1"}, {port, 63798}], {ok, C} = eredis:start_link(Options).

eredis:q(C, ["FT.CREATE", "itemIdx", "ON", "JSON", "PREFIX", "1", "item:", "SCHEMA", "$.name", "AS", "name", "TEXT", "$.description", "as", "description", "TEXT", "$.price", "AS", "price", "NUMERIC", "$.embedding", "AS", "embedding", "VECTOR", "FLAT", "6", "DIM", "4", "DISTANCE_METRIC", "L2", "TYPE", "FLOAT32"]).


f(C), f(Options), Options = [{password, "imboy123456"}, {database, 0}, {host, "127.0.0.1"}, {port, 63798}], {ok, C} = eredis:start_link(Options).

f(Arc).
Arc = [
   {<<"name">>, unicode:characters_to_binary("中文名长春市")},
   {<<"description">>, unicode:characters_to_binary("滴答滴答开讲啦科技阿凡达科技啦饭卡垃圾啊代发")},
   {<<"connection">>, [
    {<<"wireless">>, true},
    {<<"type">>, <<"Bluetooth">>}
   ]},
   {<<"price">>, 1234.12},
   {<<"stock">>, 123},
   {<<"embedding">>, <<"[1,3,3,4,5,6]">>},
   {<<"colors">>, [<<"blue">>, <<"red">>]}
].
eredis:q(C, ["JSON.SET", "item:3", "$", jsx:encode(Arc)]).


eredis:q(C, ["JSON.GET", "item:3"]).
eredis:q(C, ["JSON.GET", "item:3", "name"]).

eredis:q(C, ["FT.SEARCH", "itemIdx", "'@name:(earbuds)'"]).

RedisJson-中文全文检索 https://segmentfault.com/a/1190000041208585

redis-17137.c245.us-east-1-3.ec2.cloud.redislabs.com:17137>   FT.CREATE i_index1 ON JSON LANGUAGE chinese SCHEMA $.title TEXT

f(C), f(Options), Options = [{password, "imboy123456"}, {database, 0}, {host, "127.0.0.1"}, {port, 63798}], {ok, C} = eredis:start_link(Options).
eredis:q(C, ["FT.dropindex", "itemIdx_1"]),
eredis:q(C, ["FT.CREATE", "itemIdx_1",  "ON", "JSON", "LANGUAGE", "chinese", "SCHEMA", "$.name", "TEXT", "$.description", "text"]).

f(C), f(Options), Options = [{password, "imboy123456"}, {database, 0}, {host, "127.0.0.1"}, {port, 63798}], {ok, C} = eredis:start_link(Options).
eredis:q(C, ["FT.SEARCH", "itemIdx_1",  unicode:characters_to_binary("中文名长春市"), "LANGUAGE", "chinese"]).
eredis:q(C, ["FT.SEARCH", "itemIdx_1",  unicode:characters_to_binary("滴答"), "LANGUAGE", "chinese"]).


```

## RedisTimeSeries
https://redis.io/docs/stack/timeseries/
