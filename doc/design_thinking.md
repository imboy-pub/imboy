
记录一些功能的设计思考权衡过程

## 客户端服务端通讯安全(以实现)
* API 使用 HTTPS；
* 基于JWT的权限验证；
* 基于客户端版本号的签名密钥管理，有利于密钥更新；
* 配置在 option 和 open 里面的路由不需要做JWT认证，其他的都需要做JWT认证；
* 小部分open路由不需要做签名验证，其他所有路由都需要签名验证；
* 用户获取公钥数据的路由 /init 响应的数据通过基于版本号管的密钥的AES算法加密
* 用户注册登录密码通过公钥做 RSA 算法加密传输，通过 hmac_sha512 算法加盐存储；
* 发送到客户端的用户ID都要经过 hashids 算法混淆；

## 数据库存储(以实现)
选择强大的关系型数据库 PostgreSQL 15 ;

使用了它的一些扩展

* fuzzystrmatch postgresql 内置的，提供多个函数来判断字符串之间的相似性和距离。
* pg_trgm postgresql 内置的，三元索引增强全文检索
* plpgsql postgresql 内置的，可载入的过程语言。
* pg_jieba 用户在线搜索
* postgis 附件的人功能，postgis的基本核心功能，仅支持地理图形（矢量要素）
* pgrouting PgRouting是基于开源空间数据库PostGIS用于网络分析的扩展模块，最初它被称作pgDijkstra，因为它只是利用Dijkstra算法实现最短路径搜索，之后慢慢添加了其他的路径分析算法，如A算法，双向A算法，Dijkstra算法，双向Dijkstra算法，tsp货郎担算法等，然后被更名为pgRouting
* postgis_tiger_geocoder
* postgis_topology 拓扑功能的支持
* pgcrypto 单聊、群聊等离线消息加密存储； 用户收藏资料加密存储
* pg_stat_statements 运维相关的
* timescaledb 暂无用例 （计划用于群聊消息存储）
* pgroonga 暂无用例
* pgcrypto
* vector Postgres 的开源向量相似度搜索
* roaringbitmap


## GEO 方案 (以实现)
通过对比选择使用强大的 postgis

## 全文索引 方案 (以实现)
通过对比选择使用强大的 pg_jieba + pgroonga


## 缓存方案 (以实现)
刻意规避使用Redis，使用 erlang 的 depcache

## 数据库连接池方案 (以实现)
使用 epgsql + pooler

## 附件资源存储方案
选择了 Go-FastDFS ，基于golang实现的自建服务，功能基本够用


## 消息确认机制 QoS
https://blog.csdn.net/Jessechanrui/article/details/88399012

socket 数据粘包问题、拆包问题

## 消息投递机制 (以实现)

1. 判断用户是否在线，如果用户离线，直接存储离线消息
2. 用户在线（or 用户上线），判断 erlang is_process_alive(Pid) 马上投递一次
3. 没有收到消息之前， 2 S -> 5 S -> 7S -> 11 S 投递4次
4. 如果收到消息 清理定时器，清理数据库消息
5. 4次投递都未确认消息，待用户下次登录再投递

以上消息确认重复机制，可以确保消息不丢失

## WebSocket 链接token校验机制 (以实现)

为提升用户体验，在“WebSocket 链接”的时候，即使token过期也响应成功；

为安全，补救措施如下：

* 在校验token过期有，给客户端发送S2C消息，要求客户端在8秒内（[0,3000,5000]）刷新本token，告知服务端结果
* 在确认刷新token成功后，取消计时器；
* 否则直接给特定链接踢下线

下面是服务端调试代码
```
Uid = 513242,
DID = <<"C5931370-BDCC-55FE-AB9C-8E2B39DC5018">>,
MsgId = <<"please_refresh_token">>,
ToUid = imboy_hashids:uid_encode(Uid),
Msg = message_ds:assemble_msg(
    <<"S2C">>, <<>>, ToUid
    , [{<<"msg_type">>, MsgId}]
    , MsgId),
Msg2 = jsone:encode(Msg, [native_utf8]).

Fun = fun() ->
    Li = imboy_session:list_by_uid(Uid),
    [Pid ! {close, 4006, <<"token invalid, please login again.">>} || {Pid, {_DType1, DID1}} <- Li, DID1 == DID]
end.

message_ds:send_next(Uid, MsgId, Msg2, [3000, 5000, Fun], [DID], true).
```

## 用户通过WS服务链接成功(以实现)
* ./apps/imapi/src/websocket_handler.erl
	* line 70 user_logic:online/4
* ./apps/imapi/src/user_logic.erl
	* line 28  syn:join/4
	* line 33 user_server:cast_online/3

> 用户所有的在线设备，用户所有加入的群组，一个用户只有一个WS链接；

## 附件资源的安全验证(以实现)
* s=open 的资源做安全认证的时候不做过期校验
* 其他资源做过期校验


## 用户收藏 (以实现)
一个类似”微信里面的收藏“功能，收藏聊天的是发布的图片、视频、文件等用户觉得重要的信息。

> 收藏记录已经在 postgresql 里面已做AES加密存储

> 调整收藏方案：清除 收藏专有的存储服务，改用定时任务扫描，在判断资源配如果没收藏嘞，就不删除；否则，过期删除之

* 需求：
    * 用户在聊天中参数的图片、视频、文件数据按特定目录、特定时间存储在 Go-FastDFS 存（ String savePath = "/$prefix/${dt.year}${dt.month}/${dt.day}_${dt.hour}/";），我计划定期删除超过半年或者一年的 savePath 里面的文件。
* 设想：
    * 用户在收藏摸个附件的时候，我就上述 savePath 对应的原始文件 copy一份到 "collect_${savePath}" 目录，让后我就可以定期的放心删除半年或者一年以上的 savePath 下面的文件了。
* 目前实现方案
    * 假设两个 Go-FastDFS 服务（需要两个二级域名 c.imboy.pub 用于收藏； a.imboy.pub 用于聊天等零时附件存储）；
    * 数据库新建 attachment 表，最收藏的资源做引用计数管理，当计算器为0的时候，可以删除对应资源；
    * a.imboy.pub 服务的资源，可以按时间戳定时直接清理；
    * 该方案有缺陷：从a.imboy.pub copy 附件到 c.imboy.pub 比较耗时（解决耗时问题需要调整 Go-FastDFS 服务）
*


## APP端 sqflite3 数据库升降级功能(以实现，待测试)
APP端使用 sqflite 包的 onCreate/2 onUpgrade/3 onDowngrade/3 触发服务端接口（/app_ddl/get?type=[upgrade|downgrade|create]，实现可控的升降级功能

* 基于“一个初始版的DDL + 若干升级DDL” 或 “一个初始版的DDL - 若干升级DDL”
* 后端可控，有升降级记录

## 基于jieba 分成 和 AC自动机的敏感词过滤系统 (未实现)
* 按敏感词分类创建 tree A1 = aho_corasick:build_tree(["BC","ABCD"]). 缓存到 内存
* 对用户数文本 InputTxt 使用jieba分词 应该自动过滤的无效词
* 比较零时生成的分词集合 是否 预 特定分类的敏感词分词集合是否有交集
* 有交集，循环分词结果，调用  aho_corasick:match(Word1, A1). 找出分词，api响应警告
* 无交集，api 响应pass
* https://wudeng.github.io/2018/04/13/wordfilter/

sensitive_word

## 基于地理位置的文学阅读APP (未实现)
基于postgis，做一个一带一路的沿途文化介绍的地图功能

* https://www.cnblogs.com/ssjxx98/articles/14131142.html
* https://download.geofabrik.de/asia.html

## More
