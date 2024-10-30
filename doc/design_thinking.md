
记录一些功能的设计思考权衡过程

## 客户端服务端通讯安全(已实现)
* API 使用 HTTPS；
* 基于JWT的权限验证；
* 基于客户端版本号的签名密钥管理，有利于密钥更新；
* 配置在 option 和 open 里面的路由不需要做JWT认证，其他的都需要做JWT认证；
* 小部分open路由不需要做签名验证，其他所有路由都需要签名验证；
* 用户获取公钥数据的路由 /init 响应的数据通过基于版本号管的密钥的AES算法加密
* 用户注册登录密码通过公钥做 RSA 算法加密传输，通过 hmac_sha512 算法加盐存储；
* 发送到客户端的用户ID都要经过 hashids 算法混淆；

## 数据库存储(已实现)
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
* timescaledb 计划用于群聊消息存储
* pgroonga 暂无用例
* pgcrypto
* vector Postgres 的开源向量相似度搜索
* roaringbitmap


## GEO 方案 (已实现)
通过对比选择使用强大的 postgis

## 全文索引 方案 (已实现)
通过对比选择使用强大的 pg_jieba + pgroonga


## 缓存方案 (已实现)
刻意规避使用Redis，使用 erlang 的 depcache

## 数据库连接池方案 (已实现)
使用 epgsql + pooler

## 附件资源存储方案
选择了 Go-FastDFS ，基于golang实现的自建服务，功能基本够用


## 消息确认机制 QoS
https://blog.csdn.net/Jessechanrui/article/details/88399012

socket 数据粘包问题、拆包问题

## 消息投递机制 (已实现)

1. 判断用户是否在线，如果用户离线，直接存储离线消息
2. 用户在线（or 用户上线），判断 erlang is_process_alive(Pid) 马上投递一次
3. 没有收到消息之前， 2 S -> 5 S -> 7S -> 11 S 投递4次
4. 如果收到消息 清理定时器，清理数据库消息
5. 4次投递都未确认消息，待用户下次登录再投递

以上消息确认重复机制，可以确保消息不丢失

## WebSocket 链接token校验机制 (已实现)

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

## 用户通过WS服务链接成功(已实现)
* ./apps/imapi/src/websocket_handler.erl
	* line 70 user_logic:online/4
* ./apps/imapi/src/user_logic.erl
	* line 28  syn:join/4
	* line 33 user_server:cast_online/4

> 用户所有的在线设备，用户所有加入的群组，一个用户只有一个WS链接；

## 附件资源的安全验证(已实现)
* s=open 的资源做安全认证的时候不做过期校验
* 其他资源做过期校验


## 用户收藏 (已实现)
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

## 用户注销
* 通知当前用户所有设备已注销
* 通知用户所有朋友，该用户已经注销
* 清理所有用户相关数据，写入 时序数据库
* 用户注销以后,用户的所有好友和群组关系需要解除

流程参考 https://blog.51cto.com/u_15069441/4323079

## APP端 sqflite3 数据库升降级功能(已实现，待测试)
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

## 时间判断的问题
前提条件1 服务端用erlang生产的 JWT 里面的过期时间是  erlang:system_time(millisecond) 获取的（假设服务器设置的时区是utc+8）,
前提条件2  APP端 的用户（加速用户时区是utc+1）是通过dart语言的 DateTime.now().millisecondsSinceEpoch 获取的毫秒数

后置结果：
客户端通过  (jwt.claims['exp'] ?? 0) - 1500 > DateTime.now().millisecondsSinceEpoch
        ? true
        : false;

问题： 上面的方法判断token是否过期，会不会受到时区影响？

答： 会影响
解决办法： 获取时区差，统一用utc+0时区的时间戳计算，以避免影响

## 关于群聊的思考


https://37signals.com/group-chat-problems/
https://wx.zsxq.com/dweb2/index/group/511244584#创业笔记 733：老派的工作者

1. 禁言功能
2. 群组撤回任何消息
3. 群公告
4. 预约聊天时间、聊天主题，会议室的功能，完毕有，可以整理会议纪要（一次会议一个topic）

Basecamp
留言板，待办事项列表、个人待办事项、文档、公告、签到等

考勤记录，是公司的，也是个人的；
审批记录，是公司的，也是个人的；

附加到 Basecamp 中每个对象的评论线程（待办事项列表、个人待办事项、文档、公告、签到等）。评论线程还可以使事情保持在上下文中，因为有关待办事项（或文档或文件或公告或...）的讨论永久附加到该待办事项

群聊的优点
1. 快速解决问题
2. 红色警报
3. 玩得开心
4.归属感
5,. 聊天室里谈论一些工作，没有人反对。这导致人们假设每个人都阅读了该讨论并同意。但他们不是，或者他们没有
6.下意识的反应
7. 堆积和转移对话
8. 漫无目的和重复
9. 实时向每个人提供过多信息
10.聊天提醒你你落后了
11. 25 过去表示 1(消息过快过多，导致精神损耗)
13. 躁狂的情境转移和持续的部分注意力
14. 无法事后回顾和参考
15.缺乏背景
16. 存在、假设和期望
17. 跨时区通讯


群聊的缺点
1、精神疲劳、疲惫不堪
3. 害怕错过或没有发言权
4.一次思考一行而不是一次一个想法


## 频道功能
Telegram 是一款以安全性和隐私性著称的即时通讯软件，它提供了多种通信方式，包括私人聊天、群组聊天和频道（Channels）。频道是 Telegram 的一个独特功能，它允许用户创建一个专门的频道来发布消息、图片、视频和文件，这些内容可以被不限量的订阅者查看。

以下是 Telegram 频道的一些设计特点：

1. **单向通信**：频道允许创建者（或被授权的管理员）向订阅者发送消息，但默认情况下订阅者之间不能相互通信。这是一种单向的信息流，类似于社交媒体上的关注或订阅机制。

2. **内容发布**：频道主要用于大规模信息发布，如新闻、更新、通知等。频道可以用于个人、组织、媒体机构等。

3. **无限订阅者**：与群组有成员上限不同，Telegram 频道可以拥有无限数量的订阅者。

4. **频道管理员**：频道创建者可以添加管理员来帮助管理频道，包括批准或删除订阅者的评论。

5. **消息置顶**：频道创建者可以将重要消息置顶，确保订阅者在浏览频道时首先看到。

6. **自动回复**：频道可以设置自动回复，当新订阅者加入时发送欢迎消息。

7. **频道链接**：每个频道都有一个唯一的链接，可以通过这个链接邀请他人订阅频道。

8. **隐私**：频道创建者可以选择是否允许订阅者查看频道的成员列表。

9. **编辑和删除**：频道创建者可以编辑或删除他们发送的任何消息。

10. **频道描述**：频道可以有一个描述，帮助潜在的订阅者了解频道的内容。

11. **频道封面**：频道可以设置封面图片，增强视觉效果。

12. **搜索和发现**：Telegram 提供了搜索功能，用户可以搜索并发现感兴趣的频道。

13. **消息预览**：频道发送的消息可以生成预览，即使用户未订阅频道，也可以查看部分内容。

14. **禁止转发**：频道创建者可以设置禁止转发消息，以防止内容被复制到其他地方。

15. **动画和游戏**：Telegram 允许在频道中发送动画和游戏，增加了互动性。

16. **频道统计**：Telegram 提供了频道统计功能，让创建者可以了解频道的活跃度和订阅者的行为。

17. **超级群组**：如果频道变得非常活跃，Telegram 允许将频道转换为超级群组，以支持更大的成员数量和更多的管理功能。

Telegram 频道的设计旨在为用户提供一个强大的信息发布平台，同时保持了 Telegram 所承诺的安全性和隐私性。频道功能在新闻机构、社区、企业和个人用户中都非常受欢迎。

## 基于erlang框架cowboy Web框架做的api + websocket服务，多个节点的websocket服务如何互通消息？


## More
