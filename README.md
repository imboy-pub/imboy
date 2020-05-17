# imboy

An OTP application


https://ninenines.eu/docs/en/cowboy/2.7/guide/getting_started/

数据库在第一个发布版放出

## Handling requests
```
make new t=cowboy.rest n=api/handler/init_handler
make new t=cowboy.http n=api/handler/passport_handler
make new t=cowboy.ws n=api/handler/websocket_handler
make new t=cowboy.middleware n=infrastructure/middleware/auth_middleware
make new t=cowboy.rest n=api/handler/chat_handler

make run
```


##
在一个shelll里面执行
```
make rel

make help
  rel           Build a release for this project, if applicable
```

在另一个shelll里面执行
```
erl> help().
    lm()       -- load all modified modules

```


## 代码热更新

dep_sync = git https://github.com/rustyio/sync.git master

https://blog.csdn.net/mycwq/article/details/13290757

```
sync:go().

%% 第一种热更新方式：
{Module, Binary, Filename} = code:get_object_code(Module), 
code:load_binary(Module, Filename, Binary).

%% 第二种热更新方式：
code:purge(Module), code:load_file(Module).

%% 第三种热更新方式：
code:soft_purge(Module) andalso code:load_file(Module).

code:soft_purge(address_handler) andalso code:load_file(address_handler).
code:soft_purge(lbs_handler) andalso code:load_file(lbs_handler).
code:soft_purge(lbs_util) andalso code:load_file(lbs_util).

```

# 框架详述
参考 [【DDD】领域驱动设计实践 —— 框架实现](https://www.cnblogs.com/daoqidelv/p/7499662.html)，有细节调整
## User Interface层
门面层，对外以各种协议提供服务，该层需要明确定义支持的服务协议、契约等。包含：

### api/dto
包括request和response两部分，通过它定义入参和出参的契约，在dto层可以使用基础设施层的validation组件完成入参格式校验；

### api/handler
支持不同访问协议的控制器实现，比如：http/restful风格、tcp/二进制流协议、mq消息/json对象等等。

handler使用基础设施层公共组件完成许多通用的工作：

* 调用checklogin完成登录态/权限校验；
* 调用logging组件完成日志记录；
* 调用message-resource组件完成错误信息转义，支持I18N；

## application层
### service
应用服务层，组合domain层的领域对象和基础设施层的公共组件，根据业务需要包装出多变的服务，以适应多变的业务服务需求。

应用服务层主要访问domain领域对象，完成服务逻辑的包装。

应用服务层也会访问基础设施层的公共组件，如rabbitmq，完成领域消息的生产等。

### assembler
组装器，负责将多个domain领域对象组装为需要的dto对象，比如查询帖子列表，需要从Post（帖子）领域对象中获取帖子的详情，还需要从User（用户）领域对象中获取用户的基本信息。

组装器中不应当有业务逻辑在里面，主要负责格式转换、字段映射等职责。

## domain层
业务领域层，是我们最应当关心的一层，也是最多变的一层，需要保证这一层是高内聚的。确保所有的业务逻辑都留在这一层，而不会遗漏到其他层。按照ddd（domain driven design）理论，主要有如下概念构成：

### domain entity
领域实体。有唯一标识，可变的业务实体对象，它有着自己的生命周期。比如社区这一业务领域中，‘帖子’就是一个业务实体，它需要有一个唯一性业务标识表征，同时他的状态和内容可以不断发生变化。

### domain value object
领域值对象。可以没有唯一性业务标识，且一旦定义，他是不可变的，它通常是短暂的。这和java中的值对象（基本类型和String类型）类似。比如社区业务领域中，‘帖子的置顶信息’可以理解为是一个值对象，不需要为这一值对象定义独立的业务唯一性标识，直接使用‘帖子id‘便可表征，同时，它只有’置顶状态‘和’置顶位置‘，一旦其中一个属性需要发生变化，则重建值对象并赋值给’帖子‘实体的引用，不会对领域带来任何负面影响。

### domain factory
领域对象工厂。用于复杂领域对象的创建/重建。重建是指通过respostory加载持久化对象后，重建领域对象。

### domain service
领域服务。区别于应用服务，他属于业务领域层。

可以认为，如果某种行为无法归类给任何实体/值对象，则就为这些行为建立相应的领域服务即可。比如：转账服务（transferService），需要操作借方/贷方两个账户实体。

传统意义上的util static方法中，涉及到业务逻辑的部分，都可以考虑归入domain service。

### domain event
领域事件。领域中产生的一些消息事件，通过事件通知/订阅的方式，可以在性能和解耦层面得到好处。

### repository
仓库。我们将仓库的接口定义归类在domain层，因为他和domain entity联系紧密。仓库用户和基础实施的持久化层交互，完成领域对应的增删改查操作。

仓库的实际实现根据不同的存储介质而不同，可以是redis、oracle、mongodb等。

鉴于现在社区服务的存储介质有三套：oracle、redis、mongodb，且各个存储介质的字段属性名不一致，因此需要使用translator来做翻译，将持久化层的对象翻译为统一的领域对象。

### translator
翻译器。将持久化层的对象翻译为统一的领域对象。

翻译器中不应当有业务逻辑在里面，主要负责格式转换、字段映射等职责。

## infrastructure层
基础设施层提供公共功能组件，供controller、service、domain层调用。

### repository impl
对domain层repository接口的实现，对应每种存储介质有其特定实现，如oracle的mapper，mongodb的dao等等。repository impl会调用mybatis、mongo client、redis client完成实际的存储层操作。

### checkLogin
权限校验器，判定客户端是否有访问该资源的权限。提供给User Interface层的Controller调用。

### exception
异常分类及定义，同时提供公共的异常处理逻辑，具体由ExceptionHandler实现。

### transport
transport完成和第三方服务的交互，可以有多种协议形式的实现，如http+json、tcp+自定义协议等，配套使用的还有Resolver解析器，用于对第三方服务的请求和响应进行适配，提供一个防腐层（AnticorruptionLayer，DDD原书P255）的作用。

### transcation
提供事务管理，交给Spring管理。

### logging
日志模块，记录trace日志，使用log4j完成。


# api 约定

* websocket Status 101 正常
* websocket Status 400 - 请求无效 为定义 sec-websocket-protocol
* websocket Status 406 - 无法接受 sec-websocket-protocol 不包含 text
* websocket Status 412 - 先决条件失败 缺少token参数
* api json code 0 成功
* api json code 1 失败（通用编码）
* api json code 706 token无效 (包含缺失token情况)
* api json code 707 请刷新token

# 压力测试

```

打开文件数 for mac
sudo launchctl limit maxfiles
sudo launchctl limit maxfiles 99999999 unlimited
sudo launchctl limit maxfiles 99999999 99999999
ulimit -n 99999999

sysctl net.inet.ip.portrange.first net.inet.ip.portrange.last

## 及高范围
net.inet.ip.portrange.hifirst: 49152
net.inet.ip.portrange.hilast: 65535

sysctl -w net.inet.ip.portrange.first=1025
sysctl -w net.inet.ip.portrange.last=655350
sysctl -w net.inet.ip.tcp_rmem=655350

# 创建一百万个进程
erl +Q 134217727 +P 1000000 -env ERL_MAX_PORTS 40960000 -env ERTS_MAX_PORTS 40960000

HAProxy + Docker * N + K8S + mnesia 集群
erlang:system_info(port_limit).

locust -f src/imboy.py --no-web -c 20000 -r 1000 -t 600s --logfile=logs/imboy-no-web.log
length(chat_store_repo:lookall()).

参考：
http://m.udpwork.com/item/11782.html
https://cloud.tencent.com/developer/article/1422476
https://www.yuanmomo.net/2019/07/26/mac-max-connections-config/
https://colobu.com/2014/09/18/linux-tcpip-tuning/

https://www.cnblogs.com/duanxz/p/4464178.html 单服务器最大tcp连接数及调优汇总

https://blog.51cto.com/yaocoder/1312821

http://hk.uwenku.com/question/p-tgiqupmb-oc.html

https://knowledge.zhaoweiguo.com/8tools/mqtts/emqtts/emqtt_tune.html

https://studygolang.com/articles/2416

https://www.iteye.com/blog/mryufeng-475003  erlang 节点间通讯的通道微调

http://www.wangxingrong.com.cn/archives/tag/百万并发连接服务器

https://qunfei.wordpress.com/2016/09/20/from-c10k-to-c100k-problem-push-over-1000000-messages-to-web-clients-on-1-machine-simultaneously/

https://stackoverflow.com/questions/32711242/erlang-simultaneously-connect-1m-clients

https://colobu.com/2015/05/22/implement-C1000K-servers-by-spray-netty-undertow-and-node-js

https://blog.csdn.net/zcc_0015/article/details/26407683 Linux下基于Erlang的高并发TCP连接压力实验

https://github.com/smallnest/C1000K-Servers
```

docker run -it --rm --name imboy-1 -p 9801:9800 -v "$PWD":/usr/src/imboy -w /usr/src/imboy erlang

// 后台运行
docker-compose up -d
docker-compose -f docker-local.yml up -d


下面的命令增加了19个IP地址，其中一个给服务器用

sudo ifconfig lo0 alias 192.168.1.10
sudo ifconfig lo0 alias 192.168.1.11
sudo ifconfig lo0 alias 192.168.1.12

sudo ifconfig lo0 alias 192.168.0.10
sudo ifconfig lo0 alias 192.168.0.11
sudo ifconfig lo0 alias 192.168.0.12

sudo ifconfig lo0 alias 127.0.0.10
sudo ifconfig lo0 alias 127.0.0.11
length(chat_store_repo:lookall()).

 Erlang虚拟机默认的端口上限为65536, erlang17通过erl +Q 1000000可以修改端口上限为1000000,利用erlang:system_info(port_limit)进行查询，系统可以打开的最大文件描述符可以通过erlang:system_info(check_io)中的max_fds进行查看，查看系统当前port数量可以用erlang:length(erlang:ports())得到

erlang:length(erlang:ports()).

37208 TCP  -- 5.20 M 带宽 -- 内存 4G

0.5 G 内存 9005 TCP
1 G 内存 19005 TCP
4G 内存 42642 TCP

42642 + 19005 * 2 + 9005 = 89657

每个socket占用内存在15~20k之间
10万socket 2G内存  15M带宽
50万socket 10G内存
100万socket 20G内存
