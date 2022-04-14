# imboy

基于 [cowboy](https://github.com/ninenines/cowboy)(Small, fast, modern HTTP server for Erlang/OTP) 的即时聊天后端服务，使用 "阿里云8核16G ecs.sn1ne.2xlarge主机（100万PPS）"压测，保持100万+TCP，90分钟以上，详细测试件[测试文档](test/doc/test1.md)

因为我是中国人，所以选择了[木兰宽松许可证, 第2版](https://gitee.com/imboy-pub/imboy-flutter/blob/main/LICENSE)

https://ninenines.eu/docs/en/cowboy/2.9/guide/getting_started/

## 环境依赖

数据结构(./priv/sql/)开发中有变动，以第一个发布版为准，目前只支持MySQL

Erlang 版本
```
Erlang/OTP 23

Erlang/OTP 24 暂时不支持
```

## [Using templates](https://erlang.mk/guide/getting_started.html)
```
mkdir -p src src/api scr/api

make list-templates
make new t=cowboy.http n=api/handler/passport_handler
make new t=cowboy.ws n=api/handler/websocket_handler

make new t=rest_handler n=api/handler/test_handler
make new t=logic n=api/logic/test_logic
make new t=repository n=api/repository/test_repo
make new t=transfer n=api/transfer/test_transfer

make new t=cowboy.middleware n=common/middleware/auth_middleware
make new t=gen_server n=server/account_server

make run

// https://github.com/bullno1/reload.mk
// 更新代码之后 需要执行命令 make reload
// 为避免必须一直输入make reload，请使用 make auto-reload
// on Mac
IMBOYENV=prod make run RELOADABLE=1
IMBOYENV=test make run RELOADABLE=1
IMBOYENV=dev make run RELOADABLE=1
IMBOYENV=local make run RELOADABLE=1

// on CentOS8
export IMBOYENV='local' && make run RELOADABLE=1

observer_cli:start().

make new-lib in=imboy_lib
make new-app in=imboy_admin
make new-app in=imboy_ws
make new-app in=imboy_api
make new-app in=imboy_cli

make new t=gen_server n=infrastructure/server/my_server
make new t=gen_server n=infrastructure/server/my_server in=imboy

make dialyze
```


## make
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

// 更新 erlang.mk
make erlang-mk
```

## edoc
link http://erlang.org/doc/apps/edoc/chapter.html#Introduction
```
```

## test
```
```

## [Dialyzer](https://erlang.mk/guide/dialyzer.html)
```
make dialyze
```

## [代码热更新](https://github.com/bullno1/reload.mk)

其他方法

dep_sync = git https://github.com/rustyio/sync.git master

https://blog.csdn.net/mycwq/article/details/13290757

```

```


# 发布
```
IMBOYENV=prod make rel
IMBOYENV=test make rel
IMBOYENV=dev make rel -j8
IMBOYENV=local make rel
```

复制代码到特定的目录
```
cp ./_rel/imboy/imboy-1.0.0.tar.gz
// or
scp ./_rel/imboy/imboy-1.0.0.tar.gz root@192.168.2.207:/usr/local/imboy/

```

到特定主机的目录里面去启动服务
```

mkdir -p /usr/local/imboy

cp ./_rel/imboy/imboy-1.0.0.tar.gz /usr/local/imboy/

cd /usr/local/imboy

tar -xzf imboy-1.0.0.tar.gz

bin/imboy console

bin/imboy start

bin/imboy restart

bin/imboy stop
```
## 更新发布
link https://erlang.mk/guide/relx.html
```
IMBOYENV=prod make relup
```

For the purpose of this section, assume the initial release version was 1, and the new version is 2. The name of the release will be example.

Once all this is done, you can build the tarball for the release upgrade:
```
$ make relup
```
This will create an archive at the root directory of the release, $RELX_OUTPUT_DIR/example/example-2.tar.gz.

Move the archive to the correct location on the running node. From the release’s root directory:
```
$ mkdir releases/2/
$ mv path/to/example-2.tar.gz releases/2/
```

Finally, upgrade the release:
```
$ bin/example_release upgrade "2/example_release"

scp ./_rel/imboy/imboy-0.1.1.tar.gz root@120.24.63.33:/usr/local/imboy

mv imboy-0.1.1.tar.gz releases/0.1.1/
bin/imboy upgrade "0.1.1/imboy"

bin/imboy downgrade "0.1.0/imboy"

```
Your release was upgraded!

## [Updating Erlang.mk](https://erlang.mk/guide/updating.html#_initial_bootstrap)
```
make erlang-mk
```

## imboy.appup
```
{"0.2.0",
    所有版本"0.1.*"升级到版本"0.2.0",重启应用
   [{"0.1\\.[0-9]+", [{restart_application, imboy_app}
             ]}],
    版本"0.2.0"降级到所有版本"0.1.*",重启应用
   [{"0.1\\.[0-9]+", [{restart_application, imboy_app}
             ]}]
}.
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
* api json code 1 失败（通用编码，前端不弹出提示）
* api json code 2 失败（通用编码，前端弹出提示）
* api json code 705 请刷新token
* api json code 706 token无效 (包含缺失token情况)
* api json code 786 - 在其他平台登录

# erlang 优化
```
+K true
开启epoll调度，在linux中开启epoll，会大大增加调度的效率

+A 1024
异步线程池，为某些port调用服务

+P 2048000
最大进程数

+Q 2048000
最大port数

+sbt db
绑定调度器，绑定后调度器的任务队列不会在各个CPU线程之间跃迁，结合sub使用，可以让CPU负载均衡的同时也避免了大量的跃迁发生。

注意：一个linux系统中，最好只有一个evm开启此选项，若同时有多个erlang虚拟机在系统中运行，还是关闭为好


+sub true
开启CPU负载均衡，false的时候是采用的CPU密集调度策略，优先在某个CPU线程上运行任务，直到该CPU负载较高为止。

+swct eager
此选项设置为eager后，CPU将更频繁的被唤醒，可以增加CPU利用率

+spp true
开启并行port并行调度队列，当开启后会大大增加系统吞吐量，如果关闭，则会牺牲吞吐量换取更低的延迟。

+zdbbl 65536
分布式erlang的端口buffer大小，当buffer满的时候，向分布式的远程端口发送消息会阻塞

```

# 压力测试

```

打开文件数 for mac
sudo launchctl limit maxfiles
sudo launchctl limit maxfiles 2097152 2097152
sudo ulimit -n 2097152

sysctl net.inet.ip.portrange.first net.inet.ip.portrange.last

## 及高范围
net.inet.ip.portrange.hifirst: 49152
net.inet.ip.portrange.hilast: 65535

sysctl -w net.inet.ip.portrange.first=1025
sysctl -w net.inet.ip.portrange.last=655350
sysctl -w net.inet.ip.tcp_rmem=655350


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

100万并发连接服务器笔记之Erlang完成1M并发连接目标 https://blog.csdn.net/shallowgrave/article/details/19990345?utm_medium=distribute.pc_relevant.none-task-blog-BlogCommendFromBaidu-5.nonecase&depth_1-utm_source=distribute.pc_relevant.none-task-blog-BlogCommendFromBaidu-5.nonecase
```

docker run -it --rm --name imboy-1 -p 9801:9800 -v "$PWD":/usr/src/imboy -w /usr/src/imboy erlang

// 后台运行
docker-compose up -d
docker-compose -f docker-local.yml up -d


下面的命令增加了19个IP地址，其中一个给服务器用
sudo ifconfig lo0 alias 127.0.0.10
sudo ifconfig lo0 alias 127.0.0.11
length(chat_store_repo:lookall()).

sudo ifconfig lo0 -alias 127.0.0.10
sudo ifconfig lo0 -alias 127.0.0.11

 Erlang虚拟机默认的端口上限为65536, erlang17通过erl +Q 1000000可以修改端口上限为1000000,利用erlang:system_info(port_limit)进行查询，系统可以打开的最大文件描述符可以通过erlang:system_info(check_io)中的max_fds进行查看，查看系统当前port数量可以用erlang:length(erlang:ports())得到

erlang:system_info(port_limit)
erlang:system_info(check_io)
erlang:length(erlang:ports()).

Pid = spawn(fun() -> etop:start([{output, text}, {interval, 1}, {lines, 20}, {sort, memory}]) end).

```
查看TCP 数量
netstat -n | awk '/^tcp/ {++S[$NF]} END {for(a in S) print a, S[a]}'
ESTABLISHED 28705

free -h
              total        used        free      shared  buff/cache   available
Mem:           3.7G        2.8G        774M        452K        140M        726M
Swap:            0B          0B          0B

查看 pid
 pmap -d 6380
```

erl -name ws2@127.0.0.1 -setcookie imboy -hidden
net_adm:ping('imboy@127.0.0.1').
Ctrl + G
r 'imboy@127.0.0.1'
j
c 2

cowboy_websocket 异步消息
websocket close 传递参数

如何动态加载配置文件
```
ifeq ($(ENV),prod)
    RELX_CONFIG = $(CURDIR)/relx.prod.config
else ifeq ($(ENV),test)
    RELX_CONFIG = $(CURDIR)/relx.test.config
else ifeq ($(ENV),dev)
    RELX_CONFIG = $(CURDIR)/relx.dev.config
else ifeq ($(ENV),local)
    RELX_CONFIG = $(CURDIR)/relx.local.config
else
    RELX_CONFIG ?= $(CURDIR)/relx.config
endif
```


## Q

## 消息确认机制 QoS
https://blog.csdn.net/Jessechanrui/article/details/88399012

socket 数据粘包问题、拆包问题

## 消息投递机制

1. 判断用户是否在线，如果用户离线，直接存储离线消息
2. 用户在线（or 用户上线），判断 erlang is_process_alive(Pid) 马上投递一次
3. 没有收到消息之前， 2 S -> 5 S -> 7S -> 11 S 投递4次
4. 如果收到消息 清理定时器，清理数据库消息
5. 4次投递都未确认消息，待用户下次登录再投递


## Email
```
gen_smtp_client:send({"sender@gmail.com", ["receiver@gmail.com"], "Subject: testing"},
   [{relay, "smtp.gmail.com"}, {ssl, true}, {username, "sender@gmail.com"},
      {password, "senderpassword"}]).

```
