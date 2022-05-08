# imboy

基于 [cowboy](https://github.com/ninenines/cowboy)(Small, fast, modern HTTP server for Erlang/OTP) 的即时聊天后端服务，使用 "阿里云8核16G ecs.sn1ne.2xlarge主机（100万PPS）"压测，保持100万+TCP，90分钟以上，详细测试件[测试文档](test/doc/test1.md)

因为我是中国人，所以选择了[木兰宽松许可证, 第2版](https://gitee.com/imboy-pub/imboy-flutter/blob/main/LICENSE)

https://ninenines.eu/docs/en/cowboy/2.9/guide/getting_started/

## 环境依赖  (Environment depends on)

数据结构(./priv/sql/)开发中有变动，以第一个发布版为准；目前只支持MySQL

(Data structure (./priv/ SQL /) changes under development, subject to the first release; Currently, only MySQL is supported)

------
Erlang/OTP 23 / Erlang/OTP 24

MySQL 5.7 / MySQL 5.8

### kerl
```
// 列表可安装的版本号
kerl list releases

kerl build 24.3.3 / kerl delete build 24.3.3


kerl list builds

kerl install 24.3.3 ~/kerl/24.3.3

. /Users/leeyi/kerl/24.3.3/activate

Later on, you can leave the installation typing:
kerl_deactivate

Anytime you can check which installation, if any, is currently active with:
kerl active

```

## [Using templates](https://erlang.mk/guide/getting_started.html)
```
make list-templates
make new t=cowboy.http n=handler_passport
make new t=cowboy.ws n=handler_websocket

make new t=cowboy.middleware n=middleware_demo
make new t=cowboy.middleware n=middleware_auth
make new t=gen_server n=server_account

// 我添加的模板
make new t=rest_handler n=handler_demo
make new t=logic n=logic_demo
make new t=repository n=repo_demo
make new t=transfer n=transfer_demo


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

make new-app in=imboy_msg

make new-lib in=imboy_lib
make new-app in=imboy_admin
make new-app in=imboy_ws
make new-app in=imboy_api
make new-app in=imboy_cli

make new t=gen_server n=server_demo
make new t=gen_server n=server_demo in=imboy

make dialyze
```

reload_mk https://github.com/bullno1/reload.mk


## make

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

## 分析工具  (Analysis tool)
* [Dialyzer](https://erlang.mk/guide/dialyzer.html)
* [Look Glass](https://github.com/rabbitmq/looking_glass)

```
make dialyze

代码格式工具
get from https://github.com/sile/efmt/releases

chmod +x efmt

make efmt
```


# 发布  (Release)
```
IMBOYENV=prod make rel
IMBOYENV=test make rel
IMBOYENV=dev make rel -j8
IMBOYENV=local make rel
```

复制代码到特定的目录  (Copy code to a specific directory)

```
cp ./_rel/imboy/imboy-1.0.0.tar.gz
// or
scp ./_rel/imboy/imboy-1.0.0.tar.gz root@192.168.2.207:/usr/local/imboy/

```

去启动服务  (To start the service)

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

## 更新发布  (updates)

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

# api 约定  (api convention)
* [API参考](./priv/doc/API定义.md)
* [消息格式参考](./priv/doc/消息类型.md)


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

## 消息投递机制 (未实现功能)

1. 判断用户是否在线，如果用户离线，直接存储离线消息
2. 用户在线（or 用户上线），判断 erlang is_process_alive(Pid) 马上投递一次
3. 没有收到消息之前， 2 S -> 5 S -> 7S -> 11 S 投递4次
4. 如果收到消息 清理定时器，清理数据库消息
5. 4次投递都未确认消息，待用户下次登录再投递

## erlang 的shell 访问远程节
```
erl -name debug@127.0.0.1
auth:set_cookie('imboy'),net_adm:ping('imboy@127.0.0.1').
net_adm:names().
{ok,[{"imboy",55042},{"debug",60595}]}

按 Ctrl+G 出现user switch command
然后输入

r 'imboy@127.0.0.1'

按回车

在按 J 机器显示节点:
 --> j
   1  {shell,start,[init]}
   2* {'imboy@127.0.0.1',shell,start,[]}

在 * 的就是默认的可连接节点，其中的1 行，就是你现在的master节点

按 c 就能连接

你如果要连接到第三节点的话，直接 输入 c 6 回车就行了。

chat_store_repo:lookup(1).

curl -L https://github.com/sile/erldash/releases/download/0.1.1/erldash-0.1.1.x86_64-unknown-linux-musl -o erldash
chmod +x erldash
./erldash imboy@127.0.0.1 -c imboy
```

## websocket 在线工具调试
```
http://coolaf.com/tool/chattest
io:format("~p~n", [token_ds:encrypt_token(4)]).

ws://192.168.31.41:9800/ws?authorization=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJleHAiOjE2NTAxMjg4NDkyODcsInN1YiI6InRrIiwidWlkIjoiOHliazViIn0.LxmboEGP31xDC-E-So6SaMnAIx_T8fxTNIsr49S7R0w
ws://192.168.43.174:9800/ws?authorization=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJleHAiOjE2NTAxMjg4NDkyODcsInN1YiI6InRrIiwidWlkIjoiOHliazViIn0.LxmboEGP31xDC-E-So6SaMnAIx_T8fxTNIsr49S7R0w

(imboy@127.0.0.1)10>  hashids_translator:uid_encode(4).
<<"8ybk5b">>
(imboy@127.0.0.1)11> hashids_translator:uid_encode(1).
<<"kybqdp">>
{"id":"text5","type":"C2C","from":"8ybk5b","to":"kybqdp","payload":{"msg_type":"text","text":"text5"},"created_at":1650118822382,"server_ts":1650118823376}
```

## Email
```
gen_smtp_client:send({"sender@gmail.com", ["receiver@gmail.com"], "Subject: testing"},
   [{relay, "smtp.gmail.com"}, {ssl, true}, {username, "sender@gmail.com"},
      {password, "senderpassword"}]).

```
