# imboy

基于 [cowboy](https://github.com/ninenines/cowboy)(Small, fast, modern HTTP server for Erlang/OTP) 的即时聊天后端服务，使用 "阿里云8核16G ecs.sn1ne.2xlarge主机（100万PPS）"压测，保持100万+TCP，90分钟以上，详细测试件[测试文档](test/doc/test1.md)

因为我是中国人，所以选择了[木兰宽松许可证, 第2版](https://gitee.com/imboy-pub/imboy-flutter/blob/main/LICENSE)


一些功能的设计思考权衡过程，请参考[文档](./doc/design_thinking.md)

## Version
力求基于“语义化版本控制的规范”([语义化版本 2.0.0](https://semver.org/lang/zh-CN/))实施版本管理.

Strive to implement version management based on "Specification for Semantic version Control"([Semantic Versioning 2.0.0](https://semver.org/)).

## 环境依赖  (Environment depends on)

数据结构(./doc/postgresql/)开发中有变动，以第一个发布版为准；目前改成基于 PostgreSQL15 开发

There are changes in the data structure (./doc/postgresql/vsn0.1) under development. It is currently based on PostgreSQL15

------

* Erlang/OTP 24 (Cowboy 2.11 requires Erlang/OTP 24.0 or greater.)
* Erlang/OTP 25
* Erlang/OTP 26
* Erlang/OTP 27

* 数据库 PostgreSQL16

* [more](./doc/deps_service.md)

```
erl -eval '{ok, Version} = file:read_file(filename:join([code:root_dir(), "releases", erlang:system_info(otp_release), "OTP_VERSION"])), io:fwrite(Version), halt().' -noshell
```

## erlang 的shell 访问远程节
```
_rel/imboy/bin/imboy remote_console


net_adm:ping('imboy@api.docker.imboy.pub').
net_kernel:connect_node('imboy@api.docker.imboy.pub').

net_adm:ping('node1@127.0.0.1').

erl -name debug@127.0.0.1
auth:set_cookie('imboy'),net_adm:ping('imboy@127.0.0.1').
net_adm:names().
{ok,[{"imboy",55042},{"debug",60595}]}

按 Ctrl+G 出现user switch command
然后输入

r 'imboy@127.0.0.1'
r 'node2@127.0.0.1'

按回车

在按 J 机器显示节点:
 --> j
   1  {shell,start,[init]}
   2* {'imboy@127.0.0.1',shell,start,[]}

在 * 的就是默认的可连接节点，其中的1 行，就是你现在的master节点

按 c 就能连接

你如果要连接到第三节点的话，直接 输入 c 6 回车就行了。

curl -L https://github.com/sile/erldash/releases/download/0.1.1/erldash-0.1.1.x86_64-unknown-linux-musl -o erldash
chmod +x erldash
./erldash imboy@127.0.0.1 -c imboy

```

## [Using templates](https://erlang.mk/guide/getting_started.html)
```
make new t=cowboy.middleware n=demo_middleware
make new t=cowboy.middleware n=verify_middleware
make new t=gen_server n=account_server

make distclean

// 我添加的模板 以imboy开头
make new t=imboy.rest_handler n=demo_handler
make new t=imboy.logic n=demo_logic
make new t=imboy.repository n=demo_repo
make new t=imboy.ds n=demo_ds


make list-templates

make new-lib in=imlib
make new-lib in=imcron
make new-lib in=imds
make new t=imboy.logic n=demo_cron_logic in=imcron

make new t=imboy.rest_handler n=adm_passport_handler in=imadm

make new-lib in=imapi

make new-app in=imsos
...

IMBOYENV=local make run HTTP_PORT=9800


make run

// on Mac
IMBOYENV=pro make run
IMBOYENV=test make run
IMBOYENV=dev make run
IMBOYENV=local make run

make rel IMBOYENV=local

// on CentOS8 OR macOS
export IMBOYENV='local' && make run

observer_cli:start().

make new t=gen_server n=server_demo

// 重新加载 sys.config 配置
config_ds:local_reload()

Routes = imboy_router:get_routes(),
Dispatch = cowboy_router:compile(Routes),
cowboy:set_env(imboy_listener, dispatch, Dispatch).

make dialyze
```


## make

```

// 小心！这将构建该文件，即使它之前已经存在。
make rebar.config

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

## Many applications in one repository
```
make new-app in=webchat
```

## test

./doc/test.md

## 分析工具  (Analysis tool)
* [Dialyzer](https://erlang.mk/guide/dialyzer.html)
* [Look Glass](https://github.com/rabbitmq/looking_glass)

```
make dialyze

代码格式工具
get from https://github.com/sile/efmt/releases

VERSION=0.14.1
curl -L https://github.com/sile/efmt/releases/download/${VERSION}/efmt-${VERSION}.x86_64-unknown-linux-musl -o efmt
chmod +x efmt
./efmt

./efmt -c src/websocket_logic.erl
./efmt -w src/websocket_logic.erl
```


# 发布  (Release)
```
IMBOYENV=prod make rel
IMBOYENV=test make rel
IMBOYENV=dev make rel -j8
IMBOYENV=local make rel

% 生成自解压存档
% 自解压脚本目前仅支持以console模式启动发布
IMBOYENV=local make SFX=1
% run
_rel/imboy.run

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

bin/imboy daemon
bin/imboy remote_console

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

https://erlang.mk/guide/relx.html


```
make clean
git checkout afb81f8 && make clean && IMBOYENV=local make rel
git checkout -f dev
vim relxlocal.config
IMBOYENV=local make rel
make clean
IMBOYENV=local make relup

vsn=0.2.11 ./appup.sh

 ./bin/imboy pid
2147601

./bin/imboy versions
    Installed versions:
    * 0.2.6 permanent
    * 0.2.5 old
```
Your release was upgraded!

重复升级到
```
/usr/local/imboy/bin/imboy downgrade 0.2.7
IMBOYENV=local make relup
/usr/local/imboy/bin/imboy uninstall 0.2.8
vsn=0.2.8 ./appup.sh
```

## 分布式启动
* 启动/停止
```
make start node=node1 port=9801
make start node=node2 port=9802 cookie=imboycookie
make start node=node3 port=9803 cookie=imboycookie exclude="imadm,imcron"
make start node=node4 port=9804 cookie=imboycookie exclude="imadm,imcron" daemon=daemon

make stop node=node1
```

* 验证分布式连接
在任一节点 shell，输入：

```
net_adm:ping('node2@127.0.0.1').
% 返回 pong 则连接成功

net_adm:names().

```

* 测试 syn 分布式功能



在 node1 上：
```
imboy_syn:init().

imboy_syn:join(1, <<"ios">>, self(), <<"did11">>).
```

在 node2 上：
```
imboy_syn:init().

imboy_syn:list_by_uid(1).
% 应该能看到 node1 注册的设备
```

在 node2 上：

```
imboy_syn:publish(1, <<"hello from node2">>).
% node1 的进程会收到消
```

## [Updating Erlang.mk](https://erlang.mk/guide/updating.html#_initial_bootstrap)
```
make erlang-mk
```

## imboy.appup

Appup Cookbook https://cloud.tencent.com/developer/section/1122611

```
{"0.2.0",
    所有版本"0.1.*"升级到版本"0.2.0",重启应用
   [{"0.2\\.[0-9]+", [{restart_application, imboy_app}
             ]}],
    版本"0.2.0"降级到所有版本"0.1.*",重启应用
   [{"0.1\\.[0-9]+", [{restart_application, imboy_app}
             ]}]
}.
```

## api 约定  (api convention)
* [API参考](./doc/API定义.md)
* [消息格式参考](./doc/消息类型.md)


## erlang 优化

./doc/erlang优化.md


## cowboy Live update
```
Routes = imboy_router:get_routes(),
Dispatch = cowboy_router:compile(Routes),
cowboy:set_env(imboy_listener, dispatch, Dispatch).
```

## reload sys.config
```
config_ds:reload().
config_ds:local_reload()

erl -config config/sys.dev.config -eval 'application:which_applications(), halt().'

```

## websocket 在线工具调试

为了简化代码取消WS了在线调试（如有必要，以后可以看情况添加一个h5页面做调试工具）

http://coolaf.com/tool/chattest
io:format("~p~n", [token_ds:encrypt_token(4)]).

```
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
telnet 81.68.209.56 34780


## eturnal

https://icetest.atec-systems.com/

https://webrtc.github.io/samples/src/content/peerconnection/trickle-ice/

```

cp /data/docker/eturnal/eturnal.yml /etc/

eturnalctl daemon


cd /www/wwwroot/eturnal/

_build/product/rel/eturnal/bin/eturnal console

_build/product/rel/eturnal/bin/eturnal daemon

/www/wwwroot/eturnal/_build/product/rel/eturnal/bin/eturnal daemon

tail -f /www/wwwroot/eturnal/_build/product/rel/eturnal/log/eturnal.log
```


## other

docker-compose -f docker-compose.yml up
docker-compose -f docker-compose-pro.yml up

rm -rf  /Users/leeyi/workspace/imboy/imboy/_rel/imboy/lib/wx-2.2.2/priv/wxe_driver.so && ln -s /opt/homebrew/Cellar/erlang@25/25.3.2.7/lib/erlang/lib/wx-2.2.2/priv/wxe_driver.so /Users/leeyi/workspace/imboy/imboy/_rel/imboy/lib/wx-2.2.2/priv/wxe_driver.so
