# imboy

基于 [cowboy](https://github.com/ninenines/cowboy)(Small, fast, modern HTTP server for Erlang/OTP) 的即时聊天后端服务，当前性能与稳定性验证以 `test/performance`、`test/stress`、CI 门禁与发布前专项回归为准。

因为我是中国人，所以选择了[木兰宽松许可证, 第2版](https://gitee.com/imboy-pub/imboy-flutter/blob/main/LICENSE)


核心架构说明请参考 `./doc/architecture/overview.md` 与 `./doc/architecture/database-access.md`。

## Version
力求基于“语义化版本控制的规范”([语义化版本 2.0.0](https://semver.org/lang/zh-CN/))实施版本管理.

Strive to implement version management based on "Specification for Semantic version Control"([Semantic Versioning 2.0.0](https://semver.org/)).

## 环境依赖  (Environment depends on)

数据结构以 `priv/migrations/*.sql` 为准，当前基于 PostgreSQL 18 开发。

Schema is defined by `priv/migrations/*.sql`, and the backend currently targets PostgreSQL 18.

------
* Erlang/OTP 28+

* 数据库 PostgreSQL18+

* [more](./doc/operations/dependencies.md)

```
erl -eval '{ok, Version} = file:read_file(filename:join([code:root_dir(), "releases", erlang:system_info(otp_release), "OTP_VERSION"])), io:fwrite(Version), halt().' -noshell
```

后端技术栈：

语言: Erlang/OTP 28+
Web框架: Cowboy (基于Erlang的HTTP服务器)
数据库: PostgreSQL 18
管理后台: 独立仓库 `imboy-admin-frontend` (React + Vite)
认证方式: Cookie-based (adm_user_id)
依赖管理: Erlang.mk

## 工程化基线

### CI

- GitHub Actions: `.github/workflows/backend-ci.yml`
- 升级/交付脚本入口:
  - `script/upgrade/run_feature_100.sh`：阶段化执行 `P0~P6`，其中 `P6` 覆盖 compile / unit / integration / dialyze / migration / contract / perf / security / release 门禁。
  - `script/upgrade/run_business_100_acceptance.sh`：后台与交付链路验收脚本，输出 `artifacts/feature100/business100/reports/*.md`。
  - `script/upgrade/generate_ack_threshold_runtime_probe_trend.sh`：生成 ACK runtime probe 趋势报告。
- 默认门禁:
  - `make compile`
  - `make eunit`
  - `make ct`
  - `make dialyze`
  - 迁移文件提交前，按 `doc/standards/migration_naming.md` 中的命令做前缀与命名自检

### 依赖可复现策略

- 当前工程以 `Makefile` + `include/deps.mk` 作为依赖定义入口，不再以 `rebar.config` 作为主依赖入口。
- 新增或升级依赖时，优先固定到 tag、commit 或明确版本号，避免继续扩大 `master` / `main` 漂移分支。
- CI 缓存命中应随 `Makefile` 与 `include/deps.mk` 变化而变化，保证构建结果可复现。

### 配置与密钥

- 示例配置见 `config/sys.config.example`。
- 生产环境启动会校验 `jwt_key` 与 `postgre_aes_key`，缺失将启动失败。
- 不要在仓库中提交真实密钥与数据库密码。
- 本地单节点开发时建议将 `cluster_nodes` 设为空列表，避免启动日志出现 `no_nodes_connected` 噪音。


## Erlang 远程 Shell 与节点调试

常用方式：

```bash
_rel/imboy/bin/imboy remote_console
```

需要手动连节点时，可参考以下命令：

```erlang
net_adm:ping('imboy@api.docker.imboy.pub').
net_kernel:connect_node('imboy@api.docker.imboy.pub').

net_adm:ping('node1@127.0.0.1').

erl -name debug@127.0.0.1
auth:set_cookie('imboy'), net_adm:ping('imboy@127.0.0.1').
net_adm:names().
{ok,[{"imboy",55042},{"debug",60595}]}
```

如果进入 Erlang shell 后需要切换远程节点：

1. 按 `Ctrl+G` 进入 user switch command；
2. 输入 `r 'imboy@127.0.0.1'` 或其他目标节点；
3. 输入 `j` 查看当前可切换节点列表；
4. 输入 `c <编号>` 切换到对应节点。

示例输出：

```text
 --> j
   1  {shell,start,[init]}
   2* {'imboy@127.0.0.1',shell,start,[]}
```

说明：带 `*` 的节点是当前默认连接节点。

如需图形化查看节点，也可使用 `erldash`：

```bash
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

# 自定义模板（以 `imboy` 开头）
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

# on macOS
IMBOYENV=pro make run
IMBOYENV=test make run
IMBOYENV=dev make run
IMBOYENV=local make run

make rel IMBOYENV=local

# on CentOS 8 or macOS
export IMBOYENV='local' && make run

observer_cli:start().

make new t=gen_server n=server_demo

# 重新加载 sys.config 配置
config_ds:local_reload()

Routes = imboy_router:get_routes(),
Dispatch = cowboy_router:compile(Routes),
cowboy:set_env(imboy_listener, dispatch, Dispatch).

make dialyze
```


## make

```

# 当前工程统一通过 Makefile / erlang.mk 管理依赖与构建
make rel

make help
  rel           Build a release for this project, if applicable
```

在另一个 shell 里执行
```
erl> help().
    lm()       -- load all modified modules

# 更新 erlang.mk
make erlang-mk

```

## Many applications in one repository
```
make new-app in=webchat
```

## Test

```bash
make eunit
make ct
```

## 分析工具（Analysis Tools）
* [Dialyzer](https://erlang.mk/guide/dialyzer.html)
* [Look Glass](https://github.com/rabbitmq/looking_glass)

```
make dialyze

代码格式工具：
从 https://github.com/sile/efmt/releases 获取 `efmt` 可执行文件。

VERSION=0.14.1
curl -L https://github.com/sile/efmt/releases/download/${VERSION}/efmt-${VERSION}.x86_64-unknown-linux-musl -o efmt
chmod +x efmt
./efmt

./efmt -c src/websocket_logic.erl
./efmt -w src/websocket_logic.erl
```


## 发布（Release）

说明：以下命令用于说明 `rel/relup` 产物的构建与部署方式；其中主机地址、目录和版本号需替换成你的实际环境。

可使用部署脚本按目标环境执行发布，例如：

```bash
./script/deploy.sh <host> <new_version> <old_version>

IMBOYENV=prod make rel
IMBOYENV=test make rel
IMBOYENV=dev make rel -j8
IMBOYENV=local make rel

IMBOYENV=local make relup
cp _rel/imboy/imboy-<version>.tar.gz /usr/local/imboy/releases/<version>/

% 生成自解压存档
% 自解压脚本目前仅支持以console模式启动发布
IMBOYENV=local make SFX=1
% run
_rel/imboy.run

```

复制产物到目标目录（Copy release artifact to target directory）

```
cp ./_rel/imboy/imboy-1.0.0.tar.gz /path/to/deploy/dir/
# or
scp ./_rel/imboy/imboy-1.0.0.tar.gz user@your-host:/path/to/deploy/dir/


```

启动服务（Start the service）

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

## 升级发布（Upgrade）

参考：

- https://erlang.mk/guide/relx.html

常见升级步骤：

```bash
IMBOYENV=prod make relup
mkdir -p releases/<new_version>/
mv path/to/imboy-<new_version>.tar.gz releases/<new_version>/
./bin/imboy versions
./bin/imboy upgrade <new_version>
```

回滚或清理旧版本时，可参考：

```bash
./bin/imboy downgrade <old_version>
./bin/imboy uninstall <old_version>
```

如需维护 `appup` 生成脚本或历史版本兼容策略，建议单独放到发布流程文档或交付脚本中，不再在根 `README.md` 保留具体历史版本示例。

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

## `imboy.appup`

参考：

- https://cloud.tencent.com/developer/section/1122611

以下示例仅说明 `appup` 结构；升级版本号与回滚版本范围应按当前发布计划填写：

```erlang
{"<new_vsn>",
   [{"<old_vsn_regex>", [{restart_application, imboy_app}]}],
   [{"<rollback_vsn_regex>", [{restart_application, imboy_app}]}]
}.
```

## API 约定（API Convention）
* [API参考](./doc/api/rest-api.md)


## Erlang 优化

性能与稳定性优化以 CI 基线（`make dialyze`、测试门禁）和发布前压测结果为准。


## cowboy Live update
```
Routes = imboy_router:get_routes(),
Dispatch = cowboy_router:compile(Routes),
cowboy:set_env(imboy_listener, dispatch, Dispatch).
```

## 重新加载 `sys.config`
```erlang
config_ds:reload().
config_ds:local_reload()

erl -config config/sys.dev.config -eval 'application:which_applications(), halt().'

```

## WebSocket 在线工具调试

为了简化代码取消WS了在线调试（如有必要，以后可以看情况添加一个h5页面做调试工具）

http://coolaf.com/tool/chattest
io:format("~p~n", [token_ds:encrypt_token(4)]).

```text
(imboy@127.0.0.1)10>  hashids_translator:uid_encode(4).
<<"8ybk5b">>
(imboy@127.0.0.1)11> hashids_translator:uid_encode(1).
<<"kybqdp">>
{"id":"text5","type":"C2C","from":"8ybk5b","to":"kybqdp","payload":{"msg_type":"text","text":"text5"},"created_at":1650118822382,"server_ts":1650118823376}
```

## Email
```erlang
gen_smtp_client:send({"sender@gmail.com", ["receiver@gmail.com"], "Subject: testing"},
   [{relay, "smtp.gmail.com"}, {ssl, true}, {username, "sender@gmail.com"},
      {password, "senderpassword"}]).

```
## Eturnal / TURN 参考

说明：`eturnal`、`coturn` 一类音视频基础设施通常带有强环境绑定配置，当前仓库不再保留具体 IP、绝对路径或面板路径示例。

可参考以下公开检测页面进行联调：

- https://icetest.atec-systems.com/
- https://webrtc.github.io/samples/src/content/peerconnection/trickle-ice/

如需维护 STUN/TURN 部署脚本，建议放在交付环境、运维仓或基础设施仓中。

## 其他

```bash
docker-compose -f docker-compose.yml up
docker-compose -f docker-compose-pro.yml up
```
