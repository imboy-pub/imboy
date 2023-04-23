# 下列依赖放到 https://gitee.com/imboy-tripartite-deps 只是为了在中国加载代码快速一些
# The following dependencies are placed at https://gitee.com/imboy-tripartite-deps just to make loading code in China faster
# 特此声明：下列依赖，被人未改动过原作者源码
# Hereby declare: the following dependencies have not been modified by the original author source code

# https://erlang.mk/guide/deps.html
dep_cowboy = git https://gitee.com/imboy-tripartite-deps/cowboy 2.9.0
dep_cowlib = git https://gitee.com/imboy-tripartite-deps/cowlib 2.12.0
# Ranch is a socket acceptor pool for TCP protocols.
dep_ranch = git https://gitee.com/imboy-tripartite-deps/ranch.git 2.1.0
# gen_smtp Email服务端、客户端
dep_gen_smtp = git https://gitee.com/imboy-tripartite-deps/gen_smtp 1.2.0
# goldrush 提供了快速的事件流处理
dep_goldrush = git https://gitee.com/imboy-tripartite-deps/goldrush 0.2.0
dep_hashids = git https://gitee.com/imboy-tripartite-deps/hashids-erlang 1.0.5
# hut 小型的日志处理库
dep_hut = git https://gitee.com/imboy-tripartite-deps/hut.git 1.4.0
# jsone An Erlang library for encoding, decoding JSON data.
dep_jsone = git https://gitee.com/imboy-tripartite-deps/jsone 1.7.0
# jsx an erlang application for consuming, producing and manipulating json
dep_jsx = git https://gitee.com/imboy-tripartite-deps/jsx.git 2.9.0
# jwerl JWT library
dep_jwerl = git https://gitee.com/imboy-tripartite-deps/jwerl 1.2.0
# dep_jwerl = git https://gitee.com/mirrors_emqx/jwerl 1.1.1
# lager 日志库
dep_lager = git https://gitee.com/imboy-tripartite-deps/lager 3.9.2
# dep_mysql = git https://gitee.com/imboy-tripartite-deps/mysql-otp 1.7.0
dep_mysql = hex 1.8.0
# poolboy A hunky Erlang worker pool factory
dep_poolboy = git https://gitee.com/imboy-tripartite-deps/poolboy 1.5.2
# Observer CLI 是一个可以被放入任何 Beam 节点的库，用于帮助 DevOps 人员诊断生产节点中的问题
dep_observer_cli = git https://gitee.com/imboy-tripartite-deps/observer_cli.git 1.7.3
# Recon 希望成为一套可用于生产环境的工具，用于诊断 Erlang 问题或安全地检查生产环境。
dep_recon = git https://gitee.com/imboy-tripartite-deps/recon.git 2.5.1
# depcache is an in-memory caching server for Erlang with dependency checks, cache expiration and local in process memoization of lookups.

dep_khepri = hex 0.7.0
dep_depcache = git https://gitee.com/imboy-tripartite-deps/depcache.git master
# syn 全局进程注册表和进程组管理器，能够自动管理动态集群（添加/删除节点）并从网络分裂中恢复。
# Syn 是 Erlang/OTP global的 registry 和 pg模块的替代品。Syn 实现了 强最终一致性。
dep_syn = git https://gitee.com/imboy-tripartite-deps/syn.git 3.3.0

# Relx 是一个组装 Erlang/OTP 版本的库。给定发布规范和要在其中搜索 OTP 应用程序的目录列表，它将生成发布输出。
dep_relx = git https://gitee.com/imboy-tripartite-deps/relx.git v4.7.0
# bbmustache 一个无逻辑的模板。 deps by relx
dep_bbmustache = git https://gitee.com/imboy-tripartite-deps/bbmustache.git v1.12.2
# erlware_commons 为与 Erlang 一起分发的 stdlib 应用程序的扩展，被 qdate 依赖
dep_erlware_commons = git https://gitee.com/imboy-tripartite-deps/erlware_commons v1.5.0
# qdate_localtime 这是 erlang_localtime 的一个分支 ，专门针对与 qdate的兼容性进行了修改。两者大多兼容，但多年来出现了一些分歧。
# 被 qdate 依赖
dep_qdate_localtime = git https://gitee.com/imboy-tripartite-deps/qdate_localtime 1.1.0
# qdate - Erlang Date and Timezone Library
dep_qdate = git https://gitee.com/imboy-tripartite-deps/qdate 0.7.0
dep_throttle = git https://gitee.com/imboy-tripartite-deps/throttle.git 0.3.0
dep_eredis = git https://gitee.com/imboy-tripartite-deps/eredis.git master
# fs Native Listener (Mac Windows Linux) 被 sync 依赖
dep_fs = git https://gitee.com/imboy-tripartite-deps/fs.git 6.1
# Sync 是一个开发者工具。它会即时重新编译和重新加载您的 Erlang 代码。
dep_sync = git https://gitee.com/imboy-tripartite-deps/sync.git master
# sumo_db旨在简化 erlang 应用程序的数据库访问。
# 它提供了一个非常简单的持久层，能够与不同的数据库交互，同时为您的代码提供一致的 api。
# dep_sumo_db = git https://gitee.com/imboy-tripartite-deps/sumo_db.git main
# dep_erlfmt = git https://github.com/WhatsApp/erlfmt.git main
