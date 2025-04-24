# 下列依赖放到 https://gitee.com/imboy-tripartite-deps 只是为了在中国加载代码快速一些
# The following dependencies are placed at https://gitee.com/imboy-tripartite-deps just to make loading code in China faster
# 特此声明：下列依赖，被人未改动过原作者源码
# Hereby declare: the following dependencies have not been modified by the original author source code

dep_hex_core = hex 0.11.0

# https://erlang.mk/guide/deps.html
# Ranch is a socket acceptor pool for TCP protocols.
dep_ranch = git https://gitee.com/imboy-tripartite-deps/ranch.git 2.2.0
dep_cowlib = git https://gitee.com/imboy-tripartite-deps/cowlib 2.14.0
dep_cowboy = git https://gitee.com/imboy-tripartite-deps/cowboy 2.13.0

# gen_smtp Email服务端、客户端
# https://github.com/gen-smtp/gen_smtp.git
dep_gen_smtp = git https://gitee.com/imboy-tripartite-deps/gen_smtp 1.2.0

# jsone An Erlang library for encoding, decoding JSON data.
# 4months
dep_jsone = git https://gitee.com/imboy-tripartite-deps/jsone 1.9.0

# depcache is an in-memory caching server for Erlang with dependency checks, cache expiration and local in process memoization of lookups.
# https://github.com/zotonic/depcache.git
dep_depcache = git https://gitee.com/imboy-tripartite-deps/depcache.git 2.0.0

# jsx an erlang application for consuming, producing and manipulating json
# 2years
dep_jsx = git https://gitee.com/imboy-tripartite-deps/jsx.git v3.1.0

# Khepri 是一个用于 Erlang 和 Elixir 的树状复制磁盘数据库库。
dep_khepri = git https://gitee.com/imboy-tripartite-deps/khepri.git main

# jwerl JWT library
# 2years https://github.com/G-Corp/jwerl
dep_jwerl = git https://gitee.com/imboy-tripartite-deps/jwerl 1.2.0
# dep_jwerl = git https://gitee.com/mirrors_emqx/jwerl 1.1.1

# goldrush 提供了快速的事件流处理
# 6years
dep_goldrush = git https://gitee.com/imboy-tripartite-deps/goldrush 0.2.0
# 9years https://github.com/snaiper80/hashids-erlang
dep_hashids_erlang = git https://gitee.com/imboy-tripartite-deps/hashids-erlang 1.0.5


# syn 全局进程注册表和进程组管理器，能够自动管理动态集群（添加/删除节点）并从网络分裂中恢复。
# Syn 是 Erlang/OTP global的 registry 和 pg模块的替代品。Syn 实现了 强最终一致性。
# https://github.com/ostinelli/syn
dep_syn = git https://gitee.com/imboy-tripartite-deps/syn.git 3.3.0

# erlware_commons 为与 Erlang 一起分发的 stdlib 应用程序的扩展，被 qdate 依赖
# https://github.com/erlware/erlware_commons.git
dep_erlware_commons = git https://gitee.com/imboy-tripartite-deps/erlware_commons v1.7.0
# qdate_localtime 这是 erlang_localtime 的一个分支 ，专门针对与 qdate的兼容性进行了修改。两者大多兼容，但多年来出现了一些分歧。
# 被 qdate 依赖
# https://github.com/choptastic/qdate_localtime.git
dep_qdate_localtime = git https://gitee.com/imboy-tripartite-deps/qdate_localtime 1.2.1
# qdate - Erlang Date and Timezone Library
# https://github.com/choptastic/qdate
#dep_qdate = git https://gitee.com/imboy-tripartite-deps/qdate master
dep_qdate = hex 0.7.3

# Erlang/OTP 应用程序限制资源访问速率
# 4years https://github.com/lambdaclass/throttle.git
dep_throttle = git https://gitee.com/imboy-tripartite-deps/throttle.git 0.3.0


# ecron 用于 Erlang 的轻量级/高效的类似 cron 的作业调度库。
# https://github.com/zhongwencool/ecron
dep_ecron = git https://gitee.com/imboy-tripartite-deps/ecron.git v1.1.0
# Erlang 的纯函数式和泛型编程
# 6months
dep_datum = git https://gitee.com/imboy-tripartite-deps/datum.git 4.6.1
# Erlang 的断路器
# 4years https://github.com/jlouis/fuse.git
dep_fuse = git https://gitee.com/imboy-tripartite-deps/fuse.git 2.5.0
# Erlang 的简单持久队列
# 3years https://github.com/fogfish/esq.git

# dep_esq = git https://gitee.com/imboy-tripartite-deps/esq.git master
dep_esq = hex 2.0.6

# bbmustache 一个无逻辑的模板。 deps by relx
# https://github.com/soranoba/bbmustache
#dep_bbmustache = git https://gitee.com/imboy-tripartite-deps/bbmustache.git 1.10.0
dep_bbmustache = hex 1.14.1

# Relx 是一个组装 Erlang/OTP 版本的库。给定发布规范和要在其中搜索 OTP 应用程序的目录列表，它将生成发布输出。
# https://github.com/erlware/relx
# dep_relx = git https://gitee.com/imboy-tripartite-deps/relx.git v4.10.0
dep_relx = hex 4.10.0

# lager 日志库
# https://github.com/erlang-lager/lager
dep_lager = git https://gitee.com/imboy-tripartite-deps/lager 3.9.2

# hut 小型的日志处理库
# 2years
dep_hut = git https://gitee.com/imboy-tripartite-deps/hut.git 1.4.0
# Observer CLI 是一个可以被放入任何 Beam 节点的库，用于帮助 DevOps 人员诊断生产节点中的问题
# 8months
dep_observer_cli = git https://gitee.com/imboy-tripartite-deps/observer_cli.git 1.7.4
# Recon 希望成为一套可用于生产环境的工具，用于诊断 Erlang 问题或安全地检查生产环境。
# 4months
dep_recon = git https://gitee.com/imboy-tripartite-deps/recon.git 2.5.4
# fs Native Listener (Mac Windows Linux) 被 sync 依赖
# 8months
dep_fs = git https://gitee.com/imboy-tripartite-deps/fs.git 6.1
# Sync 是一个开发者工具。它会即时重新编译和重新加载您的 Erlang 代码。
# 3months
dep_sync = git https://gitee.com/imboy-tripartite-deps/sync.git v0.4.1
# telemetry 用于指标和仪器的动态调度库。
# 3months
dep_telemetry = git https://gitee.com/imboy-tripartite-deps/telemetry.git v1.2.1
# 8months
dep_system_monitor = git https://gitee.com/imboy-tripartite-deps/system_monitor.git 2.2.6
# erlang tracing debugger
# 4months
dep_redbug = git https://gitee.com/imboy-tripartite-deps/redbug.git 2.0.7
# PropEr：一个受 QuickCheck 启发的 Erlang 基于属性的测试工具
# 6months
dep_proper = git https://gitee.com/imboy-tripartite-deps/proper.git 2.0.7
# 第一个完整实现的 Aho-Corasick 算法的 erlang 版本。
# 6years
dep_aho_corasick = git https://gitee.com/imboy-tripartite-deps/aho-corasick.git master

# erlang fault tolerant service to generate unique identities
# 12months https://github.com/fogfish/uid.git
dep_uid = git https://gitee.com/imboy-tripartite-deps/uid.git master
# Django templates for Erlang
# 8years
dep_erlydtl = git https://gitee.com/imboy-tripartite-deps/erlydtl.git 0.14.0

# Erlang Postgres 客户端和连接池
# https://github.com/erleans/pgo.git
# https://gitee.com/imboy-tripartite-deps/pgo
dep_pgo = git https://gitee.com/imboy-tripartite-deps/pgo main
# dep_pgo = git https://github.com/chiroptical/pgo.git main
dep_pg_types = ln ../../pg_types
# dep_pgo = ln ../../pgo

dep_pooler = git https://gitee.com/imboy-tripartite-deps/pooler.git 1.6.0
#dep_epgsql = git https://gitee.com/imboy-tripartite-deps/epgsql.git 4.7.1
# https://github.com/epgsql/epgsql.git
dep_epgsql = git https://gitee.com/imboy-tripartite-deps/epgsql.git devel
# dep_epgsql = ln ../../epgsql

dep_pure_migrations = git https://gitee.com/imboy-tripartite-deps/erlang-pure-migrations.git leeyi

dep_idna = hex 6.1.1 idna
dep_hackney = git https://gitee.com/imboy-tripartite-deps/hackney.git 1.20.1
dep_guanco = git https://gitee.com/imboy-tripartite-deps/guanco.git main

# dep_simple_captcha = git https://github.com/ziyouchutuwenwu/simple_captcha.git master
# 1years 8commits
dep_simple_captcha = git https://gitee.com/imboy-tripartite-deps/simple_captcha.git master

# SEMVER library for Erlang
# /erlware_commons/src/ec_semver.erl

# dep_nksip = git https://gitee.com/imboy-tripartite-deps/nksip v0.6.1
# dep_nkpacket = git https://gitee.com/imboy-tripartite-deps/nkpacket.git master


dep_vix = git https://gitee.com/imboy-tripartite-deps/vix.git v0.23.1
dep_ersip = git https://gitee.com/imboy-tripartite-deps/ersip.git master
dep_ersip_proxy = git https://gitee.com/imboy-tripartite-deps/ersip_proxy.git master

# 用一句简单的话总结：RTSP发起/终结流媒体、RTP传输流媒体数据 、RTCP对RTP进行控制、同步。
# RTSP体系结位于RTP和RTCP之上（RTCP用于控制传输，RTP用于数据传输），使用TCP或UDP完成数据传输！
# RTSP全称实时流协议（Real Time Streaming Protocol），它是一个网络控制协议，设计用于娱乐、会议系统中控制流媒体服务器。RTSP用于在希望通讯的两端建立并控制媒体会话（session），客户端通过发出VCR-style命令如play、record和pause等来实时控制媒体流。可以参考RTSP 2326 中文版


# RTPS协议设计初衷为多播、无连接的最大努力交付，像是UDP/IP
# https://udds-portal-public.oss-cn-hangzhou.aliyuncs.com/pdf/RTPS规范-v2.2-中文版-pfu.pdf
dep_rtps = git https://gitee.com/imboy-tripartite-deps/rtps.git master
# dep_vice = git https://gitee.com/imboy-tripartite-deps/vice.git 0.1.0

#dep_membrane_rtc_engine = hex 0.17.0
#dep_membrane_rtc_engine_webrtc = hex 0.2.1


# sumo_db旨在简化 erlang 应用程序的数据库访问。
# 它提供了一个非常简单的持久层，能够与不同的数据库交互，同时为您的代码提供一致的 api。
# dep_sumo_db = git https://gitee.com/imboy-tripartite-deps/sumo_db.git main
# dep_erlfmt = git https://github.com/WhatsApp/erlfmt.git main
