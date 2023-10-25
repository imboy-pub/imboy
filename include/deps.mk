# 下列依赖放到 https://gitee.com/imboy-tripartite-deps 只是为了在中国加载代码快速一些
# The following dependencies are placed at https://gitee.com/imboy-tripartite-deps just to make loading code in China faster
# 特此声明：下列依赖，被人未改动过原作者源码
# Hereby declare: the following dependencies have not been modified by the original author source code

# https://erlang.mk/guide/deps.html
# Ranch is a socket acceptor pool for TCP protocols.
dep_ranch = git https://gitee.com/imboy-tripartite-deps/ranch.git 2.1.0
dep_cowlib = git https://gitee.com/imboy-tripartite-deps/cowlib 2.12.1
dep_cowboy = git https://gitee.com/imboy-tripartite-deps/cowboy 2.10.0

# gen_smtp Email服务端、客户端
dep_gen_smtp = git https://gitee.com/imboy-tripartite-deps/gen_smtp 1.2.0
# goldrush 提供了快速的事件流处理
dep_goldrush = git https://gitee.com/imboy-tripartite-deps/goldrush 0.2.0
dep_hashids = git https://gitee.com/imboy-tripartite-deps/hashids-erlang 1.0.5
# jsone An Erlang library for encoding, decoding JSON data.
dep_jsone = git https://gitee.com/imboy-tripartite-deps/jsone 1.8.0
# jsx an erlang application for consuming, producing and manipulating json
dep_jsx = git https://gitee.com/imboy-tripartite-deps/jsx.git v3.1.0
# jwerl JWT library
dep_jwerl = git https://gitee.com/imboy-tripartite-deps/jwerl 1.2.0
# dep_jwerl = git https://gitee.com/mirrors_emqx/jwerl 1.1.1


#dep_khepri = hex 0.7.0
dep_depcache = git https://gitee.com/imboy-tripartite-deps/depcache.git master
# syn 全局进程注册表和进程组管理器，能够自动管理动态集群（添加/删除节点）并从网络分裂中恢复。
# Syn 是 Erlang/OTP global的 registry 和 pg模块的替代品。Syn 实现了 强最终一致性。
dep_syn = git https://gitee.com/imboy-tripartite-deps/syn.git 3.3.0

# bbmustache 一个无逻辑的模板。 deps by relx
dep_bbmustache = git https://gitee.com/imboy-tripartite-deps/bbmustache.git v1.12.2
# erlware_commons 为与 Erlang 一起分发的 stdlib 应用程序的扩展，被 qdate 依赖
dep_erlware_commons = git https://gitee.com/imboy-tripartite-deps/erlware_commons v1.6.0
# qdate_localtime 这是 erlang_localtime 的一个分支 ，专门针对与 qdate的兼容性进行了修改。两者大多兼容，但多年来出现了一些分歧。
# 被 qdate 依赖
dep_qdate_localtime = git https://gitee.com/imboy-tripartite-deps/qdate_localtime 1.2.1
# qdate - Erlang Date and Timezone Library
dep_qdate = git https://gitee.com/imboy-tripartite-deps/qdate master
dep_throttle = git https://gitee.com/imboy-tripartite-deps/throttle.git 0.3.0


# ecron 用于 Erlang 的轻量级/高效的类似 cron 的作业调度库。
dep_ecron = git https://gitee.com/imboy-tripartite-deps/ecron.git v0.6.1
# Erlang 的纯函数式和泛型编程
dep_datum = git https://gitee.com/imboy-tripartite-deps/datum.git 4.6.1
# Erlang 的简单持久队列
# dep_esq = git https://gitee.com/imboy-tripartite-deps/esq.git master
dep_esq = hex 2.0.6


# Relx 是一个组装 Erlang/OTP 版本的库。给定发布规范和要在其中搜索 OTP 应用程序的目录列表，它将生成发布输出。
dep_relx = git https://gitee.com/imboy-tripartite-deps/relx.git v4.8.0
# hut 小型的日志处理库
dep_hut = git https://gitee.com/imboy-tripartite-deps/hut.git 1.4.0
# lager 日志库
dep_lager = git https://gitee.com/imboy-tripartite-deps/lager 3.9.2
# Observer CLI 是一个可以被放入任何 Beam 节点的库，用于帮助 DevOps 人员诊断生产节点中的问题
dep_observer_cli = git https://gitee.com/imboy-tripartite-deps/observer_cli.git 1.7.4
# Recon 希望成为一套可用于生产环境的工具，用于诊断 Erlang 问题或安全地检查生产环境。
dep_recon = git https://gitee.com/imboy-tripartite-deps/recon.git 2.5.4
# fs Native Listener (Mac Windows Linux) 被 sync 依赖
dep_fs = git https://gitee.com/imboy-tripartite-deps/fs.git 6.1
# Sync 是一个开发者工具。它会即时重新编译和重新加载您的 Erlang 代码。
dep_sync = git https://gitee.com/imboy-tripartite-deps/sync.git v0.4.1
# telemetry 用于指标和仪器的动态调度库。
dep_telemetry = git https://gitee.com/imboy-tripartite-deps/telemetry.git v1.2.1
# erlang tracing debugger
dep_redbug = git https://gitee.com/imboy-tripartite-deps/redbug.git 2.0.7
# PropEr：一个受 QuickCheck 启发的 Erlang 基于属性的测试工具
dep_proper = git https://gitee.com/imboy-tripartite-deps/proper.git 2.0.7


# dep_mysql = git https://gitee.com/imboy-tripartite-deps/mysql-otp 1.7.0
# dep_mysql = hex 1.8.0
# poolboy A hunky Erlang worker pool factory
#dep_poolboy = git https://gitee.com/imboy-tripartite-deps/poolboy 1.5.2

dep_epgsql = git https://gitee.com/imboy-tripartite-deps/epgsql.git 4.7.1
dep_pooler = git https://gitee.com/imboy-tripartite-deps/pooler.git 1.6.0

# depcache is an in-memory caching server for Erlang with dependency checks, cache expiration and local in process memoization of lookups.

# dep_nksip = git https://gitee.com/imboy-tripartite-deps/nksip v0.6.1
# dep_nkpacket = git https://gitee.com/imboy-tripartite-deps/nkpacket.git master

dep_ersip = git https://gitee.com/imboy-tripartite-deps/ersip.git master
dep_ersip_proxy = git https://gitee.com/imboy-tripartite-deps/ersip_proxy.git master

# 用一句简单的话总结：RTSP发起/终结流媒体、RTP传输流媒体数据 、RTCP对RTP进行控制、同步。
# RTSP体系结位于RTP和RTCP之上（RTCP用于控制传输，RTP用于数据传输），使用TCP或UDP完成数据传输！
# RTSP全称实时流协议（Real Time Streaming Protocol），它是一个网络控制协议，设计用于娱乐、会议系统中控制流媒体服务器。RTSP用于在希望通讯的两端建立并控制媒体会话（session），客户端通过发出VCR-style命令如play、record和pause等来实时控制媒体流。可以参考RTSP 2326 中文版


# RTPS协议设计初衷为多播、无连接的最大努力交付，像是UDP/IP
# https://udds-portal-public.oss-cn-hangzhou.aliyuncs.com/pdf/RTPS规范-v2.2-中文版-pfu.pdf
dep_rtps = git https://gitee.com/imboy-tripartite-deps/rtps.git master
# dep_vice = git https://gitee.com/imboy-tripartite-deps/vice.git 0.1.0

dep_membrane_rtc_engine = hex 0.17.0
dep_membrane_rtc_engine_webrtc = hex 0.2.1


# sumo_db旨在简化 erlang 应用程序的数据库访问。
# 它提供了一个非常简单的持久层，能够与不同的数据库交互，同时为您的代码提供一致的 api。
# dep_sumo_db = git https://gitee.com/imboy-tripartite-deps/sumo_db.git main
# dep_erlfmt = git https://github.com/WhatsApp/erlfmt.git main
