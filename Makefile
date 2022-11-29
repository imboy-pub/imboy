PROJECT = imboy
PROJECT_DESCRIPTION = 基于cowboy的一款即时聊天软件
PROJECT_VERSION = 1.0.0

#LOCAL_DEPS 本地依赖比较容易理解，就是otp内部项目的依赖
LOCAL_DEPS = ssl mnesia
# erlang.mk会保证 DEPS依赖的包能运行在shell、run、tests命令的时候
DEPS = goldrush lager poolboy mysql jsone ranch cowlib cowboy jsx jwerl hashids depcache recon observer_cli gen_smtp syn sync qdate

# 如果依赖包不用在erlang运行的时候跑的话，那就把它设置为BUILD_DEPS就行了，这样就只有构建的时候会用到
BUILD_DEPS = elvis_mk bbmustache relx

DEP_PLUGINS = cowboy elvis_mk

# 专为测试用的TEST_DEPS,只有当测试的时候才会运行
# TEST_DEPS = sync

# http://erlang.org/doc/apps/edoc/chapter.html#Introduction
DOC_DEPS = edown
EDOC_OPTS = {doclet, edown_doclet}

ifeq ($(IMBOYENV),prod)
	RELX_CONFIG = $(CURDIR)/relx.prod.config
else ifeq ($(IMBOYENV),test)
	RELX_CONFIG = $(CURDIR)/relx.test.config
else ifeq ($(IMBOYENV),dev)
	RELX_CONFIG = $(CURDIR)/relx.dev.config
else ifeq ($(IMBOYENV),local)
	RELX_CONFIG = $(CURDIR)/relx.local.config
	ERLC_COMPILE_OPTS = +'{parse_transform, lager_transform, debug_info}'
else
	RELX_CONFIG = $(CURDIR)/relx.config
endif
dep_cowboy_commit = 2.9.0
dep_lager_commit = 3.9.2

# 生成文档的时候会被用到的依赖项
# DOC_DEPS =
# 用户执行make shell命令的时候会用到的依赖
# SHELL_DEPS =

# dep_poolboy = git https://github.com/devinus/poolboy 1.5.2
# dep_mysql = git https://github.com/mysql-otp/mysql-otp 1.7.0
# dep_jsone = git https://github.com/sile/jsone 1.7.0
# dep_hashids = git https://github.com/snaiper80/hashids-erlang 1.0.5
# dep_jwerl = git https://github.com/G-Corp/jwerl 1.1.0
# dep_gen_smtp = git https://github.com/gen-smtp/gen_smtp 1.0.0

# 下列依赖放到 https://gitee.com/imboy-tripartite-deps 只是为了在中国加载代码快速一些
# The following dependencies are placed at https://gitee.com/imboy-tripartite-deps just to make loading code in China faster
# 特此声明：下列依赖，被人未改动过原作者源码
# Hereby declare: the following dependencies have not been modified by the original author source code

# https://erlang.mk/guide/deps.html
dep_cowboy = git https://gitee.com/imboy-tripartite-deps/cowboy 2.9.0
dep_cowlib = git https://gitee.com/imboy-tripartite-deps/cowlib 2.11.0
# Ranch is a socket acceptor pool for TCP protocols.
dep_ranch = git https://gitee.com/imboy-tripartite-deps/ranch.git 1.8.0
# elvis 代码格式检查
dep_elvis_mk = git https://gitee.com/imboy-tripartite-deps/elvis.mk 1.0.0
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
dep_mysql = git https://gitee.com/imboy-tripartite-deps/mysql-otp 1.7.0
# poolboy A hunky Erlang worker pool factory
dep_poolboy = git https://gitee.com/imboy-tripartite-deps/poolboy 1.5.2
# Observer CLI 是一个可以被放入任何 Beam 节点的库，用于帮助 DevOps 人员诊断生产节点中的问题
dep_observer_cli = git https://gitee.com/imboy-tripartite-deps/observer_cli.git 1.7.3
# Recon 希望成为一套可用于生产环境的工具，用于诊断 Erlang 问题或安全地检查生产环境。
dep_recon = git https://gitee.com/imboy-tripartite-deps/recon.git 2.5.1
# depcache is an in-memory caching server for Erlang with dependency checks, cache expiration and local in process memoization of lookups.
dep_depcache = git https://gitee.com/imboy-tripartite-deps/depcache.git master
# syn 全局进程注册表和进程组管理器，能够自动管理动态集群（添加/删除节点）并从网络分裂中恢复。
# Syn 是 Erlang/OTP global的 registry 和 pg模块的替代品。Syn 实现了 强最终一致性。
dep_syn = git https://gitee.com/imboy-tripartite-deps/syn.git 3.3.0
# Relx 是一个组装 Erlang/OTP 版本的库。给定发布规范和要在其中搜索 OTP 应用程序的目录列表，它将生成发布输出。
dep_relx = git https://gitee.com/imboy-tripartite-deps/relx.git v4.7.0
# bbmustache 一个无逻辑的模板。 deps by relx
dep_bbmustache = git https://gitee.com/imboy-tripartite-deps/bbmustache.git v1.12.2
# qdate - Erlang Date and Timezone Library
dep_qdate = git https://gitee.com/imboy-tripartite-deps/qdate.git master
# Sync 是一个开发者工具。它会即时重新编译和重新加载您的 Erlang 代码。
dep_sync = git https://gitee.com/imboy-tripartite-deps/sync.git master
# sumo_db旨在简化 erlang 应用程序的数据库访问。
# 它提供了一个非常简单的持久层，能够与不同的数据库交互，同时为您的代码提供一致的 api。
# dep_sumo_db = git https://gitee.com/imboy-tripartite-deps/sumo_db.git main
# dep_erlfmt = git https://github.com/WhatsApp/erlfmt.git main

SP = 4

include erlang.mk
include include/tpl.mk
include include/cli.mk

APP_VERSION = $(shell cat $(RELX_OUTPUT_DIR)/$(RELX_REL_NAME)/version)

# Compile flags
ERLC_COMPILE_OPTS = +'{parse_transform, lager_transform}'

# Append these settings
ERLC_OPTS += $(ERLC_COMPILE_OPTS)
ERLC_OPTS += +debug_info
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)
