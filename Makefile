PROJECT = imboy
PROJECT_DESCRIPTION = 基于cowboy的一款即时聊天软件
PROJECT_VERSION = 1.0.0

#LOCAL_DEPS 本地依赖比较容易理解，就是otp内部项目的依赖
LOCAL_DEPS = ssl mnesia
# erlang.mk会保证 DEPS依赖的包能运行在shell、run、tests命令的时候
DEPS = goldrush lager poolboy mysql jsone ranch cowlib cowboy jsx jwerl hashids depcache recon observer_cli gen_smtp syn sync

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
dep_elvis_mk = git https://gitee.com/imboy-tripartite-deps/elvis.mk 1.0.0
dep_gen_smtp = git https://gitee.com/imboy-tripartite-deps/gen_smtp 1.2.0
dep_goldrush = git https://gitee.com/imboy-tripartite-deps/goldrush 0.2.0
dep_hashids = git https://gitee.com/imboy-tripartite-deps/hashids-erlang 1.0.5
dep_hut = git https://gitee.com/imboy-tripartite-deps/hut.git 1.4.0
dep_jsone = git https://gitee.com/imboy-tripartite-deps/jsone 1.7.0
dep_jsx = git https://gitee.com/imboy-tripartite-deps/jsx.git 2.9.0
dep_jwerl = git https://gitee.com/imboy-tripartite-deps/jwerl 1.2.0
# dep_jwerl = git https://gitee.com/mirrors_emqx/jwerl 1.1.1
dep_lager = git https://gitee.com/imboy-tripartite-deps/lager 3.9.2
dep_mysql = git https://gitee.com/imboy-tripartite-deps/mysql-otp 1.7.0
dep_poolboy = git https://gitee.com/imboy-tripartite-deps/poolboy 1.5.2
dep_ranch = git https://gitee.com/imboy-tripartite-deps/ranch.git 1.8.0
dep_observer_cli = git https://gitee.com/imboy-tripartite-deps/observer_cli.git 1.7.3
dep_recon = git https://gitee.com/imboy-tripartite-deps/recon.git 2.5.1
dep_depcache = git https://gitee.com/imboy-tripartite-deps/depcache.git master
dep_syn = git https://gitee.com/imboy-tripartite-deps/syn.git 3.3.0
dep_bbmustache = git https://gitee.com/imboy-tripartite-deps/bbmustache.git v1.12.2 # deps by relx
dep_relx = git https://gitee.com/imboy-tripartite-deps/relx.git v4.7.0
dep_sync = git https://gitee.com/imboy-tripartite-deps/sync.git master
#dep_sumo_db = git https://gitee.com/imboy-tripartite-deps/sumo_db.git main
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
