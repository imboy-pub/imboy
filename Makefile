PROJECT = imboy
PROJECT_DESCRIPTION = 基于cowboy的一款即时聊天软件
PROJECT_VERSION = 0.1.0

#LOCAL_DEPS 本地依赖比较容易理解，就是otp内部项目的依赖
LOCAL_DEPS = ssl mnesia
# erlang.mk会保证 DEPS依赖的包能运行在shell、run、tests命令的时候
DEPS = lager poolboy mysql jsx cowboy jwerl hashids eid
# 如果依赖包不用在erlang运行的时候跑的话，那就把它设置为BUILD_DEPS就行了，这样就只有构建的时候会用到
# BUILD_DEPS
BUILD_DEPS = reload_mk

# 专为测试用的TEST_DEPS,只有当测试的时候才会运行
# TEST_DEPS = sync

# http://erlang.org/doc/apps/edoc/chapter.html#Introduction
DOC_DEPS = edown
EDOC_OPTS = {doclet, edown_doclet}

# Compile flags
ERLC_COMPILE_OPTS = +'{parse_transform, lager_transform}'

ifeq ($(ENV),prod)
	RELX_CONFIG = $(CURDIR)/relx.prod.config
else ifeq ($(ENV),test)
	RELX_CONFIG = $(CURDIR)/relx.test.config
else ifeq ($(ENV),dev)
	RELX_CONFIG = $(CURDIR)/relx.dev.config
else ifeq ($(ENV),local)
	RELX_CONFIG = $(CURDIR)/relx.local.config
	ERLC_COMPILE_OPTS = +'{parse_transform, lager_transform, debug_info}'
else
	RELX_CONFIG = $(CURDIR)/relx.config
endif
dep_cowboy_commit = 2.9.0
dep_lager_commit = 3.6.8

# 生成文档的时候会被用到的依赖项
# DOC_DEPS =
# 用户执行make shell命令的时候会用到的依赖
# SHELL_DEPS =

dep_poolboy = git https://github.com/devinus/poolboy 1.5.2
dep_mysql = git https://github.com/mysql-otp/mysql-otp 1.7.0
dep_jsx = git https://github.com/talentdeficit/jsx v3.1.0
dep_hashids = git https://github.com/snaiper80/hashids-erlang 1.0.5
dep_jwerl = git https://github.com/G-Corp/jwerl 1.1.0
dep_eid = git https://github.com/jur0/eid.git master

SP = 4
DEP_PLUGINS = cowboy reload_mk
RELOAD_MK_WATCH_DIRS = src templates include

include erlang.mk
include include/tpl.mk

APP_VERSION = $(shell cat $(RELX_OUTPUT_DIR)/$(RELX_REL_NAME)/version)

# Append these settings
ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)
