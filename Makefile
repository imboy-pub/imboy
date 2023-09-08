PROJECT = imboy
PROJECT_DESCRIPTION = 基于cowboy的一款即时聊天软件
PROJECT_VERSION = 0.1.12

# usage: make TARGET IMBOYENV=...
#    make run IMBOYENV=local
#    make rel IMBOYENV=local
RELX_CONFIG = $(CURDIR)/relx_$(IMBOYENV).config

# APPS_DIR ?= $(CURDIR)/app
# DEPS_DIR  = plugin/*/

include include/deps.mk

#LOCAL_DEPS 本地依赖比较容易理解，就是otp内部项目的依赖
LOCAL_DEPS = kernel stdlib mnesia sasl ssl inets

# LOCAL_DEPS += erlmedia

# erlang.mk会保证 DEPS依赖的包能运行在shell、run、tests命令的时候
DEPS = telemetry lager jsone ranch cowlib cowboy
DEPS += jsx jwerl hashids recon observer_cli gen_smtp throttle
DEPS += qdate qdate_localtime
# goldrush 提供了快速的事件流处理
DEPS += goldrush
# Erlang 的纯函数式和泛型编程
DEPS += datum
# DEPS += mysql poolboy
DEPS += epgsql pooler
DEPS += depcache
DEPS += syn
DEPS += ecron
DEPS += sync
DEPS += ersip rtps

# DEPS += esq
# DEPS += khepri

# 如果依赖包不用在erlang运行的时候跑的话，那就把它设置为BUILD_DEPS就行了，这样就只有构建的时候会用到
BUILD_DEPS = relx

DEP_PLUGINS = cowboy

# 专为测试用的TEST_DEPS,只有当测试的时候才会运行
# TEST_DEPS = sync

SP = 4

# http://erlang.org/doc/apps/edoc/chapter.html#Introduction
DOC_DEPS = edown
EDOC_OPTS = {doclet, edown_doclet}

# 生成文档的时候会被用到的依赖项
# DOC_DEPS =
# 用户执行make shell命令的时候会用到的依赖
# SHELL_DEPS =

include erlang.mk
include include/tpl.mk
include include/cli.mk

APP_VERSION = $(shell cat $(RELX_OUTPUT_DIR)/$(RELX_REL_NAME)/version)

# Compile flags
ERLC_COMPILE_OPTS = +'{parse_transform, lager_transform}'

# Append these settings
ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)
