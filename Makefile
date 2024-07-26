PROJECT = imboy
PROJECT_DESCRIPTION = 基于Erlang的Cowboy、Flutter和PostgreSQL计算的一款聊天软件。
PROJECT_VERSION = 0.4.1

# usage: make TARGET IMBOYENV=...
#    make run IMBOYENV=local
#    make rel IMBOYENV=local
RELX_CONFIG = $(CURDIR)/relx$(IMBOYENV).config

# APPS_DIR ?= $(CURDIR)/app
# DEPS_DIR  = plugin/*/

include include/deps.mk

# erlang.mk会保证 DEPS依赖的包能运行在shell、run、tests命令的时候
DEPS =  ranch cowlib cowboy
DEPS += jwerl hashids_erlang gen_smtp throttle
DEPS += qdate qdate_localtime
# goldrush 提供了快速的事件流处理
DEPS += goldrush
# Erlang 的纯函数式和泛型编程
DEPS += datum jsone jsx
# DEPS += mysql poolboy
DEPS += epgsql pooler pure_migrations
# DEPS += idna hackney
DEPS += depcache
# DEPS += khepri
DEPS += syn
DEPS += fuse
DEPS += ecron
DEPS += aho_corasick
DEPS += uid

# 运维诊断类型的库
DEPS += telemetry lager observer_cli recon redbug
DEPS += sync
DEPS += simple_captcha
DEPS += erlydtl
# DEPS += rebar3_appup_plugin
# DEPS += vix
# DEPS += nksip
# DEPS += ersip rtps

# DEPS += esq


#LOCAL_DEPS 本地依赖比较容易理解，就是otp内部项目的依赖
LOCAL_DEPS = kernel stdlib mnesia sasl ssl inets
LOCAL_DEPS += imlib
LOCAL_DEPS += imds
LOCAL_DEPS += imrepo
LOCAL_DEPS += imapi
LOCAL_DEPS += imadm
LOCAL_DEPS += imcron

# LOCAL_DEPS += imsos
# LOCAL_DEPS += erlmedia


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
