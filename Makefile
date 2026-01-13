PROJECT = imboy
PROJECT_DESCRIPTION = 基于Erlang的Cowboy、Flutter和PostgreSQL计算的一款聊天软件。
PROJECT_VERSION = 0.7.3
export PROJECT_VERSION

# usage: make TARGET IMBOYENV=...
#    make run IMBOYENV=local
#    make rel IMBOYENV=local
RELX_CONFIG = $(CURDIR)/relx$(IMBOYENV).config

# APPS_DIR ?= $(CURDIR)/app
# DEPS_DIR  = plugin/*/

include include/deps.mk

# erlang.mk会保证 DEPS依赖的包能运行在shell、run、tests命令的时候
DEPS = ranch cowlib cowboy gun

# Type system and utility libraries
DEPS += erlware_commons

DEPS += jwerl hashids_erlang gen_smtp throttle
DEPS += qdate qdate_localtime
# goldrush provides fast event flow processing
DEPS += goldrush
# Erlang functional programming and generics
DEPS += datum jsone
# DEPS += mysql poolboy
DEPS += epgsql pooler
#DEPS += ra
# DEPS += pg_types pgo
DEPS += pure_migrations
# DEPS += idna
# DEPS += hackney jiffy
DEPS += depcache
# DEPS += khepri
DEPS += syn
# DEPS += hnc_csv
DEPS += fuse
DEPS += ecron
DEPS += aho_corasick
DEPS += uid

# Operations and diagnostics libraries
DEPS += telemetry lager observer_cli recon redbug
DEPS += simple_captcha
DEPS += erlydtl
DEPS += sync
# DEPS += eimp
# DEPS += guanco
# DEPS += rebar3_appup_plugin
# DEPS += vix
# DEPS += nksip
# DEPS += ersip rtps

# DEPS += esq


#LOCAL_DEPS 本地依赖比较容易理解，就是otp内部项目的依赖
LOCAL_DEPS = mnesia sasl ssl inets eunit crypto public_key

# 如果依赖包不用在erlang运行的时候跑的话，那就把它设置为BUILD_DEPS就行了，这样就只有构建的时候会用到
BUILD_DEPS = relx

DEP_PLUGINS = cowboy

# 专为测试用的TEST_DEPS,只有当测试的时候才会运行
TEST_DEPS += meck

SP = 4

# http://erlang.org/doc/apps/edoc/chapter.html#Introduction
DOC_DEPS = edown
EDOC_OPTS = {doclet, edown_doclet}

# 生成文档的时候会被用到的依赖项
# DOC_DEPS =


include erlang.mk
include include/tpl.mk
include include/cli.mk

APP_VERSION = $(shell cat $(RELX_OUTPUT_DIR)/$(RELX_REL_NAME)/version)

# Dialyzer 配置 - 在 erlang.mk 之后覆盖，确保生效
# 注意：暂时移除 -Werror_handling，因为项目中有较多类型规范问题需要逐步修复
DIALYZER_OPTS = -Wunmatched_returns --plt $(DIALYZER_PLT) -I $(CURDIR)/include

# Compile flags
ERLC_COMPILE_OPTS = +'{parse_transform, lager_transform}' +nowarn_unused_function

# Append these settings
ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)

# EUnit configuration - 添加超时和详细输出
EUNIT_OPTS ?= verbose
EUNIT_OPTS += {timeout, 30}

# EUnit 测试配置
# 设置超时避免卡住
# 支持通过 CONFIG 参数指定配置文件，默认使用 sys.local.config
# 使用示例:
#   make eunit                           # 使用默认 config/sys.local.config
#   make eunit CONFIG=sys.local.config   # 使用指定的配置文件
#   make eunit CONFIG=sys.dev.config     # 使用 dev 配置
EUNIT_CONFIG ?= config/sys.config
EUNIT_ERL_OPTS += -config $(EUNIT_CONFIG)
# 在测试环境中设置 env 标记
EUNIT_ERL_OPTS += -eval 'application:set_env(imboy, env, test)'

# 覆盖 EUNIT_MODS - 只运行测试模块，不运行源码模块
# 这是解决 make eunit 卡住的关键
EUNIT_TEST_MODS = $(notdir $(basename $(call core_find,$(TEST_DIR)/,*_tests.erl)))
EUNIT_EBIN_MODS =
EUNIT_MODS = $(foreach mod,$(EUNIT_EBIN_MODS) $(filter-out \
	$(patsubst %,%_tests,$(EUNIT_EBIN_MODS)),$(EUNIT_TEST_MODS)),'$(mod)')

# 警告数量限制（用于 CI）
DIALYZER_WARNINGS ?= 50

# 使用命令:
#   make dialyze_build_plt  - 首次构建 PLT
#   make dialyze           - 运行 Dialyzer 分析

# ===================================================================
# Common Test 配置（erlang.mk 原生支持）
# ===================================================================
#
# erlang.mk 原生支持 Common Test，只需：
# 1. 在 test/ 目录下创建 *_SUITE.erl 文件
# 2. 运行 make ct 或 make ct-suite_name
#
# 使用方式:
#   make ct                          # 运行所有 Common Test suites
#   make ct-msg_ack_logic            # 运行特定 suite
#   make ct-msg_ack_logic t=c2c_ack  # 运行特定测试用例
#   make tests                       # 运行所有测试（EUnit + CT）
#
# CT 配置选项:
CT_OPTS ?=
CT_LOGS_DIR ?= logs/ct
