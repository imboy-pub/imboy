PROJECT = imboy
PROJECT_DESCRIPTION = 基于Erlang的Cowboy、Flutter和PostgreSQL计算的一款聊天软件。
PROJECT_VERSION = 1.0.0-rc.1
export PROJECT_VERSION

# 单一 release 配置（5 → 1）：
#   * relx.config 始终指向 config/sys.runtime.config（由下方逻辑生成）。
#   * IMBOYENV={env} 时，自动匹配 config/sys.{env}.config（如 dev → sys.dev.config）；
#     不存在则回退 sys.config。
#   * 环境变量 IMBOY_* 仍由 imboy_env.erl 在运行时覆盖（优先级更高）。
#   * 出 tarball：make rel RELX_DEV_MODE=false RELX_INCLUDE_ERTS=true
RELX_CONFIG = $(CURDIR)/relx.config

# --- 运行时配置生成（在 erlang.mk include 之前执行） ---
# IMBOYENV 匹配 config/sys.{env}.config；不存在则回退 sys.config。
# relx 构建时读取 sys.runtime.config。
_SYS_RUNTIME_SRC := config/sys.config
ifneq ($(IMBOYENV),)
  ifneq ($(wildcard config/sys.$(IMBOYENV).config),)
    _SYS_RUNTIME_SRC := config/sys.$(IMBOYENV).config
  endif
endif
$(shell mkdir -p config && cp $(_SYS_RUNTIME_SRC) config/sys.runtime.config)

# APPS_DIR ?= $(CURDIR)/app
# DEPS_DIR  = plugin/*/

include include/deps.mk

# erlang.mk会保证 DEPS依赖的包能运行在shell、run、tests命令的时候
DEPS = ranch cowlib cowboy gun

# Type system and utility libraries
DEPS += erlware_commons

DEPS += jwerl gen_smtp throttle
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
BUILD_DEPS = relx gpb

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

# Override erlang.mk's default gpb compile options
# Add maps support, strings_as_binaries, and type_specs for better Erlang integration
define compile_proto.erl
	[begin
		gpb_compile:file(F, [
			{i, "src"},
			{include_as_lib, true},
			{module_name_suffix, "_pb"},
			{o_hrl, "./include"},
			{o_erl, "./src"},
			maps,
			strings_as_binaries,
			{type_specs, true},
			{maps_unset_optional, omitted},
			{maps_oneof, flat}
		])
	end || F <- string:tokens("$1", " ")],
	halt().
endef
include include/tpl.mk
include include/cli.mk

APP_VERSION = $(shell cat $(RELX_OUTPUT_DIR)/$(RELX_REL_NAME)/version)

# Dialyzer 配置 - 在 erlang.mk 之后覆盖，确保生效
# 注意：暂时移除 -Werror_handling，因为项目中有较多类型规范问题需要逐步修复
DIALYZER_DIRS = -r ebin deps
DIALYZER_OPTS = -Wunmatched_returns --plt $(DIALYZER_PLT) -I $(CURDIR)/include $(DIALYZER_DIRS)

# Compile flags
ERLC_COMPILE_OPTS = +'{parse_transform, lager_transform}' +nowarn_unused_function

# Append these settings
ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)

# EUnit configuration - 添加超时和详细输出
EUNIT_OPTS ?= verbose
EUNIT_OPTS += {timeout, 30}

# EUnit 测试配置
# 支持通过 EUNIT_CONFIG 参数指定配置文件，默认使用 config/sys.config
# 注意：erl 的 -config 参数既支持无后缀名，也支持 *.config，下面统一做兼容处理
# 使用示例:
#   make eunit                                        # 使用默认 config/sys.config
#   make eunit EUNIT_CONFIG=config/sys.local.config   # 使用 local 配置
#   make eunit EUNIT_CONFIG=config/sys.dev.config     # 使用 dev 配置
EUNIT_CONFIG ?= config/sys.config
EUNIT_CONFIG_BASE = $(patsubst %.config,%,$(EUNIT_CONFIG))
TEST_HTTP_PORT ?= 19800
# Override erlang.mk's -pa order: put ebin before test so stubs (config_ds)
# with debug_info take precedence over test-compiled beams without it.
EUNIT_ERL_OPTS := $(filter-out -pa %,$(EUNIT_ERL_OPTS)) -pa $(CURDIR)/ebin -pa $(TEST_DIR)
EUNIT_ERL_OPTS += -config $(EUNIT_CONFIG_BASE)
# eunit_runner 在 setup 阶段会读取 application:get_env，需要预先 load 应用
EUNIT_ERL_OPTS += -eval 'application:load(imboy)'
# 在测试环境中设置 env 标记
EUNIT_ERL_OPTS += -eval 'application:set_env(imboy, env, test)'
# 避免与开发中常驻的本地后端实例争用默认 9800 端口
EUNIT_ERL_OPTS += -eval 'application:set_env(imboy, http_port, $(TEST_HTTP_PORT))'

# 覆盖 EUNIT_MODS - 只运行测试模块，不运行源码模块
# 这是解决 make eunit 卡住的关键
EUNIT_TEST_MODS = $(notdir $(basename $(call core_find,$(TEST_DIR)/,*_tests.erl)))
EUNIT_EBIN_MODS =
EUNIT_MODS = $(foreach mod,$(EUNIT_EBIN_MODS) $(filter-out \
	$(patsubst %,%_tests,$(EUNIT_EBIN_MODS)),$(EUNIT_TEST_MODS)),'$(mod)')

# Full-suite EUnit runs can deadlock when tests that start the app overlap
# with modules that globally meck shared dependencies like elib_pg. Run the
# suite module-by-module by default, while keeping `make eunit t=module_tests`
# on erlang.mk's native single-module path.
# Compile test stubs that must be in ebin/ for meck to work
# (meck:new requires the module to be loaded or exist as a beam file)
test-stubs:
	$(verbose) erlc +debug_info -o ebin test/common/config_ds.erl
	$(verbose) erlc +debug_info -o test test/common/config_ds.erl
	$(verbose) erlc +debug_info -I include -pa ebin -o test test/lib/imboy_plugin_dummy.erl

ifndef t
.PHONY: eunit
eunit: test-build test-stubs cover-data-dir
ifneq ($(wildcard src/ $(TEST_DIR)),)
	@set -e; \
	failures=""; \
	ret=0; \
	for mod in $(EUNIT_MODS); do \
		echo " GEN    eunit $$mod"; \
		if ! $(MAKE) --no-print-directory t=$$mod eunit EUNIT_CONFIG="$(EUNIT_CONFIG)"; then \
			failures="$$failures $$mod"; \
			ret=1; \
		fi; \
	done; \
	if [ $$ret -ne 0 ]; then \
		echo "EUNIT failed modules:$$failures"; \
		exit $$ret; \
	fi
endif
else
# Single-module eunit (make eunit t=module): also compile stubs after test-build
.PHONY: eunit
eunit: test-build test-stubs cover-data-dir
endif

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
CT_CONFIG ?= config/sys.config
CT_CONFIG_BASE = $(patsubst %.config,%,$(CT_CONFIG))
CT_CONFIG_BASE_ABS = $(abspath $(CT_CONFIG_BASE))
CT_ERL_ARGS = -config $(CT_CONFIG_BASE_ABS) -eval 'application:load(imboy)' -eval 'application:set_env(imboy, env, test)' -eval 'application:set_env(imboy, http_port, $(TEST_HTTP_PORT))' -eval 'application:set_env(imboy, dsync_enabled, false)'
CT_OPTS += -erl_args "$(CT_ERL_ARGS)"

FEATURE_SMOKE_BASE_URL ?=
FEATURE_SMOKE_PUBLIC_PATH ?= /v1/app/features
FEATURE_SMOKE_ADMIN_PATH ?= /adm/admin/config/features
FEATURE_SMOKE_ADMIN_HEADER ?=
FEATURE_SMOKE_FORBIDDEN_HEADER ?=
FEATURE_SMOKE_EXPECTS ?=
FEATURE_SMOKE_TIMEOUT ?= 15
FEATURE_SMOKE_INSECURE ?= 0
FEATURE_SMOKE_SHOW_BODY ?= 0

.PHONY: feature-smoke
feature-smoke:
	@if [ -z "$(strip $(FEATURE_SMOKE_BASE_URL))" ]; then \
		echo "FEATURE_SMOKE_BASE_URL is required."; \
		echo "Example:"; \
		echo "  make feature-smoke FEATURE_SMOKE_BASE_URL=https://dev.imboy.pub FEATURE_SMOKE_EXPECTS='core=true moment=false'"; \
		exit 1; \
	fi
	@set -e; \
		cmd="bash ./script/run_feature_flag_smoke.sh --base-url '$(FEATURE_SMOKE_BASE_URL)' --public-path '$(FEATURE_SMOKE_PUBLIC_PATH)' --admin-path '$(FEATURE_SMOKE_ADMIN_PATH)' --timeout '$(FEATURE_SMOKE_TIMEOUT)'"; \
		if [ "$(FEATURE_SMOKE_INSECURE)" = "1" ]; then cmd="$$cmd --insecure"; fi; \
		if [ "$(FEATURE_SMOKE_SHOW_BODY)" = "1" ]; then cmd="$$cmd --show-body"; fi; \
		if [ -n "$(strip $(FEATURE_SMOKE_ADMIN_HEADER))" ]; then cmd="$$cmd --admin-header '$(FEATURE_SMOKE_ADMIN_HEADER)'"; fi; \
		if [ -n "$(strip $(FEATURE_SMOKE_FORBIDDEN_HEADER))" ]; then cmd="$$cmd --forbidden-header '$(FEATURE_SMOKE_FORBIDDEN_HEADER)'"; fi; \
		for item in $(FEATURE_SMOKE_EXPECTS); do cmd="$$cmd --expect $$item"; done; \
		echo "$$cmd"; \
		eval "$$cmd"

# -----------------------------------------------------------------------------
# Tier-0 冒烟：C2C 端到端（RPC 发 + psql 校验 msg_store）
# 用法：
#   make smoke-c2c                                    # 默认 Alice(1000000051) -> Bob(1000000056)
#   make smoke-c2c SMOKE_FROM=1000000051 SMOKE_TO=1000000056
#   make smoke                                        # 跑所有 Tier-0 冒烟
# 前置：imboy@127.0.0.1 节点已启动，本地 PG 可连。
# -----------------------------------------------------------------------------
SMOKE_FROM ?= 1000000051
SMOKE_TO   ?= 1000000056

.PHONY: smoke smoke-c2c smoke-ws smoke-ctl
smoke-c2c:
	@./scripts/smoke/c2c_smoke.sh $(SMOKE_FROM) $(SMOKE_TO)

# Tier-0 WS 冒烟：Bob 连 /ws 订阅 + Alice 发 C2C + 校验 Bob 收到帧
#   make smoke-ws
#   make smoke-ws SMOKE_FROM=1000000051 SMOKE_TO=1000000056
# 前置：imboy@127.0.0.1 节点已启动，python3 + websockets 已安装。
smoke-ws:
	@./scripts/smoke/c2c_ws_smoke.sh $(SMOKE_FROM) $(SMOKE_TO)

smoke-ctl:
	@./scripts/smoke/ctl_smoke.sh

smoke: smoke-c2c smoke-ws smoke-ctl
	@echo "=== all Tier-0 smoke PASS ==="

# -----------------------------------------------------------------------------
# imboy_ctl — 统一 CLI 管理工具
# 用法：
#   make ctl                          # 显示帮助
#   make ctl ARGS="node status"       # 节点状态
#   make ctl ARGS="user token 51"     # 生成 token
#   make ctl ARGS="smoke all"         # 全部冒烟
#   make ctl NODE=001@127.0.0.1 ARGS="node status"  # 指定节点
# 前置：目标节点已启动。默认连接 imboy@127.0.0.1。
# 生产服务器：修改下方 CTL_NODE 默认值即可一劳永逸。
CTL_NODE ?= imboy@127.0.0.1
# -----------------------------------------------------------------------------
.PHONY: ctl
ctl:
	@IMBOY_CTL_NODE=$(CTL_NODE) escript scripts/imboy_ctl $(ARGS)

# =============================================================================
# v1.3 工程质量 target（新增于 2026-05-08，禁止改 erlang.mk，仅追加 Makefile）
# 工具：elvis 5.0.3（brew install elvis）/ erlfmt 1.8.0（brew install erlfmt）
# 配置：imboy/elvis.config（v1.3 T2.1 已落地）
# 关联：.claude/plans/quality-loop.md v1.3 T2.2
# 用法：
#   make lint-erlang        # 跑 elvis lint（exit 0 = 通过；当前基线 239/349 .erl FAIL）
#   make format-check       # 检查 erlfmt 格式（exit 1 = 有文件未格式化）
#   make format             # 应用 erlfmt 格式化（修改文件，慎用）
#   make xref-strict        # 调 erlang.mk 原生 xref（含 undefined_function_calls）
# 备注：
#   - elvis-fix 暂未实现（elvis 5.x 无原生 fix 命令；如需统一格式化请用 erlfmt）
#   - target 名故意避开 'elvis' — 与可执行 elvis 同名时 GNU make 仅扫部分文件
#     （make elvis 出 53 行 vs elvis rock 9954 行；2026-05-10 fix）
# =============================================================================

ERLFMT_FILES := 'src/*.erl' 'src/**/*.erl' 'include/*.hrl' 'src/*.hrl' 'src/**/*.hrl'

.PHONY: lint-erlang format format-check xref-strict

lint-erlang:
	@elvis rock

format:
	@erlfmt --write $(ERLFMT_FILES)

format-check:
	@erlfmt --check $(ERLFMT_FILES)

xref-strict: xref
	@echo "xref strict mode (erlang.mk 原生 xref 已含 undefined_function_calls)"

# 清理当前目录及子目录下所有 .beam 文件（跳过 deps/）
.PHONY: clear_beam
clear_beam:
	@find . -path ./deps -prune -o -name '*.beam' -print -delete
	@echo "clear_beam done"
