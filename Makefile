PROJECT = imboy
PROJECT_DESCRIPTION = 基于Erlang的Cowboy、Flutter和PostgreSQL计算的一款聊天软件。
PROJECT_VERSION = 1.0.0-rc.1
export PROJECT_VERSION

# --- 环境配置选择（IMBOYENV 驱动） ---
# IMBOYENV=dev → relxdev.config + sys.dev.config
# IMBOYENV=pro → relxpro.config + sys.pro.config
# 不存在对应文件时回退默认 relx.config / sys.config
# 环境变量 IMBOY_* 仍由 imboy_env.erl 在运行时覆盖（优先级更高）
_RELX_SRC := relx.config
_SYS_RUNTIME_SRC := config/sys.config
ifneq ($(IMBOYENV),)
  ifneq ($(wildcard relx$(IMBOYENV).config),)
    _RELX_SRC := relx$(IMBOYENV).config
  endif
  ifneq ($(wildcard config/sys.$(IMBOYENV).config),)
    _SYS_RUNTIME_SRC := config/sys.$(IMBOYENV).config
  endif
endif
RELX_CONFIG = $(CURDIR)/$(_RELX_SRC)
$(shell mkdir -p config && cp $(_SYS_RUNTIME_SRC) config/sys.runtime.config)

include include/deps.mk

# Web / HTTP
DEPS = ranch cowlib cowboy gun
# Utility
DEPS += erlware_commons jwerl gen_smtp throttle qdate qdate_localtime
DEPS += goldrush datum jsone
# Database / Cache / ID
DEPS += epgsql pooler erlang_migrate depcache syn fuse ecron aho_corasick uid
# Ops / Observability
DEPS += telemetry lager observer_cli recon redbug
# Template / Captcha
DEPS += simple_captcha erlydtl sync

LOCAL_DEPS = mnesia sasl ssl inets eunit crypto public_key
BUILD_DEPS = relx gpb
DEP_PLUGINS = cowboy
TEST_DEPS += meck

SP = 4
DOC_DEPS = edown
EDOC_OPTS = {doclet, edown_doclet}

include erlang.mk

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

# Dialyzer
DIALYZER_DIRS = -r ebin deps
DIALYZER_OPTS = -Wunmatched_returns --plt $(DIALYZER_PLT) -I $(CURDIR)/include $(DIALYZER_DIRS)
DIALYZER_WARNINGS ?= 50

# Compile flags
ERLC_COMPILE_OPTS = +'{parse_transform, lager_transform}' +nowarn_unused_function
ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)

# Common Test
CT_CONFIG ?= config/sys.config
CT_CONFIG_BASE = $(patsubst %.config,%,$(CT_CONFIG))
CT_CONFIG_BASE_ABS = $(abspath $(CT_CONFIG_BASE))
CT_ERL_ARGS = -config $(CT_CONFIG_BASE_ABS) -eval 'application:load(imboy)' -eval 'application:set_env(imboy, env, test)' -eval 'application:set_env(imboy, http_port, $(TEST_HTTP_PORT))' -eval 'application:set_env(imboy, dsync_enabled, false)'
CT_OPTS ?=
CT_OPTS += -erl_args "$(CT_ERL_ARGS)"

# Feature smoke
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

# Tier-0 冒烟：make smoke | make smoke-c2c | make smoke-ws | make smoke-ctl
SMOKE_FROM ?= 1000000051
SMOKE_TO   ?= 1000000056

.PHONY: smoke smoke-c2c smoke-ws smoke-ctl
smoke-c2c:
	@./scripts/smoke/c2c_smoke.sh $(SMOKE_FROM) $(SMOKE_TO)
smoke-ws:
	@./scripts/smoke/c2c_ws_smoke.sh $(SMOKE_FROM) $(SMOKE_TO)
smoke-ctl:
	@./scripts/smoke/ctl_smoke.sh
smoke: smoke-c2c smoke-ws smoke-ctl
	@echo "=== all Tier-0 smoke PASS ==="

# CLI: make ctl ARGS="node status"
CTL_NODE ?= imboy@127.0.0.1
.PHONY: ctl
ctl:
	@IMBOY_CTL_NODE=$(CTL_NODE) escript scripts/imboy_ctl $(ARGS)

# 代码质量: lint-erlang / format / format-check / xref-strict
ERLFMT_FILES := 'src/*.erl' 'src/**/*.erl' 'include/*.hrl' 'src/*.hrl' 'src/**/*.hrl'

.PHONY: lint-erlang format format-check xref-strict
lint-erlang:
	@elvis rock
format:
	@erlfmt --write $(ERLFMT_FILES)
format-check:
	@erlfmt --check $(ERLFMT_FILES)
xref-strict: xref
	@echo "xref strict mode done"

.PHONY: clear_beam
clear_beam:
	@find . -path ./deps -prune -o -name '*.beam' -print -delete
	@echo "clear_beam done"

## 启动 API 文档服务器（需要 Docker）
.PHONY: docs-serve docs-stop
docs-serve:
	docker compose -f docs/api-sandbox/docker-compose.yml up -d
	@echo "API Docs: http://localhost:8080"

docs-stop:
	docker compose -f docs/api-sandbox/docker-compose.yml down
