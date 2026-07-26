PROJECT = imboy
PROJECT_DESCRIPTION = 基于Erlang的Cowboy、Flutter和PostgreSQL计算的一款聊天软件。
PROJECT_VERSION = $(shell cat VERSION)
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
DEPS += erlware_commons jwerl gen_smtp throttle
DEPS += goldrush jsone  # goldrush: lager transitive dep, pin version explicitly
DEPS += jsx  # jwerl transitive dep, pin to gitee git (deps.mk) instead of hex to avoid hex_core in sub-make
# Database / Cache / ID
DEPS += epgsql pooler erlang_migrate depcache syn ecron uid
# Ops / Observability
DEPS += telemetry lager observer_cli recon redbug
# Template / Captcha
DEPS += simple_captcha erlydtl sync
# Payment（同工作区本地纯 Erlang 第三方支付库，ln 本地路径 dep）
DEPS += erlang_pay

LOCAL_DEPS = mnesia sasl ssl inets eunit crypto public_key
BUILD_DEPS = relx gpb bbmustache
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

# Dialyzer — EUnit test beams land in ebin/ after `make eunit`; scan only non-test beams.
DIALYZER_EBIN_BEAMS = $(filter-out %_tests.beam, $(wildcard ebin/*.beam))
DIALYZER_DIRS = $(DIALYZER_EBIN_BEAMS) deps
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
		cmd="bash ./scripts/run_feature_flag_smoke.sh --base-url '$(FEATURE_SMOKE_BASE_URL)' --public-path '$(FEATURE_SMOKE_PUBLIC_PATH)' --admin-path '$(FEATURE_SMOKE_ADMIN_PATH)' --timeout '$(FEATURE_SMOKE_TIMEOUT)'"; \
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

# 安全门禁: security-gate（CI 硬门本地可跑）
.PHONY: security-gate
security-gate:
	@echo "=== 服务端零密码学守护 (ADR 07 §6.3 / 08 §4) ==="
	@bash scripts/check_server_zero_crypto.sh
	@echo "=== 模块边界守护 (Handler→Logic→DS→Repo 单向依赖) ==="
	@bash scripts/check_module_boundaries.sh
	@echo "=== 安全门禁全部通过 ==="

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

# EUnit（本地真 PG）：注入 config/sys.local 使 pg_conf 可读并起全量 imboy app，
# 让 ?TEST_WITH_APP / ?TEST_WITH_DB 类用例真连本地 imboy_v1 跑；否则纯 make eunit
# 无 -config 时 eunit_runner:ensure_config_loaded/0 硬失败 {missing_config, pg_conf}，
# 一大批 ?TEST_WITH_APP setup 被 cancelled。
# 用法: make eunit-local                    # 全量
#       make eunit-local t=elib_uri_tests   # 单模块
# 前置: 本地 imboy_v1 schema 须已应用到最新迁移，否则 imboy_app:start/2 的
#       imboy_migrate:migrate/0 会 {out_of_order, ...} 使 app 启动失败。
.PHONY: eunit-local
eunit-local:
	@IMBOYENV=local $(MAKE) eunit EUNIT_ERL_OPTS="-config config/sys.local -pa ebin -pa test"

# ==================== Gradualizer（本地快检 + CI 宽网基线） ====================
# 职责: pre-push 变更快检 + CI 全仓宽网扫描；分层阻塞门禁由 eqWAlizer 承担
GRADUALIZER_DIR  := tools/gradualizer
GRADUALIZER      ?= $(GRADUALIZER_DIR)/bin/gradualizer
GRADUALIZER_REF  ?= 23533d7eb7541d8a146a507e837fdfff6499a202
GRADUALIZE_BUDGET ?= 0
# gpb 生成代码不参与门禁；elib_str 触发 Gradualizer 崩溃（pick_value none() bug，待上报上游）
# 注意：elib_log 已修复 lager:log metadata 误报，重新纳入检查（否则调用方会报 internal_log undefined）
GRADUALIZE_EXCLUDE := src/imboy_pb.erl src/lib/elib_str.erl
GRADUALIZER_OPTS ?= -pa ebin $(addprefix -pa ,$(wildcard deps/*/ebin)) \
                    -I $(CURDIR)/include --no_color --fmt_location brief
# OTP 29 把 match_alias_pats 警告升级为错误，上游未适配，构建时压制
GRADUALIZER_ERLC_OPTS = -I include -I src -pa ebin +debug_info +nowarn_match_alias_pats

.PHONY: gradualizer-setup
gradualizer-setup: ## 拉取并构建 Gradualizer escript（pin 版本）
	@test -x $(GRADUALIZER) || { \
		git clone https://github.com/josefs/Gradualizer.git $(GRADUALIZER_DIR) && \
		git -C $(GRADUALIZER_DIR) checkout $(GRADUALIZER_REF) && \
		git -C $(GRADUALIZER_DIR) rev-parse HEAD > $(GRADUALIZER_DIR)/PINNED && \
		$(MAKE) -C $(GRADUALIZER_DIR) escript ERLC_OPTS="$(GRADUALIZER_ERLC_OPTS)"; }
	@echo "✅ Gradualizer ready ($(GRADUALIZER_REF))"

.PHONY: gradualize
gradualize: ## 单文件检查: make gradualize FILE=src/lib/elib_cnv.erl
	@test -n "$(FILE)" || { echo "用法: make gradualize FILE=<path.erl>"; exit 1; }
	@$(GRADUALIZER) $(GRADUALIZER_OPTS) $(FILE)

.PHONY: gradualize-layer
gradualize-layer: ## 分层门禁（单发模式，用于转绿层）: make gradualize-layer LAYER=lib
	@test -n "$(LAYER)" || { echo "用法: make gradualize-layer LAYER=lib|repo|ds|logic|api"; exit 1; }
	$(GRADUALIZER) $(GRADUALIZER_OPTS) \
		$(filter-out $(GRADUALIZE_EXCLUDE),$(wildcard src/$(LAYER)/*.erl))

.PHONY: gradualize-audit
gradualize-audit: ## 全仓逐模块审计（预算制，基线期）: make gradualize-audit
	@mkdir -p .gradualizer/logs; fail=0; total=0; \
	for f in $(filter-out $(GRADUALIZE_EXCLUDE),$(wildcard src/*.erl src/*/*.erl)); do \
		case "$$f" in *_tests.erl) continue;; esac; \
		total=$$((total+1)); \
		log=.gradualizer/logs/$$(basename $$f .erl).log; \
		if ! $(GRADUALIZER) $(GRADUALIZER_OPTS) $$f > $$log 2>&1; then \
			fail=$$((fail+1)); echo "❌ $$f"; \
		fi; \
	done; \
	echo "== gradualize-audit: modules=$$total failing=$$fail budget=$(GRADUALIZE_BUDGET)"; \
	echo "gradualizer_failing $$fail" > .gradualizer/metrics.txt; \
	test $$fail -le $(GRADUALIZE_BUDGET)

.PHONY: gradualize-baseline
gradualize-baseline: ## 从最近一次 gradualize-audit 日志重建 pre-push ratchet 基线（入仓，只准减不准增）
	@test -d .gradualizer/logs || { echo "先跑 make gradualize-audit GRADUALIZE_BUDGET=9999"; exit 1; }
	@grep -lE '^src/.*\.erl:[0-9]' .gradualizer/logs/*.log 2>/dev/null \
		| xargs -n1 basename | sed 's/\.log$$//' | sort > .gradualizer-baseline.txt
	@echo "✅ .gradualizer-baseline.txt: $$(wc -l < .gradualizer-baseline.txt | tr -d ' ') 个存量失败模块"

# ==================== ELP / eqWAlizer（CI 分层阻塞门禁 + IDE） ====================
# 项目结构描述在 .elp.toml（入仓）；.elp/elp-repo 不入仓，由 elp-setup 拉取
ELP ?= elp
ELP_REPO_DIR := .elp/elp-repo
ELP_REPO_REF ?= c3708e6a7cc627c5323ef066a2bdfd8d1ba987e5
EQWALIZER_SUPPORT := $(ELP_REPO_DIR)/eqwalizer/eqwalizer_support
EQWALIZE_BUDGET ?= 0

.PHONY: elp-setup
elp-setup: ## 校验 elp + JVM，拉取 eqwalizer_support；校验 .elp.toml 与实际目录一致
	@command -v $(ELP) >/dev/null || { echo "❌ 未找到 elp: brew install erlang-language-platform"; exit 1; }
	@command -v java >/dev/null   || { echo "❌ eqWAlizer 需要 JVM 17+"; exit 1; }
	@if [ ! -d "$(EQWALIZER_SUPPORT)" ]; then \
		mkdir -p .elp && \
		git clone --filter=blob:none --sparse \
		  https://github.com/WhatsApp/erlang-language-platform.git $(ELP_REPO_DIR) && \
		git -C $(ELP_REPO_DIR) sparse-checkout set eqwalizer/eqwalizer_support && \
		git -C $(ELP_REPO_DIR) checkout $(ELP_REPO_REF) && \
		git -C $(ELP_REPO_DIR) rev-parse HEAD > .elp/PINNED; \
	fi
	@actual=$$(ls -d src/*/ | xargs -n1 basename | sort); \
	configured=$$(grep -o 'src/[a-z][a-z]*' .elp.toml | cut -d/ -f2 | sort -u); \
	[ "$$actual" = "$$configured" ] || \
		echo "⚠️ .elp.toml 的 src_dirs 与实际 src/ 子目录不一致，请同步"
	@echo "✅ ELP ready"

.PHONY: eqwalize
eqwalize: ## 单模块检查: make eqwalize MOD=msg_c2c_logic
	@test -n "$(MOD)" || { echo "用法: make eqwalize MOD=<module>"; exit 1; }
	@out=$$($(ELP) eqwalize $(MOD) 2>&1); rc=$$?; echo "$$out"; \
	if [ $$rc -ne 0 ]; then exit $$rc; fi; \
	echo "$$out" | grep -q "^error:" && exit 1 || true

.PHONY: eqwalize-layer
eqwalize-layer: ## 分层检查（预算制）: make eqwalize-layer LAYER=lib
	@test -n "$(LAYER)" || { echo "用法: make eqwalize-layer LAYER=lib|repo|ds|logic|api"; exit 1; }
	@mkdir -p .elp/logs; fail=0; total=0; \
	for f in src/$(LAYER)/*.erl; do \
		m=$$(basename $$f .erl); \
		case "$$m" in *_tests) continue;; esac; \
		total=$$((total+1)); \
		if $(ELP) eqwalize $$m > .elp/logs/$$m.log 2>&1; then \
			grep -q "^error:" .elp/logs/$$m.log && { fail=$$((fail+1)); echo "❌ $$m"; } || true; \
		else \
			fail=$$((fail+1)); echo "❌ $$m (crash, 详见 .elp/logs/$$m.log)"; \
		fi; \
	done; \
	echo "== eqwalize-layer: layer=$(LAYER) modules=$$total failing=$$fail budget=$(EQWALIZE_BUDGET)"; \
	test $$fail -le $(EQWALIZE_BUDGET)

.PHONY: eqwalize-all
eqwalize-all: ## 全量检查（CI 用；解析输出判定，退出码不可信）
	@$(ELP) eqwalize-all 2>&1 | tee .elp/eqwalize-all.log; \
	grep -q "^error:" .elp/eqwalize-all.log && exit 1 || true
