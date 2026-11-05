PROJECT = imboy
PROJECT_DESCRIPTION = 基于Erlang的Cowboy、Flutter和PostgreSQL计算的一款聊天软件。
PROJECT_VERSION = $(shell cat VERSION)
export PROJECT_VERSION
.DEFAULT_GOAL := test-build

# --- 环境配置选择（IMBOYENV 驱动） ---
# IMBOYENV=dev → relxdev.config + sys.dev.config
# IMBOYENV=pro → relxpro.config + sys.pro.config
# 不存在对应文件时回退默认 relx.config / sys.config
# 环境变量 IMBOY_* 仍由 imboy_env.erl 在运行时覆盖（优先级更高）
_RELX_SRC := $(or $(firstword $(wildcard relx$(IMBOYENV).config)),relx.config)
_SYS_RUNTIME_SRC := $(or $(firstword $(wildcard $(if $(IMBOYENV),config/sys.$(IMBOYENV).config) config/sys.config)),config/sys.config.example)
RELX_CONFIG = $(CURDIR)/$(_RELX_SRC)

.PHONY: runtime-config relx-config-check
runtime-config:
	@mkdir -p config
	@cp "$(_SYS_RUNTIME_SRC)" config/sys.runtime.config

relx-config-check:
	@test -f "$(RELX_CONFIG)" -o -f "$(RELX_CONFIG_SCRIPT)" || { printf 'Missing relx configuration: [%s]\n' "$(RELX_CONFIG)" >&2; exit 1; }
	@$(call erlang,$(imboy_relx_check.erl))

define imboy_relx_check.erl
	Config = $(call relx_get_config.erl),
	case lists:keyfind(release, 1, Config) of
		{release, {Name, _}, Apps} when is_atom(Name), is_list(Apps) -> halt(0);
		_ -> io:format(standard_error, "Invalid relx release configuration~n", []), halt(1)
	end.
endef

rel:: relx-config-check runtime-config
relup:: relx-config-check runtime-config
run:: relx-config-check runtime-config
relx-rel relx-relup: relx-config-check runtime-config

# efb0d04 唯一解析期 Erlang 调用是 relx 元数据；执行期沿用原生命令格式。
# override 不能阻止 := 右侧展开，故在 include 期间只返回 shell 空操作。
override define erlang
$(if $(IMBOY_MAKE_READY),$(ERL) $2 -pz $(ERLANG_MK_TMP)/rebar3/_build/prod/lib/*/ebin/ -eval "$(subst $(newline),,$(call escape_dquotes,$1))" -- erlang.mk,:)
endef

# 仅 run 加载三个 LLM 变量：命令行 > .env.local > .env > 继承环境。
# 原样保留 $/#/空格，不将 dotenv 当 Makefile 或 shell 执行。
_LLM_ENV_SRCS := .env.local .env
# 取第一个非空定义（.env.local 优先于 .env）
_llm_key = $(shell for f in $(_LLM_ENV_SRCS); do \
    [ -f $$f ] || continue; \
    v=$$(grep -h '^$(1)=' $$f 2>/dev/null | head -1 | cut -d= -f2-); \
    if [ -n "$$v" ]; then printf '%s' "$$v"; break; fi; \
  done)
_BIGMODEL_KEY := $(value BIGMODEL_API_KEY)
_ARK_KEY := $(value ARK_API_KEY)
_BAILIAN_KEY := $(value BAILIAN_API_KEY)
run:: export BIGMODEL_API_KEY = $(or $(call _llm_key,BIGMODEL_API_KEY),$(_BIGMODEL_KEY))
run:: export ARK_API_KEY = $(or $(call _llm_key,ARK_API_KEY),$(_ARK_KEY))
run:: export BAILIAN_API_KEY = $(or $(call _llm_key,BAILIAN_API_KEY),$(_BAILIAN_KEY))

include include/deps.mk

# Web / HTTP
DEPS = ranch cowlib cowboy gun
# Utility
DEPS += erlware_commons jose gen_smtp throttle
DEPS += goldrush jsone  # goldrush: lager transitive dep, pin version explicitly
# Database / Cache / ID
DEPS += epgsql pooler erlang_migrate depcache syn ecron uid
# Ops / Observability
DEPS += telemetry lager observer_cli recon redbug
## OpenTelemetry（对接 Uptrace trace.imboy.pub；版本与传递依赖定义见 include/deps.mk）
DEPS += opentelemetry_api opentelemetry opentelemetry_exporter grpcbox tls_certificate_check chatterbox ctx acceptor_pool gproc ssl_verify_fun
# Template / Captcha
DEPS += simple_captcha erlydtl
# Payment（同工作区本地纯 Erlang 第三方支付库）
DEPS += erlang_pay
DEPS += hird_runtime

# 开发期热重载：sync 会扫描源码目录自动重编译并热加载改动的模块。
# DEPS 会被 erlang.mk 写进 ebin/imboy.app 的 applications，随 release 发布
# 并在节点启动时自动 start —— 生产机上任意 .erl 写入即升级为任意代码执行。
# 全仓零代码引用 sync:*，因此只在本地开发挂载；
# 生产打包走 scripts/imboy-deploy.sh 的 `IMBOYENV=pro make rel`，天然不含。
ifeq ($(IMBOYENV),local)
DEPS += sync
endif

LOCAL_DEPS = mnesia sasl ssl inets eunit crypto public_key
BUILD_DEPS = relx gpb bbmustache
# Cowboy 插件仅提供 new 模板；审计、帮助和编译无需为它下载依赖。
DEP_PLUGINS = $(if $(filter new,$(MAKECMDGOALS)),cowboy)
TEST_DEPS += meck proper

SP = 4
DOC_DEPS = edown
EDOC_OPTS = {doclet, edown_doclet}

# 全量排除 VM 全局 TSID、专属 PG/HIRD、迁移 DDL 和附件协议套件。
# 源模块自动带入同名 *_tests，故也排除源入口；专项门负责实际执行。
# t= 定向执行原样直通；talk_record fixture 包装仅用于全量。
# group_schedule_remind 专项：scripts/test/group_schedule_remind_pg_gate.sh。
ifndef t
EUNIT_TEST_SPEC = (fun() -> Excl = [elib_tsid, agent_grant_pg, agent_run_pg, agent_grant_pg_tests, agent_recovery_pg_tests, agent_run_pg_tests, agent_tool_authorizer_pg_tests, cs_pg_widget, cs_seat_console_pg_tests, imboy_hird_acceptance_tests, imboy_hird_replay_offline_tests, agent_hird_mini_spike_tests, imboy_hird_p1_auth_tool_tests, imboy_hird_gates_tests, imboy_hird_p2_replay_tests, imboy_hird_p3_compare_tests, talk_record_cleanup_retry_pg_checks, talk_record_delete_tests, attachment_lifecycle_v5_w403_gateway_protocol_tests, attachment_lifecycle_v5_w403_driver_matrix_tests, attachment_ordinary_http_protocol_tests, eb_asset_frozen_storage_protocol_tests, enterprise_asset_driver_pg_tests, attachment_pending_location_pg_tests, message_result_pg_tests, group_schedule_remind_pg_tests, moment_delete_atomic_pg_tests], Tests = [talk_record_owned_eunit_fixture:entry(M) || M <- $1, not lists:member(M, Excl)], case Tests of [] -> erlang:error(empty_eunit_selection); _ -> Tests end end)()
endif

# BUILD-00R 后端物理裁剪接线：生成器按 manifest 产出未被选中 feature 的专属
# 模块清单（include/generated/imboy_product_features_erlc.mk），erlang.mk 原生
# ERLC_EXCLUDE 使其不参与编译；.app 的 {modules, []} 由 erlang.mk 以实际编译
# 产物回填，被排除模块自动退出 .app 与 release 包。
# 本赋值必须在 include erlang.mk 之前——erlang.mk 的 ERLC_EXCLUDE ?= 先行
# 定义空值后，后置 ?= 不再生效。
-include include/generated/imboy_product_features_erlc.mk
ERLC_EXCLUDE ?= $(IMBOY_FEATURE_ERLC_EXCLUDE)

# .app 时间戳不能证明 beam 完整或裁剪 profile 一致；失效时触发原生重编。
# ERLC_EXCLUDE_PATHS 延迟展开，裁剪模块不参与存在性检查。
app:: beam-presence-guard
test-build:: beam-presence-guard
.PHONY: beam-presence-guard
beam-presence-guard:
	@mkdir -p $(ERLANG_MK_TMP)
	@if [ "$(sort $(ERLC_EXCLUDE))" != "$$(cat $(ERLANG_MK_TMP)/$(PROJECT).erlc-exclude 2>/dev/null)" ]; then \
	  rm -f ebin/$(PROJECT).app && \
	  printf '%s' "$(sort $(ERLC_EXCLUDE))" > $(ERLANG_MK_TMP)/$(PROJECT).erlc-exclude; \
	fi
	@if [ -f ebin/$(PROJECT).app ]; then \
	  for f in $$(find src -name '*.erl'); do \
	    case " $(ERLC_EXCLUDE_PATHS) " in *" $$f "*) continue ;; esac; \
	    if [ ! -f "ebin/$$(basename "$$f" .erl).beam" ]; then \
	      echo "== WH-02: ebin beam missing for $$f, force full recompile =="; \
	      rm -f ebin/$(PROJECT).app; break; \
	    fi; \
	  done; \
	fi

# 权威判定：imboy_quality_build.erl entry_route/1；必须早于 erlang.mk 默认赋值。
# 依据 origin 区分未定义与显式空值，内层调用优先。
IMBOY_EUNIT_ROUTE := quality_full
ifneq ($(IMBOY_EUNIT_INNER),)
IMBOY_EUNIT_ROUTE := native
else ifneq ($(filter-out undefined,$(foreach name,t EUNIT_CONFIG EUNIT_MODS EUNIT_USE_RELAY,$(origin $(name)))),)
IMBOY_EUNIT_ROUTE := legacy
endif

# 不让原生 eunit 的 test-build 前置与 quality-full 重复构建。
ifeq ($(IMBOYENV):$(IMBOY_EUNIT_ROUTE),local:quality_full)
EUNIT = imboy_quality
tests:: eunit
endif
ifneq ($(origin EUNIT_MODS),undefined)
IMBOY_EUNIT_MODS := $(EUNIT_MODS)
override EUNIT_MODS = $(IMBOY_EUNIT_MODS)
export EUNIT_MODS
endif

# erlang.mk builds c_src as part of app/test-build and relx-rel depends on app.
# V3-20 退役：disk lock NIF（C 源）已删除——c_src 为空，erlang.mk 跳过 C 构建。
# 显式置空 C_SRC_OUTPUT 以免历史 priv/ 残留 .so 被误当作构建产物重新打包。
C_SRC_OUTPUT :=
ifeq ($(shell uname -s),Darwin)
LDFLAGS += -undefined dynamic_lookup
endif
include erlang.mk
IMBOY_MAKE_READY := 1
RELX_REL = $(eval RELX_REL := $(shell $(call erlang,$(get_relx_release.erl))))$(RELX_REL)
RELX_REL_NAME = $(word 1,$(RELX_REL))
RELX_REL_VSN = $(word 2,$(RELX_REL))
RELX_REL_CMD = $(if $(word 3,$(RELX_REL)),console)

# The quality framework's EUnit suites call its tools directly. Keep those
# test-support beams outside the application and outside EUNIT_MODS.
IMBOY_QUALITY_TEST_EBIN = $(ERLANG_MK_TMP)/quality-test-ebin
IMBOY_QUALITY_TEST_BEAMS = $(patsubst tools/imboy_quality/%.erl,$(IMBOY_QUALITY_TEST_EBIN)/%.beam,$(wildcard tools/imboy_quality/*.erl))
override EUNIT_ERL_OPTS += -pa $(IMBOY_QUALITY_TEST_EBIN)
.PHONY: quality-test-support
test-build:: quality-test-support
quality-test-support: $(IMBOY_QUALITY_TEST_BEAMS)
$(IMBOY_QUALITY_TEST_EBIN)/%.beam: tools/imboy_quality/%.erl Makefile
	$(verbose) mkdir -p $(IMBOY_QUALITY_TEST_EBIN)
	$(verbose) erlc $(TEST_ERLC_OPTS) -o $(IMBOY_QUALITY_TEST_EBIN) $<

# Linux limits each exec argument (including /bin/sh -c's command) to 128 KiB.
# Keep the complete test source list out of that command and compile bounded
# batches with the original options. xargs failure prevents the build stamp.
# 清单由 make 直接写入文件，完整保留测试集合；分批失败仍使构建失败。
# Test modules have distinct output names and use dependency-provided transforms.
# Bound compiler batches separately from make jobs; use 1 for serial diagnostics.
# 测试模块输出名独立；编译最多并行四批，设为 1 可恢复串行诊断。
TEST_COMPILE_JOBS ?= 4
define compile_test_erl
	$(if $(filter 3.%,$(MAKE_VERSION)),$(error GNU Make 4+ required for test compilation: make $(MAKE_VERSION) drops $$file and would compile a stale file list; use gmake))
	$(if $(and $(filter 1,$(words $(TEST_COMPILE_JOBS))),$(filter 1 2 3 4,$(TEST_COMPILE_JOBS))),,$(error TEST_COMPILE_JOBS must be one of 1 2 3 4))
	$(file >$(ERLANG_MK_TMP)/$(PROJECT).test-compile-files,$1)
	$(verbose) xargs -n 64 -s 32768 -P $(TEST_COMPILE_JOBS) erlc -v $(TEST_ERLC_OPTS) -o $(TEST_DIR) \
		-pa ebin/ -I include/ < $(ERLANG_MK_TMP)/$(PROJECT).test-compile-files
endef

autopatch-hird_runtime::
	$(verbose) mkdir -p $(DEPS_DIR)/hird_runtime/src
	$(verbose) for source in $(DEPS_DIR)/hird_runtime/runtime/*.erl; do \
		ln -sf "../runtime/$$(basename "$$source")" "$(DEPS_DIR)/hird_runtime/src/$$(basename "$$source")"; \
	done
	$(verbose) printf '%s\n' 'PROJECT = hird_runtime' \
		'PROJECT_VERSION = $(word 3,$(dep_hird_runtime))' 'ERLC_OPTS = +debug_info' \
		'include $(CURDIR)/erlang.mk' > $(DEPS_DIR)/hird_runtime/Makefile

# 原生排除仅覆盖 src/；按递归 ERL_FILES 的 basename 匹配所有层级。
ERLC_EXCLUDE_PATHS = $(foreach m,$(ERLC_EXCLUDE),$(filter %/$(m).erl,$(ERL_FILES)))

# Preserve direct-module unit coverage for physically excluded features.
# Fixtures are test-only: neither production ebin nor the release app includes them.
# Full builds never add this directory to EUnit's code path, even after a profile switch.
IMBOY_FEATURE_TEST_EBIN = $(ERLANG_MK_TMP)/feature-test-ebin
ifneq ($(strip $(ERLC_EXCLUDE)),)
override EUNIT_ERL_OPTS += -pa $(IMBOY_FEATURE_TEST_EBIN)
test-build::
	$(verbose) mkdir -p $(IMBOY_FEATURE_TEST_EBIN)
	$(verbose) erlc $(TEST_ERLC_OPTS) -pa ebin -I include \
		$(foreach p,$(wildcard $(DEPS_DIR)/*/ebin),-pa $(p)) \
		-o $(IMBOY_FEATURE_TEST_EBIN) $(ERLC_EXCLUDE_PATHS)
endif

# BUILD-00R 物理裁剪装配钩子：erlang.mk 的 ERLC_EXCLUDE 只保证被排除模块
# 不进 .app modules（relx 按整目录拷贝 ebin，不读 modules），因此必须在
# relx 组装前清掉 ebin 中的陈旧 beam，并删除同版本号的既有 release 目录
# （同一 VERSION 下 preset 切换会互相残留文件）。挂在 relx-rel 的前置条件
# 上获得确定性顺序：rel-deps → app → prune → relx 组装。
# 切换裁剪清单会失效 .app；切回 full 时原生编译自动恢复缺失 beam。
relx-rel relx-relup: imboy-prune-excluded-beams

imboy-prune-excluded-beams: app
	@rm -f $(addprefix ebin/,$(addsuffix .beam,$(ERLC_EXCLUDE)))
	@rm -f $(addprefix $(PROJECT_BEAM_CACHE_DIR)/ebin-app/,$(addsuffix .beam,$(ERLC_EXCLUDE)))
	@rm -f $(addprefix $(PROJECT_BEAM_CACHE_DIR)/ebin-test/,$(addsuffix .beam,$(ERLC_EXCLUDE)))
	@if [ -n "$(strip $(ERLC_EXCLUDE))" ]; then rm -rf "$(RELX_OUTPUT_DIR)/$(PROJECT)/lib/$(PROJECT)-$(PROJECT_VERSION)"; fi
	@echo " PRUNE  excluded=$$(echo $(ERLC_EXCLUDE) | wc -w | tr -d ' ') ebin_moment_left=$$(ls ebin/*moment*.beam 2>/dev/null | wc -l | tr -d ' ')"

.PHONY: imboy-prune-excluded-beams

# 禁用 beam-cache 的 app/test 槽位交换，避免旧 mtime 导致错误复用 beam。
# app/test 切换先删除 .app；test 构建再删除 beam，强制重编。
# 先删除 .app 可保证构建中断后，下次构建仍会重建缺失的 beam。
# app/test 构建共享 ebin，必须在不同 checkout 中运行以避免相互干扰。
# Disable app/test cache-slot swaps to avoid stale beam reuse.
# Invalidate .app on either mode switch; test builds then remove old beams.
# App/test builds share ebin; use separate checkouts for concurrent builds.
beam-cache-restore-app:
	$(verbose) rm -f ebin/$(PROJECT).app
	$(verbose) rm -f $(ERLANG_MK_TMP)/$(PROJECT).test
beam-cache-restore-test:
	$(verbose) touch $(ERLANG_MK_TMP)/$(PROJECT).test
	$(verbose) rm -f ebin/$(PROJECT).app
	$(verbose) rm -f ebin/*.beam

define compile_proto.erl
	[begin
		gpb_compile:file(F, case filename:basename(F) of "textguard.proto" -> [use_packages]; _ -> [] end ++ [
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
DIALYZER_DIRS = $(DIALYZER_EBIN_BEAMS) $(wildcard $(DEPS_DIR)/hird_runtime/ebin/*.beam) deps
DIALYZER_OPTS = -Wunmatched_returns --plt $(DIALYZER_PLT) -I $(CURDIR)/include $(DIALYZER_DIRS)
# Project analysis uses BEAMs; erlang.mk parsed its source-mode prerequisites earlier.
dialyze: deps app

DIALYZER_WARNINGS ?= 50

# Compile flags
ERLC_COMPILE_OPTS = +'{parse_transform, lager_transform}' +nowarn_unused_function
ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += -I src/lib

# Common Test
CT_CONFIG ?= config/sys.config
TEST_HTTP_PORT ?= 0
# EUnit 通过 os:getenv 读取端口；默认由内核分配，调用方可显式指定。
export TEST_HTTP_PORT
CT_CONFIG_BASE = $(patsubst %.config,%,$(CT_CONFIG))
CT_CONFIG_BASE_ABS = $(abspath $(CT_CONFIG_BASE))
CT_ERL_ARGS = -config $(CT_CONFIG_BASE_ABS) -eval 'application:load(imboy)' -eval 'application:set_env(imboy, env, test)' -eval 'application:set_env(imboy, http_port, $(TEST_HTTP_PORT))' -eval 'application:set_env(imboy, dsync_enabled, false)'
CT_OPTS ?=
CT_OPTS += -erl_args $(CT_ERL_ARGS)

# REST API black-box tests. The runner owns an isolated scratch database and
# lets Cowboy bind an ephemeral port; reports and redacted evidence stay under
# the ignored .reports/ directory.
#
# The scratch provider (imboy_pg18 container) listens on 127.0.0.1:4323. The
# workspace .env sets IMBOY_PG_PORT for the dev database — a DIFFERENT server
# — so this target pins the REST provider port (REST_* outranks IMBOY_* in the
# runner). Override for a provider elsewhere: make rest-api-test REST_PG_PORT=5433.
# Credentials come from the environment only: source .env first
# (REST_PG_PASSWORD / IMBOY_PG_PASSWORD).
REST_PG_PORT ?= 4323
.PHONY: rest-api-test rest-contract-check
rest-api-test:
	@REST_PG_PORT='$(REST_PG_PORT)' bash scripts/run_rest_api_tests.sh

rest-contract-check:
	@bash scripts/check_rest_contract_coverage.sh

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
# 8 步应用层冒烟链（Golden Gates §4.3，Golden Install 与 CI 共用）：
#   make smoke-8step
SMOKE_FROM ?= 1000000051
SMOKE_TO   ?= 1000000056

.PHONY: smoke smoke-c2c smoke-ws smoke-ctl smoke-8step golden-install golden-upgrade
smoke-c2c:
	@./scripts/smoke/c2c_smoke.sh $(SMOKE_FROM) $(SMOKE_TO)
smoke-ws:
	@./scripts/smoke/c2c_ws_smoke.sh $(SMOKE_FROM) $(SMOKE_TO)
smoke-ctl:
	@./scripts/smoke/ctl_smoke.sh
smoke: smoke-c2c smoke-ws smoke-ctl
	@echo "=== all Tier-0 smoke PASS ==="
smoke-8step:
	@SMOKE_C2C_FROM=$(SMOKE_FROM) SMOKE_C2C_TO=$(SMOKE_TO) bash ./scripts/smoke_8step.sh
# Golden Install 金安装门禁（Golden Gates §4，P2-G1）：cleanroom 全流程 + 计时
# 断言 + restart 幂等。参数见 scripts/golden_install.sh --help，例：
#   make golden-install GOLDEN_ARGS="--image-ref ghcr.io/…@sha256:… --git-ref v1.0.0-alpha.46 --profile ci …"
golden-install:
	@bash ./scripts/golden_install.sh $(GOLDEN_ARGS)
# Golden Upgrade 升级门禁（Golden Gates §5，P2-U1）：安装 vN → 产生真实数据 →
# 升级 vN+1（candidate digest）→ auto_migrate 观察 + 8 步链 + 数据保留断言。
# 参数见 scripts/golden_upgrade.sh --help，例：
#   make golden-upgrade GOLDEN_ARGS="--from v1.0.0-alpha.45 --to-image ghcr.io/…@sha256:… --to-ref v1.0.0-alpha.46 --profile ci …"
golden-upgrade:
	@bash ./scripts/golden_upgrade.sh $(GOLDEN_ARGS)

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
.PHONY: contract-export contract-check contract-regen
security-gate:
	@echo "=== 服务端零密码学守护 (ADR 07 §6.3 / 08 §4) ==="
	@bash scripts/check_server_zero_crypto.sh
	@echo "=== 模块边界守护 (Handler→Logic→DS→Repo 单向依赖) ==="
	@bash scripts/check_module_boundaries.sh
	@echo "=== 纵切架构守护 (Feature Slice 九铁律 / ADR-0007) ==="
	@bash scripts/check_feature_architecture.sh
	@echo "=== 安全门禁全部通过 ==="

# 纵切架构门禁（ADR-0007 / docs/architecture/feature-slice-rules.md）：
# 纵切单元结构、facade 只进 application、domain 纯净、Org 贯穿、跨单元依赖方向、
# Product/Plugin 隔离、假 Plugin 检测。与 check_module_boundaries.sh 互补——
# 后者管旧四层（handler→logic→ds→repo）边界，本目标管 Feature Slice 纵切纪律。
# 管辖 src/features|products|plugins 及对 src/lib 的反向检查；存量 legacy 不迁。
.PHONY: arch-check arch-check-self-test
arch-check: ## 校验 Feature Slice 九铁律（ADR-0007）
	@bash scripts/check_feature_architecture.sh

arch-check-self-test: ## 门禁自身金丝雀自检（10 条必须全触发）
	@bash scripts/check_feature_architecture.sh --self-test

# 迁移文件门禁（ADR-0002）：命名格式 / up-down 成对 / 版本号唯一 / up 非空 /
# 注释头版本自洽；编号断档仅告警不阻断。
.PHONY: migrations-check
migrations-check: ## 校验 priv/migrations/ 命名与 up-down 配对（ADR-0002）
	@bash scripts/check_migrations.sh

# 客服域迁移门（CP-ASSET-01）：先跑静态门（复用 scripts/check_migrations.sh：
# 版本号唯一 / up-down 成对 / 命名非空），再连真库校验"客服域迁移已全部应用"——
# 对比 priv/migrations 里 customer_service 迁移与目标库 schema_migrations /
# schema_migrations_history；静态违规 / 空库 / 脏库（dirty 或缺中间版本）/
# foreign 版本 / 空 oracle 均非零退出。自测：
# scripts/test/customer_service_migration_gate_test.sh（静态四负例 fixture +
# 库状态四负例 + 固定名 imboy_cp12_gate01 正例）。
# 用法：make cs-migration-gate PGDATABASE=... [PGHOST=... PGPORT=... PGUSER=... PGPASSWORD=...]
.PHONY: cs-migration-gate
cs-migration-gate: ## 客服域迁移是否已全部落库（连真库；PGDATABASE=...）
	@bash scripts/customer_service_migration_gate.sh

# 跨仓 Widget 资产配对门（CP-ASSET-04）：后端 frame 资产常量 ↔ imboyadmin
# 产物名的一致性检查；配对关系单源登记在 priv/cs_widget_asset_pairing.json。
# admin 仓默认取同父目录 ../imboyadmin，CI/工作树布局用 ADMIN_REPO_DIR 指定；
# admin 不可达时 exit 3（SKIPPED，"没核对"不冒充"一致"）。
.PHONY: widget-asset-pairing-check
widget-asset-pairing-check: ## CS widget 资产名后端↔admin 配对一致（ADMIN_REPO_DIR=...）
	@bash scripts/check_widget_asset_pairing.sh

.PHONY: terminology-check
terminology-check: app ## 校验 priv/terminology/*.json 的结构、profile 与通用概念键
	@erl -noinput -boot no_dot_erlang -pa imboy/ebin -pa ebin \
		-eval 'ok = product_terminology:validate_all(), halt().'

# 真 PostgreSQL 行为 harness 门（GZAPP：默认工作区鉴权 / 归档强交接 / 建企模板 /
# 成员有权 Workspace / 群与频道的双授权源）。三支 harness 都是真库行为矩阵，
# 没有它们这条证据面就只是"手工可复跑"。
# 前置：目标库必须迁移到与本仓 priv/migrations 一致的 head（脚本会自检并
#      拒绝在 schema 来源不符的库上跑），且集群已铺齐所需扩展。
# 用法: make pg-harness-check PGDATABASE=scratch_gzapp_fix_verify PGPORT=4323
.PHONY: pg-harness-check
pg-harness-check: ## 跑三支真库行为 harness（需已迁移的同源 PG）
	@bash scripts/run_pg_behavior_harnesses.sh

# 跨仓特性产物一致性（GZAPP-C1 回归守卫，ADR-0007 特性机制）。
# 清单 → 11 份产物分落三仓；任一仓产物与清单不一致即非零退出。
# 需要 imboyapp / imboyadmin 与 imboy 同父目录（或用 APP_DIR/ADMIN_DIR 指定）。
# 兄弟仓缺失时输出显式 SKIPPED 且退出码 3（"没核对"≠"一致"，避免假绿）。
.PHONY: feature-cross-repo-check
feature-cross-repo-check: ## 校验三仓特性产物与 config/product-feature-manifest.json 一致
	@python3 scripts/check_product_feature_cross_repo.py \
		--app-dir "$${APP_DIR:-../imboyapp}" \
		--admin-dir "$${ADMIN_DIR:-../imboyadmin}"

# 定时作业门禁（ecron）：模板真源非空 + 无 {jobs,...} 回归 + crontab 规格合法
# + MFA 目标模块/入口真实存在（改名或删模块的连带检查）；逐机运行配置
# （sys.local/pro/dev，均被 .gitignore）与模板的漂移默认只告警——
# 发布前自查用 `bash scripts/check_cron_config.sh --strict`。
.PHONY: cron-check
cron-check: ## 校验 ecron 定时作业配置（模板真源硬门 + 逐机漂移告警）
	@bash scripts/check_cron_config.sh

# P3-C2 API 契约门禁（Golden Gates §2.3 C2 / §2.2 契约变更流程）
# 真源：src/imboy_router.erl + priv/migrations CHECK 约束 + include/error_code.hrl
# 合法变更：改后端真源后，同一 PR 内 make contract-export 并提交 .contract/api_contract.json
contract-export: ## 导出 API 契约产物 .contract/api_contract.json（确定性输出）
	@python3 scripts/contract_gate.py export

contract-check: ## 契约校验：落仓产物 vs 真源自检 + admin/flutter 枚举 diff + EntityId 规则（漂移非零退出）
	@python3 scripts/contract_gate.py check \
		$(if $(ADMIN_DIR),--admin $(ADMIN_DIR)) \
		$(if $(FLUTTER_DIR),--flutter $(FLUTTER_DIR))

# P3-C3 契约物一行重生成（Golden Gates §2.3 C3；SOP 见 CONTRIBUTING「契约变更 SOP」）
# ① .contract/api_contract.json（复用 contract-export，本仓内）
# ② imboyapp/lib/config/error_code.dart（跨仓调用并排仓生成器；并排 ../imboyapp
#   是本地布局假设——CI 与他人独立 clone 不保证存在，缺仓 WARN 跳过不失败）
IMBOYAPP_DIR ?= ../imboyapp
contract-regen: contract-export ## 一行重生成全部契约物：api_contract.json + imboyapp error_code.dart
	@if [ -f $(IMBOYAPP_DIR)/scripts/generate_error_code.dart ]; then \
		echo "=== regen imboyapp/lib/config/error_code.dart（$(IMBOYAPP_DIR)）==="; \
		cd $(IMBOYAPP_DIR) && dart run scripts/generate_error_code.dart; \
	else \
		echo "WARN: $(IMBOYAPP_DIR)/scripts/generate_error_code.dart 不存在（并排仓是本地布局假设，CI/独立 clone 可能分离）"; \
		echo "WARN: 跳过 error_code.dart 重生成；请在 imboyapp 仓内执行 dart run scripts/generate_error_code.dart"; \
	fi


# E2EE 完整验证套件（一键可审计）
.PHONY: e2ee-verify
e2ee-verify: security-gate
	@echo ""
	@echo "=== E2EE 安全验证套件 (S2-S21 全守护) ==="
	@echo "--- 编译测试模块 ---"
	@mkdir -p test
	@erlc -o ebin -pa ebin $$(ls -d deps/*/ebin | sed 's/^/-pa /') -I include -DTEST src/repo/msg_store_repo.erl
	@erlc -o test -pa ebin $$(ls -d deps/*/ebin | sed 's/^/-pa /') -I include \
		$$(find test -name "*e2ee*" -o -name "*olm*" -o -name "imboy_codec_tests.erl" \
		-o -name "message_ds_tests.erl" -o -name "push_notification_logic_tests.erl" \
		-o -name "msg_store_jsonb_roundtrip_tests.erl" -o -name "device_revocation_tests.erl" \
		| grep '\.erl$$')
	@echo "--- 运行 E2EE 测试模块 ---"
	@erl -noshell -pa ebin -pa test $$(ls -d deps/*/ebin | sed 's/^/-pa /') -eval 'Modules = [e2ee_handler_tests,e2ee_handler_capability_tests,e2ee_handler_device_binding_tests,olm_handler_tests,olm_handler_claim_throttle_tests,e2ee_logic_tests,e2ee_backup_logic_tests,e2ee_recovery_logic_tests,e2ee_trust_logic_tests,group_e2ee_logic_tests,olm_identity_logic_tests,olm_otk_lifecycle_tests,e2ee_v3_passthrough_contract_tests,e2ee_c2g_passthrough_contract_tests,e2ee_sender_device_envelope_tests,e2ee_offline_sender_did_tests,e2ee_otk_claim_idempotency_tests,e2ee_otk_target_throttle_tests,e2ee_batch_claim_idempotency_tests,e2ee_otk_count_tests,e2ee_claimant_scope_drift_tests,e2ee_presign_mime_binding_tests,e2ee_otk_exhaustion_metric_tests,e2ee_fallback_signature_tests,e2ee_otk_metric_exposition_tests,e2ee_metrics_ip_gate_tests,e2ee_throttle_scope_config_tests,e2ee_kt_merkle_tests,device_revocation_tests,e2ee_backup_repo_tests,olm_identity_repo_tests,msg_store_jsonb_roundtrip_tests,e2ee_error_code_tests,elib_cipher_e2ee_v2_tests,olm_otk_cleanup_worker_tests,imboy_codec_tests,message_ds_tests,push_notification_logic_tests,e2ee_error_privacy_tests], case eunit:test(Modules, []) of ok -> io:format("~n=== E2EE verify ALL PASSED ===~n"), halt(0); error -> io:format("~n=== E2EE verify FAILED ===~n"), halt(1) end.'

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

# legacy 入口为需要应用/数据库的测试注入独立配置与端口，避免 missing_config。
# make eunit-legacy 或 make eunit-local t=elib_uri_tests 使用此配方；
# 本地无显式参数的 eunit/eunit-local 委派 quality-full。数据库须已完成迁移。
# OTP requires a code path under a directory named imboy; use a private alias outside
# the worktree so existing files named imboy are never replaced or removed.
.PHONY: eunit-local eunit-local-legacy
# EUNIT_CONFIG 可覆盖配置文件（默认 config/sys.local）：CI 无 sys.local.config，
# 物化 sys.config 后以 EUNIT_CONFIG=config/sys 传同口径全量（见 backend-ci.yml）。
EUNIT_CONFIG ?= config/sys.local
# relay 配置副本按调用唯一命名（:= 存下 $$，recipe 里由 shell 展开为本次
# 进程组 PID）：固定名 config/sys.eunit-relay.config 会让并发 eunit-local
# 互相覆写端口——2026-10-04 实证：两个会话并发跑 eunit，后写方写入的
# http 端口被先写方的 VM 读走 → eaddrinuse → app 起不来 → elib_tsid
# 未注册，连锁失败 19 例。
EUNIT_RELAY_CONF := config/sys.eunit-relay.$$$$
EUNIT_PROCESS_LIMIT ?= 32768
EUNIT_ERL ?= erl -noinput -boot no_dot_erlang +P $(EUNIT_PROCESS_LIMIT) +Q 1024
eunit-local-legacy:
	@rm -f .erlang.mk/$(PROJECT).test
	@rm -rf .erlang.mk/beam-cache/$(PROJECT)
	@rm -f ebin/imboy.app
	@alias_root=$$(mktemp -d "$${TMPDIR:-/tmp}/imboy-eunit.XXXXXXXX") || exit 1; \
	ln -s "$$(pwd -P)" "$$alias_root/imboy" || { rmdir "$$alias_root"; exit 1; }; \
	relay_pid=; \
	cleanup() { \
	  if [ -n "$$relay_pid" ]; then \
	    exec 3>&-; \
	    n=0; \
	    while kill -0 "$$relay_pid" 2>/dev/null; do \
	      if [ "$$n" -ge 100 ]; then \
	        echo "ERROR: relay supervisor did not exit; diagnostics retained at $$alias_root" >&2; return 1; \
	      fi; \
	      n=$$((n + 1)); sleep 0.1; \
	    done; \
	    wait "$$relay_pid" || { echo "ERROR: relay supervisor failed; diagnostics retained at $$alias_root" >&2; return 1; }; \
	  fi; \
	  rm -f $(EUNIT_RELAY_CONF).config "$$alias_root/relay.fifo" "$$alias_root/imboy"; rmdir "$$alias_root"; \
	}; \
	trap 'cleanup || exit 1' EXIT; \
	if command -v python3 >/dev/null 2>&1 && [ "$(EUNIT_USE_RELAY)" != "0" ] && [ -f "$(EUNIT_CONFIG).config" ] \
	     && grep -q "$(EUNIT_RELAY_TARGET)" "$(EUNIT_CONFIG).config"; then \
	  sed -e "s/$(EUNIT_RELAY_TARGET)/$(EUNIT_RELAY_PORT)/g" \
	      -e "s/{http_port, 9800}/{http_port, $(EUNIT_HTTP_PORT)}/" \
	      -e "s/{http_port_adm, 9706}/{http_port_adm, $(EUNIT_HTTP_ADM_PORT)}/" \
	      "$(EUNIT_CONFIG).config" \
	    > $(EUNIT_RELAY_CONF).config || exit 1; \
	  echo "== EUNIT PG RELAY: 127.0.0.1:$(EUNIT_RELAY_PORT) -> 127.0.0.1:$(EUNIT_RELAY_TARGET) (http $(EUNIT_HTTP_PORT)/adm $(EUNIT_HTTP_ADM_PORT)) =="; \
	  mkfifo "$$alias_root/relay.fifo" || exit 1; \
	  exec 3<>"$$alias_root/relay.fifo" || exit 1; \
	  python3 test/common/pg_relay_supervisor.py "$$alias_root/relay.fifo" $(EUNIT_RELAY_PORT) $(EUNIT_RELAY_TARGET) 3>&- & \
	  relay_pid=$$!; \
	  sleep 1 3>&-; \
	  IMBOYENV=local $(MAKE) eunit IMBOY_EUNIT_INNER=1 $(if $(t),t=$(t)) ERL="$(EUNIT_ERL)" \
	    EUNIT_ERL_OPTS="-config $(EUNIT_RELAY_CONF) -pa $$alias_root/imboy/ebin -pa ebin -pa test" 3>&-; \
	  rc=$$?; \
	  exit $$rc; \
	else \
	  sed -e "s/{http_port, 9800}/{http_port, $(EUNIT_HTTP_PORT)}/" \
	      -e "s/{http_port_adm, 9706}/{http_port_adm, $(EUNIT_HTTP_ADM_PORT)}/" \
	      "$(EUNIT_CONFIG).config" \
	    > $(EUNIT_RELAY_CONF).config || exit 1; \
	  echo "== EUNIT PG DIRECT: $(EUNIT_RELAY_TARGET) (http $(EUNIT_HTTP_PORT)/adm $(EUNIT_HTTP_ADM_PORT), EUNIT_USE_RELAY=0) =="; \
	  IMBOYENV=local $(MAKE) eunit IMBOY_EUNIT_INNER=1 $(if $(t),t=$(t)) ERL="$(EUNIT_ERL)" \
	    EUNIT_ERL_OPTS="-config $(EUNIT_RELAY_CONF) -pa $$alias_root/imboy/ebin -pa ebin -pa test"; \
	  rc=$$?; \
	  exit $$rc; \
	fi

# A1c：eunit-local 的 PG 中继端口。目标端口取本地 pg_conf 实际端口（4323）；
# 中继监听端口避开常用段。inttest_marker_db 的 <PREFIX>_PG_* env 旋钮与
# ORG08_PGPORT 亦应指向中继端口（由门运行环境导出）。
EUNIT_RELAY_PORT ?= 15432
EUNIT_RELAY_TARGET ?= 4323
# A1c：eunit VM 的 HTTP/adm 监听改用私有端口——主树 agent 的 imboy 实例
# （RTC 后端等）会间歇性绑定同款 9800/9706，撞 port → imboy 启动
# {listener_start_failed,http,eaddrinuse} → 整段 app 套件连锁 cancel
# （run12/14/15 实证）。
EUNIT_HTTP_PORT ?= 19980
EUNIT_HTTP_ADM_PORT ?= 19970

# local 默认委派质量全量；显式参数保留原生/legacy，内层禁止递归。
eunit-legacy: eunit-local-legacy
ifeq ($(IMBOYENV):$(IMBOY_EUNIT_ROUTE),local:quality_full)
eunit eunit-local: quality-full
else ifneq ($(IMBOYENV):$(IMBOY_EUNIT_ROUTE),local:native)
eunit-local: eunit-local-legacy
endif

# 质量入口使用内容指纹和逐 BEAM 收据；hit 复用，miss 复位构建并 seal。
# 带引号的编译选项经环境直传；收据位于被清理的缓存目录之外。
export IMBOY_QUALITY_PROJECT = $(PROJECT)
export IMBOY_QUALITY_ERLC_OPTS = $(ERLC_OPTS)
export IMBOY_QUALITY_TEST_ERLC_OPTS = $(TEST_ERLC_OPTS)
export IMBOY_QUALITY_FEATURES = $(ERLC_EXCLUDE)
IMBOY_QUALITY_BUILD_RECEIPT = $(ERLANG_MK_TMP)/$(PROJECT).quality-test-build.receipt
export IMBOY_QUALITY_BUILD_RECEIPT

.PHONY: quality-selftest quality-test-build quality-full eunit-all quality-group eunit-legacy
quality-selftest:
	escript scripts/quality.escript selftest

# 缺失、漂移、损坏或输入变化必须重建；仅完整性校验通过才能命中。
quality-test-build:
	@if escript scripts/quality.escript build-check; then \
		: ; \
	else \
		echo "== quality-test-build: MISS -> A1c 硬复位 + 全量 TEST 重编 + seal =="; \
		rm -f .erlang.mk/$(PROJECT).test; \
		rm -rf .erlang.mk/beam-cache/$(PROJECT); \
		rm -f ebin/imboy.app; \
		IMBOYENV=local $(MAKE) test-build || exit 1; \
		escript scripts/quality.escript build-seal || exit 1; \
	fi

# rv6 绿基线 worker 数（full-20261031T040531Z 以 IMBOY_QUALITY_WORKERS=4 取证；
# escript 内建默认 2）。命令行可覆写：make eunit IMBOY_QUALITY_WORKERS=2。
IMBOY_QUALITY_WORKERS ?= 4

# 新日常入口（P1-12 切换后 `make eunit` 的落点）。单模块/分组诊断：
# make quality-group g=mod1,mod2（旧口径可用 make eunit-local t=<模块>）。
quality-full:
	escript scripts/quality.escript full-reserve
	$(MAKE) quality-test-build
	escript scripts/quality.escript full-build-complete
	IMBOY_QUALITY_WORKERS=$(IMBOY_QUALITY_WORKERS) escript scripts/quality.escript eunit --full

eunit-all: quality-full

# 诊断：make quality-group g=mod1,mod2（报告标 PARTIAL，不产生 full PASS）
quality-group: quality-test-build
	escript scripts/quality.escript eunit --group=$(g)

# ==================== Gradualizer（本地快检 + CI 宽网基线） ====================
# 职责: pre-push 变更快检 + CI 全仓宽网扫描；分层阻塞门禁由 eqWAlizer 承担
GRADUALIZER_DIR  := tools/gradualizer
GRADUALIZER      ?= $(GRADUALIZER_DIR)/bin/gradualizer
GRADUALIZER_REF  ?= 23533d7eb7541d8a146a507e837fdfff6499a202
GRADUALIZE_BUDGET ?= 0
# gpb 生成代码不参与门禁；elib_str 触发 Gradualizer 崩溃（pick_value none() bug，待上报上游）
# 注意：elib_log 已修复 lager:log metadata 误报，重新纳入检查（否则调用方会报 internal_log undefined）
# barrel_mcp_* 为 vendored 代码（LICENSE.barrel_mcp / VENDOR.md）：不参与本仓门禁，
# 避免 patch 上游；其报错另受 gradualizer 未解析 barrel_mcp.hrl 的伪影影响
GRADUALIZE_EXCLUDE := src/imboy_pb.erl src/lib/elib_str.erl \
                      src/mcp/barrel_mcp_protocol.erl src/mcp/barrel_mcp_registry.erl \
                      src/mcp/barrel_mcp_session.erl src/mcp/barrel_mcp_tasks.erl \
                      src/mcp/barrel_mcp_schema.erl src/mcp/barrel_mcp_uri_template.erl
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
	@mkdir -p .gradualizer/logs; rm -f .gradualizer/logs/*.log; fail=0; total=0; \
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

# compile: erlang.mk 无原生 compile target（构建入口是 app）；
# 历史文档/README 大量引用 make compile，加别名使其直接可用。
.PHONY: compile
compile: app

# ==================== Dialyzer 递减基线门（CI-00） ====================
# make dialyze-check：全量 Dialyzer 分析（dialyze 语义下存量告警 exit 2，此处
# 记录退出码；原始 make rc=2 既可能是告警也可能是执行失败）+ 基线门裁决：
# 基线外新增 1 条告警即红；存量减少不红（提示可人工收紧基线）。棘轮只准减不准增。
# 产物 dialyze-last.log 供基线脚本解析与 CI 摘要。
.PHONY: dialyze-check
dialyze-check:
	@rc=0; $(MAKE) dialyze DIALYZER_OPTS="$(DIALYZER_OPTS)" > dialyze-last.log 2>&1 || rc=$$?; \
		printf 'DIALYZER_ANALYSIS_EXIT=%s\n' "$$rc"
	@bash scripts/check_dialyzer_baseline.sh dialyze-last.log

# L3 gate extension point (l3-gate.sh probes dialyze-local first):
# ratchet semantics per CI-00/TD-04A (0 NEW fingerprints = green; literal warn-0 unreachable, 477 legacy)
dialyze-local: dialyze-check

# ==================== 依赖版本审计 / 定点升级 ====================
# mod-check：审计 include/deps.mk 全部依赖的上游最新版本
#   （git 走 ls-remote tags，hex 走 hex.pm API；并发度可 MOD_CHECK_JOBS=16 调）
# mod-up：定点升级，如 make mod-up MOD="cowboy cowlib"（默认升到最新稳定版）
#   或 make mod-up MOD="cowboy=2.15.0"（指定版本）。
#   make mod-up MOD=all：构建图内全部依赖升到最新稳定版
#   （已最新/pin分支 自动跳过；大版本升级含在内，注意编译+测试验证）。
#   只改 deps.mk，可 git 回滚；升级后需 rm -rf deps/<name> 并清
#   .erlang.mk/dep_built/<name> 再 make（erlang.mk 构建戳记不会因删目录失效）。
.PHONY: mod-check
mod-check: ## 依赖审计: 检查 deps.mk 中哪些依赖可升级
	@bash scripts/mod_check.sh check

.PHONY: mod-up
mod-up: ## 依赖升级: make mod-up MOD="name[=ver] ..." 或 MOD=all（图内全部）
	@test -n "$(MOD)" || { echo "用法: make mod-up MOD=\"name[=ver] ...\"  例如 make mod-up MOD=\"cowboy cowlib=2.17.0\" 或 make mod-up MOD=all"; exit 1; }
	@bash scripts/mod_check.sh up $(MOD)
