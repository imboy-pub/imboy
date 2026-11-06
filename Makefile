PROJECT = imboy
PROJECT_DESCRIPTION = 基于Erlang的Cowboy、Flutter和PostgreSQL计算的一款聊天软件。
PROJECT_VERSION = $(shell cat VERSION)
export PROJECT_VERSION
.DEFAULT_GOAL := test-build

# IMBOYENV 选择 relx/sys 配置；不存在时回退默认配置，运行时 IMBOY_* 优先。
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

# 仅延迟 include 期间的 relx 元数据查询；发布目标执行时再读取。
# Hex 等依赖配方在 include 期间由 dep_target/eval 展开，必须保留真实命令，
# 否则下载会永久变成 ':'。Defer only the exact release metadata query.
override define erlang
$(if $(and $(if $(IMBOY_MAKE_READY),,1),$(call core_eq,$1,$(get_relx_release.erl))),:,$(ERL) $2 -pz $(ERLANG_MK_TMP)/rebar3/_build/prod/lib/*/ebin/ -eval "$(subst $(newline),,$(call escape_dquotes,$1))" -- erlang.mk)
endef

# 仅 run 读取 LLM 密钥：命令行 > .env.local > .env > 环境；保留字面值。
_LLM_ENV_SRCS := .env.local .env
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

DEPS = ranch cowlib cowboy gun
DEPS += erlware_commons jose gen_smtp throttle
DEPS += goldrush jsone  # goldrush: lager transitive dep, pin version explicitly
DEPS += epgsql pooler erlang_migrate depcache syn ecron uid
DEPS += telemetry lager observer_cli recon redbug
DEPS += opentelemetry_api opentelemetry opentelemetry_exporter grpcbox tls_certificate_check chatterbox ctx acceptor_pool gproc ssl_verify_fun
DEPS += simple_captcha erlydtl
DEPS += erlang_pay hird_runtime

# sync 仅用于本地开发，避免生产热重载任意源码。
ifeq ($(IMBOYENV),local)
DEPS += sync
endif

LOCAL_DEPS = mnesia sasl ssl inets eunit crypto public_key
BUILD_DEPS = relx gpb bbmustache
# Cowboy 插件只供 new 模板使用，避免审计、帮助和编译触发下载。
DEP_PLUGINS = $(if $(filter new,$(MAKECMDGOALS)),cowboy)
TEST_DEPS += meck proper

SP = 4
DOC_DEPS = edown
EDOC_OPTS = {doclet, edown_doclet}

# 全量排除 VM 全局状态、专属 PG/HIRD 与协议套件；专项门负责，t= 不过滤。
ifndef t
EUNIT_TEST_SPEC = (fun() -> Excl = [elib_tsid, agent_grant_pg, agent_run_pg, agent_grant_pg_tests, agent_recovery_pg_tests, agent_run_pg_tests, agent_tool_authorizer_pg_tests, cs_pg_widget, cs_seat_console_pg_tests, imboy_hird_acceptance_tests, imboy_hird_replay_offline_tests, agent_hird_mini_spike_tests, imboy_hird_p1_auth_tool_tests, imboy_hird_gates_tests, imboy_hird_p2_replay_tests, imboy_hird_p3_compare_tests, talk_record_cleanup_retry_pg_checks, talk_record_delete_tests, attachment_lifecycle_v5_w403_gateway_protocol_tests, attachment_lifecycle_v5_w403_driver_matrix_tests, attachment_ordinary_http_protocol_tests, eb_asset_frozen_storage_protocol_tests, enterprise_asset_driver_pg_tests, attachment_pending_location_pg_tests, message_result_pg_tests, group_schedule_remind_pg_tests, moment_delete_atomic_pg_tests], Tests = [talk_record_owned_eunit_fixture:entry(M) || M <- $1, not lists:member(M, Excl)], case Tests of [] -> erlang:error(empty_eunit_selection); _ -> Tests end end)()
endif

# 特性清单必须早于 erlang.mk；ERLC_EXCLUDE 同时约束编译和发布。
-include include/generated/imboy_product_features_erlc.mk
ERLC_EXCLUDE ?= $(IMBOY_FEATURE_ERLC_EXCLUDE)

# .app 不证明 BEAM 完整；裁剪配置变化或产物缺失时使其失效。
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

C_SRC_OUTPUT :=
ifeq ($(shell uname -s),Darwin)
LDFLAGS += -undefined dynamic_lookup
endif
include include/quality.mk
include erlang.mk
IMBOY_MAKE_READY := 1
RELX_REL = $(eval RELX_REL := $(shell $(call erlang,$(get_relx_release.erl))))$(RELX_REL)
RELX_REL_NAME = $(word 1,$(RELX_REL))
RELX_REL_VSN = $(word 2,$(RELX_REL))
RELX_REL_CMD = $(if $(word 3,$(RELX_REL)),console)

# 质量框架辅助 BEAM 仅供测试，不进入应用或 EUNIT_MODS。
IMBOY_QUALITY_TEST_EBIN = $(ERLANG_MK_TMP)/quality-test-ebin
IMBOY_QUALITY_TEST_BEAMS = $(patsubst tools/imboy_quality/%.erl,$(IMBOY_QUALITY_TEST_EBIN)/%.beam,$(wildcard tools/imboy_quality/*.erl))
override EUNIT_ERL_OPTS += -pa $(IMBOY_QUALITY_TEST_EBIN)
.PHONY: quality-test-support
test-build:: quality-test-support
quality-test-support: $(IMBOY_QUALITY_TEST_BEAMS)
$(IMBOY_QUALITY_TEST_EBIN)/%.beam: tools/imboy_quality/%.erl Makefile
	$(verbose) mkdir -p $(IMBOY_QUALITY_TEST_EBIN)
	$(verbose) erlc $(TEST_ERLC_OPTS) -o $(IMBOY_QUALITY_TEST_EBIN) $<

# GNU Make 4 写完整清单，xargs 限制参数长度；任一编译失败不写构建戳。
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

# 原生裁剪只覆盖 src/，这里按 basename 覆盖递归子目录。
ERLC_EXCLUDE_PATHS = $(foreach m,$(ERLC_EXCLUDE),$(filter %/$(m).erl,$(ERL_FILES)))

# 裁剪模块仅在隔离测试目录编译，供直接模块测试使用。
IMBOY_FEATURE_TEST_EBIN = $(ERLANG_MK_TMP)/feature-test-ebin
ifneq ($(strip $(ERLC_EXCLUDE)),)
override EUNIT_ERL_OPTS += -pa $(IMBOY_FEATURE_TEST_EBIN)
test-build::
	$(verbose) mkdir -p $(IMBOY_FEATURE_TEST_EBIN)
	$(verbose) erlc $(TEST_ERLC_OPTS) -pa ebin -I include \
		$(foreach p,$(wildcard $(DEPS_DIR)/*/ebin),-pa $(p)) \
		-o $(IMBOY_FEATURE_TEST_EBIN) $(ERLC_EXCLUDE_PATHS)
endif

# app → prune → relx：清理旧 BEAM 和同版本 release，避免 profile 切换残留。
relx-rel relx-relup: imboy-prune-excluded-beams

imboy-prune-excluded-beams: app
	@rm -f $(addprefix ebin/,$(addsuffix .beam,$(ERLC_EXCLUDE)))
	@rm -f $(addprefix $(PROJECT_BEAM_CACHE_DIR)/ebin-app/,$(addsuffix .beam,$(ERLC_EXCLUDE)))
	@rm -f $(addprefix $(PROJECT_BEAM_CACHE_DIR)/ebin-test/,$(addsuffix .beam,$(ERLC_EXCLUDE)))
	@if [ -n "$(strip $(ERLC_EXCLUDE))" ]; then rm -rf "$(RELX_OUTPUT_DIR)/$(PROJECT)/lib/$(PROJECT)-$(PROJECT_VERSION)"; fi
	@echo " PRUNE  excluded=$$(echo $(ERLC_EXCLUDE) | wc -w | tr -d ' ') ebin_moment_left=$$(ls ebin/*moment*.beam 2>/dev/null | wc -l | tr -d ' ')"

.PHONY: imboy-prune-excluded-beams

# 禁止 app/test 缓存槽交换；切换模式先失效 .app，共享 ebin 的构建须串行。
beam-cache-restore-app:
	$(verbose) rm -f ebin/$(PROJECT).app
	$(verbose) rm -f $(ERLANG_MK_TMP)/$(PROJECT).test
beam-cache-restore-test:
	$(verbose) touch $(ERLANG_MK_TMP)/$(PROJECT).test
	$(verbose) rm -f ebin/$(PROJECT).app
	$(verbose) rm -f ebin/*.beam

define compile_proto.erl
	[begin
		ok = gpb_compile:file(F, case filename:basename(F) of "textguard.proto" -> [use_packages]; _ -> [] end ++ [
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
		]),
		ErlFile = filename:join("src", filename:basename(F, ".proto") ++ "_pb.erl"),
		{ok, Code} = file:read_file(ErlFile),
		ok = file:write_file(ErlFile, ["%% @noformat\n", Code])
	end || F <- string:tokens("$1", " ")],
	halt().
endef

include include/tpl.mk
include include/cli.mk

# Dialyzer 使用生产 BEAM，跳过 EUnit 测试模块。
DIALYZER_EBIN_BEAMS = $(filter-out %_tests.beam, $(wildcard ebin/*.beam))
DIALYZER_DIRS = $(DIALYZER_EBIN_BEAMS) $(wildcard $(DEPS_DIR)/hird_runtime/ebin/*.beam) deps
DIALYZER_OPTS = -Wunmatched_returns --plt $(DIALYZER_PLT) -I $(CURDIR)/include $(DIALYZER_DIRS)
dialyze: deps app

ERLC_COMPILE_OPTS = +'{parse_transform, lager_transform}' +nowarn_unused_function
ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS) -I src/lib

CT_CONFIG ?= config/sys.config
TEST_HTTP_PORT ?= 0
# 测试端口默认由内核分配，调用方可覆盖。
export TEST_HTTP_PORT
CT_CONFIG_BASE = $(patsubst %.config,%,$(CT_CONFIG))
CT_CONFIG_BASE_ABS = $(abspath $(CT_CONFIG_BASE))
CT_ERL_ARGS = -config $(CT_CONFIG_BASE_ABS) -eval 'application:load(imboy)' -eval 'application:set_env(imboy, env, test)' -eval 'application:set_env(imboy, http_port, $(TEST_HTTP_PORT))' -eval 'application:set_env(imboy, dsync_enabled, false)'
CT_OPTS ?=
CT_OPTS += -erl_args $(CT_ERL_ARGS)

# REST 测试使用隔离数据库与随机 HTTP 端口；凭据来自 REST_*/IMBOY_* 环境。
REST_PG_PORT ?= 4323
.PHONY: rest-api-test rest-contract-check
rest-api-test:
	@REST_PG_PORT='$(REST_PG_PORT)' bash scripts/run_rest_api_tests.sh

rest-contract-check:
	@bash scripts/check_rest_contract_coverage.sh

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

# 冒烟与安装/升级门：make smoke、smoke-8step、golden-install、golden-upgrade。
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
golden-install:
	@bash ./scripts/golden_install.sh $(GOLDEN_ARGS)
golden-upgrade:
	@bash ./scripts/golden_upgrade.sh $(GOLDEN_ARGS)

CTL_NODE ?= imboy@127.0.0.1
.PHONY: ctl
ctl:
	@IMBOY_CTL_NODE=$(CTL_NODE) escript scripts/imboy_ctl $(ARGS)

ERLFMT_FILES := 'src/*.erl' 'src/**/*.erl' 'include/*.hrl' 'src/*.hrl' 'src/**/*.hrl'

.PHONY: lint-erlang format format-check xref-strict compile
.PHONY: security-gate arch-check arch-check-self-test migrations-check cs-migration-gate
.PHONY: widget-asset-pairing-check terminology-check pg-harness-check feature-cross-repo-check cron-check
.PHONY: contract-export contract-check contract-regen
compile: app

lint-erlang:
	@elvis rock
format:
	@erlfmt --write $(ERLFMT_FILES)
format-check:
	@erlfmt --check $(ERLFMT_FILES)
xref-strict: xref
	@echo "xref strict mode done"

security-gate:
	@echo "=== 服务端零密码学守护 (ADR 07 §6.3 / 08 §4) ==="
	@bash scripts/check_server_zero_crypto.sh
	@echo "=== 模块边界守护 (Handler→Logic→DS→Repo 单向依赖) ==="
	@bash scripts/check_module_boundaries.sh
	@echo "=== 纵切架构守护 (Feature Slice 九铁律 / ADR-0007) ==="
	@bash scripts/check_feature_architecture.sh
	@echo "=== 安全门禁全部通过 ==="

arch-check: ## 校验 Feature Slice 九铁律（ADR-0007）
	@bash scripts/check_feature_architecture.sh

arch-check-self-test: ## 门禁自身金丝雀自检（10 条必须全触发）
	@bash scripts/check_feature_architecture.sh --self-test

migrations-check: ## 校验 priv/migrations/ 命名与 up-down 配对（ADR-0002）
	@bash scripts/check_migrations.sh

cs-migration-gate: ## 客服域迁移是否已全部落库（连真库；PGDATABASE=...）
	@bash scripts/customer_service_migration_gate.sh

# 跨仓门缺少兄弟仓时退出 3（SKIPPED）；不得视为通过。
widget-asset-pairing-check: ## CS widget 资产名后端↔admin 配对一致（ADMIN_REPO_DIR=...）
	@bash scripts/check_widget_asset_pairing.sh

terminology-check: app ## 校验 priv/terminology/*.json 的结构、profile 与通用概念键
	@erl -noinput -boot no_dot_erlang -pa imboy/ebin -pa ebin \
		-eval 'ok = product_terminology:validate_all(), halt().'

pg-harness-check: ## 跑三支真库行为 harness（需已迁移的同源 PG）
	@bash scripts/run_pg_behavior_harnesses.sh

feature-cross-repo-check: ## 校验三仓特性产物与 config/product-feature-manifest.json 一致
	@python3 scripts/check_product_feature_cross_repo.py \
		--app-dir "$${APP_DIR:-../imboyapp}" \
		--admin-dir "$${ADMIN_DIR:-../imboyadmin}"

cron-check: ## 校验 ecron 定时作业配置（模板真源硬门 + 逐机漂移告警）
	@bash scripts/check_cron_config.sh

# 契约真源为路由、迁移约束和错误码；合法变更须更新落仓契约产物。
contract-export: ## 导出 API 契约产物 .contract/api_contract.json（确定性输出）
	@python3 scripts/contract_gate.py export

contract-check: ## 契约校验：落仓产物 vs 真源自检 + admin/flutter 枚举 diff + EntityId 规则（漂移非零退出）
	@python3 scripts/contract_gate.py check \
		$(if $(ADMIN_DIR),--admin $(ADMIN_DIR)) \
		$(if $(FLUTTER_DIR),--flutter $(FLUTTER_DIR))

IMBOYAPP_DIR ?= ../imboyapp
contract-regen: contract-export ## 一行重生成全部契约物：api_contract.json + imboyapp error_code.dart
	@if [ -f $(IMBOYAPP_DIR)/scripts/generate_error_code.dart ]; then \
		echo "=== regen imboyapp/lib/config/error_code.dart（$(IMBOYAPP_DIR)）==="; \
		cd $(IMBOYAPP_DIR) && dart run scripts/generate_error_code.dart; \
	else \
		echo "WARN: $(IMBOYAPP_DIR)/scripts/generate_error_code.dart 不存在（并排仓是本地布局假设，CI/独立 clone 可能分离）"; \
		echo "WARN: 跳过 error_code.dart 重生成；请在 imboyapp 仓内执行 dart run scripts/generate_error_code.dart"; \
	fi

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

.PHONY: docs-serve docs-stop
docs-serve:
	docker compose -f docs/api-sandbox/docker-compose.yml up -d
	@echo "API Docs: http://localhost:8080"

docs-stop:
	docker compose -f docs/api-sandbox/docker-compose.yml down

# 依赖审计/升级：mod-check；mod-up MOD="cowboy cowlib=2.17.0" 或 MOD=all。
.PHONY: mod-check
mod-check: ## 依赖审计: 检查 deps.mk 中哪些依赖可升级
	@bash scripts/mod_check.sh check

.PHONY: mod-up
mod-up: ## 依赖升级: make mod-up MOD="name[=ver] ..." 或 MOD=all（图内全部）
	@test -n "$(MOD)" || { echo "用法: make mod-up MOD=\"name[=ver] ...\"  例如 make mod-up MOD=\"cowboy cowlib=2.17.0\" 或 make mod-up MOD=all"; exit 1; }
	@bash scripts/mod_check.sh up $(MOD)
