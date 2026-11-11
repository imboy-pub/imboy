# 路由与 imboy_quality_build:entry_route/1 同序；必须早于 EUNIT_CONFIG 默认值。
IMBOY_EUNIT_ROUTE := quality_full
ifneq ($(IMBOY_EUNIT_INNER),)
IMBOY_EUNIT_ROUTE := native
else ifneq ($(filter-out undefined,$(foreach name,t EUNIT_CONFIG EUNIT_MODS EUNIT_USE_RELAY,$(origin $(name)))),)
IMBOY_EUNIT_ROUTE := legacy
endif

ifeq ($(IMBOYENV):$(IMBOY_EUNIT_ROUTE),local:quality_full)
EUNIT = imboy_quality
tests:: eunit
endif
ifneq ($(origin EUNIT_MODS),undefined)
IMBOY_EUNIT_MODS := $(EUNIT_MODS)
override EUNIT_MODS = $(IMBOY_EUNIT_MODS)
export EUNIT_MODS
endif

# 显式参数走 legacy；本地无参走 quality-full；内层调用不再委派。
.PHONY: eunit-local eunit-local-legacy
# CI 可用 EUNIT_CONFIG=config/sys；relay 副本按 PID 隔离。
EUNIT_CONFIG ?= config/sys.local
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

# 中继和 HTTP 端口避开开发实例；调用方可覆盖。
EUNIT_RELAY_PORT ?= 15432
EUNIT_RELAY_TARGET ?= 4323
EUNIT_HTTP_PORT ?= 19980
EUNIT_HTTP_ADM_PORT ?= 19970

eunit-legacy: eunit-local-legacy
ifeq ($(IMBOYENV):$(IMBOY_EUNIT_ROUTE),local:quality_full)
eunit eunit-local: quality-full
else ifneq ($(IMBOYENV):$(IMBOY_EUNIT_ROUTE),local:native)
eunit-local: eunit-local-legacy
endif

# 构建选项经环境直传；内容指纹与逐 BEAM 收据验证命中，失败则重建。
export IMBOY_QUALITY_PROJECT = $(PROJECT)
export IMBOY_QUALITY_ERLC_OPTS = $(ERLC_OPTS)
export IMBOY_QUALITY_TEST_ERLC_OPTS = $(TEST_ERLC_OPTS)
export IMBOY_QUALITY_FEATURES = $(ERLC_EXCLUDE)
IMBOY_QUALITY_BUILD_RECEIPT = $(ERLANG_MK_TMP)/$(PROJECT).quality-test-build.receipt
export IMBOY_QUALITY_BUILD_RECEIPT

.PHONY: quality-selftest quality-test-build quality-full eunit-all quality-group eunit-legacy
quality-selftest:
	escript scripts/quality.escript selftest

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

# 全量 worker 默认 4，可用 IMBOY_QUALITY_WORKERS 覆盖。
IMBOY_QUALITY_WORKERS ?= 4

quality-full:
	@sh scripts/quality_timed.sh full_reserve escript scripts/quality.escript full-reserve
	@sh scripts/quality_timed.sh test_build $(MAKE) quality-test-build
	@sh scripts/quality_timed.sh full_build_complete escript scripts/quality.escript full-build-complete
	@sh scripts/quality_timed.sh eunit_full env IMBOY_QUALITY_WORKERS=$(IMBOY_QUALITY_WORKERS) escript scripts/quality.escript eunit --full

eunit-all: quality-full

# 分组结果仅供诊断，不产生 full PASS。
quality-group: quality-test-build
	escript scripts/quality.escript eunit --group=$(g)

# Gradualizer：变更快检和预算审计；分层阻塞门由 eqWAlizer 承担。
GRADUALIZER_DIR  := tools/gradualizer
GRADUALIZER      ?= $(GRADUALIZER_DIR)/bin/gradualizer
GRADUALIZER_REF  ?= 23533d7eb7541d8a146a507e837fdfff6499a202
GRADUALIZE_BUDGET ?= 0
# 排除生成代码、上游崩溃模块和 vendored barrel_mcp_*。
GRADUALIZE_EXCLUDE := src/imboy_pb.erl src/lib/elib_str.erl \
                      src/mcp/barrel_mcp_protocol.erl src/mcp/barrel_mcp_registry.erl \
                      src/mcp/barrel_mcp_session.erl src/mcp/barrel_mcp_tasks.erl \
                      src/mcp/barrel_mcp_schema.erl src/mcp/barrel_mcp_uri_template.erl
GRADUALIZER_OPTS ?= -pa ebin $(addprefix -pa ,$(wildcard deps/*/ebin)) \
                    -I $(CURDIR)/include --no_color --fmt_location brief
# OTP 29 的 match_alias_pats 尚未被上游适配。
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
eqwalize-all: ## 全量检查（CI 用；同时检查退出码和错误输出）
	@mkdir -p .elp
	@$(ELP) eqwalize-all > .elp/eqwalize-all.log 2>&1; rc=$$?; \
	cat .elp/eqwalize-all.log; \
	test $$rc -eq 0 && ! grep -q "^error:" .elp/eqwalize-all.log

# Dialyzer 棘轮：原始日志保留退出码，新增告警由基线脚本裁决。
.PHONY: dialyze-check
dialyze-check:
	@rc=0; $(MAKE) dialyze DIALYZER_OPTS="$(DIALYZER_OPTS)" > dialyze-last.log 2>&1 || rc=$$?; \
		printf 'DIALYZER_ANALYSIS_EXIT=%s\n' "$$rc"
	@bash scripts/check_dialyzer_baseline.sh dialyze-last.log

dialyze-local: dialyze-check
