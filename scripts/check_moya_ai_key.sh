#!/usr/bin/env bash
# ============================================================
# 墨芽 AI 回课：密钥可用性体检 / Moya AI key preflight
# ------------------------------------------------------------
# 为什么需要它：
#   scripts/check_moya_ai_config.sh 能验「provider 名 / vision / 视频开关 /
#   ecron 作业」，但对 api_key 只能打一行告警 ——
#   「条目 api_key 由环境变量提供，需确认运行时该变量存在」。
#   原因是密钥走 {env, <<"BIGMODEL_API_KEY">>} → os:getenv/1，
#   **只存在于进程环境里，配置文件里看不到值**，静态检查无从验证。
#   于是最常见的翻车形态是：配置全对、门禁全绿，一触发却是
#   error_code=provider_unavailable（变量名写错/没 export/名字多了 IMBOY_ 前缀）。
#
# 本脚本用**一个独立的临时节点**把它验掉，不重启、不动运行中的服务：
#   1) 从仓根 .env / .env.local 注入密钥（与 scripts/start_node.sh 同源同规则）；
#   2) 用 file:consult 显式装载 llm_providers + teaching_ai_* 到应用环境
#      （注意：erl 的 -config 在 -noshell -eval 下**不会**把 app env 装好，
#        所以这里显式 set_env，见文件尾注释）；
#   3) 解析 provider，打印 vision / model / 密钥长度；
#   4) 默认再发一次**纯文本** ping，证明密钥真能通过第三方网关认证。
#
# 「密钥可用」其实是**两道独立的关**，都要过：
#   (a) 值是对的   —— 由下面的临时节点 + ping 验；
#   (b) 值真的进了**节点进程的环境** —— 由「密钥注入路径体检」逐条复现启动规则来验。
# 2026-09-15 现场栽在 (b)：config 全对、ecron 作业在跑、密钥文件里也确实有值，
# 但节点是用 `make run` 起的，而 erlang.mk 的 `run` 目标不加载任何 env 文件
# ⇒ os:getenv 取到 false ⇒ 每次 AI 都落 failed/provider_unavailable，
# **全程没有任何一处报「密钥没读到」**。只验 (a) 会给出假绿。
#
# 关于 ping 的边界（务必知情）：
#   - 只发一条纯文本（"Reply with exactly one word: pong"），**不涉及任何
#     未成年人媒体、不上传作业视频**，成本可忽略；
#   - 它证明「密钥 + 模型名可用」，**不证明**视频链路可用 ——
#     garage.public_endpoint 若是局域网地址，第三方模型照样抓不到视频，
#     届时表现为 provider_error（见 sys.config.example 的说明）。
#
# 用法：
#   bash scripts/check_moya_ai_key.sh              # 体检 + 纯文本 ping
#   bash scripts/check_moya_ai_key.sh --no-ping    # 只体检，不出网
#   IMBOY_ROOT=/path bash scripts/check_moya_ai_key.sh
# 退出码：0=密钥可用且两条启动路径都能注入；1=不可用（含未配置）
#
# 安全：只读取密钥长度，**从不打印密钥内容**。
# ============================================================
set -uo pipefail

ROOT="${IMBOY_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
DO_PING=1
[ "${1:-}" = "--no-ping" ] && DO_PING=0
# 预先算成 Erlang 字面量再注入。
# 反面教材（曾真踩）：在 erl -eval 的双引号串里写
#   case $( [ \"$DO_PING\" = 1 ] && echo true || echo false ) of
# 其中 \" 会退化成**字面引号字符**，比较变成 [ '"1"' = 1 ] → 恒 false，
# 于是默认分支永远走「已跳过 ping」——门禁静默降级。绝不在 erl 串里套嵌套引号。
if [ "$DO_PING" = 1 ]; then PING_ATOM=true; else PING_ATOM=false; fi

cd "$ROOT" || exit 1

echo "== 墨芽 AI 密钥体检（本地，独立临时节点，不影响运行中的服务）=="

# ---------- 第 1 关：密钥注入路径体检 ----------
# 逐条复现「节点是怎么起的」对应的加载规则。任一条把密钥丢了，
# 用那条路径起的服务就必然 provider_unavailable（且不报错）。
echo "  ── 密钥注入路径 ──"
INJECT_FAIL=0

# 路径A：scripts/start_node.sh —— 依次 source .env、.env.local（后者覆盖前者）
if (
  set -a
  for _f in .env .env.local; do
    if [ -f "$_f" ]; then
      # shellcheck disable=SC1090
      . "./$_f"
    fi
  done
  [ -n "${BIGMODEL_API_KEY:-}" ]
); then
  echo "  ✓ 路径A bash scripts/start_node.sh　→ 密钥到位"
else
  echo "  ✗ 路径A bash scripts/start_node.sh　→ 密钥为空（用它起服务必落 provider_unavailable）"
  echo "    修法：在仓根 .env 或 .env.local 里写 BIGMODEL_API_KEY=<id.secret>（变量名不加 IMBOY_ 前缀）"
  INJECT_FAIL=1
fi

# 路径B：erlang.mk 的 `run` 目标（IMBOYENV=local make run → relx console）
# 该目标不经过 start_node.sh，靠仓库 Makefile 顶部把密钥 export 进 recipe 环境。
if [ -f Makefile ]; then
  _tmpmk="$(mktemp -t moya_env_probe)"
  cat > "$_tmpmk" <<'MK'
_probe_llm_key_len:
	@printf '%s' "$$BIGMODEL_API_KEY" | wc -c | tr -d ' '
MK
  # 只取纯数字行，避免 Makefile 其它 stdout 噪音（如递归 make 提示）污染判定
  _b_len="$(make -s --no-print-directory -f Makefile -f "$_tmpmk" _probe_llm_key_len 2>/dev/null \
            | grep -Eo '^[0-9]+$' | tail -1)"
  rm -f "$_tmpmk"
  if [ -n "$_b_len" ] && [ "$_b_len" -gt 0 ]; then
    echo "  ✓ 路径B IMBOYENV=local make run　→ 密钥到位（Makefile 已 export）"
  else
    echo "  ✗ 路径B IMBOYENV=local make run　→ 密钥长度 ${_b_len:-0}（该路径拿不到密钥）"
    echo "    修法：确认 Makefile 顶部有从 .env/.env.local 取 BIGMODEL_API_KEY 并 export 的片段"
    INJECT_FAIL=1
  fi
fi

# 路径C：**当前运行中**的节点有没有真的拿到密钥（只读，不重启）
# 节点名/cookie 取自 config/vm_local.args —— 本地态生效的是这份，不是 config/vm.args。
_VM_LOCAL="config/vm_local.args"
if [ -f "$_VM_LOCAL" ]; then
  _node="$(sed -n 's/^-name[[:space:]]*\(.*\)$/\1/p' "$_VM_LOCAL" | head -1)"
  _cookie="$(sed -n 's/^-setcookie[[:space:]]*\(.*\)$/\1/p' "$_VM_LOCAL" | head -1)"
  if [ -n "$_node" ] && [ -n "$_cookie" ]; then
    _c_state="$(
      # 必须加 -hidden：非 hidden 探针会加入目标节点的 pg/registry 进程组，
      # 每连一次就在**目标节点的日志**里刷 8 行
      # `SYN[imboy_cm@...|pg<imboy_chat>] Node xxx has joined the cluster ...`。
      # 实测本项目已因此累积 224 行噪音（其中本脚本自己贡献 40 行）。
      # hidden 节点照样能 rpc:call，只是不进 nodes() 与进程组。
      NODE_NAME="$_node" erl -noshell -hidden -name moyakeychk@127.0.0.1 -setcookie "$_cookie" -eval '
        Target = list_to_atom(os:getenv("NODE_NAME")),
        case net_adm:ping(Target) of
          pong ->
            case rpc:call(Target, os, getenv, ["BIGMODEL_API_KEY"]) of
              false -> io:format("missing");
              "" -> io:format("missing");
              _ -> io:format("ok")
            end;
          _ -> io:format("unreachable")
        end,
        halt(0).' 2>/dev/null | tail -1
    )"
    case "$_c_state" in
      ok)
        echo "  ✓ 路径C 运行中的节点 ${_node}　→ 进程环境里已有密钥"
        ;;
      missing)
        echo "  ⚠ 路径C 运行中的节点 ${_node}　→ 进程环境里**没有**密钥（该节点起于本次修复之前？）"
        echo "    影响：此刻任何 AI 触发都会落 failed/provider_unavailable；重启该节点即修复"
        ;;
      *)
        echo "  · 路径C 运行中的节点 ${_node} 未连通（没起 或 epmd/名字不符），跳过此项"
        ;;
    esac
  fi
else
  echo "  · 找不到 ${_VM_LOCAL}，跳过「运行中节点」检查"
fi
echo "  ── 注入路径结论：$([ "$INJECT_FAIL" -eq 0 ] && echo '启动路径均可注入密钥' || echo '存在拿不到密钥的启动路径，见上') ──"

if [ "$INJECT_FAIL" -ne 0 ]; then
  echo "  启动路径体检未通过，不再继续（先把密钥能不能到达节点解决掉）"
  exit 1
fi

# ---------- 第 2 关：密钥可用性（值对不对）----------
echo "  ── 密钥可用性 ──"
if [ -f .env.local ] || [ -f .env ]; then
  # -a：把 source 进来的变量自动 export，与 start_node.sh 同规则；后者覆盖前者
  set -a
  for _f in .env .env.local; do
    if [ -f "$_f" ]; then
      # shellcheck disable=SC1090
      . "./$_f"
    fi
  done
  set +a
  echo "  ✓ 已加载 .env / .env.local"
else
  echo "  ! 既没有 .env 也没有 .env.local —— 请 cp .env.example .env 后填入 BIGMODEL_API_KEY"
fi

exec erl -pa ebin -pa deps/*/ebin -noshell -eval "
    application:ensure_all_started(inets),
    %% 走 HTTPS 必须起 ssl，否则 httpc 报 failed_connect ... ssl_not_started
    %% （那个报错看着像凭据问题，其实是本进程没起 ssl 应用）
    application:ensure_all_started(ssl),
    {ok, [Apps]} = file:consult(\"config/sys.local.config\"),
    Imboy = proplists:get_value(imboy, Apps, []),
    %% 显式装载：erl -config 在 -noshell -eval 下不会把 app env 装好
    %% （config_ds:env 会恒取默认值，看起来就像配置没写）
    lists:foreach(fun(K) ->
        case proplists:get_value(K, Imboy, '\$__absent__') of
            '\$__absent__' -> ok;
            V -> application:set_env(imboy, K, V)
        end
    end, [llm_providers, teaching_ai_llm_provider, teaching_ai_attach_video_url]),
    Provider = config_ds:env(teaching_ai_llm_provider, undefined),
    AttachVid = config_ds:env(teaching_ai_attach_video_url, false),
    io:format(\"  · teaching_ai_llm_provider = ~p~n\", [Provider]),
    io:format(\"  · teaching_ai_attach_video_url = ~p~n\", [AttachVid]),
    case Provider of
        undefined ->
            io:format(\"  ✗ provider 未配置（取默认 undefined）→ 恒 provider_unavailable~n\"),
            io:format(\"    修法：在 config/sys.local.config 的 {imboy,[...]} 里加~n\"),
            io:format(\"          {teaching_ai_llm_provider, <<\\\"bigmodel\\\">>}~n\"),
            halt(1);
        _ ->
            case imboy_llm_registry:lookup(Provider) of
                {ok, #{module := Mod, opts := Opts}} ->
                    Key = maps:get(api_key, Opts, <<>>),
                    Vision = maps:get(vision, Opts, false),
                    Model = maps:get(model, Opts, undefined),
                    io:format(\"  ✓ provider ~p 条目存在，模块 ~p~n\", [Provider, Mod]),
                    io:format(\"  · vision = ~p，model = ~p~n\", [Vision, Model]),
                    io:format(\"  · api_key 长度 = ~p~n\", [byte_size(Key)]),
                    if
                        byte_size(Key) =:= 0 ->
                            io:format(\"  ✗ 密钥为空 → resolve_provider 判 HasKey=false~n\"),
                            io:format(\"    → 终态 provider_unavailable（与\\\"没配\\\"同症状）~n\"),
                            io:format(\"    常见原因：变量名不符 / 没 export / 误加 IMBOY_ 前缀~n\"),
                            halt(1);
                        true -> ok
                    end,
                    if
                        Vision =:= true -> ok;
                        true ->
                            io:format(\"  ✗ 该条目未声明 vision=true → 恒 provider_unavailable~n\"),
                            halt(1)
                    end,
                    case ${PING_ATOM} of
                        true ->
                            io:format(\"  · 纯文本 ping（不含任何未成年人媒体）...~n\"),
                            Msg = #{<<\"role\">> => <<\"user\">>,
                                    <<\"content\">> => <<\"Reply with exactly one word: pong\">>},
                            case Mod:chat(0, [Msg], Opts) of
                                {ok, #{<<\"result\">> := R}} ->
                                    io:format(\"  ✓ PING_OK → ~p~n\", [string:slice(R, 0, 80)]),
                                    io:format(\"  ✓ 结论：密钥可用，重启节点后即可产出真 AI 草稿~n\"),
                                    io:format(\"    注意：这只证明文本通路；视频能否被抓取取决于~n\"),
                                    io:format(\"    garage.public_endpoint 是否公网可达（局域网地址会 provider_error）~n\"),
                                    halt(0);
                                {error, E} ->
                                    io:format(\"  ✗ PING_FAIL → ~p~n\", [E]),
                                    io:format(\"    密钥可能无效/过期，或模型名不对（可设 BIGMODEL_MODEL 切换）~n\"),
                                    halt(1)
                            end;
                        false ->
                            io:format(\"  · 已跳过 ping（--no-ping）：仅证明密钥被读到，未证明可用~n\"),
                            halt(0)
                    end;
                Other ->
                    io:format(\"  ✗ registry 未命中 provider ~p → ~p~n\", [Provider, Other]),
                    io:format(\"    检查 llm_providers 里是否有 name => <<\\\"~s\\\">> 的条目~n\", [Provider]),
                    halt(1)
            end
    end,
    halt(0)."
