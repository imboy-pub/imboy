#!/usr/bin/env bash
# C12 / AC-25：E2EE REST 面硬版本门测试（全离线，不需要数据库）。
#
# 实现说明：运行期生成临时 escript（完整 Erlang 解析器，规避 erl -eval
# 对原子左值匹配/外层变量闭包捕获的限制），从 ebin/deps 加载编译产物，
# meck mock DS 层（app_version_ds / app_version_policy_ds）。
#
# 覆盖三层：
#   1. 接线断言：auth_middleware_api_v1 在 verify_sign 之后调用版本门，
#      阻断响应带明确错误码；错误码已在 error_code.hrl 注册并有消息映射；
#   2. 路径判定断言（is_e2ee_api_path）：/api/v1/e2ee/* 全部 21 条路由 +
#      /api/v1/group/set_e2ee_mode 被拦截；6 条非 E2EE 路径不被误拦；
#   3. 行为断言（app_version_logic:e2ee_gate/2，meck mock DS 层）：
#      负例：伪造低版本被拒（block）、缺 vsn/空 vsn 被拒、取高门生效；
#      正例：等于最低版本边界放行、高于门放行、未配置门全开（rollout 前
#            零行为变化）、cos 缺失放行（生产链路 verify_sign 保证头真实）、
#            多段版本语义化比较。
set -uo pipefail

cd "$(dirname "$0")/../.." || exit 1

PASS=0
FAIL=0

ok() { PASS=$((PASS + 1)); echo "  PASS $1"; }
bad() { FAIL=$((FAIL + 1)); echo "  FAIL $1"; }

MID="src/api/auth_middleware_api_v1.erl"
HRL="include/error_code.hrl"

echo "== 1. 接线断言（wiring） =="

grep -q 'e2ee_version_gate(Path, Req)' "$MID" \
  && ok "中间件在 execute_authorized 中调用版本门" \
  || bad "中间件缺少版本门调用"

# 门必须位于 verify_sign 之后（签名先保证 vsn/cos 头真实，门再按头裁决）
AWK_ORDER=$(awk '
  /auth_ds:verify_sign\(Req, Env\)/ {s=NR}
  /e2ee_version_gate\(Path, Req\)/  {g=NR}
  END {print (s>0 && g>0 && g>s) ? "ordered" : "bad"}' "$MID")
[ "$AWK_ORDER" = "ordered" ] \
  && ok "版本门位于 verify_sign 之后（先验签名真实头，再裁决版本）" \
  || bad "版本门与 verify_sign 的相对顺序错误"

grep -q 'app_version_logic:e2ee_gate(Vsn, Cos)' "$MID" \
  && ok "门判定复用 app_version_logic:e2ee_gate/2（与升级提示同一 min_vsn 真源）" \
  || bad "门判定未复用 e2ee_gate/2"

grep -q 'ERR_E2EE_APP_VERSION_TOO_LOW' "$MID" \
  && ok "阻断响应使用明确错误码 ERR_E2EE_APP_VERSION_TOO_LOW" \
  || bad "阻断响应缺少明确错误码"

grep -q -- '-define(ERR_E2EE_APP_VERSION_TOO_LOW, 5070)' "$HRL" \
  && ok "错误码 5070 已在 error_code.hrl 注册" \
  || bad "错误码 5070 未注册"

grep -q '5070 => <<"客户端版本过低' "$HRL" \
  && ok "错误码 5070 已有用户可读消息映射" \
  || bad "错误码 5070 缺少消息映射"

EScript="$(mktemp /tmp/imboy_e2ee_gate_XXXXXX.erl)"
trap 'rm -f -- "$EScript"' EXIT

cat >"$EScript" <<'EEOF'
#!/usr/bin/env escript
%%! -noshell
-mode(compile).

main(_Args) ->
    Paths = ["ebin" | filelib:wildcard("deps/*/ebin")],
    true = code:add_pathsa(Paths) =/= false,
    _ = application:ensure_all_started(meck),
    PathPass = run_path_checks(),
    BehavPass = run_behavior_checks(),
    io:format("PATH_COUNT ~b~n", [PathPass]),
    io:format("BEHAV_COUNT ~b~n", [BehavPass]),
    halt(0).

%%% ===================================================
%%% 2. 路径判定断言（is_e2ee_api_path）
%%% 与 imboy_router.erl 的 E2EE REST 路由一一对应（21+1 条），
%%% 外加 6 条不得误拦的非 E2EE 负路径。
%%% ===================================================
run_path_checks() ->
    GatePaths = [
        <<"/api/v1/e2ee/user_keys">>, <<"/api/v1/e2ee/group_member_keys">>,
        <<"/api/v1/e2ee/group_history_grant">>, <<"/api/v1/e2ee/report_device_key">>,
        <<"/api/v1/e2ee/key/status">>, <<"/api/v1/e2ee/notifications/pull">>,
        <<"/api/v1/e2ee/recovery/start">>, <<"/api/v1/e2ee/compliance_key">>,
        <<"/api/v1/e2ee/backup/put">>, <<"/api/v1/e2ee/backup/get">>,
        <<"/api/v1/e2ee/backup/info">>, <<"/api/v1/e2ee/backup/delete">>,
        <<"/api/v1/e2ee/olm/identity">>, <<"/api/v1/e2ee/olm/prekeys">>,
        <<"/api/v1/e2ee/olm/fallback_key">>, <<"/api/v1/e2ee/olm/get_identity">>,
        <<"/api/v1/e2ee/olm/claim">>, <<"/api/v1/e2ee/olm/prekey_count">>,
        <<"/api/v1/e2ee/devices">>, <<"/api/v1/e2ee/devices/batch_claim">>,
        <<"/api/v1/e2ee/trust/record">>, <<"/api/v1/group/set_e2ee_mode">>
    ],
    NegPaths = [
        <<"/api/v1/ws">>, <<"/api/v1/init">>, <<"/api/v1/app_version/check">>,
        <<"/api/v1/msg/send">>, <<"/api/v1/group/create">>, <<"/api/v1/passport/login">>
    ],
    P1 = check_paths(GatePaths, true),
    P2 = check_paths(NegPaths, false),
    P1 + P2.

check_paths(Paths, Expect) ->
    lists:foldl(fun(P, Acc) ->
        Got = auth_middleware_api_v1:is_e2ee_api_path(P),
        case Got =:= Expect of
            true ->
                io:format("PASS ~s~n", [P]),
                Acc + 1;
            false ->
                io:format("FAIL ~s expect=~p got=~p~n", [P, Expect, Got]),
                Acc
        end
    end, 0, Paths).

%%% ===================================================
%%% 3. 行为断言（app_version_logic:e2ee_gate/2）
%%% meck mock app_version_ds:find/2 与 app_version_policy_ds:find_by_type/1。
%%% ===================================================
run_behavior_checks() ->
    %% 门配置基线：版本记录存在（最新 1.3.0），policy.min_vsn = 1.2.0
    V1 = #{<<"vsn">> => <<"1.3.0">>},
    P12 = #{<<"min_vsn">> => <<"1.2.0">>},
    Checks = [
        %% 负例
        {"N1 低版本 0.9.0 被拒（block）",
            {<<"0.9.0">>, <<"android">>, V1, P12}, {block, <<"1.2.0">>}},
        {"N2 缺 vsn 头被拒（undefined → 0.0.0）",
            {undefined, <<"android">>, V1, P12}, {block, <<"1.2.0">>}},
        {"N3 空 vsn 头被拒（<<>> → 0.0.0）",
            {<<>>, <<"ios">>, V1, P12}, {block, <<"1.2.0">>}},
        {"N4 policy 1.2.0 / version 级 1.4.0 → 按 1.4.0 拒 1.3.0",
            {<<"1.3.0">>, <<"android">>,
             #{<<"vsn">> => <<"1.5.0">>, <<"min_supported_vsn">> => <<"1.4.0">>}, P12},
             {block, <<"1.4.0">>}},
        %% 正例
        {"P1 等于最低版本 1.2.0 放行（边界 >=）",
            {<<"1.2.0">>, <<"android">>, V1, P12}, allow},
        {"P2 高于门 1.3.0 放行",
            {<<"1.3.0">>, <<"android">>, V1, P12}, allow},
        {"P3 未配置版本记录全开（rollout 前零行为变化）",
            {<<"0.0.1">>, <<"android">>, #{}, #{}}, allow},
        {"P4 有记录但门为 0.0.0 全开",
            {<<"0.0.1">>, <<"android">>, #{<<"vsn">> => <<"1.3.0">>}, #{}}, allow},
        {"P5 cos 缺失放行（verify_sign 已保证真实头）",
            {<<"1.3.0">>, <<>>, V1, P12}, allow},
        {"P6 多段版本 1.10.0 > 1.2.0 放行（语义化比较）",
            {<<"1.10.0">>, <<"android">>, V1, P12}, allow}
    ],
    lists:foldl(fun({Name, {Vsn, Cos, VInfo, Policy}, Expect}, Acc) ->
        Actual = with_mock(VInfo, Policy, fun() ->
            app_version_logic:e2ee_gate(Vsn, Cos)
        end),
        case Actual =:= Expect of
            true ->
                io:format("PASS ~ts~n", [Name]),
                Acc + 1;
            false ->
                io:format("FAIL ~ts expect=~p got=~p~n", [Name, Expect, Actual]),
                Acc
        end
    end, 0, Checks).

with_mock(VersionInfo, Policy, Fun) ->
    try meck:unload() catch _:_ -> ok end,
    ok = meck:new(app_version_ds, [no_link]),
    ok = meck:new(app_version_policy_ds, [no_link]),
    ok = meck:expect(app_version_ds, find, fun(_, _) -> VersionInfo end),
    ok = meck:expect(app_version_policy_ds, find_by_type, fun(_) -> Policy end),
    try
        Fun()
    after
        try meck:unload() catch _:_ -> ok end
    end.
EEOF

echo
echo "== 2+3. 路径判定与行为断言（escript：is_e2ee_api_path + e2ee_gate/2） =="

ES_OUT="$(escript "$EScript" 2>&1)"
echo "$ES_OUT" | grep '^PASS\|^FAIL' | sed 's/^PASS /  PASS /; s/^FAIL /  FAIL /'

P_CNT=$(echo "$ES_OUT" | grep -c "^PASS")
F_CNT=$(echo "$ES_OUT" | grep -c "^FAIL")
PATH_CNT=$(echo "$ES_OUT" | sed -n 's/^PATH_COUNT //p')
BEHAV_CNT=$(echo "$ES_OUT" | sed -n 's/^BEHAV_COUNT //p')

if [ "$F_CNT" -eq 0 ] && [ "$P_CNT" -eq 38 ] \
   && [ "$PATH_CNT" = "28" ] && [ "$BEHAV_CNT" = "10" ]; then
  PASS=$((PASS + 38))
  ok "escript 汇总：路径 28/28（22 拦截 + 6 放行）+ 行为 10/10（4 负例 + 6 正例）"
else
  FAIL=$((FAIL + 1))
  echo "  （escript 断言异常：PASS=${P_CNT} FAIL=${F_CNT} PATH=${PATH_CNT} BEHAV=${BEHAV_CNT}，期望 38/0/28/10）"
  echo "$ES_OUT" | grep -v '^PASS\|^FAIL\|^PATH_COUNT\|^BEHAV_COUNT' | head -5
fi

echo
echo "总计: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
