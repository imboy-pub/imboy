%%% E2EE-062 第七刀：**per-claimant 限流的配置漂移可见性**（残留 ⑥）。
%%%
%%% == 缺口 ==
%%%
%%% 第二刀已实证：`throttle:check/2` 遇到**未注册的 scope** 返回原子
%%% `rate_not_set`（不崩），朴素写法
%%%
%%%     case throttle:check(Scope, Key) of
%%%         {limit_exceeded, _, _} -> 拒;
%%%         _ -> 放行
%%%     end
%%%
%%% 会把它当成「未超限」**静默放行**——`sys.config` 少写一条 scope，
%%% 整道限流就无声消失，且没有任何信号。
%%% 当时只修了目标层（`target_rate_limited/1`），
%%% **per-claimant 那道门（`olm_claim`）被明确记为「未动，列入残留」**
%%% （见 evidence/E2EE-062-per-target-throttle.md §4.1 与 §5 第 9 项）。
%%%
%%% 它是 claim / batch_claim 两条路径上的**第一道**门。它无声失效，等于
%%% 单账号高频 claim 完全不受限，目标层的 60/min 成为唯一防线。
%%%
%%% == 本文件守护 ==
%%%
%%% 1. `olm_claim` scope 未注册时，claim / batch_claim 必须**打出 ERROR 日志**
%%%    （配置漂移可见），不得与「未超限」混为一谈；
%%% 2. 【正向可用性】该情形下请求仍须**照常放行**——scope 缺失是配置错误而非
%%%    攻击，拒掉全部 claim 会让整个 E2EE 建会话不可用，代价远大于「限流暂时失效」。
%%%    一个「一律拒绝」的实现在可见性指标上也能满分，必须被这条否掉；
%%% 3. 【对照组】scope 正常注册时**不得**打这条日志（否则日志成噪音，
%%%    真正的配置漂移就被淹没）；
%%% 4. 【对照组】超限仍然 429（本刀不得削弱既有行为）。
-module(e2ee_claimant_scope_drift_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(CLAIMANT, 100).

%% 记录 elib_log:internal_log/4 的全部调用，供断言「配置漂移是否留下信号」
-define(LOG_KEY, {?MODULE, logs}).

record_log(Msg) ->
    Prev =
        case persistent_term:get(?LOG_KEY, undefined) of
            undefined -> [];
            L -> L
        end,
    persistent_term:put(?LOG_KEY, Prev ++ [Msg]).

logs() ->
    case persistent_term:get(?LOG_KEY, undefined) of
        undefined -> [];
        L -> L
    end.

reset_logs() ->
    persistent_term:erase(?LOG_KEY).

%% 是否出现了「per-claimant scope 缺失」的信号。
%% 只认 olm_claim（目标层的 olm_claim_target 是第二刀已有的，不能拿来充数）。
has_claimant_scope_signal(Logs) ->
    lists:any(
        fun
            ({_Tag, olm_claim}) -> true;
            (_) -> false
        end,
        Logs
    ).

mecks(ThrottleFun, PostVals) ->
    [
        {imboy_policy, [{'e2ee_enabled', 0, fun() -> true end}]},
        {auth_ds, [{'current_uid', 1, fun(_State) -> ?CLAIMANT end}]},
        {throttle, [{'check', 2, ThrottleFun}]},
        {elib_param, [{'post', 1, fun(_Req) -> PostVals end}]},
        {elib_log, [
            {'internal_log', 4, fun(_Level, Msg, _Mod, _Line) ->
                record_log(Msg),
                ok
            end}
        ]},
        {olm_identity_logic, [
            {'claim_keys', 3, fun(_C, _T, _D) -> {ok, #{<<"type">> => <<"one_time">>}} end},
            {'batch_claim_keys', 3, fun(_C, _T, _D) -> {ok, #{<<"claimed">> => #{}}} end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, _P) -> {responded, success} end},
            {'error', 3, fun(_Req, Msg, Code) -> {responded, error, Msg, Code} end}
        ]}
    ].

claim_post() ->
    #{<<"target_uid">> => <<"200">>, <<"device_id">> => <<"dev-X">>}.

batch_post() ->
    #{<<"target_uid">> => <<"200">>, <<"device_ids">> => [<<"dev-A">>]}.

%% ===================================================================
%% 1. 缺失 scope 必须留下信号，且必须放行
%% ===================================================================

claim_key_missing_claimant_scope_is_visible_test_() ->
    ?WITH_MECKS(
        mecks(
            fun
                %% per-claimant scope 未注册（真实返回值，第二刀 erl 实测）
                (olm_claim, _) -> rate_not_set;
                (olm_claim_target, _) -> ok
            end,
            claim_post()
        ),
        fun() ->
            reset_logs(),
            {ok, Result, _} = olm_handler:init(cowboy_req_ok, #{action => claim_key}),
            Logs = logs(),
            reset_logs(),
            ?assert(
                has_claimant_scope_signal(Logs),
                "olm_claim scope 缺失必须打 ERROR 日志；静默放行 = 整道 per-claimant "
                "限流无声消失且无任何信号"
            ),
            %% 【正向可用性】仍须照常放行：scope 缺失是配置错误不是攻击
            ?assertEqual({responded, success}, Result)
        end
    ).

batch_claim_missing_claimant_scope_is_visible_test_() ->
    ?WITH_MECKS(
        mecks(
            fun
                (olm_claim, _) -> rate_not_set;
                (olm_claim_target, _) -> ok
            end,
            batch_post()
        ),
        fun() ->
            reset_logs(),
            {ok, Result, _} = olm_handler:init(cowboy_req_ok, #{action => batch_claim}),
            Logs = logs(),
            reset_logs(),
            ?assert(
                has_claimant_scope_signal(Logs),
                "batch_claim 走同一道 per-claimant 门，配置漂移同样必须可见"
            ),
            ?assertEqual({responded, success}, Result)
        end
    ).

%% ===================================================================
%% 2. 对照组：scope 正常时不得打这条日志（改前改后都必须绿）
%% ===================================================================

%% 若实现无脑对每次 check 都打日志，配置漂移的信号会被正常流量淹没，
%% 等于没有信号。
claim_key_healthy_scope_is_silent_test_() ->
    ?WITH_MECKS(
        mecks(fun(_Scope, _Key) -> ok end, claim_post()),
        fun() ->
            reset_logs(),
            {ok, Result, _} = olm_handler:init(cowboy_req_ok, #{action => claim_key}),
            Logs = logs(),
            reset_logs(),
            ?assertNot(
                has_claimant_scope_signal(Logs),
                "scope 正常时不得打配置漂移日志，否则真正的漂移会被噪音淹没"
            ),
            ?assertEqual({responded, success}, Result)
        end
    ).

%% ===================================================================
%% 3. 对照组：超限仍 429（本刀不得削弱既有行为）
%% ===================================================================

claim_key_limit_exceeded_still_429_test_() ->
    ?WITH_MECKS(
        mecks(
            fun
                (olm_claim, _) -> {limit_exceeded, 30, 60};
                (olm_claim_target, _) -> ok
            end,
            claim_post()
        ),
        fun() ->
            reset_logs(),
            {ok, Result, _} = olm_handler:init(cowboy_req_ok, #{action => claim_key}),
            reset_logs(),
            ?assertEqual({responded, error, <<"rate_limited">>, 429}, Result)
        end
    ).

batch_claim_limit_exceeded_still_429_test_() ->
    ?WITH_MECKS(
        mecks(
            fun
                (olm_claim, _) -> {limit_exceeded, 30, 60};
                (olm_claim_target, _) -> ok
            end,
            batch_post()
        ),
        fun() ->
            reset_logs(),
            {ok, Result, _} = olm_handler:init(cowboy_req_ok, #{action => batch_claim}),
            reset_logs(),
            ?assertEqual({responded, error, <<"rate_limited">>, 429}, Result)
        end
    ).
