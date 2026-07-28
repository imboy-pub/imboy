%%% E2EE-062 续：OTK claim 的**目标级**限流（21-playbook E2EE-025
%%% 「不同攻击 request 触发目标级限流」）。
%%%
%%% == 缺口 ==
%%%
%%% 上一刀（幂等租约，见 evidence/E2EE-062-otk-claim-idempotent-lease.md）
%%% 只挡住「同一个 request_id 重放」。它挡不住两件事：
%%%   1. 攻击者每次换一个新 request_id —— 每次都是「新请求」，租约不命中；
%%%   2. N 个账号协同 —— 每个账号都在自己的 per-claimant 配额（30/min）之内。
%%% 两者都能把**同一个目标**的 OTK 池抽干，把该用户的所有新会话逼到复用
%%% 同一条 fallback prekey（前向保密显著下降）。
%%%
%%% 现状只有 per-claimant 一层：`throttle:check(olm_claim, CurrentUid)`。
%%% 限流的键是**领取方**，与被抽干的**目标**无关——换个号就绕过。
%%%
%%% == 本文件守护 ==
%%%
%%% 1. claim / batch_claim 必须**额外**以**目标 uid** 为键做一次限流；
%%% 2. 目标级超限 → 429，且**不得到达 logic 层**（不得消费 OTK）；
%%% 3. 【正向可用性】两层都未超限时，请求必须照常到达 logic 并成功——
%%%    一个「一律 429」的实现在限流指标上恒得满分，必须被这条否掉；
%%% 4. per-claimant 层不得被削弱（既有守护在
%%%    `olm_handler_claim_throttle_tests`，此处只补目标层）。
-module(e2ee_otk_target_throttle_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(CLAIMANT, 100).
-define(TARGET, 200).

%% 记录 throttle:check/2 的全部 (Scope, Key) 调用，供断言「按哪个键限流」
-define(SCOPE_KEY, {?MODULE, scopes}).

record_scope(Scope, Key) ->
    Prev =
        case persistent_term:get(?SCOPE_KEY, undefined) of
            undefined -> [];
            L -> L
        end,
    persistent_term:put(?SCOPE_KEY, Prev ++ [{Scope, Key}]).

scopes() ->
    case persistent_term:get(?SCOPE_KEY, undefined) of
        undefined -> [];
        L -> L
    end.

reset_scopes() ->
    persistent_term:erase(?SCOPE_KEY).

%% ===================================================================
%% 1. claim_key：必须按目标 uid 限流
%% ===================================================================

claim_key_checks_target_scope_test_() ->
    ?WITH_MECKS(
        [
            {imboy_policy, [{'e2ee_enabled', 0, fun() -> true end}]},
            {auth_ds, [{'current_uid', 1, fun(_State) -> ?CLAIMANT end}]},
            {throttle, [
                {'check', 2, fun(Scope, Key) ->
                    record_scope(Scope, Key),
                    ok
                end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{<<"target_uid">> => <<"200">>, <<"device_id">> => <<"dev-X">>}
                end}
            ]},
            {olm_identity_logic, [
                {'claim_keys', 3, fun(_C, _T, _D) ->
                    {ok, #{<<"type">> => <<"one_time">>}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, _P) -> {responded, success} end},
                {'error', 3, fun(_Req, Msg, Code) -> {responded, error, Msg, Code} end}
            ]}
        ],
        fun() ->
            reset_scopes(),
            {ok, Result, _} = olm_handler:init(cowboy_req_ok, #{action => claim_key}),
            Scopes = scopes(),
            reset_scopes(),
            %% per-claimant 层不得被削弱
            ?assert(lists:member({olm_claim, ?CLAIMANT}, Scopes)),
            %% 目标层：键必须是**目标 uid**，不是领取方
            ?assert(
                lists:member({olm_claim_target, ?TARGET}, Scopes),
                "claim 必须额外以目标 uid 为键限流，否则换个账号即可抽干同一目标的 OTK 池"
            ),
            %% 【正向可用性】两层都未超限 → 照常成功
            ?assertEqual({responded, success}, Result)
        end
    ).

%% ===================================================================
%% 2. 目标级超限 → 429，且不得到达 logic
%% ===================================================================

claim_key_target_limited_returns_429_test_() ->
    ?WITH_MECKS(
        [
            {imboy_policy, [{'e2ee_enabled', 0, fun() -> true end}]},
            {auth_ds, [{'current_uid', 1, fun(_State) -> ?CLAIMANT end}]},
            {throttle, [
                {'check', 2, fun
                    %% 领取方自己**没超限**——这正是本用例的要点：
                    %% N 个协同账号各自都在配额内，只有目标层能拦住它们
                    (olm_claim, _) -> ok;
                    (olm_claim_target, _) -> {limit_exceeded, 60, 10}
                end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{<<"target_uid">> => <<"200">>, <<"device_id">> => <<"dev-X">>}
                end}
            ]},
            {olm_identity_logic, [
                {'claim_keys', 3, fun(_C, _T, _D) ->
                    erlang:error(must_not_reach_logic_when_target_limited)
                end},
                {'claim_keys', 4, fun(_C, _T, _D, _R) ->
                    erlang:error(must_not_reach_logic_when_target_limited)
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, _P) -> {responded, success} end},
                {'error', 3, fun(_Req, Msg, Code) -> {responded, error, Msg, Code} end}
            ]}
        ],
        fun() ->
            {ok, Result, _} = olm_handler:init(cowboy_req_ok, #{action => claim_key}),
            ?assertEqual({responded, error, <<"rate_limited">>, 429}, Result)
        end
    ).

%% ===================================================================
%% 3. batch_claim 同样必须按目标 uid 限流
%% ===================================================================

batch_claim_checks_target_scope_test_() ->
    ?WITH_MECKS(
        [
            {imboy_policy, [{'e2ee_enabled', 0, fun() -> true end}]},
            {auth_ds, [{'current_uid', 1, fun(_State) -> ?CLAIMANT end}]},
            {throttle, [
                {'check', 2, fun(Scope, Key) ->
                    record_scope(Scope, Key),
                    ok
                end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{
                        <<"target_uid">> => <<"200">>,
                        <<"device_ids">> => [<<"dev-A">>, <<"dev-B">>]
                    }
                end}
            ]},
            {olm_identity_logic, [
                {'batch_claim_keys', 3, fun(_C, _T, _D) ->
                    {ok, #{<<"claimed">> => #{}}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, _P) -> {responded, success} end},
                {'error', 3, fun(_Req, Msg, Code) -> {responded, error, Msg, Code} end}
            ]}
        ],
        fun() ->
            reset_scopes(),
            {ok, Result, _} = olm_handler:init(cowboy_req_ok, #{action => batch_claim}),
            Scopes = scopes(),
            reset_scopes(),
            ?assert(lists:member({olm_claim, ?CLAIMANT}, Scopes)),
            ?assert(
                lists:member({olm_claim_target, ?TARGET}, Scopes),
                "batch_claim 是同一条抽干路径（一次请求多设备），目标层限流不得漏"
            ),
            ?assertEqual({responded, success}, Result)
        end
    ).

batch_claim_target_limited_returns_429_test_() ->
    ?WITH_MECKS(
        [
            {imboy_policy, [{'e2ee_enabled', 0, fun() -> true end}]},
            {auth_ds, [{'current_uid', 1, fun(_State) -> ?CLAIMANT end}]},
            {throttle, [
                {'check', 2, fun
                    (olm_claim, _) -> ok;
                    (olm_claim_target, _) -> {limit_exceeded, 60, 3}
                end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{
                        <<"target_uid">> => <<"200">>,
                        <<"device_ids">> => [<<"dev-A">>, <<"dev-B">>]
                    }
                end}
            ]},
            {olm_identity_logic, [
                {'batch_claim_keys', 3, fun(_C, _T, _D) ->
                    erlang:error(must_not_reach_logic_when_target_limited)
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, _P) -> {responded, success} end},
                {'error', 3, fun(_Req, Msg, Code) -> {responded, error, Msg, Code} end}
            ]}
        ],
        fun() ->
            {ok, Result, _} = olm_handler:init(cowboy_req_ok, #{action => batch_claim}),
            ?assertEqual({responded, error, <<"rate_limited">>, 429}, Result)
        end
    ).

%% ===================================================================
%% 4. 目标层不得替换掉领取方层（两层都要在）
%% ===================================================================

%% 领取方超限时仍必须 429——目标层的加入不得把 per-claimant 那道门顶掉。
claim_key_claimant_limit_still_enforced_test_() ->
    ?WITH_MECKS(
        [
            {imboy_policy, [{'e2ee_enabled', 0, fun() -> true end}]},
            {auth_ds, [{'current_uid', 1, fun(_State) -> ?CLAIMANT end}]},
            {throttle, [
                {'check', 2, fun
                    (olm_claim, _) -> {limit_exceeded, 60, 30};
                    (olm_claim_target, _) -> ok
                end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{<<"target_uid">> => <<"200">>, <<"device_id">> => <<"dev-X">>}
                end}
            ]},
            {olm_identity_logic, [
                {'claim_keys', 3, fun(_C, _T, _D) ->
                    erlang:error(must_not_reach_logic_when_claimant_limited)
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, _P) -> {responded, success} end},
                {'error', 3, fun(_Req, Msg, Code) -> {responded, error, Msg, Code} end}
            ]}
        ],
        fun() ->
            {ok, Result, _} = olm_handler:init(cowboy_req_ok, #{action => claim_key}),
            ?assertEqual({responded, error, <<"rate_limited">>, 429}, Result)
        end
    ).

%% ===================================================================
%% 5. 配置漂移可见性（本轮实证发现）
%% ===================================================================

%% 已实证：`throttle:check/2` 遇到未注册 scope 返回原子 `rate_not_set`（不崩），
%% 会被朴素的 `_ -> false` 当成「未超限」**静默放行**——整道限流无声消失。
%% 此处钉死：该情形必须放行（不能拒掉全部 claim，那会让 E2EE 建会话不可用），
%% 但实现必须显式识别该原子（生产代码里配 ERROR 日志），不得与「未超限」混为一谈。
claim_key_missing_scope_degrades_visibly_test_() ->
    ?WITH_MECKS(
        [
            {imboy_policy, [{'e2ee_enabled', 0, fun() -> true end}]},
            {auth_ds, [{'current_uid', 1, fun(_State) -> ?CLAIMANT end}]},
            {throttle, [
                {'check', 2, fun
                    (olm_claim, _) -> ok;
                    %% 未注册 scope 的真实返回值（erl 实测，见 evidence §4.1）
                    (olm_claim_target, _) -> rate_not_set
                end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{<<"target_uid">> => <<"200">>, <<"device_id">> => <<"dev-X">>}
                end}
            ]},
            {olm_identity_logic, [
                {'claim_keys', 3, fun(_C, _T, _D) ->
                    {ok, #{<<"type">> => <<"one_time">>}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, _P) -> {responded, success} end},
                {'error', 3, fun(_Req, Msg, Code) -> {responded, error, Msg, Code} end}
            ]}
        ],
        fun() ->
            {ok, Result, _} = olm_handler:init(cowboy_req_ok, #{action => claim_key}),
            ?assertEqual(
                {responded, success},
                Result,
                "scope 缺失是配置错误不是攻击；拒掉全部 claim 会让 E2EE 建会话不可用"
            )
        end
    ).
