%%% S1.3 OTK claim 限流（P1）：claim_key 和 batch_claim 必须有 per-claimant 速率限制。
%%%
%%% 复现旧缺口：恶意客户端可高频调用 /api/v1/e2ee/olm/claim 耗尽目标设备 OTK 池，
%%% 诱发 fallback 或拒发。修复后 throttle:check 超限返回 429。
-module(olm_handler_claim_throttle_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% claim_key 超限时必须返回 429，不得到达 logic 层。
%% 当前代码无 throttle 调用，此测试在修复前 FAIL。
claim_key_rate_limited_returns_429_test_() ->
    ?WITH_MECKS(
        [
            {imboy_policy, [
                {'e2ee_enabled', 0, fun() -> true end}
            ]},
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 100 end}
            ]},
            {throttle, [
                {'check', 2, fun(_Key, _Uid) -> {limit_exceeded, 60, 10} end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{<<"target_uid">> => <<"200">>, <<"device_id">> => <<"dev-X">>}
                end}
            ]},
            {olm_identity_logic, [
                {'claim_keys', 3, fun(_Claimant, _Target, _Did) ->
                    %% 不应到达此处
                    {ok, #{<<"type">> => <<"one_time">>}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, _Payload) -> {responded, success} end},
                {'error', 3, fun(_Req, Msg, Code) -> {responded, error, Msg, Code} end}
            ]}
        ],
        fun() ->
            Req0 = cowboy_req_ok,
            {ok, Result, _} = olm_handler:init(Req0, #{action => claim_key}),
            ?assertEqual({responded, error, <<"rate_limited">>, 429}, Result)
        end
    ).

%% batch_claim 超限时必须返回 429。
batch_claim_rate_limited_returns_429_test_() ->
    ?WITH_MECKS(
        [
            {imboy_policy, [
                {'e2ee_enabled', 0, fun() -> true end}
            ]},
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 100 end}
            ]},
            {throttle, [
                {'check', 2, fun(_Key, _Uid) -> {limit_exceeded, 60, 5} end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{<<"target_uid">> => <<"200">>, <<"device_ids">> => [<<"dev-A">>, <<"dev-B">>]}
                end}
            ]},
            {olm_identity_logic, [
                {'batch_claim_keys', 3, fun(_Claimant, _Target, _Dids) ->
                    {ok, #{<<"claimed">> => #{}}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, _Payload) -> {responded, success} end},
                {'error', 3, fun(_Req, Msg, Code) -> {responded, error, Msg, Code} end}
            ]}
        ],
        fun() ->
            Req0 = cowboy_req_ok,
            {ok, Result, _} = olm_handler:init(Req0, #{action => batch_claim}),
            ?assertEqual({responded, error, <<"rate_limited">>, 429}, Result)
        end
    ).

%% 正向：未超限时 claim_key 正常到达 logic 层。
claim_key_within_limit_succeeds_test_() ->
    ?WITH_MECKS(
        [
            {imboy_policy, [
                {'e2ee_enabled', 0, fun() -> true end}
            ]},
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 100 end}
            ]},
            {throttle, [
                {'check', 2, fun(_Key, _Uid) -> ok end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{<<"target_uid">> => <<"200">>, <<"device_id">> => <<"dev-X">>}
                end}
            ]},
            {olm_identity_logic, [
                {'claim_keys', 3, fun(100, 200, <<"dev-X">>) ->
                    {ok, #{
                        <<"type">> => <<"one_time">>,
                        <<"key_id">> => <<"k1">>,
                        <<"key_base64">> => <<"abc">>
                    }}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, _Payload) -> {responded, success} end},
                {'error', 3, fun(_Req, Msg, Code) -> {responded, error, Msg, Code} end}
            ]}
        ],
        fun() ->
            Req0 = cowboy_req_ok,
            {ok, Result, _} = olm_handler:init(Req0, #{action => claim_key}),
            ?assertEqual({responded, success}, Result)
        end
    ).
