-module(qr_login_status_grace_window_tests).
-include_lib("eunit/include/eunit.hrl").
-include("error_code.hrl").

%%%===================================================================
%%% @doc
%%% DEF-F6-QR-STATUS-403-FREEZE 回归（提案：runs/uxfix-batch4-20261116/
%%% f6-backend-proposal.md 第四节 R1-R6）。
%%%
%%% 缺陷（4/4 确定性）：cache 权威 confirmed 交付 token 即删会话 → App 2s
%%% 轮询踩 `with_web_session` not_found → 泛化 403 "Forbidden QR session"
%%% → 端上 StopSilently 冻结「已扫描」态，QR 登录全链死。
%%%
%%% 修复口径：
%%% - 改动点1：status/cancel 的 web_human 面 not_found → 既有 5203
%%%   （二维码已确认，请刷新），403 仅保留真 forbidden（purpose/platform 不匹配）；
%%% - 改动点2：cache claim 不删除会话，改写无 token 的 confirmed 墓碑
%%%   （TTL 120s ≥ 轮询宽限窗），一次性交付语义不变。
%%%===================================================================

-define(UID, 111174215241304064).
-define(TOKEN, <<"grace-window-tk">>).
-define(WEB, #{jwt_purpose => web_human}).

%% ===================================================================
%% 注：原 cache 权威时代真缓存套件（R1/R2/R3-cache/R6，authority=cache +
%% app 未启动假设）已随 LEE-5 权威收敛删除——合并版 qr_login_authority 恒 pg
%% （fail-closed），cache 领取路径不复存在；其行为合同（poll 幂等/墓碑重领
%% 拒绝/并发单赢家）由 pg 权威面 r3_pg/r4/r5 与 qr_login_uid_contract 套件
%% 覆盖（2026-11-19 首次真跑定性，v5 实证）。
%% ===================================================================
%% PG 权威面（R3-pg / R4）与真 forbidden 保持（R5）——meck DS。
%% ===================================================================

r3_pg_expired_row_is_5203_not_403_test_() ->
    {timeout, 15, fun() ->
        with_mocks(
            handler_mocks() ++
                [
                    {qr_login_session_ds, [
                        {find_by_session_token, fun(_ST) -> undefined end}
                    ]}
                ],
            fun() ->
                with_pg_authority(fun() ->
                    %% 行过期/不存在：with_web_session not_found → 5203（改动点1）。
                    {error, Msg, Code} = web_call_raw(status, <<"pg-expired-row">>),
                    ?assertEqual(?ERR_QR_LOGIN_ALREADY_USED, Code),
                    ?assertEqual(<<"二维码已确认，请刷新"/utf8>>, Msg)
                end)
            end
        )
    end}.

r4_pg_consumed_claim_is_5203_not_403_test_() ->
    {timeout, 15, fun() ->
        with_mocks(
            handler_mocks() ++
                [
                    {qr_login_session_ds, [
                        {find_by_session_token, fun(_ST) ->
                            confirmed_session(<<"pg-consumed">>)
                        end},
                        {claim_login_token, fun(_ST) -> {error, not_claimable} end}
                    ]}
                ],
            fun() ->
                with_pg_authority(fun() ->
                    %% 已消费（consumed_at 置位）→ 既有 ALREADY_USED 兼容响应。
                    {error, _Msg, Code} = web_call_raw(status, <<"pg-consumed">>),
                    ?assertEqual(?ERR_QR_LOGIN_ALREADY_USED, Code),
                    ?assert(?ERR_FORBIDDEN =/= Code)
                end)
            end
        )
    end}.

r5_true_forbidden_preserved_test_() ->
    {timeout, 15, fun() ->
        with_mocks(
            handler_mocks() ++
                [
                    {qr_login_session_ds, [
                        {find_by_session_token, fun(_ST) ->
                            %% purpose=seat：web_human 面不匹配 → 真 forbidden。
                            %% LEE-5 合并后权威恒 pg，会话门读 DS 行而非 cache，
                            %% 原 imboy_cache mock 已是死路径（nightly 2026-11-21/22
                            %% 实测 403 退化为 5203 的根因）。
                            (confirmed_session(<<"seat-only">>))#{
                                <<"purpose">> => <<"seat">>
                            }
                        end}
                    ]}
                ],
            fun() ->
                {error, Msg, Code} = web_call_raw(status, <<"seat-session">>),
                ?assertEqual(?ERR_FORBIDDEN, Code),
                ?assertEqual(<<"Forbidden QR session">>, Msg)
            end
        )
    end}.

%% ===================================================================
%% Helpers
%% ===================================================================

confirmed_session(ST) ->
    #{
        <<"session_token">> => ST,
        <<"status">> => <<"confirmed">>,
        <<"login_token">> => ?TOKEN,
        <<"scanned_by">> => ?UID,
        <<"purpose">> => <<"human">>,
        <<"platform">> => <<"web">>,
        <<"device_id">> => <<"grace-device">>,
        <<"device_name">> => <<"Chrome">>,
        <<"expires_at">> => erlang:system_time(millisecond) + 60000
    }.

web_call_raw(status, ST) ->
    {stop, Req, _} = qr_login_handler:handle_request(
        req(<<"GET">>, <<>>, [{<<"session_token">>, ST}]),
        web_state(status)
    ),
    result(Req).

web_state(Action) ->
    maps:merge(?WEB, #{action => Action}).

req(Method, Body, Qs) ->
    #{method => Method, body => Body, qs => Qs, response_result => unset}.

result(Req) ->
    case maps:get(response_result, Req) of
        {success, Payload} -> {success, Payload};
        {error, Msg, Code} -> {error, Msg, Code};
        Other -> error({unexpected_response, Other})
    end.

handler_mocks() ->
    [
        {cowboy_req, [
            {method, fun(Req) -> maps:get(method, Req) end},
            {read_body, fun(Req) -> {ok, maps:get(body, Req), Req} end},
            {parse_qs, fun(Req) -> maps:get(qs, Req) end}
        ]},
        {elib_response, [
            {success, fun(Req, Payload) -> Req#{response_result => {success, Payload}} end},
            {success, fun(Req, Payload, _) -> Req#{response_result => {success, Payload}} end},
            {error, fun(Req, Msg, Code) -> Req#{response_result => {error, Msg, Code}} end},
            {error_with_status, fun(Req, Status, Msg, Code) ->
                Req#{response_result => {error_with_status, Status, Msg, Code}}
            end}
        ]},
        %% confirm 真链路依赖（qr_login_logic:log_device_login 真跑）：
        {user_ds, [
            {find_by_id, fun(?UID, <<"status,account_type">>) ->
                #{<<"status">> => 1, <<"account_type">> => 0}
            end}
        ]},
        {auth_session_ds, [
            {current_epoch, fun(?UID) -> {ok, 7} end}
        ]},
        {token_ds, [
            {encrypt_web_token, fun(?UID, <<"grace-device">>, 7, 456) -> ?TOKEN end}
        ]},
        {user_device_ds, [
            {register_web, fun(_Now, ?UID, <<"grace-device">>, _Vals) -> {ok, 456} end}
        ]}
    ].

with_pg_authority(Test) ->
    application:set_env(imboy, qr_login_authority, pg),
    try
        Test()
    after
        application:unset_env(imboy, qr_login_authority)
    end.

with_mocks(Mocks, Test) ->
    try
        lists:foreach(
            fun({Module, Expectations}) ->
                Options =
                    case Module of
                        qr_login_logic -> [passthrough, no_link, non_strict, unstick];
                        _ -> [no_link, non_strict, unstick]
                    end,
                meck:new(Module, Options),
                lists:foreach(
                    fun({Name, Fun}) -> meck:expect(Module, Name, Fun) end,
                    Expectations
                )
            end,
            Mocks
        ),
        Test()
    after
        lists:foreach(
            fun({Module, _}) -> catch meck:unload(Module) end,
            lists:reverse(Mocks)
        )
    end.

%% --- 真 depcache fixture（对齐 web_human_bootstrap_login_tests 惯式）---
