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
%% 真缓存套件（R1/R2/R3-cache/R6）——depcache 真库，claim/global:trans/
%% TTL 全走真实实现，仅 mock 传输面与 DS 层。
%% ===================================================================

grace_window_real_cache_test_() ->
    {setup, fun real_cache_setup/0, fun real_cache_cleanup/1, fun(_Ctx) ->
        {timeout, 30, ?_test(real_cache_cases())}
    end}.

real_cache_cases() ->
    with_mocks(handler_mocks(), fun() ->
        R1 = poll_idempotent_after_delivery(),
        r2_second_claim_rejected(R1),
        r6_concurrent_claims_single_winner(),
        r3_evaporated_key_is_5203_not_403()
    end).

%% R1（主反例）：cache create → scan → confirm → poll#1（200 confirmed+token）
%% → sleep 2s → poll#2 必须 200 {status: confirmed} 无 token，不得 403。
poll_idempotent_after_delivery() ->
    #{session_token := ST, qr_token := QRTok} = web_create(),
    {success, Scanned} = app_call(scan, #{<<"qr_token">> => QRTok}),
    ?assertEqual(<<"scanned">>, maps:get(<<"status">>, Scanned)),
    {success, Confirmed} = app_call(confirm, #{<<"qr_token">> => QRTok}),
    ?assertEqual(#{<<"status">> => <<"confirmed">>}, Confirmed),
    %% poll#1：web_human 面领取（一次性交付）。
    {success, P1} = web_call(status, ST),
    ?assertEqual(<<"confirmed">>, maps:get(<<"status">>, P1)),
    ?assertEqual(?TOKEN, maps:get(<<"token">>, P1)),
    ?assertEqual(?UID, maps:get(<<"uid">>, P1)),
    %% App 下一轮 2s 轮询：必须幂等 confirmed，不得 403。
    timer:sleep(2000),
    {success, P2} = web_call(status, ST),
    ?assertEqual(#{<<"status">> => <<"confirmed">>}, P2),
    #{session_token => ST, qr_token => QRTok}.

%% R2：墓碑后二次领取拒绝（token 不重复发放）。
r2_second_claim_rejected(#{session_token := ST}) ->
    ?assertEqual(
        {error, not_claimable},
        qr_login_logic:claim_web_cache_token(ST)
    ),
    {ok, Tombstone} = imboy_cache:get({qr_login, ST}),
    ?assertEqual(<<"confirmed">>, maps:get(<<"status">>, Tombstone)),
    ?assertEqual(null, maps:get(<<"login_token">>, Tombstone)),
    ?assert(is_integer(maps:get(<<"claimed_at">>, Tombstone))),
    %% 宽限窗内轮询保持幂等 200。
    ?assertEqual({success, #{<<"status">> => <<"confirmed">>}}, web_call(status, ST)).

%% R6：同 session_token 并发双 claim（global:trans）——恰好一个赢家。
r6_concurrent_claims_single_winner() ->
    ST = <<"grace-r6-", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    Session = confirmed_session(ST),
    ok = imboy_cache:set({qr_login, ST}, Session, 60),
    Parent = self(),
    Ref = make_ref(),
    Pids = [
        spawn(fun() ->
            _ = depcache:in_process(true),
            Parent ! {Ref, self(), qr_login_logic:claim_web_cache_token(ST)}
        end)
     || _ <- [1, 2]
    ],
    Results = [
        receive
            {Ref, P, R} -> {P, R}
        after 5000 -> error(claim_timeout)
        end
     || _ <- Pids
    ],
    Winners = [P || {_, {ok, #{login_token := ?TOKEN, scanned_by := ?UID}}} = P <- Results],
    Losers = [P || {_, {error, not_claimable}} = P <- Results],
    ?assertEqual(1, length(Winners)),
    ?assertEqual(1, length(Losers)),
    {ok, Tombstone} = imboy_cache:get({qr_login, ST}),
    ?assertEqual(null, maps:get(<<"login_token">>, Tombstone)),
    ok = imboy_cache:delete({qr_login, ST}).

%% R3（cache 面）：键已蒸发（TTL 过期）→ 5203 幂等，不得 403。
r3_evaporated_key_is_5203_not_403() ->
    ST = <<"grace-evaporated-never-created">>,
    ?assertEqual(undefined, imboy_cache:get({qr_login, ST})),
    {error, Msg, Code} = web_call_raw(status, ST),
    ?assertEqual(?ERR_QR_LOGIN_ALREADY_USED, Code),
    ?assert(?ERR_FORBIDDEN =/= Code),
    ?assertEqual(<<"二维码已确认，请刷新"/utf8>>, Msg).

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
                    {imboy_cache, [
                        {get, fun({qr_login, _ST}) ->
                            %% purpose=seat：web_human 面不匹配 → 真 forbidden。
                            {ok, (confirmed_session(<<"seat-only">>))#{
                                <<"purpose">> => <<"seat">>
                            }}
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

%% web_human 面 create → #{{session_token, qr_token}}。
web_create() ->
    Body = jsone:encode(
        #{
            <<"device_id">> => <<"grace-device">>,
            <<"device_name">> => <<"Chrome">>
        },
        [native_utf8]
    ),
    {stop, Req, _} = qr_login_handler:handle_request(
        req(<<"POST">>, Body, []), web_state(create)
    ),
    {success, Payload} = result(Req),
    #{
        session_token => maps:get(<<"session_token">>, Payload),
        qr_token => maps:get(<<"qr_token">>, Payload)
    }.

%% 手机端（JWT 面）scan/confirm。
app_call(Action, Data) ->
    Body = jsone:encode(Data, [native_utf8]),
    {stop, Req, _} = qr_login_handler:handle_request(
        req(<<"POST">>, Body, []), #{action => Action, current_uid => ?UID}
    ),
    result(Req).

%% web_human 面 status（query 走 session_token）。
web_call(status, ST) ->
    web_call_raw(status, ST).

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

real_cache_setup() ->
    ?assertNot(lists:keymember(imboy, 1, application:which_applications())),
    ?assertEqual(undefined, whereis(imboy_cache)),
    %% DEF-F6 R6 前置：claim 的 global:trans 互斥依赖 global_name_server。
    %% eunit VM 以 `-boot no_dot_erlang -kernel start_distribution false`
    %% 极简引导（Makefile ERL），global 不随内核启动——生产 vm.args 是
    %% `-name imboy@...` 分布式启动、global 在线，契约一致化后再跑并发用例。
    _ = (catch global:start()),
    ?assert(is_pid(whereis(global_name_server))),
    Values = [
        {dsync_enabled, false},
        {cache_fenced_namespaces, []},
        {cache_registry_overrides, #{}},
        {qr_login_authority, cache}
    ],
    Saved = [{K, application:get_env(imboy, K)} || {K, _} <- Values],
    lists:foreach(fun({K, V}) -> application:set_env(imboy, K, V) end, Values),
    try depcache:start_link(imboy_cache, #{memory_max => 1}) of
        {ok, Pid} ->
            unlink(Pid),
            {links, Linked} = process_info(Pid, links),
            Children = lists:delete(self(), Linked),
            #{cache_pid => Pid, children => Children, saved_env => Saved}
    catch
        Class:Reason:Stack ->
            restore_env(Saved),
            erlang:raise(Class, Reason, Stack)
    end.

real_cache_cleanup(#{cache_pid := Pid, children := Children, saved_env := Saved}) ->
    Refs = [{P, erlang:monitor(process, P)} || P <- [Pid | Children]],
    try
        ok = gen_server:stop(Pid, shutdown, 5000),
        lists:foreach(
            fun({P, Ref}) ->
                receive
                    {'DOWN', Ref, process, P, shutdown} -> ok
                after 5000 -> error({owned_cache_process_not_stopped, P})
                end
            end,
            Refs
        ),
        ?assertEqual(undefined, whereis(imboy_cache))
    after
        restore_env(Saved)
    end.

restore_env(Saved) ->
    lists:foreach(
        fun
            ({K, undefined}) -> application:unset_env(imboy, K);
            ({K, {ok, V}}) -> application:set_env(imboy, K, V)
        end,
        Saved
    ).
