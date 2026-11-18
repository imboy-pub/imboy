-module(qr_login_status_grace_window_tests).
-include_lib("eunit/include/eunit.hrl").
-include("error_code.hrl").

%%% DEF-F6-QR-STATUS-403-FREEZE：Web 轮询兼容性，服从 LEE-5 PG 唯一权威。
%%% R1/R2/R4：首次 PG claim 交付凭据；重复领取返回既有 5203，不重发 token。
%%% R3：PG 行不存在/过期时 status/cancel 返回 5203，不泛化成 403。
%%% R5：purpose/platform 真不匹配仍为 403；PG 故障必须为 503。
%%% 本套件是真 handler/logic、mock PG DS 与传输边界，不是数据库并发证明。
%%% R6 单赢家由 qr_login_claim_acceptance_tests 的真实 PG polling/SSE/混合
%%% 竞争与双 peer dcheck 验证；这里不再启动 cache 或模拟 cache 原子领取。

-define(UID, 111174215241304064).
-define(TOKEN, <<"grace-window-tk">>).

r1_r2_poll_delivers_once_then_returns_5203_test_() ->
    {timeout, 15, fun() ->
        ST = <<"grace-pg-once">>,
        Claims = atomics:new(1, []),
        %% 会话读取不是凭据来源；只能返回同一次 PG claim 的 token 与身份。
        Session = (confirmed_session(ST))#{
            <<"login_token">> => <<"stale-row-token">>, <<"scanned_by">> => 999
        },
        with_mocks(
            session_mocks(ST, Session, fun(ST0) when ST0 =:= ST ->
                case atomics:add_get(Claims, 1, 1) of
                    1 -> {ok, #{login_token => ?TOKEN, scanned_by => ?UID}};
                    N when N =< 3 -> {error, not_claimable}
                end
            end),
            fun() ->
                ?assertEqual(
                    {success, #{
                        <<"status">> => <<"confirmed">>,
                        <<"token">> => ?TOKEN,
                        <<"uid">> => ?UID,
                        <<"uid_str">> => integer_to_binary(?UID)
                    }},
                    web_call(status, ST)
                ),
                %% 后续轮询不得重发 token，也不得把已消费映射为 forbidden。
                ?assertEqual(already_used(), web_call(status, ST)),
                ?assertEqual(already_used(), web_call(status, ST)),
                ?assertEqual(3, atomics:get(Claims, 1))
            end
        )
    end}.

r3_missing_pg_session_is_5203_not_403_test_() ->
    {timeout, 15, fun() ->
        lists:foreach(
            fun(Action) ->
                ST = <<"grace-pg-expired">>,
                with_mocks(session_mocks(ST, undefined, unexpected_claim()), fun() ->
                    ?assertEqual(already_used(), web_call(Action, ST)),
                    ?assertEqual(
                        1, meck:num_calls(qr_login_session_ds, find_by_session_token, [ST])
                    ),
                    ?assertEqual(0, meck:num_calls(qr_login_session_ds, claim_login_token, '_')),
                    ?assertEqual(0, meck:num_calls(qr_login_session_ds, transition_cancel, '_'))
                end)
            end,
            [status, cancel]
        )
    end}.

r4_pg_consumed_claim_is_5203_not_403_test_() ->
    {timeout, 15, fun() ->
        ST = <<"grace-pg-consumed">>,
        Session = (confirmed_session(ST))#{<<"login_token">> => null, <<"consumed_at">> => 1},
        with_mocks(
            session_mocks(ST, Session, fun(ST0) when ST0 =:= ST -> {error, not_claimable} end),
            fun() ->
                ?assertEqual(already_used(), web_call(status, ST)),
                ?assertEqual(1, meck:num_calls(qr_login_session_ds, claim_login_token, [ST]))
            end
        )
    end}.

r5_true_purpose_or_platform_mismatch_remains_forbidden_test_() ->
    {timeout, 15, fun() ->
        ST = <<"grace-pg-forbidden">>,
        Sessions = [
            (confirmed_session(ST))#{<<"purpose">> => <<"seat">>},
            (confirmed_session(ST))#{<<"platform">> => <<"ios">>},
            maps:remove(<<"purpose">>, confirmed_session(ST))
        ],
        lists:foreach(
            fun({Action, Session}) ->
                with_mocks(session_mocks(ST, Session, unexpected_claim()), fun() ->
                    ?assertEqual(
                        {error, <<"Forbidden QR session">>, ?ERR_FORBIDDEN},
                        web_call(Action, ST)
                    ),
                    ?assertEqual(0, meck:num_calls(qr_login_session_ds, claim_login_token, '_')),
                    ?assertEqual(0, meck:num_calls(qr_login_session_ds, transition_cancel, '_'))
                end)
            end,
            [{Action, Session} || Action <- [status, cancel], Session <- Sessions]
        )
    end}.

pg_web_session_lookup_failure_is_503_test_() ->
    {timeout, 15, fun() ->
        lists:foreach(
            fun(Action) ->
                ST = <<"grace-pg-unavailable">>,
                with_mocks(
                    session_mocks(ST, {error, {pg_error, closed}}, unexpected_claim()),
                    fun() ->
                        ?assertEqual(
                            {error_with_status, 503, <<"QR session unavailable">>, 503},
                            web_call(Action, ST)
                        ),
                        ?assertEqual(
                            1, meck:num_calls(qr_login_session_ds, find_by_session_token, [ST])
                        ),
                        ?assertEqual(
                            0, meck:num_calls(qr_login_session_ds, claim_login_token, '_')
                        ),
                        ?assertEqual(0, meck:num_calls(qr_login_session_ds, transition_cancel, '_'))
                    end
                )
            end,
            [status, cancel]
        )
    end}.

pg_status_read_failure_after_web_validation_is_503_test_() ->
    {timeout, 15, fun() ->
        ST = <<"grace-pg-read-failure">>,
        Reads = atomics:new(1, []),
        Replies = {confirmed_session(ST), {error, {pg_error, closed}}},
        with_mocks(
            [
                {qr_login_session_ds, [
                    {find_by_session_token, fun(ST0) when ST0 =:= ST ->
                        element(atomics:add_get(Reads, 1, 1), Replies)
                    end},
                    {claim_login_token, unexpected_claim()}
                ]}
            ],
            fun() ->
                ?assertEqual(service_unavailable(), web_call(status, ST)),
                ?assertEqual(2, atomics:get(Reads, 1)),
                ?assertEqual(0, meck:num_calls(qr_login_session_ds, claim_login_token, '_'))
            end
        )
    end}.

pg_claim_failure_is_503_without_retry_test_() ->
    {timeout, 15, fun() ->
        ST = <<"grace-pg-claim-failure">>,
        with_mocks(
            session_mocks(ST, confirmed_session(ST), fun(ST0) when ST0 =:= ST ->
                {error, {pg_error, closed}}
            end),
            fun() ->
                ?assertEqual(service_unavailable(), web_call(status, ST)),
                ?assertEqual(1, meck:num_calls(qr_login_session_ds, claim_login_token, [ST]))
            end
        )
    end}.

pg_cancel_failure_is_503_without_retry_test_() ->
    {timeout, 15, fun() ->
        ST = <<"grace-pg-cancel-failure">>,
        with_mocks(
            [
                {qr_login_session_ds, [
                    {find_by_session_token, fun(ST0) when ST0 =:= ST -> confirmed_session(ST) end},
                    {transition_cancel, fun(ST0) when ST0 =:= ST -> {error, {pg_error, closed}} end}
                ]}
            ],
            fun() ->
                ?assertEqual(service_unavailable(), web_call(cancel, ST)),
                ?assertEqual(1, meck:num_calls(qr_login_session_ds, transition_cancel, [ST])),
                ?assertEqual(1, meck:num_calls(qr_login_session_ds, find_by_session_token, [ST]))
            end
        )
    end}.

already_used() ->
    {error, <<"二维码已确认，请刷新"/utf8>>, ?ERR_QR_LOGIN_ALREADY_USED}.

service_unavailable() ->
    {error, <<"服务暂不可用，请稍后再试"/utf8>>, ?ERR_SERVICE_UNAVAILABLE}.

confirmed_session(ST) ->
    #{
        <<"session_token">> => ST,
        <<"status">> => <<"confirmed">>,
        <<"login_token">> => ?TOKEN,
        <<"scanned_by">> => ?UID,
        <<"purpose">> => <<"human">>,
        <<"platform">> => <<"web">>
    }.

session_mocks(ST, Session, Claim) ->
    [
        {qr_login_session_ds, [
            {find_by_session_token, fun(ST0) when ST0 =:= ST -> Session end},
            {claim_login_token, Claim},
            {transition_cancel, fun(_) -> error(unexpected_cancel) end}
        ]}
    ].

unexpected_claim() ->
    fun(_) -> error(unexpected_claim) end.

web_call(Action, ST) ->
    Method =
        case Action of
            status -> <<"GET">>;
            cancel -> <<"POST">>
        end,
    Req = #{
        method => Method,
        body => jsone:encode(#{<<"session_token">> => ST}),
        qs => [{<<"session_token">>, ST}]
    },
    {stop, Out, _} = qr_login_handler:handle_request(Req, #{
        jwt_purpose => web_human, action => Action
    }),
    maps:get(response_result, Out).

handler_mocks() ->
    [
        {cowboy_req, [
            {method, fun(Req) -> maps:get(method, Req) end},
            {read_body, fun(Req) -> {ok, maps:get(body, Req), Req} end},
            {parse_qs, fun(Req) -> maps:get(qs, Req) end}
        ]},
        {elib_response, [
            {success, fun(Req, Payload) -> Req#{response_result => {success, Payload}} end},
            {error, fun(Req, Msg, Code) -> Req#{response_result => {error, Msg, Code}} end},
            {error_with_status, fun(Req, Status, Msg, Code) ->
                Req#{response_result => {error_with_status, Status, Msg, Code}}
            end}
        ]},
        {imboy_cache, [
            {get, fun(_) -> error(unexpected_cache_read) end},
            {get_wait, fun(_) -> error(unexpected_cache_read) end},
            {set, fun(_, _, _) -> error(unexpected_cache_write) end},
            {set, fun(_, _, _, _) -> error(unexpected_cache_write) end},
            {delete, fun(_) -> error(unexpected_cache_delete) end}
        ]}
    ].

with_mocks(SessionMocks, Test) ->
    SavedAuthority = application:get_env(imboy, qr_login_authority),
    Mocks = handler_mocks() ++ SessionMocks,
    application:set_env(imboy, qr_login_authority, pg),
    try
        lists:foreach(
            fun({Module, Expectations}) ->
                meck:new(Module, [no_link, non_strict, unstick]),
                lists:foreach(
                    fun({Name, Fun}) -> meck:expect(Module, Name, Fun) end,
                    Expectations
                )
            end,
            Mocks
        ),
        Test(),
        ?assertEqual([], meck:history(imboy_cache)),
        lists:foreach(fun({Module, _}) -> ?assert(meck:validate(Module)) end, Mocks)
    after
        lists:foreach(fun({Module, _}) -> catch meck:unload(Module) end, lists:reverse(Mocks)),
        case SavedAuthority of
            undefined -> application:unset_env(imboy, qr_login_authority);
            {ok, Value} -> application:set_env(imboy, qr_login_authority, Value)
        end
    end.
