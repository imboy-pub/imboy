%%%-------------------------------------------------------------------
%%% LEE-5 故障合同回归（MOCK，不是数据库/跨节点原子性证明）。
%%% Real handler -> DS -> repo; only SQL/transport/signing/device boundaries are mocked.
%%% 验证零行与 SQL 故障分离、所有错误入口 503、无凭据/通知、无自动重试。
%%% Real PostgreSQL and multi-node evidence remain in the acceptance suite/dcheck.
%%%-------------------------------------------------------------------
-module(qr_login_pg_failure_tests).
-include_lib("eunit/include/eunit.hrl").
-include("error_code.hrl").

-define(ST, <<"lee5-fault-session">>).
-define(QT, <<"lee5-fault-qr">>).
-define(UID, 990778).
-define(TOKEN, <<"synthetic-lee5-fault-token">>).
-define(VERSION_KEY, {imboy, qr_login_pg_version_num}).
-define(MOCKS, [elib_pg, cowboy_req, elib_response, qr_login_logic, token_ds, qr_login_event_ds]).

pg_failure_contract_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun repo_write_errors_are_not_zero_rows/0,
        fun repo_zero_rows_keep_business_results/0,
        fun cancel_recheck_error_is_not_idempotent_success/0,
        fun lookup_and_transition_errors_return_503/0,
        fun scan_fallback_lookup_error_returns_503/0,
        fun confirm_each_storage_stage_returns_503/0,
        fun cancel_handler_recheck_error_returns_503/0,
        fun sse_storage_errors_never_echo_event_credentials/0,
        fun uncertain_claim_is_not_retried_or_reissued/0
    ]}.

setup() ->
    Saved = #{
        version => persistent_term:get(?VERSION_KEY, undefined),
        authority => application:get_env(imboy, qr_login_authority)
    },
    persistent_term:put(?VERSION_KEY, 180000),
    application:set_env(imboy, qr_login_authority, pg),
    lists:foreach(fun(M) -> meck:new(M, [no_link]) end, ?MOCKS),
    meck:expect(cowboy_req, method, fun(Req) -> maps:get(method, Req) end),
    meck:expect(cowboy_req, read_body, fun(Req) -> {ok, maps:get(body, Req), Req} end),
    meck:expect(cowboy_req, parse_qs, fun(Req) -> maps:get(qs, Req) end),
    meck:expect(cowboy_req, stream_body, fun(_, _, _) -> ok end),
    meck:expect(elib_response, error, fun(Req, Msg, Code) ->
        Req#{result => {error, Msg, Code}}
    end),
    meck:expect(elib_response, success, fun(Req, Payload) ->
        Req#{result => {success, Payload}}
    end),
    meck:expect(qr_login_logic, log_device_login, fun(_, _, _, _) -> ok end),
    meck:expect(token_ds, encrypt_token, fun(_, _) -> ?TOKEN end),
    meck:expect(qr_login_event_ds, notify, fun(_, _) -> error(unexpected_notification) end),
    Saved.

cleanup(#{version := Version, authority := Authority}) ->
    lists:foreach(fun(M) -> meck:unload(M) end, lists:reverse(?MOCKS)),
    case Version of
        undefined -> persistent_term:erase(?VERSION_KEY);
        _ -> persistent_term:put(?VERSION_KEY, Version)
    end,
    case Authority of
        undefined -> application:unset_env(imboy, qr_login_authority);
        {ok, Value} -> application:set_env(imboy, qr_login_authority, Value)
    end.

%% SQL errors propagate as pg_error. In particular, cancel must not read again
%% after an uncertain UPDATE and turn the failure into an idempotent success.
repo_write_errors_are_not_zero_rows() ->
    Counter = query_sequence(lists:duplicate(4, {error, closed})),
    Expected = {error, {pg_error, closed}},
    ?assertEqual(Expected, qr_login_session_repo:transition_scan(?ST, ?UID)),
    ?assertEqual(Expected, qr_login_session_repo:transition_confirm(?ST, ?UID, ?TOKEN)),
    ?assertEqual(Expected, qr_login_session_repo:transition_cancel(?ST)),
    ?assertEqual(Expected, qr_login_session_repo:claim_login_token(?ST)),
    ?assertEqual(4, atomics:get(Counter, 1)).

repo_zero_rows_keep_business_results() ->
    Counter = query_sequence(lists:duplicate(5, {ok, []})),
    ?assertEqual({error, not_transitioned}, qr_login_session_repo:transition_scan(?ST, ?UID)),
    ?assertEqual(
        {error, not_transitioned}, qr_login_session_repo:transition_confirm(?ST, ?UID, ?TOKEN)
    ),
    ?assertEqual({error, not_transitioned}, qr_login_session_repo:transition_cancel(?ST)),
    ?assertEqual({error, not_claimable}, qr_login_session_repo:claim_login_token(?ST)),
    ?assertEqual(5, atomics:get(Counter, 1)).

cancel_recheck_error_is_not_idempotent_success() ->
    Counter = query_sequence([{ok, []}, {error, closed}]),
    ?assertEqual({error, {pg_error, closed}}, qr_login_session_repo:transition_cancel(?ST)),
    ?assertEqual(2, atomics:get(Counter, 1)).

lookup_and_transition_errors_return_503() ->
    %% status read, QR reverse lookup (scan/confirm), and cancel UPDATE.
    lists:foreach(
        fun(Action) -> assert_http_failure(Action, [{error, closed}]) end,
        [status, scan, confirm, cancel]
    ),
    %% status read succeeds, but the atomic claim itself fails.
    assert_http_failure(status, [{ok, [confirmed()]}, {error, closed}]),
    %% scan reverse lookup succeeds, but the scan CAS fails.
    assert_http_failure(scan, [{ok, [qr_mapping()]}, {error, closed}]),
    ?assertNot(meck:called(token_ds, encrypt_token, ['_', '_'])),
    ?assertNot(meck:called(qr_login_event_ds, notify, ['_', '_'])).

scan_fallback_lookup_error_returns_503() ->
    assert_http_failure(scan, [{ok, [qr_mapping()]}, {ok, []}, {error, closed}]).

confirm_each_storage_stage_returns_503() ->
    Mapping = {ok, [qr_mapping()]},
    Scanned = {ok, [scanned()]},
    %% First session read and second device/purpose read fail independently.
    assert_http_failure(confirm, [Mapping, {error, closed}]),
    assert_http_failure(confirm, [Mapping, Scanned, {error, closed}]),
    ?assertNot(meck:called(token_ds, encrypt_token, ['_', '_'])),
    %% Signing remains before the existing confirm CAS; failure must not notify
    %% or retry the write/signing operation, nor put a token in the response.
    assert_http_failure(confirm, [Mapping, Scanned, Scanned, {error, closed}]),
    ?assertEqual(1, meck:num_calls(token_ds, encrypt_token, ['_', '_'])),
    ?assertNot(meck:called(qr_login_event_ds, notify, ['_', '_'])).

cancel_handler_recheck_error_returns_503() ->
    %% Repo's idempotency recheck fails.
    assert_http_failure(cancel, [{ok, []}, {error, closed}]),
    %% Repo recheck sees confirmed; the handler's later classification read fails.
    assert_http_failure(cancel, [{ok, []}, {ok, [confirmed()]}, {error, closed}]).

sse_storage_errors_never_echo_event_credentials() ->
    lists:foreach(
        fun(Status) ->
            Counter = query_sequence([{error, closed}]),
            meck:reset(cowboy_req),
            ?assertMatch(
                {stop, _, _},
                qr_login_sse_handler:info(
                    #{<<"status">> => Status, <<"token">> => ?TOKEN, <<"uid">> => ?UID},
                    #{},
                    #{session_token => ?ST}
                )
            ),
            Chunks = [
                iolist_to_binary(Body)
             || {_, {cowboy_req, stream_body, [Body, _, _]}, _} <- meck:history(cowboy_req)
            ],
            ?assertEqual([<<": service_unavailable\n\n">>], Chunks),
            ?assertEqual(1, atomics:get(Counter, 1))
        end,
        [<<"confirmed_ready">>, <<"confirmed">>]
    ).

%% Boundary mock of a lost commit acknowledgement, not a database durability test.
%% A subsequent request still claims atomically and observes the consumed state.
uncertain_claim_is_not_retried_or_reissued() ->
    Counter = query_sequence([
        {ok, [confirmed()]},
        {error, closed},
        {ok, [confirmed()]},
        {ok, []}
    ]),
    ?assertMatch({error, _, ?ERR_SERVICE_UNAVAILABLE}, request(status)),
    ?assertEqual(2, atomics:get(Counter, 1)),
    ?assertMatch({error, _, ?ERR_QR_LOGIN_ALREADY_USED}, request(status)),
    ?assertEqual(4, atomics:get(Counter, 1)),
    ?assertNot(meck:called(token_ds, encrypt_token, ['_', '_'])).

assert_http_failure(Action, Replies) ->
    Counter = query_sequence(Replies),
    ?assertEqual(
        {error, <<"服务暂不可用，请稍后再试"/utf8>>, ?ERR_SERVICE_UNAVAILABLE},
        request(Action)
    ),
    ?assertEqual(length(Replies), atomics:get(Counter, 1)).

query_sequence(Replies) ->
    Answers = list_to_tuple(Replies),
    Counter = atomics:new(1, []),
    meck:expect(elib_pg, query, fun(_, _) ->
        %% Exhaustion raises: unexpected retries/extra reads cannot silently pass.
        element(atomics:add_get(Counter, 1, 1), Answers)
    end),
    Counter.

request(Action) ->
    Method =
        case Action of
            status -> <<"GET">>;
            _ -> <<"POST">>
        end,
    Req = #{
        method => Method,
        body => jsone:encode(#{<<"qr_token">> => ?QT, <<"session_token">> => ?ST}),
        qs => [{<<"session_token">>, ?ST}]
    },
    {stop, Out, _} = qr_login_handler:handle_request(Req, #{action => Action, current_uid => ?UID}),
    maps:get(result, Out).

qr_mapping() ->
    #{<<"session_token">> => ?ST}.

confirmed() ->
    #{<<"status">> => <<"confirmed">>, <<"scanned_by">> => ?UID}.

scanned() ->
    #{
        <<"status">> => <<"scanned">>,
        <<"scanned_by">> => ?UID,
        <<"device_id">> => <<"fault-device">>,
        <<"device_name">> => <<"Fault Test">>,
        <<"platform">> => <<"ios">>,
        <<"purpose">> => <<"human">>
    }.
