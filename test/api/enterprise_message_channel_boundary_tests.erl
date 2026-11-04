-module(enterprise_message_channel_boundary_tests).
-include_lib("eunit/include/eunit.hrl").

%% Exercise the public handler with the real locate_tx/3 and a synthetic Repo.
%% No application startup, database connection or real message writes.
channel_boundary_test_() ->
    [
        {"workspace located before grant and send", fun() ->
            check(
                42,
                {ok, #{<<"workspace_id">> => 73}},
                ok,
                <<"send_probe">>,
                [principal, locate, boundary, send]
            )
        end},
        {"missing or hidden channel stops before grant", fun() ->
            check(
                42,
                {error, not_found},
                ok,
                <<"resource_not_found">>,
                [principal, locate]
            )
        end},
        {"database error is not double wrapped", fun() ->
            check(
                42,
                {error, db_unavailable},
                ok,
                <<"internal_error">>,
                [principal, locate]
            )
        end},
        {"grant rejection stops before send", fun() ->
            check(
                42,
                {ok, #{<<"workspace_id">> => 73}},
                {error, denied},
                <<"grant_denied">>,
                [principal, locate, boundary]
            )
        end}
    ] ++
        [
            {"invalid channel id", fun() ->
                check(Id, unused, ok, <<"invalid_request">>, [principal])
            end}
         || Id <- [0, -1, <<"42">>, undefined]
        ].

check(Id, RepoResult, GrantResult, ExpectedCode, ExpectedEvents) ->
    Modules = [
        cowboy_req,
        enterprise_internal_idempotency,
        elib_pg,
        enterprise_message_logic,
        channel_repo,
        enterprise_internal_boundary,
        enterprise_channel_message_logic,
        enterprise_internal_error,
        enterprise_webhook_logic
    ],
    erase(boundary_events),
    try
        [meck:new(M, [non_strict]) || M <- Modules],
        mocks(Id, RepoResult, GrantResult),
        Req = #{enterprise_internal_body => <<>>},
        State = #{
            action => channel,
            channel_id => Id,
            enterprise_internal => #{organization_id => 11}
        },
        ?assertEqual(
            {ok, ExpectedCode, maps:remove(action, State)},
            enterprise_message_handler:init(Req, State)
        ),
        ?assertEqual(ExpectedEvents, lists:reverse(get(boundary_events))),
        [?assert(meck:validate(M)) || M <- Modules]
    after
        [meck:unload(M) || M <- Modules],
        erase(boundary_events)
    end.

mocks(Id, RepoResult, GrantResult) ->
    meck:expect(cowboy_req, method, fun(_) -> <<"POST">> end),
    meck:expect(cowboy_req, header, fun(_, _) -> <<"synthetic-key">> end),
    meck:expect(
        enterprise_internal_idempotency,
        request_digest,
        fun(_, _, _) -> {ok, <<"digest">>} end
    ),
    meck:expect(
        enterprise_internal_idempotency,
        begin_tx,
        fun(test_conn, _, _, _, _) -> {ok, inserted} end
    ),
    meck:expect(elib_pg, with_tx, fun(F) ->
        try
            F(test_conn)
        catch
            throw:{rollback, _} = R -> R
        end
    end),
    meck:expect(
        enterprise_message_logic,
        with_principal_tx,
        fun(test_conn, Ctx) ->
            event(principal),
            Ctx#{principal => resolved}
        end
    ),
    meck:expect(channel_repo, internal_find_tx, fun(test_conn, 11, ActualId) ->
        ?assertEqual(Id, ActualId),
        event(locate),
        RepoResult
    end),
    meck:expect(
        enterprise_internal_boundary,
        enforce,
        fun(test_conn, #{principal := resolved}, <<"INT-50">>, 73) ->
            event(boundary),
            GrantResult
        end
    ),
    meck:expect(
        enterprise_internal_boundary,
        error_code,
        fun(denied) -> <<"grant_denied">> end
    ),
    meck:expect(
        enterprise_channel_message_logic,
        channel_tx,
        fun(test_conn, #{principal := resolved}, ActualId, #{}) ->
            ?assertEqual(Id, ActualId),
            event(send),
            {error, {<<"send_probe">>, synthetic_stop}}
        end
    ),
    meck:expect(
        enterprise_webhook_logic,
        emit_event_failed,
        fun(_, _, _, _) -> ok end
    ),
    meck:expect(enterprise_internal_error, reply, fun(_, Code) -> Code end).

event(Event) ->
    Events =
        case get(boundary_events) of
            undefined -> [];
            E -> E
        end,
    put(boundary_events, [Event | Events]),
    ok.
