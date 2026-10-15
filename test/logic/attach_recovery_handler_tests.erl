-module(attach_recovery_handler_tests).
-include_lib("eunit/include/eunit.hrl").

recovery_boundary_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun own_terminal_is_conflict/0,
        fun foreign_terminal_is_hidden/0,
        fun database_failure_is_unavailable/0,
        fun revoked_status_is_hidden/0
    ]}.

setup() ->
    lists:foreach(
        fun(M) -> meck:new(M, [non_strict, no_link]) end,
        [cowboy_req, auth_ds, attach_pending_repo, attach_logic, elib_response]
    ),
    meck:expect(cowboy_req, method, fun(_) -> <<"GET">> end),
    meck:expect(auth_ds, current_uid, fun(_) -> 7 end),
    meck:expect(elib_response, error, fun(Req, _, Code) -> Req#{code => Code} end).

cleanup(_) ->
    lists:foreach(
        fun meck:unload/1,
        [cowboy_req, auth_ds, attach_pending_repo, attach_logic, elib_response]
    ).

request(Operation, Lookup) ->
    meck:expect(cowboy_req, parse_qs, fun(_) ->
        [{<<"operation">>, Operation}, {<<"object_key">>, <<"fixture">>}]
    end),
    meck:expect(attach_pending_repo, status_lookup, fun(<<"fixture">>) -> Lookup end),
    {ok, Req, _} = attach_handler:init(#{}, #{action => presign}),
    maps:get(code, Req).

own_terminal_is_conflict() ->
    ?assertEqual(
        409,
        request(
            <<"renew">>, {ok, #{<<"creator_user_id">> => 7, <<"session_state">> => <<"confirmed">>}}
        )
    ).

foreign_terminal_is_hidden() ->
    ?assertEqual(
        404,
        request(
            <<"renew">>, {ok, #{<<"creator_user_id">> => 8, <<"session_state">> => <<"confirmed">>}}
        )
    ).

database_failure_is_unavailable() ->
    lists:foreach(
        fun(Op) ->
            ?assertEqual(503, request(Op, {error, database_down}))
        end,
        [<<"status">>, <<"renew">>]
    ).

revoked_status_is_hidden() ->
    meck:expect(
        attach_logic,
        reauthorize_upload_session,
        fun(7, _) -> {error, forbidden} end
    ),
    ?assertEqual(404, request(<<"status">>, {ok, #{<<"creator_user_id">> => 7}})).
