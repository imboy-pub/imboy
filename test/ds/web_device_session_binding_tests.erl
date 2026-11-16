-module(web_device_session_binding_tests).
-include_lib("eunit/include/eunit.hrl").

binding_rebuild_and_cross_face_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(Table) -> ?_test(check_binding(Table)) end}.

setup() ->
    Table = ets:new(web_binding_owned, [public, set]),
    ets:insert(Table, {row, 456, <<"web">>}),
    Modules = [config_ds, imboy_jwt, user_ds, auth_session_ds, elib_pg, elib_pg_sql],
    lists:foreach(fun(M) -> meck:new(M, [no_link]) end, Modules),
    meck:expect(config_ds, env, fun(jwt_key, <<>>) -> <<"unit-only">> end),
    meck:expect(imboy_jwt, sign, fun(C, _) -> term_to_binary(C) end),
    meck:expect(imboy_jwt, verify, fun(T, _) -> {ok, binary_to_term(T, [safe])} end),
    meck:expect(user_ds, find_by_id, fun(123, _) -> #{<<"status">> => 1, <<"account_type">> => 0} end),
    meck:expect(auth_session_ds, current_epoch, fun(123) -> {ok, 2} end),
    meck:expect(auth_session_ds, revoked, fun(123, 2) -> false end),
    meck:expect(elib_pg_sql, public_tablename, fun(_) -> <<"public.user_device">> end),
    meck:expect(elib_pg, query, fun(Sql, Args) ->
        Native = binary:match(Sql, <<"IS DISTINCT FROM 'web'">>) =/= nomatch,
        Active = case {Args, ets:lookup(Table, row)} of
            {[123, <<"web-1">>, Id], [{row, Id, <<"web">>}]} -> true;
            {[123, <<"web-1">>], [{row, _, Type}]} when Type =/= <<"web">> -> Native;
            _ -> false
        end,
        {ok, case Active of true -> [#{<<"active">> => 1}]; false -> [] end}
    end),
    {Table, Modules}.

cleanup({Table, Modules}) ->
    lists:foreach(fun meck:unload/1, lists:reverse(Modules)), ets:delete(Table).

check_binding({Table, _}) ->
    Pair = token_ds:issue_web_session(123, <<"web-1">>, 2, 456),
    Access = maps:get(<<"token">>, Pair), Refresh = maps:get(<<"refreshtoken">>, Pair),
    ?assertMatch({ok, 123, _, 2, _, 456}, auth_ds:verify_web_token(Access)),
    {ok, 123, <<"web-1">>, 2, _, VerifiedRow} = auth_ds:verify_web_refresh_token(Refresh),
    State = #{surface => web_human, jwt_purpose => web_human, web_auth => ws,
        current_uid => 123, current_did => <<"web-1">>, session_epoch => 2,
        token_expire_at => 9999999999, device_session_id => 456},
    ?assert(websocket_ds:session_still_valid(State)),
    ets:delete(Table, row),
    assert_old_rejected(Access, Refresh, State),
    %% No sleep: same-second reconstruction cannot change the old binding.
    ets:insert(Table, {row, 789, <<"web">>}),
    assert_old_rejected(Access, Refresh, State),
    RacedAccess = token_ds:encrypt_web_token(123, <<"web-1">>, 2, VerifiedRow),
    ?assertMatch({error, _, _}, auth_ds:verify_web_token(RacedAccess)),
    Fresh = token_ds:issue_web_session(123, <<"web-1">>, 2, 789),
    ?assertMatch({ok, 123, _, 2, _, 789}, auth_ds:verify_web_token(maps:get(<<"token">>, Fresh))),
    ?assertMatch({ok, 123, _, 2, _, 789}, auth_ds:verify_web_refresh_token(maps:get(<<"refreshtoken">>, Fresh))),
    lists:foreach(fun(Token) ->
        ?assertMatch({error, 706, _, _}, token_ds:decrypt_token(Token))
    end, [Access, Refresh, RacedAccess, maps:get(<<"token">>, Fresh)]),
    check_native_rejection(Access, Refresh),
    %% Pre-upgrade Web tickets have no dsid; trusted type blocks generic faces.
    Legacy = token_ds:issue_device_session(123, <<"web-1">>, 2),
    ?assertMatch({error, _, _}, auth_ds:verify_token(maps:get(<<"token">>, Legacy))),
    ?assertNot(user_device_ds:is_active_native(123, <<"web-1">>)),
    check_native_rejection(maps:get(<<"token">>, Legacy), maps:get(<<"refreshtoken">>, Legacy)),
    ?assertNot(websocket_ds:session_still_valid(#{current_uid => 123, did => <<"web-1">>, session_epoch => 2})),
    ?assertMatch({error, _, _}, auth_ds:verify_web_token(maps:get(<<"token">>, Legacy))),
    %% Real Native-shaped ticket remains valid with no new claim.
    ets:insert(Table, {row, 789, <<"android">>}),
    ?assertEqual({ok, 123, <<"web-1">>}, auth_ds:verify_token(maps:get(<<"token">>, Legacy))),
    ?assert(user_device_ds:is_active_native(123, <<"web-1">>)),
    meck:expect(elib_pg, query, fun(_, _) -> {error, db_down} end),
    ?assertNot(user_device_ds:is_active_native(123, <<"web-1">>)),
    ?assertNot(user_device_ds:is_active_web(123, <<"web-1">>, 789)).

assert_old_rejected(Access, Refresh, State) ->
    ?assertMatch({error, _, _}, auth_ds:verify_web_token(Access)),
    ?assertMatch({error, _, _}, auth_ds:verify_web_refresh_token(Refresh)),
    ?assertNot(websocket_ds:session_still_valid(State)).

check_native_rejection(Access, Refresh) ->
    Modules = [cowboy_req, throttle, user_logic, elib_response, elib_log],
    lists:foreach(fun(M) -> meck:new(M, [no_link]) end, Modules),
    try
        meck:expect(cowboy_req, header, fun(<<"imboy-refreshtoken">>, _) -> Refresh end),
        meck:expect(cowboy_req, reply, fun(Status, _, _, Req) -> Req#{status => Status} end),
        meck:expect(throttle, check, fun(_, _) -> ok end),
        meck:expect(user_logic, get_status, fun(123) -> 1 end),
        meck:expect(elib_response, error, fun(Req, _, Code) -> Req#{code => Code} end),
        meck:expect(elib_response, success, fun(_, _) -> error(must_not_refresh_web_on_native) end),
        meck:expect(elib_log, internal_log, fun(_, _, _, _) -> ok end),
        meck:expect(elib_log, internal_log, fun(_, _, _, _, _) -> ok end),
        {ok, Reply, _} = passport_handler:init(#{}, #{action => refreshtoken}),
        ?assert(lists:member(maps:get(code, Reply), [401, 706])),
        ?assertMatch({ok, #{status := 401}, _}, websocket_ds:auth(Access, #{}, #{}, #{}))
    after lists:foreach(fun meck:unload/1, lists:reverse(Modules)) end.
