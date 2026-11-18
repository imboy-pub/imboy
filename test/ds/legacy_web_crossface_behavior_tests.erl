-module(legacy_web_crossface_behavior_tests).
-include_lib("eunit/include/eunit.hrl").

legacy_web_device_cannot_use_generic_human_face_test_() ->
    {setup,
        fun() ->
            Modules = [config_ds, imboy_jwt, auth_session_ds, imboy_cache, elib_pg, elib_pg_sql],
            lists:foreach(fun(M) -> meck:new(M, [no_link]) end, Modules),
            meck:expect(config_ds, env, fun(jwt_key, <<>>) -> <<"unit-only">> end),
            meck:expect(imboy_jwt, sign, fun(C, _) -> term_to_binary(C) end),
            meck:expect(imboy_jwt, verify, fun(T, _) -> {ok, binary_to_term(T, [safe])} end),
            meck:expect(auth_session_ds, revoked, fun(123, 2) -> false end),
            meck:expect(imboy_cache, memo, fun(F, _, _) -> F() end),
            meck:expect(elib_pg_sql, public_tablename, fun(_) -> <<"public.user_device">> end),
            %% The replacement row is active, but its authoritative device_type is Web.
            meck:expect(elib_pg, query, fun(Sql, [123, <<"web-1">>]) ->
                case binary:match(Sql, <<"IS DISTINCT FROM 'web'">>) of
                    nomatch -> {ok, [#{<<"active">> => 1}]};
                    _ -> {ok, []}
                end
            end),
            Modules
        end,
        fun(Modules) -> lists:foreach(fun meck:unload/1, lists:reverse(Modules)) end, fun(_) ->
            ?_test(begin
                Pair = token_ds:issue_device_session(123, <<"web-1">>, 2),
                Access = maps:get(<<"token">>, Pair),
                ?assertNot(maps:is_key(<<"dsid">>, binary_to_term(Access, [safe]))),
                ?assertMatch({error, 401, _}, auth_ds:verify_token(Access))
            end)
        end}.
