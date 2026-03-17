-module(user_log_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

add_internal_without_conn_calls_repo_add_1_test_() ->
    ?WITH_MECK(user_log_repo, [
        {'add', 1, fun(Data) ->
            ?assertEqual(110, maps:get(type, Data)),
            ?assertEqual(100, maps:get(uid, Data)),
            ?assertEqual(<<"body">>, maps:get(body, Data)),
            ?assertEqual(<<"2026-03-16T00:00:00Z">>, maps:get(created_at, Data)),
            {ok, 1}
        end}
    ], fun() ->
        Result = user_log_ds:add_internal(
            undefined,
            110,
            100,
            <<"body">>,
            <<"2026-03-16T00:00:00Z">>
        ),
        ?assertEqual({ok, 1}, Result)
    end).

add_internal_with_conn_calls_repo_add_2_test_() ->
    ?WITH_MECK(user_log_repo, [
        {'add', 2, fun(Conn, Data) ->
            ?assertEqual(self(), Conn),
            ?assertEqual(102, maps:get(type, Data)),
            ?assertEqual(101, maps:get(uid, Data)),
            ?assertEqual(<<"body-2">>, maps:get(body, Data)),
            ?assertEqual(<<"2026-03-16T01:00:00Z">>, maps:get(created_at, Data)),
            {ok, 1, inserted}
        end}
    ], fun() ->
        Conn = self(),
        Result = user_log_ds:add_internal(
            Conn,
            102,
            101,
            <<"body-2">>,
            <<"2026-03-16T01:00:00Z">>
        ),
        ?assertEqual({ok, 1, inserted}, Result)
    end).

add_password_change_log_builds_encoded_body_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'header', 3, fun
                (<<"vsn">>, _Req, undefined) -> <<"1.2.3">>;
                (<<"did">>, _Req, undefined) -> <<"device-1">>;
                (<<"cos">>, _Req, undefined) -> <<"ios">>;
                (<<"x-forwarded-for">>, _Req, undefined) -> <<"127.0.0.1">>
            end}
        ]},
        {jsone_encode, [
            {'encode', 2, fun(Map, [native_utf8]) ->
                ?assertEqual(<<"1.2.3">>, maps:get(<<"app_vsn">>, Map)),
                ?assertEqual(<<"device-1">>, maps:get(<<"did">>, Map)),
                ?assertEqual(<<"ios">>, maps:get(<<"dtype">>, Map)),
                ?assertEqual(<<"127.0.0.1">>, maps:get(<<"ip">>, Map)),
                {ok, <<"encoded-password-change-body">>}
            end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2026-03-16T02:00:00Z">> end}
        ]},
        {user_log_repo, [
            {'add', 1, fun(Data) ->
                ?assertEqual(110, maps:get(type, Data)),
                ?assertEqual(100, maps:get(uid, Data)),
                ?assertEqual(<<"encoded-password-change-body">>, maps:get(body, Data)),
                ?assertEqual(<<"2026-03-16T02:00:00Z">>, maps:get(created_at, Data)),
                {ok, saved}
            end}
        ]}
    ], fun() ->
        Result = user_log_ds:add_password_change_log(undefined, 100, #{}, 110),
        ?assertEqual({ok, saved}, Result)
    end).

add_logout_apply_log_uses_default_type_102_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'header', 3, fun
                (<<"vsn">>, _Req, undefined) -> <<"2.0.0">>;
                (<<"did">>, _Req, undefined) -> <<"device-2">>;
                (<<"cos">>, _Req, undefined) -> <<"android">>;
                (<<"x-forwarded-for">>, _Req, undefined) -> <<"10.0.0.8">>
            end}
        ]},
        {jsone_encode, [
            {'encode', 2, fun(Map, [native_utf8]) ->
                ?assertEqual(<<"2.0.0">>, maps:get(<<"app_vsn">>, Map)),
                ?assertEqual(<<"device-2">>, maps:get(<<"did">>, Map)),
                ?assertEqual(<<"android">>, maps:get(<<"dtype">>, Map)),
                ?assertEqual(<<"10.0.0.8">>, maps:get(<<"ip">>, Map)),
                {ok, <<"encoded-logout-apply-body">>}
            end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2026-03-16T03:00:00Z">> end}
        ]},
        {user_log_repo, [
            {'add', 2, fun(Conn, Data) ->
                ?assertEqual(self(), Conn),
                ?assertEqual(102, maps:get(type, Data)),
                ?assertEqual(101, maps:get(uid, Data)),
                ?assertEqual(<<"encoded-logout-apply-body">>, maps:get(body, Data)),
                ?assertEqual(<<"2026-03-16T03:00:00Z">>, maps:get(created_at, Data)),
                {ok, 1}
            end}
        ]}
    ], fun() ->
        Conn = self(),
        Result = user_log_ds:add_logout_apply_log(Conn, 101, #{}),
        ?assertEqual({ok, 1}, Result)
    end).
