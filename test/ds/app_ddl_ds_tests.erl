-module(app_ddl_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

save_add_success_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'pluck_value', 5, fun(<<"public.app_ddl">>, <<"count(*)">>, _Where, #{}, 0) ->
                0
            end}
        ]},
        {app_ddl_repo, [
            {'add', 1, fun(Data) ->
                ?assertEqual(1, maps:get(admin_user_id, Data)),
                ?assert(maps:is_key(created_at, Data)),
                {ok, 1}
            end}
        ]},
        {elib_dt, [
            {'now', 0, fun() ->
                <<"2026-03-16T00:00:00Z">>
            end}
        ]}
    ], fun() ->
        Result = app_ddl_ds:save(1, <<"2">>, <<"1">>, <<"1">>, <<"ddl64">>, <<"down64">>),
        ?assertEqual({ok, 1}, Result)
    end).

save_update_success_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'pluck_value', 5, fun(<<"public.app_ddl">>, <<"count(*)">>, _Where, #{}, 0) ->
                1
            end},
            {'update', 4, fun(<<"app_ddl">>, Data, <<"old_vsn = $1 AND new_vsn = $2">>, [<<"1">>, <<"2">>]) ->
                ?assert(maps:is_key(updated_at, Data)),
                {ok, 1}
            end}
        ]},
        {app_ddl_repo, [
            {'tablename', 0, fun() ->
                <<"app_ddl">>
            end}
        ]},
        {elib_dt, [
            {'now', 0, fun() ->
                <<"2026-03-16T00:00:00Z">>
            end}
        ]}
    ], fun() ->
        Result = app_ddl_ds:save(1, <<"2">>, <<"1">>, <<"1">>, <<"ddl64">>, <<"down64">>),
        ?assertEqual({ok, 1}, Result)
    end).

delete_success_test_() ->
    ?WITH_MECKS([
        {app_ddl_repo, [
            {'tablename', 0, fun() ->
                <<"app_ddl">>
            end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(<<"DELETE FROM app_ddl WHERE status = 0 AND id = $1">>, [3]) ->
                {ok, 1}
            end}
        ]}
    ], fun() ->
        ?assertEqual({ok, 1}, app_ddl_ds:delete(3))
    end).

get_ddl_success_test_() ->
    ?WITH_MECKS([
        {app_ddl_repo, [
            {'tablename', 0, fun() ->
                <<"app_ddl">>
            end}
        ]},
        {elib_pg, [
            {'page_with_total', 6, fun(<<"app_ddl">>, <<"ddl">>, #{}, <<"id desc">>, 1, 500) ->
                Encoded = base64:encode(<<"CREATE TABLE t (id INT);\n-- comment\nINSERT INTO t VALUES (1);">>),
                {ok, #{list => [#{<<"ddl">> => Encoded}]}}
            end}
        ]}
    ], fun() ->
        Result = app_ddl_ds:get_ddl(#{}, <<"id desc">>, <<"ddl">>),
        ?assert(lists:member(<<"CREATE TABLE t (id INT)">>, Result)),
        ?assert(lists:member(<<"INSERT INTO t VALUES (1)">>, Result))
    end).
