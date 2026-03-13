-module(channel_message_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc channel_message_repo 的 repo 层单元测试（基于 mock，无数据库依赖）
%%%===================================================================

tablename_returns_public_channel_message_table_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(<<"public.channel_message">>, channel_message_repo:tablename())
    end).

add_passes_insert_result_to_parse_result_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'insert', 3, fun(<<"public.channel_message">>, Data, <<"RETURNING id">>) ->
                ?assertEqual(11, maps:get(channel_id, Data)),
                {ok, #{raw => true}}
            end}
        ]},
        {elib_pg_sql, [
            {'parse_result', 1, fun({ok, #{raw := true}}) ->
                {ok, 99, #{}}
            end}
        ]}
    ], fun() ->
        Data = #{channel_id => 11, author_id => 1001, content => <<"hello">>},
        ?assertEqual({ok, 99, #{}}, channel_message_repo:add(Data))
    end).

find_by_id_returns_row_when_exists_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'one', 2, fun(Sql, [99]) ->
                SqlBin = iolist_to_binary(Sql),
                ?assert(re:run(SqlBin, <<"WHERE id = \\$1 AND status = 1">>) =/= nomatch),
                {ok, #{<<"id">> => 99, <<"channel_id">> => 11}}
            end}
        ]}
    ], fun() ->
        ?assertMatch(#{<<"id">> := 99}, channel_message_repo:find_by_id(99))
    end).

find_by_id_returns_error_when_missing_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'one', 2, fun(_Sql, [101]) -> {error, not_found} end}
        ]}
    ], fun() ->
        ?assertEqual({error, not_found}, channel_message_repo:find_by_id(101))
    end).

list_by_channel_cursor_zero_fetches_latest_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'query', 2, fun(Sql, [11, 20]) ->
                SqlBin = iolist_to_binary(Sql),
                ?assert(re:run(SqlBin, <<"ORDER BY created_at DESC LIMIT \\$2">>) =/= nomatch),
                {ok, [#{<<"id">> => 1}]}
            end}
        ]}
    ], fun() ->
        ?assertEqual({ok, [#{<<"id">> => 1}]}, channel_message_repo:list_by_channel(11, 0, 20))
    end).

list_by_channel_with_cursor_uses_rfc3339_timestamp_test_() ->
    ?WITH_MECKS([
        {elib_dt, [
            {'to_rfc3339', 1, fun(1700000000000) -> <<"2023-11-14T00:00:00Z">> end}
        ]},
        {elib_pg, [
            {'query', 2, fun(Sql, [11, <<"2023-11-14T00:00:00Z">>, 30]) ->
                SqlBin = iolist_to_binary(Sql),
                ?assert(re:run(SqlBin, <<"created_at < \\$2">>) =/= nomatch),
                {ok, []}
            end}
        ]}
    ], fun() ->
        ?assertEqual({ok, []}, channel_message_repo:list_by_channel(11, 1700000000000, 30))
    end).

update_removes_id_field_before_persisting_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'update', 4, fun(<<"public.channel_message">>, UpdateData, <<"id = $1">>, [99]) ->
                ?assertEqual(false, maps:is_key(<<"id">>, UpdateData)),
                ?assertEqual(true, maps:is_key(content, UpdateData)),
                {ok, 1}
            end}
        ]}
    ], fun() ->
        Data = #{<<"id">> => 99, content => <<"patched">>},
        ?assertEqual({ok, 1}, channel_message_repo:update(99, Data))
    end).

delete_soft_marks_status_negative_one_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'update', 4, fun(<<"public.channel_message">>, #{status := -1}, <<"id = $1">>, [99]) ->
                {ok, 1}
            end}
        ]}
    ], fun() ->
        ?assertEqual({ok, 1}, channel_message_repo:delete(99))
    end).

increment_view_count_executes_atomic_update_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'execute', 2, fun(Sql, [99]) ->
                SqlBin = iolist_to_binary(Sql),
                ?assert(re:run(SqlBin, <<"view_count = view_count \\+ 1">>) =/= nomatch),
                {ok, 1}
            end}
        ]}
    ], fun() ->
        ?assertEqual({ok, 1}, channel_message_repo:increment_view_count(99))
    end).

revoke_marks_message_revoked_with_idempotent_condition_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'execute', 2, fun(Sql, [99, 1001, <<"2026-02-22T10:00:00Z">>]) ->
                SqlBin = iolist_to_binary(Sql),
                ?assert(re:run(SqlBin, <<"SET revoked = true">>) =/= nomatch),
                ?assert(re:run(SqlBin, <<"revoked = false">>) =/= nomatch),
                {ok, 1}
            end}
        ]}
    ], fun() ->
        ?assertEqual({ok, 1}, channel_message_repo:revoke(99, 1001, <<"2026-02-22T10:00:00Z">>))
    end).
