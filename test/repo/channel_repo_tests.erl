-module(channel_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc channel_repo 的 repo 层单元测试（基于 mock，无数据库依赖）
%%%===================================================================

tablename_returns_public_channel_table_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(<<"public.channel">>, channel_repo:tablename())
    end).

find_by_id_converts_binary_id_and_returns_row_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'build_select', 4, fun(_Tb, <<"*">>, #{id := 123, status := 1}, #{limit := 1}) ->
                {<<"SELECT * FROM public.channel WHERE id=$1 AND status=$2 LIMIT 1">>, [123, 1]}
            end}
        ]},
        {elib_pg, [
            {'one', 2, fun(_Sql, [123, 1]) ->
                {ok, #{<<"id">> => 123, <<"name">> => <<"Tech">>}}
            end}
        ]}
    ], fun() ->
        Result = channel_repo:find_by_id(<<"123">>, <<"*">>),
        ?assertMatch(#{<<"id">> := 123}, Result)
    end).

find_by_id_returns_error_when_query_fails_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'build_select', 4, fun(_Tb, <<"id">>, #{id := 11, status := 1}, #{limit := 1}) ->
                {<<"SELECT id FROM public.channel WHERE id=$1 AND status=$2 LIMIT 1">>, [11, 1]}
            end}
        ]},
        {elib_pg, [
            {'one', 2, fun(_Sql, [11, 1]) ->
                {error, timeout}
            end}
        ]}
    ], fun() ->
        ?assertEqual({error, timeout}, channel_repo:find_by_id(11, <<"id">>))
    end).

list_by_ids_empty_returns_empty_list_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual({ok, []}, channel_repo:list_by_ids([], <<"*">>))
    end).

list_by_ids_since_empty_returns_empty_list_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual({ok, []}, channel_repo:list_by_ids_since([], 0))
    end).

list_by_ids_since_converts_millisecond_since_to_rfc3339_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'query', 2, fun(_Sql, [11, 12, SinceTs]) ->
                ?assertEqual(elib_dt:to_rfc3339(1700000000000, millisecond), SinceTs),
                {ok, []}
            end}
        ]}
    ], fun() ->
        ?assertEqual({ok, []}, channel_repo:list_by_ids_since([11, 12], 1700000000000))
    end).

increment_subscribers_positive_uses_plus_operator_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'execute', 2, fun(Sql, [3, 11]) ->
                SqlBin = iolist_to_binary(Sql),
                ?assert(re:run(SqlBin, <<"subscriber_count = subscriber_count \\+ \\$1">>) =/= nomatch),
                {ok, 1}
            end}
        ]}
    ], fun() ->
        ?assertEqual({ok, 1}, channel_repo:increment_subscribers(11, 3))
    end).

increment_subscribers_negative_uses_minus_operator_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'execute', 2, fun(Sql, [2, 11]) ->
                SqlBin = iolist_to_binary(Sql),
                ?assert(re:run(SqlBin, <<"subscriber_count = subscriber_count - \\$1">>) =/= nomatch),
                {ok, 1}
            end}
        ]}
    ], fun() ->
        ?assertEqual({ok, 1}, channel_repo:increment_subscribers(11, -2))
    end).

increment_subscribers_with_conn_uses_execute3_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'execute', 3, fun(fake_conn, Sql, [4, 11]) ->
                SqlBin = iolist_to_binary(Sql),
                ?assert(re:run(SqlBin, <<"subscriber_count = subscriber_count \\+ \\$1">>) =/= nomatch),
                {ok, 1}
            end}
        ]}
    ], fun() ->
        ?assertEqual({ok, 1}, channel_repo:increment_subscribers(fake_conn, 11, 4))
    end).

has_viewed_message_returns_true_when_row_exists_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'one', 2, fun(_Sql, [99, 1001]) ->
                {ok, #{<<"exists">> => 1}}
            end}
        ]}
    ], fun() ->
        ?assertEqual(true, channel_repo:has_viewed_message(99, 1001))
    end).

has_viewed_message_returns_false_when_missing_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'one', 2, fun(_Sql, [99, 1001]) ->
                {error, not_found}
            end}
        ]}
    ], fun() ->
        ?assertEqual(false, channel_repo:has_viewed_message(99, 1001))
    end).

get_reaction_count_returns_zero_when_query_fails_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'one', 2, fun(_Sql, [11]) ->
                {error, db_down}
            end}
        ]}
    ], fun() ->
        ?assertEqual({ok, 0}, channel_repo:get_reaction_count(11))
    end).
