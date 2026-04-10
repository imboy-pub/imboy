-module(msg_archive_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc  msg_archive_repo 单元测试
%%%
%%% 测试覆盖：
%%%   conv_key/3        — C2C uid 排序、C2G 格式
%%%   next_conv_seq/1   — 成功、空返回、数据库错误
%%%   archive/1         — C2C 归档、C2G 归档、s2c 跳过、幂等性
%%%   get_history/3,4   — 正序游标、倒序游标、空结果
%%%===================================================================

%% ===================================================================
%% conv_key/3 测试
%% ===================================================================

conv_key_c2c_small_first_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% 小 uid 在前
        Key = msg_archive_repo:conv_key(<<"c2c">>, 100, 200),
        ?assertEqual(<<"c2c:100:200">>, Key)
    end).

conv_key_c2c_large_first_normalizes_order_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% 传入顺序相反，函数内部自动排序
        Key = msg_archive_repo:conv_key(<<"c2c">>, 200, 100),
        ?assertEqual(<<"c2c:100:200">>, Key)
    end).

conv_key_c2c_same_uid_test_() ->
    ?TEST_SIMPLE(fun() ->
        Key = msg_archive_repo:conv_key(<<"c2c">>, 500, 500),
        ?assertEqual(<<"c2c:500:500">>, Key)
    end).

conv_key_c2g_test_() ->
    ?TEST_SIMPLE(fun() ->
        Key = msg_archive_repo:conv_key(<<"c2g">>, 100, 9999),
        ?assertEqual(<<"c2g:9999">>, Key)
    end).

conv_key_c2c_commutative_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% 对称性：任意顺序传入，结果相同
        KeyAB = msg_archive_repo:conv_key(<<"c2c">>, 111, 222),
        KeyBA = msg_archive_repo:conv_key(<<"c2c">>, 222, 111),
        ?assertEqual(KeyAB, KeyBA)
    end).

%% ===================================================================
%% next_conv_seq/1 测试
%% ===================================================================

next_conv_seq_returns_integer_on_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(Sql, [ConvKey]) ->
            %% 验证 SQL 包含 ON CONFLICT DO UPDATE
            ?assertNotEqual(nomatch, binary:match(Sql, <<"INSERT INTO">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"ON CONFLICT">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"DO UPDATE">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"RETURNING seq">>)),
            ?assert(is_binary(ConvKey)),
            {ok, [#{<<"seq">> => 42}]}
        end}
    ], fun() ->
        Result = msg_archive_repo:next_conv_seq(<<"c2c:100:200">>),
        ?assertEqual({ok, 42}, Result)
    end).

next_conv_seq_first_insert_returns_one_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [#{<<"seq">> => 1}]}
        end}
    ], fun() ->
        Result = msg_archive_repo:next_conv_seq(<<"c2c:1:2">>),
        ?assertEqual({ok, 1}, Result)
    end).

next_conv_seq_empty_rows_returns_error_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) -> {ok, []} end}
    ], fun() ->
        Result = msg_archive_repo:next_conv_seq(<<"c2c:100:200">>),
        ?assertEqual({error, no_seq_returned}, Result)
    end).

next_conv_seq_db_error_propagates_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {error, connection_timeout}
        end}
    ], fun() ->
        Result = msg_archive_repo:next_conv_seq(<<"c2c:100:200">>),
        ?assertEqual({error, connection_timeout}, Result)
    end).

%% ===================================================================
%% archive/1 - C2C 归档测试
%% ===================================================================

archive_c2c_success_test_() ->
    Row = #{
        <<"type">>       => <<"c2c">>,
        <<"msg_id">>     => <<"msg-001">>,
        <<"msg_type">>   => <<"text">>,
        <<"from_id">>    => 100,
        <<"to_id">>      => 200,
        <<"e2ee">>       => null,
        <<"payload">>    => <<"{\"body\":\"hello\"}">>,
        <<"created_at">> => <<"2024-01-01T00:00:00Z">>,
        <<"server_ts">>  => <<"2024-01-01T00:00:01Z">>
    },
    ?WITH_MECKS([
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) ->
                %% next_conv_seq 返回序列号 1
                {ok, [#{<<"seq">> => 1}]}
            end},
            {'insert', 2, fun(Table, Data) ->
                ?assertEqual(<<"public.msg_store">>, Table),
                ?assertEqual(<<"c2c">>,              maps:get(chat_type, Data)),
                ?assertEqual(<<"c2c:100:200">>,      maps:get(conv_key, Data)),
                ?assertEqual(1,                      maps:get(conv_seq, Data)),
                ?assertEqual(<<"msg-001">>,           maps:get(msg_id, Data)),
                ?assertEqual(100,                    maps:get(from_id, Data)),
                ?assertEqual(200,                    maps:get(to_id, Data)),
                ?assertEqual(null,                   maps:get(group_id, Data)),
                {ok, 1}
            end}
        ]}
    ], fun() ->
        Result = msg_archive_repo:archive(Row),
        ?assertEqual(ok, Result)
    end).

archive_c2c_idempotent_on_unique_violation_test_() ->
    Row = #{
        <<"type">>       => <<"c2c">>,
        <<"msg_id">>     => <<"msg-dup">>,
        <<"msg_type">>   => <<"text">>,
        <<"from_id">>    => 100,
        <<"to_id">>      => 200,
        <<"e2ee">>       => null,
        <<"payload">>    => <<"{}">>,
        <<"created_at">> => <<"2024-01-01T00:00:00Z">>,
        <<"server_ts">>  => <<"2024-01-01T00:00:00Z">>
    },
    ?WITH_MECKS([
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, [#{<<"seq">> => 5}]}
            end},
            {'insert', 2, fun(_Table, _Data) ->
                %% 模拟 PostgreSQL 唯一约束冲突（Worker 重试场景）
                {error, {error, {error, <<"23505">>, unique_violation, <<"duplicate key">>, []}}}
            end}
        ]}
    ], fun() ->
        %% 唯一约束冲突应被视为成功（幂等）
        Result = msg_archive_repo:archive(Row),
        ?assertEqual(ok, Result)
    end).

archive_c2c_seq_error_propagates_test_() ->
    Row = #{
        <<"type">>       => <<"c2c">>,
        <<"msg_id">>     => <<"msg-seq-err">>,
        <<"msg_type">>   => <<"text">>,
        <<"from_id">>    => 100,
        <<"to_id">>      => 200,
        <<"e2ee">>       => null,
        <<"payload">>    => <<"{}">>,
        <<"created_at">> => <<"2024-01-01T00:00:00Z">>,
        <<"server_ts">>  => <<"2024-01-01T00:00:00Z">>
    },
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {error, seq_table_not_found}
        end}
    ], fun() ->
        Result = msg_archive_repo:archive(Row),
        ?assertEqual({error, seq_table_not_found}, Result)
    end).

%% ===================================================================
%% archive/1 - C2G 归档测试
%% ===================================================================

archive_c2g_success_test_() ->
    %% C2G payload 中包含 group_id（TSID），需要能解析
    %% binary_to_integer(<<"abc">>) 需要 mock
    Row = #{
        <<"type">>       => <<"c2g">>,
        <<"msg_id">>     => <<"msg-g001">>,
        <<"msg_type">>   => <<"text">>,
        <<"from_id">>    => 100,
        <<"to_id_list">> => [200, 300, 400],
        <<"e2ee">>       => null,
        <<"payload">>    => <<"{\"to\":\"grp123\",\"body\":\"hi group\"}">>,
        <<"created_at">> => <<"2024-01-01T00:00:00Z">>,
        <<"server_ts">>  => <<"2024-01-01T00:00:01Z">>
    },
    ?WITH_MECKS([
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, [#{<<"seq">> => 1}]}
            end},
            {'insert', 2, fun(Table, Data) ->
                ?assertEqual(<<"public.msg_store">>, Table),
                ?assertEqual(<<"c2g">>,              maps:get(chat_type, Data)),
                ?assertEqual(<<"c2g:9000">>,         maps:get(conv_key, Data)),
                ?assertEqual(9000,                   maps:get(group_id, Data)),
                ?assertEqual(null,                   maps:get(to_id, Data)),
                {ok, 1}
            end}
        ]}
    ], fun() ->
        Result = msg_archive_repo:archive(Row),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% archive/1 - s2c / c2s 跳过测试
%% ===================================================================

archive_s2c_skipped_test_() ->
    Row = #{
        <<"type">>   => <<"s2c">>,
        <<"msg_id">> => <<"sys-001">>
    },
    ?TEST_SIMPLE(fun() ->
        %% s2c 不调用数据库，直接返回 ok
        Result = msg_archive_repo:archive(Row),
        ?assertEqual(ok, Result)
    end).

archive_c2s_skipped_test_() ->
    Row = #{
        <<"type">>   => <<"c2s">>,
        <<"msg_id">> => <<"c2s-001">>
    },
    ?TEST_SIMPLE(fun() ->
        Result = msg_archive_repo:archive(Row),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% get_history/3,4 测试
%% ===================================================================

get_history_asc_validates_sql_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(Sql, [ConvKey, AfterSeq, Limit]) ->
            %% 验证 SQL 结构
            ?assertNotEqual(nomatch, binary:match(Sql, <<"conv_key = $1">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"conv_seq">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<" > ">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"ORDER BY conv_seq ASC">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"LIMIT $3">>)),
            %% 验证参数
            ?assertEqual(<<"c2c:100:200">>, ConvKey),
            ?assertEqual(0, AfterSeq),
            ?assertEqual(50, Limit),
            {ok, [
                #{<<"msg_id">> => <<"m1">>, <<"conv_seq">> => 1},
                #{<<"msg_id">> => <<"m2">>, <<"conv_seq">> => 2}
            ]}
        end}
    ], fun() ->
        {ok, Rows} = msg_archive_repo:get_history(<<"c2c:100:200">>, 0, 50),
        ?assertEqual(2, length(Rows)),
        [First | _] = Rows,
        ?assertEqual(<<"m1">>, maps:get(<<"msg_id">>, First))
    end).

get_history_desc_uses_less_than_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(Sql, _Params) ->
            ?assertNotEqual(nomatch, binary:match(Sql, <<"ORDER BY conv_seq DESC">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<" < ">>)),
            {ok, []}
        end}
    ], fun() ->
        Result = msg_archive_repo:get_history(<<"c2c:100:200">>, 100, 20, desc),
        ?assertEqual({ok, []}, Result)
    end).

get_history_empty_result_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) -> {ok, []} end}
    ], fun() ->
        Result = msg_archive_repo:get_history(<<"c2c:100:200">>, 999, 50),
        ?assertEqual({ok, []}, Result)
    end).

get_history_db_error_propagates_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {error, relation_does_not_exist}
        end}
    ], fun() ->
        Result = msg_archive_repo:get_history(<<"c2c:100:200">>, 0, 50),
        ?assertEqual({error, relation_does_not_exist}, Result)
    end).
