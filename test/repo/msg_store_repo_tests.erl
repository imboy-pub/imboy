-module(msg_store_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc  msg_store_repo 高质量测试套件
%%%
%%% 质量改进：
%%% 1. 验证 SQL 语句正确性
%%% 2. 验证数据映射完整性
%%% 3. 验证参数类型和范围
%%% 4. 补充边界条件测试
%%% 5. 发现源码逻辑错误
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_qualified_table_name_test_() ->
    ?WITH_MECK(elib_pg_sql, [
        {'public_tablename', 1, fun(<<"msg_store_staging">>) ->
            <<"public.msg_store_staging">>
        end}
    ], fun() ->
        Result = msg_store_repo:tablename(),
        ?assertEqual(<<"public.msg_store_staging">>, Result),
        ?assert(is_binary(Result)),
        ?assert(Result =/= <<>>)
    end).

%% ===================================================================
%% stage/10 测试 - 单聊消息 (integer ToId)
%% ===================================================================

stage_with_integer_toid_validates_all_fields_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(<<"msg_store_staging">>) ->
                <<"public.msg_store_staging">>
            end}
        ]},
        {elib_pg, [
            {'insert', 2, fun(TableName, Data) ->
                % 验证表名
                ?assertEqual(<<"public.msg_store_staging">>, TableName),

                % 验证必需字段存在
                RequiredFields = [type, msg_id, msg_type, action, payload,
                                 from_id, to_id, created_at, server_ts, retry_count],
                lists:foreach(fun(Field) ->
                    ?assert(maps:is_key(Field, Data),
                            io_lib:format("Missing field: ~p", [Field]))
                end, RequiredFields),

                % 验证字段值正确性
                ?assertEqual(<<"c2c">>, maps:get(type, Data)),
                ?assertEqual(<<"msg123">>, maps:get(msg_id, Data)),
                ?assertEqual(<<"text">>, maps:get(msg_type, Data)),
                ?assertEqual(<<"send">>, maps:get(action, Data)),
                ?assertEqual(<<"{\"body\": \"hello\"}">>, maps:get(payload, Data)),
                ?assertEqual(100, maps:get(from_id, Data)),
                ?assertEqual(200, maps:get(to_id, Data)),
                ?assertEqual(<<"2024-01-01T00:00:00Z">>, maps:get(created_at, Data)),
                ?assertEqual(<<"2024-01-01T00:00:01Z">>, maps:get(server_ts, Data)),
                ?assertEqual(0, maps:get(retry_count, Data)),

                % 验证 ID 类型和范围
                FromId = maps:get(from_id, Data),
                ToId = maps:get(to_id, Data),
                ?assert(is_integer(FromId), "from_id must be integer"),
                ?assert(is_integer(ToId), "to_id must be integer"),
                ?assert(FromId > 0, "from_id must be positive"),
                ?assert(ToId > 0, "to_id must be positive"),

                % 验证时间戳格式（RFC3339）
                CreatedAt = maps:get(created_at, Data),
                ServerTs = maps:get(server_ts, Data),
                ?assertMatch(<<_:4,"-",_:2,"-",_:2,"T",_:8,"Z">>, CreatedAt),
                ?assertMatch(<<_:4,"-",_:2,"-",_:2,"T",_:8,"Z">>, ServerTs),

                {ok, 1}
            end}
        ]}
    ], fun() ->
        Result = msg_store_repo:stage(
            <<"c2c">>, <<"msg123">>, <<"text">>, <<"send">>, <<>>,
            <<"{\"body\": \"hello\"}">>, 100, 200,
            <<"2024-01-01T00:00:00Z">>, <<"2024-01-01T00:00:01Z">>
        ),
        ?assertEqual({ok, 1}, Result)
    end).

stage_with_empty_e2ee_converts_to_null_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"public.msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'insert', 2, fun(_TableName, Data) ->
                % 验证空 E2EE 被转换为 null
                E2EE = maps:get(e2ee, Data),
                ?assertEqual(null, E2EE),
                % 确保不是空 binary
                ?assertNotEqual(<<>>, E2EE),
                {ok, 1}
            end}
        ]}
    ], fun() ->
        Result = msg_store_repo:stage(
            <<"c2c">>, <<"msg456">>, <<"text">>, <<"send">>, <<>>,
            <<"{}">>, 100, 200,
            <<"2024-01-01T00:00:00Z">>, <<"2024-01-01T00:00:01Z">>
        ),
        ?assertEqual({ok, 1}, Result)
    end).

stage_with_e2ee_data_preserves_value_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"public.msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'insert', 2, fun(_TableName, Data) ->
                % 验证非空 E2EE 被保留
                E2EE = maps:get(e2ee, Data),
                ?assertEqual(<<"e2ee_data">>, E2EE),
                ?assert(is_binary(E2EE)),
                ?assert(E2EE =/= <<>>),
                ?assertNotEqual(null, E2EE),
                {ok, 1}
            end}
        ]}
    ], fun() ->
        Result = msg_store_repo:stage(
            <<"c2c">>, <<"msg789">>, <<"text">>, <<"send">>, <<"e2ee_data">>,
            <<"{}">>, 100, 200,
            <<"2024-01-01T00:00:00Z">>, <<"2024-01-01T00:00:01Z">>
        ),
        ?assertEqual({ok, 1}, Result)
    end).

stage_unique_violation_converts_to_business_error_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'insert', 2, fun(_Tb, _Data) ->
                % 模拟 PostgreSQL 唯一约束错误
                {error, {error, [[{23505, unique_constraint}]]}}
            end}
        ]}
    ], fun() ->
        Result = msg_store_repo:stage(
            <<"c2c">>, <<"msg_duplicate">>, <<"text">>, <<"send">>, <<>>,
            <<"{}">>, 100, 200,
            <<"2024-01-01T00:00:00Z">>, <<"2024-01-01T00:00:01Z">>
        ),
        % 验证错误被正确转换为业务错误
        ?assertMatch({error, {unique_violation, <<"msg_duplicate">>}}, Result),
        % 验证 MsgId 在错误信息中
        case Result of
            {error, {unique_violation, MsgId}} ->
                ?assertEqual(<<"msg_duplicate">>, MsgId);
            _ ->
                ?assert(false, "Error format incorrect")
        end
    end).

stage_with_zero_from_id_rejected_test_() ->
    % 边界测试：from_id = 0 应该被拒绝
    ?WITH_MECK(elib_pg, [
        {'insert', 2, fun(_Tb, _Data) ->
            % 不应该到达这里，因为 from_id = 0 是无效的
            ?assert(false, "from_id = 0 should be rejected")
        end}
    ], fun() ->
        % 注意：当前实现可能没有这个验证，测试会失败
        % 这体现了测试的价值：发现边界条件缺失
        Result = msg_store_repo:stage(
            <<"c2c">>, <<"msg_zero">>, <<"text">>, <<"send">>, <<>>,
            <<"{}">>, 0, 200,
            <<"2024-01-01T00:00:00Z">>, <<"2024-01-01T00:00:01Z">>
        ),
        % 期望：应该返回错误或断言失败
        ?assertMatch({error, _}, Result)
    end).

stage_with_negative_from_id_rejected_test_() ->
    % 边界测试：负数 from_id 应该被拒绝
    ?WITH_MECK(elib_pg, [
        {'insert', 2, fun(_Tb, Data) ->
            % 如果到达这里，至少验证数据库层会处理
            FromId = maps:get(from_id, Data),
            ?assert(FromId > 0, "Negative from_id should not reach database"),
            {ok, 1}
        end}
    ], fun() ->
        Result = msg_store_repo:stage(
            <<"c2c">>, <<"msg_negative">>, <<"text">>, <<"send">>, <<>>,
            <<"{}">>, -1, 200,
            <<"2024-01-01T00:00:00Z">>, <<"2024-01-01T00:00:01Z">>
        ),
        % 负数 ID 应该被拒绝
        case Result of
            {ok, _} ->
                % 如果接受负数，测试失败并警告
                ?assert(false, "Negative from_id should be rejected");
            {error, _} ->
                % 期望行为
                ok
        end
    end).

%% ===================================================================
%% stage/10 测试 - 群聊消息 (list ToIdList)
%% ===================================================================

stage_with_list_toidlist_validates_structure_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"public.msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'insert', 2, fun(_Tb, Data) ->
                % 验证群聊消息使用 to_id_list 而非 to_id
                ?assert(maps:is_key(to_id_list, Data)),
                ?assertNot(maps:is_key(to_id, Data)),

                % 验证 to_id_list 内容
                ToIdList = maps:get(to_id_list, Data),
                ?assert(is_list(ToIdList)),
                ?assertEqual([100, 200, 300], ToIdList),
                ?assertEqual(3, length(ToIdList)),

                % 验证所有元素都是正整数
                lists:foreach(fun(Id) ->
                    ?assert(is_integer(Id)),
                    ?assert(Id > 0)
                end, ToIdList),

                % 验证消息类型为群聊
                ?assertEqual(<<"c2g">>, maps:get(type, Data)),

                {ok, 1}
            end}
        ]}
    ], fun() ->
        Result = msg_store_repo:stage(
            <<"c2g">>, <<"msg_group">>, <<"text">>, <<"send">>, <<>>,
            <<"{}">>, 50, [100, 200, 300],
            <<"2024-01-01T00:00:00Z">>, <<"2024-01-01T00:00:01Z">>
        ),
        ?assertEqual({ok, 1}, Result)
    end).

stage_with_empty_toidlist_should_fail_test_() ->
    % 边界测试：空成员列表应该被拒绝
    ?WITH_MECK(elib_pg, [
        {'insert', 2, fun(_Tb, Data) ->
            ToIdList = maps:get(to_id_list, Data),
            ?assert(length(ToIdList) > 0, "Empty to_id_list should be rejected"),
            {ok, 1}
        end}
    ], fun() ->
        Result = msg_store_repo:stage(
            <<"c2g">>, <<"msg_empty_group">>, <<"text">>, <<"send">>, <<>>,
            <<"{}">>, 50, [],
            <<"2024-01-01T00:00:00Z">>, <<"2024-01-01T00:00:01Z">>
        ),
        % 空成员列表应该被拒绝（当前实现可能没有此验证）
        case Result of
            {ok, _} ->
                ?assert(false, "Empty to_id_list should be rejected");
            {error, _} ->
                ok
        end
    end).

stage_with_list_unique_violation_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'insert', 2, fun(_Tb, _Data) ->
                {error, {error, [[{23505, unique_constraint}]]}}
            end}
        ]}
    ], fun() ->
        Result = msg_store_repo:stage(
            <<"c2g">>, <<"msg_group_dup">>, <<"text">>, <<"send">>, <<>>,
            <<"{}">>, 50, [100, 200],
            <<"2024-01-01T00:00:00Z">>, <<"2024-01-01T00:00:01Z">>
        ),
        ?assertMatch({error, {unique_violation, <<"msg_group_dup">>}}, Result)
    end).

%% ===================================================================
%% unstage/2 测试
%% ===================================================================

unstage_validates_sql_parameters_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(<<"msg_store_staging">>) ->
                <<"public.msg_store_staging">>
            end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(Sql, [Type, MsgId]) ->
                % 验证 SQL 结构
                ?assert(is_binary(Sql)),
                ?assert(Sql =/= <<>>),
                ?assertNotEqual(0, byte_size(Sql)),

                % 验证 SQL 包含关键部分
                ?assertNotEqual(nomatch, binary:match(Sql, <<"DELETE FROM">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"WHERE type =">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"AND msg_id =">>)),

                % 验证参数类型和值
                ?assert(is_binary(Type)),
                ?assertEqual(<<"c2c">>, Type),
                ?assert(is_binary(MsgId)),
                ?assertEqual(<<"msg123">>, MsgId),

                {ok, 1}
            end}
        ]}
    ], fun() ->
        Result = msg_store_repo:unstage(<<"c2c">>, <<"msg123">>),
        ?assertEqual({ok, 1}, Result)
    end).

unstage_nonexistent_returns_zero_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) -> {ok, 0} end}
        ]}
    ], fun() ->
        Result = msg_store_repo:unstage(<<"c2c">>, <<"nonexistent">>),
        ?assertEqual({ok, 0}, Result)
    end).

unstage_with_empty_type_rejected_test_() ->
    % 边界测试：空 Type 应该被处理
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(_Sql, [Type, _MsgId]) ->
            % 如果到达数据库，验证 Type 非空
            ?assert(Type =/= <<>>, "Empty type should not reach database"),
            {ok, 0}
        end}
    ], fun() ->
        Result = msg_store_repo:unstage(<<>>, <<"msg123">>),
        % 当前实现可能不验证空 Type
        case Result of
            {ok, _} ->
                % 警告：应该验证输入
                ok;
            {error, _} ->
                ok
        end
    end).

%% ===================================================================
%% claim_pending/2 测试
%% ===================================================================

claim_pending_validates_transaction_logic_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'with_tx', 1, fun(TxFun) ->
                % 验证事务函数被调用
                ?assert(is_function(TxFun, 1)),
                % 执行事务函数
                TxFun(self())
            end},
            {'query', 3, fun(_Conn, Sql, [Limit]) ->
                % 验证 SQL 包含 SKIP LOCKED
                ?assertNotEqual(nomatch, binary:match(Sql, <<"FOR UPDATE SKIP LOCKED">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"processed_at IS NULL">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"available_at <= NOW()">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"ORDER BY created_at ASC">>)),

                % 验证 LIMIT 参数
                ?assert(is_integer(Limit)),
                ?assertEqual(10, Limit),
                ?assert(Limit > 0),

                {ok, [[
                    #{<<"id">> => 1, <<"msg_id">> => <<"msg1">>, <<"payload">> => <<"{}">>},
                    #{<<"id">> => 2, <<"msg_id">> => <<"msg2">>, <<"payload">> => <<"{}">>}
                ]]}
            end},
            {'execute', 3, fun(_Conn, LeaseSql, [_LeaseSeconds, Ids]) ->
                % 验证租约 SQL
                ?assertNotEqual(nomatch, binary:match(LeaseSql, <<"UPDATE ">>)),
                ?assertNotEqual(nomatch, binary:match(LeaseSql, <<"SET available_at = NOW()">>)),
                ?assertNotEqual(nomatch, binary:match(LeaseSql, <<"INTERVAL '1 second' * ">>)),

                % 验证 ID 列表
                ?assert(is_list(Ids)),
                ?assert(lists:all(fun(Id) -> is_integer(Id) end, Ids)),
                ?assertEqual([1, 2], Ids),

                {ok, 2}
            end}
        ]}
    ], fun() ->
        Result = msg_store_repo:claim_pending(10, 60),
        ?assertMatch({ok, [_, _]}, Result),
        case Result of
            {ok, Rows} ->
                % 验证返回的行包含必要字段
                lists:foreach(fun(Row) ->
                    ?assert(maps:is_key(<<"id">>, Row)),
                    ?assert(maps:is_key(<<"msg_id">>, Row)),
                    ?assert(maps:is_key(<<"payload">>, Row))
                end, Rows);
            _ ->
                ?assert(false)
        end
    end).

claim_pending_empty_returns_empty_list_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'with_tx', 1, fun(TxFun) -> TxFun(self()) end},
            {'query', 3, fun(_Conn, _Sql, _Params) -> {ok, []} end}
        ]}
    ], fun() ->
        Result = msg_store_repo:claim_pending(10, 60),
        ?assertEqual({ok, []}, Result)
    end).

claim_pending_with_zero_limit_test_() ->
    % 边界测试：Limit = 0 应该返回空列表
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'with_tx', 1, fun(TxFun) -> TxFun(self()) end},
            {'query', 3, fun(_Conn, _Sql, [Limit]) ->
                ?assertEqual(0, Limit),
                {ok, []}
            end}
        ]}
    ], fun() ->
        Result = msg_store_repo:claim_pending(0, 60),
        ?assertEqual({ok, []}, Result)
    end).

%% ===================================================================
%% mark_processed/2 测试
%% ===================================================================

mark_processed_validates_sql_structure_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(Sql, [Type, MsgId]) ->
                % 验证 SQL 结构
                ?assertNotEqual(nomatch, binary:match(Sql, <<"UPDATE ">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"SET processed_at = NOW()">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"error_msg = NULL">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"WHERE type = $1">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"AND msg_id = $2">>)),

                % 验证参数
                ?assertEqual(<<"c2c">>, Type),
                ?assertEqual(<<"msg123">>, MsgId),

                {ok, 1}
            end}
        ]}
    ], fun() ->
        Result = msg_store_repo:mark_processed(<<"c2c">>, <<"msg123">>),
        ?assertEqual({ok, 1}, Result)
    end).

%% ===================================================================
%% mark_failed/4 测试
%% ===================================================================

mark_failed_validates_parameters_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(Sql, [Type, MsgId, ErrorMsg, DelaySeconds]) ->
                % 验证 SQL 结构
                ?assertNotEqual(nomatch, binary:match(Sql, <<"retry_count = retry_count + 1">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"error_msg = $3">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"available_at = NOW() + INTERVAL">>)),

                % 验证参数类型
                ?assert(is_binary(Type)),
                ?assertEqual(<<"c2c">>, Type),
                ?assert(is_binary(MsgId)),
                ?assertEqual(<<"msg123">>, MsgId),
                ?assert(is_binary(ErrorMsg)),
                ?assertEqual(<<"connection failed"/utf8>>, ErrorMsg),
                ?assert(is_integer(DelaySeconds)),
                ?assert(DelaySeconds > 0),
                ?assertEqual(60, DelaySeconds),

                {ok, 1}
            end}
        ]}
    ], fun() ->
        Result = msg_store_repo:mark_failed(
            <<"c2c">>, <<"msg123">>, <<"connection failed"/utf8>>, 60
        ),
        ?assertEqual({ok, 1}, Result)
    end).

mark_failed_with_zero_delay_test_() ->
    % 边界测试：Delay = 0 可能是无效的
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, [_Type, _MsgId, _ErrorMsg, Delay]) ->
                ?assert(Delay >= 0, "Delay should be non-negative"),
                {ok, 1}
            end}
        ]}
    ], fun() ->
        Result = msg_store_repo:mark_failed(
            <<"c2c">>, <<"msg123">>, <<"error"/utf8>>, 0
        ),
        ?assertEqual({ok, 1}, Result)
    end).

%% ===================================================================
%% get_unstaged/1 测试
%% ===================================================================

get_unstaged_validates_limit_parameter_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'query', 2, fun(Sql, [Limit]) ->
                % 验证 SQL 结构
                ?assertNotEqual(nomatch, binary:match(Sql, <<"SELECT ">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"WHERE processed_at IS NULL">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"ORDER BY created_at ASC">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"LIMIT $1">>)),

                % 验证 LIMIT 参数
                ?assert(is_integer(Limit)),
                ?assertEqual(100, Limit),
                ?assert(Limit > 0),

                {ok, [[
                    #{<<"msg_id">> => <<"msg1">>, <<"from_id">> => 100},
                    #{<<"msg_id">> => <<"msg2">>, <<"from_id">> => 200}
                ]]}
            end}
        ]}
    ], fun() ->
        Result = msg_store_repo:get_unstaged(100),
        ?assertMatch({ok, [_, _]}, Result),
        case Result of
            {ok, Rows} ->
                ?assertEqual(2, length(Rows));
            _ ->
                ?assert(false)
        end
    end).

get_unstaged_with_negative_limit_test_() ->
    % 边界测试：负数 Limit
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, [Limit]) ->
                % 如果到达数据库，验证 Limit
                ?assert(Limit >= 0, "Limit should be non-negative"),
                {ok, []}
            end}
        ]}
    ], fun() ->
        Result = msg_store_repo:get_unstaged(-1),
        case Result of
            {ok, Rows} ->
                ?assertEqual([], Rows);
            {error, _} ->
                ok
        end
    end).

%% ===================================================================
%% delete_processed/1 测试 - **发现源码 Bug！**
%% ===================================================================

delete_processed_validates_sql_correctness_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(Sql, [Seconds]) ->
                % 验证 SQL 结构
                ?assertNotEqual(nomatch, binary:match(Sql, <<"DELETE FROM ">>)),

                % **关键测试**：发现源码 bug
                % 源码 line 191: " WHERE processed_at IS NULL " 是错误的！
                % 正确应该是: " WHERE processed_at IS NOT NULL "
                % 因为我们要删除已处理的记录，而不是未处理的

                % 检查是否有 IS NULL（这是 bug）
                case binary:match(Sql, <<"processed_at IS NULL">>) of
                    nomatch ->
                        % 正确：删除已处理的记录
                        ok;
                    _ ->
                        % Bug：当前实现会删除未处理的记录！
                        ?assert(false, "BUG: Should use IS NOT NULL, not IS NULL")
                end,

                % 验证 IS NOT NULL 存在
                ?assertNotEqual(nomatch, binary:match(Sql, <<"processed_at IS NOT NULL">>),
                    "Should delete processed records, not unprocessed"),

                % 验证时间条件
                ?assertNotEqual(nomatch, binary:match(Sql, <<"processed_at < NOW() - INTERVAL">>)),

                % 验证参数
                ?assert(is_integer(Seconds)),
                ?assert(Seconds > 0),
                ?assertEqual(3600, Seconds),

                {ok, 100}
            end}
        ]}
    ], fun() ->
        Result = msg_store_repo:delete_processed(3600),
        ?assertEqual({ok, 100}, Result)
    end).

%% ===================================================================
%% get_staging_stats/0 测试
%% ===================================================================

get_staging_stats_validates_aggregation_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'query', 2, fun(Sql, []) ->
                % 验证 SQL 包含聚合函数
                ?assertNotEqual(nomatch, binary:match(Sql, <<"COUNT(*) FILTER">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"processed_at IS NULL">>)),  % pending
                ?assertNotEqual(nomatch, binary:match(Sql, <<"processed_at IS NOT NULL">>)),  % processed
                ?assertNotEqual(nomatch, binary:match(Sql, <<"error_msg IS NOT NULL">>)),  % failed
                ?assertNotEqual(nomatch, binary:match(Sql, <<"COUNT(*) as total">>)),

                {ok, [[
                    #{<<"pending">> => 10, <<"processed">> => 100, <<"failed">> => 2, <<"total">> => 112}
                ]]}
            end}
        ]}
    ], fun() ->
        Result = msg_store_repo:get_staging_stats(),
        ?assertMatch({ok, #{
            <<"pending">> := 10,
            <<"processed">> := 100,
            <<"failed">> := 2,
            <<"total">> := 112
        }}, Result)
    end).

%% ===================================================================
%% truncate_processed/0 测试
%% ===================================================================

truncate_processed_validates_sql_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'query', 2, fun(Sql, []) ->
                % 验证 TRUNCATE 命令
                ?assertNotEqual(nomatch, binary:match(Sql, <<"TRUNCATE TABLE ">>)),
                {ok, [], []}
            end}
        ]}
    ], fun() ->
        Result = msg_store_repo:truncate_processed(),
        ?assertEqual({ok, [], []}, Result)
    end).

%% ===================================================================
%% vacuum_table/0 测试
%% ===================================================================

vacuum_table_validates_sql_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'query', 2, fun(Sql, []) ->
                % 验证 VACUUM 命令
                ?assertNotEqual(nomatch, binary:match(Sql, <<"VACUUM ANALYZE ">>)),
                {ok, [], []}
            end}
        ]}
    ], fun() ->
        Result = msg_store_repo:vacuum_table(),
        ?assertEqual({ok, [], []}, Result)
    end).

%% ===================================================================
%% ensure_table_exists/0 测试
%% ===================================================================

ensure_table_exists_validates_ddl_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(Sql, []) ->
                % 验证 CREATE TABLE 语句
                ?assertNotEqual(nomatch, binary:match(Sql, <<"CREATE TABLE IF NOT EXISTS ">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"id BIGSERIAL PRIMARY KEY">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"type VARCHAR(10) NOT NULL">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"msg_id VARCHAR(50) NOT NULL">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"payload JSONB NOT NULL">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"from_id BIGINT NOT NULL">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"retry_count INTEGER NOT NULL DEFAULT 0">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"processed_at TIMESTAMPTZ">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"UNIQUE (type, msg_id)">>)),

                {ok, [], []}
            end}
        ]}
    ], fun() ->
        Result = msg_store_repo:ensure_table_exists(),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% create_indexes/1 测试
%% ===================================================================

create_indexes_validates_index_ddl_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(Sql, []) ->
            % 验证索引创建语句
            ?assertNotEqual(nomatch, binary:match(Sql, <<"CREATE INDEX IF NOT EXISTS ">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"_processed_at_idx">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"_available_at_idx">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"_created_at_idx">>)),
            {ok, [], []}
        end}
    ], fun() ->
        Result = msg_store_repo:create_indexes(<<"msg_store_staging">>),
        ?assertEqual(ok, Result)
    end).
