-module(msg_c2g_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_c2g_repo 模块的 EUnit 测试
%%%
%%% 目标：验证 C2G 群组消息数据访问层功能
%%% 覆盖：群组消息查询、插入
%%%===================================================================

%% Common meck expectations used across tests

-define(MOCK_ENV,
    {config_ds, [
        {'env', 1, fun
            (sql_driver) -> pgsql;
            (_) -> undefined
        end}
    ]}
).
-define(MOCK_TSID, {elib_tsid, [{'generate', 1, fun(_Table) -> 123456789 end}]}).

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_table_test_() ->
    ?WITH_MECKS([?MOCK_ENV], fun() ->
        Result = msg_c2g_repo:tablename(),
        ?assertEqual(<<"public.msg_c2g">>, Result)
    end).

%% ===================================================================
%% write_msg/8 测试
%% ===================================================================

write_msg_with_valid_data_test_() ->
    ?WITH_MECKS(
        [
            ?MOCK_ENV,
            ?MOCK_TSID,
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end},
                {'execute', 3, fun(_Conn, _Sql, _Params) -> {ok, []} end}
            ]}
        ],
        fun() ->
            CreatedAt = <<"2024-01-01T00:00:00Z">>,
            MsgId = <<"test_msg_c2g_123">>,
            Payload = <<"{\"text\":\"hello group\"}">>,
            FromId = 1,
            ToUids = [2, 3],
            Gid = 7,
            MsgType = <<"text">>,
            E2EE = <<>>,
            Result = msg_c2g_repo:write_msg(
                CreatedAt, MsgId, Payload, FromId, ToUids, Gid, MsgType, E2EE
            ),
            ?assertEqual(ok, Result)
        end
    ).

write_msg_with_e2ee_test_() ->
    ?WITH_MECKS(
        [
            ?MOCK_ENV,
            ?MOCK_TSID,
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end},
                {'execute', 3, fun(_Conn, _Sql, _Params) -> {ok, []} end}
            ]}
        ],
        fun() ->
            CreatedAt = <<"2024-01-01T00:00:00Z">>,
            MsgId = <<"test_msg_c2g_e2ee">>,
            Payload = <<"{\"text\":\"encrypted\"}">>,
            FromId = 1,
            ToUids = [2, 3],
            Gid = 7,
            MsgType = <<"text">>,
            E2EE = <<"{\"key\":\"...\"}">>,
            Result = msg_c2g_repo:write_msg(
                CreatedAt, MsgId, Payload, FromId, ToUids, Gid, MsgType, E2EE
            ),
            ?assertEqual(ok, Result)
        end
    ).

write_msg_with_integer_timestamp_test_() ->
    ?WITH_MECKS(
        [
            ?MOCK_ENV,
            ?MOCK_TSID,
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end},
                {'execute', 3, fun(_Conn, _Sql, _Params) -> {ok, []} end}
            ]}
        ],
        fun() ->
            % 2024-01-01 00:00:00 毫秒时间戳
            CreatedAt = 1704067200000,
            MsgId = <<"test_msg_c2g_timestamp">>,
            Payload = <<"{\"text\":\"test\"}">>,
            FromId = 1,
            ToUids = [2],
            Gid = 7,
            MsgType = <<"text">>,
            E2EE = <<>>,
            Result = msg_c2g_repo:write_msg(
                CreatedAt, MsgId, Payload, FromId, ToUids, Gid, MsgType, E2EE
            ),
            ?assertEqual(ok, Result)
        end
    ).

write_msg_with_single_touid_test_() ->
    ?WITH_MECKS(
        [
            ?MOCK_ENV,
            ?MOCK_TSID,
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end},
                {'execute', 3, fun(_Conn, _Sql, _Params) -> {ok, []} end}
            ]}
        ],
        fun() ->
            CreatedAt = <<"2024-01-01T00:00:00Z">>,
            MsgId = <<"test_msg_c2g_single_touid">>,
            Payload = <<"{\"text\":\"test\"}">>,
            FromId = 1,
            ToUids = [4],
            Gid = 7,
            MsgType = <<"text">>,
            E2EE = <<>>,
            Result = msg_c2g_repo:write_msg(
                CreatedAt, MsgId, Payload, FromId, ToUids, Gid, MsgType, E2EE
            ),
            ?assertEqual(ok, Result)
        end
    ).

%% ===================================================================
%% 主表写入幂等性测试（msg_id 去重，防 worker 崩溃重启重复入库）
%% ===================================================================

write_msg_main_insert_is_idempotent_test_() ->
    ?WITH_MECKS(
        [
            ?MOCK_ENV,
            ?MOCK_TSID,
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end},
                {'execute', 3, fun(_Conn, Sql, _Params) ->
                    Prev =
                        case get(c2g_captured_sqls) of
                            undefined -> [];
                            L -> L
                        end,
                    put(c2g_captured_sqls, [iolist_to_binary(Sql) | Prev]),
                    {ok, []}
                end}
            ]}
        ],
        fun() ->
            put(c2g_captured_sqls, []),
            ok = msg_c2g_repo:write_msg(
                <<"2024-01-01T00:00:00Z">>,
                <<"dup_msg_1">>,
                <<"{\"t\":\"x\"}">>,
                1,
                [2, 3],
                7,
                <<"text">>,
                <<>>
            ),
            Sqls = get(c2g_captured_sqls),
            %% 主表 msg_c2g 的 INSERT 必须带 msg_id 幂等子句；
            %% timeline 用的是 (to_uid, msg_id, created_at)，不会误匹配此断言
            HasMainIdempotent = lists:any(
                fun(S) ->
                    binary:match(S, <<"ON CONFLICT (msg_id, created_at) DO NOTHING">>) =/= nomatch
                end,
                Sqls
            ),
            ?assert(HasMainIdempotent)
        end
    ).

%% ===================================================================
%% list_by_ids/2 测试
%% ===================================================================

list_by_ids_non_empty_test_() ->
    ?WITH_MECKS(
        [
            ?MOCK_ENV,
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) -> {ok, []} end}
            ]}
        ],
        fun() ->
            Ids = [<<"msg1">>, <<"msg2">>, <<"msg3">>],
            Column = <<"id, msg_id, payload">>,
            Result = msg_c2g_repo:list_by_ids(Ids, Column),
            ?assertMatch({ok, _}, Result),
            case Result of
                {ok, Rows} ->
                    ?assert(is_list(Rows)),
                    ?assert(length(Rows) =< length(Ids));
                _ ->
                    ok
            end
        end
    ).

list_by_ids_empty_list_test_() ->
    ?WITH_MECKS([?MOCK_ENV], fun() ->
        Ids = [],
        Column = <<"id">>,
        Result = msg_c2g_repo:list_by_ids(Ids, Column),
        ?assertEqual({ok, []}, Result)
    end).

list_by_ids_with_single_id_test_() ->
    ?WITH_MECKS(
        [
            ?MOCK_ENV,
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) -> {ok, []} end}
            ]}
        ],
        fun() ->
            Ids = [<<"msg1">>],
            Column = <<"id, msg_id, from_id">>,
            Result = msg_c2g_repo:list_by_ids(Ids, Column),
            ?assertMatch({ok, _}, Result),
            case Result of
                {ok, Rows} ->
                    ?assert(is_list(Rows));
                _ ->
                    ok
            end
        end
    ).

%% ===================================================================
%% delete_msg/1,2 测试
%% ===================================================================

delete_msg_by_msgid_test_() ->
    ?WITH_MECKS(
        [
            ?MOCK_ENV,
            {elib_pg, [
                {'execute', 2, fun(_Sql, _Params) -> {ok, 0} end}
            ]}
        ],
        fun() ->
            MsgId = <<"test_msg_delete_c2g">>,
            Result = msg_c2g_repo:delete_msg(MsgId),
            ?assertMatch({ok, _}, Result)
        end
    ).

delete_msg_with_where_test_() ->
    ?WITH_MECKS(
        [
            ?MOCK_ENV,
            {elib_pg, [
                {'execute', 2, fun(_Sql, _Params) -> {ok, 0} end}
            ]}
        ],
        fun() ->
            Where = <<"WHERE to_id = $1">>,
            Params = [7],
            Result = msg_c2g_repo:delete_msg(Where, Params),
            ?assertMatch({ok, _}, Result)
        end
    ).

delete_msg_with_complex_where_test_() ->
    ?WITH_MECKS(
        [
            ?MOCK_ENV,
            {elib_pg, [
                {'execute', 2, fun(_Sql, _Params) -> {ok, 0} end}
            ]}
        ],
        fun() ->
            Where = <<"WHERE to_id = $1 AND from_id = $2">>,
            Params = [7, 1],
            Result = msg_c2g_repo:delete_msg(Where, Params),
            ?assertMatch({ok, _}, Result)
        end
    ).

%% ===================================================================
%% 引用回复功能测试
%% ===================================================================

write_msg_with_reply_info_test_() ->
    ?WITH_MECKS(
        [
            ?MOCK_ENV,
            ?MOCK_TSID,
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end},
                {'execute', 3, fun(_Conn, _Sql, _Params) -> {ok, []} end}
            ]}
        ],
        fun() ->
            CreatedAt = <<"2024-01-01T00:00:00Z">>,
            MsgId = <<"test_msg_c2g_reply_123">>,
            Payload = <<"{\"text\":\"hello\"}">>,
            FromId = 1,
            ToUids = [2, 3, 4],
            Gid = 7,
            MsgType = <<"text">>,
            E2EE = <<>>,
            ReplyToMsgId = <<"original_msg_456">>,
            ReplyToFromId = 2,
            ReplySnippet = <<"原始群消息内容摘要"/utf8>>,

            Result = msg_c2g_repo:write_msg_with_reply(
                CreatedAt,
                MsgId,
                Payload,
                FromId,
                ToUids,
                Gid,
                MsgType,
                E2EE,
                ReplyToMsgId,
                ReplyToFromId,
                ReplySnippet
            ),
            ?assertMatch(ok, Result)
        end
    ).

find_msg_with_reply_info_test_() ->
    ?WITH_MECKS(
        [
            ?MOCK_ENV,
            {elib_pg, [
                {'query', 2, fun(_Sql, [_MsgId]) ->
                    {ok, [
                        #{
                            <<"from_id">> => 1,
                            <<"to_id">> => 7,
                            <<"created_at">> => <<"2024-01-01">>,
                            <<"payload">> => <<"test">>
                        }
                    ]}
                end}
            ]}
        ],
        fun() ->
            MsgId = <<"test_msg_c2g_find_reply_123">>,
            Result = msg_c2g_repo:find_msg_by_id(MsgId),
            ?assertMatch({ok, _}, Result),
            case Result of
                {ok, Msg} ->
                    ?assert(is_map(Msg)),
                    ?assert(maps:is_key(<<"from_id">>, Msg));
                _ ->
                    ok
            end
        end
    ).

find_msgs_by_reply_to_msg_id_test_() ->
    ?WITH_MECKS([?MOCK_ENV], fun() ->
        OriginalMsgId = <<"original_msg_c2g_789">>,
        Result = msg_c2g_repo:find_by_reply_to_msg_id(OriginalMsgId),
        ?assertEqual({ok, []}, Result)
    end).
