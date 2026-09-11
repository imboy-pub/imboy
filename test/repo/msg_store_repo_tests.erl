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

workspace_guard_meck() ->
    {workspace_guard, [
        {'ensure_writable_tx', 2, fun(_Conn, {group, 100}) ->
            put({?MODULE, workspace_locked}, true),
            ok
        end},
        {'abort_on_error', 1, fun(ok) -> ok end}
    ]}.

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_qualified_table_name_test_() ->
    ?WITH_MECK(
        elib_pg_sql,
        [
            {'public_tablename', 1, fun(<<"msg_store_staging">>) ->
                <<"public.msg_store_staging">>
            end}
        ],
        fun() ->
            Result = msg_store_repo:tablename(),
            ?assertEqual(<<"public.msg_store_staging">>, Result),
            ?assert(is_binary(Result)),
            ?assert(Result =/= <<>>)
        end
    ).

find_staged_is_scoped_to_type_and_pending_state_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun(_) -> <<"public.msg_store_staging">> end}
            ]},
            {elib_pg, [
                {'query', 2, fun(Sql, [<<"c2g">>, <<"shared-id">>]) ->
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"type = $1">>)),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"msg_id = $2">>)),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"processed_at IS NULL">>)),
                    {ok, [#{<<"msg_id">> => <<"shared-id">>}]}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {ok, #{<<"msg_id">> := <<"shared-id">>}},
                msg_store_repo:find_by_msg_id(<<"c2g">>, <<"shared-id">>)
            )
        end
    ).

%% ===================================================================
%% stage/10 测试 - 单聊消息 (integer ToId)
%% ===================================================================

stage_with_integer_toid_validates_all_fields_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun(<<"msg_store_staging">>) ->
                    <<"public.msg_store_staging">>
                end},
                {'insert', 2, fun(_Tb, Data) ->
                    % 验证必需字段存在
                    RequiredFields = [
                        type,
                        msg_id,
                        msg_type,
                        action,
                        payload,
                        from_id,
                        to_id,
                        created_at,
                        server_ts,
                        retry_count
                    ],
                    lists:foreach(
                        fun(Field) ->
                            ?assert(
                                maps:is_key(Field, Data),
                                io_lib:format("Missing field: ~p", [Field])
                            )
                        end,
                        RequiredFields
                    ),

                    % 验证字段值正确性
                    ?assertEqual(<<"c2c">>, maps:get(type, Data)),
                    ?assertEqual(<<"msg123">>, maps:get(msg_id, Data)),
                    ?assertEqual(100, maps:get(from_id, Data)),
                    ?assertEqual(200, maps:get(to_id, Data)),
                    ?assertEqual(0, maps:get(retry_count, Data)),

                    {<<"SELECT 1">>, []}
                end}
            ]},
            {elib_tsid, [
                {'generate', 1, fun(msg_store) -> 12345 end}
            ]},
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            Result = msg_store_repo:stage(
                <<"c2c">>,
                <<"msg123">>,
                <<"text">>,
                <<"send">>,
                <<>>,
                <<"{\"body\": \"hello\"}">>,
                100,
                200,
                <<"2024-01-01T00:00:00Z">>,
                <<"2024-01-01T00:00:01Z">>
            ),
            ?assertEqual({ok, 12345}, Result)
        end
    ).

stage_with_empty_e2ee_converts_to_null_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun(_) -> <<"public.msg_store_staging">> end},
                {'insert', 2, fun(_Tb, Data) ->
                    E2EE = maps:get(e2ee, Data),
                    ?assertEqual(null, E2EE),
                    {<<"SELECT 1">>, []}
                end}
            ]},
            {elib_tsid, [
                {'generate', 1, fun(msg_store) -> 12345 end}
            ]},
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            Result = msg_store_repo:stage(
                <<"c2c">>,
                <<"msg456">>,
                <<"text">>,
                <<"send">>,
                <<>>,
                <<"{}">>,
                100,
                200,
                <<"2024-01-01T00:00:00Z">>,
                <<"2024-01-01T00:00:01Z">>
            ),
            ?assertEqual({ok, 12345}, Result)
        end
    ).

stage_with_e2ee_data_preserves_value_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun(_) -> <<"public.msg_store_staging">> end},
                {'insert', 2, fun(_Tb, Data) ->
                    E2EE = maps:get(e2ee, Data),
                    %% e2ee 列是 JSONB：裸 binary 会被包装为 JSON 字符串落库
                    ?assertEqual(<<"\"e2ee_data\"">>, E2EE),
                    {<<"SELECT 1">>, []}
                end}
            ]},
            {elib_tsid, [
                {'generate', 1, fun(msg_store) -> 12345 end}
            ]},
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            Result = msg_store_repo:stage(
                <<"c2c">>,
                <<"msg789">>,
                <<"text">>,
                <<"send">>,
                <<"e2ee_data">>,
                <<"{}">>,
                100,
                200,
                <<"2024-01-01T00:00:00Z">>,
                <<"2024-01-01T00:00:01Z">>
            ),
            ?assertEqual({ok, 12345}, Result)
        end
    ).

stage_unique_violation_converts_to_business_error_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
            ]},
            {elib_tsid, [
                {'generate', 1, fun(msg_store) -> 12345 end}
            ]},
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) ->
                    {error,
                        {error, {error, <<"23505">>, unique_violation, <<"duplicate key">>, []}}}
                end}
            ]}
        ],
        fun() ->
            Result = msg_store_repo:stage(
                <<"c2c">>,
                <<"msg_duplicate">>,
                <<"text">>,
                <<"send">>,
                <<>>,
                <<"{}">>,
                100,
                200,
                <<"2024-01-01T00:00:00Z">>,
                <<"2024-01-01T00:00:01Z">>
            ),
            ?assertMatch({error, {unique_violation, <<"msg_duplicate">>}}, Result)
        end
    ).

stage_with_zero_from_id_accepted_test_() ->
    % 边界测试：from_id = 0 当前源码不拒绝
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
            ]},
            {elib_tsid, [
                {'generate', 1, fun(msg_store) -> 12345 end}
            ]},
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            Result = msg_store_repo:stage(
                <<"c2c">>,
                <<"msg_zero">>,
                <<"text">>,
                <<"send">>,
                <<>>,
                <<"{}">>,
                0,
                200,
                <<"2024-01-01T00:00:00Z">>,
                <<"2024-01-01T00:00:01Z">>
            ),
            ?assertMatch({ok, _}, Result)
        end
    ).

stage_with_negative_from_id_accepted_test_() ->
    % 边界测试：负数 from_id 当前源码不拒绝
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
            ]},
            {elib_tsid, [
                {'generate', 1, fun(msg_store) -> 12345 end}
            ]},
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            Result = msg_store_repo:stage(
                <<"c2c">>,
                <<"msg_negative">>,
                <<"text">>,
                <<"send">>,
                <<>>,
                <<"{}">>,
                -1,
                200,
                <<"2024-01-01T00:00:00Z">>,
                <<"2024-01-01T00:00:01Z">>
            ),
            ?assertMatch({ok, _}, Result)
        end
    ).

%% ===================================================================
%% stage/10 测试 - 群聊消息 (list ToIdList)
%% ===================================================================

c2g_stage_with_recipient_list_is_rejected_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun(_) -> <<"public.msg_store_staging">> end},
                {'insert', 2, fun(_Tb, Data) ->
                    ?assert(maps:is_key(to_id_list, Data)),
                    ?assertNot(maps:is_key(to_id, Data)),
                    ToIdList = maps:get(to_id_list, Data),
                    ?assertEqual([100, 200, 300], ToIdList),
                    ?assertEqual(<<"c2g">>, maps:get(type, Data)),
                    {<<"SELECT 1">>, []}
                end}
            ]},
            {elib_tsid, [
                {'generate', 1, fun(msg_store) -> 12345 end}
            ]},
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            Result = msg_store_repo:stage(
                <<"c2g">>,
                <<"msg_group">>,
                <<"text">>,
                <<"send">>,
                <<>>,
                <<"{}">>,
                50,
                [100, 200, 300],
                <<"2024-01-01T00:00:00Z">>,
                <<"2024-01-01T00:00:01Z">>
            ),
            ?assertEqual({error, c2g_group_id_required}, Result)
        end
    ).

c2g_stage_with_empty_recipient_list_is_rejected_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
            ]},
            {elib_tsid, [
                {'generate', 1, fun(msg_store) -> 12345 end}
            ]},
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            Result = msg_store_repo:stage(
                <<"c2g">>,
                <<"msg_empty_group">>,
                <<"text">>,
                <<"send">>,
                <<>>,
                <<"{}">>,
                50,
                [],
                <<"2024-01-01T00:00:00Z">>,
                <<"2024-01-01T00:00:01Z">>
            ),
            ?assertEqual({error, c2g_group_id_required}, Result)
        end
    ).

c2g_stage_list_bypass_precedes_database_write_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
            ]},
            {elib_tsid, [
                {'generate', 1, fun(msg_store) -> 12345 end}
            ]},
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) ->
                    {error,
                        {error, {error, <<"23505">>, unique_violation, <<"duplicate key">>, []}}}
                end}
            ]}
        ],
        fun() ->
            Result = msg_store_repo:stage(
                <<"c2g">>,
                <<"msg_group_dup">>,
                <<"text">>,
                <<"send">>,
                <<>>,
                <<"{}">>,
                50,
                [100, 200],
                <<"2024-01-01T00:00:00Z">>,
                <<"2024-01-01T00:00:01Z">>
            ),
            ?assertEqual({error, c2g_group_id_required}, Result)
        end
    ).

c2g_stage_commits_authorized_snapshot_and_role_test_() ->
    ?WITH_MECKS(
        [
            {msg_archive_ds, [
                {'conv_key_c2g', 1, fun(100) -> <<"c2g:100">> end}
            ]},
            workspace_guard_meck(),
            {elib_tsid, [
                {'generate', 1, fun(msg_store) -> 12345 end}
            ]},
            {epgsql, [
                {'equery', 3, fun(_, Sql, [<<"c2g:100">>]) ->
                    ?assertEqual(true, get({?MODULE, workspace_locked})),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"RETURNING seq">>)),
                    {ok, [], [], [{7}]}
                end}
            ]},
            {elib_pg_sql, [
                {'public_tablename', 1, fun(_) -> <<"public.msg_store_staging">> end},
                {'insert', 2, fun(_, Data) ->
                    ?assertEqual(100, maps:get(to_id, Data)),
                    ?assertEqual([50, 60], maps:get(to_id_list, Data)),
                    ?assertEqual(7, maps:get(conv_seq, Data)),
                    {<<"INSERT staged">>, []}
                end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Tx) -> Tx(fake_conn) end},
                {'query', 3, fun
                    (_, Sql, [100, 50, 3, 5001]) ->
                        ?assertNotEqual(nomatch, binary:match(Sql, <<"caller.role >= $3">>)),
                        ?assertNotEqual(nomatch, binary:match(Sql, <<"caller.status = 1">>)),
                        ?assertNotEqual(nomatch, binary:match(Sql, <<"recipient.status = 1">>)),
                        ?assertNotEqual(nomatch, binary:match(Sql, <<"grp.status = 1">>)),
                        {ok, [#{<<"user_id">> => 50}, #{<<"user_id">> => 60}]};
                    (
                        _,
                        Sql,
                        [
                            <<"msg-c2g-role">>,
                            50,
                            100,
                            <<>>,
                            <<"text">>,
                            null,
                            <<"{}">>,
                            <<"did-50">>,
                            _
                        ]
                    ) ->
                        request_ledger_new(Sql, <<"msg-c2g-role">>);
                    (_, <<"INSERT staged">>, []) ->
                        {ok, 1};
                    (_, Sql, [<<"msg-c2g-role">>, 50, 100, 7, [50, 60], _]) ->
                        ?assertNotEqual(
                            nomatch, binary:match(Sql, <<"msg_c2g_recipient_snapshot">>)
                        ),
                        {ok, [#{<<"msg_id">> => <<"msg-c2g-role">>}]};
                    (_, Sql, [<<"msg-c2g-role">>, 50, 100, 7]) ->
                        ?assertNotEqual(nomatch, binary:match(Sql, <<"UPDATE public.attachment">>)),
                        ?assertNotEqual(nomatch, binary:match(Sql, <<"anchor_conv_seq = $4">>)),
                        ?assertNotEqual(nomatch, binary:match(Sql, <<"group_file_id IS NULL">>)),
                        ?assertNotEqual(nomatch, binary:match(Sql, <<"anchor_conv_seq IS NULL">>)),
                        {ok, 0}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, 12345, [50, 60]},
                msg_store_repo:stage(
                    <<"c2g">>,
                    <<"msg-c2g-role">>,
                    <<"text">>,
                    <<>>,
                    null,
                    <<"{}">>,
                    50,
                    100,
                    <<"2026-09-11T00:00:00Z">>,
                    <<"2026-09-11T00:00:00Z">>,
                    <<"did-50">>,
                    3
                )
            )
        end
    ).

c2g_stage_archived_workspace_does_not_allocate_sequence_test_() ->
    ?WITH_MECKS(
        [
            {msg_archive_ds, [
                {'conv_key_c2g', 1, fun(_) -> <<"c2g:100">> end}
            ]},
            {workspace_guard, [
                {'ensure_writable_tx', 2, fun(_, {group, 100}) ->
                    {error, {980, <<"archived">>}}
                end},
                {'abort_on_error', 1, fun({error, Reason}) -> throw({abort_tx, Reason}) end}
            ]},
            {elib_tsid, [{'generate', 1, fun(msg_store) -> 12345 end}]},
            {elib_pg_sql, [
                {'public_tablename', 1, fun(_) -> <<"public.msg_store_staging">> end}
            ]},
            {epgsql, [
                {'equery', 3, fun(_, _, _) -> erlang:error(sequence_must_not_be_allocated) end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Tx) ->
                    try Tx(fake_conn) of
                        Result -> Result
                    catch
                        throw:{abort_tx, Reason} -> {error, Reason}
                    end
                end}
            ]}
        ],
        fun() ->
            ?assertEqual({error, {980, <<"archived">>}}, stage_c2g_test_msg(<<"archived">>)),
            ?assertEqual(0, meck:num_calls(epgsql, equery, 3))
        end
    ).

c2g_stage_sequence_failures_are_rolled_back_test_() ->
    ?WITH_MECKS(
        [
            {msg_archive_ds, [
                {'conv_key_c2g', 1, fun(_) -> <<"c2g:100">> end}
            ]},
            workspace_guard_meck(),
            {elib_tsid, [{'generate', 1, fun(msg_store) -> 12345 end}]},
            {elib_pg_sql, [
                {'public_tablename', 1, fun(_) -> <<"public.msg_store_staging">> end}
            ]},
            {epgsql, [
                {'equery', 3, fun(_, _, _) -> get({?MODULE, sequence_result}) end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Tx) ->
                    try Tx(fake_conn) of
                        Result -> Result
                    catch
                        throw:{rollback, Reason} -> {rollback, Reason}
                    end
                end}
            ]}
        ],
        fun() ->
            put({?MODULE, sequence_result}, {error, connection_lost}),
            ?assertEqual(
                {error, {conv_seq_allocate_failed, connection_lost}},
                stage_c2g_test_msg(<<"seq-error">>)
            ),
            put({?MODULE, sequence_result}, {ok, [], [], []}),
            ?assertMatch(
                {error, {conv_seq_allocate_failed, {unexpected_result, _}}},
                stage_c2g_test_msg(<<"seq-empty">>)
            ),
            put({?MODULE, sequence_result}, {ok, [], [], [{0}]}),
            ?assertMatch(
                {error, {conv_seq_allocate_failed, {unexpected_result, _}}},
                stage_c2g_test_msg(<<"seq-invalid">>)
            )
        end
    ).

stage_c2g_test_msg(MsgId) ->
    msg_store_repo:stage(
        <<"c2g">>,
        MsgId,
        <<"text">>,
        <<>>,
        null,
        <<"{}">>,
        50,
        100,
        <<"2026-09-11T00:00:00Z">>,
        <<"2026-09-11T00:00:00Z">>,
        <<>>,
        1
    ).

request_ledger_new(Sql, MsgId) ->
    ?assertNotEqual(nomatch, binary:match(Sql, <<"msg_c2g_request_ledger">>)),
    ?assertNotEqual(nomatch, binary:match(Sql, <<"digest(convert_to">>)),
    ?assertNotEqual(nomatch, binary:match(Sql, <<"{payload,revoked_at}">>)),
    {ok, [#{<<"msg_id">> => MsgId}]}.

c2g_stage_attachment_bind_error_rolls_back_test_() ->
    ?WITH_MECKS(
        [
            {msg_archive_ds, [
                {'conv_key_c2g', 1, fun(_) -> <<"c2g:100">> end}
            ]},
            workspace_guard_meck(),
            {elib_tsid, [{'generate', 1, fun(msg_store) -> 12345 end}]},
            {epgsql, [{'equery', 3, fun(_, _, _) -> {ok, [], [], [{7}]} end}]},
            {elib_pg_sql, [
                {'public_tablename', 1, fun(_) -> <<"public.msg_store_staging">> end},
                {'insert', 2, fun(_, _) -> {<<"INSERT staged">>, []} end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Tx) ->
                    try Tx(fake_conn) of
                        Result -> Result
                    catch
                        throw:{rollback, Reason} -> {rollback, Reason}
                    end
                end},
                {'query', 3, fun
                    (_, _, [100, 50, 1, 5001]) ->
                        {ok, [#{<<"user_id">> => 50}, #{<<"user_id">> => 60}]};
                    (
                        _,
                        Sql,
                        [
                            <<"msg-c2g-bind-error">>,
                            50,
                            100,
                            <<>>,
                            <<"text">>,
                            null,
                            <<"{}">>,
                            null,
                            _
                        ]
                    ) ->
                        request_ledger_new(Sql, <<"msg-c2g-bind-error">>);
                    (_, <<"INSERT staged">>, []) ->
                        {ok, 1};
                    (_, _, [<<"msg-c2g-bind-error">>, 50, 100, 7, [50, 60], _]) ->
                        {ok, [#{<<"msg_id">> => <<"msg-c2g-bind-error">>}]};
                    (_, _, [<<"msg-c2g-bind-error">>, 50, 100, 7]) ->
                        {error, connection_lost}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, {attachment_anchor_bind_failed, connection_lost}},
                msg_store_repo:stage(
                    <<"c2g">>,
                    <<"msg-c2g-bind-error">>,
                    <<"text">>,
                    <<>>,
                    null,
                    <<"{}">>,
                    50,
                    100,
                    <<"2026-09-11T00:00:00Z">>,
                    <<"2026-09-11T00:00:00Z">>,
                    <<>>,
                    1
                )
            )
        end
    ).

c2g_stage_duplicate_does_not_bind_attachment_test_() ->
    ?WITH_MECKS(
        [
            {msg_archive_ds, [
                {'conv_key_c2g', 1, fun(_) -> <<"c2g:100">> end}
            ]},
            workspace_guard_meck(),
            {elib_tsid, [{'generate', 1, fun(msg_store) -> 12345 end}]},
            {epgsql, [{'equery', 3, fun(_, _, _) -> {ok, [], [], [{7}]} end}]},
            {elib_pg_sql, [
                {'public_tablename', 1, fun(_) -> <<"public.msg_store_staging">> end},
                {'insert', 2, fun(_, _) -> {<<"INSERT duplicate">>, []} end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Tx) ->
                    try Tx(fake_conn) of
                        Result -> Result
                    catch
                        throw:{rollback, Reason} -> {rollback, Reason}
                    end
                end},
                {'query', 3, fun
                    (_, _, [100, 50, 1, 5001]) ->
                        {ok, [#{<<"user_id">> => 50}]};
                    (
                        _,
                        _,
                        [
                            <<"msg-c2g-duplicate">>,
                            50,
                            100,
                            <<>>,
                            <<"text">>,
                            null,
                            <<"{}">>,
                            null,
                            _
                        ]
                    ) ->
                        {ok, []};
                    (
                        _,
                        Sql,
                        [
                            <<"msg-c2g-duplicate">>,
                            <<>>,
                            <<"text">>,
                            null,
                            <<"{}">>,
                            null
                        ]
                    ) ->
                        case binary:match(Sql, <<"INSERT INTO">>) of
                            nomatch ->
                                ?assertEqual(nomatch, binary:match(Sql, <<"$7">>)),
                                {ok, [
                                    #{
                                        <<"from_id">> => 50,
                                        <<"to_gid">> => 100,
                                        <<"same_action">> => true,
                                        <<"same_request">> => true
                                    }
                                ]};
                            _ ->
                                {ok, []}
                        end
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, {unique_violation, <<"msg-c2g-duplicate">>}},
                msg_store_repo:stage(
                    <<"c2g">>,
                    <<"msg-c2g-duplicate">>,
                    <<"text">>,
                    <<>>,
                    null,
                    <<"{}">>,
                    50,
                    100,
                    <<"2026-09-11T00:00:00Z">>,
                    <<"2026-09-11T00:00:00Z">>,
                    <<>>,
                    1
                )
            ),
            ?assertEqual(3, meck:num_calls(elib_pg, query, 3))
        end
    ).

c2g_stage_rejects_msg_id_owned_by_another_sender_test_() ->
    ?WITH_MECKS(
        [
            {msg_archive_ds, [
                {'conv_key_c2g', 1, fun(_) -> <<"c2g:100">> end}
            ]},
            workspace_guard_meck(),
            {elib_tsid, [{'generate', 1, fun(msg_store) -> 12345 end}]},
            {epgsql, [{'equery', 3, fun(_, _, _) -> {ok, [], [], [{7}]} end}]},
            {elib_pg_sql, [
                {'public_tablename', 1, fun(_) -> <<"public.msg_store_staging">> end},
                {'insert', 2, fun(_, _) -> erlang:error(staging_insert_must_not_run) end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Tx) ->
                    try Tx(fake_conn) of
                        Result -> Result
                    catch
                        throw:{rollback, Reason} -> {rollback, Reason}
                    end
                end},
                {'query', 3, fun
                    (_, _, [100, 50, 1, 5001]) ->
                        {ok, [#{<<"user_id">> => 50}]};
                    (
                        _,
                        _,
                        [<<"foreign-msg-id">>, 50, 100, <<>>, <<"text">>, null, <<"{}">>, null, _]
                    ) ->
                        {ok, []};
                    (
                        _,
                        Sql,
                        [<<"foreign-msg-id">>, <<>>, <<"text">>, null, <<"{}">>, null]
                    ) ->
                        case binary:match(Sql, <<"INSERT INTO">>) of
                            nomatch ->
                                {ok, [
                                    #{
                                        <<"from_id">> => 999,
                                        <<"to_gid">> => 100,
                                        <<"same_action">> => true,
                                        <<"same_request">> => true
                                    }
                                ]};
                            _ ->
                                {ok, []}
                        end
                end}
            ]}
        ],
        fun() ->
            ?assertEqual({error, msg_id_conflict}, stage_c2g_test_msg(<<"foreign-msg-id">>))
        end
    ).

c2g_stage_5000_recipients_commits_test_() ->
    RecipientUids = lists:seq(1, 5000),
    ?WITH_MECKS(
        [
            {msg_archive_ds, [
                {'conv_key_c2g', 1, fun(_) -> <<"c2g:100">> end}
            ]},
            workspace_guard_meck(),
            {elib_tsid, [
                {'generate', 1, fun(msg_store) -> 12345 end}
            ]},
            {epgsql, [
                {'equery', 3, fun(_, _, _) -> {ok, [], [], [{7}]} end}
            ]},
            {elib_pg_sql, [
                {'public_tablename', 1, fun(_) -> <<"public.msg_store_staging">> end},
                {'insert', 2, fun(_, Data) ->
                    ?assertEqual(RecipientUids, maps:get(to_id_list, Data)),
                    {<<"INSERT staged">>, []}
                end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Tx) -> Tx(fake_conn) end},
                {'query', 3, fun
                    (_, _, [100, 50, 1, 5001]) ->
                        {ok, [#{<<"user_id">> => Uid} || Uid <- RecipientUids]};
                    (
                        _,
                        Sql,
                        [<<"msg-c2g-limit-ok">>, 50, 100, <<>>, <<"text">>, null, <<"{}">>, null, _]
                    ) ->
                        request_ledger_new(Sql, <<"msg-c2g-limit-ok">>);
                    (_, <<"INSERT staged">>, []) ->
                        {ok, 1};
                    (_, _, [<<"msg-c2g-limit-ok">>, 50, 100, 7, SnapshotUids, _]) ->
                        ?assertEqual(RecipientUids, SnapshotUids),
                        {ok, [#{<<"msg_id">> => <<"msg-c2g-limit-ok">>}]};
                    (_, Sql, [<<"msg-c2g-limit-ok">>, 50, 100, 7]) ->
                        ?assertNotEqual(nomatch, binary:match(Sql, <<"UPDATE public.attachment">>)),
                        {ok, 5000}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, 12345, RecipientUids},
                msg_store_repo:stage(
                    <<"c2g">>,
                    <<"msg-c2g-limit-ok">>,
                    <<"text">>,
                    <<>>,
                    null,
                    <<"{}">>,
                    50,
                    100,
                    <<"2026-09-11T00:00:00Z">>,
                    <<"2026-09-11T00:00:00Z">>,
                    <<>>,
                    1
                )
            )
        end
    ).

c2g_stage_5001_recipients_rolls_back_test_() ->
    ?WITH_MECKS(
        [
            {msg_archive_ds, [
                {'conv_key_c2g', 1, fun(_) -> <<"c2g:100">> end}
            ]},
            workspace_guard_meck(),
            {elib_tsid, [
                {'generate', 1, fun(msg_store) -> 12345 end}
            ]},
            {elib_pg_sql, [
                {'public_tablename', 1, fun(_) -> <<"public.msg_store_staging">> end}
            ]},
            {epgsql, [
                {'equery', 3, fun(_, _, _) -> {ok, [], [], [{7}]} end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Tx) ->
                    try Tx(fake_conn) of
                        Result -> Result
                    catch
                        throw:{rollback, Reason} -> {rollback, Reason}
                    end
                end},
                {'query', 3, fun(_, _, [100, 50, 1, 5001]) ->
                    {ok, [#{<<"user_id">> => Uid} || Uid <- lists:seq(1, 5001)]}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, recipient_limit_exceeded},
                msg_store_repo:stage(
                    <<"c2g">>,
                    <<"msg-c2g-limit">>,
                    <<"text">>,
                    <<>>,
                    null,
                    <<"{}">>,
                    50,
                    100,
                    <<"2026-09-11T00:00:00Z">>,
                    <<"2026-09-11T00:00:00Z">>,
                    <<>>,
                    1
                )
            )
        end
    ).

c2g_stage_snapshot_error_rolls_back_test_() ->
    ?WITH_MECKS(
        [
            {msg_archive_ds, [
                {'conv_key_c2g', 1, fun(_) -> <<"c2g:100">> end}
            ]},
            workspace_guard_meck(),
            {elib_tsid, [
                {'generate', 1, fun(msg_store) -> 12345 end}
            ]},
            {elib_pg_sql, [
                {'public_tablename', 1, fun(_) -> <<"public.msg_store_staging">> end}
            ]},
            {epgsql, [
                {'equery', 3, fun(_, _, _) -> {ok, [], [], [{7}]} end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Tx) ->
                    try Tx(fake_conn) of
                        Result -> Result
                    catch
                        throw:{rollback, Reason} -> {rollback, Reason}
                    end
                end},
                {'query', 3, fun(_, _, [100, 50, 1, 5001]) -> {error, connection_lost} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, {recipient_snapshot_failed, connection_lost}},
                msg_store_repo:stage(
                    <<"c2g">>,
                    <<"msg-c2g-db-error">>,
                    <<"text">>,
                    <<>>,
                    null,
                    <<"{}">>,
                    50,
                    100,
                    <<"2026-09-11T00:00:00Z">>,
                    <<"2026-09-11T00:00:00Z">>,
                    <<>>,
                    1
                )
            )
        end
    ).

c2g_action_stage_uses_original_snapshot_and_generation_test_() ->
    ?WITH_MECKS(
        [
            {msg_archive_ds, [
                {'conv_key_c2g', 1, fun(100) -> <<"c2g:100">> end}
            ]},
            workspace_guard_meck(),
            {elib_tsid, [
                {'generate', 1, fun(msg_store) -> 12346 end}
            ]},
            {epgsql, [
                {'equery', 3, fun(_, Sql, [<<"c2g:100">>]) ->
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"RETURNING seq">>)),
                    {ok, [], [], [{8}]}
                end}
            ]},
            {elib_pg_sql, [
                {'public_tablename', 1, fun(_) -> <<"public.msg_store_staging">> end},
                {'insert', 2, fun(_, Data) ->
                    ?assertEqual([50, 60], maps:get(to_id_list, Data)),
                    ?assertEqual(8, maps:get(conv_seq, Data)),
                    {<<"INSERT action">>, []}
                end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Tx) -> Tx(fake_conn) end},
                {'query', 3, fun
                    (_, Sql, [100, 50, 1, <<"original-msg">>, 5001]) ->
                        ?assertNotEqual(
                            nomatch, binary:match(Sql, <<"msg_c2g_recipient_snapshot">>)
                        ),
                        ?assertEqual(nomatch, binary:match(Sql, <<"msg_c2g_timeline">>)),
                        ?assertEqual(nomatch, binary:match(Sql, <<"msg_store_staging">>)),
                        ?assertNotEqual(nomatch, binary:match(Sql, <<"caller_gen.start_seq">>)),
                        ?assertNotEqual(nomatch, binary:match(Sql, <<"recipient_gen.start_seq">>)),
                        ?assertNotEqual(nomatch, binary:match(Sql, <<"unnest">>)),
                        {ok, [#{<<"user_id">> => 50}, #{<<"user_id">> => 60}]};
                    (
                        _,
                        Sql,
                        [
                            <<"action-msg">>,
                            50,
                            100,
                            <<"message_revoke_ack">>,
                            <<"custom">>,
                            null,
                            <<"{}">>,
                            <<"did-50">>,
                            _
                        ]
                    ) ->
                        request_ledger_new(Sql, <<"action-msg">>);
                    (_, _, [<<"action-msg">>, 50, 100, 8, [50, 60], _]) ->
                        {ok, [#{<<"msg_id">> => <<"action-msg">>}]};
                    (_, <<"INSERT action">>, []) ->
                        {ok, 1}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, 12346, [50, 60]},
                msg_store_repo:stage_action(
                    <<"c2g">>,
                    <<"action-msg">>,
                    <<"custom">>,
                    <<"message_revoke_ack">>,
                    null,
                    <<"{}">>,
                    50,
                    100,
                    <<"2026-09-11T00:00:00Z">>,
                    <<"2026-09-11T00:00:00Z">>,
                    <<"did-50">>,
                    1,
                    <<"original-msg">>
                )
            ),
            ?assertEqual(4, meck:num_calls(elib_pg, query, 3))
        end
    ).

c2g_action_stage_snapshot_primary_key_is_durable_idempotency_test_() ->
    ?WITH_MECKS(
        [
            {msg_archive_ds, [
                {'conv_key_c2g', 1, fun(100) -> <<"c2g:100">> end}
            ]},
            workspace_guard_meck(),
            {elib_tsid, [
                {'generate', 1, fun(msg_store) -> 12346 end}
            ]},
            {epgsql, [
                {'equery', 3, fun(_, _, _) -> {ok, [], [], [{9}]} end}
            ]},
            {elib_pg_sql, [
                {'public_tablename', 1, fun(_) -> <<"public.msg_store_staging">> end},
                {'insert', 2, fun(_, _) -> erlang:error(staging_insert_must_not_run) end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Tx) ->
                    try Tx(fake_conn) of
                        Result -> Result
                    catch
                        throw:{rollback, Reason} -> {rollback, Reason}
                    end
                end},
                {'query', 3, fun
                    (_, _, [100, 50, 1, <<"original-msg">>, 5001]) ->
                        {ok, [#{<<"user_id">> => 50}, #{<<"user_id">> => 60}]};
                    (
                        _,
                        _,
                        [
                            <<"replayed-action">>,
                            50,
                            100,
                            <<"message_edit_ack">>,
                            <<"text">>,
                            null,
                            <<"{}">>,
                            null,
                            _
                        ]
                    ) ->
                        {ok, []};
                    (
                        _,
                        Sql,
                        [
                            <<"replayed-action">>,
                            <<"message_edit_ack">>,
                            <<"text">>,
                            null,
                            <<"{}">>,
                            null
                        ]
                    ) ->
                        case binary:match(Sql, <<"INSERT INTO">>) of
                            nomatch ->
                                {ok, [
                                    #{
                                        <<"from_id">> => 50,
                                        <<"to_gid">> => 100,
                                        <<"same_action">> => true,
                                        <<"same_request">> => true
                                    }
                                ]};
                            _ ->
                                {ok, []}
                        end
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, {unique_violation, <<"replayed-action">>}},
                msg_store_repo:stage_action(
                    <<"c2g">>,
                    <<"replayed-action">>,
                    <<"text">>,
                    <<"message_edit_ack">>,
                    null,
                    <<"{}">>,
                    50,
                    100,
                    <<"2026-09-11T02:00:00Z">>,
                    <<"2026-09-11T02:00:00Z">>,
                    <<>>,
                    1,
                    <<"original-msg">>
                )
            )
        end
    ).

c2g_action_stage_rejects_invisible_original_test_() ->
    ?WITH_MECKS(
        [
            {msg_archive_ds, [
                {'conv_key_c2g', 1, fun(_) -> <<"c2g:100">> end}
            ]},
            workspace_guard_meck(),
            {elib_tsid, [
                {'generate', 1, fun(msg_store) -> 12346 end}
            ]},
            {elib_pg_sql, [
                {'public_tablename', 1, fun(_) -> <<"public.msg_store_staging">> end}
            ]},
            {epgsql, [
                {'equery', 3, fun(_, _, _) -> {ok, [], [], [{8}]} end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Tx) ->
                    try Tx(fake_conn) of
                        Result -> Result
                    catch
                        throw:{rollback, Reason} -> {rollback, Reason}
                    end
                end},
                {'query', 3, fun(_, _, [100, 50, 1, <<"old-generation-msg">>, 5001]) ->
                    {ok, []}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, action_target_forbidden},
                msg_store_repo:stage_action(
                    <<"c2g">>,
                    <<"action-msg">>,
                    <<"text">>,
                    <<"message_edit_ack">>,
                    null,
                    <<"{}">>,
                    50,
                    100,
                    <<"2026-09-11T00:00:00Z">>,
                    <<"2026-09-11T00:00:00Z">>,
                    <<>>,
                    1,
                    <<"old-generation-msg">>
                )
            )
        end
    ).

%% ===================================================================
%% unstage/2 测试
%% ===================================================================

unstage_validates_sql_parameters_test_() ->
    ?WITH_MECKS(
        [
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
        ],
        fun() ->
            Result = msg_store_repo:unstage(<<"c2c">>, <<"msg123">>),
            ?assertEqual({ok, 1}, Result)
        end
    ).

unstage_nonexistent_returns_zero_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
            ]},
            {elib_pg, [
                {'execute', 2, fun(_Sql, _Params) -> {ok, 0} end}
            ]}
        ],
        fun() ->
            Result = msg_store_repo:unstage(<<"c2c">>, <<"nonexistent">>),
            ?assertEqual({ok, 0}, Result)
        end
    ).

unstage_with_empty_type_accepted_test_() ->
    % 边界测试：空 Type 被传递到数据库（源码不验证）
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
            ]},
            {elib_pg, [
                {'execute', 2, fun(_Sql, _Params) -> {ok, 0} end}
            ]}
        ],
        fun() ->
            Result = msg_store_repo:unstage(<<>>, <<"msg123">>),
            % 源码不验证空 Type，直接传到数据库
            ?assertMatch({ok, _}, Result)
        end
    ).

%% ===================================================================
%% claim_pending/2 测试
%% ===================================================================

claim_pending_validates_transaction_logic_test_() ->
    ?WITH_MECKS(
        [
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
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"conv_seq">>)),

                    % 验证 LIMIT 参数
                    ?assert(is_integer(Limit)),
                    ?assertEqual(10, Limit),
                    ?assert(Limit > 0),

                    {ok, [
                        #{<<"id">> => 1, <<"msg_id">> => <<"msg1">>, <<"payload">> => <<"{}">>},
                        #{<<"id">> => 2, <<"msg_id">> => <<"msg2">>, <<"payload">> => <<"{}">>}
                    ]}
                end},
                {'execute', 3, fun(_Conn, LeaseSql, [_LeaseSeconds, Ids]) ->
                    % 验证租约 SQL
                    ?assertNotEqual(nomatch, binary:match(LeaseSql, <<"UPDATE ">>)),
                    ?assertNotEqual(
                        nomatch, binary:match(LeaseSql, <<"SET available_at = NOW()">>)
                    ),
                    ?assertNotEqual(nomatch, binary:match(LeaseSql, <<"INTERVAL '1 second' * ">>)),

                    % 验证 ID 列表
                    ?assert(is_list(Ids)),
                    ?assert(lists:all(fun(Id) -> is_integer(Id) end, Ids)),
                    ?assertEqual([1, 2], Ids),

                    {ok, 2}
                end}
            ]}
        ],
        fun() ->
            Result = msg_store_repo:claim_pending(10, 60),
            ?assertMatch({ok, [_, _]}, Result),
            case Result of
                {ok, Rows} ->
                    % 验证返回的行包含必要字段
                    lists:foreach(
                        fun(Row) ->
                            ?assert(maps:is_key(<<"id">>, Row)),
                            ?assert(maps:is_key(<<"msg_id">>, Row)),
                            ?assert(maps:is_key(<<"payload">>, Row))
                        end,
                        Rows
                    );
                _ ->
                    ?assert(false)
            end
        end
    ).

claim_pending_empty_returns_empty_list_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(TxFun) -> TxFun(self()) end},
                {'query', 3, fun(_Conn, _Sql, _Params) -> {ok, []} end}
            ]}
        ],
        fun() ->
            Result = msg_store_repo:claim_pending(10, 60),
            ?assertEqual({ok, []}, Result)
        end
    ).

claim_pending_with_zero_limit_test_() ->
    % 边界测试：Limit = 0 应该返回空列表
    ?WITH_MECKS(
        [
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
        ],
        fun() ->
            Result = msg_store_repo:claim_pending(0, 60),
            ?assertEqual({ok, []}, Result)
        end
    ).

%% ===================================================================
%% mark_processed/1 测试（实现已从 /2 收敛为按 msg_id 单参）
%% ===================================================================

mark_processed_validates_sql_structure_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
            ]},
            {elib_pg, [
                {'execute', 2, fun(Sql, [MsgId]) ->
                    % 验证 SQL 结构
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"UPDATE ">>)),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"SET processed_at = NOW()">>)),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"error_msg = NULL">>)),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"WHERE msg_id = $1">>)),

                    % 验证参数
                    ?assertEqual(<<"msg123">>, MsgId),

                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            Result = msg_store_repo:mark_processed(<<"msg123">>),
            ?assertEqual({ok, 1}, Result)
        end
    ).

%% ===================================================================
%% mark_failed/4 测试
%% ===================================================================

mark_failed_validates_parameters_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
            ]},
            {elib_pg, [
                {'execute', 2, fun(Sql, [Type, MsgId, ErrorMsg, DelaySeconds]) ->
                    % 验证 SQL 结构
                    ?assertNotEqual(
                        nomatch, binary:match(Sql, <<"retry_count = retry_count + 1">>)
                    ),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"error_msg = $3">>)),
                    ?assertNotEqual(
                        nomatch, binary:match(Sql, <<"available_at = NOW() + INTERVAL">>)
                    ),

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
        ],
        fun() ->
            Result = msg_store_repo:mark_failed(
                <<"c2c">>, <<"msg123">>, <<"connection failed"/utf8>>, 60
            ),
            ?assertEqual({ok, 1}, Result)
        end
    ).

mark_failed_with_zero_delay_test_() ->
    % 边界测试：Delay = 0 可能是无效的
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
            ]},
            {elib_pg, [
                {'execute', 2, fun(_Sql, [_Type, _MsgId, _ErrorMsg, Delay]) ->
                    ?assert(Delay >= 0, "Delay should be non-negative"),
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            Result = msg_store_repo:mark_failed(
                <<"c2c">>, <<"msg123">>, <<"error"/utf8>>, 0
            ),
            ?assertEqual({ok, 1}, Result)
        end
    ).

%% ===================================================================
%% get_unstaged/1 测试
%% ===================================================================

get_unstaged_validates_limit_parameter_test_() ->
    ?WITH_MECKS(
        [
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
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"conv_seq">>)),

                    % 验证 LIMIT 参数
                    ?assert(is_integer(Limit)),
                    ?assertEqual(100, Limit),
                    ?assert(Limit > 0),

                    {ok, [
                        #{<<"msg_id">> => <<"msg1">>, <<"from_id">> => 100},
                        #{<<"msg_id">> => <<"msg2">>, <<"from_id">> => 200}
                    ]}
                end}
            ]}
        ],
        fun() ->
            Result = msg_store_repo:get_unstaged(100),
            ?assertMatch({ok, [_, _]}, Result),
            case Result of
                {ok, Rows} ->
                    ?assertEqual(2, length(Rows));
                _ ->
                    ?assert(false)
            end
        end
    ).

get_unstaged_with_negative_limit_accepted_test_() ->
    % 边界测试：负数 Limit 被传递到数据库（源码不验证）
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
            ]},
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) -> {ok, []} end}
            ]}
        ],
        fun() ->
            Result = msg_store_repo:get_unstaged(-1),
            % 源码不验证负数 Limit，直接传到数据库
            ?assertMatch({ok, _}, Result)
        end
    ).

%% ===================================================================
%% delete_processed/1 测试 - **发现源码 Bug！**
%% ===================================================================

delete_processed_validates_sql_correctness_test_() ->
    ?WITH_MECKS(
        [
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
                    ?assertNotEqual(
                        nomatch,
                        binary:match(Sql, <<"processed_at IS NOT NULL">>),
                        "Should delete processed records, not unprocessed"
                    ),

                    % 验证时间条件
                    ?assertNotEqual(
                        nomatch, binary:match(Sql, <<"processed_at < NOW() - INTERVAL">>)
                    ),

                    % 验证参数
                    ?assert(is_integer(Seconds)),
                    ?assert(Seconds > 0),
                    ?assertEqual(3600, Seconds),

                    {ok, 100}
                end}
            ]}
        ],
        fun() ->
            Result = msg_store_repo:delete_processed(3600),
            ?assertEqual({ok, 100}, Result)
        end
    ).

delete_expired_c2g_ledgers_filters_live_rows_and_deletes_snapshot_first_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'with_tx', 1, fun(Tx) ->
                try Tx(fake_conn) of
                    Result -> Result
                catch
                    throw:{rollback, Reason} -> {rollback, Reason}
                end
            end},
            {'query', 3, fun(fake_conn, Sql, [AgeSeconds, Limit]) ->
                ?assertEqual(370 * 86400, AgeSeconds),
                ?assertEqual(1000, Limit),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"msg_c2g_request_ledger">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"NOT EXISTS">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"public.msg_c2g msg">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"public.msg_store_staging">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"FOR UPDATE OF ledger SKIP LOCKED">>)),
                {ok, [#{<<"msg_id">> => <<"old-1">>}, #{<<"msg_id">> => <<"old-2">>}]}
            end},
            {'execute', 3, fun(fake_conn, Sql, [[<<"old-1">>, <<"old-2">>]]) ->
                case binary:match(Sql, <<"msg_c2g_recipient_snapshot">>) of
                    nomatch ->
                        ?assertEqual(snapshot_deleted, get(c2g_cleanup_order)),
                        {ok, 2};
                    _ ->
                        put(c2g_cleanup_order, snapshot_deleted),
                        {ok, 2}
                end
            end}
        ],
        fun() ->
            erase(c2g_cleanup_order),
            ?assertEqual({ok, 2}, msg_store_repo:delete_expired_c2g_ledgers(370 * 86400, 1000)),
            ?assertEqual(2, meck:num_calls(elib_pg, execute, 3)),
            erase(c2g_cleanup_order)
        end
    ).

delete_expired_c2g_ledgers_empty_batch_skips_deletes_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'with_tx', 1, fun(Tx) -> Tx(fake_conn) end},
            {'query', 3, fun(_, _, _) -> {ok, []} end},
            {'execute', 3, fun(_, _, _) -> erlang:error(delete_must_not_run) end}
        ],
        fun() ->
            ?assertEqual({ok, 0}, msg_store_repo:delete_expired_c2g_ledgers(1, 1)),
            ?assertEqual(0, meck:num_calls(elib_pg, execute, 3))
        end
    ).

delete_expired_c2g_ledgers_rolls_back_when_snapshot_delete_fails_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'with_tx', 1, fun(Tx) ->
                try Tx(fake_conn) of
                    Result -> Result
                catch
                    throw:{rollback, Reason} -> {rollback, Reason}
                end
            end},
            {'query', 3, fun(_, _, _) -> {ok, [#{<<"msg_id">> => <<"old-1">>}]} end},
            {'execute', 3, fun(_, Sql, _) ->
                ?assertNotEqual(nomatch, binary:match(Sql, <<"msg_c2g_recipient_snapshot">>)),
                {error, connection_lost}
            end}
        ],
        fun() ->
            ?assertEqual(
                {error, {recipient_snapshot_cleanup_failed, connection_lost}},
                msg_store_repo:delete_expired_c2g_ledgers(1, 1)
            ),
            ?assertEqual(1, meck:num_calls(elib_pg, execute, 3))
        end
    ).

%% ===================================================================
%% get_staging_stats/0 测试
%% ===================================================================

get_staging_stats_validates_aggregation_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
            ]},
            {elib_pg, [
                {'query', 2, fun(Sql, []) ->
                    % 验证 SQL 包含聚合函数
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"COUNT(*) FILTER">>)),
                    % pending
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"processed_at IS NULL">>)),
                    % processed
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"processed_at IS NOT NULL">>)),
                    % failed
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"error_msg IS NOT NULL">>)),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"COUNT(*) as total">>)),

                    {ok, [
                        [
                            #{
                                <<"pending">> => 10,
                                <<"processed">> => 100,
                                <<"failed">> => 2,
                                <<"total">> => 112
                            }
                        ]
                    ]}
                end}
            ]}
        ],
        fun() ->
            Result = msg_store_repo:get_staging_stats(),
            %% epgsql returns list-of-lists: {ok, [[#{...}]]}
            ?assertMatch(
                {ok, [
                    [
                        #{
                            <<"pending">> := 10,
                            <<"processed">> := 100,
                            <<"failed">> := 2,
                            <<"total">> := 112
                        }
                    ]
                ]},
                Result
            )
        end
    ).

%% ===================================================================
%% truncate_processed/0 测试
%% ===================================================================

truncate_processed_validates_sql_test_() ->
    ?WITH_MECKS(
        [
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
        ],
        fun() ->
            Result = msg_store_repo:truncate_processed(),
            ?assertEqual({ok, [], []}, Result)
        end
    ).

%% ===================================================================
%% vacuum_table/0 测试
%% ===================================================================

vacuum_table_validates_sql_test_() ->
    ?WITH_MECKS(
        [
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
        ],
        fun() ->
            Result = msg_store_repo:vacuum_table(),
            ?assertEqual({ok, [], []}, Result)
        end
    ).

%% ===================================================================
%% ensure_table_exists/0 测试
%% ===================================================================

ensure_table_exists_validates_ddl_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg_sql, [
                {'public_tablename', 1, fun(_) -> <<"msg_store_staging">> end}
            ]},
            {elib_pg, [
                {'execute', 2, fun(Sql, []) ->
                    case binary:match(Sql, <<"CREATE TABLE IF NOT EXISTS ">>) of
                        nomatch ->
                            % Index creation calls -- just return ok
                            {ok, []};
                        _ ->
                            % 验证 CREATE TABLE 语句
                            ?assertNotEqual(
                                nomatch, binary:match(Sql, <<"id BIGINT PRIMARY KEY">>)
                            ),
                            ?assertNotEqual(
                                nomatch, binary:match(Sql, <<"type VARCHAR(10) NOT NULL">>)
                            ),
                            ?assertNotEqual(
                                nomatch, binary:match(Sql, <<"msg_id VARCHAR(50) NOT NULL">>)
                            ),
                            ?assertNotEqual(
                                nomatch, binary:match(Sql, <<"payload JSONB NOT NULL">>)
                            ),
                            ?assertNotEqual(
                                nomatch, binary:match(Sql, <<"from_id BIGINT NOT NULL">>)
                            ),
                            ?assertNotEqual(
                                nomatch,
                                binary:match(Sql, <<"retry_count INTEGER NOT NULL DEFAULT 0">>)
                            ),
                            ?assertNotEqual(
                                nomatch, binary:match(Sql, <<"processed_at TIMESTAMPTZ">>)
                            ),
                            ?assertNotEqual(
                                nomatch, binary:match(Sql, <<"UNIQUE (type, msg_id)">>)
                            ),
                            {ok, []}
                    end
                end}
            ]}
        ],
        fun() ->
            Result = msg_store_repo:ensure_table_exists(),
            ?assertEqual(ok, Result)
        end
    ).

%% ===================================================================
%% create_indexes/1 测试
%% ===================================================================

create_indexes_validates_index_ddl_test_() ->
    % Source calls elib_pg:execute 3 times (one per index), so we collect
    % all SQL statements and verify each index name appears at least once.
    ?WITH_MECK(
        elib_pg,
        [
            {'execute', 2, fun(Sql, []) ->
                % Accumulate SQL statements in process dictionary
                Prev = get(create_idx_sqls),
                put(create_idx_sqls, [Sql | Prev]),
                {ok, [], []}
            end}
        ],
        fun() ->
            put(create_idx_sqls, []),
            Result = msg_store_repo:create_indexes(<<"msg_store_staging">>),
            AllSql = get(create_idx_sqls),
            Combined = iolist_to_binary(lists:reverse(AllSql)),
            % 验证索引创建语句
            ?assertNotEqual(nomatch, binary:match(Combined, <<"CREATE INDEX IF NOT EXISTS ">>)),
            ?assertNotEqual(nomatch, binary:match(Combined, <<"_processed_at_idx">>)),
            ?assertNotEqual(nomatch, binary:match(Combined, <<"_available_at_idx">>)),
            ?assertNotEqual(nomatch, binary:match(Combined, <<"_created_at_idx">>)),
            ?assertEqual(ok, Result),
            erase(create_idx_sqls),
            ok
        end
    ).

%% ===================================================================
%% msg_store_payload_to_jsonb/1 测试（E2EE 密文 staging 崩溃回归）
%% ===================================================================

%% 真机实测缺陷：E2EE 裸密文以数字开头（如 "14bVk..."）被首字符启发式
%% 误判为 JSON 数字，原样写入 JSONB 列触发 PG 22P02，
%% stage_and_send_c2c 返回 error 导致 WS 进程 case_clause 崩溃。
payload_to_jsonb_wraps_digit_leading_ciphertext_test() ->
    Ct = <<"14bVksw4V9G7cRCkGh3zAbCdEf">>,
    Encoded = msg_store_repo:msg_store_payload_to_jsonb(Ct),
    %% 包装后必须是合法 JSON，且解回原密文
    ?assertEqual(Ct, jsone:decode(Encoded)).

payload_to_jsonb_wraps_plain_base64_test() ->
    Ct = <<"aGVsbG8gd29ybGQ=">>,
    Encoded = msg_store_repo:msg_store_payload_to_jsonb(Ct),
    ?assertEqual(Ct, jsone:decode(Encoded)).

payload_to_jsonb_passthrough_valid_json_object_test() ->
    Obj = <<"{\"text\":\"hi\"}">>,
    ?assertEqual(Obj, msg_store_repo:msg_store_payload_to_jsonb(Obj)).

payload_to_jsonb_encodes_map_test() ->
    Encoded = msg_store_repo:msg_store_payload_to_jsonb(#{<<"text">> => <<"hi">>}),
    ?assertEqual(#{<<"text">> => <<"hi">>}, jsone:decode(Encoded, [{object_format, map}])).

payload_to_jsonb_null_test() ->
    ?assertEqual(<<"null">>, msg_store_repo:msg_store_payload_to_jsonb(null)).
