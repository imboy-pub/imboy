-module(messaging_logic_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%% ARCH-01：offline/2(Req0,State) 已拆分为 handler 层解析 + 本函数纯参数
%% messaging_logic:offline/6，直接调用纯函数，不再需要 cowboy_req/响应 mock。
offline_returns_expected_shape_test_() ->
    ?WITH_MECKS(
        [
            {elib_dt, [
                {'to_rfc3339', 2, fun(_Ts, _Unit) -> <<"1970-01-01T00:00:00Z">> end}
            ]},
            {msg_c2c_repo, [
                {'tablename', 0, fun() -> <<"public.msg_c2c">> end}
            ]},
            {msg_c2g_timeline_repo, [
                {'tablename', 0, fun() -> <<"public.msg_c2g_timeline">> end}
            ]},
            {msg_s2c_repo, [
                {'tablename', 0, fun() -> <<"public.msg_s2c">> end}
            ]},
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) -> {ok, [#{<<"count">> => 0}]} end}
            ]},
            {msg_c2c_ds, [
                {'read_msg_for_device', 4, fun(_Uid, _DID, _Limit, _LastMsgAt) -> [] end},
                {'count_unread_since', 3, fun(_Uid, _LastMsgAt, _DID) -> 0 end}
            ]},
            {msg_c2g_ds, [
                {'read_msg', 3, fun(_Uid, _Limit, _LastMsgAt) ->
                    [#{<<"msg_id">> => <<"m1">>, <<"conv_seq">> => 481}]
                end}
            ]},
            {msg_s2c_ds, [
                {'read_msg_for_device', 4, fun(_Uid, _DID, _Limit, _LastMsgAt) -> [] end},
                {'count_since', 3, fun(_Uid, _LastMsgAt, _DID) -> 0 end}
            ]}
        ],
        fun() ->
            Payload = messaging_logic:offline(12345, 1000, 0, 0, 0, <<>>),
            lists:foreach(
                fun(Type) ->
                    TypeMap = maps:get(Type, Payload),
                    ?assertEqual(false, maps:get(<<"has_more">>, TypeMap)),
                    ?assertEqual(0, maps:get(<<"total">>, TypeMap)),
                    ?assertEqual([], maps:get(<<"list">>, TypeMap))
                end,
                [<<"c2c">>, <<"s2c">>]
            ),
            C2gMap = maps:get(<<"c2g">>, Payload),
            [C2gMsg] = maps:get(<<"list">>, C2gMap),
            ?assertEqual(481, maps:get(<<"conv_seq">>, C2gMsg))
        end
    ).

%% D4 契约修复：next_last_msg_at 必须回毫秒整数（客户端把它原样回传为
%% *_last_msg_at 请求参数）。历史实现直接回传行内 created_at 的 RFC3339
%% 字符串，客户端解析恒失败、游标恒 0，每轮退化为全量拉取。
offline_next_cursor_millisecond_contract_test_() ->
    ?WITH_MECKS(
        [
            {elib_dt, [
                {'to_rfc3339', 2, fun(_Ts, _Unit) -> <<"1970-01-01T00:00:00Z">> end},
                {'rfc3339_to', 2, fun
                    (<<"2026-06-01T12:00:00.500+08:00">>, millisecond) ->
                        1780296000500;
                    (_, _) ->
                        {error, empty_input}
                end}
            ]},
            {msg_c2c_repo, [
                {'tablename', 0, fun() -> <<"public.msg_c2c">> end}
            ]},
            {msg_c2g_timeline_repo, [
                {'tablename', 0, fun() -> <<"public.msg_c2g_timeline">> end}
            ]},
            {msg_s2c_repo, [
                {'tablename', 0, fun() -> <<"public.msg_s2c">> end}
            ]},
            {elib_pg, [
                {'query', 2, fun(_Sql, _Params) -> {ok, [#{<<"count">> => 0}]} end}
            ]},
            {msg_c2c_ds, [
                {'read_msg_for_device', 4, fun(_Uid, _DID, _Limit, _LastMsgAt) ->
                    [
                        #{
                            <<"msg_id">> => <<"c2c-1">>,
                            <<"created_at">> => <<"2026-06-01T11:00:00.000+08:00">>
                        },
                        #{
                            <<"msg_id">> => <<"c2c-2">>,
                            %% 末行时间即下一轮游标
                            <<"created_at">> => <<"2026-06-01T12:00:00.500+08:00">>
                        }
                    ]
                end},
                {'count_unread_since', 3, fun(_Uid, _LastMsgAt, _DID) -> 2 end}
            ]},
            {msg_c2g_ds, [
                {'read_msg', 3, fun(_Uid, _Limit, _LastMsgAt) -> [] end}
            ]},
            {msg_s2c_ds, [
                {'read_msg_for_device', 4, fun(_Uid, _DID, _Limit, _LastMsgAt) -> [] end},
                {'count_since', 3, fun(_Uid, _LastMsgAt, _DID) -> 0 end}
            ]}
        ],
        fun() ->
            Payload = messaging_logic:offline(12345, 1000, 1000, 2000, 3000, <<>>),
            C2c = maps:get(<<"c2c">>, Payload),
            ?assert(is_integer(maps:get(<<"next_last_msg_at">>, C2c))),
            ?assertEqual(1780296000500, maps:get(<<"next_last_msg_at">>, C2c)),
            %% 空列表：游标不回退，回传入参毫秒值
            S2c = maps:get(<<"s2c">>, Payload),
            ?assertEqual(3000, maps:get(<<"next_last_msg_at">>, S2c)),
            C2g = maps:get(<<"c2g">>, Payload),
            ?assertEqual(2000, maps:get(<<"next_last_msg_at">>, C2g))
        end
    ).

encode_history_msg_decodes_jsonb_columns_test() ->
    E2EEJson = <<"{\"protocol\":\"olm\",\"version\":3}">>,
    Row = #{
        <<"msg_id">> => <<"m-history-e2ee">>,
        <<"from_id">> => 1001,
        <<"to_id">> => 1002,
        <<"group_id">> => null,
        <<"e2ee">> => E2EEJson,
        <<"payload">> => <<"\"\"">>
    },
    Msg = messaging_logic:encode_history_msg(1002, Row),
    ?assertEqual(#{<<"protocol">> => <<"olm">>, <<"version">> => 3}, maps:get(<<"e2ee">>, Msg)),
    ?assertEqual(<<>>, maps:get(<<"payload">>, Msg)),
    %% S0 契约（e23a9df0）：信封 ID 统一归一化为 binary 字符串。
    ?assertEqual(<<"1001">>, maps:get(<<"from">>, Msg)),
    ?assertEqual(<<"1002">>, maps:get(<<"to">>, Msg)).

route_ws_delegates_c2c_to_existing_logic_modules_test_() ->
    ?WITH_MECK(
        msg_c2c_logic,
        [
            {'c2c', 3, fun(MsgId, CurrentUid, Data) ->
                self() ! {c2c_routed, MsgId, CurrentUid, Data},
                ok
            end}
        ],
        fun() ->
            MsgId = <<"msg_123">>,
            CurrentUid = 1001,
            Data = #{<<"payload">> => #{<<"text">> => <<"hello">>}},
            Type = <<"C2C">>,
            OriginalMsg = <<"{\"payload\":{\"text\":\"hello\"}}">>,

            ?assertEqual(ok, messaging_logic:route_ws(MsgId, CurrentUid, Data, Type, OriginalMsg)),
            ?assertEqual(
                {c2c_routed, MsgId, CurrentUid, Data},
                receive
                    Routed -> Routed
                after 1000 ->
                    timeout
                end
            )
        end
    ).

%% ARCH-01：offline_ack/2(Req0,State) 已拆分为 handler 层解析（含 type 小写化）
%% + 本函数纯参数 messaging_logic:offline_ack/4，Type 传入时已是小写。
offline_ack_uses_messaging_boundary_test_() ->
    ?WITH_MECKS(
        [
            {elib_log, [
                {'internal_log', 5, fun(_Level, _Fmt, _Args, _Module, _Line) -> ok end}
            ]},
            {msg_c2c_repo, [
                {'delete_by_msg_ids_and_to_id', 2, fun(MsgIds, 12345) ->
                    self() ! {acked_ids, MsgIds},
                    2
                end}
            ]}
        ],
        fun() ->
            {ok, Payload} = messaging_logic:offline_ack(
                12345, <<"c2c">>, [<<"m1">>, <<"m2">>], <<>>
            ),
            ?assertEqual(
                {acked_ids, [<<"m1">>, <<"m2">>]},
                receive
                    Acked -> Acked
                after 1000 ->
                    timeout
                end
            ),
            ?assertEqual(<<"c2c">>, maps:get(<<"type">>, Payload)),
            ?assertEqual(2, maps:get(<<"processed_count">>, Payload)),
            ?assertEqual(2, maps:get(<<"msg_ids_count">>, Payload))
        end
    ).

%% ARCH-01：reaction_add/2(Req0,State) 已拆分为 handler 层解析（elib_req:body）
%% + 本函数纯参数 messaging_logic:reaction_add/4。
reaction_add_uses_messaging_boundary_test_() ->
    ?WITH_MECK(
        msg_reaction_logic,
        [
            {'add', 4, fun(<<"msg-1">>, <<"c2c">>, 12345, Emoji) ->
                self() ! {reaction_added, Emoji},
                {ok, #{<<"user_id">> => 12345, <<"created_at">> => 1700000000}}
            end}
        ],
        fun() ->
            Emoji = <<240, 159, 145, 141>>,
            {ok, Payload, _Msg} = messaging_logic:reaction_add(
                12345, <<"msg-1">>, <<"c2c">>, Emoji
            ),
            ?assertEqual(
                {reaction_added, Emoji},
                receive
                    Added -> Added
                after 1000 ->
                    timeout
                end
            ),
            ?assertEqual(<<"msg-1">>, maps:get(<<"msg_id">>, Payload)),
            ?assertEqual(12345, maps:get(<<"user_id">>, Payload))
        end
    ).

%%%===================================================================
%%% history/5 增量同步（conv_seq range fetch）契约测试
%%% 覆盖：游标推进 / 重连幂等 / gap / 空结果 / archive 未开启降级 / 参数校验
%%%===================================================================

history_invalid_peer_rejected_before_dependencies_test_() ->
    ?WITH_MECKS(
        [
            {msg_archive_ds, [
                {'conv_key_c2c', 2, fun(_, _) -> error(unexpected_archive_access) end},
                {'conv_key_c2g', 1, fun(_) -> error(unexpected_archive_access) end},
                %% R02/D05：logic 层改走一致快照 safe_history/6（history/3 已退役）
                {'safe_history', 6, fun(_, _, _, _, _, _) -> error(unexpected_archive_access) end}
            ]},
            {group_ds, [
                {'authorize_group_history', 2, fun(_, _) -> error(unexpected_group_access) end}
            ]}
        ],
        fun() ->
            Invalid = [
                <<"not-an-id">>, <<"1.5">>, <<"0">>, <<"-1">>, <<"9223372036854775808">>
            ],
            lists:foreach(
                fun({Type, Peer}) ->
                    ?assertMatch(
                        {error, _, ?ERR_BAD_REQUEST},
                        messaging_logic:history(1, Type, Peer, 0, 10)
                    )
                end,
                [{Type, Peer} || Type <- [<<"c2c">>, <<"c2g">>], Peer <- Invalid]
            ),
            ?assertEqual(0, meck:num_calls(msg_archive_ds, conv_key_c2c, 2)),
            ?assertEqual(0, meck:num_calls(msg_archive_ds, conv_key_c2g, 1)),
            ?assertEqual(0, meck:num_calls(msg_archive_ds, safe_history, 6)),
            ?assertEqual(0, meck:num_calls(group_ds, authorize_group_history, 2))
        end
    ).

history_decimal_peer_compatibility_test_() ->
    ?WITH_MECKS(
        [
            {msg_archive_ds, [
                {'conv_key_c2c', 2, fun(_, Peer) -> integer_to_binary(Peer) end},
                {'conv_key_c2g', 1, fun(Peer) -> integer_to_binary(Peer) end},
                %% R02/D05：safe_history/6 快照 {ok, Rows, SafeHead, TerminalGaps}；
                %% 本例只验证十进制 peer 解析与 conv_key 透传，行集/边界取空固定值
                {'safe_history', 6, fun(_, _, _, _, _, _) -> {ok, [], 0, []} end}
            ]},
            {group_ds, [
                {'authorize_group_history', 2, fun(_, _) -> {ok, #{start_seq => 1}} end}
            ]}
        ],
        fun() ->
            Peers = [
                {<<"0009">>, 9}, {<<"+9">>, 9}, {<<"9223372036854775807">>, 9223372036854775807}
            ],
            lists:foreach(
                fun({Type, {Peer, Expected}}) ->
                    {ok, Result} = messaging_logic:history(1, Type, Peer, 0, 10),
                    ?assertEqual(integer_to_binary(Expected), maps:get(<<"conv_key">>, Result))
                end,
                [{Type, Peer} || Type <- [<<"c2c">>, <<"c2g">>], Peer <- Peers]
            )
        end
    ).

%% 正常 range fetch：next_seq 取最后一行 conv_seq，满页 has_more=true。
%% 服务端按 Limit+1 取数判定：ds 返回 3 行（> limit=2）→ has_more=true，
%% 且第 3 行（探测行）不随 messages 下发、next_seq 取截断后末行。
%% R02/D05 边界推导：行 6,7,8 均已正式归档、无非终态 staging →
%% safe_head(B) = 正式表 max = 8；expected next_seq = min(旧行为 7, B=8) = 7，
%% 不越过 B（blocked=false）。
history_range_fetch_advances_cursor_test_() ->
    ?WITH_MECKS(
        [
            {msg_archive_ds, [
                {'conv_key_c2c', 2, fun(A, B) ->
                    <<"c2c:", (integer_to_binary(min(A, B)))/binary, ":",
                        (integer_to_binary(max(A, B)))/binary>>
                end},
                {'safe_history', 6, fun(_ConvKey, AfterSeq, Limit, _Type, _A, _B) ->
                    ?assertEqual(5, AfterSeq),
                    ?assertEqual(3, Limit),
                    {ok,
                        [
                            #{<<"conv_seq">> => 6, <<"from_id">> => 100, <<"to_id">> => 200},
                            #{<<"conv_seq">> => 7, <<"from_id">> => 200, <<"to_id">> => 100},
                            #{<<"conv_seq">> => 8, <<"from_id">> => 100, <<"to_id">> => 200}
                        ],
                        8, []}
                end}
            ]}
        ],
        fun() ->
            {ok, Res} = messaging_logic:history(100, <<"c2c">>, <<"200">>, 5, 2),
            ?assertEqual(7, maps:get(<<"next_seq">>, Res)),
            ?assertEqual(true, maps:get(<<"has_more">>, Res)),
            ?assertEqual(2, length(maps:get(<<"messages">>, Res))),
            % R02 验证要求：断言最终消息集合，不能只断言 next_seq 数字
            ?assertEqual([6, 7], [maps:get(<<"conv_seq">>, M) || M <- maps:get(<<"messages">>, Res)]),
            ?assertEqual(false, maps:get(<<"blocked">>, Res)),
            % from_id/to_id 重命名为 from/to（S0 契约：ID 归一化 binary）
            [First | _] = maps:get(<<"messages">>, Res),
            ?assertEqual(<<"100">>, maps:get(<<"from">>, First)),
            ?assertEqual(false, maps:is_key(<<"from_id">>, First))
        end
    ).

%% 末页恰好满额：ds 不足 Limit+1 条（恰返回 Limit 条）→ has_more=false。
%% 旧判定 >= Limit 在此场景虚报 true，客户端多拉一次空页。
%% R02/D05 边界推导：行 6,7 恰为全部正式行、无非终态 staging →
%% safe_head(B) = 7；expected next_seq = min(旧行为 7, B=7) = 7。
history_exact_full_page_has_more_false_test_() ->
    ?WITH_MECKS(
        [
            {msg_archive_ds, [
                {'conv_key_c2c', 2, fun(A, B) ->
                    <<"c2c:", (integer_to_binary(min(A, B)))/binary, ":",
                        (integer_to_binary(max(A, B)))/binary>>
                end},
                {'safe_history', 6, fun(_ConvKey, _AfterSeq, _Limit, _Type, _A, _B) ->
                    {ok,
                        [
                            #{<<"conv_seq">> => 6, <<"from_id">> => 100, <<"to_id">> => 200},
                            #{<<"conv_seq">> => 7, <<"from_id">> => 200, <<"to_id">> => 100}
                        ],
                        7, []}
                end}
            ]}
        ],
        fun() ->
            {ok, Res} = messaging_logic:history(100, <<"c2c">>, <<"200">>, 5, 2),
            ?assertEqual(false, maps:get(<<"has_more">>, Res)),
            ?assertEqual(2, length(maps:get(<<"messages">>, Res))),
            ?assertEqual(7, maps:get(<<"next_seq">>, Res)),
            ?assertEqual(false, maps:get(<<"blocked">>, Res))
        end
    ).

%% 重连幂等：同一 after_seq 重复拉取结果一致（服务端无隐藏游标状态）。
%% R02/D05 边界推导：行 3 为全部正式行 → safe_head(B)=3；
%% expected next_seq = min(旧行为 3, B=3) = 3，重复请求一致（无状态快照）。
history_reconnect_idempotent_test_() ->
    ?WITH_MECKS(
        [
            {msg_archive_ds, [
                {'conv_key_c2g', 1, fun(Gid) -> <<"c2g:", (integer_to_binary(Gid))/binary>> end},
                {'safe_history', 6, fun(_ConvKey, _AfterSeq, _Limit, _Type, _A, _B) ->
                    {ok, [#{<<"conv_seq">> => 3, <<"from_id">> => 1, <<"group_id">> => 9}], 3, []}
                end}
            ]},
            {group_ds, [
                {'is_member', 2, fun(1, 9) -> true end},
                %% E2EE-2026-012：c2g 授权改走共享谓词（start_seq=1 = 无钳制）
                {'authorize_group_history', 2, fun(_Uid, _Gid) ->
                    {ok, #{generation_no => 1, start_seq => 1}}
                end}
            ]}
        ],
        fun() ->
            {ok, R1} = messaging_logic:history(1, <<"c2g">>, <<"9">>, 2, 10),
            {ok, R2} = messaging_logic:history(1, <<"c2g">>, <<"9">>, 2, 10),
            ?assertEqual(R1, R2),
            ?assertEqual(3, maps:get(<<"next_seq">>, R1)),
            % 未满页 has_more=false
            ?assertEqual(false, maps:get(<<"has_more">>, R1))
        end
    ).

%% 非活跃成员必须在归档查询前被拒绝，防止退群/被移除用户继续拉取群历史。
history_c2g_rejects_non_member_before_archive_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'is_member', 2, fun(1, 9) -> false end},
                %% E2EE-2026-012：无 open 世代（非成员）fail-closed deny
                {'authorize_group_history', 2, fun(_Uid, _Gid) -> {error, denied} end}
            ]},
            {msg_archive_ds, [
                {'conv_key_c2g', 1, fun(_) -> error(conv_key_must_not_be_built) end},
                {'safe_history', 6, fun(_, _, _, _, _, _) -> error(history_must_not_be_queried) end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {error, _, ?ERR_ACCESS_DENIED},
                messaging_logic:history(1, <<"c2g">>, <<"9">>, 0, 10)
            )
        end
    ).

%% gap 场景：存储侧 conv_seq 不连续（如 6,9,10），next_seq 取末行；
%% gap 检测是客户端职责（按 conv_seq 连续性判断），服务端如实返回。
%% R02/D05 边界推导：行 6,9,10 均为正式行 → safe_head(B)=10；
%% expected next_seq = min(旧行为 10, B=10) = 10；未分配/已清理的跳号
%% （7,8）既不在正式表也不在 staging，不属 terminal_failure/authorization
%% 空洞，D05 要求 gaps 为空、客户端自行按 seq 连续性发现。
history_gap_rows_next_seq_is_last_test_() ->
    ?WITH_MECKS(
        [
            {msg_archive_ds, [
                {'conv_key_c2c', 2, fun(_, _) -> <<"c2c:1:2">> end},
                {'safe_history', 6, fun(_ConvKey, _AfterSeq, _Limit, _Type, _A, _B) ->
                    {ok,
                        [
                            #{<<"conv_seq">> => 6, <<"from_id">> => 1, <<"to_id">> => 2},
                            #{<<"conv_seq">> => 9, <<"from_id">> => 2, <<"to_id">> => 1},
                            #{<<"conv_seq">> => 10, <<"from_id">> => 1, <<"to_id">> => 2}
                        ],
                        10, []}
                end}
            ]}
        ],
        fun() ->
            {ok, Res} = messaging_logic:history(1, <<"c2c">>, <<"2">>, 5, 10),
            ?assertEqual(10, maps:get(<<"next_seq">>, Res)),
            Seqs = [maps:get(<<"conv_seq">>, M) || M <- maps:get(<<"messages">>, Res)],
            ?assertEqual([6, 9, 10], Seqs),
            ?assertEqual([], maps:get(<<"gaps">>, Res)),
            ?assertEqual(false, maps:get(<<"blocked">>, Res))
        end
    ).

%% 空结果：游标不回退（next_seq 保持 after_seq）。
%% R02/D05 只进不退：NextSeq = max(AfterSeq, min(next_raw, max(SafeHead, AfterSeq)))
%% ——即使快照 SafeHead=0（B 尚无已归档行），游标也不得回退到 42 以下。
history_empty_keeps_cursor_test_() ->
    ?WITH_MECKS(
        [
            {msg_archive_ds, [
                {'conv_key_c2c', 2, fun(_, _) -> <<"c2c:1:2">> end},
                {'safe_history', 6, fun(_ConvKey, _AfterSeq, _Limit, _Type, _A, _B) ->
                    {ok, [], 0, []}
                end}
            ]}
        ],
        fun() ->
            {ok, Res} = messaging_logic:history(1, <<"c2c">>, <<"2">>, 42, 10),
            ?assertEqual(42, maps:get(<<"next_seq">>, Res)),
            ?assertEqual(false, maps:get(<<"has_more">>, Res)),
            ?assertEqual([], maps:get(<<"messages">>, Res))
        end
    ).

%% archive 未开启：DS 报错时返回 500 级错误与可读提示
history_archive_disabled_returns_error_test_() ->
    ?WITH_MECKS(
        [
            {msg_archive_ds, [
                {'conv_key_c2c', 2, fun(_, _) -> <<"c2c:1:2">> end},
                {'safe_history', 6, fun(_, _, _, _, _, _) -> {error, disabled} end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {error, _, ?ERR_INTERNAL_SERVER_ERROR},
                messaging_logic:history(1, <<"c2c">>, <<"2">>, 0, 10)
            )
        end
    ).

%% 参数校验：非法 chat_type / 缺 peer_id 均 400
history_param_validation_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertMatch(
            {error, _, ?ERR_BAD_REQUEST},
            messaging_logic:history(1, <<"c2x">>, <<"2">>, 0, 10)
        ),
        ?assertMatch(
            {error, _, ?ERR_BAD_REQUEST},
            messaging_logic:history(1, <<"c2c">>, <<>>, 0, 10)
        )
    end).
