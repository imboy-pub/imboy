-module(group_e2ee_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("group_role.hrl").

%%%===================================================================
%%% @doc 群级 E2EE 开关与 fail-closed 门测试（P0-B B4）
%%%
%%% 覆盖：仅群主可开 / 0→1 单向 / e2ee_mode=1 拒明文 /
%%% 群查询失败拒发（fail-closed）/ 非内容动作放行不查库。
%%%===================================================================

encrypted_e2ee() ->
    #{<<"iv">> => <<"aXY=">>, <<"ct">> => <<"Y3Q=">>}.

%% ===================================================================
%% set_e2ee_mode：权限与单向性
%% ===================================================================

set_e2ee_mode_rejects_disable_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% 0→1 单向：任何非 1 的目标模式（含关闭）一律拒绝
        ?assertMatch({error, _}, group_logic:set_e2ee_mode(1001, 42, 0)),
        ?assertMatch({error, _}, group_logic:set_e2ee_mode(1001, 42, 2))
    end).

set_e2ee_mode_owner_only_test_() ->
    ?WITH_MECKS(
        [
            {group_member_ds, [
                {'find_by_gid_and_uid', 3, fun(42, 1001, <<"role">>) ->
                    %% 副群主也不行（edit 通道允许副群主，本开关更严格）
                    #{<<"role">> => ?ROLE_VICE_OWNER}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch({error, _}, group_logic:set_e2ee_mode(1001, 42, 1))
        end
    ).

set_e2ee_mode_owner_enables_and_broadcasts_test_() ->
    ?WITH_MECKS(
        [
            {group_member_ds, [
                {'find_by_gid_and_uid', 3, fun(42, 1001, <<"role">>) ->
                    #{<<"role">> => ?ROLE_OWNER}
                end}
            ]},
            {group_ds, [
                {'update_by_id', 2, fun(42, Data) ->
                    ?assertEqual(1, maps:get(e2ee_mode, Data)),
                    {ok, 1}
                end},
                {'flush_e2ee_mode', 1, fun(42) -> ok end},
                {'member_uids', 1, fun(42) -> [1001, 1002] end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(1001, [1001, 1002], <<"group_e2ee_mode">>, _, _, Payload, save) ->
                    ?assertEqual(1, maps:get(<<"e2ee_mode">>, Payload)),
                    ok
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, group_logic:set_e2ee_mode(1001, 42, 1)),
            %% 开关落库后必须清缓存 + 广播
            ?assertEqual(1, meck:num_calls(group_ds, flush_e2ee_mode, 1)),
            ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7))
        end
    ).

%% ===================================================================
%% group_e2ee_gate：fail-closed 门
%% ===================================================================

gate_rejects_plaintext_when_required_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'e2ee_mode', 1, fun(42) -> {ok, 1} end}
            ]},
            {elib_metric, [
                {'increment', 1, fun(_) -> ok end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"encrypted_message_required">>},
                msg_c2g_logic:group_e2ee_gate(42, <<"text">>, <<>>, null, <<"{\"plain\":1}">>)
            ),
            %% message_edit 同为内容动作，同样拦截
            ?assertEqual(
                {error, <<"encrypted_message_required">>},
                msg_c2g_logic:group_e2ee_gate(
                    42, <<"text">>, <<"message_edit">>, null, <<"{\"plain\":1}">>
                )
            ),
            %% C12-followup（AC-26）：每次拒收必须递增计数器——
            %% rollout 期旧客户端撞墙的关键可观测信号，拒收不能静默
            ?assertEqual(
                2,
                meck:num_calls(elib_metric, increment, [group_e2ee_plaintext_rejected_total])
            )
        end
    ).

gate_allows_encrypted_when_required_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'e2ee_mode', 1, fun(42) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                ok,
                msg_c2g_logic:group_e2ee_gate(
                    42, <<"text">>, <<>>, encrypted_e2ee(), <<"{\"e2ee\":true}">>
                )
            )
        end
    ).

gate_allows_plaintext_when_off_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'e2ee_mode', 1, fun(42) -> {ok, 0} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                ok, msg_c2g_logic:group_e2ee_gate(42, <<"text">>, <<>>, null, <<"{\"plain\":1}">>)
            )
        end
    ).

%% fail-closed：群配置查询失败一律拒发，不降级放行
gate_fails_closed_on_query_error_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'e2ee_mode', 1, fun(42) ->
                    {error, #{gid => 42, secret => <<"synthetic-group-key-canary">>}}
                end}
            ]},
            {elib_metric, [
                {'increment', 1, fun(_) -> ok end}
            ]},
            {elib_log, [
                {internal_log, 4, fun(_, _, _, _) -> ok end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"group_e2ee_check_failed">>},
                msg_c2g_logic:group_e2ee_gate(42, <<"text">>, <<>>, null, <<"{\"plain\":1}">>)
            ),
            %% C12-followup（AC-26）：fail-closed 拒发同样要计数——
            %% 否则 DB 故障被误读为"群开着 E2EE 在正常拦明文"
            ?assertEqual(
                1,
                meck:num_calls(elib_metric, increment, [group_e2ee_check_failed_total])
            ),
            ?assertEqual(
                1,
                meck:num_calls(
                    elib_log,
                    internal_log,
                    [error, group_e2ee_gate_query_failed, '_', '_']
                )
            ),
            ?assertEqual(
                1, meck:num_calls(elib_log, internal_log, 4)
            )
        end
    ).

%% 非内容动作（撤回/已读等）放行且零查库（热路径保护）
gate_skips_non_content_actions_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'e2ee_mode', 1, fun(_) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                ok,
                msg_c2g_logic:group_e2ee_gate(42, <<>>, <<"msg_revoke">>, null, <<"{}">>)
            ),
            ?assertEqual(0, meck:num_calls(group_ds, e2ee_mode, 1))
        end
    ).

%% ===================================================================
%% C08 / AC-18：D3 历史授权区间的「grant 扩大」与「跨 generation 重放」
%% 服务端负例。
%%
%% 覆盖：
%% - grant 区间来源唯一锚定服务端 attestation 表（caller 无法注入/扩大）；
%% - leave 即失效（世代关闭 → denied）；
%% - 重入后旧 session 授权 denied（sm.generation_no ≠ 当前 open 世代）；
%% - 重入后 extend 旧 session → e2ee_session_generation_mismatch；
%% - 重放旧 session 的 room key 注册（换 msg_id）→ e2ee_session_conflict；
%% - authorize SQL 防线裁剪守护（防未来重构弱化 grant 扩大防线）与
%%   畸形返回 fail-closed（多行 / 非正 generation / end < start → denied）。
%% ===================================================================

uid() ->
    erlang:unique_integer([positive]) +
        erlang:phash2(binary:encode_hex(crypto:strong_rand_bytes(8))).

c08_ts() ->
    <<"2026-10-03 08:00:00+08">>.

c08_mk_group(Gid, OwnerUid) ->
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO public.\"group\" (id, owner_uid, creator_uid, member_max, "
            "member_count, introduction, avatar, title, chat_aes_key, status, "
            "created_at, e2ee_mode, scope) VALUES ($1,$2,$3,500,0,'','','t','k',1,now(),0,'personal')"
        >>,
        [Gid, OwnerUid, OwnerUid]
    ),
    ok.

c08_stage_room_key(Gid, FromUid, SenderDid, SessionId, MsgId) ->
    Payload = jsone:encode(#{
        <<"to">> => integer_to_binary(Gid),
        <<"payload">> => #{
            <<"msg_type">> => <<"e2ee_room_key">>,
            <<"gid">> => Gid,
            <<"session_id">> => SessionId,
            <<"keys">> => []
        }
    }),
    msg_store_ds:stage(
        <<"c2g">>,
        MsgId,
        <<"e2ee_room_key">>,
        <<"e2ee_room_key">>,
        null,
        Payload,
        FromUid,
        Gid,
        c08_ts(),
        c08_ts(),
        SenderDid,
        1
    ).

c08_stage_megolm(Gid, FromUid, SenderDid, SessionId, MsgId) ->
    E2EE = #{
        <<"meta_version">> => 3,
        <<"protocol_metadata">> => #{
            <<"protocol">> => <<"megolm">>,
            <<"gid">> => Gid,
            <<"session_id">> => SessionId
        }
    },
    Payload = jsone:encode(#{
        <<"to">> => integer_to_binary(Gid),
        <<"payload">> => <<"ciphertext">>
    }),
    msg_store_ds:stage(
        <<"c2g">>,
        MsgId,
        <<"text">>,
        <<>>,
        E2EE,
        Payload,
        FromUid,
        Gid,
        c08_ts(),
        c08_ts(),
        SenderDid,
        1
    ).

c08_open_generation_start(Gid, Uid) ->
    {ok, [#{<<"start_seq">> := StartSeq}]} = elib_pg:query(
        <<"SELECT start_seq FROM public.group_member_generation ",
            "WHERE group_id = $1 AND user_id = $2 AND end_seq IS NULL">>,
        [Gid, Uid]
    ),
    StartSeq.

c08_grant_expansion_and_generation_replay_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Gid = 551000 + (uid() rem 100000),
        A = 7561,
        B = 7562,
        SessionOld = iolist_to_binary(["c08-old-", integer_to_binary(uid())]),
        SessionNew = iolist_to_binary(["c08-new-", integer_to_binary(uid())]),
        SenderDid = <<"did-c08">>,
        c08_mk_group(Gid, A),
        ok = group_member_logic:join_group(<<"invite">>, A, Gid, #{}),
        ok = group_member_logic:join_group(<<"invite">>, B, Gid, #{}),

        %% A 建旧 session（gen1 世代：A、B 均在）
        {ok, new, _, _} = c08_stage_room_key(
            Gid,
            A,
            SenderDid,
            SessionOld,
            iolist_to_binary(["rk-old-", integer_to_binary(uid())])
        ),
        {ok, #{generation_no := 1}} = group_ds:authorize_group_history(B, Gid, SessionOld),

        %% ---- leave 即失效：世代关闭（end_seq 非空）→ denied ----
        ok = group_member_logic:leave(B, Gid, B),
        ?assertEqual(
            {error, denied},
            group_ds:authorize_group_history(B, Gid, SessionOld)
        ),

        %% ---- 跨 generation 重放（授权）：重入 gen2 后旧 session 授权 denied ----
        ok = group_member_logic:join_group(<<"invite">>, B, Gid, #{}),
        BGen2Start = c08_open_generation_start(Gid, B),
        ?assertEqual(
            {error, denied},
            group_ds:authorize_group_history(B, Gid, SessionOld)
        ),

        %% ---- 跨 generation 重放（extend）：A 在 B 重入后 extend 旧 session → 拒 ----
        ?assertEqual(
            {error, e2ee_session_generation_mismatch},
            c08_stage_megolm(
                Gid,
                A,
                SenderDid,
                SessionOld,
                iolist_to_binary(["m-old-after-rejoin-", integer_to_binary(uid())])
            )
        ),

        %% ---- 跨 generation 重放（注册）：换 msg_id 重放旧 room key → 冲突拒 ----
        ?assertEqual(
            {error, e2ee_session_conflict},
            c08_stage_room_key(
                Gid,
                A,
                SenderDid,
                SessionOld,
                iolist_to_binary(["rk-replay-", integer_to_binary(uid())])
            )
        ),

        %% ---- grant 区间锚定：重入后的新 session 授权区间不含旧区间 ----
        {ok, new, NewRoomKeySeq, _} = c08_stage_room_key(
            Gid,
            A,
            SenderDid,
            SessionNew,
            iolist_to_binary(["rk-new-", integer_to_binary(uid())])
        ),
        {ok, #{generation_no := GenNo, start_seq := StartSeq, end_seq := EndSeq}} =
            group_ds:authorize_group_history(B, Gid, SessionNew),
        ?assertEqual(2, GenNo),
        ?assert(StartSeq >= BGen2Start),
        ?assertEqual(NewRoomKeySeq, StartSeq),
        ?assert(EndSeq >= StartSeq),
        %% 未注册 session 恒 denied（无行即拒，不回退布尔成员判断）
        ?assertEqual(
            {error, denied},
            group_ds:authorize_group_history(B, Gid, <<"c08-never-registered">>)
        )
    end).

%% authorize SQL 防线裁剪守护（meck 级，与 msg_store_repo_tests 的 authorize
%% 正例对称；防未来重构弱化 grant 扩大/跨世代防线）。
c08_authorize_sql_guards_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(Sql, [42, <<"session-a">>, 123]) ->
                    Guards = [
                        <<"e2ee_group_session_attestation sa">>,
                        <<"e2ee_group_session_member sm">>,
                        %% grant 扩大防线：session start 不得早于成员注册世代起点
                        <<"sa.start_seq >= sm.generation_start_seq">>,
                        %% 跨 generation 重放防线：注册世代 = 当前 open 世代
                        <<"gmg.generation_no = sm.generation_no">>,
                        <<"gmg.end_seq IS NULL">>,
                        <<"gm.status = 1">>,
                        <<"grp.status = 1">>,
                        <<"sa.end_seq >= sa.start_seq">>
                    ],
                    [
                        ?assertNotEqual(nomatch, binary:match(Sql, G))
                     || G <- Guards
                    ],
                    {ok, [
                        #{
                            <<"generation_no">> => 2,
                            <<"start_seq">> => 500,
                            <<"end_seq">> => 550
                        }
                    ]}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, #{generation_no => 2, start_seq => 500, end_seq => 550}},
                msg_store_repo:authorize_group_session_history(123, 42, <<"session-a">>)
            )
        end
    ).

%% authorize 畸形返回 fail-closed：多行 / 非正 generation / 区间倒置 / 查询错误。
c08_authorize_malformed_rows_denied_test_() ->
    Malformed = [
        {ok, [
            #{<<"generation_no">> => 2, <<"start_seq">> => 500, <<"end_seq">> => 550},
            #{<<"generation_no">> => 3, <<"start_seq">> => 500, <<"end_seq">> => 600}
        ]},
        {ok, [#{<<"generation_no">> => 0, <<"start_seq">> => 500, <<"end_seq">> => 550}]},
        {ok, [#{<<"generation_no">> => 2, <<"start_seq">> => 550, <<"end_seq">> => 500}]},
        {error, pg_down}
    ],
    [
        begin
            Rows = RowsIn,
            ?WITH_MECKS(
                [{elib_pg, [{'query', 2, fun(_Sql, _Params) -> Rows end}]}],
                fun() ->
                    ?assertEqual(
                        {error, denied},
                        msg_store_repo:authorize_group_session_history(123, 42, <<"session-a">>)
                    )
                end
            )
        end
     || RowsIn <- Malformed
    ].
