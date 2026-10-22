%%% @doc talk_record T15 内容域（原文/发言人/摘要）logic 级测试（真库 55433）。
%%%
%%% 覆盖矩阵（T15 卡交付 6）：
%%%   * owner/editor/viewer 逐接口矩阵；
%%%   * 跨 record 子 ID 404（segment/speaker）；
%%%   * 并发修改 412（segment 与 summary 各一，stale If-Match CAS）；
%%%   * 修改后摘要 stale；claim 冲突 409 + 审计；目录外 matched_user_id 拒绝；
%%%   * q 高亮位置；审计行存在（metadata 白名单、不含正文）。
%%% 夹具说明：transcript/speakers/segments/summary 由本套件 SQL 直插
%%% （模拟 T13 ASR 产物形状），不冒充真实转写质量。
-module(talk_record_content_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    State = talk_record_t15_support:setup_all(),
    talk_record_setup_fixture:prepare(
        State,
        fun() ->
            talk_record_t15_support:seed(State),
            State
        end,
        fun talk_record_t15_support:teardown_all/1
    ).

cleanup(State) ->
    talk_record_t15_support:teardown_all(State).

with_db(TestFun) ->
    {timeout, 120,
        {setup, fun setup/0, fun cleanup/1, fun(State) ->
            ?_test(TestFun(State))
        end}}.

%% @doc 标准已转写夹具：U1 owner + U2 viewer grant + U3 editor grant；
%% 2 speakers + 4 segments（含 q 关键词两次命中）+ draft summary 引用 S1。
%% 返回 #{record => Rid, sp1 => .., sp2 => .., segs => [..], summary_id => ..}。
-spec transcribed_fixture(map(), integer()) -> map().
transcribed_fixture(State, N) ->
    Conn = maps:get(conn, State),
    Ids = talk_record_t15_support:ids(),
    U1 = maps:get(u1, Ids),
    U2 = maps:get(u2, Ids),
    U3 = maps:get(u3, Ids),
    {ok, Rid} = talk_record_t15_support:create_record(U1, N, <<"t15-content"/utf8>>),
    talk_record_t15_support:add_grant(Conn, Rid, U2, U1, <<"viewer">>),
    talk_record_t15_support:add_grant(Conn, Rid, U3, U1, <<"editor">>),
    _Tid = talk_record_t15_support:seed_transcript(Conn, Rid, 1),
    Sp1 = talk_record_t15_support:seed_speaker(Conn, Rid, <<"SPEAKER_00">>, #{}),
    Sp2 = talk_record_t15_support:seed_speaker(
        Conn,
        Rid,
        <<"SPEAKER_01">>,
        #{display_name => <<"张三"/utf8>>}
    ),
    S1 = talk_record_t15_support:seed_segment(
        Conn,
        Rid,
        1,
        0,
        Sp1,
        0,
        <<"会议开始，先过项目进度"/utf8>>
    ),
    S2 = talk_record_t15_support:seed_segment(
        Conn,
        Rid,
        1,
        1,
        Sp1,
        5000,
        <<"本周会议重点有三个事项"/utf8>>
    ),
    S3 = talk_record_t15_support:seed_segment(
        Conn,
        Rid,
        1,
        2,
        Sp2,
        10000,
        <<"预算按上次会议结论执行"/utf8>>
    ),
    _S4 = talk_record_t15_support:seed_segment(
        Conn,
        Rid,
        1,
        3,
        null,
        15000,
        <<"散会，各自跟进"/utf8>>
    ),
    SumId = talk_record_t15_support:seed_summary(Conn, Rid, 1, #{
        overview => <<"会议纪要草稿"/utf8>>,
        decisions => [#{<<"text">> => <<"通过方案A"/utf8>>, <<"segment_ids">> => [S1]}],
        actions => [#{<<"text">> => <<"下周提交报告"/utf8>>, <<"segment_ids">> => [S2]}]
    }),
    #{
        record => Rid,
        conn => Conn,
        u1 => U1,
        u2 => U2,
        u3 => U3,
        sp1 => Sp1,
        sp2 => Sp2,
        segs => [S1, S2, S3, _S4],
        summary_id => SumId
    }.

%%%===================================================================
%%% GET /:id/segments
%%%===================================================================

segments_list_roles_test_() ->
    with_db(fun(State) ->
        F = transcribed_fixture(State, 1),
        U2 = maps:get(u2, F),
        U7 = 700007,
        Rid = maps:get(record, F),

        %% owner 正序全量 + etag 惯例 + origin/revision 投影
        {ok, Result} = talk_record_content_segments:list(
            maps:get(u1, F),
            Rid,
            #{speaker_id => undefined, q => <<>>},
            undefined,
            20
        ),
        Segments = maps:get(<<"segments">>, Result),
        ?assertEqual(4, length(Segments)),
        ?assertEqual(1, maps:get(<<"transcript_version">>, Result)),
        ?assertEqual(<<"ready">>, maps:get(<<"transcript_status">>, Result)),
        [First | _] = Segments,
        ?assertEqual(0, maps:get(<<"start_ms">>, First)),
        ?assertEqual(<<"model">>, maps:get(<<"origin">>, First)),
        ?assertEqual(
            <<"segment:", (integer_to_binary(maps:get(<<"id">>, First)))/binary, ":1">>,
            maps:get(<<"etag">>, First)
        ),

        %% viewer（活跃 grant）同样可读
        ?assertMatch(
            {ok, _},
            talk_record_content_segments:list(
                U2,
                Rid,
                #{speaker_id => undefined, q => <<>>},
                undefined,
                20
            )
        ),

        %% 陌生人（无 grant）404；record 不存在 404
        ?assertEqual(
            {error, not_found},
            talk_record_content_segments:list(
                U7,
                Rid,
                #{speaker_id => undefined, q => <<>>},
                undefined,
                20
            )
        ),
        ?assertEqual(
            {error, not_found},
            talk_record_content_segments:list(
                maps:get(u1, F),
                999999999,
                #{speaker_id => undefined, q => <<>>},
                undefined,
                20
            )
        )
    end).

segments_list_speaker_filter_test_() ->
    with_db(fun(State) ->
        F = transcribed_fixture(State, 2),
        {ok, Result} = talk_record_content_segments:list(
            maps:get(u1, F),
            maps:get(record, F),
            #{speaker_id => maps:get(sp2, F), q => <<>>},
            undefined,
            20
        ),
        Segments = maps:get(<<"segments">>, Result),
        ?assertEqual(1, length(Segments)),
        ?assertEqual(maps:get(sp2, F), maps:get(<<"speaker_id">>, hd(Segments)))
    end).

segments_list_q_highlight_test_() ->
    with_db(fun(State) ->
        F = transcribed_fixture(State, 3),
        Q = <<"会议"/utf8>>,
        {ok, Result} = talk_record_content_segments:list(
            maps:get(u1, F),
            maps:get(record, F),
            #{speaker_id => undefined, q => Q},
            undefined,
            20
        ),
        Segments = maps:get(<<"segments">>, Result),
        %% 4 段中 3 段含「会议」；不含 q 的段被过滤
        ?assertEqual(3, length(Segments)),
        [S1, S2, _S3] = Segments,
        %% 「会议开始，先过项目进度」：高亮 start=0 end=2（codepoint）
        ?assertEqual([#{<<"start">> => 0, <<"end">> => 2}], maps:get(<<"highlights">>, S1)),
        %% 「本周会议重点有三个事项」：高亮 start=2 end=4
        ?assertEqual([#{<<"start">> => 2, <<"end">> => 4}], maps:get(<<"highlights">>, S2)),
        %% 无 q 时不出高亮字段
        {ok, Plain} = talk_record_content_segments:list(
            maps:get(u1, F),
            maps:get(record, F),
            #{speaker_id => undefined, q => <<>>},
            undefined,
            20
        ),
        [P1 | _] = maps:get(<<"segments">>, Plain),
        ?assertNot(maps:is_key(<<"highlights">>, P1))
    end).

segments_list_cursor_paging_test_() ->
    with_db(fun(State) ->
        F = transcribed_fixture(State, 4),
        U1 = maps:get(u1, F),
        Rid = maps:get(record, F),
        {ok, Page1} = talk_record_content_segments:list(
            U1,
            Rid,
            #{speaker_id => undefined, q => <<>>},
            undefined,
            2
        ),
        ?assertEqual(2, length(maps:get(<<"segments">>, Page1))),
        Cursor1 = maps:get(<<"next_cursor">>, Page1),
        ?assert(is_binary(Cursor1)),
        {ok, Page2} = talk_record_content_segments:list(
            U1,
            Rid,
            #{speaker_id => undefined, q => <<>>},
            Cursor1,
            2
        ),
        ?assertEqual(2, length(maps:get(<<"segments">>, Page2))),
        ?assertEqual(null, maps:get(<<"next_cursor">>, Page2)),
        %% 正序贯穿：4 段无重叠无遗漏
        All = maps:get(<<"segments">>, Page1) ++ maps:get(<<"segments">>, Page2),
        ?assertEqual([0, 5000, 10000, 15000], [maps:get(<<"start_ms">>, S) || S <- All]),
        %% 换筛选沿用游标 → cursor_invalid（422）
        ?assertEqual(
            {error, cursor_invalid},
            talk_record_content_segments:list(
                U1,
                Rid,
                #{speaker_id => maps:get(sp1, F), q => <<>>},
                Cursor1,
                2
            )
        )
    end).

segments_list_not_transcribed_test_() ->
    with_db(fun(State) ->
        Ids = talk_record_t15_support:ids(),
        {ok, Rid} = talk_record_t15_support:create_record(
            maps:get(u1, Ids),
            24,
            <<"仅录音未转写"/utf8>>
        ),
        {ok, Result} = talk_record_content_segments:list(
            maps:get(u1, Ids),
            Rid,
            #{speaker_id => undefined, q => <<>>},
            undefined,
            20
        ),
        ?assertEqual([], maps:get(<<"segments">>, Result)),
        ?assertEqual(null, maps:get(<<"transcript_version">>, Result)),
        ?assertEqual(<<"not_requested">>, maps:get(<<"transcript_status">>, Result))
    end).

%%%===================================================================
%%% PATCH /:id/segments/:segment_id
%%%===================================================================

segment_update_owner_editor_test_() ->
    with_db(fun(State) ->
        F = transcribed_fixture(State, 5),
        Conn = maps:get(conn, F),
        U1 = maps:get(u1, F),
        U3 = maps:get(u3, F),
        Rid = maps:get(record, F),
        [S1 | _] = maps:get(segs, F),

        %% owner 编辑：text 修订 → origin=human、revision+1、content_revision+1、
        %% 摘要 stale、审计存在
        {ok, R1} = talk_record_content_segments:update(
            U1,
            Rid,
            S1,
            <<"\"1\"">>,
            #{<<"text">> => <<"会议开始（人工修订）"/utf8>>}
        ),
        Seg1 = maps:get(<<"segment">>, R1),
        ?assertEqual(2, maps:get(<<"revision">>, Seg1)),
        ?assertEqual(<<"human">>, maps:get(<<"origin">>, Seg1)),
        ?assertEqual(U1, maps:get(<<"edited_by">>, Seg1)),
        ?assertEqual(2, maps:get(<<"content_revision">>, R1)),
        ?assertEqual(2, maps:get(<<"revision">>, Seg1)),
        SummaryRow = talk_record_t15_support:one(
            Conn,
            <<"SELECT status FROM talk_record_summary WHERE record_id=",
                (integer_to_binary(Rid))/binary>>
        ),
        ?assertEqual(<<"stale">>, maps:get(<<"status">>, SummaryRow)),
        ?assert(talk_record_t15_support:audit_count(Conn, Rid, <<"segment_edit">>) >= 1),

        %% editor（grant）二次编辑：新 revision 域 + content_revision 再推
        {ok, R2} = talk_record_content_segments:update(
            U3,
            Rid,
            S1,
            <<"segment:", (integer_to_binary(S1))/binary, ":2">>,
            #{<<"text">> => <<"再修订"/utf8>>}
        ),
        ?assertEqual(3, maps:get(<<"revision">>, maps:get(<<"segment">>, R2))),
        ?assertEqual(3, maps:get(<<"content_revision">>, R2)),

        %% 换绑同记录 speaker 也可（sp2）
        {ok, R3} = talk_record_content_segments:update(
            U3,
            Rid,
            S1,
            <<"3">>,
            #{<<"speaker_id">> => maps:get(sp2, F)}
        ),
        ?assertEqual(
            maps:get(sp2, F),
            maps:get(<<"speaker_id">>, maps:get(<<"segment">>, R3))
        )
    end).

segment_update_denials_test_() ->
    with_db(fun(State) ->
        F = transcribed_fixture(State, 6),
        U2 = maps:get(u2, F),
        [S1 | _] = maps:get(segs, F),
        Rid = maps:get(record, F),

        %% viewer 403（有读权限无编辑能力）
        ?assertEqual(
            {error, forbidden},
            talk_record_content_segments:update(
                U2,
                Rid,
                S1,
                <<"\"1\"">>,
                #{<<"text">> => <<"x">>}
            )
        ),
        %% 陌生人 404
        ?assertEqual(
            {error, not_found},
            talk_record_content_segments:update(
                700007,
                Rid,
                S1,
                <<"\"1\"">>,
                #{<<"text">> => <<"x">>}
            )
        ),
        %% 缺 If-Match 428
        ?assertEqual(
            {error, precondition_required},
            talk_record_content_segments:update(
                maps:get(u1, F),
                Rid,
                S1,
                undefined,
                #{<<"text">> => <<"x">>}
            )
        ),
        %% 空改 422
        ?assertEqual(
            {error, missing_editable_field},
            talk_record_content_segments:update(maps:get(u1, F), Rid, S1, <<"\"1\"">>, #{})
        ),
        %% If-Match 令牌携带他人 id → 422
        ?assertEqual(
            {error, invalid_if_match},
            talk_record_content_segments:update(
                maps:get(u1, F),
                Rid,
                S1,
                <<"segment:999:1">>,
                #{<<"text">> => <<"x">>}
            )
        )
    end).

segment_update_concurrent_412_test_() ->
    with_db(fun(State) ->
        F = transcribed_fixture(State, 7),
        U1 = maps:get(u1, F),
        U3 = maps:get(u3, F),
        Rid = maps:get(record, F),
        [S1 | _] = maps:get(segs, F),

        %% 并发修改：owner 先改（rev 1→2），editor 持旧 rev 1 再改 → 412
        {ok, _} = talk_record_content_segments:update(
            U1,
            Rid,
            S1,
            <<"\"1\"">>,
            #{<<"text">> => <<"第一次修改"/utf8>>}
        ),
        ?assertEqual(
            {error, precondition_failed},
            talk_record_content_segments:update(
                U3,
                Rid,
                S1,
                <<"\"1\"">>,
                #{<<"text">> => <<"基于旧版本的并发修改"/utf8>>}
            )
        )
    end).

segment_update_cross_record_404_test_() ->
    with_db(fun(State) ->
        F1 = transcribed_fixture(State, 8),
        F2 = transcribed_fixture(State, 9),
        %% 记录 B 的 segment 拿到记录 A 路径下编辑 → 404（复合 FK 兜底 + 显式）
        [SegB | _] = maps:get(segs, F2),
        ?assertEqual(
            {error, segment_not_found},
            talk_record_content_segments:update(
                maps:get(u1, F1),
                maps:get(record, F1),
                SegB,
                <<"\"1\"">>,
                #{<<"text">> => <<"越权改写"/utf8>>}
            )
        ),
        %% 记录 B 的 speaker 绑到记录 A 的 segment → 404
        [SegA | _] = maps:get(segs, F1),
        ?assertEqual(
            {error, speaker_not_found},
            talk_record_content_segments:update(
                maps:get(u1, F1),
                maps:get(record, F1),
                SegA,
                <<"\"1\"">>,
                #{<<"speaker_id">> => maps:get(sp1, F2)}
            )
        )
    end).

%%%===================================================================
%%% GET/PATCH /:id/summary
%%%===================================================================

summary_get_states_test_() ->
    with_db(fun(State) ->
        F = transcribed_fixture(State, 10),
        U1 = maps:get(u1, F),
        Rid = maps:get(record, F),
        Segs = maps:get(segs, F),

        %% 已生成：draft + overview/decisions/actions + 源标识 + ETag
        {ok, Summary} = talk_record_content_summary:get(U1, Rid),
        ?assertEqual(<<"draft">>, maps:get(<<"status">>, Summary)),
        ?assertEqual(1, maps:get(<<"source_transcript_version">>, Summary)),
        ?assertEqual(1, maps:get(<<"source_content_revision">>, Summary)),
        ?assertEqual(<<"summary:1">>, maps:get(<<"etag">>, Summary)),
        Decisions = maps:get(<<"decisions">>, Summary),
        ?assertEqual(1, length(Decisions)),
        ?assertEqual([hd(Segs)], maps:get(<<"segment_ids">>, hd(Decisions))),

        %% viewer 可读
        ?assertMatch({ok, _}, talk_record_content_summary:get(maps:get(u2, F), Rid)),
        %% 陌生人 404
        ?assertEqual({error, not_found}, talk_record_content_summary:get(700007, Rid)),

        %% 未生成：状态对象，不伪装 404
        {ok, Plain} = talk_record_t15_support:create_record(U1, 11, <<"t15-no-summary"/utf8>>),
        {ok, StatusObj} = talk_record_content_summary:get(U1, Plain),
        ?assertEqual(<<"not_generated">>, maps:get(<<"status">>, StatusObj)),
        ?assertEqual(null, maps:get(<<"summary">>, StatusObj))
    end).

summary_edit_and_confirm_test_() ->
    with_db(fun(State) ->
        F = transcribed_fixture(State, 12),
        Conn = maps:get(conn, F),
        U1 = maps:get(u1, F),
        U3 = maps:get(u3, F),
        Rid = maps:get(record, F),
        [_, S2 | _] = maps:get(segs, F),

        %% editor 人工编辑（合法 segment_ids）→ revision+1、status=draft、源重基
        {ok, R1} = talk_record_content_summary:update(U3, Rid, <<"\"1\"">>, #{
            <<"overview">> => <<"人工校对后的纪要"/utf8>>,
            <<"decisions">> => [#{<<"text">> => <<"决策1"/utf8>>, <<"segment_ids">> => [S2]}]
        }),
        Sum1 = maps:get(<<"summary">>, R1),
        ?assertEqual(2, maps:get(<<"revision">>, Sum1)),
        ?assertEqual(<<"draft">>, maps:get(<<"status">>, Sum1)),
        ?assert(talk_record_t15_support:audit_count(Conn, Rid, <<"summary_edit">>) >= 1),

        %% confirm（无字段）→ confirmed + confirmed_by + 审计；revision 再推
        {ok, R2} = talk_record_content_summary:update(
            U1,
            Rid,
            <<"summary:2">>,
            #{<<"confirm">> => true}
        ),
        Sum2 = maps:get(<<"summary">>, R2),
        ?assertEqual(<<"confirmed">>, maps:get(<<"status">>, Sum2)),
        ?assertEqual(U1, maps:get(<<"confirmed_by">>, Sum2)),
        ?assert(talk_record_t15_support:audit_count(Conn, Rid, <<"summary_confirm">>) >= 1),

        %% 编辑+confirm 同请求：重基后源一致，直接确认成功
        {ok, R3} = talk_record_content_summary:update(
            U1,
            Rid,
            <<"summary:", (integer_to_binary(maps:get(<<"revision">>, Sum2)))/binary>>,
            #{<<"overview">> => <<"终稿"/utf8>>, <<"confirm">> => true}
        ),
        ?assertEqual(<<"confirmed">>, maps:get(<<"status">>, maps:get(<<"summary">>, R3))),

        %% 幂等重确认：返回现状（revision 不再推进）
        Rev3 = maps:get(<<"revision">>, maps:get(<<"summary">>, R3)),
        {ok, R4} = talk_record_content_summary:update(
            U1,
            Rid,
            <<"summary:", (integer_to_binary(Rev3))/binary>>,
            #{<<"confirm">> => true}
        ),
        ?assertEqual(Rev3, maps:get(<<"revision">>, maps:get(<<"summary">>, R4))),

        %% viewer 403；陌生人 404；缺 If-Match 428
        ?assertEqual(
            {error, forbidden},
            talk_record_content_summary:update(
                maps:get(u2, F),
                Rid,
                <<"\"1\"">>,
                #{<<"confirm">> => true}
            )
        ),
        ?assertEqual(
            {error, not_found},
            talk_record_content_summary:update(
                700007,
                Rid,
                <<"\"1\"">>,
                #{<<"confirm">> => true}
            )
        ),
        ?assertEqual(
            {error, precondition_required},
            talk_record_content_summary:update(U1, Rid, undefined, #{<<"confirm">> => true})
        )
    end).

summary_stale_confirm_409_test_() ->
    with_db(fun(State) ->
        F = transcribed_fixture(State, 13),
        U1 = maps:get(u1, F),
        U3 = maps:get(u3, F),
        Rid = maps:get(record, F),
        [S1 | _] = maps:get(segs, F),

        %% 场景 A（source revision 不匹配 409）：owner 改原文（摘要标 stale、
        %% content_revision 推进）后，editor 拿最新摘要 rev 直接 confirm →
        %% If-Match 过，但 source_content_revision ≠ 当前 content_revision → 409
        {ok, _} = talk_record_content_segments:update(
            U1,
            Rid,
            S1,
            <<"\"1\"">>,
            #{<<"text">> => <<"改写后原文"/utf8>>}
        ),
        {ok, Fresh} = talk_record_content_summary:get(U1, Rid),
        ?assertEqual(<<"stale">>, maps:get(<<"status">>, Fresh)),
        ?assertEqual(
            {error, summary_source_outdated},
            talk_record_content_summary:update(
                U3,
                Rid,
                maps:get(<<"etag">>, Fresh),
                #{<<"confirm">> => true}
            )
        ),

        %% 场景 B（并发修改 412）：两个主体同持 rev 1，A 先编辑（rev→2），
        %% B 持 rev 1 再编辑 → If-Match CAS 412
        F2 = transcribed_fixture(State, 22),
        {ok, _First} = talk_record_content_summary:update(
            maps:get(u1, F2),
            maps:get(record, F2),
            <<"\"1\"">>,
            #{<<"overview">> => <<"A 先改"/utf8>>}
        ),
        ?assertEqual(
            {error, precondition_failed},
            talk_record_content_summary:update(
                maps:get(u3, F2),
                maps:get(record, F2),
                <<"\"1\"">>,
                #{<<"overview">> => <<"B 基于旧版本改"/utf8>>}
            )
        )
    end).

summary_invalid_refs_test_() ->
    with_db(fun(State) ->
        F = transcribed_fixture(State, 14),
        F2 = transcribed_fixture(State, 15),
        Rid = maps:get(record, F),
        [SegB | _] = maps:get(segs, F2),

        %% 引用他记录 segment → 422
        ?assertEqual(
            {error, invalid_summary_segment_ref},
            talk_record_content_summary:update(maps:get(u1, F), Rid, <<"\"1\"">>, #{
                <<"decisions">> => [#{<<"text">> => <<"x"/utf8>>, <<"segment_ids">> => [SegB]}]
            })
        ),
        %% 结构不合法（segment_ids 非数组）→ 422
        ?assertEqual(
            {error, invalid_summary_items},
            talk_record_content_summary:update(maps:get(u1, F), Rid, <<"\"1\"">>, #{
                <<"actions">> => [#{<<"segment_ids">> => 123}]
            })
        ),
        %% confirm 非布尔 → 422
        ?assertEqual(
            {error, invalid_confirm},
            talk_record_content_summary:update(maps:get(u1, F), Rid, <<"\"1\"">>, #{
                <<"confirm">> => <<"yes">>
            })
        )
    end).

summary_patch_not_generated_404_test_() ->
    with_db(fun(State) ->
        Ids = talk_record_t15_support:ids(),
        {ok, Rid} = talk_record_t15_support:create_record(
            maps:get(u1, Ids),
            25,
            <<"无摘要记录"/utf8>>
        ),
        %% GET 状态对象（不伪装 404）已覆盖；PATCH 无摘要可编辑 → 404
        ?assertEqual(
            {error, summary_not_found},
            talk_record_content_summary:update(
                maps:get(u1, Ids),
                Rid,
                <<"\"1\"">>,
                #{<<"confirm">> => true}
            )
        )
    end).

summary_edit_rebase_unstales_test_() ->
    with_db(fun(State) ->
        F = transcribed_fixture(State, 26),
        U1 = maps:get(u1, F),
        Rid = maps:get(record, F),
        [S1 | _] = maps:get(segs, F),

        %% 原文校对 → 摘要 stale（source 停在旧值）；人工编辑重基后可确认
        {ok, _} = talk_record_content_segments:update(
            U1,
            Rid,
            S1,
            <<"\"1\"">>,
            #{<<"text">> => <<"重基场景原文"/utf8>>}
        ),
        {ok, Stale} = talk_record_content_summary:get(U1, Rid),
        ?assertEqual(<<"stale">>, maps:get(<<"status">>, Stale)),
        {ok, R1} = talk_record_content_summary:update(
            U1,
            Rid,
            maps:get(<<"etag">>, Stale),
            #{<<"overview">> => <<"重基后的纪要"/utf8>>}
        ),
        Edited = maps:get(<<"summary">>, R1),
        ?assertEqual(<<"draft">>, maps:get(<<"status">>, Edited)),
        ?assertEqual(2, maps:get(<<"source_content_revision">>, Edited)),
        %% 重基后确认通过（不再 409）
        {ok, R2} = talk_record_content_summary:update(
            U1,
            Rid,
            maps:get(<<"etag">>, Edited),
            #{<<"confirm">> => true}
        ),
        ?assertEqual(<<"confirmed">>, maps:get(<<"status">>, maps:get(<<"summary">>, R2)))
    end).

%%%===================================================================
%%% GET/PATCH /:id/speakers + claim
%%%===================================================================

speakers_list_and_patch_test_() ->
    with_db(fun(State) ->
        F = transcribed_fixture(State, 16),
        Conn = maps:get(conn, F),
        U1 = maps:get(u1, F),
        U3 = maps:get(u3, F),
        Rid = maps:get(record, F),
        Sp1 = maps:get(sp1, F),

        %% 清单：白名单字段 + 匿名标签 + claimable + etag；无 embedding 类字段
        {ok, Listed} = talk_record_content_speakers:list(U1, Rid),
        Speakers = maps:get(<<"speakers">>, Listed),
        ?assertEqual(2, length(Speakers)),
        [A, B] = Speakers,
        ?assertEqual(<<"anonymous">>, maps:get(<<"identity_source">>, A)),
        ?assertEqual(true, maps:get(<<"claimable">>, A)),
        ?assertEqual(
            <<"speaker:", (integer_to_binary(maps:get(<<"id">>, A)))/binary, ":1">>,
            maps:get(<<"etag">>, A)
        ),
        %% 两个夹具 speaker 均未绑定用户 → 可认领
        ?assertEqual(true, maps:get(<<"claimable">>, B)),
        lists:foreach(
            fun(S) ->
                lists:foreach(
                    fun(K) -> ?assertNot(maps:is_key(K, S)) end,
                    [<<"embedding">>, <<"voice">>, <<"verified">>]
                )
            end,
            Speakers
        ),

        %% editor 人工命名（仅 display_name 不动来源）
        {ok, R1} = talk_record_content_speakers:update(
            U3,
            Rid,
            Sp1,
            <<"\"1\"">>,
            #{<<"display_name">> => <<"主持人"/utf8>>}
        ),
        Sp1b = maps:get(<<"speaker">>, R1),
        ?assertEqual(2, maps:get(<<"revision">>, Sp1b)),
        ?assertEqual(<<"anonymous">>, maps:get(<<"identity_source">>, Sp1b)),

        %% owner 人工标记 U3（同 org 成员 → 目录内）→ manual + confirmed_by + 审计
        {ok, R2} = talk_record_content_speakers:update(
            U1,
            Rid,
            Sp1,
            <<"speaker:", (integer_to_binary(Sp1))/binary, ":2">>,
            #{<<"matched_user_id">> => U3}
        ),
        Sp1c = maps:get(<<"speaker">>, R2),
        ?assertEqual(<<"manual">>, maps:get(<<"identity_source">>, Sp1c)),
        ?assertEqual(U1, maps:get(<<"confirmed_by">>, Sp1c)),
        ?assertEqual(3, maps:get(<<"content_revision">>, R2)),
        ?assert(talk_record_t15_support:audit_count(Conn, Rid, <<"speaker_edit">>) >= 1),

        %% 客户端指定 identity_source → 422
        ?assertEqual(
            {error, invalid_identity_source},
            talk_record_content_speakers:update(
                U1,
                Rid,
                Sp1,
                <<"\"3\"">>,
                #{<<"identity_source">> => <<"self_claim">>}
            )
        ),
        %% viewer 403 / 陌生人 404 / 缺 If-Match 428
        ?assertEqual(
            {error, forbidden},
            talk_record_content_speakers:update(
                maps:get(u2, F),
                Rid,
                Sp1,
                <<"\"3\"">>,
                #{<<"display_name">> => <<"x"/utf8>>}
            )
        ),
        ?assertEqual(
            {error, not_found},
            talk_record_content_speakers:update(
                700007,
                Rid,
                Sp1,
                <<"\"3\"">>,
                #{<<"display_name">> => <<"x"/utf8>>}
            )
        ),
        ?assertEqual(
            {error, precondition_required},
            talk_record_content_speakers:update(
                U1,
                Rid,
                Sp1,
                undefined,
                #{<<"display_name">> => <<"x"/utf8>>}
            )
        )
    end).

speakers_directory_guard_test_() ->
    with_db(fun(State) ->
        F = transcribed_fixture(State, 17),
        Conn = maps:get(conn, F),
        Ids = talk_record_t15_support:ids(),
        U1 = maps:get(u1, F),
        U2 = maps:get(u2, Ids),
        U3 = maps:get(u3, Ids),
        U4 = maps:get(u4, Ids),
        U7 = maps:get(u7, Ids),
        Rid = maps:get(record, F),
        Sp1 = maps:get(sp1, F),

        %% org 记录（不绑定 ws）：目录 = 同 org active 成员
        {ok, OrgRid} = talk_record_t15_support:create_org_record(
            U1,
            maps:get(org_a, Ids),
            0,
            18
        ),
        SpOrg = talk_record_t15_support:seed_speaker(Conn, OrgRid, <<"SPEAKER_00">>, #{}),
        talk_record_t15_support:add_grant(Conn, OrgRid, U3, U1, <<"editor">>),

        %% 同 org 成员（U3）→ manual 通过
        ?assertMatch(
            {ok, _},
            talk_record_content_speakers:update(
                U1,
                OrgRid,
                SpOrg,
                <<"\"1\"">>,
                #{<<"matched_user_id">> => U3}
            )
        ),
        %% 目录外：ORG_B 用户（跨企业）→ 422
        ?assertEqual(
            {error, target_not_in_directory},
            talk_record_content_speakers:update(
                U1,
                OrgRid,
                SpOrg,
                <<"\"2\"">>,
                #{<<"matched_user_id">> => U4}
            )
        ),
        %% 目录外：无任何组织关系的陌生人 → 422
        ?assertEqual(
            {error, target_not_in_directory},
            talk_record_content_speakers:update(
                U1,
                OrgRid,
                SpOrg,
                <<"\"2\"">>,
                #{<<"matched_user_id">> => U7}
            )
        ),

        %% org 记录（绑定 WS_A1）：目录再叠加 ws 资源授权——U2 是 org 成员但
        %% 无 ws 授权 → 422；U1（ws 成员）→ 通过
        {ok, WsRid} = talk_record_t15_support:create_org_record(
            U1,
            maps:get(org_a, Ids),
            maps:get(ws_a1, Ids),
            23
        ),
        SpWs = talk_record_t15_support:seed_speaker(Conn, WsRid, <<"SPEAKER_00">>, #{}),
        ?assertEqual(
            {error, target_not_in_directory},
            talk_record_content_speakers:update(
                U1,
                WsRid,
                SpWs,
                <<"\"1\"">>,
                #{<<"matched_user_id">> => U2}
            )
        ),
        ?assertMatch(
            {ok, _},
            talk_record_content_speakers:update(
                U1,
                WsRid,
                SpWs,
                <<"\"1\"">>,
                #{<<"matched_user_id">> => U1}
            )
        ),

        %% 跨 record speaker 404
        ?assertEqual(
            {error, speaker_not_found},
            talk_record_content_speakers:update(
                U1,
                Rid,
                SpOrg,
                <<"\"1\"">>,
                #{<<"display_name">> => <<"x"/utf8>>}
            )
        ),

        %% personal：不存在用户 → 422
        ?assertEqual(
            {error, target_not_in_directory},
            talk_record_content_speakers:update(
                U1,
                Rid,
                Sp1,
                <<"\"1\"">>,
                #{<<"matched_user_id">> => 987654321}
            )
        )
    end).

speaker_claim_test_() ->
    with_db(fun(State) ->
        F = transcribed_fixture(State, 19),
        Conn = maps:get(conn, F),
        Ids = talk_record_t15_support:ids(),
        U1 = maps:get(u1, F),
        U2 = maps:get(u2, F),
        U3 = maps:get(u3, F),
        Rid = maps:get(record, F),
        Sp1 = maps:get(sp1, F),
        Sp2 = maps:get(sp2, F),

        %% U2 有 viewer grant 但非参与者 → 403
        ?assertEqual(
            {error, claim_not_participant},
            talk_record_content_speakers:claim(U2, Rid, Sp1)
        ),

        %% U2 加入参与者 → 认领成功：self_claim + matched=U2 + content_revision+1 + 审计
        talk_record_t15_support:add_participant(Conn, Rid, U2, <<"到场同事"/utf8>>),
        {ok, C1} = talk_record_content_speakers:claim(U2, Rid, Sp1),
        Claimed = maps:get(<<"speaker">>, C1),
        ?assertEqual(<<"self_claim">>, maps:get(<<"identity_source">>, Claimed)),
        ?assertEqual(U2, maps:get(<<"matched_user_id">>, Claimed)),
        ?assertEqual(U2, maps:get(<<"confirmed_by">>, Claimed)),
        ?assertEqual(2, maps:get(<<"content_revision">>, C1)),
        ?assert(talk_record_t15_support:audit_count(Conn, Rid, <<"speaker_claim">>) >= 1),
        %% 认领不是生物认证：DTO 无 verified 类字段
        ?assertNot(maps:is_key(<<"verified">>, Claimed)),

        %% 幂等重认领：返回现状（revision 不再推进）
        {ok, C2} = talk_record_content_speakers:claim(U2, Rid, Sp1),
        ?assertEqual(
            maps:get(<<"revision">>, Claimed),
            maps:get(<<"revision">>, maps:get(<<"speaker">>, C2))
        ),

        %% owner 人工标记 Sp2 → U3；U2 再认领 Sp2 → 409 + 冲突审计
        {ok, _} = talk_record_content_speakers:update(
            U1,
            Rid,
            Sp2,
            <<"\"1\"">>,
            #{<<"matched_user_id">> => U3}
        ),
        Before = talk_record_t15_support:audit_count(Conn, Rid, <<"speaker_claim">>),
        ?assertEqual(
            {error, speaker_claim_conflict},
            talk_record_content_speakers:claim(U2, Rid, Sp2)
        ),
        ?assertEqual(
            Before + 1, talk_record_t15_support:audit_count(Conn, Rid, <<"speaker_claim">>)
        ),
        ConflictRow = talk_record_t15_support:audit_row(Conn, Rid, <<"speaker_claim">>),
        ?assertEqual(
            <<"conflict">>,
            maps:get(
                <<"outcome">>,
                maps:get(<<"metadata_allowlist">>, ConflictRow)
            )
        ),

        %% 陌生人 404；不存在的 speaker 404
        ?assertEqual(
            {error, not_found},
            talk_record_content_speakers:claim(maps:get(u7, Ids), Rid, Sp1)
        ),
        ?assertEqual(
            {error, speaker_not_found},
            talk_record_content_speakers:claim(U2, Rid, 999999998)
        ),

        %% 参与者名单 ≠ 授权：U7 在参与者名单（直插）但无记录访问权 → 404
        talk_record_t15_support:add_participant(Conn, Rid, maps:get(u7, Ids), <<"路过"/utf8>>),
        ?assertEqual(
            {error, not_found},
            talk_record_content_speakers:claim(maps:get(u7, Ids), Rid, Sp1)
        )
    end).

%%%===================================================================
%%% 版本域纪律 + 审计白名单
%%%===================================================================

version_domains_test_() ->
    with_db(fun(State) ->
        F = transcribed_fixture(State, 20),
        Conn = maps:get(conn, F),
        U1 = maps:get(u1, F),
        Rid = maps:get(record, F),
        [S1 | _] = maps:get(segs, F),
        Sp1 = maps:get(sp1, F),

        Row0 = talk_record_t15_support:one(
            Conn,
            <<"SELECT revision, content_revision, acl_revision FROM talk_record WHERE id=",
                (integer_to_binary(Rid))/binary>>
        ),

        %% segment 编辑：content_revision+1；record.revision / acl_revision 不动
        {ok, _} = talk_record_content_segments:update(
            U1,
            Rid,
            S1,
            <<"\"1\"">>,
            #{<<"text">> => <<"v 域校验"/utf8>>}
        ),
        %% speaker 编辑：content_revision 再 +1
        {ok, _} = talk_record_content_speakers:update(
            U1,
            Rid,
            Sp1,
            <<"\"1\"">>,
            #{<<"display_name">> => <<"名字"/utf8>>}
        ),
        Row1 = talk_record_t15_support:one(
            Conn,
            <<"SELECT revision, content_revision, acl_revision FROM talk_record WHERE id=",
                (integer_to_binary(Rid))/binary>>
        ),
        ?assertEqual(
            maps:get(<<"content_revision">>, Row0) + 2,
            maps:get(<<"content_revision">>, Row1)
        ),
        ?assertEqual(maps:get(<<"revision">>, Row0), maps:get(<<"revision">>, Row1)),
        ?assertEqual(maps:get(<<"acl_revision">>, Row0), maps:get(<<"acl_revision">>, Row1)),

        %% summary 编辑（重基 draft）：三者仍不动，仅 summary.revision 推进
        {ok, _} = talk_record_content_summary:update(
            U1,
            Rid,
            <<"\"1\"">>,
            #{<<"overview">> => <<"版本域校验纪要"/utf8>>}
        ),
        Row2 = talk_record_t15_support:one(
            Conn,
            <<"SELECT revision, content_revision, acl_revision FROM talk_record WHERE id=",
                (integer_to_binary(Rid))/binary>>
        ),
        ?assertEqual(Row1, Row2)
    end).

audit_allowlist_test_() ->
    with_db(fun(State) ->
        F = transcribed_fixture(State, 21),
        Conn = maps:get(conn, F),
        U1 = maps:get(u1, F),
        Rid = maps:get(record, F),
        [S1 | _] = maps:get(segs, F),

        {ok, _} = talk_record_content_segments:update(
            U1,
            Rid,
            S1,
            <<"\"1\"">>,
            #{<<"text">> => <<"审计白名单校验用修订文本"/utf8>>}
        ),
        Row = talk_record_t15_support:audit_row(Conn, Rid, <<"segment_edit">>),
        ?assertEqual(S1, maps:get(<<"target_id">>, Row)),
        ?assertEqual(U1, maps:get(<<"actor_id">>, Row)),
        Meta = maps:get(<<"metadata_allowlist">>, Row),
        %% 白名单键存在；正文（text/新旧文本）绝不入审计
        lists:foreach(
            fun(K) -> ?assert(maps:is_key(K, Meta)) end,
            [<<"fields">>, <<"revision">>, <<"content_revision">>, <<"origin">>]
        ),
        ?assertEqual([<<"text">>], maps:get(<<"fields">>, Meta)),
        Forbidden = [<<"text">>, <<"old_text">>, <<"new_text">>, <<"overview">>, <<"decisions">>],
        lists:foreach(
            fun(K) -> ?assertNot(maps:is_key(K, Meta), {forbidden_audit_key, K}) end,
            Forbidden
        )
    end).
