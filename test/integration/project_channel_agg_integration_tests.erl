-module(project_channel_agg_integration_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include("eunit_setup.hrl").

%%% ZC-04 — 四类聚合（Pinned / Resources / Activity / Related Posts）真库集成测试
%%% （PG 不可用自动 skip）
%%%
%%% 覆盖（W2 计划 TDD 用例 6/8/9/10）：
%%%   6.  Pinned 不含 Group Notice：置顶查询只读 channel_message 且排除 notice 类
%%%       消息（group_notice/announcement 独立存储本就不混入，此处再加
%%%       msg_type 防线并对"公告形态消息落频道流"的未来演化兜底）
%%%   8.  Activity 不含正文：事件列表只有元数据，payload 无 content/message 类字段
%%%   9.  Related Posts 有界摘要：每关联 channel 最近 N=5 条 + 总量上限 50，无正文
%%%   10. 四聚合空态（空数组非错误）+ 稳定分页（同序键 created_at DESC, id DESC）
%%%
%%% SQL 查询数上限（固定 1-2 条 SQL/请求，无 N+1）由
%%% project_channel_logic_tests:sql_bound_test_ 以 mock 计数承接。

%%% ===================================================================
%%% 用例 6：Pinned 排除公告形态消息，只返回普通置顶
%%% ===================================================================

pinned_excludes_notice_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Ctx = setup_agg_world(<<"ZC04-agg-pin">>),
        #{conn := Conn, project_id := ProjId, owner := Owner, ch1 := Ch1} = Ctx,
        try
            {ok, created} = project_channel_logic:link(Owner, ProjId, Ch1),
            NormalId = insert_msg(Conn, Ch1, Owner, <<"normal pinned">>, <<"text">>, true),
            NoticeId = insert_msg(Conn, Ch1, Owner, <<"notice body">>, <<"notice">>, true),
            {ok, #{list := Rows}} = project_channel_logic:pinned(Owner, ProjId, 1, 20),
            Ids = [maps:get(<<"id">>, R) || R <- Rows],
            ?assert(lists:member(NormalId, Ids), "普通置顶应在 Pinned 中"),
            ?assertNot(
                lists:member(NoticeId, Ids),
                "公告形态消息（msg_type=notice）不得混入 Pinned"
            ),
            %% 群公告存储于独立 group_notice 表（group_id 维度），结构上不可能
            %% 出现在 channel_message 查询结果中——此处同时断言输出列是
            %% channel_message 元数据白名单，无正文列
            lists:foreach(
                fun(R) ->
                    ?assertEqual(false, maps:is_key(<<"content">>, R)),
                    ?assertEqual(false, maps:is_key(<<"payload">>, R)),
                    ?assertEqual(false, maps:is_key(<<"body">>, R))
                end,
                Rows
            )
        after
            cleanup_agg_world(Conn, Ctx)
        end
    end).

%%% ===================================================================
%%% 用例 8：Activity 只含元数据（payload 无 content/message/body/text）
%%% ===================================================================

activity_metadata_only_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Ctx = setup_agg_world(<<"ZC04-agg-act">>),
        #{conn := Conn, project_id := ProjId, owner := Owner, ch1 := Ch1} = Ctx,
        try
            {ok, created} = project_channel_logic:link(Owner, ProjId, Ch1),
            {ok, unlinked} = project_channel_logic:unlink(Owner, ProjId, Ch1),
            {ok, _} = project_channel_logic:update_links(Owner, ProjId, [
                #{<<"name">> => <<"N">>, <<"url">> => <<"https://e.com">>}
            ]),
            {ok, #{list := Rows}} = project_channel_logic:activity(Owner, ProjId, 1, 50),
            Types = [maps:get(<<"event_type">>, R) || R <- Rows],
            ?assert(lists:member(<<"channel_linked">>, Types)),
            ?assert(lists:member(<<"channel_unlinked">>, Types)),
            ?assert(lists:member(<<"links_updated">>, Types)),
            lists:foreach(
                fun(R) ->
                    Payload = maps:get(<<"payload">>, R),
                    ?assert(is_map(Payload)),
                    lists:foreach(
                        fun(Key) ->
                            ?assertEqual(
                                false,
                                maps:is_key(Key, Payload),
                                io_lib:format("payload 不得含正文字段 ~s", [Key])
                            )
                        end,
                        [<<"content">>, <<"message">>, <<"body">>, <<"text">>]
                    ),
                    ?assertEqual(false, maps:is_key(<<"content">>, R))
                end,
                Rows
            )
        after
            cleanup_agg_world(Conn, Ctx)
        end
    end).

%%% ===================================================================
%%% 用例 9：Related Posts 有界摘要（每频道 5 条上限 + 总量上限 + 无正文）
%%% ===================================================================

related_posts_bounded_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Ctx = setup_agg_world(<<"ZC04-agg-rp">>),
        #{
            conn := Conn,
            project_id := ProjId,
            owner := Owner,
            ch1 := Ch1,
            ch2 := Ch2
        } = Ctx,
        try
            {ok, created} = project_channel_logic:link(Owner, ProjId, Ch1),
            {ok, created} = project_channel_logic:link(Owner, ProjId, Ch2),
            %% ch1 造 7 条、ch2 造 3 条
            lists:foreach(
                fun(I) ->
                    insert_msg(
                        Conn,
                        Ch1,
                        Owner,
                        <<"c1-", (integer_to_binary(I))/binary>>,
                        <<"text">>,
                        false,
                        I
                    )
                end,
                lists:seq(1, 7)
            ),
            lists:foreach(
                fun(I) ->
                    insert_msg(
                        Conn,
                        Ch2,
                        Owner,
                        <<"c2-", (integer_to_binary(I))/binary>>,
                        <<"text">>,
                        false,
                        I
                    )
                end,
                lists:seq(1, 3)
            ),
            {ok, Rows} = project_channel_logic:related_posts(Owner, ProjId),
            ?assertEqual(
                8,
                length(Rows),
                "每频道上限 5（ch1 取 7 条中最近 5）+ ch2 全部 3 条"
            ),
            Ch1Count = length([R || R <- Rows, maps:get(<<"channel_id">>, R) =:= Ch1]),
            Ch2Count = length([R || R <- Rows, maps:get(<<"channel_id">>, R) =:= Ch2]),
            ?assertEqual(5, Ch1Count, "单频道截取最近 5 条"),
            ?assertEqual(3, Ch2Count),
            %% 只取有界摘要元数据，无正文
            lists:foreach(
                fun(R) ->
                    ?assertEqual(false, maps:is_key(<<"content">>, R)),
                    ?assertEqual(false, maps:is_key(<<"payload">>, R))
                end,
                Rows
            )
        after
            cleanup_agg_world(Conn, Ctx)
        end
    end).

%%% ===================================================================
%%% 用例 10：四聚合空态 + Pinned 稳定分页
%%% ===================================================================

empty_states_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Ctx = setup_agg_world(<<"ZC04-agg-empty">>),
        #{conn := Conn, project_id := ProjId, owner := Owner} = Ctx,
        try
            %% 无关联/无事件/无 links → 空数组而非错误
            ?assertMatch(
                {ok, #{list := [], total := 0}},
                project_channel_logic:pinned(Owner, ProjId, 1, 20)
            ),
            ?assertMatch(
                {ok, #{list := [], total := 0}},
                project_channel_logic:activity(Owner, ProjId, 1, 20)
            ),
            ?assertMatch({ok, []}, project_channel_logic:related_posts(Owner, ProjId)),
            ?assertMatch({ok, []}, project_channel_logic:resources(Owner, ProjId)),
            ?assertMatch(
                {ok, #{list := [], total := 0}},
                project_channel_logic:list_channels(Owner, ProjId, 1, 20)
            )
        after
            cleanup_agg_world(Conn, Ctx)
        end
    end).

pinned_stable_pagination_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Ctx = setup_agg_world(<<"ZC04-agg-page">>),
        #{conn := Conn, project_id := ProjId, owner := Owner, ch1 := Ch1, ch2 := Ch2} = Ctx,
        try
            {ok, created} = project_channel_logic:link(Owner, ProjId, Ch1),
            {ok, created} = project_channel_logic:link(Owner, ProjId, Ch2),
            M1 = insert_msg(Conn, Ch1, Owner, <<"m1">>, <<"text">>, true, 1),
            M2 = insert_msg(Conn, Ch2, Owner, <<"m2">>, <<"text">>, true, 2),
            M3 = insert_msg(Conn, Ch1, Owner, <<"m3">>, <<"text">>, true, 3),
            {ok, #{list := Page1}} = project_channel_logic:pinned(Owner, ProjId, 1, 2),
            {ok, #{list := Page2}} = project_channel_logic:pinned(Owner, ProjId, 2, 2),
            ?assertEqual(2, length(Page1)),
            ?assertEqual(1, length(Page2)),
            Ids1 = [maps:get(<<"id">>, R) || R <- Page1],
            Ids2 = [maps:get(<<"id">>, R) || R <- Page2],
            %% Seq 越大 created_at 越早（NOW()-Seq 分钟）；DESC 序 = M1,M2,M3
            ?assertEqual([M1, M2], Ids1, "第一页应为最近两条（created_at DESC）"),
            ?assertEqual([M3], Ids2, "第二页应为最早一条（同序键跨页稳定）"),
            ?assertEqual(
                lists:usort(Ids1 ++ Ids2), Ids1 ++ Ids2, "分页间不得重叠/遗漏"
            )
        after
            cleanup_agg_world(Conn, Ctx)
        end
    end).

%%% ===================================================================
%%% Internal：造数与清理
%%% ===================================================================

%% @doc 造聚合测试世界：ws + project + owner/member2 active project_member
%% + ch1/ch2（同 ws workspace 频道）
setup_agg_world(Tag) ->
    {ok, Conn} = take_conn(),
    {ok, _, [{Owner}, {Member2}]} = epgsql:equery(
        Conn, <<"SELECT id FROM \"user\" ORDER BY id ASC LIMIT 2">>, []
    ),
    WsId = elib_tsid:generate(workspace),
    ProjId = elib_tsid:generate(project),
    Ch1 = elib_tsid:generate(channel),
    Ch2 = elib_tsid:generate(channel),
    {ok, 1} = epgsql:equery(
        Conn,
        <<"INSERT INTO workspace (id, name, owner_id, status)", " VALUES ($1, $2, $3, 'active')">>,
        [WsId, Tag, Owner]
    ),
    {ok, 2} = epgsql:equery(
        Conn,
        <<"INSERT INTO workspace_member (workspace_id, user_id, role, invited_by, joined_at, status)",
            " VALUES ($1, $2, 'owner', NULL, CURRENT_TIMESTAMP, 'active'),",
            "        ($1, $3, 'member', $2, CURRENT_TIMESTAMP, 'active')">>,
        [WsId, Owner, Member2]
    ),
    {ok, 1} = epgsql:equery(
        Conn,
        <<"INSERT INTO project (id, workspace_id, name, owner_id, status)",
            " VALUES ($1, $2, $3, $4, 'active')">>,
        [ProjId, WsId, Tag, Owner]
    ),
    {ok, 2} = epgsql:equery(
        Conn,
        <<"INSERT INTO project_member (workspace_id, project_id, user_id, joined_at, status)",
            " VALUES ($1, $2, $3, CURRENT_TIMESTAMP, 'active'),",
            "        ($1, $2, $4, CURRENT_TIMESTAMP, 'active')">>,
        [WsId, ProjId, Owner, Member2]
    ),
    {ok, 2} = epgsql:equery(
        Conn,
        <<"INSERT INTO channel (id, name, creator_uid, status, scope, workspace_id)",
            " VALUES ($1, $2, $3, 1, 'workspace', $4),",
            "        ($5, $6, $3, 1, 'workspace', $4)">>,
        [Ch1, <<Tag/binary, "-ch1">>, Owner, WsId, Ch2, <<Tag/binary, "-ch2">>]
    ),
    #{
        conn => Conn,
        ws_id => WsId,
        project_id => ProjId,
        owner => Owner,
        member2 => Member2,
        ch1 => Ch1,
        ch2 => Ch2
    }.

cleanup_agg_world(Conn, Ctx) ->
    ChIds = [maps:get(K, Ctx) || K <- [ch1, ch2], maps:is_key(K, Ctx)],
    catch epgsql:equery(
        Conn,
        <<"DELETE FROM channel_message WHERE channel_id = ANY($1::bigint[])">>,
        [ChIds]
    ),
    catch epgsql:equery(
        Conn, <<"DELETE FROM channel WHERE id = ANY($1::bigint[])">>, [ChIds]
    ),
    catch epgsql:equery(
        Conn, <<"DELETE FROM workspace WHERE id = $1">>, [maps:get(ws_id, Ctx)]
    ),
    ok = pooler:return_member(pgsql, Conn, ok),
    ok.

%% @doc 插入频道消息（默认 created_at = NOW() - (Seq) 分钟，保证可排序）
insert_msg(Conn, ChannelId, AuthorId, Content, MsgType, Pinned) ->
    insert_msg(Conn, ChannelId, AuthorId, Content, MsgType, Pinned, 0).

insert_msg(Conn, ChannelId, AuthorId, Content, MsgType, Pinned, Seq) ->
    Id = elib_tsid:generate(),
    {ok, 1} = epgsql:equery(
        Conn,
        <<"INSERT INTO channel_message",
            " (id, channel_id, author_id, author_name, content, msg_type,",
            "  is_pinned, status, revoked, created_at)",
            " VALUES ($1, $2, $3, '', $4, $5, $6, 1, false,",
            "  CURRENT_TIMESTAMP - ($7::int * INTERVAL '1 minute'))">>,
        [Id, ChannelId, AuthorId, Content, MsgType, Pinned, Seq]
    ),
    Id.

-spec take_conn() -> {ok, pid()} | {error, term()}.
take_conn() ->
    case pooler:take_member(pgsql) of
        error_no_members ->
            timer:sleep(200),
            case pooler:take_member(pgsql) of
                error_no_members -> {error, no_connection};
                Conn -> {ok, Conn}
            end;
        Conn ->
            {ok, Conn}
    end.
