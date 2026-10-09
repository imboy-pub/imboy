-module(cs_inbox_pg_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FIX, cs_pg_test_fixture).

read_and_reply_facts_test() ->
    with_scope(fun(Scope) ->
        Session = insert_session(Scope, <<"active">>),
        First = insert_message(Scope, <<"contact">>, <<"visible">>),
        Last = insert_message(Scope, <<"contact">>, <<"visible">>),
        _Hidden = insert_message(Scope, <<"contact">>, <<"hidden">>),
        Row = only_row(page(Scope, <<"active">>, #{})),
        ?assertEqual(2, maps:get(unread_count, Row)),
        ?assertEqual(true, maps:get(needs_reply, Row)),
        ?assertEqual(Last, maps:get(last_message_id, Row)),
        insert_read_cursor(Scope, Session, identity(Scope), Last),
        Read = only_row(page(Scope, <<"active">>, #{})),
        ?assertEqual(0, maps:get(unread_count, Read)),
        ?assertEqual(true, maps:get(needs_reply, Read)),
        Reply = insert_message(Scope, <<"business_identity">>, <<"visible">>),
        Replied = only_row(page(Scope, <<"active">>, #{})),
        ?assertEqual(false, maps:get(needs_reply, Replied)),
        ?assertEqual(Reply, maps:get(last_message_id, Replied)),
        _FollowUp = insert_message(Scope, <<"contact">>, <<"visible">>),
        ?assertEqual(true, maps:get(needs_reply, only_row(page(Scope, <<"active">>, #{})))),
        ok = ?FIX:exec(
            <<"UPDATE customer_service_session SET status='closed',closed_at=CURRENT_TIMESTAMP WHERE organization_id=$1 AND id=$2">>,
            [org(Scope), Session]
        ),
        ?assertEqual(false, maps:get(needs_reply, only_row(page(Scope, <<"closed">>, #{})))),
        ?assert(Last > First)
    end).

%% CSB-02R：summary_match_ids 面（阶段 B SQL 注入）。$9 是 filtered CTE 的
%% OR 并集（注入命中 id ∪ 服务端 search 命中），不是收窄——空 search 时
%% filtered 恒等于全 scoped 集。命中集 = 计数集 = 分页集由构造保证（counts
%% 与 page 读同一 filtered）；键集分页在并集内成立；空集恒不改变形状；非法
%% 项 fail-closed。并存非 closed 会话须各自持独立 conversation
%% （uq_csss_org_conv_open）。
summary_match_ids_face_test() ->
    with_scope(fun(Scope) ->
        ConvA = maps:get(conversation_id, Scope),
        ConvB = insert_conversation(Scope),
        SessionA = insert_session_conv(Scope, <<"active">>, ConvA),
        _MsgA = insert_message_conv(Scope, <<"contact">>, <<"visible">>, ConvA),
        SessionB = insert_session_conv(Scope, <<"active">>, ConvB),
        _MsgB = insert_message_conv(Scope, <<"contact">>, <<"visible">>, ConvB),
        ?assert(SessionB > SessionA),
        %% OR 并集：search 命中 A（id 串），注入更大 id 的 B → filtered={A,B}。
        Union = page(
            Scope,
            <<"active">>,
            #{
                search => integer_to_binary(SessionA),
                summary_match_ids => [SessionB]
            }
        ),
        ?assertEqual(2, maps:get(total, Union)),
        ?assertEqual(
            [SessionA, SessionB],
            lists:sort([maps:get(id, R) || R <- maps:get(rows, Union)])
        ),
        ?assertEqual(
            #{queued => 0, active => 2, closed => 0},
            maps:get(total_by_status, Union)
        ),
        %% 注入 id 与 search 命中重叠 → 同一行只计一次。
        Overlap = page(
            Scope,
            <<"active">>,
            #{
                search => integer_to_binary(SessionA),
                summary_match_ids => [SessionA]
            }
        ),
        ?assertEqual(1, maps:get(total, Overlap)),
        ?assertEqual([SessionA], [maps:get(id, R) || R <- maps:get(rows, Overlap)]),
        %% 不匹配任何行的 search 下，集合恰为注入 id（生产阶段 B 调用形状）。
        OnlyInjected = page(
            Scope,
            <<"active">>,
            #{
                search => <<"zzz-no-such-summary-token">>,
                summary_match_ids => [SessionA]
            }
        ),
        ?assertEqual(1, maps:get(total, OnlyInjected)),
        ?assertEqual(
            [SessionA], [maps:get(id, R) || R <- maps:get(rows, OnlyInjected)]
        ),
        %% 空集恒不改变形状（空 search → 全 scoped 集）。
        PageAll = page(Scope, <<"active">>, #{summary_match_ids => []}),
        ?assertEqual(2, maps:get(total, PageAll)),
        %% 键集分页在并集内成立：limit=1 取到较大 id 的 B，游标 B 下一页取 A。
        {ok, FirstPage} = cs_pg_store:seat_session_page(
            org(Scope),
            <<"active">>,
            0,
            1,
            ws(Scope),
            filters(Scope, #{
                search => integer_to_binary(SessionA),
                summary_match_ids => [SessionB]
            })
        ),
        ?assertEqual([SessionB], [maps:get(id, R) || R <- maps:get(rows, FirstPage)]),
        ?assertEqual(2, maps:get(total, FirstPage)),
        Cursor = maps:get(id, only_row(FirstPage)),
        {ok, SecondPage} = cs_pg_store:seat_session_page(
            org(Scope),
            <<"active">>,
            Cursor,
            20,
            ws(Scope),
            filters(Scope, #{
                search => integer_to_binary(SessionA),
                summary_match_ids => [SessionB]
            })
        ),
        ?assertEqual([SessionA], [maps:get(id, R) || R <- maps:get(rows, SecondPage)]),
        ?assertEqual(2, maps:get(total, SecondPage)),
        %% 非法项 fail-closed（不静默丢弃）。
        ?assertMatch(
            {error, {invalid_param_value, summary_match_ids}},
            cs_pg_store:seat_session_page(
                org(Scope),
                <<"active">>,
                0,
                20,
                ws(Scope),
                filters(Scope, #{summary_match_ids => [bad]})
            )
        )
    end).

%% CX02-A03：锚点面真库验收——last_message_id 由可见消息 max(id) 派生
%% （隐藏消息不抬锚点；无可见消息的会话过滤；closed 会话仍参与；org/workspace
%% 钉死；mine=true 按持有 identity 过滤）。
search_anchors_derivation_test() ->
    with_scope(fun(Scope) ->
        ConvA = maps:get(conversation_id, Scope),
        SessionA = insert_session_conv(Scope, <<"active">>, ConvA),
        _First = insert_message_conv(Scope, <<"contact">>, <<"visible">>, ConvA),
        Last = insert_message_conv(Scope, <<"contact">>, <<"visible">>, ConvA),
        Hidden = insert_message_conv(Scope, <<"contact">>, <<"hidden">>, ConvA),
        ?assert(Hidden > Last),
        ConvB = insert_conversation(Scope),
        SessionB = insert_session_conv(Scope, <<"active">>, ConvB),
        _MsgB = insert_message_conv(Scope, <<"business_identity">>, <<"visible">>, ConvB),
        ConvC = insert_conversation(Scope),
        SessionC = insert_session_conv(Scope, <<"closed">>, ConvC),
        _MsgC = insert_message_conv(Scope, <<"contact">>, <<"visible">>, ConvC),
        ConvD = insert_conversation(Scope),
        _NoMsgSession = insert_session_conv(Scope, <<"active">>, ConvD),
        ConvE = insert_conversation(Scope),
        _HiddenOnlySession = insert_session_conv(Scope, <<"active">>, ConvE),
        _HiddenOnly = insert_message_conv(Scope, <<"contact">>, <<"hidden">>, ConvE),
        {ok, Anchors} = cs_pg_store:seat_session_search_anchors(
            org(Scope),
            ws(Scope),
            #{business_identity_id => identity(Scope), mine => false}
        ),
        ById = maps:from_list([{maps:get(id, A), A} || A <- Anchors]),
        ?assertEqual(
            [SessionA, SessionB, SessionC], lists:sort(maps:keys(ById))
        ),
        AnchorA = maps:get(SessionA, ById),
        ?assertEqual(Last, maps:get(last_message_id, AnchorA)),
        ?assertEqual(ConvA, maps:get(conversation_id, AnchorA)),
        ?assertEqual(org(Scope), maps:get(organization_id, AnchorA)),
        ?assertEqual(ws(Scope), maps:get(workspace_id, AnchorA)),
        ?assertEqual(
            ConvB, maps:get(conversation_id, maps:get(SessionB, ById))
        ),
        %% mine=true + 非持有 identity → 空（$3/$4 绑定生效）。
        ?assertEqual(
            {ok, []},
            cs_pg_store:seat_session_search_anchors(
                org(Scope),
                ws(Scope),
                #{
                    business_identity_id => maps:get(sales_identity_id, Scope),
                    mine => true
                }
            )
        ),
        %% 跨 org 不可见。
        ?assertEqual(
            {ok, []},
            cs_pg_store:seat_session_search_anchors(
                maps:get(other_org_id, Scope),
                ws(Scope),
                #{business_identity_id => identity(Scope), mine => false}
            )
        )
    end).

%% CX02-A03+A04 组合：锚点（阶段 A）→（装配层摘要匹配的命中 id 集）→ $9
%% 注入（阶段 B）。装配层的解密/截断/大小写折叠匹配由契约测试（meck
%% facade）覆盖，此处以种子事实推定命中集，验收两面在真库上的衔接：
%% 非空 search + 注入命中集（生产调用形状）下 命中集 = 计数集 = 分页集。
summary_search_two_phase_real_test() ->
    with_scope(fun(Scope) ->
        ConvHit = maps:get(conversation_id, Scope),
        HitSession = insert_session_conv(Scope, <<"active">>, ConvHit),
        _HitMsg = insert_message_conv(Scope, <<"contact">>, <<"visible">>, ConvHit),
        ConvOther = insert_conversation(Scope),
        OtherSession = insert_session_conv(Scope, <<"active">>, ConvOther),
        _OtherMsg = insert_message_conv(Scope, <<"contact">>, <<"visible">>, ConvOther),
        ConvNoMsg = insert_conversation(Scope),
        _NoMsgSession = insert_session_conv(Scope, <<"active">>, ConvNoMsg),
        {ok, Anchors} = cs_pg_store:seat_session_search_anchors(
            org(Scope),
            ws(Scope),
            #{business_identity_id => identity(Scope), mine => false}
        ),
        ?assertEqual(
            lists:sort([HitSession, OtherSession]),
            lists:sort([maps:get(id, A) || A <- Anchors])
        ),
        Garbage = <<"zzz-no-such-summary-token">>,
        Hit = page(
            Scope,
            <<"active">>,
            #{search => Garbage, summary_match_ids => [HitSession]}
        ),
        ?assertEqual(1, maps:get(total, Hit)),
        ?assertEqual([HitSession], [maps:get(id, R) || R <- maps:get(rows, Hit)]),
        ?assertEqual(
            #{queued => 0, active => 1, closed => 0}, maps:get(total_by_status, Hit)
        ),
        %% 零命中：过滤形状不变 → 空。
        Zero = page(
            Scope, <<"active">>, #{search => Garbage, summary_match_ids => []}
        ),
        ?assertEqual(0, maps:get(total, Zero)),
        ?assertEqual([], maps:get(rows, Zero))
    end).

server_search_and_cursor_test() ->
    with_scope(fun(Scope) ->
        Sessions = [insert_session(Scope, <<"closed">>) || _Index <- lists:seq(1, 7)],
        [Oldest | _Rest] = Sessions,
        {ok, First} = cs_pg_store:seat_session_page(
            org(Scope),
            <<"closed">>,
            0,
            2,
            ws(Scope),
            filters(Scope, #{})
        ),
        ?assertEqual(7, maps:get(total, First)),
        FirstRows = maps:get(rows, First),
        Cursor = maps:get(id, lists:last(FirstRows)),
        {ok, Second} = cs_pg_store:seat_session_page(
            org(Scope),
            <<"closed">>,
            Cursor,
            2,
            ws(Scope),
            filters(Scope, #{})
        ),
        ?assertEqual([], [
            maps:get(id, Row)
         || Row <- FirstRows,
            lists:any(
                fun(Next) -> maps:get(id, Next) =:= maps:get(id, Row) end,
                maps:get(rows, Second)
            )
        ]),
        Match = page(Scope, <<"closed">>, #{search => integer_to_binary(Oldest)}),
        ?assertEqual(Oldest, maps:get(id, only_row(Match))),
        ?assertEqual(1, maps:get(total, Match)),
        ?assertEqual(false, maps:get(needs_reply, only_row(Match))),
        ?assertEqual(
            0, maps:get(total, page(Scope, <<"closed">>, #{search => <<"%' OR true --">>}))
        )
    end).

mine_and_scope_test() ->
    with_scope(fun(Scope) ->
        Session = insert_session(Scope, <<"active">>),
        ?assertEqual(Session, maps:get(id, only_row(page(Scope, <<"active">>, #{mine => true})))),
        WrongIdentity = filters(Scope, #{
            mine => true, business_identity_id => maps:get(sales_identity_id, Scope)
        }),
        {ok, OtherSeat} = cs_pg_store:seat_session_page(
            org(Scope),
            <<"active">>,
            0,
            20,
            ws(Scope),
            WrongIdentity
        ),
        ?assertEqual([], maps:get(rows, OtherSeat)),
        ?assertEqual(0, maps:get(total, OtherSeat)),
        {ok, OtherWorkspace} = cs_pg_store:seat_session_page(
            org(Scope),
            <<"active">>,
            0,
            20,
            maps:get(other_workspace_id, Scope),
            filters(Scope, #{})
        ),
        ?assertEqual([], maps:get(rows, OtherWorkspace)),
        {ok, OtherOrg} = cs_pg_store:seat_session_page(
            maps:get(other_org_id, Scope),
            <<"active">>,
            0,
            20,
            ws(Scope),
            filters(Scope, #{search => integer_to_binary(Session)})
        ),
        ?assertEqual([], maps:get(rows, OtherOrg))
    end).

read_cursor_is_identity_local_test() ->
    with_scope(fun(Scope) ->
        Session = insert_session(Scope, <<"active">>),
        First = insert_message(Scope, <<"contact">>, <<"visible">>),
        Last = insert_message(Scope, <<"contact">>, <<"visible">>),
        OtherIdentity = insert_other_seat(Scope),
        Unread = only_row(page(Scope, <<"active">>, #{business_identity_id => OtherIdentity})),
        ?assertEqual(2, maps:get(unread_count, Unread)),
        insert_read_cursor(Scope, Session, OtherIdentity, First),
        insert_read_cursor(Scope, Session, identity(Scope), Last),
        ?assertEqual(0, maps:get(unread_count, only_row(page(Scope, <<"active">>, #{})))),
        Other = only_row(
            page(
                Scope,
                <<"active">>,
                #{business_identity_id => OtherIdentity}
            )
        ),
        ?assertEqual(1, maps:get(unread_count, Other)),
        ?assertEqual(
            First,
            ?FIX:scalar(
                <<
                    "SELECT last_read_message_id FROM customer_service_read_cursor"
                    " WHERE organization_id=$1 AND session_id=$2 AND business_identity_id=$3"
                >>,
                [org(Scope), Session, OtherIdentity],
                0
            )
        ),
        ?assertEqual(true, maps:get(needs_reply, Other)),
        ?assertEqual(Last, maps:get(last_message_id, Other))
    end).

hidden_business_reply_does_not_clear_waiting_test() ->
    with_scope(fun(Scope) ->
        _Session = insert_session(Scope, <<"active">>),
        Contact = insert_message(Scope, <<"contact">>, <<"visible">>),
        HiddenReply = insert_message(Scope, <<"business_identity">>, <<"hidden">>),
        ?assert(HiddenReply > Contact),
        Waiting = only_row(page(Scope, <<"active">>, #{})),
        ?assertEqual(true, maps:get(needs_reply, Waiting)),
        ?assertEqual(1, maps:get(unread_count, Waiting)),
        ?assertEqual(Contact, maps:get(last_message_id, Waiting)),
        Reply = insert_message(Scope, <<"business_identity">>, <<"visible">>),
        Replied = only_row(page(Scope, <<"active">>, #{})),
        ?assertEqual(false, maps:get(needs_reply, Replied)),
        ?assertEqual(Reply, maps:get(last_message_id, Replied)),
        ?assertEqual(1, maps:get(unread_count, Replied))
    end).

masked_search_does_not_expose_name_test() ->
    with_scope(fun(Scope) ->
        _Session = insert_session(Scope, <<"queued">>),
        ok = ?FIX:exec(
            <<"UPDATE enterprise_contact SET display_name=$1 WHERE organization_id=$2 AND id=$3">>,
            [unicode:characters_to_binary("王秘密明"), org(Scope), maps:get(contact_id, Scope)]
        ),
        ?assertEqual(
            0,
            maps:get(
                total, page(Scope, <<"queued">>, #{search => unicode:characters_to_binary("秘密")})
            )
        ),
        ?assertEqual(
            1,
            maps:get(
                total, page(Scope, <<"queued">>, #{search => unicode:characters_to_binary("王***明")})
            )
        )
    end).

%% CX02-A03+A04 跨页组合验收（真库，>两页 fixture）：摘要搜索命中「非已
%% 加载」深处会话；scope 全集键集走查不重不漏、严格降序、计数逐页与服务端
%% 事实一致；scope 钉死（workspace/跨 org 注入不泄漏）；等待/最近消息/
%% 接待人字段与库中事实一致（来源真实）；重复认领二次 conflict 且刷新页
%% 归属真实（「409 刷新归属不盲重写」的后端事实腿）。
cx02_a03_a04_cross_page_test() ->
    with_scope_opts(#{max_concurrent => 16}, fun(Scope) ->
        %% 10 会话：7 active + 2 queued + 1 closed（limit=2 → active 五页）。
        ConvDeep = insert_conversation(Scope),
        Deep = insert_session_conv(Scope, <<"active">>, ConvDeep),
        _MsgDeep = insert_message_conv(Scope, <<"contact">>, <<"visible">>, ConvDeep),
        ConvMid = insert_conversation(Scope),
        Mid = insert_session_conv(Scope, <<"active">>, ConvMid),
        _MsgMid = insert_message_conv(Scope, <<"contact">>, <<"visible">>, ConvMid),
        Younger = [
            begin
                C = insert_conversation(Scope),
                insert_session_conv(Scope, <<"active">>, C)
            end
         || _ <- lists:seq(1, 5)
        ],
        ConvQ = insert_conversation(Scope),
        Queued = insert_session_conv(Scope, <<"queued">>, ConvQ),
        MsgQ = insert_message_conv(Scope, <<"contact">>, <<"visible">>, ConvQ),
        ConvQ2 = insert_conversation(Scope),
        _Queued2 = insert_session_conv(Scope, <<"queued">>, ConvQ2),
        ConvC = insert_conversation(Scope),
        _Closed = insert_session_conv(Scope, <<"closed">>, ConvC),
        ?assert(Deep < Mid),
        Actives = lists:sort(fun(A, B) -> A > B end, [Deep, Mid] ++ Younger),

        %% CX02-A03：scope 全集走查（limit=2 → 五页）不重不漏、严格降序、
        %% 计数逐页一致（total=7、queued=2/active=7/closed=1 为服务端事实）。
        Walk = walk_pages(Scope, <<"active">>, 2, #{}),
        Ids = Walk,
        ?assertEqual(Actives, Ids),
        ?assertEqual(lists:usort(Ids), lists:reverse(Ids)),
        %% 命中目标在前两「已加载页」（前 4 行）之外——深处会话。
        ?assertNot(lists:member(Deep, lists:sublist(Ids, 4))),
        ?assertNot(lists:member(Mid, lists:sublist(Ids, 4))),
        ?assertEqual(Deep, lists:last(Ids)),

        %% 摘要搜索命中跨页集合：生产阶段 B 形状（非空 search + 注入命中
        %% 集），limit=1 键集走查两命中各出现恰好一次、降序。
        Garbage = <<"zzz-no-such-summary-token">>,
        HitWalk = walk_pages(
            Scope,
            <<"active">>,
            1,
            #{search => Garbage, summary_match_ids => [Deep, Mid]}
        ),
        ?assertEqual([Mid, Deep], HitWalk),

        %% scope 钉死：其他 workspace 无泄漏；跨 org 注入命中 id 也不泄漏。
        {ok, XWs} = cs_pg_store:seat_session_page(
            org(Scope),
            <<"active">>,
            0,
            5,
            maps:get(other_workspace_id, Scope),
            filters(Scope, #{search => Garbage, summary_match_ids => [Deep]})
        ),
        ?assertEqual(0, maps:get(total, XWs)),
        {ok, XOrg} = cs_pg_store:seat_session_page(
            maps:get(other_org_id, Scope),
            <<"active">>,
            0,
            5,
            ws(Scope),
            filters(Scope, #{search => Garbage, summary_match_ids => [Deep, Mid]})
        ),
        ?assertEqual(0, maps:get(total, XOrg)),
        %% 非持有 identity 的 mine=true → 0；走查后 cursor=0 重查仍全集
        %% （服务端无游标残留状态）。
        ?assertEqual(
            0,
            maps:get(
                total,
                page(Scope, <<"active">>, #{
                    mine => true,
                    business_identity_id => maps:get(sales_identity_id, Scope)
                })
            )
        ),
        ?assertEqual(7, maps:get(total, page(Scope, <<"active">>, #{}))),

        %% CX02-A04 等待事实（认领前，来源真实）：queued 行的等待时间戳、
        %% 未读/待回复、最近消息均来自库中事实；无接待人。
        QueuedRow = only_row(
            page(Scope, <<"queued">>, #{search => integer_to_binary(Queued)})
        ),
        ?assertEqual(Queued, maps:get(id, QueuedRow)),
        ?assertEqual(undefined, maps:get(business_identity_id, QueuedRow)),
        ?assert(maps:get(queued_at, QueuedRow) > 1600000000),
        ?assertEqual(1, maps:get(unread_count, QueuedRow)),
        ?assertEqual(true, maps:get(needs_reply, QueuedRow)),
        ?assertEqual(MsgQ, maps:get(last_message_id, QueuedRow)),
        ?assertEqual(<<"contact">>, maps:get(last_message_sender_type, QueuedRow)),

        %% 真实认领 → 重复认领二次 conflict（409 后端腿）→ 刷新页归属真实。
        {ok, _} =
            cs_session_app:claim(org(Scope), #{
                workspace_id => ws(Scope),
                session_id => Queued,
                business_identity_id => identity(Scope),
                expected_version => 1,
                at => 1700000100
            }),
        %% 重复认领：409 后端腿返回结构化冲突，自带权威当前态
        %% （actual_status/actual_version 即「刷新归属」的事实来源）。
        ?assertMatch(
            {error, {cas_mismatch, #{actual_status := active, actual_version := 2}}},
            cs_session_app:claim(org(Scope), #{
                workspace_id => ws(Scope),
                session_id => Queued,
                business_identity_id => identity(Scope),
                expected_version => 1,
                at => 1700000200
            })
        ),
        Refresh = only_row(
            page(Scope, <<"active">>, #{search => integer_to_binary(Queued)})
        ),
        ?assertEqual(identity(Scope), maps:get(business_identity_id, Refresh)),
        ?assertEqual(1700000100, maps:get(claimed_at, Refresh))
    end).

with_scope(Check) ->
    with_scope_opts(#{}, Check).

with_scope_opts(Opts, Check) ->
    %% 真 PG 套件依赖 pgsql 连接池；单模块 t= 运行时没有其他套件顺带启动
    %% app，这里幂等起全量 imboy（与 cs_read_cursor_pg_tests 的 setup 同源；
    %% app 级服务按 eunit_runner 惯例不在测试侧停止）。
    case eunit_runner:eunit_setup() of
        {app_not_started, _} -> erlang:error(imboy_app_not_started);
        _ -> ok
    end,
    Scope = ?FIX:new_scope(Opts),
    try
        Check(Scope)
    after
        ?FIX:cleanup(Scope)
    end.

%% 键集整表走查：逐页断言 total 与 total_by_status 不随游标漂移（counts 与
%% page 读同一 filtered CTE），页间严格降序（无重复/遗漏的排序侧证）；返回
%% 全部行 id。
walk_pages(Scope, Status, Limit, Extra) ->
    {ok, First} = cs_pg_store:seat_session_page(
        org(Scope), Status, 0, Limit, ws(Scope), filters(Scope, Extra)
    ),
    walk_pages(
        Scope,
        Status,
        Limit,
        Extra,
        maps:get(total, First),
        maps:get(total_by_status, First),
        undefined,
        [],
        0
    ).

walk_pages(_Scope, _Status, _Limit, _Extra, _Total, _TBS, _Prev, _Acc, 30) ->
    erlang:error(walk_page_limit_exceeded);
walk_pages(Scope, Status, Limit, Extra, Total, TBS, Prev, Acc, N) ->
    Cursor =
        case Acc of
            [] -> 0;
            _ -> lists:last(Acc)
        end,
    {ok, Page} = cs_pg_store:seat_session_page(
        org(Scope), Status, Cursor, Limit, ws(Scope), filters(Scope, Extra)
    ),
    ?assertEqual(Total, maps:get(total, Page)),
    ?assertEqual(TBS, maps:get(total_by_status, Page)),
    Ids = [maps:get(id, R) || R <- maps:get(rows, Page)],
    case Prev of
        undefined -> ok;
        P -> ?assert(lists:all(fun(I) -> I < P end, Ids))
    end,
    case Ids =:= [] orelse length(Ids) < Limit of
        true ->
            Acc ++ Ids;
        false ->
            walk_pages(
                Scope,
                Status,
                Limit,
                Extra,
                Total,
                TBS,
                lists:last(Ids),
                Acc ++ Ids,
                N + 1
            )
    end.

page(Scope, Status, Extra) ->
    {ok, Page} = cs_pg_store:seat_session_page(
        org(Scope), Status, 0, 20, ws(Scope), filters(Scope, Extra)
    ),
    Page.

filters(Scope, Extra) ->
    maps:merge(#{business_identity_id => identity(Scope), mine => false, search => <<>>}, Extra).

only_row(#{rows := [Row]}) -> Row.
org(Scope) -> maps:get(org_id, Scope).
ws(Scope) -> maps:get(workspace_id, Scope).
identity(Scope) -> maps:get(service_identity_id, Scope).

insert_other_seat(Scope) ->
    Identity = ?FIX:id(),
    Owner = maps:get(owner_user_id, Scope),
    ok = ?FIX:exec(
        <<
            "INSERT INTO organization_business_identity"
            " (id,organization_id,function_key,display_name,status,version,created_by_user_id)"
            " VALUES ($1,$2,'customer_service',$3,'active',1,$4)"
        >>,
        [Identity, org(Scope), integer_to_binary(Identity), Owner]
    ),
    ok = ?FIX:exec(
        <<
            "INSERT INTO customer_service_seat"
            " (organization_id,business_identity_id,function_key,enabled,max_concurrent,created_by_user_id)"
            " VALUES ($1,$2,'customer_service',true,1,$3)"
        >>,
        [org(Scope), Identity, Owner]
    ),
    Identity.

insert_read_cursor(Scope, Session, Identity, LastMessage) ->
    ok = ?FIX:exec(
        <<
            "INSERT INTO customer_service_read_cursor"
            " (id,organization_id,workspace_id,session_id,business_identity_id,last_read_message_id)"
            " VALUES ($1,$2,$3,$4,$5,$6)"
        >>,
        [?FIX:id(), org(Scope), ws(Scope), Session, Identity, LastMessage]
    ).

insert_session(Scope, Status) ->
    insert_session_conv(Scope, Status, maps:get(conversation_id, Scope)).

%% 并存的非 closed 会话各自需要独立 conversation（uq_csss_org_conv_open：
%% 同 org+conversation 唯一，WHERE status <> 'closed'）。
insert_conversation(Scope) ->
    Conv = ?FIX:id(),
    ok = ?FIX:exec(
        <<
            "INSERT INTO enterprise_conversation"
            " (id,organization_id,workspace_id,contact_id,business_identity_id,status,version,"
            "  notice_version,consent_at,consent_subject,consent_evidence_kind)"
            " VALUES ($1,$2,$3,$4,$5,'active',1,'cs01-notice-v1',CURRENT_TIMESTAMP,$6,'synthetic')"
        >>,
        [
            Conv,
            org(Scope),
            ws(Scope),
            maps:get(contact_id, Scope),
            maps:get(service_identity_id, Scope),
            <<"cs01-consent-", (integer_to_binary(Conv))/binary>>
        ]
    ),
    Conv.

insert_session_conv(Scope, Status, ConversationId) ->
    Session = ?FIX:id(),
    Identity =
        case Status of
            <<"queued">> -> null;
            _Other -> identity(Scope)
        end,
    ok = ?FIX:exec(
        <<
            "INSERT INTO customer_service_session"
            " (id,organization_id,workspace_id,contact_id,conversation_id,business_identity_id,status,closed_at)"
            " VALUES ($1,$2,$3,$4,$5,$6,$7,CASE WHEN $7='closed' THEN CURRENT_TIMESTAMP END)"
        >>,
        [
            Session,
            org(Scope),
            ws(Scope),
            maps:get(contact_id, Scope),
            ConversationId,
            Identity,
            Status
        ]
    ),
    Session.

insert_message(Scope, Sender, Visibility) ->
    insert_message_conv(
        Scope, Sender, Visibility, maps:get(conversation_id, Scope)
    ).

insert_message_conv(Scope, Sender, Visibility, ConversationId) ->
    Message = ?FIX:id(),
    {Contact, Identity, Actor} =
        case Sender of
            <<"contact">> -> {maps:get(contact_id, Scope), null, null};
            <<"business_identity">> -> {null, identity(Scope), maps:get(actor_user_id, Scope)}
        end,
    ok = ?FIX:exec(
        <<
            "INSERT INTO enterprise_message"
            " (id,organization_id,workspace_id,conversation_id,sender_type,sender_contact_id,"
            " sender_business_identity_id,actor_user_id,client_msg_id,retention_days,retain_until,visibility)"
            " VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,1095,CURRENT_TIMESTAMP + interval '1095 days',$10)"
        >>,
        [
            Message,
            org(Scope),
            ws(Scope),
            ConversationId,
            Sender,
            Contact,
            Identity,
            Actor,
            integer_to_binary(Message),
            Visibility
        ]
    ),
    Message.
