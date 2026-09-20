%%% @doc 坐席 SSE 事件流的应用层套件（BE-S01b，contracts/sse-event-contract.json）。
%%%
%%% `cs_seat_event_app` 每次调用返回一页确定性事实（流式写出/心跳/撤权关流在
%%% `cs_tenant_handler` 的 seat_events 分支，由 cs_seat_workbench_tests 的真
%%% HTTP 流式用例覆盖）。本套件用 fake store 逐条核对合同的应用侧不变量：
%%%
%%%   * 信封九字段 + TSID integer 形态；type 恰六种（action → type 映射）；
%%%   * 未映射 action（内部审计动作）不投递，但游标照常推进（页尾）；
%%%   * 游标语义：合法续传 / 跨 (Org, Workspace) 游标 = `{error, cross_org}`
%%%     （403 面，不静默回退）/ 缺失与超窗 = resync（reason: unknown|expired）；
%%%   * 键集升序读页 ⇒ 单流内 id 严格递增（乱序不产生、不重）；
%%%   * payload 零正文：信封只有资源 ID/动作快照，detail 不出站。
%%%
%%% 纪律：零 SQL、零 socket（流式线格式的套件见 cs_seat_workbench_tests）。
-module(cs_seat_event_app_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FAKE, cs_fake_store).
-define(ORG, 830000000000001).
-define(WS, 830000000000002).
-define(ORG2, 830000000000003).
-define(WS2, 830000000000004).
-define(SESSION, 830000000000005).
-define(IDENTITY, 830000000000006).
-define(MSG, 830000000000007).

seat_event_app_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() ->
    ok = ?FAKE:init(),
    {ok, fixed}.

cleanup(_) ->
    ?FAKE:destroy(),
    ok.

cases(_State) ->
    [
        {timeout, 30, fun first_connect_resyncs_from_watermark/0},
        {timeout, 30, fun valid_cursor_continues_without_resync/0},
        {timeout, 30, fun cross_org_cursor_is_rejected/0},
        {timeout, 30, fun cross_workspace_cursor_is_rejected/0},
        {timeout, 30, fun expired_cursor_resyncs_with_expired_reason/0},
        {timeout, 30, fun unmapped_actions_are_dropped_but_advance_cursor/0},
        {timeout, 30, fun pages_are_ascending_and_deduplicated/0},
        {timeout, 30, fun envelope_projection_maps_six_types/0},
        {timeout, 30, fun envelope_carries_no_payload_body/0},
        {timeout, 30, fun resync_envelope_shape/0},
        {timeout, 30, fun workspace_scope_is_strict/0},
        {timeout, 30, fun invalid_params_are_structured_rejections/0}
    ].

%% ===================================================================
%% 游标语义（cursor_rule / resync）
%% ===================================================================

first_connect_resyncs_from_watermark() ->
    ok = ?FAKE:init(),
    seed_event(?ORG, ?WS, <<"session.opened">>, #{<<"session_id">> => ?SESSION}),
    seed_event(?ORG, ?WS, <<"message.appended">>, #{<<"message_id">> => ?MSG}),
    {ok, Page} = cs_seat_event_app:events(?ORG, params(#{})),
    %% 无游标首连 = resync（reason unknown），从当前水位起订阅，不重放历史。
    ?assertEqual(true, maps:get(resync_required, Page)),
    ?assertEqual(<<"unknown">>, maps:get(resync_reason, Page)),
    ?assertEqual([], maps:get(events, Page)),
    W = watermark_of(?ORG, ?WS),
    ?assert(W > 0),
    ?assertEqual(W, maps:get(cursor, Page)).

valid_cursor_continues_without_resync() ->
    ok = ?FAKE:init(),
    Id1 = seed_event(?ORG, ?WS, <<"session.opened">>, #{<<"session_id">> => ?SESSION}),
    Id2 = seed_event(?ORG, ?WS, <<"message.appended">>, #{<<"message_id">> => ?MSG}),
    {ok, Page} = cs_seat_event_app:events(?ORG, params(#{after_id => Id1})),
    ?assertEqual(false, maps:get(resync_required, Page)),
    Events = maps:get(events, Page),
    ?assertEqual(1, length(Events)),
    [Envelope] = Events,
    ?assertEqual(Id2, maps:get(event_id, Envelope)),
    ?assertEqual(<<"message.appended">>, maps:get(type, Envelope)),
    ?assertEqual(Id2, maps:get(cursor, Page)).

cross_org_cursor_is_rejected() ->
    ok = ?FAKE:init(),
    %% 游标事件存在于他 Org（同 id 全局唯一）⇒ 作用域不符，显式 403 面。
    ForeignId = seed_event(?ORG2, ?WS2, <<"session.opened">>, #{<<"session_id">> => ?SESSION}),
    ?assertMatch(
        {error, cross_org},
        cs_seat_event_app:events(?ORG, params(#{after_id => ForeignId}))
    ),
    %% 不存在（缺失/超窗）的游标走 resync，不是 cross_org。
    ?assertMatch(
        {ok, #{resync_required := true, resync_reason := <<"expired">>}},
        cs_seat_event_app:events(?ORG, params(#{after_id => 999999999}))
    ).

cross_workspace_cursor_is_rejected() ->
    ok = ?FAKE:init(),
    OtherWs = seed_event(?ORG, ?WS2, <<"session.opened">>, #{<<"session_id">> => ?SESSION}),
    %% 同 Org 换 Workspace 的游标同样跨作用域（流作用域 = Org+Workspace）。
    ?assertMatch(
        {error, cross_org},
        cs_seat_event_app:events(?ORG, params(#{after_id => OtherWs}))
    ).

expired_cursor_resyncs_with_expired_reason() ->
    ok = ?FAKE:init(),
    Id1 = seed_event(?ORG, ?WS, <<"session.opened">>, #{<<"session_id">> => ?SESSION}),
    {ok, Page} = cs_seat_event_app:events(?ORG, params(#{after_id => Id1 - 1})),
    %% Id1-1 不存在任何行（模拟 86400s retention 外已回收）⇒ resync=expired。
    ?assertEqual(true, maps:get(resync_required, Page)),
    ?assertEqual(<<"expired">>, maps:get(resync_reason, Page)),
    W = watermark_of(?ORG, ?WS),
    ?assertEqual(W, maps:get(cursor, Page)).

%% ===================================================================
%% 投递规则（六种 type / 未映射动作 / 单调不重）
%% ===================================================================

unmapped_actions_are_dropped_but_advance_cursor() ->
    ok = ?FAKE:init(),
    %% 哨兵事件：其 id 是「合法存在的游标」（缺失游标走 resync 合同，不补历史）。
    Sentinel = seed_event(?ORG, ?WS, <<"unmapped.sentinel">>, #{}),
    _Id2 = seed_event(?ORG, ?WS, <<"widget.session_created">>, #{}),
    _Id3 = seed_event(?ORG, ?WS, <<"platform.provisioned">>, #{}),
    {ok, Page} = cs_seat_event_app:events(?ORG, params(#{after_id => Sentinel})),
    %% 未映射动作不投递（流只承载客户端可见的六类变化）……
    ?assertEqual([], maps:get(events, Page)),
    %% ……但游标推进到本页尾行（否则整页未映射动作会让游标永不前进）。
    ?assertEqual(watermark_of(?ORG, ?WS), maps:get(cursor, Page)),
    ?assertEqual(false, maps:get(resync_required, Page)).

pages_are_ascending_and_deduplicated() ->
    ok = ?FAKE:init(),
    Sentinel = seed_event(?ORG, ?WS, <<"unmapped.sentinel">>, #{}),
    Ids = [
        seed_event(?ORG, ?WS, <<"message.appended">>, #{<<"message_id">> => ?MSG + N})
     || N <- lists:seq(0, 4)
    ],
    Sorted = lists:sort(Ids),
    {ok, Page} = cs_seat_event_app:events(?ORG, params(#{after_id => Sentinel})),
    Delivered = [maps:get(event_id, E) || E <- maps:get(events, Page)],
    %% 键集升序读页：单流内 id 严格递增（乱序不产生），`id >` 游标（不重）。
    ?assertEqual(Sorted, Delivered),
    ?assertEqual(lists:last(Sorted), maps:get(cursor, Page)),
    {ok, Page2} = cs_seat_event_app:events(?ORG, params(#{after_id => lists:last(Sorted)})),
    ?assertEqual([], maps:get(events, Page2)).

%% ===================================================================
%% 信封投影（envelope 六字段合同）
%% ===================================================================

envelope_projection_maps_six_types() ->
    ok = ?FAKE:init(),
    Sentinel = seed_event(?ORG, ?WS, <<"unmapped.sentinel">>, #{}),
    Expectations = [
        {<<"session.opened">>, <<"queue.changed">>, <<"queue">>, <<"created">>},
        {<<"session.claimed">>, <<"assignment.changed">>, <<"assignment">>, <<"created">>},
        {<<"session.transferred">>, <<"assignment.changed">>, <<"assignment">>, <<"updated">>},
        {<<"session.closed">>, <<"session.changed">>, <<"session">>, <<"updated">>},
        {<<"session.rated">>, <<"session.changed">>, <<"session">>, <<"updated">>},
        {<<"seat.suspended">>, <<"seat.changed">>, <<"seat">>, <<"revoked">>}
    ],
    [seed_event(?ORG, ?WS, Action, #{<<"session_id">> => ?SESSION})
     || {Action, _, _, _} <- Expectations],
    {ok, Page} = cs_seat_event_app:events(?ORG, params(#{after_id => Sentinel})),
    Events = maps:get(events, Page),
    ?assertEqual(length(Expectations), length(Events)),
    lists:zipwith(
        fun({Action, Type, ResourceType, Reason}, E) ->
            ?assertEqual(Type, maps:get(type, E)),
            ?assertEqual(ResourceType, maps:get(resource_type, E)),
            ?assertEqual(Reason, maps:get(reason, E)),
            ok
        end,
        Expectations,
        Events
    ),
    ok.

envelope_carries_no_payload_body() ->
    ok = ?FAKE:init(),
    Sentinel = seed_event(?ORG, ?WS, <<"unmapped.sentinel">>, #{}),
    _Id =
        seed_event(?ORG, ?WS, <<"message.appended">>, #{
            <<"message_id">> => ?MSG,
            %% 注入恶意/敏感 detail 值——信封投影必须原样丢弃。
            <<"body">> => <<"机密正文"/utf8>>,
            <<"object_key">> => <<"bucket/path/secret">>,
            <<"token">> => <<"never-leak">>
        }),
    {ok, Page} = cs_seat_event_app:events(?ORG, params(#{after_id => Sentinel})),
    [E] = maps:get(events, Page),
    %% 九字段恰齐（TSID integer 形态；出站由 handler 编 TSID-string）。
    ?assertEqual(
        lists:sort([
            event_id,
            type,
            organization_id,
            workspace_id,
            resource_type,
            resource_id,
            resource_version,
            occurred_at,
            reason
        ]),
        lists:sort(maps:keys(E))
    ),
    ?assertEqual(1, maps:get(resource_version, E)),
    ?assertEqual(?ORG, maps:get(organization_id, E)),
    ?assertEqual(?WS, maps:get(workspace_id, E)),
    ?assertEqual(?MSG, maps:get(resource_id, E)),
    ?assertMatch(<<"message.appended">>, maps:get(type, E)),
    %% occurred_at = RFC3339（YYYY-MM-DDThh:mm:ssZ 形状）。
    ?assertMatch(<<_:4/binary, "-", _:2/binary, "-", _:2/binary, "T", _/binary>>,
        maps:get(occurred_at, E)),
    true.

resync_envelope_shape() ->
    Envelope = cs_seat_event_app:resync_envelope(12345, ?ORG, ?WS, <<"expired">>),
    ?assertEqual(<<"resync.required">>, maps:get(type, Envelope)),
    ?assertEqual(12345, maps:get(event_id, Envelope)),
    ?assertEqual(?ORG, maps:get(organization_id, Envelope)),
    ?assertEqual(?WS, maps:get(workspace_id, Envelope)),
    ?assertEqual(<<"expired">>, maps:get(reason, Envelope)),
    ?assertEqual(1, maps:get(resource_version, Envelope)),
    ?assert(maps:is_key(occurred_at, Envelope)).

%% ===================================================================
%% 作用域与参数形状
%% ===================================================================

workspace_scope_is_strict() ->
    ok = ?FAKE:init(),
    InScope = seed_event(?ORG, ?WS, <<"message.appended">>, #{<<"message_id">> => ?MSG}),
    %% 其他 Workspace / 其他 Org 的事件不可见（同语句绑定 (Org, Workspace)）。
    _OtherWs = seed_event(?ORG, ?WS2, <<"message.appended">>, #{<<"message_id">> => ?MSG + 1}),
    _OtherOrg = seed_event(?ORG2, ?WS, <<"message.appended">>, #{<<"message_id">> => ?MSG + 2}),
    AfterInScope = seed_event(?ORG, ?WS, <<"session.opened">>, #{<<"session_id">> => ?SESSION}),
    {ok, Page} = cs_seat_event_app:events(?ORG, params(#{after_id => InScope})),
    %% 以 InScope 为游标续传：跨 Org/Workspace 的两行被作用域过滤，只剩本流。
    Events = maps:get(events, Page),
    ?assertEqual([AfterInScope], [maps:get(event_id, E) || E <- Events]),
    ?assertEqual(AfterInScope, maps:get(cursor, Page)).

invalid_params_are_structured_rejections() ->
    ok = ?FAKE:init(),
    %% workspace 缺失 → 422 面。
    ?assertMatch(
        {error, {invalid_workspace_id, undefined}},
        cs_seat_event_app:events(?ORG, params2(#{store => ?FAKE}))
    ),
    %% 非法游标取值 → 结构化 422 面。
    ?assertMatch(
        {error, {invalid_after_id, _}},
        cs_seat_event_app:events(?ORG, params(#{after_id => -1}))
    ).

%% ===================================================================
%% 夹具
%% ===================================================================

params(Extra) ->
    params2(
        maps:merge(
            #{
                workspace_id => ?WS,
                store => ?FAKE
            },
            Extra
        )
    ).

params2(Extra) ->
    Extra.

%% 种一条事件（fake append 语义：workspace_id 与 organization_id 由行携带）。
seed_event(OrgId, WorkspaceId, Action, Detail) ->
    {ok, Id} = ?FAKE:append_event(OrgId, #{
        workspace_id => WorkspaceId,
        session_id => ?SESSION,
        business_identity_id => ?IDENTITY,
        actor_kind => <<"visitor">>,
        action => Action,
        detail => Detail,
        created_at => 1760000000
    }),
    Id.

watermark_of(OrgId, WorkspaceId) ->
    {ok, W} = ?FAKE:event_watermark(OrgId, WorkspaceId),
    W.
