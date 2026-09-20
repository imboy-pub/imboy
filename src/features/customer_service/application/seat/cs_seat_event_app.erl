%%% @doc 坐席 SSE 事件流的应用层用例（BE-S01b，contracts/sse-event-contract.json）。
%%%
%%% 职责：游标裁决 + 事件读页 + 合同信封投影。**不是**流本身——流式写出、
%%% 心跳与撤权关流在接口层（`cs_tenant_handler` 的 seat_events 分支），本模块
%%% 每次调用只返回一页确定性事实（handler 轮询复用同一用例）。
%%%
%%% 合同要点（逐字实现）：
%%%   * 信封九字段：event_id / type / organization_id / workspace_id /
%%%     resource_type / resource_id / resource_version / occurred_at / reason；
%%%   * type 恰六种：queue.changed | session.changed | message.appended |
%%%     assignment.changed | seat.changed | resync.required（resync 是合成帧，
%%%     不进事件表，handler 在开流时发一次）；
%%%   * event_id 单 (Org, Workspace) 流内单调唯一（TSID 主键 + 键集升序读页
%%%     保证——乱序不产生）；
%%%   * payload 零正文：只有资源 ID / 动作快照，不含消息正文、附件
%%%     URL/object key、token 或客户敏感资料；
%%%   * 游标语义：存在且 (Org, Workspace) 逐字相等 ⇒ 续传；存在但作用域
%%%     不符 ⇒ `{error, cross_org}`（403 面，**不静默回退**）；不存在
%%%     （缺失/超窗）⇒ resync：从当前水位继续；
%%%   * 未映射 action 的事件（内部审计动作）不投递，但游标照常推进——
%%%     流只承载客户端可见的六类变化。
-module(cs_seat_event_app).

-export([events/2, envelope/2, resync_envelope/4]).

-define(POLL_LIMIT, 200).
-define(RESOURCE_VERSION, 1).

%% ===================================================================
%% 用例入口（facade seat_events/2 的委派目标；handler 开流与轮询共用）
%% ===================================================================

%% @doc 读取一页事件 + 游标状态。
%%
%% Params：workspace_id 必填；after_id 可选（TSID integer，handler 已把
%% Last-Event-ID 头与查询参数归一为同一键——头优先）；store 可注入。
%%
%% 返回：`{ok, #{events => [envelope()], cursor => AfterId|Watermark,
%% resync_required => boolean(), resync_reason => binary()}}`。
%%   * 游标有效 ⇒ cursor = after_id，resync_required = false；
%%   * 游标超窗（86400s retention 外已被回收）⇒ cursor = 当前水位，
%%     resync_required = true，resync_reason = <<"expired">>；
%%   * 无游标首连 ⇒ 同 resync（reason = <<"unknown">>；客户端开流后先全量
%%     刷新，自水位起订阅）。
-spec events(integer(), map()) -> {ok, map()} | {error, term()}.
events(OrgId, #{workspace_id := WorkspaceId} = Params) when
    is_integer(OrgId), is_integer(WorkspaceId), is_map(Params)
->
    case cursor_scope(OrgId, WorkspaceId, Params) of
        {error, _} = Err ->
            Err;
        {ok, Cursor, ResyncRequired, ResyncReason} ->
            page(OrgId, WorkspaceId, Cursor, ResyncRequired, ResyncReason, Params)
    end;
events(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_workspace_id, maps:get(workspace_id, Params, undefined)}};
events(OrgId, _Params) ->
    {error, {invalid_organization_id, OrgId}}.

%% 游标裁决：合法续传 / 跨作用域 403 / 缺失超窗 resync（reason 透传给 handler
%% 的合成帧——超窗 = expired，首连无游标 = unknown）。
cursor_scope(OrgId, WorkspaceId, Params) ->
    case maps:get(after_id, Params, undefined) of
        undefined ->
            watermark(OrgId, WorkspaceId, Params, true, <<"unknown">>);
        AfterId when is_integer(AfterId), AfterId > 0 ->
            with_store(Params, fun(Store) ->
                case Store:fetch_event_scope(OrgId, AfterId) of
                    {ok, #{organization_id := OrgId, workspace_id := WorkspaceId}} ->
                        {ok, AfterId, false, <<"unknown">>};
                    {ok, _OtherScope} ->
                        %% 跨 Org/Workspace 游标：显式拒绝，不静默回退。
                        {error, cross_org};
                    {error, not_found} ->
                        watermark(OrgId, WorkspaceId, Params, true, <<"expired">>);
                    {error, _} = Err ->
                        Err
                end
            end);
        BadCursor ->
            {error, {invalid_after_id, BadCursor}}
    end.

watermark(OrgId, WorkspaceId, Params, ResyncRequired, Reason) ->
    with_store(Params, fun(Store) ->
        case Store:event_watermark(OrgId, WorkspaceId) of
            {ok, W} when is_integer(W), W >= 0 ->
                {ok, W, ResyncRequired, Reason};
            {error, _} = Err ->
                Err
        end
    end).

page(OrgId, WorkspaceId, Cursor, ResyncRequired, ResyncReason, Params) ->
    with_store(Params, fun(Store) ->
        case Store:list_events_page(OrgId, WorkspaceId, Cursor, ?POLL_LIMIT) of
            {error, _} = Err ->
                Err;
            {ok, Rows} ->
                Envelopes = [envelope(Row, OrgId) || Row <- Rows],
                Deliverable = [E || E <- Envelopes, E =/= dropped],
                {ok, #{
                    events => Deliverable,
                    %% 游标推进到本页最后一行（含被滤除的内部审计事件）——键集
                    %% 读页按 id 升序，页尾即「已消费」边界；停在投递帧会制造
                    %% 「整页皆未映射动作 ⇒ 游标永不前进」的活锁。未映射动作
                    %% 本就一次性跳过（不重放是意图，不是丢失）。
                    cursor => page_cursor(Cursor, Rows),
                    resync_required => ResyncRequired,
                    resync_reason => ResyncReason
                }}
        end
    end).

%% 本页尾行 id（行已按 id 升序）；空页停在原位。
page_cursor(Cursor, []) ->
    Cursor;
page_cursor(_Cursor, Rows) ->
    maps:get(id, lists:last(Rows)).

with_store(Params, Fun) ->
    cs_app_support:with_store(Params, Fun).

%% ===================================================================
%% 合同信封投影（action → type 六种；纯函数，导出供套件零 socket 断言）
%% ===================================================================

%% @doc 事件行 → 合同信封（TSID integer 形态；出站由 handler 经
%% cs_http:encode_entity 编为 TSID-string）。未映射 action 返回 `dropped`。
%%
%% 映射（六种 type 的唯一落点）：
%%   * session.opened      → queue.changed      / queue      / created
%%   * session.claimed     → assignment.changed / assignment / created
%%   * session.transferred → assignment.changed / assignment / updated
%%   * session.closed      → session.changed    / session    / updated
%%   * session.rated       → session.changed    / session    / updated
%%   * message.appended    → message.appended   / message    / created
%%   * seat.created        → seat.changed       / seat       / created
%%   * seat.resumed        → seat.changed       / seat       / created
%%   * seat.suspended      → seat.changed       / seat       / revoked
-spec envelope(map(), integer()) -> map() | dropped.
envelope(Row, OrgId) when is_map(Row) ->
    Action = action_of(Row),
    case mapping(Action) of
        dropped ->
            dropped;
        {Type, ResourceType, Reason} ->
            #{
                event_id => maps:get(id, Row),
                type => Type,
                organization_id => OrgId,
                workspace_id => maps:get(workspace_id, Row),
                resource_type => ResourceType,
                resource_id => resource_id(ResourceType, Row),
                resource_version => ?RESOURCE_VERSION,
                occurred_at => occurred_at(maps:get(created_at, Row, undefined)),
                reason => Reason
            }
    end;
envelope(_Row, _OrgId) ->
    dropped.

action_of(Row) ->
    case maps:get(action, Row, undefined) of
        Bin when is_binary(Bin) -> Bin;
        Atom when is_atom(Atom) -> atom_to_binary(Atom, utf8);
        _Other -> <<>>
    end.

mapping(<<"session.opened">>) -> {<<"queue.changed">>, <<"queue">>, <<"created">>};
mapping(<<"session.claimed">>) -> {<<"assignment.changed">>, <<"assignment">>, <<"created">>};
mapping(<<"session.transferred">>) -> {<<"assignment.changed">>, <<"assignment">>, <<"updated">>};
mapping(<<"session.closed">>) -> {<<"session.changed">>, <<"session">>, <<"updated">>};
mapping(<<"session.rated">>) -> {<<"session.changed">>, <<"session">>, <<"updated">>};
mapping(<<"message.appended">>) -> {<<"message.appended">>, <<"message">>, <<"created">>};
mapping(<<"seat.created">>) -> {<<"seat.changed">>, <<"seat">>, <<"created">>};
mapping(<<"seat.resumed">>) -> {<<"seat.changed">>, <<"seat">>, <<"created">>};
mapping(<<"seat.suspended">>) -> {<<"seat.changed">>, <<"seat">>, <<"revoked">>};
mapping(_Other) -> dropped.

%% 资源 id 取自事件行的绑定列：queue/assignment/session 用 session_id；
%% seat 用 business_identity_id；message 用 detail.message_id（资源 ID，
%% 非正文——payload_limits 合同）。
resource_id(<<"message">>, Row) ->
    case maps:get(detail, Row, #{}) of
        #{<<"message_id">> := MsgId} when is_integer(MsgId) -> MsgId;
        _ -> undefined
    end;
resource_id(<<"seat">>, Row) ->
    maps:get(business_identity_id, Row, undefined);
resource_id(_Other, Row) ->
    maps:get(session_id, Row, undefined).

%% created_at（SQL 取出恒为 Unix 秒）→ RFC3339（合同 occurred_at）。
occurred_at(Seconds) when is_integer(Seconds) ->
    elib_dt:to_rfc3339(Seconds * 1000);
occurred_at(_Other) ->
    elib_dt:to_rfc3339(os:system_time(millisecond)).

%% @doc 合成 resync.required 帧（不进事件表；handler 在开流时发一次）。
%% event_id = 续传水位（单调不回退）；reason 按 resync 成因给值
%% （游标超窗 = expired；无游标首连 = unknown）。
-spec resync_envelope(integer(), integer(), binary(), binary()) -> map().
resync_envelope(Watermark, OrgId, WorkspaceId, Reason) when is_integer(Watermark) ->
    #{
        event_id => Watermark,
        type => <<"resync.required">>,
        organization_id => OrgId,
        workspace_id => WorkspaceId,
        resource_type => <<"queue">>,
        resource_id => undefined,
        resource_version => ?RESOURCE_VERSION,
        occurred_at => elib_dt:to_rfc3339(os:system_time(millisecond)),
        reason => Reason
    };
resync_envelope(_Watermark, _OrgId, _WorkspaceId, _Reason) ->
    dropped.
