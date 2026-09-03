-module(project_channel_ds).
-compile([nowarn_deprecated_catch]).
%%%
% project_channel_ds 是 project_channel domain service 缩写
% 项目↔频道关联与四类聚合领域服务（channel-firstclass W2 ZC-04）
%
% 核心职责（写路径全部单事务，with_tx）：
%   1. link：project 归属解析（404）→ 归档写守卫（980，先锁 workspace 行）→
%      频道可关联前置校验（存在/scope=workspace/同 Workspace/status=1 → 400）→
%      INSERT ... ON CONFLICT DO NOTHING 幂等（PK 冲突吸收，23503 兜底 400）→
%      仅新建行写 channel_linked 事件（同一 Conn = 同一事务，无孤儿事件）。
%   2. unlink：同守卫 → DELETE → 0 行=关联不存在 404（决策：unlink 为定向
%      删除语义取 404；link 才是幂等吸收方向）→ 1 行写 channel_unlinked 事件。
%   3. update_links：同守卫 → project.links 全量替换（DB trg_project_links_shape
%      兜底形状）→ links_updated 事件（payload 仅 count 元数据）。
%
% 读路径（聚合，全部有界 + 固定 SQL 条数，无 N+1）：
%   list_channels / pinned / related_posts / activity / resources。
%
% 事件类型契约（chk_project_event_type 14 值，DB CHECK 定死，不得自造）：
%   channel_linked / channel_unlinked / links_updated（本卡三个）。
%%%

-export([link/3]).
-export([unlink/3]).
-export([list_channels/3]).
-export([update_links/3]).
-export([pinned/3]).
-export([related_posts/1]).
-export([activity/3]).
-export([resources/1]).

-include("log.hrl").

%% 分页钳制上限（有界聚合；页大小 ≤50）
-define(MAX_PAGE_SIZE, 50).
%% Related Posts：每关联频道最近 5 条、总量上限 50 条
-define(RP_PER_CHANNEL, 5).
-define(RP_TOTAL_CAP, 50).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 关联频道到项目（幂等；仅新建行写 channel_linked 事件，同事务）
%% 返回 {ok, created} | {ok, existing} | {error, {Code, Msg}}
-spec link(integer(), integer(), integer()) ->
    {ok, created | existing} | {error, {integer(), binary()}}.
link(ActorUid, ProjectId, ChannelId) ->
    Result =
        elib_pg:with_tx(fun(Conn) ->
            WsId = ensure_project_writable_tx(Conn, ProjectId),
            Channel = ensure_channel_linkable_tx(Conn, WsId, ChannelId),
            case
                project_channel_rel_repo:insert_on_conflict_tx(
                    Conn, WsId, ProjectId, ChannelId, ActorUid
                )
            of
                {ok, 1} ->
                    %% 事件原子性：与 rel 插入同一 Conn（同事务）
                    {ok, _} = project_event_repo:insert_tx(
                        Conn,
                        event_data(
                            ProjectId,
                            ActorUid,
                            <<"channel_linked">>,
                            ChannelId,
                            channel_linked_payload(Channel)
                        )
                    ),
                    created;
                {ok, 0} ->
                    %% 幂等吸收：已存在不重复写行、不重复写事件
                    existing;
                {error, {error, _Severity, <<"23503">>, foreign_key_violation, _Msg, _Extra}} ->
                    %% DB 复合 FK 兜底（前置校验已拦截大多数场景）→ 400 语义
                    throw(
                        {abort_tx, {400, <<"频道不可关联（不存在/个人频道/跨工作区）"/utf8>>}}
                    );
                {error, Reason} ->
                    _ = ?ERROR_LOG([project_channel_link_failed, ProjectId, ChannelId, Reason]),
                    throw({abort_tx, {500, <<"关联失败，请稍后重试"/utf8>>}})
            end
        end),
    case Result of
        Status when Status =:= created; Status =:= existing ->
            _ = ?INFO_LOG([project_channel_linked, ActorUid, ProjectId, ChannelId, Status]),
            {ok, Status};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 解除关联（channel_unlinked 事件同事务）
%% 决策：解除不存在的关联 → 404（定向删除语义）；成功 → {ok, unlinked}
-spec unlink(integer(), integer(), integer()) ->
    {ok, unlinked} | {error, {integer(), binary()}}.
unlink(ActorUid, ProjectId, ChannelId) ->
    Result =
        elib_pg:with_tx(fun(Conn) ->
            _WsId = ensure_project_writable_tx(Conn, ProjectId),
            case project_channel_rel_repo:delete_tx(Conn, ProjectId, ChannelId) of
                {ok, 1} ->
                    {ok, _} = project_event_repo:insert_tx(
                        Conn,
                        event_data(
                            ProjectId,
                            ActorUid,
                            <<"channel_unlinked">>,
                            ChannelId,
                            #{<<"channel_id">> => ChannelId}
                        )
                    ),
                    unlinked;
                {ok, 0} ->
                    throw({abort_tx, {404, <<"关联不存在"/utf8>>}});
                {error, Reason} ->
                    _ = ?ERROR_LOG([project_channel_unlink_failed, ProjectId, ChannelId, Reason]),
                    throw({abort_tx, {500, <<"解除关联失败，请稍后重试"/utf8>>}})
            end
        end),
    case Result of
        unlinked ->
            _ = ?INFO_LOG([project_channel_unlinked, ActorUid, ProjectId, ChannelId]),
            {ok, unlinked};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 项目关联频道列表（JOIN channel 元数据；稳定分页；2 条 SQL）
-spec list_channels(integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
list_channels(ProjectId, Page0, Size0) ->
    {Page, Size} = clamp_page(Page0, Size0),
    project_channel_rel_repo:page_channels_by_project(ProjectId, Page, Size).

%% @doc 全量替换 project.links（logic 层已做应用层形状校验；
%% DB trg_project_links_shape 兜底；links_updated 事件同事务）
-spec update_links(integer(), integer(), [map()]) ->
    {ok, [map()]} | {error, {integer(), binary()}}.
update_links(ActorUid, ProjectId, Links) when is_list(Links) ->
    Result =
        elib_pg:with_tx(fun(Conn) ->
            _WsId = ensure_project_writable_tx(Conn, ProjectId),
            Json = jsone:encode(Links, [native_utf8]),
            Now = elib_dt:now(),
            case
                project_repo:update_fields_tx(
                    Conn, ProjectId, #{<<"links">> => Json, <<"updated_at">> => Now}
                )
            of
                {ok, 1} -> ok;
                {ok, 0} -> throw({abort_tx, {404, <<"项目不存在"/utf8>>}});
                {error, Reason} -> throw({abort_tx, {links_update_failed, Reason}})
            end,
            {ok, _} = project_event_repo:insert_tx(
                Conn,
                event_data(
                    ProjectId,
                    ActorUid,
                    <<"links_updated">>,
                    ProjectId,
                    #{<<"count">> => length(Links)}
                )
            ),
            ok
        end),
    case Result of
        ok -> {ok, Links};
        {error, Reason} -> {error, Reason}
    end;
update_links(_ActorUid, _ProjectId, _Links) ->
    {error, {400, <<"links 必须是对象数组"/utf8>>}}.

%% @doc Pinned 聚合分页（置顶消息元数据；排除公告形态；size ≤50；2 条 SQL）
-spec pinned(integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
pinned(ProjectId, Page0, Size0) ->
    {Page, Size} = clamp_page(Page0, Size0),
    project_channel_agg_repo:pinned_page(ProjectId, Page, Size).

%% @doc Related Posts 聚合（每频道最近 5 条 + 总量上限 50 的有界摘要；单条 SQL）
-spec related_posts(integer()) -> {ok, [map()]} | {error, term()}.
related_posts(ProjectId) ->
    project_channel_agg_repo:related_posts(ProjectId, ?RP_PER_CHANNEL, ?RP_TOTAL_CAP).

%% @doc Activity 聚合分页（project_event 元数据流；size ≤50；2 条 SQL）
-spec activity(integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
activity(ProjectId, Page0, Size0) ->
    {Page, Size} = clamp_page(Page0, Size0),
    project_channel_agg_repo:activity_page(ProjectId, Page, Size).

%% @doc Resources 聚合：project.links 原样返回（用户配置 url 不改写；
%% 空/解析失败 → 空数组）
-spec resources(integer()) -> {ok, [map()]} | {error, term()}.
resources(ProjectId) ->
    case project_repo:find_by_id(ProjectId, <<"links">>) of
        #{<<"links">> := Links} when is_binary(Links) ->
            case catch jsone:decode(Links, [{object_format, map}]) of
                L when is_list(L) -> {ok, L};
                _ -> {ok, []}
            end;
        #{<<"links">> := Links} when is_list(Links) ->
            {ok, Links};
        #{} ->
            {error, {404, <<"项目不存在"/utf8>>}};
        {error, Reason} ->
            {error, Reason};
        Other ->
            {error, {unexpected_project_result, Other}}
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% 事务内：project 归属解析 + 归档写守卫（返回 workspace_id；404 语义）
-spec ensure_project_writable_tx(any(), integer()) -> integer() | no_return().
ensure_project_writable_tx(Conn, ProjectId) ->
    Project = project_repo:find_tx(Conn, ProjectId, <<"id,workspace_id">>),
    case maps:get(<<"workspace_id">>, Project, undefined) of
        WsId when is_integer(WsId) ->
            ok = workspace_guard:abort_on_error(
                workspace_guard:ensure_writable_tx(Conn, {workspace, WsId})
            ),
            WsId;
        _ ->
            throw({abort_tx, {404, <<"项目不存在"/utf8>>}})
    end.

%% 事务内：频道可关联前置校验（存在 / scope=workspace / 同 Workspace /
%% status=1 正常）——DB 复合 FK 23503 的应用层等价语义（400）
-spec ensure_channel_linkable_tx(any(), integer(), integer()) -> map() | no_return().
ensure_channel_linkable_tx(Conn, WsId, ChannelId) ->
    Channel = project_channel_rel_repo:find_channel_tx(
        Conn, ChannelId, <<"id,workspace_id,scope,status,name">>
    ),
    case Channel of
        {error, Reason} ->
            throw({abort_tx, {channel_lookup_failed, Reason}});
        _ ->
            ensure_channel_linkable_row(Channel, WsId)
    end.

ensure_channel_linkable_row(Channel, WsId) ->
    case maps:get(<<"id">>, Channel, undefined) of
        undefined ->
            throw({abort_tx, {404, <<"频道不存在"/utf8>>}});
        _ ->
            Scope = maps:get(<<"scope">>, Channel, <<"personal">>),
            ChWsId = maps:get(<<"workspace_id">>, Channel, null),
            Status = maps:get(<<"status">>, Channel, 0),
            if
                Scope =/= <<"workspace">>; ChWsId =:= null ->
                    throw(
                        {abort_tx, {400, <<"个人频道不可关联到项目"/utf8>>}}
                    );
                ChWsId =/= WsId ->
                    throw(
                        {abort_tx, {400, <<"跨工作区频道不可关联"/utf8>>}}
                    );
                Status =/= 1 ->
                    throw(
                        {abort_tx, {400, <<"频道当前状态不可关联"/utf8>>}}
                    );
                true ->
                    Channel
            end
    end.

channel_linked_payload(Channel) ->
    #{
        <<"channel_id">> => maps:get(<<"id">>, Channel, 0),
        <<"channel_name">> => maps:get(<<"name">>, Channel, <<>>)
    }.

event_data(ProjectId, ActorUid, EventType, TargetId, Payload) ->
    #{
        <<"project_id">> => ProjectId,
        <<"actor_id">> => ActorUid,
        <<"event_type">> => EventType,
        <<"target_id">> => TargetId,
        <<"payload">> => jsone:encode(Payload, [native_utf8]),
        <<"created_at">> => elib_dt:now()
    }.

-spec clamp_page(integer(), integer()) -> {integer(), integer()}.
clamp_page(Page0, Size0) ->
    Page = max(1, elib_cnv:safe_to_integer(Page0)),
    Size = max(1, min(elib_cnv:safe_to_integer(Size0), ?MAX_PAGE_SIZE)),
    {Page, Size}.
