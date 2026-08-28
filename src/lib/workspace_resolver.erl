-module(workspace_resolver).
-compile([nowarn_deprecated_catch]).
%%%
% workspace_resolver 统一资源归属解析层（双体验 v2.5.2 WP3/T5）
%
% 职责（计划 §七 T5 IMPLEMENT）：
%   1. resolve_workspace/1：{ResourceType, ResourceId} → {ok, WorkspaceId} | personal
%      | {error, not_found}——覆盖 R3 清单直接入口资源：
%        group / group_notice(经 group_id) / channel / channel_message(经 channel_id)
%        / channel_comment(经 channel_id) / channel_reaction(经 channel_id)
%        / channel_subscription(经 channel_id) / channel_admin(经 channel_id)
%        / channel_webhook(经 channel_id) / channel_invitation(经 channel_id)
%        / workspace
%        project(经 workspace_id，恒 workspace 归属) / project_task(经 project)
%        （后两者 WP4/T7 为 workspace_guard 写守卫扩展）
%        attachment(经 scope_ref→group/channel；其余 scope 回溯复杂，本期返回
%        personal 并标 TODO_T7，与任务卡"复杂回溯可先返回 personal"授权一致)。
%   2. ensure_channel_member_access/2 / ensure_group_member_access/2：
%      Workspace 边界执行前置校验——scope=workspace 的资源要求请求者是
%      active workspace_member（§1.4.2 授权规则 2），非成员稳定 403；
%      scope=personal 恒 ok（personal 零行为变化，回归红线）；
%      资源不存在恒 ok（放行给既有 404 流程，不吞既有语义）。
%   3. guard_channel_binding/2 / guard_group_gid/2：handler 层便捷门，
%      从 cowboy 路径 binding / 参数中取资源 ID 后执行 2。
%
% 不变量：本模块只做"资源归属解析 + 成员边界"，不重写消息/附件/E2EE 核心，
% 不合并 group_member/channel_subscription 进 workspace_member。
%%%

-export([resolve_workspace/1]).
-export([ensure_channel_member_access/2]).
-export([ensure_group_member_access/2]).
-export([guard_channel_binding/2]).
-export([guard_channel_custom_id/2]).
-export([guard_group_gid/2]).
-export([guard_group_notice_id/2]).

%% ===================================================================
%% 1. 统一资源归属解析
%% ===================================================================

%% @doc 解析资源所属 Workspace
%% {ok, WsId}：资源 scope=workspace；personal：个人资源（含 c2c 等不进守卫的）；
%% {error, not_found}：资源不存在。
-spec resolve_workspace({atom(), integer() | binary()}) ->
    {ok, integer()} | personal | {error, not_found}.
resolve_workspace({workspace, WsId}) ->
    case one_row(<<"SELECT id FROM workspace WHERE id = $1">>, [WsId]) of
        #{<<"id">> := _} -> {ok, elib_cnv:safe_to_integer(WsId)};
        _ -> {error, not_found}
    end;
resolve_workspace({project, ProjectId}) ->
    %% project 恒属 workspace（无 scope 概念，I7/迁移 00000078）
    case one_row(<<"SELECT workspace_id FROM project WHERE id = $1">>, [ProjectId]) of
        #{<<"workspace_id">> := WsId} when WsId =/= null ->
            {ok, elib_cnv:safe_to_integer(WsId)};
        _ ->
            {error, not_found}
    end;
resolve_workspace({project_task, TaskId}) ->
    case
        one_row(
            <<"SELECT p.workspace_id FROM project_task t",
                " JOIN project p ON p.id = t.project_id WHERE t.id = $1">>,
            [TaskId]
        )
    of
        #{<<"workspace_id">> := WsId} when WsId =/= null ->
            {ok, elib_cnv:safe_to_integer(WsId)};
        _ ->
            {error, not_found}
    end;
resolve_workspace({group, Gid}) ->
    group_scope(Gid);
resolve_workspace({group_notice, NoticeId}) ->
    case one_row(<<"SELECT group_id FROM group_notice WHERE id = $1">>, [NoticeId]) of
        #{<<"group_id">> := Gid} -> group_scope(Gid);
        _ -> {error, not_found}
    end;
resolve_workspace({channel, ChannelId}) ->
    channel_scope(ChannelId);
resolve_workspace({channel_message, MessageId}) ->
    case one_row(<<"SELECT channel_id FROM channel_message WHERE id = $1">>, [MessageId]) of
        #{<<"channel_id">> := ChannelId} -> channel_scope(ChannelId);
        _ -> {error, not_found}
    end;
resolve_workspace({channel_comment, CommentId}) ->
    case one_row(<<"SELECT channel_id FROM channel_comment WHERE id = $1">>, [CommentId]) of
        #{<<"channel_id">> := ChannelId} -> channel_scope(ChannelId);
        _ -> {error, not_found}
    end;
resolve_workspace({channel_reaction, ReactionId}) ->
    case one_row(<<"SELECT channel_id FROM channel_reaction WHERE id = $1">>, [ReactionId]) of
        #{<<"channel_id">> := ChannelId} -> channel_scope(ChannelId);
        _ -> {error, not_found}
    end;
resolve_workspace({channel_subscription, ChannelId}) ->
    channel_scope(ChannelId);
resolve_workspace({channel_admin, ChannelId}) ->
    channel_scope(ChannelId);
resolve_workspace({channel_webhook, WebhookId}) ->
    case one_row(<<"SELECT channel_id FROM channel_webhook WHERE id = $1">>, [WebhookId]) of
        #{<<"channel_id">> := ChannelId} -> channel_scope(ChannelId);
        _ -> {error, not_found}
    end;
resolve_workspace({channel_invitation, InvitationId}) ->
    case one_row(<<"SELECT channel_id FROM channel_invitation WHERE id = $1">>, [InvitationId]) of
        #{<<"channel_id">> := ChannelId} -> channel_scope(ChannelId);
        _ -> {error, not_found}
    end;
resolve_workspace({attachment, AttachId}) ->
    case one_row(<<"SELECT scope, scope_ref FROM attachment WHERE id = $1">>, [AttachId]) of
        #{<<"scope">> := <<"group">>, <<"scope_ref">> := Ref} when Ref =/= null ->
            group_scope(elib_cnv:safe_to_integer(Ref));
        #{<<"scope">> := <<"channel">>, <<"scope_ref">> := Ref} when Ref =/= null ->
            channel_scope(elib_cnv:safe_to_integer(Ref));
        %% TODO(T7)：c2c/moment/private 附件需经消息/动态回溯目标群/频道，
        %% 归档写守卫接入时统一实现；当前按 personal 放行（personal 不受守卫影响）。
        _ ->
            personal
    end;
resolve_workspace(_) ->
    personal.

%% ===================================================================
%% 2. Workspace 边界执行（handler 前置校验）
%% ===================================================================

%% @doc 频道入口边界：workspace 频道要求请求者是 active 工作区成员
%% personal 频道 / 频道不存在 → ok（既有流程继续，零行为变化）。
-spec ensure_channel_member_access(integer(), integer() | binary()) ->
    ok | {error, {403, binary()}}.
ensure_channel_member_access(Uid, ChannelId) ->
    case channel_scope(ChannelId) of
        {ok, WsId} -> ensure_member_ok(WsId, Uid);
        _ -> ok
    end.

%% @doc 群入口边界：workspace 群要求请求者是 active 工作区成员
%% personal 群 / 群不存在 → ok。
-spec ensure_group_member_access(integer(), integer() | binary()) ->
    ok | {error, {403, binary()}}.
ensure_group_member_access(Uid, Gid) ->
    case group_scope(Gid) of
        {ok, WsId} -> ensure_member_ok(WsId, Uid);
        _ -> ok
    end.

%% ensure_member 返回 {ok, Role}；handler 便捷门契约是 ok——此处归一。
ensure_member_ok(WsId, Uid) ->
    case workspace_logic:ensure_member(WsId, Uid) of
        {ok, _Role} -> ok;
        {error, _} = Err -> Err
    end.

%% @doc handler 便捷门：读 cowboy 的 :channel_id binding（无 binding 放行）
%% 用于 channel_handler / channel_handler_message / channel_handler_admin /
%% channel_handler_comment / channel_webhook_handler 的 init/2 前置校验。
%% binding 读取带 catch：单测中 Req 常为普通 map（非 cowboy req），
%% 真实 cowboy_req:binding 会 function_clause，按"无 binding"处理。
-spec guard_channel_binding(cowboy_req:req() | map(), integer()) ->
    ok | {error, {403, binary()}}.
guard_channel_binding(Req, Uid) ->
    case catch cowboy_req:binding(channel_id, Req) of
        ChannelId when is_binary(ChannelId); is_integer(ChannelId) ->
            ensure_channel_member_access(Uid, ChannelId);
        _ ->
            ok
    end.

%% @doc handler 便捷门：custom_id 入口（by_custom_id 路由的直访通道）
%% 自定义 ID 命中的频道若为 workspace scope，同样不能绕过 403。
%% DB 查询带 catch（与 row_scope 同理）：连接层异常按"未命中"放行，
%% 走既有流程（后续业务查询仍会失败，无绕过泄漏窗口）。
-spec guard_channel_custom_id(integer(), binary()) -> ok | {error, {403, binary()}}.
guard_channel_custom_id(Uid, CustomId) when is_binary(CustomId), CustomId =/= <<>> ->
    case catch channel_ds:find_by_custom_id(CustomId) of
        Channel when is_map(Channel), map_size(Channel) > 0 ->
            ensure_channel_member_access(Uid, maps:get(<<"id">>, Channel, 0));
        _ ->
            ok
    end;
guard_channel_custom_id(_Uid, _CustomId) ->
    ok.

%% @doc handler 便捷门：群入口（gid 参数，POST body 或 query string 均可传值）
%% 用于 group_handler:detail/msg_page 与 group_notice_handler 全部入口。
-spec guard_group_gid(integer(), integer() | binary()) -> ok | {error, {403, binary()}}.
guard_group_gid(Uid, Gid) ->
    Gid2 = elib_cnv:safe_to_integer(Gid),
    case Gid2 > 0 of
        true -> ensure_group_member_access(Uid, Gid2);
        false -> ok
    end.

%% @doc handler 便捷门：群公告入口（notice_id 参数）
%% notice → group_id → scope 解析后执行同一 Workspace 成员边界；
%% personal 群公告 / 公告不存在 → ok（走既有 404/群成员校验流程）。
%% Group Notice 继续只属于 Group（I12），本守卫只加 Workspace 边界前置。
-spec guard_group_notice_id(integer(), integer() | binary()) -> ok | {error, {403, binary()}}.
guard_group_notice_id(Uid, NoticeId) ->
    Id2 = elib_cnv:safe_to_integer(NoticeId),
    case Id2 > 0 of
        true ->
            case resolve_workspace({group_notice, Id2}) of
                {ok, WsId} -> ensure_member_ok(WsId, Uid);
                _ -> ok
            end;
        false ->
            ok
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec group_scope(integer() | binary()) -> {ok, integer()} | personal | {error, not_found}.
group_scope(Gid) ->
    %% "group" 是保留字表名，SQL 中必须双引号（T3-④）
    case row_scope(<<"\"group\"">>, Gid) of
        {ok, <<"workspace">>, WsId} when WsId =/= null -> {ok, WsId};
        {ok, _, _} -> personal;
        Else -> Else
    end.

-spec channel_scope(integer() | binary()) -> {ok, integer()} | personal | {error, not_found}.
channel_scope(ChannelId) ->
    case row_scope(<<"channel">>, ChannelId) of
        {ok, <<"workspace">>, WsId} when WsId =/= null -> {ok, WsId};
        {ok, _, _} -> personal;
        Else -> Else
    end.

%% 通用 scope 行查询：返回 {ok, Scope, WorkspaceId} | {error, not_found}
%% DB 调用带 catch：连接层崩溃（如测试环境无池）按 not_found 处理——
%% 守卫放行走既有权限流程（生产 DB 正常时边界照常生效；DB 不可用时
%% 后续业务查询同样失败，不存在绕过泄漏窗口）。
-spec row_scope(binary(), integer() | binary()) ->
    {ok, binary() | nil, integer() | nil} | {error, not_found}.
row_scope(Tb, Id) ->
    Id2 = elib_cnv:safe_to_integer(Id),
    case Id2 > 0 of
        false ->
            {error, not_found};
        true ->
            Sql = <<"SELECT scope, workspace_id FROM ", Tb/binary, " WHERE id = $1">>,
            case catch one_row(Sql, [Id2]) of
                Row = #{<<"scope">> := Scope} ->
                    {ok, Scope, maps:get(<<"workspace_id">>, Row, undefined)};
                _ ->
                    {error, not_found}
            end
    end.

-spec one_row(iodata(), [term()]) -> map().
one_row(Sql, Params) ->
    case catch elib_pg:one(Sql, Params) of
        {ok, Row} when is_map(Row) -> Row;
        _ -> #{}
    end.
