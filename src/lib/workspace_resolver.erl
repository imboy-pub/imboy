-module(workspace_resolver).
-compile([nowarn_deprecated_catch]).
%%%
% workspace_resolver 统一资源归属解析层（双体验 v2.5.2 WP3/T5）
%
% 职责（计划 §七 T5 IMPLEMENT）：
%   1. resolve_workspace/1：{ResourceType, ResourceId} → {ok, WorkspaceId} | personal
%      | {error, not_found} | {error, {db_error, Reason}}
%      | {error, {unsupported_resource, _}} | {error, {unsupported_scope, _}}
%      ——覆盖 R3 清单直接入口资源：
%        group / group_notice(经 group_id) / channel / channel_message(经 channel_id)
%        / channel_comment(经 channel_id) / channel_reaction(经 channel_id)
%        / channel_subscription(经 channel_id) / channel_admin(经 channel_id)
%        / channel_webhook(经 channel_id) / channel_invitation(经 channel_id)
%        / workspace
%        project(经 workspace_id，恒 workspace 归属) / project_task(经 project)
%        attachment(见下方"附件归属最小闭环"）
%   2. ensure_channel_member_access/2 / ensure_group_member_access/2：
%      Workspace 边界执行前置校验——scope=workspace 的资源要求请求者是
%      active workspace_member（§1.4.2 授权规则 2），非成员稳定 403；
%      scope=personal 恒 ok（personal 零行为变化，回归红线）；
%      资源不存在恒 ok（放行给既有 404 流程，不吞既有语义）。
%   3. guard_channel_binding/2 / guard_group_gid/2 等：handler 层便捷门，
%      从 cowboy 路径 binding / 参数中取资源 ID 后执行 2。
%
% SEC-03 fail-closed 收口（不变量，全模块强制）：
%   * DB 异常（连接池不可用/查询失败/驱动崩溃）与"资源不存在"严格三态区分，
%     绝不把 {error, db_error} 归一成 not_found（elib_pg:one/2 无行时返回
%     {ok, #{}（默认值），错误才是 {error, Reason}——one_row/2 按此归一）。
%   * 归属解析失败（db_error / unsupported_resource / unsupported_scope）
%     在所有 ensure_*/guard_* 门上返回 {error, {503, Msg}}（服务不可用），
%     绝不 ok 放行——DB 故障不得成为越权窗口。
%   * 未知资源类型 / 附件 scope 非法：显式错误，绝不默认 personal
%     （不发明默认归属，任务卡 Stop 条款）。
%
% 附件归属最小闭环（SEC-03）：
%   attachment.scope 枚举 public|private|c2c|group|channel|moment（迁移
%   00000013），各 scope 的归属链以 attach_logic:authorize/3（统一读鉴权，
%   docs/architecture/resource-access-control.md）为事实源：
%     group / channel   → scope_ref 回溯 group/channel 行（本模块 SQL），
%                          归属解析与读写守卫已实现（写侧另见 attach_logic
%                          :attach_scope_target/2）；
%     c2c               → scope_ref=conv_key，attach_logic:authorize(<<"c2c">>)
%                          经 conv_key_vo:c2c_members/1 判两会话方——纯用户域
%                          会话，schema 无任何 workspace 绑定 → personal（可证）；
%     moment            → scope_ref=moment_id，attach_logic:authorize(<<"moment">)
%                          经 moment_ds ACL；moment_post 表（迁移 00000004）仅
%                          author_uid/visibility，无 workspace 列 → personal（可证）；
%     private           → 仅 creator_user_id 可读（authorize(<<"private">>)）
%                          → personal（可证）；
%     public            → 全员可读（authorize(<<"public">>) → true）
%                          → personal（可证，非 workspace 资源）；
%     枚举外 scope 值 / group|channel 行 scope_ref 缺失 → {error,{unsupported_scope,_}}
%     （脏数据显式拒绝，不发明默认归属）；附件行不存在 → {error, not_found}。
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

-include("error_code.hrl").
-include("log.hrl").

%% ===================================================================
%% 1. 统一资源归属解析
%% ===================================================================

%% @doc 解析资源所属 Workspace
%% {ok, WsId}：资源 scope=workspace；personal：个人资源（含 c2c/moment 等
%% 不进守卫的，均有可证归属链，见模块头）；
%% {error, not_found}：资源不存在（行未命中 / ID 非法）；
%% {error, {db_error, Reason}}：DB 层异常（绝不与 not_found 混淆）；
%% {error, {unsupported_resource, _}}：未知资源类型（收紧原 `_ -> personal` 兜底）；
%% {error, {unsupported_scope, _}}：attachment.scope 脏数据 / 引用缺失。
-spec resolve_workspace(term()) ->
    {ok, integer()}
    | personal
    | {error, not_found}
    | {error, {db_error, term()}}
    | {error, {unsupported_resource, term()}}
    | {error, {unsupported_scope, term()}}.
resolve_workspace({workspace, WsId}) ->
    case one_row(<<"SELECT id FROM workspace WHERE id = $1">>, [WsId]) of
        {row, _} -> {ok, elib_cnv:safe_to_integer(WsId)};
        {error, _} = E -> E
    end;
resolve_workspace({project, ProjectId}) ->
    %% project 恒属 workspace（无 scope 概念，I7/迁移 00000078）
    case one_row(<<"SELECT workspace_id FROM project WHERE id = $1">>, [ProjectId]) of
        {row, #{<<"workspace_id">> := WsId}} when WsId =/= null ->
            {ok, elib_cnv:safe_to_integer(WsId)};
        {row, _} ->
            {error, not_found};
        {error, _} = E ->
            E
    end;
resolve_workspace({project_task, TaskId}) ->
    case
        one_row(
            <<"SELECT p.workspace_id FROM project_task t",
                " JOIN project p ON p.id = t.project_id WHERE t.id = $1">>,
            [TaskId]
        )
    of
        {row, #{<<"workspace_id">> := WsId}} when WsId =/= null ->
            {ok, elib_cnv:safe_to_integer(WsId)};
        {row, _} ->
            {error, not_found};
        {error, _} = E ->
            E
    end;
resolve_workspace({group, Gid}) ->
    group_scope(Gid);
resolve_workspace({group_notice, NoticeId}) ->
    case one_row(<<"SELECT group_id FROM group_notice WHERE id = $1">>, [NoticeId]) of
        {row, #{<<"group_id">> := Gid}} -> group_scope(Gid);
        {error, _} = E -> E
    end;
resolve_workspace({channel, ChannelId}) ->
    channel_scope(ChannelId);
resolve_workspace({channel_message, MessageId}) ->
    case one_row(<<"SELECT channel_id FROM channel_message WHERE id = $1">>, [MessageId]) of
        {row, #{<<"channel_id">> := ChannelId}} -> channel_scope(ChannelId);
        {error, _} = E -> E
    end;
resolve_workspace({channel_comment, CommentId}) ->
    case one_row(<<"SELECT channel_id FROM channel_comment WHERE id = $1">>, [CommentId]) of
        {row, #{<<"channel_id">> := ChannelId}} -> channel_scope(ChannelId);
        {error, _} = E -> E
    end;
resolve_workspace({channel_reaction, ReactionId}) ->
    case one_row(<<"SELECT channel_id FROM channel_reaction WHERE id = $1">>, [ReactionId]) of
        {row, #{<<"channel_id">> := ChannelId}} -> channel_scope(ChannelId);
        {error, _} = E -> E
    end;
resolve_workspace({channel_subscription, ChannelId}) ->
    channel_scope(ChannelId);
resolve_workspace({channel_admin, ChannelId}) ->
    channel_scope(ChannelId);
resolve_workspace({channel_webhook, WebhookId}) ->
    case one_row(<<"SELECT channel_id FROM channel_webhook WHERE id = $1">>, [WebhookId]) of
        {row, #{<<"channel_id">> := ChannelId}} -> channel_scope(ChannelId);
        {error, _} = E -> E
    end;
resolve_workspace({channel_invitation, InvitationId}) ->
    case one_row(<<"SELECT channel_id FROM channel_invitation WHERE id = $1">>, [InvitationId]) of
        {row, #{<<"channel_id">> := ChannelId}} -> channel_scope(ChannelId);
        {error, _} = E -> E
    end;
resolve_workspace({attachment, AttachId}) ->
    %% 附件归属最小闭环：见模块头"附件归属最小闭环"证明链。
    case one_row(<<"SELECT scope, scope_ref FROM attachment WHERE id = $1">>, [AttachId]) of
        {row, #{<<"scope">> := <<"group">>, <<"scope_ref">> := Ref}} when
            Ref =/= null, Ref =/= <<>>
        ->
            group_scope(elib_cnv:safe_to_integer(Ref));
        {row, #{<<"scope">> := <<"channel">>, <<"scope_ref">> := Ref}} when
            Ref =/= null, Ref =/= <<>>
        ->
            channel_scope(elib_cnv:safe_to_integer(Ref));
        %% 可证 personal 域（c2c/moment/private/public，证明链见模块头）：
        %% 读鉴权在 attach_logic:authorize/3 独立执行，本守卫只判 workspace 边界。
        {row, #{<<"scope">> := PersonalScope}} when
            PersonalScope =:= <<"c2c">>;
            PersonalScope =:= <<"moment">>;
            PersonalScope =:= <<"private">>;
            PersonalScope =:= <<"public">>
        ->
            personal;
        %% scope 非法（枚举外脏数据）或 group/channel 行 scope_ref 缺失：
        %% 显式拒绝，不发明默认归属（SEC-03 Stop 条款）。
        {row, #{<<"scope">> := OtherScope}} ->
            {error, {unsupported_scope, OtherScope}};
        {row, _NoScopeColumn} ->
            {error, {unsupported_scope, undefined}};
        {error, _} = E ->
            E
    end;
resolve_workspace(Target) ->
    %% SEC-03 收紧：未知资源类型不再默认 personal（原 fail-open 兜底）。
    %% 全仓核查（rg resolve_workspace）：生产调用方仅传上方已知类型元组，
    %% 无存量路径依赖 unknown→personal；测试断言已同步改为显式错误。
    {error, {unsupported_resource, Target}}.

%% ===================================================================
%% 2. Workspace 边界执行（handler 前置校验）
%% ===================================================================

%% @doc 频道入口边界：workspace 频道要求请求者是 active 工作区成员
%% personal 频道 / 频道不存在 → ok（既有流程继续，零行为变化）；
%% 归属解析 DB 异常 / 非法数据 → {error, {503, _}}（fail-closed，绝不放行）。
-spec ensure_channel_member_access(integer(), integer() | binary()) ->
    ok | {error, {integer(), binary()}}.
ensure_channel_member_access(Uid, ChannelId) ->
    gate_scope(channel_scope(ChannelId), Uid).

%% @doc 群入口边界：workspace 群要求请求者是 active 工作区成员
%% personal 群 / 群不存在 → ok；DB 异常 → {error, {503, _}}（fail-closed）。
-spec ensure_group_member_access(integer(), integer() | binary()) ->
    ok | {error, {integer(), binary()}}.
ensure_group_member_access(Uid, Gid) ->
    gate_scope(group_scope(Gid), Uid).

%% 归属解析结果 → 门决策（SEC-03 fail-closed 核心）：
%%   {ok, WsId}       → 成员边界校验；
%%   personal         → ok（回归红线：个人资源零行为变化）；
%%   {error, not_found} → ok（资源不存在放行走既有 404 流程，调用方语义依赖）；
%%   其他错误（db_error / unsupported_*）→ 503 拒绝，绝不 ok 放行。
-spec gate_scope(
    {ok, integer()} | personal | {error, not_found} | {error, term()}, integer()
) ->
    ok | {error, {integer(), binary()}}.
gate_scope({ok, WsId}, Uid) ->
    ensure_member_ok(WsId, Uid);
gate_scope(personal, _Uid) ->
    ok;
gate_scope({error, not_found}, _Uid) ->
    ok;
gate_scope({error, Reason}, _Uid) ->
    _ = ?ERROR_LOG([workspace_scope_gate_denied, Reason]),
    {error, {?ERR_SERVICE_UNAVAILABLE, <<"资源归属校验暂不可用，请稍后重试"/utf8>>}}.

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
%% （binding 读取是路由层操作，不是 DB 调用，与 fail-closed 无关。）
-spec guard_channel_binding(cowboy_req:req() | map(), integer()) ->
    ok | {error, {integer(), binary()}}.
guard_channel_binding(Req, Uid) ->
    case catch cowboy_req:binding(channel_id, Req) of
        ChannelId when is_binary(ChannelId); is_integer(ChannelId) ->
            ensure_channel_member_access(Uid, ChannelId);
        _ ->
            ok
    end.

%% @doc handler 便捷门：custom_id 入口（by_custom_id 路由的直访通道）
%% 自定义 ID 命中的频道若为 workspace scope，同样不能绕过 403。
%% SEC-03 fail-closed：find_by_custom_id 返回 {error, Reason} 或崩溃
%% （连接层异常）时按 503 拒绝，不再按"未命中"放行；
%% 空结果（elib_pg:one/2 无行默认 #{}）才是 custom_id 未命中 → 走既有 404。
-spec guard_channel_custom_id(integer(), binary()) -> ok | {error, {integer(), binary()}}.
guard_channel_custom_id(Uid, CustomId) when is_binary(CustomId), CustomId =/= <<>> ->
    case catch channel_ds:find_by_custom_id(CustomId) of
        Channel when is_map(Channel), map_size(Channel) > 0 ->
            ensure_channel_member_access(Uid, maps:get(<<"id">>, Channel, 0));
        {error, Reason} ->
            custom_id_lookup_denied(Reason);
        {'EXIT', Reason} ->
            custom_id_lookup_denied(Reason);
        _Empty ->
            %% custom_id 未命中（空 map）：既有 404 流程，语义不变
            ok
    end;
guard_channel_custom_id(_Uid, _CustomId) ->
    ok.

custom_id_lookup_denied(Reason) ->
    _ = ?ERROR_LOG([workspace_custom_id_lookup_denied, Reason]),
    {error, {?ERR_SERVICE_UNAVAILABLE, <<"资源归属校验暂不可用，请稍后重试"/utf8>>}}.

%% @doc handler 便捷门：群入口（gid 参数，POST body 或 query string 均可传值）
%% 用于 group_handler:detail/msg_page 与 group_notice_handler 全部入口。
-spec guard_group_gid(integer(), integer() | binary()) -> ok | {error, {integer(), binary()}}.
guard_group_gid(Uid, Gid) ->
    Gid2 = elib_cnv:safe_to_integer(Gid),
    case Gid2 > 0 of
        true -> ensure_group_member_access(Uid, Gid2);
        false -> ok
    end.

%% @doc handler 便捷门：群公告入口（notice_id 参数）
%% notice → group_id → scope 解析后执行同一 Workspace 成员边界；
%% personal 群公告 / 公告不存在 → ok（走既有 404/群成员校验流程）；
%% 归属解析 DB 异常 → {error, {503, _}}（fail-closed）。
%% Group Notice 继续只属于 Group（I12），本守卫只加 Workspace 边界前置。
-spec guard_group_notice_id(integer(), integer() | binary()) ->
    ok | {error, {integer(), binary()}}.
guard_group_notice_id(Uid, NoticeId) ->
    Id2 = elib_cnv:safe_to_integer(NoticeId),
    case Id2 > 0 of
        true ->
            gate_scope(resolve_workspace({group_notice, Id2}), Uid);
        false ->
            ok
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec group_scope(integer() | binary()) ->
    {ok, integer()}
    | personal
    | {error, not_found}
    | {error, {db_error, term()}}.
group_scope(Gid) ->
    %% "group" 是保留字表名，SQL 中必须双引号（T3-④）
    case row_scope(<<"\"group\"">>, Gid) of
        {ok, <<"workspace">>, WsId} when WsId =/= null -> {ok, WsId};
        {ok, _, _} -> personal;
        Else -> Else
    end.

-spec channel_scope(integer() | binary()) ->
    {ok, integer()}
    | personal
    | {error, not_found}
    | {error, {db_error, term()}}.
channel_scope(ChannelId) ->
    case row_scope(<<"channel">>, ChannelId) of
        {ok, <<"workspace">>, WsId} when WsId =/= null -> {ok, WsId};
        {ok, _, _} -> personal;
        Else -> Else
    end.

%% 通用 scope 行查询：{ok, Scope, WorkspaceId} | {error, not_found}
%% | {error, {db_error, Reason}}（三态，SEC-03）
%% DB 调用带 catch 防御驱动层崩溃；异常与"无行"严格区分，
%% 绝不把连接层故障归一成 not_found。
-spec row_scope(binary(), integer() | binary()) ->
    {ok, binary() | nil, integer() | nil}
    | {error, not_found}
    | {error, {db_error, term()}}.
row_scope(Tb, Id) ->
    Id2 = elib_cnv:safe_to_integer(Id),
    case Id2 > 0 of
        false ->
            {error, not_found};
        true ->
            Sql = <<"SELECT scope, workspace_id FROM ", Tb/binary, " WHERE id = $1">>,
            case one_row(Sql, [Id2]) of
                {row, #{<<"scope">> := Scope} = Row} ->
                    {ok, Scope, maps:get(<<"workspace_id">>, Row, undefined)};
                {error, _} = E ->
                    E
            end
    end.

%% 单行查询三态归一（SEC-03）：
%%   {row, Row}              ——命中一行（rows_to_maps 产物恒非空 map）；
%%   {error, not_found}      ——无行（elib_pg:one/2 空结果返回默认值 #{}）；
%%   {error, {db_error, _}}  ——{error, Reason}（池不可用/查询失败）或崩溃
%%                             （catch 得 {'EXIT', _} 等），一律视为 DB 异常。
-spec one_row(iodata(), [term()]) ->
    {row, map()} | {error, not_found} | {error, {db_error, term()}}.
one_row(Sql, Params) ->
    case catch elib_pg:one(Sql, Params) of
        {ok, Row} when is_map(Row), map_size(Row) > 0 ->
            {row, Row};
        {ok, _EmptyDefault} ->
            {error, not_found};
        {error, Reason} ->
            {error, {db_error, Reason}};
        {'EXIT', Reason} ->
            {error, {db_error, Reason}};
        Other ->
            {error, {db_error, Other}}
    end.
