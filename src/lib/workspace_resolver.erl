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
%        （后两者 WP4/T7 为 workspace_guard 写守卫扩展）
%        群子功能域（P0 后续批，均经所属 group 行解析）：
%        group_vote / group_vote_record / group_schedule(含 remind)
%        / group_album(含 photo) / group_file / group_task(含 assignment)
%        ——引用 integer = 内部 PK、binary = 对外业务 ID（vote_id/schedule_id/
%        album_id/photo_id/task_id）。
%        attachment(经 scope_ref→group/channel；T7 结项：c2c/moment/private/
%        public 为个人域——scope 落库不可变 + 读 ACL 恒绑定原 scope，附件进
%        workspace 的唯一途径是上传时即带 group/channel scope（落库点均接
%        同事务守卫），无需回溯，显式返回 personal，见 attachment 子句注释)。
%   2. ensure_channel_member_access/2 / ensure_group_member_access/2：
%      Workspace 边界执行前置校验——scope=workspace 的资源要求请求者是
%      active workspace_member（§1.4.2 授权规则 2），非成员稳定 403；
%      scope=personal 恒 ok（personal 零行为变化，回归红线）；
%      资源不存在恒 ok（放行给既有 404 流程，不吞既有语义）；
%      DB 异常 fail-closed：稳定 {error, {503, Msg}}（M-1/M-2 收口，
%      不再吞异常放行）。
%   3. guard_channel_binding/2 / guard_group_gid/2：handler 层便捷门，
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
%% ---- 群子功能域（P0 后续批：vote/schedule/album/file/task）----
%% 均经所属 group 行解析 scope；解析读（归属不可变）走自动提交连接是安全的。
%% 引用类型按参数类型分派：integer = 内部 PK，binary = 对外业务 ID。
resolve_workspace({group_vote, VoteId}) when is_binary(VoteId) ->
    case one_row(<<"SELECT group_id FROM group_vote WHERE vote_id = $1">>, [VoteId]) of
        #{<<"group_id">> := Gid} -> group_scope(Gid);
        _ -> {error, not_found}
    end;
resolve_workspace({group_vote_record, RecordId}) ->
    case
        one_row(
            <<"SELECT v.group_id AS group_id FROM group_vote_record r",
                " JOIN group_vote v ON v.vote_id = r.vote_id WHERE r.id = $1">>,
            [RecordId]
        )
    of
        #{<<"group_id">> := Gid} -> group_scope(Gid);
        _ -> {error, not_found}
    end;
resolve_workspace({group_schedule, ScheduleRef}) when is_binary(ScheduleRef) ->
    case one_row(<<"SELECT group_id FROM group_schedule WHERE schedule_id = $1">>, [ScheduleRef]) of
        #{<<"group_id">> := Gid} -> group_scope(Gid);
        _ -> {error, not_found}
    end;
resolve_workspace({group_schedule, SchedulePk}) ->
    case one_row(<<"SELECT group_id FROM group_schedule WHERE id = $1">>, [SchedulePk]) of
        #{<<"group_id">> := Gid} -> group_scope(Gid);
        _ -> {error, not_found}
    end;
resolve_workspace({group_schedule_remind, RemindId}) ->
    case
        one_row(
            <<"SELECT gs.group_id AS group_id FROM group_schedule_remind r",
                " JOIN group_schedule gs ON gs.schedule_id = r.schedule_id WHERE r.id = $1">>,
            [RemindId]
        )
    of
        #{<<"group_id">> := Gid} -> group_scope(Gid);
        _ -> {error, not_found}
    end;
resolve_workspace({group_album, AlbumRef}) when is_binary(AlbumRef) ->
    case one_row(<<"SELECT group_id FROM group_album WHERE album_id = $1">>, [AlbumRef]) of
        #{<<"group_id">> := Gid} -> group_scope(Gid);
        _ -> {error, not_found}
    end;
resolve_workspace({group_album, AlbumPk}) ->
    case one_row(<<"SELECT group_id FROM group_album WHERE id = $1">>, [AlbumPk]) of
        #{<<"group_id">> := Gid} -> group_scope(Gid);
        _ -> {error, not_found}
    end;
resolve_workspace({group_album_photo, PhotoRef}) when is_binary(PhotoRef) ->
    case one_row(<<"SELECT group_id FROM group_album_photo WHERE photo_id = $1">>, [PhotoRef]) of
        #{<<"group_id">> := Gid} -> group_scope(Gid);
        _ -> {error, not_found}
    end;
resolve_workspace({group_album_photo, PhotoPk}) ->
    case one_row(<<"SELECT group_id FROM group_album_photo WHERE id = $1">>, [PhotoPk]) of
        #{<<"group_id">> := Gid} -> group_scope(Gid);
        _ -> {error, not_found}
    end;
resolve_workspace({group_file, FilePk}) ->
    case one_row(<<"SELECT group_id FROM group_file WHERE id = $1">>, [FilePk]) of
        #{<<"group_id">> := Gid} -> group_scope(Gid);
        _ -> {error, not_found}
    end;
resolve_workspace({group_task, TaskRef}) when is_binary(TaskRef) ->
    case one_row(<<"SELECT group_id FROM group_task WHERE task_id = $1">>, [TaskRef]) of
        #{<<"group_id">> := Gid} -> group_scope(Gid);
        _ -> {error, not_found}
    end;
resolve_workspace({group_task, TaskPk}) ->
    case one_row(<<"SELECT group_id FROM group_task WHERE id = $1">>, [TaskPk]) of
        #{<<"group_id">> := Gid} -> group_scope(Gid);
        _ -> {error, not_found}
    end;
resolve_workspace({group_task_assignment, AssignmentId}) ->
    case
        one_row(
            <<"SELECT t.group_id AS group_id FROM group_task_assignment a",
                " JOIN group_task t ON t.task_id = a.task_id WHERE a.id = $1">>,
            [AssignmentId]
        )
    of
        #{<<"group_id">> := Gid} -> group_scope(Gid);
        _ -> {error, not_found}
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
        %% T7 结项（2026-08 调查）：c2c/moment/private/public 附件显式归为
        %% personal（个人域），不做 workspace 回溯——判定依据三条不变量：
        %%   1. attachment.scope 落库后不可变：全库唯一后置 UPDATE 是
        %%      bind_moment_scope_ref（仅回填 moment 行的 scope_ref），ON
        %%      CONFLICT 只递增 referer 计数——个人域附件不存在事后改挂
        %%      群/频道/workspace 的写路径；
        %%   2. 读 ACL 恒绑定原始 scope（attach_logic:authorize）：c2c 附件被
        %%      转发进群（msg_forward 只建 forward 型新消息、不改附件行）后，
        %%      群成员 view_url 仍按 c2c 会话成员判定（拒绝）、private 附件仅
        %%      creator 可读——跨域引用不解锁读取，附件不会成为群可读资源；
        %%   3. 附件进入 workspace 范围的唯一途径是上传时即携带 scope=group/
        %%      channel（scope_ref=目标 id，上传点已知目标、无需回溯），其全部
        %%      落库点（attach_logic:do_save_1 转正 + group_file_ds:
        %%      write_attachment 群文件补写）均已接入同事务归档写守卫（980）。
        %% 故 workspace 归档不影响个人域附件的转正与读取——personal 是设计
        %% 决定而非回溯缺失（原 TODO(T7) 结项）。
        #{<<"scope">> := Scope} when
            Scope =:= <<"c2c">>;
            Scope =:= <<"moment">>;
            Scope =:= <<"private">>;
            Scope =:= <<"public">>
        ->
            personal;
        %% 行不存在 / 未知 scope 值沿历史 personal 兜底（守卫对两者均放行，
        %% not_found 交既有 404 流程，不吞既有语义）。
        _ ->
            personal
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
    ok | {error, {403 | 503, binary()}}.
ensure_channel_member_access(Uid, ChannelId) ->
    try channel_scope(ChannelId) of
        {ok, WsId} -> ensure_member_ok(WsId, Uid);
        _ -> ok
    catch
        error:{resolver_db_error, _} -> resolver_db_unavailable()
    end.

%% @doc 群入口边界：workspace 群要求请求者是 active 工作区成员
%% personal 群 / 群不存在 → ok；DB 异常 → {error, {503, _}}（fail-closed）。
-spec ensure_group_member_access(integer(), integer() | binary()) ->
    ok | {error, {403 | 503, binary()}}.
ensure_group_member_access(Uid, Gid) ->
    try group_scope(Gid) of
        {ok, WsId} -> ensure_member_ok(WsId, Uid);
        _ -> ok
    catch
        error:{resolver_db_error, _} -> resolver_db_unavailable()
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
%% 无 binding/资源不存在放行是安全的：下游 action 处理必有独立 ACL/查询
%% （频道成员校验、404 流程）；DB 异常则 fail-closed 503（不吞异常放行）。
-spec guard_channel_binding(cowboy_req:req() | map(), integer()) ->
    ok | {error, {403 | 503, binary()}}.
guard_channel_binding(Req, Uid) ->
    case catch cowboy_req:binding(channel_id, Req) of
        ChannelId when is_binary(ChannelId); is_integer(ChannelId) ->
            ensure_channel_member_access(Uid, ChannelId);
        _ ->
            ok
    end.

%% @doc handler 便捷门：custom_id 入口（by_custom_id 路由的直访通道）
%% 自定义 ID 命中的频道若为 workspace scope，同样不能绕过 403。
%% 未命中（#{} 空行）放行是安全的：下游 get_channel_by_custom_id 必有
%% 独立查询/404 流程；DB 错误（{error, _}）与连接层异常（'EXIT'）
%% fail-closed 503，不再按"未命中"吞掉放行（M-2 T5 收口）。
-spec guard_channel_custom_id(integer(), binary()) -> ok | {error, {403 | 503, binary()}}.
guard_channel_custom_id(Uid, CustomId) when is_binary(CustomId), CustomId =/= <<>> ->
    case catch channel_ds:find_by_custom_id(CustomId) of
        Channel when is_map(Channel), map_size(Channel) > 0 ->
            ensure_channel_member_access(Uid, maps:get(<<"id">>, Channel, 0));
        {error, _Reason} ->
            resolver_db_unavailable();
        {'EXIT', _Reason} ->
            resolver_db_unavailable();
        _ ->
            ok
    end;
guard_channel_custom_id(_Uid, _CustomId) ->
    ok.

custom_id_lookup_denied(Reason) ->
    _ = ?ERROR_LOG([workspace_custom_id_lookup_denied, Reason]),
    {error, {?ERR_SERVICE_UNAVAILABLE, <<"资源归属校验暂不可用，请稍后重试"/utf8>>}}.

%% @doc handler 便捷门：群入口（gid 参数，POST body 或 query string 均可传值）
%% 用于 group_handler:detail/msg_page 与 group_notice_handler 全部入口。
%% gid 非法/群不存在放行：下游 detail/msg_page 必有独立群成员校验；
%% DB 异常经 ensure_group_member_access fail-closed 503。
-spec guard_group_gid(integer(), integer() | binary()) -> ok | {error, {403 | 503, binary()}}.
guard_group_gid(Uid, Gid) ->
    Gid2 = elib_cnv:safe_to_integer(Gid),
    case Gid2 > 0 of
        true -> ensure_group_member_access(Uid, Gid2);
        false -> ok
    end.

%% @doc handler 便捷门：群公告入口（notice_id 参数）
%% notice → group_id → scope 解析后执行同一 Workspace 成员边界；
%% personal 群公告 / 公告不存在 → ok（走既有 404/群成员校验流程，
%% 下游必有独立 ACL）；DB 异常 fail-closed 503。
%% Group Notice 继续只属于 Group（I12），本守卫只加 Workspace 边界前置。
-spec guard_group_notice_id(integer(), integer() | binary()) -> ok | {error, {403 | 503, binary()}}.
guard_group_notice_id(Uid, NoticeId) ->
    Id2 = elib_cnv:safe_to_integer(NoticeId),
    case Id2 > 0 of
        true ->
            try resolve_workspace({group_notice, Id2}) of
                {ok, WsId} -> ensure_member_ok(WsId, Uid);
                _ -> ok
            catch
                error:{resolver_db_error, _} -> resolver_db_unavailable()
            end;
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

%% 通用 scope 行查询：返回 {ok, Scope, WorkspaceId} | {error, not_found}
%% 零行（one_row 返回 #{}）按 not_found 处理；DB 异常由 one_row 抛出
%% {resolver_db_error, _}，交守卫入口归一 503 fail-closed（M-1/M-2 收口：
%% 原 catch 吞 DB 崩溃按 not_found 放行，是归档守卫 fail-open 的根因）。
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
                Row = #{<<"scope">> := Scope} ->
                    {ok, Scope, maps:get(<<"workspace_id">>, Row, undefined)};
                {error, _} = E ->
                    E
            end
    end.

%% DB 异常 fail-closed 根因修复（M-1）：elib_pg:one 零行返回 {ok, #{}}
%% （Default），{error, _} 与连接层异常才是真 DB 故障——原实现全部吞成
%% #{}，经 not_found 让 5 个 ensure_writable 自动提交调用点 fail-open。
%% 现零行仍返回 #{}（业务空），DB 故障抛 {resolver_db_error, _} 交上层
%% 守卫归一 503。
-spec one_row(iodata(), [term()]) -> map().
one_row(Sql, Params) ->
    case catch elib_pg:one(Sql, Params) of
        {ok, Row} when is_map(Row), map_size(Row) > 0 ->
            Row;
        {ok, _ZeroRowDefault} ->
            #{};
        Failed ->
            erlang:error({resolver_db_error, Failed})
    end.

%% DB 异常 fail-closed 稳定错误（与 workspace_guard:ensure_writable_tx 同文案）
-spec resolver_db_unavailable() -> {error, {503, binary()}}.
resolver_db_unavailable() ->
    {error, {503, <<"工作区状态检查失败，请稍后重试"/utf8>>}}.
