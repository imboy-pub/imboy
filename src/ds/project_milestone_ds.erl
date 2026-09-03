-module(project_milestone_ds).
-compile([nowarn_deprecated_catch]).
%%%
% project_milestone_ds 是 project_milestone domain service 缩写
% 项目里程碑领域服务（迁移 00000081，Channel-first-class W2 / ZC-03）
%
% 核心职责（全部单事务，with_tx）：
%   1. create：project 归属解析 → 归档写守卫（先锁 workspace 行）→
%      写权限校验（Workspace active member 且 role≠guest；且 Project Owner
%      或 active Project Member）→ INSERT（status 恒为 planned）→
%      project_event 写入（同一 Conn = 同一事务，event_type='milestone_created'，
%      无孤儿事件——回滚时事件与业务行一起消失）。
%   2. update：milestone→project→workspace 解析 → 守卫 → 权限 →
%      仅更新提交字段（name/due_date 白名单；status 不可经 update 变更，
%      只能走 reach）→ UPDATE → milestone_updated 事件同事务。
%   3. reach：守卫 → 权限 → 状态机（planned→reached 单向，UPDATE status +
%      reached_at 同语句组且 WHERE 带 status='planned' 并发守卫——{ok,0} 按
%      already_reached 幂等短路，不重复写事件；已 reached 幂等短路返回）→
%      milestone_reached 事件同事务。
%
% 状态机（计划边界）：仅 planned|reached 两态，单向 planned→reached；
% 无回退端点（reached→planned 拒绝由"不存在该操作"保证，DB CHECK
% chk_project_milestone_reached 强制 status ⟺ reached_at 一致性）。
% 禁依赖/甘特图/复杂状态机（表 COMMENT 计划契约）。
%
% W2 权限（ZC-03）：project_member 只读查询暂经
% project_milestone_repo:find_project_member_tx/4（project_member_repo 由
% ZC-02 并行产出，ZC-05 整合统一到 project_member_logic）。
%%%

-export([create/4]).
-export([find_by_id/1]).
-export([list_by_project/4]).
-export([update/4]).
-export([reach/2]).

-include("log.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 创建里程碑（status 恒 planned；DueDate = {Y,M,D} | null | undefined；
%% Owner/active Project Member 且 workspace role≠guest；Guest 403；archived 980；
%% 事件与业务行同事务）
-spec create(integer(), integer(), binary(), {integer(), integer(), integer()} | null | undefined) ->
    {ok, map()} | {error, term()}.
create(CreatorUid, ProjectId, Name, DueDate) ->
    Result =
        elib_pg:with_tx(fun(Conn) ->
            {WsId, OwnerId} = ensure_project_ctx_tx(Conn, ProjectId),
            ok = workspace_guard:abort_on_error(
                workspace_guard:ensure_writable_tx(Conn, {workspace, WsId})
            ),
            ok = ensure_writer_tx(Conn, WsId, ProjectId, OwnerId, CreatorUid),
            Now = elib_dt:now(),
            Data = #{
                <<"workspace_id">> => WsId,
                <<"project_id">> => ProjectId,
                <<"name">> => Name,
                <<"due_date">> => normalize_due_date(DueDate),
                <<"status">> => <<"planned">>,
                <<"created_at">> => Now,
                <<"updated_at">> => Now
            },
            case project_milestone_repo:add_tx(Conn, Data) of
                {ok, MsId} ->
                    case
                        event_tx(
                            Conn,
                            ProjectId,
                            MsId,
                            CreatorUid,
                            <<"milestone_created">>,
                            #{<<"name">> => Name}
                        )
                    of
                        {ok, _} ->
                            %% M-5：事务内回读（提交后读抖动不再拖垮成功写）
                            {ok, MsId, project_milestone_repo:find_by_id_tx(Conn, MsId)};
                        {error, Reason} ->
                            throw({abort_tx, {milestone_create_failed, Reason}})
                    end;
                {error, Reason} ->
                    throw({abort_tx, {milestone_create_failed, Reason}})
            end
        end),
    case Result of
        {ok, MsId, Row} ->
            _ = ?INFO_LOG([project_milestone_created, CreatorUid, ProjectId, MsId]),
            {ok, Row};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 里程碑详情
-spec find_by_id(integer()) -> map() | {error, term()}.
find_by_id(MsId) ->
    project_milestone_repo:find_by_id(MsId, project_milestone_repo:full_columns()).

%% @doc 项目里程碑列表（status all|planned|reached；分页参数由 logic 层归一）
-spec list_by_project(integer(), binary() | all, integer(), integer()) ->
    {ok, [map()]} | {error, term()}.
list_by_project(ProjectId, Status, Page, Size) ->
    project_milestone_repo:list_by_project(ProjectId, Status, Page, Size).

%% @doc 更新里程碑（字段白名单 name/due_date；Name/DueDate = undefined 表示
%% 保留原值；DueDate = null 表示清空；status 不可经本入口变更——只能走 reach；
%% 无提交字段时为 no-op，不写行不写事件；事件与更新同事务）
-spec update(
    integer(),
    integer(),
    binary() | undefined,
    {integer(), integer(), integer()} | null | undefined
) ->
    {ok, map()} | {error, term()}.
update(ActorUid, MsId, Name, DueDate) ->
    Result =
        elib_pg:with_tx(fun(Conn) ->
            Ms = project_milestone_repo:find_tx(
                Conn, MsId, <<"id,project_id,workspace_id,status">>
            ),
            ProjectId = maps:get(<<"project_id">>, Ms, undefined),
            case is_integer(ProjectId) andalso ProjectId > 0 of
                false ->
                    throw({abort_tx, {404, <<"里程碑不存在"/utf8>>}});
                true ->
                    {WsId, OwnerId} = ensure_project_ctx_tx(Conn, ProjectId),
                    ok = workspace_guard:abort_on_error(
                        workspace_guard:ensure_writable_tx(Conn, {workspace, WsId})
                    ),
                    ok = ensure_writer_tx(Conn, WsId, ProjectId, OwnerId, ActorUid),
                    Data0 =
                        case Name of
                            N when is_binary(N), N =/= <<>> -> #{<<"name">> => N};
                            _ -> #{}
                        end,
                    Data1 =
                        case DueDate of
                            D when is_tuple(D), tuple_size(D) =:= 3 ->
                                Data0#{<<"due_date">> => D};
                            null ->
                                Data0#{<<"due_date">> => null};
                            _ ->
                                Data0
                        end,
                    case map_size(Data1) of
                        0 ->
                            {ok, no_fields, project_milestone_repo:find_by_id_tx(Conn, MsId)};
                        _ ->
                            Now = elib_dt:now(),
                            case
                                project_milestone_repo:update_fields_tx(
                                    Conn, MsId, Data1#{<<"updated_at">> => Now}
                                )
                            of
                                {ok, _} ->
                                    Payload = update_event_payload(Name, DueDate),
                                    case
                                        event_tx(
                                            Conn,
                                            ProjectId,
                                            MsId,
                                            ActorUid,
                                            <<"milestone_updated">>,
                                            Payload
                                        )
                                    of
                                        {ok, _} ->
                                            {ok, updated,
                                                project_milestone_repo:find_by_id_tx(Conn, MsId)};
                                        {error, Reason} ->
                                            throw({abort_tx, {milestone_update_failed, Reason}})
                                    end;
                                {error, Reason2} ->
                                    throw({abort_tx, {milestone_update_failed, Reason2}})
                            end
                    end
            end
        end),
    case Result of
        {ok, _Flag, Row} ->
            {ok, Row};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 达成里程碑（planned→reached 单向；已 reached 幂等返回 already_reached
%% 且不重复写事件；reached_at 与 status 同事务写入；事件同事务）
-spec reach(integer(), integer()) ->
    {ok, map(), reached | already_reached} | {error, term()}.
reach(ActorUid, MsId) ->
    Result =
        elib_pg:with_tx(fun(Conn) ->
            Ms = project_milestone_repo:find_tx(
                Conn, MsId, <<"id,project_id,workspace_id,status">>
            ),
            ProjectId = maps:get(<<"project_id">>, Ms, undefined),
            case is_integer(ProjectId) andalso ProjectId > 0 of
                false ->
                    throw({abort_tx, {404, <<"里程碑不存在"/utf8>>}});
                true ->
                    {WsId, OwnerId} = ensure_project_ctx_tx(Conn, ProjectId),
                    ok = workspace_guard:abort_on_error(
                        workspace_guard:ensure_writable_tx(Conn, {workspace, WsId})
                    ),
                    ok = ensure_writer_tx(Conn, WsId, ProjectId, OwnerId, ActorUid),
                    case maps:get(<<"status">>, Ms, <<>>) of
                        <<"reached">> ->
                            %% 幂等：重复 reach 返回成功，不更新、不重复写事件
                            {ok, already_reached, project_milestone_repo:find_by_id_tx(Conn, MsId)};
                        <<"planned">> ->
                            Now = elib_dt:now(),
                            case
                                project_milestone_repo:mark_reached_tx(Conn, MsId, #{
                                    <<"status">> => <<"reached">>,
                                    <<"reached_at">> => Now,
                                    <<"updated_at">> => Now
                                })
                            of
                                {ok, 1} ->
                                    case
                                        event_tx(
                                            Conn,
                                            ProjectId,
                                            MsId,
                                            ActorUid,
                                            <<"milestone_reached">>,
                                            #{
                                                <<"from">> => <<"planned">>,
                                                <<"to">> => <<"reached">>
                                            }
                                        )
                                    of
                                        {ok, _} ->
                                            {ok, reached,
                                                project_milestone_repo:find_by_id_tx(Conn, MsId)};
                                        {error, Reason} ->
                                            throw({abort_tx, {milestone_reach_failed, Reason}})
                                    end;
                                {ok, 0} ->
                                    %% 并发守卫（M-2）：UPDATE 带 status='planned'
                                    %% 谓词，0 行 = 另一事务已在本事务读取后达成
                                    %% （READ COMMITTED 重评估）——幂等短路，
                                    %% 不覆盖 reached_at、不重复写事件
                                    {ok, already_reached,
                                        project_milestone_repo:find_by_id_tx(Conn, MsId)};
                                {error, Reason2} ->
                                    throw({abort_tx, {milestone_reach_failed, Reason2}})
                            end;
                        _ ->
                            throw({abort_tx, {404, <<"里程碑不存在"/utf8>>}})
                    end
            end
        end),
    case Result of
        {ok, Flag, Row} ->
            _ = ?INFO_LOG([project_milestone_reached, ActorUid, MsId, Flag]),
            {ok, Row, Flag};
        {error, Reason} ->
            {error, Reason}
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% 事务内：解析 project 归属（返回 {workspace_id, owner_id}；不存在 404）
-spec ensure_project_ctx_tx(any(), integer()) -> {integer(), integer()} | no_return().
ensure_project_ctx_tx(Conn, ProjectId) ->
    Project = project_repo:find_tx(Conn, ProjectId, <<"id,workspace_id,owner_id">>),
    case maps:get(<<"workspace_id">>, Project, undefined) of
        WsId when is_integer(WsId) ->
            {WsId, maps:get(<<"owner_id">>, Project, 0)};
        _ ->
            throw({abort_tx, {404, <<"项目不存在"/utf8>>}})
    end.

%% 事务内写权限（ZC-03 语义）：
%%   1. 必须是同 workspace 的 active workspace_member；role=guest 只读 403；
%%   2. 且必须是 Project Owner 或 active Project Member，否则 403。
%% project_member 查询复用 project_member_repo，并保持在当前事务连接内。
-spec ensure_writer_tx(any(), integer(), integer(), integer(), integer()) -> ok | no_return().
ensure_writer_tx(Conn, WsId, ProjectId, OwnerId, Uid) ->
    case workspace_member_repo:find_tx(Conn, WsId, Uid, <<"role,status">>) of
        #{<<"status">> := <<"active">>, <<"role">> := <<"guest">>} ->
            throw({abort_tx, {403, <<"Guest 角色只能查看里程碑，写操作被拒绝"/utf8>>}});
        #{<<"status">> := <<"active">>} ->
            case Uid =:= OwnerId of
                true ->
                    ok;
                false ->
                    case
                        project_milestone_repo:find_project_member_tx(
                            Conn, ProjectId, Uid, <<"status">>
                        )
                    of
                        #{<<"status">> := <<"active">>} -> ok;
                        _ -> throw({abort_tx, {403, <<"仅项目成员可写里程碑"/utf8>>}})
                    end
            end;
        _ ->
            throw({abort_tx, {403, <<"非工作区成员，禁止访问该资源"/utf8>>}})
    end.

%% 事务内写事件（与业务写同 Conn = 同事务；event_type 见迁移 00000081
%% chk_project_event_type 契约：milestone_created|milestone_updated|milestone_reached）
-spec event_tx(any(), integer(), integer(), integer(), binary(), map()) ->
    {ok, integer()} | {error, term()}.
event_tx(Conn, ProjectId, TargetId, ActorId, EventType, PayloadExtra) ->
    Payload = PayloadExtra#{<<"actor">> => ActorId},
    project_event_repo:insert_tx(Conn, #{
        <<"project_id">> => ProjectId,
        <<"actor_id">> => ActorId,
        <<"event_type">> => EventType,
        <<"target_id">> => TargetId,
        <<"payload">> => jsone:encode(Payload, [native_utf8]),
        <<"created_at">> => elib_dt:now()
    }).

%% update 事件 payload（name/due_date 仅记录实际提交的字段；
%% due_date tuple 转 ISO binary 以便 jsonb 编码——格式与
%% project_milestone_repo:due_date_to_iso（读路径归一口径）一致）
-spec update_event_payload(binary() | undefined, {Y, M, D} | null | undefined) -> map() when
    Y :: integer(), M :: integer(), D :: integer().
update_event_payload(Name, DueDate) ->
    P0 =
        case Name of
            N when is_binary(N), N =/= <<>> -> #{<<"name">> => N};
            _ -> #{}
        end,
    case DueDate of
        {Y, M, D} when is_integer(Y), is_integer(M), is_integer(D) ->
            P0#{<<"due_date">> => due_date_to_binary({Y, M, D})};
        null ->
            P0#{<<"due_date">> => null};
        _ ->
            P0
    end.

%% 入参归一：undefined 视为未提供 → SQL NULL（create 无到期日）
-spec normalize_due_date({integer(), integer(), integer()} | null | undefined) ->
    {integer(), integer(), integer()} | null.
normalize_due_date(D) when is_tuple(D), tuple_size(D) =:= 3 -> D;
normalize_due_date(_) -> null.

-spec due_date_to_binary({integer(), integer(), integer()}) -> binary().
due_date_to_binary({Y, M, D}) ->
    iolist_to_binary(
        io_lib:format("~4..0B-~2..0B-~2..0B", [Y, M, D])
    ).
