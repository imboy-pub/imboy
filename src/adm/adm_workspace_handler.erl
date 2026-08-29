-module(adm_workspace_handler).
-compile([nowarn_deprecated_catch]).

%%%
% adm_workspace 控制器模块（双体验 v2.5.2 WP7/T11b）
% Workspace / Project 运营管理 API——部署者"看得到、管得住"新实体：
%   * 列表/详情/工作区成员（Workspace Members 全称）只读
%   * 运营归档/恢复（写审计列 + adm_operation_log；与 T7 写守卫 980 联动）
%   * Project 详情只读（W0 无 project member，仅 assignee 概览）
%
% 鉴权镜像 adm_group_handler / adm_channel_handler：每个 action 显式走
% adm_acl:ensure_permission（fail-closed：无权限/无 adm_user_id 恒 403，
% 绝不放行——admin 端点 fail-open 是已知事故模式，鉴权测试见
% test/adm/adm_workspace_handler_tests.erl）。
%%%

-behavior(cowboy_rest).

-export([init/2]).

-include("log.hrl").
-include("common.hrl").
-include("error_code.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case imboy_plugin_registry:required_feature(admin, adm_workspace_handler, Action) of
            undefined ->
                dispatch(Action, Method, Req0, State);
            Feature ->
                case imboy_feature:ensure_enabled(Req0, Feature) of
                    ok ->
                        dispatch(Action, Method, Req0, State);
                    {error, RespReq} ->
                        RespReq
                end
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec dispatch(atom() | false, binary(), cowboy_req:req(), map()) -> cowboy_req:req().
dispatch(list, Method, Req0, State) ->
    list_action(Method, Req0, State);
dispatch(detail, Method, Req0, State) ->
    detail_action(Method, Req0, State);
dispatch(members, Method, Req0, State) ->
    members_action(Method, Req0, State);
dispatch(archive, Method, Req0, State) ->
    archive_action(Method, Req0, State);
dispatch(restore, Method, Req0, State) ->
    restore_action(Method, Req0, State);
dispatch(project_list, Method, Req0, State) ->
    project_list_action(Method, Req0, State);
dispatch(project_detail, Method, Req0, State) ->
    project_detail_action(Method, Req0, State);
dispatch(project_members, Method, Req0, State) ->
    project_members_action(Method, Req0, State);
dispatch(project_milestones, Method, Req0, State) ->
    project_milestones_action(Method, Req0, State);
dispatch(project_channels, Method, Req0, State) ->
    project_channels_action(Method, Req0, State);
dispatch(project_aggregations, Method, Req0, State) ->
    project_aggregations_action(Method, Req0, State);
dispatch(_, _Method, Req0, _State) ->
    Req0.

%% @doc 工作区分页列表（搜索/状态筛选；资源计数随行返回）
-spec list_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
list_action(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"workspaces:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            {Page, Size} = elib_param:page(Req0),
            {ok, Status} = elib_param:binary(status, Req0, <<"all">>),
            {ok, Keyword} = elib_param:binary(keyword, Req0, <<>>),
            case workspace_logic:admin_page(Page, Size, Status, Keyword) of
                {ok, P} ->
                    elib_response:success(Req0, normalize_ws_page(P));
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end;
list_action(_, Req0, _State) ->
    method_not_allowed(Req0).

%% @doc 工作区详情（基本信息 + branding + 工作区成员预览 + 资源清单）
-spec detail_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
detail_action(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"workspaces:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            case parse_ws_id(Req0) of
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {ok, WsId} ->
                    case workspace_logic:admin_detail(WsId) of
                        {ok, Detail} ->
                            elib_response:success(Req0, normalize_ws_detail(Detail));
                        {error, {Code, Msg}} ->
                            elib_response:error(Req0, Msg, Code)
                    end
            end
    end;
detail_action(_, Req0, _State) ->
    method_not_allowed(Req0).

%% @doc 工作区成员分页列表（详情页"工作区成员"，role 徽标）
-spec members_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
members_action(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"workspaces:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            case parse_ws_id(Req0) of
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {ok, WsId} ->
                    {Page, Size} = elib_param:page(Req0),
                    case workspace_logic:admin_member_page(WsId, Page, Size) of
                        {ok, P} ->
                            elib_response:success(Req0, normalize_member_page(P));
                        {error, {Code, Msg}} ->
                            elib_response:error(Req0, Msg, Code)
                    end
            end
    end;
members_action(_, Req0, _State) ->
    method_not_allowed(Req0).

%% @doc 运营归档（二次确认在前端；服务端写审计列 + 操作日志；
%% 归档后 workspace 业务写被 T7 守卫以稳定错误码 980 拒绝）
-spec archive_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
archive_action(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"workspaces:update">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            AdmUserId = maps:get(adm_user_id, State, 0),
            case parse_ws_id(Req0) of
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {ok, WsId} ->
                    case workspace_logic:admin_archive(AdmUserId, WsId) of
                        {ok, Result} ->
                            _ = audit_workspace_governance(AdmUserId, WsId, <<"archive">>, Req0),
                            elib_response:success(
                                Req0, normalize_ws_ids(Result), <<"工作区已归档"/utf8>>
                            );
                        {error, {Code, Msg}} ->
                            elib_response:error(Req0, Msg, Code)
                    end
            end
    end;
archive_action(_, Req0, _State) ->
    method_not_allowed(Req0).

%% @doc 运营恢复（清空归档审计列；恢复后写守卫放行）
-spec restore_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
restore_action(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"workspaces:update">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            AdmUserId = maps:get(adm_user_id, State, 0),
            case parse_ws_id(Req0) of
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {ok, WsId} ->
                    case workspace_logic:admin_restore(AdmUserId, WsId) of
                        {ok, Result} ->
                            _ = audit_workspace_governance(AdmUserId, WsId, <<"restore">>, Req0),
                            elib_response:success(
                                Req0, normalize_ws_ids(Result), <<"工作区已恢复"/utf8>>
                            );
                        {error, {Code, Msg}} ->
                            elib_response:error(Req0, Msg, Code)
                    end
            end
    end;
restore_action(_, Req0, _State) ->
    method_not_allowed(Req0).

%% @doc 项目分页列表（只读；任务计数随行返回）
-spec project_list_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
project_list_action(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"workspaces:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            {Page, Size} = elib_param:page(Req0),
            {ok, Status} = elib_param:binary(status, Req0, <<"all">>),
            {ok, Keyword} = elib_param:binary(keyword, Req0, <<>>),
            case project_logic:admin_page(Page, Size, Status, Keyword) of
                {ok, P} ->
                    elib_response:success(Req0, normalize_project_page(P));
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end;
project_list_action(_, Req0, _State) ->
    method_not_allowed(Req0).

%% @doc 项目详情（只读：workspace 概要 + task 状态分布 + assignee 概览；
%% W0 无 project member，不提供成员读写）
-spec project_detail_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
project_detail_action(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"workspaces:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            case parse_project_id(Req0) of
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {ok, ProjectId} ->
                    case project_logic:admin_detail(ProjectId) of
                        {ok, Detail} ->
                            elib_response:success(Req0, normalize_project_detail(Detail));
                        {error, {Code, Msg}} ->
                            elib_response:error(Req0, Msg, Code)
                    end
            end
    end;
project_detail_action(_, Req0, _State) ->
    method_not_allowed(Req0).

%% @doc 项目成员分页（W2 治理只读；ZC-05）
-spec project_members_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
project_members_action(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"workspaces:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            case parse_project_id(Req0) of
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {ok, ProjectId} ->
                    {Page, Size} = elib_param:page(Req0),
                    case project_member_logic:admin_page(ProjectId, Page, Size) of
                        {ok, P} ->
                            elib_response:success(Req0, P);
                        {error, {Code, Msg}} ->
                            elib_response:error(Req0, Msg, Code)
                    end
            end
    end;
project_members_action(_, Req0, _State) ->
    method_not_allowed(Req0).

%% @doc 项目里程碑分页（W2 治理只读；status=all|planned|reached）
-spec project_milestones_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
project_milestones_action(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"workspaces:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            case parse_project_id(Req0) of
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {ok, ProjectId} ->
                    {Page, Size} = elib_param:page(Req0),
                    {ok, StatusBin} = elib_param:binary(status, Req0, <<"all">>),
                    Status =
                        case StatusBin of
                            <<"planned">> -> <<"planned">>;
                            <<"reached">> -> <<"reached">>;
                            _ -> all
                        end,
                    case project_milestone_logic:admin_page(ProjectId, Status, Page, Size) of
                        {ok, P} ->
                            elib_response:success(Req0, P);
                        {error, {Code, Msg}} ->
                            elib_response:error(Req0, Msg, Code)
                    end
            end
    end;
project_milestones_action(_, Req0, _State) ->
    method_not_allowed(Req0).

%% @doc 项目关联频道分页（W2 治理只读）
-spec project_channels_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
project_channels_action(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"workspaces:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            case parse_project_id(Req0) of
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {ok, ProjectId} ->
                    {Page, Size} = elib_param:page(Req0),
                    case project_channel_logic:admin_channels(ProjectId, Page, Size) of
                        {ok, P} ->
                            elib_response:success(Req0, P);
                        {error, {Code, Msg}} ->
                            elib_response:error(Req0, Msg, Code)
                    end
            end
    end;
project_channels_action(_, Req0, _State) ->
    method_not_allowed(Req0).

%% @doc 项目四类聚合只读（W2 治理；type=pinned|resources|activity|related_posts）
-spec project_aggregations_action(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
project_aggregations_action(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"workspaces:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            case parse_project_id(Req0) of
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                {ok, ProjectId} ->
                    {Page, Size} = elib_param:page(Req0),
                    {ok, Type} = elib_param:binary(type, Req0, <<"pinned">>),
                    case project_channel_logic:admin_aggregation(ProjectId, Type, Page, Size) of
                        {ok, P} ->
                            elib_response:success(Req0, P);
                        {error, {Code, Msg}} ->
                            elib_response:error(Req0, Msg, Code)
                    end
            end
    end;
project_aggregations_action(_, Req0, _State) ->
    method_not_allowed(Req0).

%% ===================================================================
%% 参数解析 / 归一化（TSID 一律 string 下发，防 JS 精度丢失）
%% ===================================================================

-spec parse_ws_id(cowboy_req:req()) -> {ok, integer()} | {error, binary()}.
parse_ws_id(Req0) ->
    parse_id_param(workspace_id, <<"工作区ID不能为空"/utf8>>, Req0).

-spec parse_project_id(cowboy_req:req()) -> {ok, integer()} | {error, binary()}.
parse_project_id(Req0) ->
    parse_id_param(project_id, <<"项目ID不能为空"/utf8>>, Req0).

-spec parse_id_param(atom(), binary(), cowboy_req:req()) -> {ok, integer()} | {error, binary()}.
parse_id_param(Key, EmptyMsg, Req0) ->
    case elib_param:binary(Key, Req0, <<>>) of
        {ok, Bin} when byte_size(Bin) > 0 ->
            case catch binary_to_integer(string:trim(Bin)) of
                Id when is_integer(Id), Id > 0 ->
                    {ok, Id};
                _ ->
                    {error, <<"ID格式错误"/utf8>>}
            end;
        _ ->
            {error, EmptyMsg}
    end.

-spec method_not_allowed(cowboy_req:req()) -> cowboy_req:req().
method_not_allowed(Req0) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-define(WS_ID_KEYS, [<<"id">>, <<"owner_id">>, <<"archived_by">>, <<"workspace_id">>]).
-define(WS_ROW_KEYS, [<<"id">>, <<"owner_id">>, <<"archived_by">>, <<"invited_by">>]).

-spec normalize_ws_page(map()) -> map().
normalize_ws_page(P) ->
    normalize_page(P, fun normalize_ws_row/1).

-spec normalize_member_page(map()) -> map().
normalize_member_page(P) ->
    normalize_page(P, fun normalize_member_row/1).

-spec normalize_project_page(map()) -> map().
normalize_project_page(P) ->
    normalize_page(P, fun normalize_project_row/1).

-spec normalize_page(map(), fun((map()) -> map())) -> map().
normalize_page(#{list := List} = P, Fun) ->
    maps:remove(items, P#{list => [Fun(Item) || Item <- List]});
normalize_page(P, _Fun) ->
    P.

%% 列表行：TSID id/owner_id/archived_by 转 string
-spec normalize_ws_row(map()) -> map().
normalize_ws_row(Row) ->
    elib_id:tsid_keys_to_bin(Row, ?WS_ROW_KEYS).

%% 工作区成员行：workspace_id/user_id/invited_by 转 string
-spec normalize_member_row(map()) -> map().
normalize_member_row(Row) ->
    elib_id:tsid_keys_to_bin(Row, [<<"workspace_id">>, <<"user_id">>, <<"invited_by">>]).

%% 项目列表行：TSID id/workspace_id/owner_id 转 string
-spec normalize_project_row(map()) -> map().
normalize_project_row(Row) ->
    elib_id:tsid_keys_to_bin(Row, [<<"id">>, <<"workspace_id">>, <<"owner_id">>]).

%% 详情：workspace 主行 + owner + 成员/资源清单嵌套归一
-spec normalize_ws_detail(map()) -> map().
normalize_ws_detail(Detail) ->
    Detail2 = elib_id:tsid_keys_to_bin(Detail, ?WS_ID_KEYS),
    Detail2#{
        owner => normalize_user(maps:get(owner, Detail2, #{})),
        members => normalize_member_page(maps:get(members, Detail2, #{})),
        projects => [normalize_resource_ids(R) || R <- maps:get(projects, Detail2, [])],
        groups => [normalize_resource_ids(R) || R <- maps:get(groups, Detail2, [])],
        channels => [normalize_resource_ids(R) || R <- maps:get(channels, Detail2, [])]
    }.

-spec normalize_resource_ids(map()) -> map().
normalize_resource_ids(Row) ->
    elib_id:tsid_keys_to_bin(Row, [<<"id">>, <<"owner_id">>, <<"owner_uid">>, <<"creator_uid">>]).

%% 项目详情：主行 + workspace/owner + assignee 聚合
-spec normalize_project_detail(map()) -> map().
normalize_project_detail(Detail) ->
    Detail2 = elib_id:tsid_keys_to_bin(Detail, [<<"id">>, <<"workspace_id">>, <<"owner_id">>]),
    Detail2#{
        workspace =>
            elib_id:tsid_keys_to_bin(
                maps:get(workspace, Detail2, #{}), [<<"id">>, <<"owner_id">>]
            ),
        owner => normalize_user(maps:get(owner, Detail2, #{})),
        assignees => [
            elib_id:tsid_keys_to_bin(A, [<<"assignee_id">>])
         || A <- maps:get(assignees, Detail2, [])
        ]
    }.

-spec normalize_user(map()) -> map().
normalize_user(User) ->
    elib_id:tsid_keys_to_bin(User, [<<"id">>]).

%% 归档/恢复返回值归一（atom 键 + TSID int → JSON 友好形态）
-spec normalize_ws_ids(map()) -> map().
normalize_ws_ids(Result) ->
    Bin = fun
        (V) when is_integer(V) -> integer_to_binary(V);
        (V) -> V
    end,
    maps:fold(
        fun(K, V, Acc) ->
            Acc#{to_binary_key(K) => Bin(V)}
        end,
        #{},
        Result
    ).

-spec to_binary_key(atom() | binary()) -> binary().
to_binary_key(K) when is_atom(K) -> atom_to_binary(K, utf8);
to_binary_key(K) when is_binary(K) -> K.

%% 操作审计（镜像 adm_channel_handler 的 channel_order_refund 模式）
-spec audit_workspace_governance(integer(), integer(), binary(), cowboy_req:req()) -> ok.
audit_workspace_governance(AdmUserId, WsId, Action, Req0) ->
    try
        _ = adm_operation_log_ds:insert(
            AdmUserId,
            <<"workspace_", Action/binary>>,
            WsId,
            <<"workspace">>,
            #{<<"workspace_id">> => WsId, <<"action">> => Action},
            elib_req:peer_ip(Req0)
        ),
        ok
    catch
        Class:Reason:Stacktrace ->
            %% 审计失败不阻断已完成的归档/恢复（主流程已成功）
            ?DEBUG_LOG("workspace governance audit failed: ~p", [{Class, Reason, Stacktrace}]),
            ok
    end.
