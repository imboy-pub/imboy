-module(project_member_handler).
%%%
% 项目成员 HTTP handler（Channel-first-class W2 ZC-02）
%
% 项目成员管理（§ZC-02 范围）：成员列表（分页）/邀请/移除/Owner 转移；
% 权限模型见 project_member_logic 文件头（Project Owner 管理权 +
% Workspace Owner 治理权 + Guest 只读 + 非 Project Member 直访 403）。
%
%%% =====================================================================
%%% 路由片段清单（ZC-05 统一注册 imboy_router.erl 时按此合并；本模块不改 router）
%%% =====================================================================
%%% METHOD path                                                       → handler 动作
%%% GET    /api/v1/projects/:project_id/members                       → members
%%% POST   /api/v1/projects/:project_id/members/invite                → invite
%%% POST   /api/v1/projects/:project_id/members/remove                → remove
%%% POST   /api/v1/projects/:project_id/members/transfer_owner        → transfer_owner
%%%
%%% 建议路由条目（追加到 "/api/v1/tasks/:task_id" 组之后，全部走
%%% /api/v1/* JWT 默认门）：
%%%   {"/api/v1/projects/:project_id/members",
%%%    project_member_handler, #{action => members}},
%%%   {"/api/v1/projects/:project_id/members/invite",
%%%    project_member_handler, #{action => invite}},
%%%   {"/api/v1/projects/:project_id/members/remove",
%%%    project_member_handler, #{action => remove}},
%%%   {"/api/v1/projects/:project_id/members/transfer_owner",
%%%    project_member_handler, #{action => transfer_owner}},
%%%
%%% 测试约定：路由未注册期间用直接模块调用方式测（不起 HTTP），见
%%% test/api/project_member_handler_tests.erl。
%%% =====================================================================

-behavior(cowboy_rest).

-export([init/2]).
-export([handle_action/3]).

%% ===================================================================
%% API functions
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = handle_action(Action, Req0, State),
    {ok, Req1, State}.

-spec handle_action(atom() | false, cowboy_req:req(), map()) -> cowboy_req:req().
handle_action(members, Req, State) -> list(Req, State);
handle_action(invite, Req, State) -> invite(Req, State);
handle_action(remove, Req, State) -> remove(Req, State);
handle_action(transfer_owner, Req, State) -> transfer_owner(Req, State);
handle_action(false, Req, _State) -> Req.

%% @doc 项目成员列表（分页；Owner/治理方/active 项目成员可读）
-spec list(cowboy_req:req(), map()) -> cowboy_req:req().
list(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    {Page, Size} = elib_param:page(Req0),
    case resolve_project_id(Req0) of
        {error, Req} ->
            Req;
        {ok, ProjectId} ->
            case project_member_logic:list(Uid, ProjectId, Page, Size) of
                {ok, Result} ->
                    elib_response:success(Req0, Result);
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% @doc 邀请项目成员（body: user_id；仅 Project Owner；幂等）
-spec invite(cowboy_req:req(), map()) -> cowboy_req:req().
invite(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    case resolve_project_id(Req0) of
        {error, Req} ->
            Req;
        {ok, ProjectId} ->
            case to_uid(maps:get(<<"user_id">>, PostVals, undefined)) of
                0 ->
                    elib_response:error(Req0, <<"user_id 必须"/utf8>>, 400);
                TargetUid ->
                    case project_member_logic:invite(Uid, ProjectId, TargetUid) of
                        {ok, Member, created} ->
                            elib_response:success(Req0, Member#{status_flag => created});
                        {ok, Member, existing} ->
                            %% 重复邀请幂等：返回既有成员，不报错、不重复入库
                            elib_response:success(Req0, Member#{status_flag => existing});
                        {error, {Code, Msg}} ->
                            elib_response:error(Req0, Msg, Code)
                    end
            end
    end.

%% @doc 移除项目成员（body: user_id；Project Owner 或 Workspace Owner；幂等）
-spec remove(cowboy_req:req(), map()) -> cowboy_req:req().
remove(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    case resolve_project_id(Req0) of
        {error, Req} ->
            Req;
        {ok, ProjectId} ->
            case to_uid(maps:get(<<"user_id">>, PostVals, undefined)) of
                0 ->
                    elib_response:error(Req0, <<"user_id 必须"/utf8>>, 400);
                TargetUid ->
                    case project_member_logic:remove(Uid, ProjectId, TargetUid) of
                        {ok, Result, Status} ->
                            elib_response:success(Req0, Result#{status_flag => Status});
                        {error, {Code, Msg}} ->
                            elib_response:error(Req0, Msg, Code)
                    end
            end
    end.

%% @doc 项目 Owner 转移（body: user_id；仅 Project Owner）
-spec transfer_owner(cowboy_req:req(), map()) -> cowboy_req:req().
transfer_owner(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    case resolve_project_id(Req0) of
        {error, Req} ->
            Req;
        {ok, ProjectId} ->
            case to_uid(maps:get(<<"user_id">>, PostVals, undefined)) of
                0 ->
                    elib_response:error(Req0, <<"user_id 必须"/utf8>>, 400);
                TargetUid ->
                    case project_member_logic:transfer_owner(Uid, ProjectId, TargetUid) of
                        {ok, Result} ->
                            elib_response:success(Req0, Result);
                        {error, {Code, Msg}} ->
                            elib_response:error(Req0, Msg, Code)
                    end
            end
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 解析 project_id（路径 binding 优先，post body 兜底）
-spec resolve_project_id(cowboy_req:req()) ->
    {ok, integer()} | {error, cowboy_req:req()}.
resolve_project_id(Req0) ->
    Raw =
        case cowboy_req:binding(project_id, Req0) of
            undefined ->
                PostVals = elib_param:post(Req0),
                maps:get(<<"project_id">>, PostVals, undefined);
            Binding ->
                Binding
        end,
    ProjectId = elib_cnv:safe_to_integer(Raw),
    case is_integer(ProjectId) andalso ProjectId > 0 of
        true ->
            {ok, ProjectId};
        false ->
            {error, elib_response:error(Req0, <<"project id 必须"/utf8>>, 400)}
    end.

%% user_id 参数清洗：正整数 → 该值；其余（含缺失）→ 0（logic 层 400）
-spec to_uid(term()) -> integer().
to_uid(undefined) ->
    0;
to_uid(V) ->
    Id = elib_cnv:safe_to_integer(V),
    case is_integer(Id) andalso Id > 0 of
        true -> Id;
        false -> 0
    end.
