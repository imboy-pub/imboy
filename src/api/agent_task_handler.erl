-module(agent_task_handler).

%%%
% Agent 任务群内审批端点 / Agent task in-group approval endpoint（Phase 4 T4.2）。
% 群内可点击审批卡片(imboyapp)的后端落点：POST /api/v1/agent_task/{approve,reject}。
% ApproverUid 恒取自 JWT 认证会话的 current_uid（**绝不**从请求体读），只传 task_id。
% 授权/去重仲裁在 agent_task_observer（群成员 + 非 agent 本人 + first-wins）。
%%%

-export([init/2]).
-export([approve/2, reject/2]).

-include("log.hrl").
-include("error_code.hrl").

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0, false),
    State = maps:remove(action, State0),
    Req1 = handle_action(Action, Req0, State),
    {ok, Req1, State}.

-spec handle_action(atom(), cowboy_req:req(), map()) -> cowboy_req:req().
handle_action(approve, Req, State) -> approve(Req, State);
handle_action(reject, Req, State) -> reject(Req, State);
handle_action(_, Req, _State) -> Req.

-spec approve(cowboy_req:req(), map()) -> cowboy_req:req().
approve(Req0, State) ->
    decide(Req0, State, approve).

-spec reject(cowboy_req:req(), map()) -> cowboy_req:req().
reject(Req0, State) ->
    decide(Req0, State, reject).

%% ===================================================================
%% Internal
%% ===================================================================

decide(Req0, State, Action) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = safe_post(Req0),
    case task_id(PostVals) of
        <<>> ->
            elib_response:error(Req0, <<"task_id 必须提供"/utf8>>, ?ERR_BAD_REQUEST);
        TaskId ->
            Result =
                case Action of
                    approve -> agent_task_observer:approve(TaskId, CurrentUid);
                    reject -> agent_task_observer:reject(TaskId, CurrentUid)
                end,
            respond(Req0, TaskId, Result)
    end.

respond(Req, TaskId, {ok, Decision}) ->
    elib_response:success(Req, #{
        <<"task_id">> => TaskId,
        <<"status">> => atom_to_binary(Decision, utf8)
    });
respond(Req, _TaskId, {error, not_authorized}) ->
    elib_response:error(Req, <<"无权审批该任务"/utf8>>, ?ERR_BAD_REQUEST);
respond(Req, _TaskId, {error, already_decided}) ->
    elib_response:error(Req, <<"该任务已被处理"/utf8>>, ?ERR_BAD_REQUEST);
respond(Req, _TaskId, {error, not_found}) ->
    elib_response:error(Req, <<"任务不存在或已过期"/utf8>>, ?ERR_BAD_REQUEST);
respond(Req, _TaskId, {error, _}) ->
    elib_response:error(Req, <<"操作失败"/utf8>>).

%% task_id 兼容 task_id / taskId 两种键，统一为 binary
task_id(PostVals) ->
    Raw = maps:get(<<"task_id">>, PostVals, maps:get(<<"taskId">>, PostVals, <<>>)),
    case Raw of
        B when is_binary(B) -> B;
        _ -> <<>>
    end.

safe_post(Req0) ->
    try elib_param:post(Req0) of
        PostVals when is_map(PostVals) -> PostVals;
        PostVals when is_list(PostVals) -> maps:from_list(PostVals);
        _ -> #{}
    catch
        _:_ -> #{}
    end.
