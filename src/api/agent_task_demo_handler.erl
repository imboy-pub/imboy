-module(agent_task_demo_handler).

%%%
% Agent 任务生命周期 **PoC 演示端点** / demo endpoint（Phase 4 T4.2）。
% POST /api/v1/agent_task/demo：触发一段演示任务生命周期，让可观测前端（群内任务
% 卡片/进度条）在无真实 agent 驱动时也有数据可渲染。
%
% agent_uid：演示期以 current_uid（JWT 认证会话）**充当** agent 身份替身——真实场景
%   agent 应是独立一等成员身份，此处仅为 PoC 让演示可跑。绝不从请求体读身份。
% group_id：从请求体取，成员经 group_ds:member_uids/1 解析后交 agent_task_demo:run_demo。
%%%

-export([init/2]).
-export([demo/2]).

-include("log.hrl").
-include("error_code.hrl").

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0, false),
    State = maps:remove(action, State0),
    Req1 = handle_action(Action, Req0, State),
    {ok, Req1, State}.

-spec handle_action(atom(), cowboy_req:req(), map()) -> cowboy_req:req().
handle_action(demo, Req, State) -> demo(Req, State);
handle_action(_, Req, _State) -> Req.

-spec demo(cowboy_req:req(), map()) -> cowboy_req:req().
demo(Req0, State) ->
    AgentUid = maps:get(current_uid, State),
    PostVals = safe_post(Req0),
    case group_id(PostVals) of
        0 ->
            elib_response:error(Req0, <<"group_id 必须提供"/utf8>>, ?ERR_BAD_REQUEST);
        GroupId ->
            MemberUids = group_ds:member_uids(GroupId),
            ok = agent_task_demo:run_demo(AgentUid, GroupId, MemberUids),
            elib_response:success(Req0, #{
                <<"agent_uid">> => AgentUid,
                <<"group_id">> => GroupId,
                <<"member_count">> => length(MemberUids)
            })
    end.

%% ===================================================================
%% Internal
%% ===================================================================

%% group_id 兼容 group_id / gid 两种键，统一为正整数（无效则 0）
group_id(PostVals) ->
    Raw = maps:get(<<"group_id">>, PostVals, maps:get(<<"gid">>, PostVals, 0)),
    case Raw of
        I when is_integer(I), I > 0 -> I;
        B when is_binary(B) ->
            try binary_to_integer(B) of
                I when I > 0 -> I;
                _ -> 0
            catch
                _:_ -> 0
            end;
        _ ->
            0
    end.

safe_post(Req0) ->
    try elib_param:post(Req0) of
        PostVals when is_map(PostVals) -> PostVals;
        PostVals when is_list(PostVals) -> maps:from_list(PostVals);
        _ -> #{}
    catch
        _:_ -> #{}
    end.
