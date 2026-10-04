-module(moya_roster_handler).
-moduledoc "墨芽教师侧只读班级学员名单 HTTP 适配层（MN-ROSTER-01）。".
%%%
% 墨芽教师侧只读班级学员名单 HTTP 适配层（MN-ROSTER-01，P0-2）
% GET  /api/v1/moya/classes/:id/learners   只读班级学员名单
%%%
% 路由注册与错误码 5430 的 error_code.hrl 宏接线由 Wave 2 统一完成；
% 本模块用整数常量传码（本地测试直接断言返回形状，模式同 moya_task_handler）。
% JWT 由路由中间件注入 State#current_uid；非法/缺失 TSID 返回参数错误，
% 不触达 DB，不泄漏班级存在性。

-behavior(cowboy_rest).

-export([init/2]).
-export([handle_action/3]).

-include("log.hrl").
-include("error_code.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = handle_action(Action, Req0, State),
    {ok, Req1, State}.

-spec handle_action(atom() | false, cowboy_req:req(), map()) -> cowboy_req:req().
handle_action(list, Req, State) -> list_learners(Req, State);
handle_action(false, Req, _State) -> Req.

%%%===================================================================
%%% Actions
%%%===================================================================

-spec list_learners(cowboy_req:req(), map()) -> cowboy_req:req().
list_learners(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case path_group_id(Req0) of
        {ok, GroupId} ->
            case moya_roster_logic:list(Uid, GroupId) of
                {ok, Payload} ->
                    elib_response:success_rfc3339(Req0, Payload);
                {error, Reason} ->
                    to_response(Req0, Reason)
            end;
        error ->
            elib_response:error(Req0, <<"班级参数错误"/utf8>>, ?ERR_PARAM_INVALID)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% 错误映射：新错误码 5430（error_code.hrl 宏接线留给 Wave 2，
%% 此处以整数常量传码）；cross_org 用既有宏 5426；
%% 其余 reason 走 moya_error 既有通道
-spec to_response(cowboy_req:req(), atom()) -> cowboy_req:req().
to_response(Req, Reason) ->
    case Reason of
        class_not_visible ->
            elib_response:error(Req, <<"班级不存在或不可见"/utf8>>, 5430);
        cross_org ->
            elib_response:error(
                Req, <<"跨机构访问被拒绝"/utf8>>, ?ERR_TEACHING_CROSS_ORG
            );
        Other ->
            moya_error:to_response(Req, Other)
    end.

-spec path_group_id(cowboy_req:req()) -> {ok, integer()} | error.
path_group_id(Req) ->
    case cowboy_req:binding(id, Req, undefined) of
        Bin when is_binary(Bin) -> elib_tsid:from_binary(Bin);
        _ -> error
    end.
