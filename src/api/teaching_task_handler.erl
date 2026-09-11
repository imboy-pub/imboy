-module(teaching_task_handler).
%%%
% 墨芽教师侧教学作业 HTTP 适配层（MN-TASK-01，P0-3）
% GET  /api/v1/teaching/tasks          老师作业列表（?group_id=&page=）
% POST /api/v1/teaching/tasks          发布教学作业（Idempotency-Key 必填）
%%%
% 路由注册与错误码 5430/5431/5432 的 error_code.hrl 宏接线由 Wave 2 统一完成；
% 本模块用整数常量传码（本地测试直接断言返回形状）。

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
handle_action(list, Req, State) -> list_tasks(Req, State);
handle_action(create, Req, State) -> create_task(Req, State);
handle_action(false, Req, _State) -> Req.

%%%===================================================================
%%% Actions
%%%===================================================================

-spec list_tasks(cowboy_req:req(), map()) -> cowboy_req:req().
list_tasks(Req0, State) ->
    Uid = maps:get(current_uid, State),
    Qs = cowboy_req:parse_qs(Req0),
    case group_id_param(proplists:get_value(<<"group_id">>, Qs, <<>>)) of
        {ok, GroupIdOpt} ->
            Page = page_param(Qs),
            case teaching_task_logic:list(Uid, GroupIdOpt, Page) of
                {ok, Payload} ->
                    elib_response:success_rfc3339(Req0, Payload);
                {error, Reason} ->
                    to_response(Req0, Reason)
            end;
        error ->
            elib_response:error(Req0, <<"班级参数错误"/utf8>>, ?ERR_PARAM_INVALID)
    end.

-spec create_task(cowboy_req:req(), map()) -> cowboy_req:req().
create_task(Req0, State) ->
    Uid = maps:get(current_uid, State),
    IdemKey = cowboy_req:header(<<"idempotency-key">>, Req0, <<>>),
    case IdemKey of
        Key when is_binary(Key), byte_size(Key) >= 8, byte_size(Key) =< 128 ->
            Body = elib_param:post(Req0),
            case teaching_task_logic:create(Uid, Key, Body) of
                {ok, Payload} ->
                    elib_response:success_rfc3339(Req0, Payload, <<"发布成功"/utf8>>);
                {error, Reason} ->
                    to_response(Req0, Reason)
            end;
        _ ->
            elib_response:error(Req0, <<"缺少幂等键"/utf8>>, ?ERR_IDEMPOTENCY_KEY_REQUIRED)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% 错误映射：新错误码 5430/5431/5432（error_code.hrl 宏接线留给 Wave 2，
%% 此处以整数常量传码；其余 reason 走 teaching_error 既有通道）
-spec to_response(cowboy_req:req(), atom()) -> cowboy_req:req().
to_response(Req, Reason) ->
    case Reason of
        class_not_visible ->
            elib_response:error(Req, <<"班级不存在或不可见"/utf8>>, 5430);
        learner_not_in_class ->
            elib_response:error(Req, <<"学员不在本班或已移出"/utf8>>, 5431);
        guardian_setup_required ->
            elib_response:error(Req, <<"监护关系需完善"/utf8>>, 5432);
        Other ->
            teaching_error:to_response(Req, Other)
    end.

%% group_id 可选：缺省=汇总全部 active staff 班级；提供时必须合法 TSID
-spec group_id_param(binary()) -> {ok, integer() | undefined} | error.
group_id_param(<<>>) ->
    {ok, undefined};
group_id_param(Bin) when is_binary(Bin) ->
    tsid(Bin);
group_id_param(_) ->
    error.

-spec tsid(binary()) -> {ok, integer()} | error.
tsid(Bin) when is_binary(Bin) ->
    try binary_to_integer(Bin) of
        Int when Int > 0 -> {ok, Int};
        _ -> error
    catch
        _:_ -> error
    end;
tsid(_) ->
    error.

-spec page_param(list()) -> {integer(), integer()}.
page_param(Qs) ->
    Page = int_param(Qs, <<"page">>, 1),
    Size = int_param(Qs, <<"size">>, 10),
    {max(1, Page), min(100, max(1, Size))}.

-spec int_param(list(), binary(), integer()) -> integer().
int_param(Qs, Key, Default) ->
    try binary_to_integer(proplists:get_value(Key, Qs, <<>>)) of
        Int when is_integer(Int) -> Int
    catch
        _:_ -> Default
    end.
