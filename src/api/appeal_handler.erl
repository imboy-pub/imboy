-module(appeal_handler).

%% R-04：用户侧申诉 API（薄适配层）。
%%   POST /api/v1/appeal/create   {action_id, reason}
%%   GET  /api/v1/appeal/my
%%   GET  /api/v1/appeal/actions  针对我的处置动作（R-04.1 申诉入口）
%% 可用性门：imboy_feature:enabled(appeal)（logic 层同样校验，双保险）。

-behavior(cowboy_rest).

-export([init/2]).

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 =
        case Action of
            create -> create(Req0, State);
            my -> my(Req0, State);
            my_actions -> my_actions(Req0, State);
            _ -> Req0
        end,
    {ok, Req1, State}.

-spec create(cowboy_req:req(), map()) -> cowboy_req:req().
create(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    PostVals = elib_param:post(Req0),
    ActionId = ec_cnv:to_integer(maps:get(<<"action_id">>, PostVals, 0)),
    Reason = maps:get(<<"reason">>, PostVals, <<>>),
    case Uid > 0 of
        false ->
            elib_response:error(Req0, <<"请先登录"/utf8>>);
        true ->
            case moderation_appeal_logic:submit(Uid, ActionId, Reason) of
                {ok, Appeal} ->
                    elib_response:success(Req0, #{<<"appeal">> => Appeal}, "success.");
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

-spec my(cowboy_req:req(), map()) -> cowboy_req:req().
my(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    case Uid > 0 of
        false ->
            elib_response:error(Req0, <<"请先登录"/utf8>>);
        true ->
            case moderation_appeal_logic:my_list(Uid) of
                {ok, Appeals} ->
                    elib_response:success(Req0, #{<<"appeals">> => Appeals}, "success.");
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% GET /api/v1/appeal/actions——针对我的处置动作（申诉入口列表）
-spec my_actions(cowboy_req:req(), map()) -> cowboy_req:req().
my_actions(Req0, State) ->
    Uid = maps:get(current_uid, State, 0),
    case Uid > 0 of
        false ->
            elib_response:error(Req0, <<"请先登录"/utf8>>);
        true ->
            case moderation_appeal_logic:my_actions(Uid) of
                {ok, Actions} ->
                    elib_response:success(Req0, #{<<"actions">> => Actions}, "success.");
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.
