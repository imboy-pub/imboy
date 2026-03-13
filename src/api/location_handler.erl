-module(location_handler).

%%%
% location 控制器模块
% location controller module
%%%
-behavior(cowboy_rest).

-export([init/2]).

-include_lib("eunit/include/eunit.hrl").

-include_lib("kernel/include/logger.hrl").

-include("common.hrl").
-include("log.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    % ?DEBUG_LOG(State),
    Action = maps:get(action, State0),
    % ?DEBUG_LOG([people_nearby, handler, Action]),
    State = maps:remove(action, State0),
    Req1 =
        case imboy_feature:ensure_enabled(Req0, location) of
            ok ->
                case Action of
                    make_myself_visible ->
                        make_myself_visible(Req0, State);
                    make_myself_unvisible ->
                        make_myself_unvisible(Req0, State);
                    people_nearby ->
                        people_nearby(Req0, State);
                    false ->
                        Req0
                end;
            {error, RespReq} ->
                RespReq
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 让自己可见
%% 设置用户位置为可见状态
%%
%% @param Req0 Cowboy请求对象，包含经纬度信息
%% @param State 状态映射，包含 current_uid
%% @return 返回成功或错误响应
%% @end
-spec make_myself_visible(cowboy_req:req(), map()) -> cowboy_req:req().
make_myself_visible(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    Lat = maps:get(<<"latitude">>, PostVals, <<>>),
    Lng = maps:get(<<"longitude">>, PostVals, <<>>),
    % ?DEBUG_LOG([CurrentUid, Lat, Lng]),
    case location_logic:make_myself_visible(CurrentUid, Lat, Lng) of
        ok ->
            elib_response:success(Req0, #{}, "success.");
        {error, Msg} ->
            elib_response:error(Req0, Msg)
    end.

%% @doc 让自己不可见
%% 设置用户位置为不可见状态
%%
%% @param Req0 Cowboy请求对象
%% @param State 状态映射，包含 current_uid
%% @return 返回成功响应
%% @end
-spec make_myself_unvisible(cowboy_req:req(), map()) -> cowboy_req:req().
make_myself_unvisible(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    location_logic:make_myself_unvisible(CurrentUid),
    elib_response:success(Req0, #{}, "success.").

% 附近的人
-spec people_nearby(cowboy_req:req(), map()) -> cowboy_req:req().
people_nearby(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    Qs = cowboy_req:parse_qs(Req0),
    Lng = proplists:get_value(<<"longitude">>, Qs, undefined),
    Lat = proplists:get_value(<<"latitude">>, Qs, undefined),
    Unit = proplists:get_value(<<"unit">>, Qs, <<"m">>),
    {ok, Radius} = elib_param:int(radius, Req0, 500),
    {ok, Limit} = elib_param:int(limit, Req0, 100),
    % ?DEBUG_LOG([people_nearby, handler, Lng, Lat, Radius, Unit, Limit]),
    % 直接传递 integer，location_logic:people_nearby 的签名支持 binary() | integer()
    List = location_logic:people_nearby(CurrentUid, Lng, Lat, Radius, Unit, Limit),
    Payload = #{
        <<"radius">> => Radius,
        <<"size">> => length(List),
        <<"unit">> => <<"m">>,
        <<"list">> => List
    },
    elib_response:success(Req0, Payload).

%% ===================================================================
%% EUnit tests.
%% ===================================================================

