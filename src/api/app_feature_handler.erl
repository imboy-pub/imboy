-module(app_feature_handler).

-behavior(cowboy_rest).

-export([init/2]).

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0, false),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case Action of
            features ->
                features(Method, Req0);
            policy ->
                policy(Method, Req0);
            ice_servers ->
                ice_servers(Method, Req0);
            false ->
                Req0
        end,
    {ok, Req1, State}.

-spec features(binary(), cowboy_req:req()) -> cowboy_req:req().
features(<<"GET">>, Req0) ->
    elib_response:success(Req0, imboy_feature:all());
features(_, Req0) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec policy(binary(), cowboy_req:req()) -> cowboy_req:req().
policy(<<"GET">>, Req0) ->
    elib_response:success(Req0, imboy_policy:effective_view());
policy(_, Req0) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc 返回 WebRTC ICE Server 配置列表
%% 客户端在发起音视频通话前调用此接口获取 STUN/TURN 服务器地址
-spec ice_servers(binary(), cowboy_req:req()) -> cowboy_req:req().
ice_servers(<<"GET">>, Req0) ->
    Servers = config_ds:ice_servers(),
    elib_response:success(Req0, #{<<"ice_servers">> => Servers});
ice_servers(_, Req0) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).
