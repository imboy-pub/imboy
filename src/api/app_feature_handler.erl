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
