-module(feature_gate_middleware).
-behaviour(cowboy_middleware).

-export([execute/2]).

-spec execute(cowboy_req:req(), map()) ->
    {ok, cowboy_req:req(), map()} | {stop, cowboy_req:req()}.
execute(Req, #{handler_opts := #{required_feature := Feature}} = Env) ->
    case imboy_feature:ensure_enabled(Req, Feature) of
        ok -> {ok, Req, Env};
        {error, Req1} -> {stop, Req1}
    end;
execute(Req, Env) ->
    {ok, Req, Env}.
