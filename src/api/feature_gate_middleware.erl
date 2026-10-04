-module(feature_gate_middleware).
-moduledoc "Cowboy feature-gate 中间件 —— 按 handler_opts 的 required_feature 经 imboy_feature 门控请求。".
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
