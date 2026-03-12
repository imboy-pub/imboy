-module(metrics_handler).

-behavior(cowboy_rest).

-export([init/2]).

%% @doc Metrics endpoint for runtime observability.
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Req1 =
        case fetch_metrics() of
            {ok, Metrics} ->
                elib_response:success(Req0, Metrics, "success.");
            {error, Reason} ->
                elib_response:error(Req0, format_error(Reason))
        end,
    {ok, Req1, State0}.

-spec fetch_metrics() -> {ok, map()} | {error, term()}.
fetch_metrics() ->
    try
        {ok, elib_metric:get_all_metrics()}
    catch
        Class:Reason ->
            {error, {Class, Reason}}
    end.

-spec format_error(term()) -> binary().
format_error(Reason) ->
    unicode:characters_to_binary(io_lib:format("metrics unavailable: ~p", [Reason])).
