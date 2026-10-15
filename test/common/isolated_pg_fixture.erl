-module(isolated_pg_fixture).
-export([run/1, run_case/1]).

%% Each PG case owns a fresh BEAM: TSID and named pools cannot reuse App state.
run(TestFun) when is_function(TestFun, 0) ->
    Paths = [filename:absname(P) || P <- code:get_path()],
    ConfigArgs =
        case init:get_argument(config) of
            {ok, Groups} ->
                lists:append([["-config", filename:absname(P)] || Group <- Groups, P <- Group]);
            error ->
                []
        end,
    {ok, Peer, _} = peer:start_link(#{
        connection => standard_io,
        args => ["-no_dot_erlang", "-pa" | lists:reverse(Paths)] ++ ConfigArgs,
        wait_boot => 10000
    }),
    try
        %% Leave time for boot and shutdown inside the outer 900-second budget.
        peer:call(Peer, ?MODULE, run_case, [TestFun], 880000)
    after
        peer:stop(Peer)
    end.

run_case(TestFun) when is_function(TestFun, 0) ->
    %% Load the inherited -config values without starting imboy or its pools.
    case application:load(imboy) of
        ok -> ok;
        {error, {already_loaded, imboy}} -> ok
    end,
    TestFun().
