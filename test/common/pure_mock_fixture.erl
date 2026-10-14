-module(pure_mock_fixture).
-export([run/1, with_mocks/2]).

%% Standard-I/O peer transport creates no distributed node or application listener.
run(TestFun) when is_function(TestFun, 0) ->
    Paths = [filename:absname(P) || P <- code:get_path()],
    {ok, Peer, _} = peer:start(#{
        connection => standard_io,
        args => ["-no_dot_erlang", "-pa" | Paths],
        wait_boot => 10000
    }),
    try
        peer:call(Peer, erlang, apply, [TestFun, []], 20000)
    after
        peer:stop(Peer)
    end.

with_mocks(Configs, TestFun) ->
    false = lists:keymember(imboy, 1, application:which_applications()),
    Guards = [{M, []} || M <- [elib_pg, elib_tsid], not lists:keymember(M, 1, Configs)],
    All = Guards ++ Configs,
    Modules = [M || {M, _} <- All],
    Existing = meck:mocked(),
    [] = [M || M <- Modules, lists:member(M, Existing)],
    Env = application:get_all_env(imboy),
    application:set_env(imboy, sql_driver, pgsql),
    application:set_env(imboy, dsync_enabled, false),
    try
        lists:foreach(
            fun({M, Expectations}) ->
                {module, M} = code:ensure_loaded(M),
                Exports = M:module_info(exports),
                ok = meck:new(M, [non_strict, no_link]),
                %% Undef can be caught without a meck history entry. Export explicit
                %% throwing guards so cleanup detects even swallowed undeclared calls.
                lists:foreach(
                    fun({F, Arity}) ->
                        case
                            F =:= module_info orelse
                                lists:any(
                                    fun({Name, A, _}) -> Name =:= F andalso A =:= Arity end,
                                    Expectations
                                )
                        of
                            true ->
                                ok;
                            false ->
                                ok = meck:expect(
                                    M,
                                    F,
                                    Arity,
                                    meck:raise(error, {undeclared_mock_call, M, F, Arity})
                                )
                        end
                    end,
                    Exports
                ),
                lists:foreach(
                    fun({F, Arity, Fun}) ->
                        ok = meck:expect(M, F, Arity, Fun)
                    end,
                    Expectations
                )
            end,
            All
        ),
        erlang:apply(TestFun, [])
    after
        cleanup(All, Env)
    end.

cleanup(Configs, Env) ->
    Modules = [M || {M, _} <- Configs],
    Declared = fun(M, F, Args) ->
        lists:any(
            fun({Name, Arity, _}) -> Name =:= F andalso Arity =:= length(Args) end,
            proplists:get_value(M, Configs)
        )
    end,
    Unexpected = lists:any(
        fun(M) ->
            case lists:member(M, meck:mocked()) of
                false ->
                    false;
                true ->
                    lists:any(
                        fun(Entry) ->
                            {Mod, F, Args} = element(2, Entry),
                            not Declared(Mod, F, Args)
                        end,
                        meck:history(M)
                    )
            end
        end,
        Modules
    ),
    lists:foreach(
        fun(M) ->
            case lists:member(M, meck:mocked()) of
                true -> ok = meck:unload(M);
                false -> ok
            end
        end,
        Modules
    ),
    lists:foreach(
        fun({K, _}) -> application:unset_env(imboy, K) end,
        application:get_all_env(imboy)
    ),
    lists:foreach(fun({K, V}) -> application:set_env(imboy, K, V) end, Env),
    false = Unexpected,
    ok.
