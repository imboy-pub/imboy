%% Pure mock fixtures. Never starts imboy, eunit_runner, migrations or a DB pool.
%% Existing mocks belong to their owner: a collision fails instead of unloading them.
-define(MC_WITH_MECKS(Configs, TestFun),
    {setup, fun() -> mc_setup(Configs) end, fun mc_cleanup/1, fun(_) -> ?_test((TestFun)()) end}
).

mc_setup(Configs) -> mc_setup(Configs, []).
mc_setup([], Acc) ->
    Acc;
mc_setup([{Module, Expectations} | Rest], Acc) ->
    meck:new(Module, [non_strict, no_link]),
    try
        lists:foreach(
            fun({Function, Arity, Implementation}) ->
                {arity, Arity} = erlang:fun_info(Implementation, arity),
                meck:expect(Module, Function, Implementation)
            end,
            Expectations
        ),
        mc_setup(Rest, [Module | Acc])
    catch
        Class:Reason:Stack ->
            meck:unload(Module),
            erlang:raise(Class, Reason, Stack)
    end.

mc_cleanup(Modules) -> lists:foreach(fun meck:unload/1, Modules).
