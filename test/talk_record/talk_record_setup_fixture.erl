-module(talk_record_setup_fixture).
-export([prepare/3, mocks/2, unload/1]).

%% EUnit does not invoke teardown when setup fails after allocating State.
prepare(State, Prepare, Cleanup) ->
    try
        Prepare()
    catch
        Class:Reason:Stack ->
            try
                Cleanup(State)
            catch
                CleanupClass:CleanupReason:CleanupStack ->
                    error(
                        {fixture_cleanup_failed, {Class, Reason, Stack},
                            {CleanupClass, CleanupReason, CleanupStack}}
                    )
            end,
            erlang:raise(Class, Reason, Stack)
    end.

mocks(Modules, Configure) ->
    acquire(Modules, Configure, #{}).

acquire([], Configure, Owners) ->
    ok = Configure(),
    Owners;
acquire([Module | Rest], Configure, Owners) ->
    ok = meck:new(Module, [passthrough, unstick, no_link]),
    Pid = whereis(mock_name(Module)),
    true = is_pid(Pid),
    prepare(
        {Module, Pid},
        fun() -> acquire(Rest, Configure, Owners#{Module => Pid}) end,
        fun unload_owner/1
    ).

unload(Owners) ->
    unload_all(maps:to_list(Owners)).

unload_all([]) ->
    ok;
unload_all([Owner | Rest]) ->
    try
        unload_owner(Owner)
    after
        unload_all(Rest)
    end.

unload_owner({Module, Pid}) ->
    case whereis(mock_name(Module)) of
        Pid -> meck:unload(Module);
        undefined -> ok;
        _ -> error({handler_mock_ownership_lost, Module})
    end.

mock_name(Module) -> list_to_atom(atom_to_list(Module) ++ "_meck").
