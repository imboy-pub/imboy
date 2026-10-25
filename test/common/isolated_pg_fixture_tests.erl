-module(isolated_pg_fixture_tests).
-include_lib("eunit/include/eunit.hrl").
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

dead_peer_preserves_call_failure_test() ->
    with_peer({ok, unused}, normal, fun(Peer) ->
        ok = gen_server:stop(Peer),
        TestFun = fun() -> unreachable end,
        ?assertMatch(
            {exit,
                {noproc,
                    {gen_server, call, [
                        Peer, {call, isolated_pg_fixture, run_case, [TestFun]}, 1000
                    ]}},
                [_ | _]},
            capture(fun() -> isolated_pg_fixture:run(TestFun, 21) end)
        ),
        {timeout, 21, Execute} = isolated_pg_fixture:test(fun() -> [] end, 21),
        ?assertMatch({exit, {noproc, {gen_server, call, _}}, [_ | _]}, capture(Execute))
    end).

remote_exception_preserved_test_() ->
    [
        {atom_to_list(Class), fun() ->
            {Class, Reason, Stack} = capture(fun() -> apply(erlang, Class, [business_failure]) end),
            with_peer({Class, {Reason, Stack}}, normal, fun(Peer) ->
                TestFun = fun() -> unused end,
                Expected = capture(fun() ->
                    peer:call(Peer, isolated_pg_fixture, run_case, [TestFun], 1000)
                end),
                ?assertMatch({Class, Reason, [_ | _]}, Expected),
                ok = meck:expect(peer, stop, fun(Dest) ->
                    ok = gen_server:stop(Dest),
                    meck:passthrough([Dest])
                end),
                ?assertEqual(
                    Expected,
                    capture(fun() -> isolated_pg_fixture:run(TestFun, 21) end)
                ),
                ?assertNot(is_process_alive(Peer))
            end)
        end}
     || Class <- [error, exit, throw]
    ].

successful_call_cleans_up_test() ->
    with_peer({ok, {result, 42}}, normal, fun(Peer) ->
        ?assertEqual({result, 42}, isolated_pg_fixture:run(fun() -> unused end, 21)),
        ?assertNot(is_process_alive(Peer))
    end).

successful_call_dead_cleanup_fails_test() ->
    with_peer({ok, success}, normal, fun(Peer) ->
        ok = meck:expect(peer, stop, fun(Dest) ->
            ok = gen_server:stop(Dest),
            meck:passthrough([Dest])
        end),
        ?assertExit(noproc, isolated_pg_fixture:run(fun() -> unused end, 21)),
        ?assertEqual(1, meck:num_calls(peer, stop, [Peer]))
    end).

other_cleanup_failure_propagates_test_() ->
    [
        {atom_to_list(Outcome), fun() ->
            Reply =
                case Outcome of
                    success ->
                        {ok, success};
                    failure ->
                        {Class, Reason, Stack} = capture(fun() -> error(business_failure) end),
                        {Class, {Reason, Stack}}
                end,
            with_peer(Reply, cleanup_failed, fun(_) ->
                ?assertExit(cleanup_failed, isolated_pg_fixture:run(fun() -> unused end, 21))
            end)
        end}
     || Outcome <- [success, failure]
    ].

with_peer(Reply, StopReason, Test) ->
    {ok, Peer} = gen_server:start(?MODULE, {Reply, StopReason}, []),
    try
        ok = meck:new(peer, [passthrough, unstick, no_link]),
        try
            ok = meck:expect(peer, start_link, fun(_) -> {ok, Peer, node()} end),
            Test(Peer)
        after
            meck:unload(peer)
        end
    after
        exit(Peer, kill)
    end.

capture(Fun) ->
    try Fun() of
        Result -> {returned, Result}
    catch
        Class:Reason:Stack -> {Class, Reason, Stack}
    end.

init(State) -> {ok, State}.
handle_call({call, isolated_pg_fixture, run_case, [_]}, _From, {Reply, _} = State) ->
    {reply, Reply, State}.
handle_cast(_Message, State) -> {noreply, State}.
terminate(_Reason, {_, normal}) -> ok;
terminate(_Reason, {_, StopReason}) -> exit(StopReason).
