%%% Ownership authorization must be part of the database write predicate.
-module(group_repo_owner_guard_tests).
-include_lib("eunit/include/eunit.hrl").

owner_guard_test_() ->
    [
        {timeout, 30, fun() -> verify_guard(1) end},
        {timeout, 30, fun() -> verify_guard(0) end}
    ].

verify_guard(AffectedRows) ->
    try
        meck:new(elib_pg, [non_strict, no_link]),
        meck:expect(elib_pg, execute, fun(Conn, Sql, Params) ->
            ?assertEqual(self(), Conn),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"AND owner_uid = $4">>)),
            ?assertMatch([200, _, 1, 100], Params),
            {ok, AffectedRows}
        end),
        %% discussion 访问锁（791b8817）：无 discussion 绑定语义 → 放行
        meck:expect(elib_pg, query, fun
            (Conn, <<"LOCK TABLE public.channel", _/binary>>, []) ->
                ?assertEqual(self(), Conn),
                {ok, []};
            (Conn, <<"SELECT id FROM public.channel", _/binary>>, _) ->
                {ok, []};
            (Conn, <<"SELECT id FROM public.\"group\"", _/binary>>, _) ->
                {ok, []}
        end),
        Result = group_repo:update_owner_tx(self(), 1, 200, 100),
        case AffectedRows of
            1 -> ?assertEqual(ok, Result);
            0 -> ?assertMatch({error, Message} when is_binary(Message), Result)
        end,
        ?assertEqual(1, meck:num_calls(elib_pg, execute, 3))
    after
        catch meck:unload(elib_pg)
    end.
