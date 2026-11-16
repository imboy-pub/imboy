-module(group_invite_concurrency_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-export([check/3]).

%% Default entry uses the fully migrated disposable marker database, without
%% skip semantics. check/3 also runs the identical assertions against three
%% connections to an explicitly prepared inttest_ database (target-SQL evidence).
%% SQL, transaction boundaries, membership writes and generation rows are real.
%% Only connection routing, the post-commit cache notification and a barrier
%% after the real generation-close statement are intercepted.
-define(OWNER, 99013001).
-define(INVITER, 99013002).
-define(JOINER, 99013003).
-define(GROUPS, [99013011, 99013012, 99013013]).
-define(CONNECTION, {?MODULE, connection}).
-define(BARRIER, {?MODULE, barrier}).

group_invite_concurrency_test_() ->
    {timeout, 900, {setup, fun setup/0, fun cleanup/1, fun(S) ->
        {timeout, 60, ?_test(check(maps:get(conn, S), maps:get(a, S), maps:get(b, S)))}
    end}}.

setup() ->
    S = inttest_marker_db:provision(#{
        env_prefix => <<"GROUP_INVITE_INTTEST">>,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }),
    Server = maps:get(server, S),
    Opts = maps:merge(
        (maps:with([host, port, username, password], Server))#{database => maps:get(db, S)},
        maps:get(connect_extra, Server, #{})
    ),
    try
        {ok, A} = inttest_marker_db:safe_connect(Opts),
        try
            {ok, B} = inttest_marker_db:safe_connect(Opts),
            S#{a => A, b => B}
        catch ClassB:ReasonB:StackB ->
            epgsql:close(A),
            erlang:raise(ClassB, ReasonB, StackB)
        end
    catch Class:Reason:Stack ->
        inttest_marker_db:release(S),
        erlang:raise(Class, Reason, Stack)
    end.

cleanup(S) ->
    epgsql:close(maps:get(a, S)),
    epgsql:close(maps:get(b, S)),
    inttest_marker_db:release(S).

check(Observer, A, B) ->
    ?assertEqual(3, length(lists:usort([Observer, A, B]))),
    [#{<<"db">> := <<"inttest_", _/binary>> = Db}] =
        rows(Observer, <<"SELECT current_database() AS db">>, []),
    [Db] = lists:usort([
        maps:get(<<"db">>, hd(rows(C, <<"SELECT current_database() AS db">>, [])))
     || C <- [A, B]
    ]),
    case elib_tsid:readiness() of
        not_initialized -> elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3});
        ready -> ok
    end,
    SqlDriver = application:get_env(imboy, sql_driver),
    application:set_env(imboy, sql_driver, pgsql),
    try
        seed(Observer),
        try
            install_hooks(),
            race(Observer, A, B, 99013011, invite),
            race(Observer, A, B, 99013012, public),
            race(Observer, A, B, 99013013, enterprise_remove),
            ok
        after
            unload_hooks(),
            %% A killed caller does not own the epgsql connection process.
            %% Release any leftover transaction before observer-side cleanup.
            [(catch elib_pg:query(C, <<"ROLLBACK">>, [])) || C <- [A, B]],
            cleanup_rows(Observer)
        end
    after
        case SqlDriver of
            {ok, Value} -> application:set_env(imboy, sql_driver, Value);
            undefined -> application:unset_env(imboy, sql_driver)
        end
    end.

install_hooks() ->
    meck:new(pooler, [passthrough, no_link]),
    meck:expect(pooler, take_member, fun(Driver) ->
        case get(?CONNECTION) of
            undefined -> meck:passthrough([Driver]);
            C -> C
        end
    end),
    meck:expect(pooler, return_member, fun(Driver, C) ->
        case get(?CONNECTION) of
            C -> ok;
            _ -> meck:passthrough([Driver, C])
        end
    end),
    meck:expect(pooler, return_member, fun(Driver, C, Status) ->
        case get(?CONNECTION) of
            C -> ok;
            _ -> meck:passthrough([Driver, C, Status])
        end
    end),
    meck:new(group_ds, [passthrough, no_link]),
    meck:expect(group_ds, join, fun(_, _) -> ok end),
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, execute, fun(C, Sql, Params) ->
        Result = meck:passthrough([C, Sql, Params]),
        case {get(?BARRIER), binary:match(iolist_to_binary(Sql),
            <<"UPDATE public.group_member_generation g ">>)} of
            {{Parent, Ref}, {_, _}} ->
                ?assertMatch({ok, _}, Result),
                Parent ! {Ref, generation_closed},
                receive {Ref, release} -> ok after 10000 -> error(barrier_timeout) end;
            _ -> ok
        end,
        Result
    end).

unload_hooks() ->
    [meck:unload(M) || M <- [elib_pg, group_ds, pooler], lists:member(M, meck:mocked())],
    ok.

race(Observer, A, B, Gid, Mode) ->
    Parent = self(),
    Ref = make_ref(),
    [#{<<"pid">> := APid}] = rows(A, <<"SELECT pg_backend_pid() AS pid">>, []),
    [#{<<"pid">> := BPid}] = rows(B, <<"SELECT pg_backend_pid() AS pid">>, []),
    Leaving = worker(A, Parent, Ref, fun(C) ->
        put(?BARRIER, {Parent, Ref}),
        case Mode of
            enterprise_remove ->
                enterprise_group_repo:deactivate_member_tx(C, Gid, ?INVITER, <<"enterprise_remove">>);
            _ -> group_member_ds:leave(C, ?INVITER, Gid, ?INVITER)
        end
    end),
    try
        receive {Ref, generation_closed} -> ok after 10000 -> error(leave_barrier_missing) end,
        Joining = worker(B, Parent, Ref, fun(C) ->
            Options = case Mode of
                public -> #{public_self_join => true};
                _ -> #{invite_issuer => ?INVITER}
            end,
            group_member_ds:join_group(C, <<"qrcode">>, ?JOINER, Gid, Options)
        end),
        try
            %% Assert the wait location, not merely a scheduling delay. The old
            %% order waits on gm/seq instead; releasing A then forms a deadlock.
            wait_group_lock(Observer, APid, BPid, 250),
            element(1, Leaving) ! {Ref, release},
            LeaveResult = result(Leaving, Ref),
            JoinResult = result(Joining, Ref),
            assert_results(Observer, Gid, Mode, LeaveResult, JoinResult)
        after
            element(1, Leaving) ! {Ref, release},
            stop_worker(Joining)
        end
    after
        element(1, Leaving) ! {Ref, release},
        stop_worker(Leaving)
    end.

worker(C, Parent, Ref, Work) ->
    spawn_monitor(fun() ->
        put(?CONNECTION, C),
        Result = try elib_pg:with_tx(fun(Conn) ->
            rows(Conn, <<"SET LOCAL statement_timeout='10s'">>, []),
            Work(Conn)
        end) catch Class:Reason:Stack -> {worker_exception, Class, Reason, Stack} end,
        Parent ! {Ref, self(), Result}
    end).

result({Pid, _}, Ref) ->
    receive {Ref, Pid, Value} -> Value after 12000 -> error(worker_result_timeout) end.

stop_worker({Pid, Monitor}) ->
    receive
        {'DOWN', Monitor, process, Pid, normal} -> ok;
        {'DOWN', Monitor, process, Pid, Reason} -> error({worker_failed, Reason})
    after 12000 ->
        exit(Pid, kill),
        erlang:demonitor(Monitor, [flush]),
        error(worker_exit_timeout)
    end.

wait_group_lock(_, _, _, 0) -> error(group_lock_wait_not_observed);
wait_group_lock(C, APid, BPid, Attempts) ->
    case rows(C,
        <<"SELECT query FROM pg_stat_activity WHERE pid=$1 AND $2=ANY(pg_blocking_pids(pid))">>,
        [BPid, APid]) of
        [#{<<"query">> := Query}] ->
            ?assertNotEqual(nomatch, binary:match(Query, <<"SELECT id FROM public.\"group\"">>)),
            ?assertNotEqual(nomatch, binary:match(Query, <<"FOR UPDATE">>));
        [] ->
            timer:sleep(20),
            wait_group_lock(C, APid, BPid, Attempts - 1)
    end.

assert_results(C, Gid, Mode, LeaveResult, JoinResult) ->
    case Mode of
        enterprise_remove -> ?assertEqual({ok, true}, LeaveResult);
        _ -> ?assertMatch({ok, 1, _}, LeaveResult)
    end,
    Count = case Mode of
        public -> ?assertEqual({ok, 2}, JoinResult), 2;
        _ -> ?assertEqual({error, invalid_group_invitation}, JoinResult), 1
    end,
    ?assertEqual([#{<<"member_count">> => Count}],
        rows(C, <<"SELECT member_count FROM public.\"group\" WHERE id=$1">>, [Gid])),
    ?assertEqual([#{<<"end_seq">> => 10}], rows(C,
        <<"SELECT end_seq FROM public.group_member_generation WHERE group_id=$1 AND user_id=$2">>,
        [Gid, ?INVITER])),
    Expected = case Mode of public -> [#{<<"start_seq">> => 11, <<"end_seq">> => null}]; _ -> [] end,
    ?assertEqual(Expected, rows(C,
        <<"SELECT start_seq,end_seq FROM public.group_member_generation WHERE group_id=$1 AND user_id=$2">>,
        [Gid, ?JOINER])),
    ?assertEqual(Count, length(rows(C,
        <<"SELECT user_id FROM public.group_member WHERE group_id=$1 AND status=1">>, [Gid]))).

seed(C) ->
    rows(C, <<"BEGIN">>, []),
    try
        rows(C, <<"INSERT INTO public.\"user\"(id,account,password,reg_ip,reg_cosv) VALUES "
            "($1,'invite-lock-owner','synthetic','127.0.0.1','synthetic'),"
            "($2,'invite-lock-inviter','synthetic','127.0.0.1','synthetic'),"
            "($3,'invite-lock-joiner','synthetic','127.0.0.1','synthetic')">>,
            [?OWNER, ?INVITER, ?JOINER]),
        lists:foreach(fun(Gid) ->
            {Type, Limit} = case Gid of 99013012 -> {1, 1}; _ -> {2, 3} end,
            rows(C, <<"INSERT INTO public.\"group\"(id,owner_uid,creator_uid,title,type,join_limit,member_count) "
                "VALUES($1,$2,$2,'synthetic invitation lock regression',$3,$4,2)">>,
                [Gid, ?OWNER, Type, Limit]),
            rows(C, <<"INSERT INTO public.group_member(id,group_id,user_id,role,status) "
                "VALUES($1,$2,$3,4,1),($4,$2,$5,1,1)">>,
                [Gid * 10, Gid, ?OWNER, Gid * 10 + 1, ?INVITER]),
            rows(C, <<"INSERT INTO public.group_member_generation(group_id,user_id,start_seq) "
                "VALUES($1,$2,1),($1,$3,1)">>, [Gid, ?OWNER, ?INVITER]),
            rows(C, <<"INSERT INTO public.msg_store_seq(conv_key,seq) VALUES('c2g:'||$1::bigint::text,10)">>, [Gid])
        end, ?GROUPS),
        rows(C, <<"COMMIT">>, []),
        ok
    catch Class:Reason:Stack ->
        rows(C, <<"ROLLBACK">>, []),
        erlang:raise(Class, Reason, Stack)
    end.

cleanup_rows(C) ->
    [rows(C, <<"DELETE FROM public.", Table/binary, " WHERE group_id=ANY($1::bigint[])">>, [?GROUPS])
     || Table <- [<<"group_log">>, <<"group_member_generation">>, <<"group_member">>]],
    rows(C, <<"DELETE FROM public.msg_store_seq WHERE conv_key=ANY($1::varchar[])">>,
        [[<<"c2g:", (integer_to_binary(Gid))/binary>> || Gid <- ?GROUPS]]),
    rows(C, <<"DELETE FROM public.\"group\" WHERE id=ANY($1::bigint[])">>, [?GROUPS]),
    rows(C, <<"DELETE FROM public.\"user\" WHERE id=ANY($1::bigint[])">>, [[?OWNER, ?INVITER, ?JOINER]]),
    ok.

rows(C, Sql, Params) ->
    {ok, Rows} = elib_pg:query(C, Sql, Params),
    Rows.
