-module(conversation_preference_concurrency_pg_tests).
-include_lib("eunit/include/eunit.hrl").

-define(UID, 881471).
-define(PEER, 881472).
-define(OTHER_PEER, 881473).
-define(GID, 881475).
-define(GENERATION, 881476).

conversation_preference_concurrency_test_() ->
    {setup, fun eunit_runner:eunit_setup_with_db/0,
        fun
            ({ok, Conn}) -> eunit_runner:eunit_cleanup_db(Conn);
            ({error, _}) -> ok
        end, fun
            ({ok, Conn}) -> {timeout, 90, ?_test(with_fixture(Conn))};
            ({error, _}) -> []
        end}.

with_fixture(Conn) ->
    cleanup_rows(Conn),
    try
        meck:new(msg_s2c_ds, [no_link]),
        meck:expect(
            msg_s2c_ds,
            send,
            fun(?UID, [?UID], _Action, <<>>, null, _Payload, no_save) -> ok end
        ),
        meck:new(imboy_cache, [passthrough, no_link]),
        meck:expect(imboy_cache, invalidate, fun(_Key) -> ok end),
        seed_rows(Conn),
        cas_first_write(Conn),
        different_conversations_first_write(Conn),
        empty_no_op(Conn),
        stale_generation(Conn),
        lists:foreach(
            fun({Action, Type, Cid}) -> same_target(Conn, Action, Type, Cid) end,
            [
                {pin, <<"c2c">>, ?PEER},
                {delete, <<"c2c">>, ?PEER},
                {pin, <<"c2g">>, ?GID},
                {delete, <<"c2g">>, ?GID}
            ]
        )
    after
        meck:unload(msg_s2c_ds),
        meck:unload(imboy_cache),
        cleanup_rows(Conn)
    end.

seed_rows(Conn) ->
    {ok, _} = elib_pg:execute(
        Conn,
        <<
            "INSERT INTO \"user\" (id, account, nickname, password, reg_ip, reg_cosv) VALUES "
            "($1,'w09-pref-concurrency-uid','synthetic','synthetic','127.0.0.1','synthetic'),"
            "($2,'w09-pref-concurrency-peer','synthetic','synthetic','127.0.0.1','synthetic'),"
            "($3,'w09-pref-concurrency-other','synthetic','synthetic','127.0.0.1','synthetic')"
        >>,
        [?UID, ?PEER, ?OTHER_PEER]
    ),
    {ok, _} = elib_pg:execute(
        Conn,
        <<
            "INSERT INTO group_member_generation "
            "(id, group_id, user_id, generation_no, start_seq) VALUES ($1, $2, $3, 1, 100)"
        >>,
        [?GENERATION, ?GID, ?UID]
    ),
    ok.

cleanup_rows(Conn) ->
    clear_preferences(Conn),
    {ok, _} = elib_pg:execute(
        Conn,
        <<
            "DELETE FROM group_member_generation WHERE group_id = $1 AND user_id = $2"
        >>,
        [?GID, ?UID]
    ),
    {ok, _} = elib_pg:execute(
        Conn,
        <<"DELETE FROM \"user\" WHERE id = ANY($1)">>,
        [[?UID, ?PEER, ?OTHER_PEER]]
    ),
    ok.

clear_preferences(Conn) ->
    lists:foreach(
        fun(Sql) -> {ok, _} = elib_pg:execute(Conn, Sql, [?UID]) end,
        [
            <<"DELETE FROM user_setting WHERE user_id = $1">>,
            <<"DELETE FROM conversation_pin WHERE user_id = $1">>,
            <<"DELETE FROM conversation_delete WHERE user_id = $1">>
        ]
    ).

cas_first_write(Conn) ->
    clear_preferences(Conn),
    Uk3 = user_setting_ds:conversation_uk3(<<"c2c">>, ?UID, ?PEER),
    Write = fun() -> user_setting_ds:set_manual_pending(?UID, Uk3, true, 0) end,
    ?assertEqual([], setting_rows(Conn)),
    Results = race(Conn, [Write, Write]),
    Winners = [Entry || {ok, Entry} <- Results],
    Conflicts = [Entry || {conflict, Entry} <- Results],
    ?assertMatch([#{<<"value">> := true, <<"version">> := 1}], Winners),
    ?assertEqual(Winners, Conflicts),
    [Winner] = Winners,
    ?assertEqual(#{Uk3 => Winner}, pending_map(Conn)),
    ?assertEqual({ok, Winner}, user_setting_ds:set_manual_pending(?UID, Uk3, true, 1)),
    ?assertEqual(#{Uk3 => Winner}, pending_map(Conn)).

different_conversations_first_write(Conn) ->
    clear_preferences(Conn),
    First = user_setting_ds:conversation_uk3(<<"c2c">>, ?UID, ?PEER),
    Second = user_setting_ds:conversation_uk3(<<"c2c">>, ?UID, ?OTHER_PEER),
    ?assertEqual([], setting_rows(Conn)),
    Results = race(Conn, [
        fun() -> user_setting_ds:set_manual_pending(?UID, First, true, 0) end,
        fun() -> user_setting_ds:set_manual_pending(?UID, Second, false, 0) end
    ]),
    ?assertMatch(
        [
            {ok, #{<<"value">> := true, <<"version">> := 1}},
            {ok, #{<<"value">> := false, <<"version">> := 1}}
        ],
        Results
    ),
    [{ok, FirstEntry}, {ok, SecondEntry}] = Results,
    ?assertEqual(#{First => FirstEntry, Second => SecondEntry}, pending_map(Conn)).

empty_no_op(Conn) ->
    clear_preferences(Conn),
    Before = meck:num_calls(msg_s2c_ds, send, '_'),
    ?assertEqual(
        [ok, ok],
        race(Conn, [
            fun() -> conversation_pin_logic:unpin(?UID, ?PEER, <<"c2c">>) end,
            fun() -> conversation_logic:restore(?UID, ?PEER, <<"c2c">>) end
        ])
    ),
    ?assertEqual([], setting_rows(Conn)),
    ?assertEqual(Before, meck:num_calls(msg_s2c_ds, send, '_')).

stale_generation(Conn) ->
    clear_preferences(Conn),
    {ok, 1} = conversation_pin_repo:pin_tx(Conn, ?UID, ?GID, <<"c2g">>, 0),
    {ok, 1} = conversation_delete_repo:mark_deleted_tx(Conn, ?UID, ?GID, <<"c2g">>, 0),
    Before = meck:num_calls(msg_s2c_ds, send, '_'),
    ?assertEqual(
        [ok, ok],
        race(Conn, [
            fun() -> conversation_pin_logic:unpin(?UID, ?GID, <<"c2g">>) end,
            fun() -> conversation_logic:restore(?UID, ?GID, <<"c2g">>) end
        ])
    ),
    ?assertEqual([], setting_rows(Conn)),
    lists:foreach(
        fun(Action) ->
            ?assertEqual([#{<<"generation_id">> => 0}], business_rows(Conn, Action))
        end,
        [pin, delete]
    ),
    ?assertEqual(Before, meck:num_calls(msg_s2c_ds, send, '_')),
    ?assertEqual(
        [ok, ok],
        race(Conn, [
            fun() -> conversation_pin_logic:pin(?UID, ?GID, <<"c2g">>) end,
            fun() -> conversation_logic:delete(?UID, ?GID, <<"c2g">>) end
        ])
    ),
    lists:foreach(
        fun(Action) ->
            ?assertEqual([#{<<"generation_id">> => ?GENERATION}], business_rows(Conn, Action))
        end,
        [pin, delete]
    ),
    Uk3 = user_setting_ds:conversation_uk3(<<"c2g">>, ?UID, ?GID),
    ?assertMatch(#{<<"value">> := false, <<"version">> := 2}, entry(Conn, Uk3)),
    ?assertEqual(Before + 2, meck:num_calls(msg_s2c_ds, send, '_')).

same_target(Conn, Action, Type, Cid) ->
    clear_preferences(Conn),
    Uk3 = user_setting_ds:conversation_uk3(Type, ?UID, Cid),
    {Apply, Undo, AppliedEvent, UndoneEvent} = actions(Action, Type, Cid),
    AppliedBefore = notifications(AppliedEvent, Type, Cid),
    UndoneBefore = notifications(UndoneEvent, Type, Cid),
    ?assertEqual([], setting_rows(Conn)),
    ?assertEqual([ok, ok], race(Conn, [Apply, Apply])),
    ?assertMatch(#{<<"value">> := false, <<"version">> := 1}, entry(Conn, Uk3)),
    ?assertEqual([#{<<"generation_id">> => generation(Type)}], business_rows(Conn, Action)),
    ?assertEqual(AppliedBefore + 1, notifications(AppliedEvent, Type, Cid)),
    AfterApply = pending_map(Conn),
    ?assertEqual(ok, Apply()),
    ?assertEqual(AfterApply, pending_map(Conn)),
    ?assertEqual(AppliedBefore + 1, notifications(AppliedEvent, Type, Cid)),
    ?assertEqual([ok, ok], race(Conn, [Undo, Undo])),
    ?assertEqual([], business_rows(Conn, Action)),
    ?assertMatch(#{<<"value">> := false, <<"version">> := 2}, entry(Conn, Uk3)),
    ?assertEqual(UndoneBefore + 1, notifications(UndoneEvent, Type, Cid)),
    AfterUndo = pending_map(Conn),
    ?assertEqual(ok, Undo()),
    ?assertEqual(AfterUndo, pending_map(Conn)),
    ?assertEqual(UndoneBefore + 1, notifications(UndoneEvent, Type, Cid)).

actions(pin, Type, Cid) ->
    {
        fun() -> conversation_pin_logic:pin(?UID, Cid, Type) end,
        fun() -> conversation_pin_logic:unpin(?UID, Cid, Type) end,
        <<"conversation_pinned">>,
        <<"conversation_unpinned">>
    };
actions(delete, Type, Cid) ->
    {
        fun() -> conversation_logic:delete(?UID, Cid, Type) end,
        fun() -> conversation_logic:restore(?UID, Cid, Type) end,
        <<"conversation_deleted">>,
        <<"conversation_restored">>
    }.

generation(<<"c2c">>) -> 0;
generation(<<"c2g">>) -> ?GENERATION.

notifications(Event, Type, Cid) ->
    Payload = #{<<"conversation_id">> => Cid, <<"conversation_type">> => Type},
    meck:num_calls(msg_s2c_ds, send, [?UID, [?UID], Event, <<>>, null, Payload, no_save]).

business_rows(Conn, Action) ->
    Sql =
        case Action of
            pin -> <<"SELECT generation_id FROM conversation_pin WHERE user_id = $1">>;
            delete -> <<"SELECT generation_id FROM conversation_delete WHERE user_id = $1">>
        end,
    {ok, Rows} = elib_pg:query(Conn, Sql, [?UID]),
    Rows.

setting_rows(Conn) ->
    {ok, Rows} = elib_pg:query(
        Conn,
        <<"SELECT setting FROM user_setting WHERE user_id = $1">>,
        [?UID]
    ),
    Rows.

pending_map(Conn) ->
    [#{<<"setting">> := Json}] = setting_rows(Conn),
    #{<<"manual_pending">> := Pending} = jsone:decode(Json, [{object_format, map}]),
    Pending.

entry(Conn, Uk3) ->
    maps:get(Uk3, pending_map(Conn)).

race(Conn, Functions) ->
    Parent = self(),
    RaceRef = make_ref(),
    Workers = [
        spawn_monitor(fun() -> worker(Parent, RaceRef, Work) end)
     || Work <- Functions
    ],
    try
        ok = epgsql:with_transaction(
            Conn,
            fun(LockConn) ->
                ok = user_setting_repo:lock_preferences_tx(LockConn, ?UID),
                lists:foreach(fun({Pid, _}) -> Pid ! {RaceRef, start} end, Workers),
                wait_blocked(LockConn, length(Workers), 250)
            end,
            [{reraise, true}]
        ),
        [worker_result(RaceRef, Pid) || {Pid, _} <- Workers]
    after
        lists:foreach(fun({Pid, _}) -> Pid ! {RaceRef, stop} end, Workers),
        Exits = [worker_exit(Pid, Monitor) || {Pid, Monitor} <- Workers],
        ?assertEqual(lists:duplicate(length(Workers), normal), Exits)
    end.

worker(Parent, RaceRef, Work) ->
    receive
        {RaceRef, start} ->
            Result =
                try
                    Work()
                catch
                    Class:Reason:Stacktrace -> {worker_exception, Class, Reason, Stacktrace}
                end,
            Parent ! {RaceRef, self(), Result};
        {RaceRef, stop} ->
            ok
    after 10000 -> error(worker_start_timeout)
    end.

worker_result(RaceRef, Pid) ->
    receive
        {RaceRef, Pid, Result} -> Result
    after 10000 -> error({worker_result_timeout, Pid})
    end.

worker_exit(Pid, Monitor) ->
    receive
        {'DOWN', Monitor, process, Pid, Reason} -> Reason
    after 10000 -> error({worker_exit_timeout, Pid})
    end.

wait_blocked(_Conn, _Expected, 0) ->
    error(preference_lock_barrier_timeout);
wait_blocked(Conn, Expected, Attempts) ->
    {ok, [#{<<"count">> := Count}]} = elib_pg:query(
        Conn,
        <<
            "SELECT count(DISTINCT waiting.pid) AS count FROM pg_locks held "
            "JOIN pg_locks waiting ON waiting.locktype = held.locktype "
            "AND waiting.database = held.database AND waiting.classid = held.classid "
            "AND waiting.objid = held.objid AND waiting.objsubid = held.objsubid "
            "WHERE held.pid = pg_backend_pid() AND held.locktype = 'advisory' "
            "AND held.granted AND NOT waiting.granted"
        >>,
        []
    ),
    case Count of
        Expected ->
            ok;
        _ ->
            timer:sleep(20),
            wait_blocked(Conn, Expected, Attempts - 1)
    end.
