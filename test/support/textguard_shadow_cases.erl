-module(textguard_shadow_cases).

-include_lib("eunit/include/eunit.hrl").
-export([
    manifest/0,
    setup/0,
    cleanup/1,
    reset/2,
    worker/1,
    start_owned/1,
    restart/1,
    await/1,
    idle/0,
    counter/1,
    counters/0,
    expect_category/3,
    real_response/2,
    snapshot/2,
    semantics/1,
    with_enhanced/1,
    with_hang/2,
    config/1,
    rows/0,
    digest/0,
    receipt/2,
    filter_sender/1,
    assert_private_event/2,
    grpc_stream_lifecycle/1,
    running_workers/1,
    log/2
]).

-define(F, textguard_business_fixture).

manifest() ->
    Local = [
        digest_encoding, digest_identity, classifier_matrix, unavailable_snapshot, mode_defaults
    ],
    Real =
        [
            {business, Scene, Decision}
         || Scene <- [channel_message, moment_post, profile_field],
            Decision <- [allow, review, block]
        ] ++
            [
                compat_vectors,
                compat_inert,
                cached_version,
                package_identity,
                enhanced_witnesses,
                private_exclusion,
                facility_error,
                nonblocking,
                queue_bounds,
                admission_ttl,
                running_ttl,
                mode_legacy,
                mode_rust,
                input_bounds,
                mailbox_privacy,
                worker_unavailable
            ],
    [entry(http, Kind) || Kind <- Local] ++
        [entry(Transport, Kind) || Transport <- [http, grpc], Kind <- Real] ++
        [
            entry(http, grpc_log_filter),
            entry(grpc, grpc_stream_lifecycle),
            entry(grpc, grpc_guardian_failure),
            entry(grpc, grpc_coordinator_restart),
            entry(grpc, grpc_total_deadline),
            entry(grpc, grpc_connection_failure)
        ].

entry(Transport, Kind) ->
    #{
        id => iolist_to_binary(io_lib:format("~p/~p", [Transport, Kind])),
        transport => Transport,
        kind => Kind,
        acceptance => acceptance(Kind)
    }.

acceptance(digest_encoding) -> [<<"AC-084">>];
acceptance(digest_identity) -> [<<"AC-084">>];
acceptance(classifier_matrix) -> [<<"AC-084">>, <<"AC-085">>];
acceptance(cached_version) -> [<<"AC-085">>];
acceptance(package_identity) -> [<<"AC-084">>, <<"AC-085">>];
acceptance(enhanced_witnesses) -> [<<"AC-085">>];
acceptance(compat_vectors) -> [<<"AC-084">>];
acceptance(compat_inert) -> [<<"AC-084">>];
acceptance({business, _, _}) -> [<<"AC-084">>, <<"AC-086">>];
acceptance(_) -> [<<"AC-086">>].

setup() ->
    ?assertEqual(undefined, whereis(moderation_shadow)),
    Fixture = ?F:setup(),
    Holder = ets:new(tg21_owned_worker, [public, set]),
    State = #{fixture => Fixture, holder => Holder},
    try
        {ok, Pid} = moderation_shadow:start_link(),
        unlink(Pid),
        ets:insert(Holder, {worker, Pid}),
        ok = moderation_shadow:set_mode(legacy),
        semantics(<<"compat-v1">>),
        State
    catch
        Class:Reason:Stack ->
            cleanup(State),
            erlang:raise(Class, Reason, Stack)
    end.

cleanup(State) ->
    Holder = maps:get(holder, State),
    case ets:lookup(Holder, worker) of
        [{worker, Pid}] ->
            case is_process_alive(Pid) of
                true ->
                    try
                        moderation_shadow:set_mode(legacy)
                    catch
                        _:_ -> ok
                    end,
                    try
                        gen_server:stop(Pid, normal, 2000)
                    catch
                        _:_ -> ok
                    end;
                false ->
                    ok
            end;
        [] ->
            ok
    end,
    ets:delete(Holder),
    ?F:cleanup(maps:get(fixture, State)).

worker(State) -> ets:lookup_element(maps:get(holder, State), worker, 2).

start_owned(State) ->
    {ok, Pid} = moderation_shadow:start_link(),
    unlink(Pid),
    ets:insert(maps:get(holder, State), {worker, Pid}),
    ok.

restart(State) ->
    ok = start_owned(State),
    ok = moderation_shadow:set_mode(shadow).

reset(State, Transport) ->
    ok = moderation_shadow:set_mode(legacy),
    ?F:reset(maps:get(fixture, State), Transport),
    imboy_cache:flush({moderation_sensitive_words}),
    ok = moderation_shadow:set_mode(shadow),
    idle().

await(Predicate) -> await(Predicate, erlang:monotonic_time(millisecond) + 1900).
await(Predicate, Deadline) ->
    case Predicate() of
        true ->
            ok;
        false ->
            case erlang:monotonic_time(millisecond) < Deadline of
                true -> receive
                    after 10 -> await(Predicate, Deadline)
                    end;
                false -> error(shadow_bounded_poll_expired)
            end
    end.

idle() ->
    await(fun() ->
        #{queued := Queued, running := Running} = moderation_shadow:status(),
        Queued =:= 0 andalso Running =:= 0
    end).

counters() -> maps:get(counters, moderation_shadow:status()).
counter(Category) -> maps:get(Category, counters(), 0).

expect_category(Scene, Fields, Category) ->
    Snapshot = snapshot(Scene, Fields),
    Before = counter(Category),
    ?assertEqual(accepted, moderation_shadow:submit(Scene, Fields, Snapshot)),
    await(fun() -> counter(Category) =:= Before + 1 end),
    idle().

snapshot(Scene, Fields) ->
    {_, Snapshot} = moderation_policy:inspect_snapshot(Scene, Fields),
    Snapshot.

rows() ->
    [{Word, Category, Severity} || {_, Word, Category, Severity} <- textguard_golden:rows()] ++
        [{<<"comma,canonical">>, <<"test">>, <<"medium">>}].

digest() -> textguard_base_digest:digest(rows()).

real_response(Scene, Fields) ->
    Transport = moderation_detector_transport:select(),
    ?assert(lists:member(Transport, [moderation_detector_http, moderation_detector_grpc])),
    Id = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    {ok, Response} = Transport:check(
        #{request_id => Id, scene => Scene, fields => Fields}, #{request_id => Id}
    ),
    ?assertEqual(?F:export(), maps:get(ruleset_revision, Response)),
    ?assertEqual(true, maps:get(scan_complete, Response)),
    ?assert(byte_size(maps:get(build_id, Response)) > 0),
    Response.

semantics(Mode) ->
    Path = os:getenv("TG19_ENH_FILE"),
    {ok, Before} = file:read_file(Path),
    Enhancement = jsone:decode(Before),
    Modes = maps:from_list([
        {Scene, Mode}
     || Scene <-
            [<<"channel_message">>, <<"moment_post">>, <<"profile_field">>]
    ]),
    Changed = Enhancement#{<<"semantics_by_scene">> => Modes},
    case maps:get(<<"semantics_by_scene">>, Enhancement) =:= Modes of
        true ->
            ok;
        false ->
            ok = file:write_file(Path, jsone:encode(Changed)),
            ?F:republish(),
            ok
    end.

with_enhanced(Fun) ->
    ok = moderation_shadow:set_mode(legacy),
    semantics(<<"enhanced-v1">>),
    ok = moderation_shadow:set_mode(shadow),
    try
        Fun()
    after
        ok = moderation_shadow:set_mode(legacy),
        semantics(<<"compat-v1">>),
        ok = moderation_shadow:set_mode(shadow)
    end.

config(Overrides) ->
    {ok, Current} = application:get_env(imboy, moderation_detector),
    ok = application:set_env(
        imboy,
        moderation_detector,
        Overrides ++
            [
                Entry
             || {Key, _} = Entry <- Current,
                not lists:keymember(Key, 1, Overrides)
            ]
    ).

with_hang(grpc, Fun) ->
    textguard_fault_service:set(hang),
    config([{transport, grpc}, {grpc_channel, tg20_fault}, {timeout_ms, 5000}]),
    Fun(fun textguard_fault_service:count/0);
with_hang(http, Fun) ->
    {ok, Listener} = gen_tcp:listen(0, [
        binary,
        {active, false},
        {ip, {127, 0, 0, 1}},
        {reuseaddr, true},
        {backlog, 4}
    ]),
    {ok, Port} = inet:port(Listener),
    Table = ets:new(tg21_peer, [public, set]),
    ets:insert(Table, {requests, 0}),
    Acceptor = spawn(fun() -> accept(Listener, Table) end),
    config([
        {transport, http},
        {http_base_url, <<"http://127.0.0.1:", (integer_to_binary(Port))/binary>>},
        {timeout_ms, 5000}
    ]),
    try
        Fun(fun() -> ets:lookup_element(Table, requests, 2) end)
    after
        gen_tcp:close(Listener),
        exit(Acceptor, kill),
        lists:foreach(
            fun
                ({{child, Pid}, true}) -> exit(Pid, kill);
                (_) -> ok
            end,
            ets:tab2list(Table)
        ),
        ets:delete(Table)
    end.

accept(Listener, Table) ->
    case gen_tcp:accept(Listener) of
        {ok, Socket} ->
            Pid = spawn(fun() ->
                receive
                    {socket, Owned} ->
                        try
                            read_request(Owned, <<>>),
                            ets:update_counter(Table, requests, 1),
                            receive
                            after 5000 -> ok
                            end
                        after
                            gen_tcp:close(Owned)
                        end
                end
            end),
            ets:insert(Table, {{child, Pid}, true}),
            ok = gen_tcp:controlling_process(Socket, Pid),
            Pid ! {socket, Socket},
            accept(Listener, Table);
        {error, closed} ->
            ok
    end.

read_request(Socket, Buffer) when byte_size(Buffer) =< 131072 ->
    case binary:match(Buffer, <<"\r\n\r\n">>) of
        nomatch ->
            {ok, More} = gen_tcp:recv(Socket, 0, 1000),
            read_request(Socket, <<Buffer/binary, More/binary>>);
        {Offset, 4} ->
            <<Headers:Offset/binary, _:4/binary, Body/binary>> = Buffer,
            Lines = binary:split(string:lowercase(Headers), <<"\r\n">>, [global]),
            [Length] = [
                binary_to_integer(string:trim(Value))
             || Line <- Lines,
                [<<"content-length">>, Value] <- [binary:split(Line, <<":">>)]
            ],
            true = Length >= 0 andalso Length =< 98304,
            case Length - byte_size(Body) of
                Remaining when Remaining > 0 -> {ok, _} = gen_tcp:recv(Socket, Remaining, 1000);
                _ -> ok
            end
    end.

filter_sender(Fun) ->
    {Sender, Monitor} = spawn_monitor(Fun),
    receive
        {'DOWN', Monitor, process, Sender, normal} -> ok;
        {'DOWN', Monitor, process, Sender, Reason} -> error({filter_sender_failed, Reason})
    after 1900 ->
        exit(Sender, kill),
        await_down(Monitor, Sender),
        error(filter_sender_timeout)
    end.

assert_private_event(Event, Sensitive) ->
    Filtered = textguard_grpc_call:filter_log(Event, []),
    ?assertEqual([level, meta, msg], lists:sort(maps:keys(Filtered))),
    ?assertEqual({string, "textguard_grpc_stream_terminated"}, maps:get(msg, Filtered)),
    ?assertEqual(error, maps:get(level, Filtered)),
    Meta = maps:get(meta, Filtered),
    ?assertEqual([domain, pid, time], lists:sort(maps:keys(Meta))),
    ?assertEqual(self(), maps:get(pid, Meta)),
    ?assertEqual([imboy, textguard], maps:get(domain, Meta)),
    ?assert(is_integer(maps:get(time, Meta))),
    ?assertEqual(false, contains_sensitive(Filtered, Sensitive)),
    ?assertEqual(true, contains_sensitive(Event, Sensitive)).

grpc_stream_lifecycle(State) ->
    {module, textguard_grpc_call} = code:ensure_loaded(textguard_grpc_call),
    Target = {textguard_grpc_call, open_stream, 5},
    ?assertEqual({traced, false}, erlang:trace_info(Target, traced)),
    ?assertEqual(
        {traced, false}, erlang:trace_info({textguard_grpc_call, stream_handle, 3}, traced)
    ),
    Response = real_response(profile_field, [{<<"content">>, <<"ordinary">>}]),
    {ok, Conf} = application:get_env(imboy, moderation_detector),
    Sensitive = [?F:canary(), proplists:get_value(token, Conf)],
    ?assert(lists:all(fun(Value) -> is_binary(Value) andalso byte_size(Value) > 0 end, Sensitive)),
    Table = ets:new(tg21_stream_observations, [public, set]),
    {Tracer, TracerMonitor} = spawn_monitor(fun() -> stream_trace(Table) end),
    try
        ok = logger:add_handler(tg21_stream_capture, ?MODULE, #{
            level => all,
            config => #{table => Table, sensitive => Sensitive}
        }),
        try
            ?assert(
                erlang:trace_pattern(
                    Target, [{'_', [], [{return_trace}]}], [local]
                ) > 0
            ),
            ?assert(
                erlang:trace_pattern(
                    {textguard_grpc_call, stream_handle, 3},
                    [{'_', [], [{return_trace}]}],
                    [local]
                ) > 0
            ),
            erlang:trace(worker(State), true, [call, arity, set_on_spawn, {tracer, Tracer}]),
            try
                stream_rounds(State, Table, Response)
            after
                stop_stream_trace(State, Table)
            end
        after
            ok = logger:remove_handler(tg21_stream_capture)
        end
    after
        Tracer ! stop,
        await_down(TracerMonitor, Tracer),
        ets:delete(Table)
    end.

stream_rounds(State, Table, Response) ->
    log_probe(Table),
    Channel = gproc:where({n, l, {grpcbox_channel, tg20_fault}}),
    ?assert(is_pid(Channel)),
    lists:foreach(
        fun(Trigger) ->
            stream_cancel_case(Trigger, State, Table),
            stream_reuse(Table, Response),
            ?assertEqual(Channel, gproc:where({n, l, {grpcbox_channel, tg20_fault}}))
        end,
        [
            running_ttl,
            mode_switch,
            worker_kill,
            running_ttl,
            mode_switch,
            worker_kill,
            late_result,
            duplicate_result
        ]
    ),
    late_open(State, Table),
    stream_reuse(Table, Response),
    ?assertEqual(Channel, gproc:where({n, l, {grpcbox_channel, tg20_fault}})),
    ?assertEqual(36, observation(Table, total_calls)),
    log_probe(Table),
    ?assertEqual(0, observation(Table, leaks)).

stream_cancel_case(Trigger, State, Table) ->
    ok = moderation_shadow:set_mode(legacy),
    idle(),
    ets:match_delete(Table, {{stream, '_'}, '_'}),
    ets:insert(Table, {calls, 0}),
    with_hang(grpc, fun(Count) ->
        ok = moderation_shadow:set_mode(shadow),
        Before = counter(cancel_category(Trigger)),
        Fields = [{<<"content">>, ?F:canary()}],
        Snapshot = snapshot(profile_field, Fields),
        Started = erlang:monotonic_time(millisecond),
        submit_pair(Fields, Snapshot),
        await(fun() -> Count() =:= 2 andalso length(streams(Table)) =:= 2 end),
        Entries = streams(Table),
        ?assertEqual(
            [2],
            lists:usort([
                chatterbox_h2_stream_set:my_active_count(maps:get(streams, E))
             || {_, E} <- Entries
            ])
        ),
        Processes = stream_processes(Entries),
        ?assert(lists:all(fun is_process_alive/1, Processes)),
        ?assertEqual(2, maps:get(running, moderation_shadow:status())),
        Outers = running_workers(State),
        ?assertEqual(2, length(Outers)),
        assert_guard_monitors(State, Entries),
        Monitors = [{monitor(process, Pid), Pid} || Pid <- Processes],
        try
            trigger_cancel(Trigger, Outers, Entries, State),
            lists:foreach(fun({Ref, Pid}) -> await_down(Ref, Pid) end, Monitors),
            idle(),
            ?assert(erlang:monotonic_time(millisecond) - Started < 1900),
            await(fun() -> counter(cancel_category(Trigger)) =:= Before + 2 end),
            log_probe(Table),
            ?assertEqual(2, Count()),
            ?assertEqual(2, observation(Table, calls)),
            ?assert(lists:all(fun(Pid) -> not is_process_alive(Pid) end, Processes)),
            same_connection(Table, Entries),
            ?assertEqual(0, observation(Table, leaks))
        after
            lists:foreach(fun({Ref, _}) -> demonitor(Ref, [flush]) end, Monitors),
            ok = moderation_shadow:set_mode(legacy),
            idle()
        end
    end).

cancel_category(running_ttl) -> expired;
cancel_category(mode_switch) -> mode_switch;
cancel_category(worker_kill) -> worker_unavailable;
cancel_category(late_result) -> expired;
cancel_category(duplicate_result) -> comparable_match.

trigger_cancel(running_ttl, _, _, _) ->
    ok;
trigger_cancel(late_result, Outers, _, State) ->
    Before = counter(comparable_match),
    Coordinator = worker(State),
    ok = sys:suspend(Coordinator),
    try
        lists:foreach(fun(Pid) -> Coordinator ! {result, Pid, comparable_match} end, Outers),
        receive
        after 1100 -> ok
        end
    after
        ok = sys:resume(Coordinator)
    end,
    await(fun() -> maps:get(running, moderation_shadow:status()) =:= 0 end),
    ?assertEqual(Before, counter(comparable_match));
trigger_cancel(duplicate_result, Outers, _, State) ->
    Before = counter(comparable_match),
    lists:foreach(
        fun(Pid) ->
            worker(State) ! {result, Pid, comparable_match},
            worker(State) ! {result, Pid, comparable_match}
        end,
        Outers
    ),
    await(fun() -> counter(comparable_match) =:= Before + 2 end),
    BeforeMode = counter(mode_switch),
    ok = moderation_shadow:set_mode(legacy),
    ok = moderation_shadow:set_mode(rust),
    ?assertEqual(BeforeMode, counter(mode_switch));
trigger_cancel(Trigger, Outers, Entries, State) ->
    with_suspended_guards(Entries, fun() -> held_slot_cancel(Trigger, Outers, Entries, State) end).

held_slot_cancel(mode_switch, _, _, _) ->
    Before = counter(mode_switch),
    lists:foreach(
        fun(Mode) ->
            ok = moderation_shadow:set_mode(Mode),
            ?assertEqual(Before + 2, counter(mode_switch)),
            ?assertEqual(2, maps:get(running, moderation_shadow:status()))
        end,
        [legacy, rust, legacy]
    );
held_slot_cancel(worker_kill, Outers, Entries, State) ->
    Before = counter(worker_unavailable),
    kill_workers(Outers),
    await(fun() -> counter(worker_unavailable) =:= Before + 2 end),
    ?assertEqual(2, maps:get(running, moderation_shadow:status())),
    assert_guard_monitors(State, Entries).

kill_workers(Outers) ->
    lists:foreach(
        fun(Outer) ->
            Ref = monitor(process, Outer),
            exit(Outer, kill),
            await_down(Ref, Outer)
        end,
        Outers
    ).

with_suspended_guards(Entries, Fun) ->
    Guards = [maps:get(actor, Entry) || {_, Entry} <- Entries],
    Suspended = ets:new(tg21_suspended_guards, [set]),
    try
        lists:foreach(
            fun(Guard) ->
                true = erlang:suspend_process(Guard),
                ets:insert(Suspended, {Guard})
            end,
            Guards
        ),
        Fun()
    after
        lists:foreach(
            fun({Guard}) ->
                case is_process_alive(Guard) of
                    true -> erlang:resume_process(Guard);
                    false -> ok
                end
            end,
            ets:tab2list(Suspended)
        ),
        ets:delete(Suspended)
    end.

running_workers(State) ->
    Pid = worker(State),
    {monitors, Monitors} = process_info(Pid, monitors),
    {links, Links} = process_info(Pid, links),
    [Child || {process, Child} <- Monitors, is_pid(Child), lists:member(Child, Links)].

assert_guard_monitors(State, Entries) ->
    {monitors, Monitors} = process_info(worker(State), monitors),
    lists:foreach(
        fun({_, Entry}) ->
            ?assert(lists:member({process, maps:get(actor, Entry)}, Monitors))
        end,
        Entries
    ).

submit_pair(Fields, Snapshot) ->
    lists:foreach(
        fun(_) ->
            ?assertEqual(accepted, moderation_shadow:submit(profile_field, Fields, Snapshot))
        end,
        [first, second]
    ).

stream_processes(Entries) ->
    lists:append([[Stream, maps:get(actor, Entry)] || {{stream, Stream}, Entry} <- Entries]).

late_open(State, Table) ->
    ok = moderation_shadow:set_mode(legacy),
    idle(),
    ets:match_delete(Table, {{stream, '_'}, '_'}),
    ets:insert(Table, {calls, 0}),
    ets:insert(Table, {late_pending, true}),
    with_hang(grpc, fun(Count) ->
        {ok, {Subchannel, undefined}} = grpcbox_channel:pick(tg20_fault, unary),
        {ok, Streams, _} = grpcbox_subchannel:conn(Subchannel, 1000),
        Connection = chatterbox_h2_stream_set:connection(Streams),
        ?assertEqual(0, chatterbox_h2_stream_set:my_active_count(Streams)),
        ok = sys:suspend(Connection),
        try
            hold_late_open(State, Table, Connection, Streams),
            ?assertEqual(0, Count())
        after
            ok = sys:resume(Connection)
        end,
        await(fun() -> length(streams(Table)) =:= 2 end),
        await(fun() ->
            lists:all(
                fun(Pid) -> not is_process_alive(Pid) end,
                stream_processes(streams(Table))
            )
        end),
        idle(),
        same_connection(Table, streams(Table)),
        ?assertEqual(2, observation(Table, calls)),
        ?assertEqual(0, Count()),
        log_probe(Table),
        ?assertEqual(0, observation(Table, leaks))
    end),
    ets:delete(Table, late_pending).

hold_late_open(State, Table, Connection, Streams) ->
    ok = moderation_shadow:set_mode(shadow),
    Fields = [{<<"content">>, ?F:canary()}],
    Before = counter(worker_unavailable),
    submit_pair(Fields, snapshot(profile_field, Fields)),
    await(fun() ->
        Guards = guardian_pids(State),
        length(Guards) =:= 2 andalso
            lists:sort(Guards) =:= lists:sort(pending_stream_callers(Connection))
    end),
    Guards = guardian_pids(State),
    Outers = running_workers(State),
    ?assertEqual(2, length(Outers)),
    kill_workers(Outers),
    await(fun() -> counter(worker_unavailable) =:= Before + 2 end),
    receive
    after 5100 -> ok
    end,
    ?assertEqual(2, maps:get(running, moderation_shadow:status())),
    ?assert(lists:all(fun is_process_alive/1, Guards)),
    ?assertEqual(lists:sort(Guards), lists:sort(guardian_pids(State))),
    ?assertEqual(lists:sort(Guards), lists:sort(pending_stream_callers(Connection))),
    ?assertEqual(0, chatterbox_h2_stream_set:my_active_count(Streams)),
    ?assertEqual([], streams(Table)),
    ?assertEqual(2, observation(Table, calls)).

guardian_pids(State) ->
    Running = maps:get(running, sys:get_state(worker(State))),
    [Guard || #{guard := Guard} <- maps:values(Running)].

pending_stream_callers(Connection) ->
    {messages, Messages} = process_info(Connection, messages),
    ?assertEqual(false, contains_sensitive(Messages, [?F:canary()])),
    [Pid || {'$gen_call', {Pid, _}, {new_stream, _, _, _, _, Pid}} <- Messages].

stream_reuse(Table, Response) ->
    ets:match_delete(Table, {{stream, '_'}, '_'}),
    ets:insert(Table, {calls, 0}),
    textguard_fault_service:set({success, Response#{decision => 'ALLOW'}}),
    ok = moderation_shadow:set_mode(shadow),
    Before = counter(comparable_match),
    Fields = [{<<"content">>, ?F:canary()}],
    submit_pair(Fields, snapshot(profile_field, Fields)),
    await(fun() -> counter(comparable_match) =:= Before + 2 end),
    idle(),
    await(fun() -> length(streams(Table)) =:= 2 end),
    ?assertEqual(2, textguard_fault_service:count()),
    ?assertEqual(2, observation(Table, calls)),
    ?assert(lists:all(fun(Pid) -> not is_process_alive(Pid) end, stream_processes(streams(Table)))),
    same_connection(Table, streams(Table)),
    ?assert(grpcbox_channel:is_ready(tg20_fault)).

same_connection(Table, Entries) ->
    lists:foreach(
        fun({_, Entry}) ->
            ?assertEqual(0, chatterbox_h2_stream_set:my_active_count(maps:get(streams, Entry)))
        end,
        Entries
    ),
    [Connection] = lists:usort([maps:get(connection, Entry) || {_, Entry} <- Entries]),
    ?assert(is_process_alive(Connection)),
    case ets:lookup(Table, connection) of
        [] -> ets:insert(Table, {connection, Connection});
        [{connection, Before}] -> ?assertEqual(Before, Connection)
    end.

streams(Table) -> ets:match_object(Table, {{stream, '_'}, '_'}).

stream_trace(Table) ->
    receive
        {trace, _, call, {textguard_grpc_call, open_stream, 5}} ->
            ets:update_counter(Table, calls, 1, {calls, 0}),
            ets:update_counter(Table, total_calls, 1, {total_calls, 0}),
            stream_trace(Table);
        {trace, Actor, return_from, {textguard_grpc_call, open_stream, 5}, {ok, Handle}} ->
            remember_stream(Table, Actor, Handle),
            stream_trace(Table);
        {trace, Actor, return_from, {textguard_grpc_call, stream_handle, 3}, Handle} ->
            case ets:lookup(Table, late_pending) of
                [{late_pending, true}] -> remember_stream(Table, Actor, Handle);
                [] -> ok
            end,
            stream_trace(Table);
        stop ->
            ok;
        _ ->
            stream_trace(Table)
    end.

remember_stream(Table, Actor, #{channel := Conn, stream_id := Id, stream_pid := Stream}) ->
    ets:insert(
        Table,
        {{stream, Stream}, #{
            actor => Actor,
            streams => Conn,
            connection => chatterbox_h2_stream_set:connection(Conn),
            id => Id
        }}
    ).

stop_stream_trace(State, Table) ->
    try
        ok = moderation_shadow:set_mode(legacy),
        idle()
    after
        erlang:trace_pattern({textguard_grpc_call, open_stream, 5}, false, [local]),
        erlang:trace_pattern({textguard_grpc_call, stream_handle, 3}, false, [local]),
        erlang:trace(worker(State), false, [call, arity, set_on_spawn]),
        lists:foreach(
            fun({_, #{actor := Actor}}) ->
                case is_process_alive(Actor) of
                    true -> erlang:trace(Actor, false, [call, arity, set_on_spawn]);
                    false -> ok
                end
            end,
            streams(Table)
        )
    end.

await_down(Monitor, Pid) ->
    receive
        {'DOWN', Monitor, process, Pid, _} -> ok
    after 1900 -> error(physical_grpc_process_did_not_terminate)
    end.

log(Event, #{config := #{table := Table, sensitive := Sensitive}}) ->
    case contains_sensitive(Event, Sensitive) of
        true -> ets:update_counter(Table, leaks, 1, {leaks, 0});
        false -> ok
    end,
    case Event of
        #{msg := {report, #{label := tg21_stream_probe}}} ->
            ets:update_counter(Table, probes, 1, {probes, 0});
        _ ->
            ok
    end,
    ok.

log_probe(Table) ->
    Before = observation(Table, probes),
    logger:error(#{label => tg21_stream_probe}),
    await(fun() -> observation(Table, probes) =:= Before + 1 end).

observation(Table, Key) ->
    case ets:lookup(Table, Key) of
        [{Key, Count}] -> Count;
        [] -> 0
    end.

contains_sensitive(Event, Sensitive) ->
    Encoded = term_to_binary(Event),
    lists:any(fun(Value) -> binary:match(Encoded, Value) =/= nomatch end, Sensitive).

receipt(Case, Outcome) ->
    Row = (maps:with([id, acceptance], Case))#{outcome => Outcome},
    ok = file:write_file(
        filename:join(os:getenv("TG19_EVIDENCE"), "case-results.jsonl"),
        [jsone:encode(Row), <<"\n">>],
        [append]
    ).
