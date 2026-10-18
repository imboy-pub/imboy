-module(textguard_shadow_restart).

-include_lib("eunit/include/eunit.hrl").
-export([run/1, deadline/1, connection_failure/1]).

-define(C, textguard_shadow_cases).
-define(F, textguard_business_fixture).

run(State) ->
    with_fault(State, fun(Streams, Connection, Response, Count) ->
        with_suspended(Connection, fun() ->
            ok = moderation_shadow:set_mode(shadow),
            Fields = fields(),
            Snapshot = ?C:snapshot(profile_field, Fields),
            submit_pair(Fields, Snapshot),
            Guards = await_pending(Connection, 2),
            assert_registered(State, Guards, Connection),
            with_handles(Guards, fun(Trace) ->
                Old = ?C:worker(State),
                exit(Old, kill),
                ?C:await(fun() -> not is_process_alive(Old) end),
                ?C:await(fun() -> whereis(moderation_shadow) =:= undefined end),
                ok = ?C:restart(State),
                Caller = start_call(fun() ->
                    put(textguard_shadow_coordinator, whereis(moderation_shadow)),
                    moderation_detector_grpc:check(detection(), #{})
                end),
                try
                    ?assertMatch({{error, overloaded}, _}, take_result(Caller, 300))
                after
                    stop_call(Caller)
                end,
                submit_pair(Fields, Snapshot),
                receive
                after 120 -> ok
                end,
                ?assertEqual(Guards, pending(Connection)),
                ?assert(lists:all(fun is_process_alive/1, Guards)),
                ?assert(maps:get(running, moderation_shadow:status()) =< 2),
                ?assert(is_pid(whereis(textguard_shadow_slot_one))),
                ?assert(is_pid(whereis(textguard_shadow_slot_two))),
                ?assertEqual(0, chatterbox_h2_stream_set:my_active_count(Streams)),
                ?assertEqual(0, Count()),
                ok = moderation_shadow:set_mode(legacy),
                ok = sys:resume(Connection),
                assert_cancelled(Trace, Guards, Streams),
                ?C:idle(),
                ?assertEqual(0, Count()),
                reuse(Streams, Connection, Response)
            end)
        end)
    end).

deadline(State) ->
    with_fault(State, fun(Streams, Connection, Response, Count) ->
        ?C:config([{timeout_ms, 60}]),
        with_suspended(Connection, fun() ->
            Caller = start_call(fun() -> moderation_detector_grpc:check(detection(), #{}) end),
            try
                Guards = await_pending(Connection, 1),
                with_handles(Guards, fun(Trace) ->
                    {{error, timeout}, Elapsed} = take_result(Caller, 300),
                    ?assert(Elapsed < 300),
                    ?assertEqual(Guards, pending(Connection)),
                    ?assert(lists:all(fun is_process_alive/1, Guards)),
                    ?assertEqual(0, Count()),
                    ok = sys:resume(Connection),
                    assert_cancelled(Trace, Guards, Streams),
                    ?assertEqual({messages, []}, process_info(maps:get(pid, Caller), messages)),
                    ?assertEqual(0, Count()),
                    reuse(Streams, Connection, Response)
                end)
            after
                stop_call(Caller)
            end
        end)
    end).

connection_failure(State) ->
    lists:foreach(fun(Kind) -> connection_failure(State, Kind) end, [bridge, transport, business]).

connection_failure(State, Kind) ->
    with_fault(State, fun(_Streams, Connection, _Response, Count) ->
        ok = moderation_shadow:set_mode(rust),
        ?C:config([{timeout_ms, 1000}, {failure_policy, availability_compat}]),
        with_suspended(Connection, fun() ->
            {Fun, Expected} = connection_call(Kind),
            Caller = start_call(Fun),
            try
                Guards = await_pending(Connection, 1),
                exit(Connection, kill),
                {Result, _Elapsed} = take_result(Caller, 1900),
                ?assertEqual(Expected, Result),
                ?C:await(fun() -> lists:all(fun(Pid) -> not is_process_alive(Pid) end, Guards) end),
                ?assertEqual(0, Count())
            after
                stop_call(Caller)
            end
        end)
    end).

connection_call(bridge) ->
    Raw = #{
        request_id => <<"tg21-local-connection-failure">>,
        scene => <<"profile_field">>,
        fields => [#{field_id => <<"content">>, text => ?F:canary()}]
    },
    {
        fun() -> moderation_detector_grpcbox:call(#{}, check_text, Raw, 1000) end,
        {error, connection_failed}
    };
connection_call(transport) ->
    {fun() -> moderation_detector_grpc:check(detection(), #{}) end, {error, connection_failed}};
connection_call(business) ->
    {
        fun() -> moderation_business:inspect(profile_field, fields(), true) end,
        {unchecked_degraded, connection_failed}
    }.

with_fault(State, Fun) ->
    ?assert(is_pid(maps:get(fault, maps:get(fixture, State)))),
    ok = moderation_shadow:set_mode(legacy),
    ?C:idle(),
    ?C:config([{transport, grpc}, {grpc_channel, tg20_real}, {timeout_ms, 1000}]),
    {ok, Conf} = application:get_env(imboy, moderation_detector),
    ?assertEqual(undefined, proplists:get_value(grpc_transport, Conf)),
    Response = ?C:real_response(profile_field, [{<<"content">>, <<"ordinary">>}]),
    Channel = gproc:where({n, l, {grpcbox_channel, tg20_fault}}),
    ?assert(is_pid(Channel)),
    try
        ?C:with_hang(grpc, fun(Count) ->
            ?assertEqual(moderation_detector_grpc, moderation_detector_transport:select()),
            {Streams, Connection} = connection(),
            ?assertEqual(0, chatterbox_h2_stream_set:my_active_count(Streams)),
            Fun(Streams, Connection, Response, Count)
        end)
    after
        ensure_coordinator(State),
        ok = moderation_shadow:set_mode(legacy),
        stop_channel(Channel),
        ?C:idle(),
        restart_channel()
    end.

ensure_coordinator(State) ->
    case whereis(moderation_shadow) of
        undefined -> ?C:restart(State);
        Pid -> ?assertEqual(?C:worker(State), Pid)
    end.

connection() ->
    {ok, {Subchannel, undefined}} = grpcbox_channel:pick(tg20_fault, unary),
    {ok, Streams, _} = grpcbox_subchannel:conn(Subchannel, 1000),
    {Streams, chatterbox_h2_stream_set:connection(Streams)}.

fields() -> [{<<"content">>, ?F:canary()}].

detection() ->
    #{request_id => <<"tg21-pending-deadline">>, scene => profile_field, fields => fields()}.

submit_pair(Fields, Snapshot) ->
    lists:foreach(
        fun(_) ->
            ?assertEqual(accepted, moderation_shadow:submit(profile_field, Fields, Snapshot))
        end,
        [first, second]
    ).

pending(Connection) ->
    {messages, Messages} = process_info(Connection, messages),
    ?assertEqual(nomatch, binary:match(term_to_binary(Messages), ?F:canary())),
    lists:sort([
        Pid
     || {'$gen_call', {Pid, _}, {new_stream, grpcbox_client_stream, _, _, [], Pid}} <- Messages
    ]).

await_pending(Connection, Expected) ->
    ?C:await(fun() -> length(pending(Connection)) =:= Expected end),
    Guards = pending(Connection),
    ?assertEqual(Expected, length(lists:usort(Guards))),
    Guards.

assert_registered(State, Guards, Connection) ->
    ?C:await(fun() ->
        Entries = maps:values(maps:get(running, sys:get_state(?C:worker(State)))),
        lists:sort([maps:get(guard, Entry, undefined) || Entry <- Entries]) =:= Guards andalso
            lists:all(
                fun(Entry) ->
                    maps:get(connection, Entry, undefined) =:= Connection andalso
                        is_reference(maps:get(connection_monitor, Entry, undefined))
                end,
                Entries
            )
    end).

with_suspended(Connection, Fun) ->
    ok = sys:suspend(Connection),
    try
        Fun()
    after
        case is_process_alive(Connection) of
            true ->
                try
                    sys:resume(Connection)
                catch
                    _:_ -> ok
                end;
            false ->
                ok
        end
    end.

start_call(Fun) ->
    Parent = self(),
    Tag = make_ref(),
    Started = now_ms(),
    {Pid, Monitor} = spawn_monitor(fun() ->
        Result = Fun(),
        Parent ! {Tag, Result, now_ms() - Started},
        receive
            stop -> ok
        end
    end),
    #{pid => Pid, monitor => Monitor, tag => Tag, started => Started}.

take_result(#{tag := Tag, monitor := Monitor, pid := Pid, started := Started}, Budget) ->
    Remaining = max(0, Started + Budget - now_ms()),
    receive
        {Tag, Result, Elapsed} ->
            demonitor(Monitor, [flush]),
            {Result, Elapsed};
        {'DOWN', Monitor, process, Pid, _} ->
            error(shadow_public_caller_failed)
    after Remaining -> error(shadow_public_caller_exceeded_deadline)
    end.

stop_call(#{pid := Pid, monitor := Monitor, tag := Tag}) ->
    exit(Pid, kill),
    demonitor(Monitor, [flush]),
    receive
        {Tag, _, _} -> ok
    after 0 -> ok
    end.

with_handles(Guards, Fun) ->
    Target = {textguard_grpc_call, stream_handle, 3},
    ?assertEqual({traced, false}, erlang:trace_info(Target, traced)),
    Parent = self(),
    Tag = make_ref(),
    Tracer = spawn(fun() -> handles(Parent, Tag) end),
    try
        erlang:trace_pattern(Target, [{'_', [], [{return_trace}]}], [local]),
        lists:foreach(
            fun(Guard) ->
                1 = erlang:trace(Guard, true, [call, arity, {tracer, Tracer}])
            end,
            Guards
        ),
        Fun(Tag)
    after
        lists:foreach(
            fun(Guard) ->
                case is_process_alive(Guard) of
                    true -> erlang:trace(Guard, false, [call]);
                    false -> ok
                end
            end,
            Guards
        ),
        erlang:trace_pattern(Target, false, [local]),
        exit(Tracer, kill)
    end.

handles(Parent, Tag) ->
    receive
        {trace, Guard, return_from, {textguard_grpc_call, stream_handle, 3}, #{stream_pid := Pid}} ->
            Parent ! {Tag, Guard, Pid},
            handles(Parent, Tag);
        _ ->
            handles(Parent, Tag)
    end.

assert_cancelled(Tag, Guards, Streams) ->
    StreamPids = [
        receive
            {Tag, Guard, Pid} -> Pid
        after 1900 -> error(shadow_late_stream_not_observed)
        end
     || Guard <- Guards
    ],
    ?assertEqual(length(Guards), length(lists:usort(StreamPids))),
    ?C:await(fun() ->
        lists:all(fun(Pid) -> not is_process_alive(Pid) end, Guards ++ StreamPids) andalso
            chatterbox_h2_stream_set:my_active_count(Streams) =:= 0
    end).

reuse(Streams, Connection, Response) ->
    textguard_fault_service:set({success, Response#{decision => 'ALLOW'}}),
    ?C:config([{timeout_ms, 1000}]),
    {ok, Checked} = moderation_detector_grpc:check(detection(), #{}),
    ?assertEqual(allow, maps:get(decision, Checked)),
    ?assertEqual(1, textguard_fault_service:count()),
    {SameStreams, SameConnection} = connection(),
    ?assertEqual(Connection, SameConnection),
    ?assertEqual(Streams, SameStreams),
    ?C:await(fun() -> chatterbox_h2_stream_set:my_active_count(Streams) =:= 0 end).

stop_channel(Channel) ->
    case gproc:where({n, l, {grpcbox_channel, tg20_fault}}) of
        undefined ->
            ok;
        Channel ->
            ok = textguard_grpcbox_transport:stop(tg20_fault),
            ?C:await(fun() -> not is_process_alive(Channel) end),
            ?C:await(fun() ->
                gproc:where({n, l, {grpcbox_channel, tg20_fault}}) =:= undefined
            end);
        _ ->
            error(shadow_fault_channel_owner_changed)
    end.

restart_channel() ->
    ?assertEqual(undefined, gproc:where({n, l, {grpcbox_channel, tg20_fault}})),
    Port = list_to_integer(os:getenv("TG19_FAULT_GRPC_PORT")),
    {ok, _} = textguard_grpcbox_transport:start(tg20_fault, <<"127.0.0.1">>, Port),
    ?assert(grpcbox_channel:is_ready(tg20_fault)),
    ok.

now_ms() -> erlang:monotonic_time(millisecond).
