-module(textguard_shadow_faults).

-include_lib("eunit/include/eunit.hrl").
-export([run/1]).

-define(C, textguard_shadow_cases).
-define(F, textguard_business_fixture).

run(State) ->
    ?assert(is_pid(maps:get(fault, maps:get(fixture, State)))),
    ok = moderation_shadow:set_mode(legacy),
    ?C:idle(),
    Channel = gproc:where({n, l, {grpcbox_channel, tg20_fault}}),
    ?assert(is_pid(Channel)),
    ?assert(grpcbox_channel:is_ready(tg20_fault)),
    try
        ?C:with_hang(grpc, fun(Count) -> guardian_failure(State, Count) end)
    after
        ok = moderation_shadow:set_mode(legacy),
        stop_owned_channel(Channel),
        ?C:idle(),
        restart_owned_channel()
    end.

guardian_failure(State, Count) ->
    {ok, {Subchannel, undefined}} = grpcbox_channel:pick(tg20_fault, unary),
    {ok, Streams, _} = grpcbox_subchannel:conn(Subchannel, 1000),
    Connection = chatterbox_h2_stream_set:connection(Streams),
    ?assertEqual(0, chatterbox_h2_stream_set:my_active_count(Streams)),
    ok = moderation_shadow:set_mode(shadow),
    Fields = [{<<"content">>, ?F:canary()}],
    Snapshot = ?C:snapshot(profile_field, Fields),
    submit_pair(Fields, Snapshot),
    ?C:await(fun() -> Count() =:= 2 andalso registered_pair(State, Connection) end),
    Entries = running(State),
    ?assertEqual(2, chatterbox_h2_stream_set:my_active_count(Streams)),
    Guards = [maps:get(guard, Entry) || Entry <- maps:values(Entries)],
    ?assertEqual(2, length(lists:usort(Guards))),
    lists:foreach(fun(Guard) -> exit(Guard, kill) end, Guards),
    ?C:await(fun() -> retained_pair(State, Entries) end),
    ?assert(lists:all(fun(Pid) -> not is_process_alive(Pid) end, maps:keys(Entries))),
    ?assert(lists:all(fun(Pid) -> not is_process_alive(Pid) end, Guards)),
    reject_late_results(State, Entries),
    assert_no_replacement(State, Entries, Streams, Count, Fields, Snapshot),
    ok = moderation_shadow:set_mode(legacy),
    ?assertEqual(0, maps:get(queued, moderation_shadow:status())),
    ?assert(retained_pair(State, Entries)),
    ?assert(is_process_alive(Connection)),
    Channel = gproc:where({n, l, {grpcbox_channel, tg20_fault}}),
    stop_owned_channel(Channel),
    ?C:await(fun() -> not is_process_alive(Connection) end),
    ?C:idle(),
    ?assertEqual(#{}, running(State)),
    ?assertEqual(2, Count()).

submit_pair(Fields, Snapshot) ->
    lists:foreach(
        fun(_) ->
            ?assertEqual(accepted, moderation_shadow:submit(profile_field, Fields, Snapshot))
        end,
        [first, second]
    ).

running(State) -> maps:get(running, sys:get_state(?C:worker(State))).

registered_pair(State, Connection) ->
    Entries = running(State),
    map_size(Entries) =:= 2 andalso
        lists:all(
            fun(Entry) ->
                is_pid(maps:get(guard, Entry, undefined)) andalso
                    maps:get(connection, Entry, undefined) =:= Connection andalso
                    is_reference(maps:get(connection_monitor, Entry, undefined))
            end,
            maps:values(Entries)
        ).

retained_pair(State, Original) ->
    Entries = running(State),
    lists:sort(maps:keys(Entries)) =:= lists:sort(maps:keys(Original)) andalso
        lists:all(
            fun(Entry) ->
                maps:get(worker_down, Entry, false) =:= true andalso
                    maps:get(guard_down, Entry, false) =:= true andalso
                    maps:get(reported, Entry, false) =:= true andalso
                    maps:get(cleanup_confirmed, Entry, false) =:= false
            end,
            maps:values(Entries)
        ).

reject_late_results(State, Entries) ->
    Before = ?C:counters(),
    lists:foreach(
        fun(Worker) ->
            ?C:worker(State) ! {result, Worker, comparable_match},
            ?C:worker(State) ! {result, Worker, comparable_match}
        end,
        maps:keys(Entries)
    ),
    ?assertEqual(Before, ?C:counters()),
    ?assert(retained_pair(State, Entries)).

assert_no_replacement(State, Entries, Streams, Count, Fields, Snapshot) ->
    ?assertEqual(accepted, moderation_shadow:submit(profile_field, Fields, Snapshot)),
    ?assertEqual(
        #{queued => 1, running => 2},
        maps:with(
            [queued, running], moderation_shadow:status()
        )
    ),
    receive
    after 100 -> ok
    end,
    ?assert(retained_pair(State, Entries)),
    ?assertEqual(1, maps:get(queued, moderation_shadow:status())),
    ?assertEqual(2, Count()),
    ?assert(chatterbox_h2_stream_set:my_active_count(Streams) =< 2),
    receive
    after 1100 -> ok
    end,
    ?assert(retained_pair(State, Entries)),
    ?assertEqual(2, maps:get(running, moderation_shadow:status())),
    ?assertEqual(2, Count()),
    ?assert(chatterbox_h2_stream_set:my_active_count(Streams) =< 2).

stop_owned_channel(Channel) ->
    case gproc:where({n, l, {grpcbox_channel, tg20_fault}}) of
        undefined ->
            ok;
        Channel when is_pid(Channel) ->
            ok = textguard_grpcbox_transport:stop(tg20_fault),
            ?C:await(fun() -> not is_process_alive(Channel) end),
            ?C:await(fun() ->
                gproc:where({n, l, {grpcbox_channel, tg20_fault}}) =:= undefined
            end);
        _ ->
            error(shadow_fault_channel_owner_changed)
    end.

restart_owned_channel() ->
    ?assertEqual(undefined, gproc:where({n, l, {grpcbox_channel, tg20_fault}})),
    Port = list_to_integer(os:getenv("TG19_FAULT_GRPC_PORT")),
    {ok, _} = textguard_grpcbox_transport:start(tg20_fault, <<"127.0.0.1">>, Port),
    ?assert(grpcbox_channel:is_ready(tg20_fault)),
    ok.
