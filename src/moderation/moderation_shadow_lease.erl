-module(moderation_shadow_lease).
-export([acquire/1, bind/2, connection/3, cleanup/1]).

acquire(Deadline) ->
    acquire([textguard_shadow_slot_one, textguard_shadow_slot_two], Deadline).

acquire([], _) ->
    {error, overloaded};
acquire([Name | Rest], Deadline) ->
    Owner = self(),
    Ref = make_ref(),
    {Lease, Monitor} = spawn_monitor(fun() -> reserve(Name, Owner, Ref) end),
    receive
        {Ref, acquired} ->
            demonitor(Monitor, [flush]),
            {ok, Lease};
        {Ref, busy} ->
            demonitor(Monitor, [flush]),
            acquire(Rest, Deadline);
        {'DOWN', Monitor, process, Lease, _} ->
            acquire(Rest, Deadline)
    after max(0, Deadline - erlang:monotonic_time(millisecond)) ->
        exit(Lease, kill),
        demonitor(Monitor, [flush]),
        {error, timeout}
    end.

reserve(Name, Owner, Ref) ->
    Reserved =
        try
            register(Name, self())
        catch
            error:badarg -> false
        end,
    case Reserved of
        true ->
            Monitor = monitor(process, Owner),
            Owner ! {Ref, acquired},
            receive
                {bind, Guard, BindRef} ->
                    GuardMonitor = monitor(process, Guard),
                    demonitor(Monitor, [flush]),
                    Guard ! {BindRef, true},
                    hold(Guard, GuardMonitor, undefined, false);
                {'DOWN', Monitor, process, Owner, _} ->
                    ok
            end;
        _ ->
            Owner ! {Ref, busy}
    end.

bind(undefined, _) -> true;
bind(Lease, Deadline) -> request(Lease, {bind, self()}, Deadline).

connection(undefined, _, _) ->
    true;
connection(Lease, Connection, Deadline) ->
    request(Lease, {connection, self(), Connection}, Deadline).

request(Lease, Message, Deadline) ->
    Ref = make_ref(),
    Monitor = monitor(process, Lease),
    Lease ! erlang:append_element(Message, Ref),
    receive
        {Ref, true} ->
            demonitor(Monitor, [flush]),
            true;
        {'DOWN', Monitor, process, Lease, _} ->
            false
    after max(0, Deadline - erlang:monotonic_time(millisecond)) ->
        demonitor(Monitor, [flush]),
        false
    end.

cleanup(undefined) ->
    ok;
cleanup(Lease) ->
    Lease ! {cleanup, self()},
    ok.

hold(Guard, GuardMonitor, Connection, Clean) ->
    receive
        {connection, Guard, Pid, Ref} when Connection =:= undefined ->
            Monitor = monitor(process, Pid),
            Guard ! {Ref, true},
            hold(Guard, GuardMonitor, {Pid, Monitor}, Clean);
        {cleanup, Guard} ->
            hold(Guard, GuardMonitor, Connection, true);
        {'DOWN', GuardMonitor, process, Guard, _} ->
            case {Clean, Connection} of
                {true, _} ->
                    ok;
                {_, undefined} ->
                    ok;
                {false, {Pid, Monitor}} ->
                    receive
                        {'DOWN', Monitor, process, Pid, _} -> ok
                    end
            end;
        {'DOWN', Monitor, process, Pid, _} when Connection =:= {Pid, Monitor} ->
            ok;
        _ ->
            hold(Guard, GuardMonitor, Connection, Clean)
    end.
