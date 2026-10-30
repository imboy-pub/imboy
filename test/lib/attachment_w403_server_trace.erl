%%% Unauthenticated diagnostic trace only: any local process can forge trace tuples.
%%% Never use these records to establish a required acceptance leaf.
%%% Test-only per-request tracing. Never record request bodies, tokens, endpoint
%%% credentials, or the unsanitized location returned by storage resolution.
-module(attachment_w403_server_trace).
-export([start/1, collect/1, stop/1]).
-ifdef(TEST).
-export([start_for_test/2]).
-endif.

start(Pid) ->
    start_trace(Pid, [
        {attachment_storage, session_location, 1},
        {attachment_storage, frozen_location, 2}
    ]).

-ifdef(TEST).
start_for_test(Pid, MFAs) -> start_trace(Pid, MFAs).
-endif.

start_trace(Pid, MFAs) when is_pid(Pid), is_list(MFAs) ->
    Caller = self(),
    Ref = make_ref(),
    {Tracer, Monitor} = spawn_monitor(fun() -> init(Caller, Ref, Pid, MFAs) end),
    receive
        {Ref, ready} ->
            erlang:demonitor(Monitor, [flush]),
            {ok, Tracer};
        {'DOWN', Monitor, process, Tracer, Reason} ->
            {error, {trace_start, Reason}}
    after 2000 ->
        exit(Tracer, kill),
        erlang:demonitor(Monitor, [flush]),
        {error, trace_start_timeout}
    end.

collect(Tracer) when is_pid(Tracer) ->
    Ref = make_ref(),
    Monitor = erlang:monitor(process, Tracer),
    Tracer ! {collect, self(), Ref},
    receive
        {Ref, Result} ->
            erlang:demonitor(Monitor, [flush]),
            Result;
        {'DOWN', Monitor, process, Tracer, Reason} ->
            {error, {trace_stopped, Reason}}
    after 3000 ->
        exit(Tracer, kill),
        erlang:demonitor(Monitor, [flush]),
        {error, trace_collect_timeout}
    end.

stop(Tracer) when is_pid(Tracer) ->
    exit(Tracer, kill),
    ok.

init(Caller, Ref, Pid, MFAs) ->
    Session = trace:session_create(w403_server_location, self(), []),
    try
        lists:foreach(
            fun({M, _, _} = MFA) ->
                {module, M} = code:ensure_loaded(M),
                1 = trace:function(Session, MFA, [{'_', [], [{return_trace}]}], [local])
            end,
            MFAs
        ),
        1 = trace:process(Session, Pid, true, [call]),
        Caller ! {Ref, ready},
        loop(Session, Pid, MFAs, [])
    after
        trace:session_destroy(Session)
    end.

loop(Session, Pid, MFAs, Events) ->
    receive
        {trace, Pid, return_from, MFA, {ok, Identity}} when is_map(Identity) ->
            loop(Session, Pid, MFAs, record(MFA, Identity, MFAs, Events));
        {collect, Caller, Ref} ->
            Delivered = trace:delivered(Session, Pid),
            drain(Session, Pid, MFAs, Events, Caller, Ref, Delivered);
        _ ->
            loop(Session, Pid, MFAs, Events)
    after 120000 -> ok
    end.

drain(Session, Pid, MFAs, Events, Caller, Ref, Delivered) ->
    receive
        {trace_delivered, Pid, Delivered} ->
            Caller ! {Ref, {ok, lists:reverse(Events)}};
        {trace, Pid, return_from, MFA, {ok, Identity}} when is_map(Identity) ->
            drain(Session, Pid, MFAs, record(MFA, Identity, MFAs, Events), Caller, Ref, Delivered);
        _ ->
            drain(Session, Pid, MFAs, Events, Caller, Ref, Delivered)
    after 2000 -> Caller ! {Ref, {error, trace_delivery_timeout}}
    end.

record(MFA, Identity, MFAs, Events) when length(Events) < 64 ->
    Fields = [driver, namespace, container, key, locator_digest],
    case
        lists:member(MFA, MFAs) andalso lists:all(fun(K) -> maps:is_key(K, Identity) end, Fields)
    of
        true ->
            [
                #{
                    trust => diagnostic_untrusted,
                    source => list_to_binary(io_lib:format("~p:~p/~p", tuple_to_list(MFA))),
                    identity => maps:with(Fields, Identity)
                }
                | Events
            ];
        false ->
            Events
    end;
record(_, _, _, _) ->
    erlang:error(trace_event_limit).
