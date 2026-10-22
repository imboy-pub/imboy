-module(textguard_grpc_call).
-include_lib("grpcbox/include/grpcbox.hrl").
-export([call/5, filter_log/2]).

call(Context, Path, Request, Definition, Options) ->
    case logger:add_primary_filter(textguard_grpc_privacy, {fun ?MODULE:filter_log/2, []}) of
        ok ->
            guarded(Context, Path, Request, Definition, Options);
        {error, {already_exist, textguard_grpc_privacy}} ->
            guarded(Context, Path, Request, Definition, Options);
        _ ->
            {error, not_ready}
    end.

guarded(Context, Path, Request, Definition, Options) ->
    Coordinator = get(textguard_shadow_coordinator),
    Deadline =
        erlang:monotonic_time(millisecond) +
            grpcbox_utils:get_timeout_from_ctx(Context, 150),
    case lease(Coordinator, Deadline) of
        {ok, Lease} ->
            guarded(Context, Path, Request, Definition, Options, Coordinator, Lease, Deadline);
        Error ->
            Error
    end.

lease(undefined, _) -> {ok, undefined};
lease(_, Deadline) -> moderation_shadow_lease:acquire(Deadline).

guarded(Context, Path, Request, Definition, Options, Coordinator, Lease, Deadline) ->
    Owner = self(),
    Ref = erlang:alias([reply]),
    {Pid, Monitor} = spawn_monitor(fun() ->
        put(textguard_grpc_private, true),
        put(textguard_shadow_coordinator, Coordinator),
        put(textguard_grpc_owner, Owner),
        put(textguard_grpc_lease, Lease),
        put(textguard_grpc_deadline, Deadline),
        OwnerMonitor = monitor(process, Owner),
        put(textguard_grpc_owner_monitor, OwnerMonitor),
        Result =
            try
                case
                    moderation_shadow_lease:bind(Lease, Deadline) andalso
                        admitted(Coordinator, Owner, OwnerMonitor)
                of
                    true -> stream(Context, Path, Request, Definition, Options, OwnerMonitor);
                    false -> {error, timeout}
                end
            catch
                throw:text_too_large -> {error, text_too_large};
                exit:{timeout, _} -> {error, timeout};
                exit:timeout -> {error, timeout};
                _:_ -> {error, malformed_transport_response}
            end,
        case get(textguard_grpc_pending) =/= true of
            true ->
                moderation_shadow_lease:cleanup(Lease),
                case is_pid(Coordinator) of
                    true -> Coordinator ! {grpc_cleanup, Owner, self()};
                    false -> ok
                end;
            false ->
                ok
        end,
        Ref ! {Ref, Result}
    end),
    await_result(Ref, Pid, Monitor, Deadline).

await_result(Ref, Pid, Monitor, Deadline) ->
    receive
        {Ref, Result} ->
            erlang:unalias(Ref),
            demonitor(Monitor, [flush]),
            Result;
        {'DOWN', Monitor, process, Pid, _} ->
            erlang:unalias(Ref),
            {error, connection_failed}
    after max(0, Deadline - erlang:monotonic_time(millisecond)) ->
        erlang:unalias(Ref),
        demonitor(Monitor, [flush]),
        receive
            {Ref, _} -> ok
        after 0 -> ok
        end,
        {error, timeout}
    end.

remaining() ->
    max(0, get(textguard_grpc_deadline) - erlang:monotonic_time(millisecond)).

admitted(undefined, _, _) ->
    true;
admitted(Coordinator, Owner, OwnerMonitor) ->
    Ref = make_ref(),
    Coordinator ! {grpc_guard, Owner, self(), Ref},
    receive
        {Ref, Accepted} -> Accepted;
        {'DOWN', OwnerMonitor, process, _, _} -> false
    after remaining() -> false
    end.

stream(Context, Path, Request, Definition, Options, OwnerMonitor) ->
    Deadline = get(textguard_grpc_deadline),
    receive
        {'DOWN', OwnerMonitor, process, _, _} -> {error, timeout}
    after 0 ->
        case open_stream(Context, Path, Request, Definition, Options) of
            {ok, Stream} ->
                try
                    collect(Stream, OwnerMonitor, Deadline, #{})
                after
                    cancel(Stream)
                end;
            {error, {shutdown, econnrefused}} ->
                {error, econnrefused};
            Error ->
                Error
        end
    end.

open_stream(Context, Path, Request, Definition, Options) ->
    case grpcbox_channel:pick(maps:get(channel, Options), unary) of
        {ok, {Channel, undefined}} ->
            case
                grpcbox_subchannel:conn(Channel, grpcbox_utils:get_timeout_from_ctx(Context, 150))
            of
                {ok, Streams, Config} ->
                    create_stream(Context, Streams, Config, Path, Request, Definition, Options);
                Error ->
                    Error
            end;
        {ok, {_, _Interceptor}} ->
            {error, not_ready};
        Error ->
            Error
    end.

create_stream(Context, Streams, Config, Path, Request, Definition, Options) ->
    #grpcbox_def{
        service = Service,
        message_type = MessageType,
        marshal_fun = Marshal,
        unmarshal_fun = Unmarshal
    } = Definition,
    Encoding = maps:get(encoding, Options, maps:get(encoding, Config)),
    Body = grpcbox_frame:encode(Encoding, Marshal(Request)),
    Budget = grpcbox_utils:get_timeout_from_ctx(Context, 150),
    Metadata = grpcbox_utils:encode_headers(
        maps:to_list(grpcbox_metadata:from_outgoing_ctx(Context))
    ),
    Headers = [
        {<<":method">>, <<"POST">>},
        {<<":path">>, Path},
        {<<":scheme">>, maps:get(scheme, Config)},
        {<<":authority">>, maps:get(authority, Config)},
        {<<"grpc-encoding">>, atom_to_binary(Encoding)},
        {<<"grpc-message-type">>, MessageType},
        {<<"content-type">>, <<"application/grpc+proto">>},
        {<<"te">>, <<"trailers">>},
        {<<"grpc-timeout">>, <<(integer_to_binary(max(1, Budget)))/binary, "m">>}
        | Metadata
    ],
    State = #{
        service => Service,
        marshal_fun => Marshal,
        unmarshal_fun => Unmarshal,
        path => Path,
        buffer => <<>>,
        stats_handler => maps:get(stats_handler, Config),
        stats => #{},
        client_pid => self()
    },
    Connection = chatterbox_h2_stream_set:connection(Streams),
    case
        moderation_shadow_lease:connection(
            get(textguard_grpc_lease), Connection, get(textguard_grpc_deadline)
        ) andalso bind_connection(Connection) andalso remaining() > 0
    of
        true ->
            put(textguard_grpc_pending, true),
            Pending = gen_server:send_request(
                Connection, {new_stream, grpcbox_client_stream, [State], Headers, [], self()}
            ),
            await_creation(Pending, Streams, Body, get(textguard_grpc_deadline));
        false ->
            {error, timeout}
    end.

bind_connection(Connection) ->
    case get(textguard_shadow_coordinator) of
        undefined ->
            true;
        Coordinator ->
            Ref = make_ref(),
            Coordinator ! {grpc_connection, get(textguard_grpc_owner), self(), Connection, Ref},
            OwnerMonitor = get(textguard_grpc_owner_monitor),
            receive
                {Ref, Accepted} -> Accepted;
                {'DOWN', OwnerMonitor, process, _, _} -> false
            after remaining() -> false
            end
    end.

await_creation(Pending, Streams, Body, Deadline) ->
    OwnerMonitor = get(textguard_grpc_owner_monitor),
    receive
        {'DOWN', OwnerMonitor, process, _, _} -> discard_creation(Pending, Streams)
    after 0 ->
        Remaining = Deadline - erlang:monotonic_time(millisecond),
        case Remaining =< 0 of
            true ->
                discard_creation(Pending, Streams);
            false ->
                case gen_server:wait_response(Pending, min(10, Remaining)) of
                    timeout ->
                        await_creation(Pending, Streams, Body, Deadline);
                    {reply, {Id, Pid}} when is_pid(Pid) ->
                        Stream = stream_handle(Streams, Id, Pid),
                        receive
                            {'DOWN', OwnerMonitor, process, _, _} ->
                                cancel(Stream),
                                {error, timeout}
                        after 0 ->
                            case erlang:monotonic_time(millisecond) < Deadline of
                                true ->
                                    chatterbox_h2_connection:send_body(Streams, Id, Body),
                                    {ok, Stream};
                                false ->
                                    cancel(Stream),
                                    {error, timeout}
                            end
                        end;
                    {reply, Error} ->
                        put(textguard_grpc_pending, false),
                        Error;
                    {error, _} ->
                        put(textguard_grpc_pending, false),
                        {error, connection_failed}
                end
        end
    end.

discard_creation(Pending, Streams) ->
    case gen_server:wait_response(Pending, infinity) of
        {reply, {Id, Pid}} when is_pid(Pid) -> cancel(stream_handle(Streams, Id, Pid));
        _ -> put(textguard_grpc_pending, false)
    end,
    {error, timeout}.

stream_handle(Streams, Id, Pid) ->
    #{
        channel => Streams,
        stream_id => Id,
        stream_pid => Pid,
        monitor_ref => monitor(process, Pid)
    }.

collect(#{stream_id := Id, monitor_ref := StreamMonitor} = Stream, OwnerMonitor, Deadline, Acc) ->
    Remaining = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {'DOWN', OwnerMonitor, process, _, _} ->
            {error, timeout};
        {'DOWN', StreamMonitor, process, _, _} ->
            {error, connection_failed};
        {headers, Id, Headers} ->
            collect(Stream, OwnerMonitor, Deadline, Acc#{headers => Headers});
        {data, Id, Data} when not is_map_key(data, Acc) ->
            collect(Stream, OwnerMonitor, Deadline, Acc#{data => Data});
        {data, Id, _} ->
            {error, malformed_transport_response};
        {trailers, Id, Trailers} ->
            collect(Stream, OwnerMonitor, Deadline, Acc#{trailers => Trailers});
        {eos, Id} ->
            reply(Acc)
    after Remaining -> {error, timeout}
    end.

reply(#{headers := Headers, trailers := {Status, Message, Trailers}} = Acc) ->
    Metadata = #{headers => Headers, trailers => Trailers},
    case {maps:get(<<":status">>, Headers, undefined), Status} of
        {<<"200">>, <<"0">>} when is_map_key(data, Acc) -> {ok, maps:get(data, Acc), Metadata};
        {<<"200">>, <<"0">>} -> {error, malformed_transport_response};
        {<<"200">>, _} -> {error, {Status, Message}, Metadata};
        _ -> {error, malformed_transport_response}
    end;
reply(_) ->
    {error, malformed_transport_response}.

cancel(#{
    stream_pid := Pid,
    stream_id := Id,
    channel := Streams,
    monitor_ref := OriginalMonitor
}) ->
    Monitor = monitor(process, Pid),
    chatterbox_h2_stream:rst_stream(Pid, 8),
    receive
        {'DOWN', Monitor, process, Pid, _} -> ok
    after 100 ->
        exit(Pid, kill),
        receive
            {'DOWN', Monitor, process, Pid, _} -> ok
        end
    end,
    demonitor(OriginalMonitor, [flush]),
    close_active(Id, Streams),
    put(textguard_grpc_pending, false),
    ok.

close_active(Id, Streams) ->
    try
        Stream = chatterbox_h2_stream_set:get(Id, Streams),
        _ =
            case chatterbox_h2_stream_set:type(Stream) of
                active -> chatterbox_h2_stream_set:close(Stream, garbage, Streams);
                _ -> ok
            end,
        false = chatterbox_h2_stream_set:type(chatterbox_h2_stream_set:get(Id, Streams)) =:= active,
        ok
    catch
        _:_ ->
            Connection = chatterbox_h2_stream_set:connection(Streams),
            Ref = monitor(process, Connection),
            receive
                {'DOWN', Ref, process, Connection, _} -> ok
            end
    end.

filter_log(#{level := Level, msg := {report, #{label := {Family, _}} = Report}}, _) when
    Family =:= gen_statem; Family =:= proc_lib; Family =:= supervisor
->
    try
        case get(textguard_grpc_private) =:= true orelse private_report(Report) of
            true ->
                case Family of
                    gen_statem -> put(textguard_grpc_private, true);
                    _ -> ok
                end,
                #{
                    level => Level,
                    msg => {string, "textguard_grpc_stream_terminated"},
                    meta => #{
                        pid => self(),
                        time => erlang:system_time(microsecond),
                        domain => [imboy, textguard]
                    }
                };
            false ->
                ignore
        end
    catch
        _:_ -> stop
    end;
filter_log(_, _) ->
    ignore.

private_report(#{service := 'imboy.textguard.v1.TextGuard'}) -> true;
private_report(Map) when is_map(Map) -> lists:any(fun private_report/1, maps:values(Map));
private_report(Tuple) when is_tuple(Tuple) -> private_report(tuple_to_list(Tuple));
private_report([Head | Tail]) -> private_report(Head) orelse private_report(Tail);
private_report(_) -> false.
