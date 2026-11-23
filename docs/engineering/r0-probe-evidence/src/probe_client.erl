-module(probe_client).
-export([main/1]).

%% R0 capacity probe client: a SEPARATE node so client processes never
%% pollute the server's process/port counters.
%%
%% usage (erl -s probe_client main -- Args):
%%   Port N Phase1TimeoutMs CloseK Phase2WaitMs

main(Args0) ->
    Args = [to_list(A) || A <- Args0],
    [PortS, NS, TimeoutS, CloseKS, Ph2S | Rest] = Args,
    TrickleN = arg(Rest, 1, 0),
    TrickleGap = arg(Rest, 2, 100),
    Port = list_to_integer(PortS),
    N = list_to_integer(NS),
    Timeout = list_to_integer(TimeoutS),
    CloseK = list_to_integer(CloseKS),
    Ph2 = list_to_integer(Ph2S),
    {ok, _GunApps} = application:ensure_all_started(gun),
    application:ensure_all_started(inets),
    Self = self(),
    stats(<<"stats0_idle">>, Port),
    %% Ramp in batches so kernel accept-queue overflow (a client-side artifact
    %% of a thundering herd) is not misread as a server-side limit.
    spawn_batch(1, N, Self, Port, Timeout, 100, 150),
    {Up1, Fail1, _} = collect(#{}, #{}, #{}, deadline(Timeout)),
    report(#{
        phase => <<"t1_after_phase1_timeout">>, attempted => N, up => map_size(Up1),
        failed => map_size(Fail1), still_pending => N - map_size(Up1) - map_size(Fail1),
        failure_classes => classes(Fail1)
    }),
    stats(<<"stats1_saturated">>, Port),

    %% 1b) slow trickle: mimics organic growth up to the cliff and fills
    %% whatever transient slots the burst left behind.
    {Up1x, Fail1x} =
        case TrickleN of
            0 ->
                {Up1, Fail1};
            _ ->
                [
                    begin
                        spawn_link(fun() -> conn(N + I, Self, Port, Timeout) end),
                        receive
                        after TrickleGap -> ok
                        end
                    end
                 || I <- lists:seq(1, TrickleN)
                ],
                {Up1a, Fail1a, _} = collect(Up1, Fail1, #{}, deadline(4000)),
                report(#{
                    phase => <<"t1b_after_trickle">>, trickled => TrickleN, up => map_size(Up1a),
                    failed => map_size(Fail1a), failure_classes => classes(Fail1a)
                }),
                {Up1a, Fail1a}
        end,
    stats(<<"stats1b_after_trickle">>, Port),
    report(#{
        phase => <<"http_api_while_full">>, stats_requests => 20,
        http_failures => http_probe(20, Port)
    }),
    stats(<<"stats1c_after_http_probe">>, Port),

    %% 1) established connections keep working while the node is saturated
    Sample = lists:sublist(lists:sort(maps:to_list(Up1x)), 20),
    [Pid ! {ping, t1} || {_, Pid} <- Sample],
    {Up1b, Fail1b, Pongs1} = collect(Up1x, Fail1x, #{}, deadline(3000)),
    report(#{
        phase => <<"ping_while_saturated">>, sampled => length(Sample),
        pongs_received => map_size(Pongs1), still_up => map_size(Up1b)
    }),

    %% 2) free CloseK slots and watch pending handshakes get admitted
    {Closing, Keeping} = split(CloseK, lists:sort(maps:to_list(Up1b))),
    KeepMap = maps:from_list(Keeping),
    [Pid ! close_now || {_, Pid} <- Closing],
    {Up2, Fail2, _} = collect(KeepMap, Fail1b, #{}, deadline(Ph2)),
    NewlyAdmitted = length([I || I <- maps:keys(Up2), not maps:is_key(I, KeepMap)]),
    report(#{
        phase => <<"t2_after_releasing_slots">>, closed_by_client => length(Closing),
        still_up => map_size(Up2), newly_admitted => NewlyAdmitted,
        pending_remaining => N - map_size(Up2) - map_size(Fail2),
        failure_classes => classes(Fail2)
    }),
    stats(<<"stats2_after_release">>, Port),

    %% 3) teardown
    [Pid ! close_now || Pid <- maps:values(Up2)],
    {Up3, _, _} = collect(Up2, Fail2, #{}, deadline(3000)),
    report(#{phase => <<"t3_teardown">>, still_up => map_size(Up3)}),
    stats(<<"stats3_after_teardown">>, Port),
    erlang:halt(0).

spawn_batch(First, N, Coord, Port, Timeout, Batch, PauseMs) when First =< N ->
    Last = min(N, First + Batch - 1),
    [spawn_link(fun() -> conn(I, Coord, Port, Timeout) end) || I <- lists:seq(First, Last)],
    receive
    after PauseMs -> ok
    end,
    spawn_batch(Last + 1, N, Coord, Port, Timeout, Batch, PauseMs);
spawn_batch(_, _, _, _, _, _, _) ->
    ok.

conn(Id, Coord, Port, Timeout) ->
    {ok, Pid} = gun:open("127.0.0.1", Port, #{
        protocols => [http],
        retry => 0,
        connect_timeout => Timeout,
        tcp_opts => [{nodelay, true}]
    }),
    case gun:await_up(Pid, Timeout) of
        {ok, _Protocol} ->
            StreamRef = gun:ws_upgrade(Pid, "/api/v1/ws", [], #{}),
            wait_upgrade(Id, Coord, Pid, StreamRef);
        Else ->
            Coord ! {fail, Id, {await_up, Else}}
    end.

wait_upgrade(Id, Coord, Pid, StreamRef) ->
    receive
        {gun_upgrade, Pid, StreamRef, _Protocols, _Headers} ->
            Coord ! {up, Id, self()},
            live(Id, Coord, Pid, StreamRef);
        {gun_response, Pid, StreamRef, _, Status, _Headers} ->
            Coord ! {fail, Id, {http_status, Status}};
        {gun_error, Pid, StreamRef, Reason} ->
            Coord ! {fail, Id, {gun_error, Reason}};
        {gun_down, Pid, _, Reason, _, _} ->
            Coord ! {fail, Id, {down, Reason}}
    end.

live(Id, Coord, Pid, StreamRef) ->
    receive
        {gun_ws, Pid, StreamRef, {text, <<"pong">>}} ->
            Coord ! {pong, Id},
            live(Id, Coord, Pid, StreamRef);
        {gun_ws, Pid, StreamRef, _} ->
            live(Id, Coord, Pid, StreamRef);
        {ping, _Tag} ->
            gun:ws_send(Pid, StreamRef, {text, <<"ping">>}),
            live(Id, Coord, Pid, StreamRef);
        close_now ->
            gun:shutdown(Pid),
            exit(normal);
        {gun_down, Pid, _, Reason, _, _} ->
            Coord ! {dropped, Id, Reason},
            exit(normal)
    end.

collect(Up, Fail, Pongs, Deadline) ->
    case erlang:monotonic_time(millisecond) >= Deadline of
        true ->
            {Up, Fail, Pongs};
        false ->
            receive
                {up, Id, Pid} -> collect(Up#{Id => Pid}, Fail, Pongs, Deadline);
                {fail, Id, Reason} -> collect(Up, Fail#{Id => Reason}, Pongs, Deadline);
                {dropped, Id, Reason} -> collect(maps:remove(Id, Up), Fail#{Id => {dropped, Reason}}, Pongs, Deadline);
                {pong, Id} -> collect(Up, Fail, Pongs#{Id => true}, Deadline)
            after 25 -> collect(Up, Fail, Pongs, Deadline)
            end
    end.

split(0, L) -> {[], L};
split(_, []) -> {[], []};
split(K, [H | T]) ->
    {A, B} = split(K - 1, T),
    {[H | A], B}.

classes(Fail) ->
    Count = lists:foldl(
        fun({_, R}, Acc) -> maps:update_with(class(R), fun(V) -> V + 1 end, 1, Acc) end,
        #{},
        maps:to_list(Fail)
    ),
    lists:join(<<",">>, [
        [atom_to_binary(M, utf8), <<":">>, integer_to_binary(N)]
     || {M, N} <- lists:sort(maps:to_list(Count))
    ]).

class({down, {closed, _}}) -> tcp_closed_by_server;
class({down, closed}) -> tcp_closed_by_server;
class({down, _}) -> down_other;
class({gun_error, closed}) -> gun_error_closed;
class({gun_error, _}) -> gun_error;
class({http_status, _}) -> http_non_101;
class({await_up, _}) -> connect_failed;
class({dropped, _}) -> dropped_after_up;
class(_) -> other.

report(Map) ->
    Joined = lists:join(<<",">>, [
        [atom_to_binary(K, utf8), <<":">>, encode(V)] || {K, V} <- maps:to_list(Map)
    ]),
    io:format("CLIENT {~s}~n", [Joined]).

encode(V) when is_integer(V) -> integer_to_binary(V);
encode(V) when is_binary(V) -> V;
encode(V) when is_atom(V) -> atom_to_binary(V, utf8);
encode(V) -> iolist_to_binary(io_lib:format("~0p", [V])).

stats(Label, Port) ->
    Url = "http://127.0.0.1:" ++ integer_to_list(Port) ++ "/stats",
    Body =
        case httpc:request(get, {Url, []}, [], []) of
            {ok, {{_, 200, _}, _, B}} -> B;
            Other -> iolist_to_binary(io_lib:format("stats request failed: ~0p", [Other]))
        end,
    io:format("SERVER ~s ~s", [Label, Body]).

%% Sequential plain HTTP requests (each needs fresh connection + request
%% processes on the server): returns how many did NOT return 200.
http_probe(0, _Port) ->
    0;
http_probe(N, Port) ->
    Url = "http://127.0.0.1:" ++ integer_to_list(Port) ++ "/stats",
    Ok =
        case httpc:request(get, {Url, []}, [{timeout, 3000}], []) of
            {ok, {{_, 200, _}, _, _}} -> 0;
            _ -> 1
        end,
    Ok + http_probe(N - 1, Port).

arg(List, N, Default) when length(List) >= N ->
    list_to_integer(lists:nth(N, List));
arg(_, _, Default) ->
    Default.

deadline(Ms) -> erlang:monotonic_time(millisecond) + Ms.

to_list(A) when is_atom(A) -> atom_to_list(A);
to_list(A) when is_binary(A) -> binary_to_list(A);
to_list(A) when is_list(A) -> A.
