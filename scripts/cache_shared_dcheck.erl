%% Production shared_cache public-API checks on isolated loopback BEAM peers.
%% No database, registry override, fabricated version, or recovery-state edits.
%% Reuses the established fixture and fault helpers; same-host evidence only.
%% Compile this plus cache_v2_bench and cache_v21_dcheck into a separate ebin.
%% ERL_EPMD_PORT=43729 erl +S 3 -noshell -pa <driver-ebin> -pa ebin
%%   -pa deps/*/ebin -eval 'cache_shared_dcheck:main(["/tmp/shared.json"]),halt().'
%% Parent starts the isolated epmd daemon. Any failed assertion exits nonzero.
-module(cache_shared_dcheck).
-export([main/1, boot/1]).

-define(TIMEOUT, 30000).

main([Output]) ->
    Started = erlang:monotonic_time(millisecond),
    distribution(),
    Results = [default_denied()] ++ [matrix(N) || N <- [1, 2, 3]],
    {ok, Source} = file:read_file("scripts/cache_shared_dcheck.erl"),
    Report = #{driver_sha256 => binary:encode_hex(crypto:hash(sha256, Source), lowercase),
        elapsed_ms => erlang:monotonic_time(millisecond) - Started,
        scope => <<"same-host loopback; public API convergence after quiescence; not linearizability">>,
        results => Results},
    ok = file:write_file(Output, jsone:encode(Report)),
    io:format("SHARED_CACHE_DCHECK ~s~n", [jsone:encode(Report)]),
    case lists:all(fun(#{status := S}) -> S =:= pass end, Results) of
        true -> ok;
        false -> halt(2)
    end.

distribution() ->
    "43729" = os:getenv("ERL_EPMD_PORT"),
    ok = application:set_env(kernel, inet_dist_use_interface, {127, 0, 0, 1}),
    ok = application:set_env(kernel, prevent_overlapping_partitions, false),
    nonode@nohost = node(),
    Name = list_to_atom("shared_check_" ++ os:getpid() ++ "@127.0.0.1"),
    {ok, _} = net_kernel:start([Name, longnames]),
    true = erlang:set_cookie(node(), list_to_atom("shared_check_" ++ os:getpid())),
    ok.

boot(Rep) ->
    true = os:putenv("CACHE_V2_ARM", "cand"),
    ok = cache_v2_bench:app_up(#{replication_namespace => shared_cache,
        registry_overrides => #{}, replication => Rep, memory_max => 64}),
    0 = map_size(application:get_env(imboy, cache_registry_overrides, missing)),
    replicate = imboy_cache_policy:policy({shared_cache, production_registry}),
    ok.

%% Every matrix owns its peers, including same-name replacements on failures.
owned(Name, Fun) ->
    put(owned_peers, []),
    put(cluster_nodes, []),
    try
        Details = Fun(),
        #{name => Name, status => pass, checks => Details}
    catch Class:Reason:Stack ->
        #{name => Name, status => fail,
          phase => fmt(get(phase)),
          error => fmt({Class, Reason, lists:sublist(Stack, 5)})}
    after
        [try peer:stop(P) catch _:_ -> ok end || P <- get(owned_peers)],
        erase(owned_peers),
        erase(cluster_nodes)
    end.

start_peer(Name, Rep) ->
    Paths = lists:append([["-pa", filename:absname(P)] || P <- lists:reverse(code:get_path())]),
    Args = ["+S", "3", "-setcookie", atom_to_list(erlang:get_cookie()),
        "-kernel", "inet_dist_use_interface", "{127,0,0,1}",
        "prevent_overlapping_partitions", "false"] ++ Paths,
    {ok, P, N} = peer:start(#{name => Name, host => "127.0.0.1",
        connection => standard_io, args => Args}),
    put(owned_peers, [P | get(owned_peers)]),
    %% Join the existing authority before boot; a cold replacement must not
    %% bootstrap itself as a standalone empty authority.
    [true = call(N, net_kernel, connect_node, [Other])
        || Other <- get(cluster_nodes), Other =/= N],
    put(cluster_nodes, lists:usort([N | get(cluster_nodes)])),
    ok = peer:call(P, ?MODULE, boot, [Rep], 60000),
    {P, N, Name}.

cluster(Count, Rep) ->
    Peers = [start_peer(peer:random_name("shared"), Rep) || _ <- lists:seq(1, Count)],
    connect(nodes_of(Peers)),
    Peers.
nodes_of(Peers) -> [N || {_, N, _} <- Peers].
call(N, M, F, Args) ->
    case rpc:call(N, M, F, Args, ?TIMEOUT) of
        {badrpc, Why} -> error({rpc_failed, N, M, F, Why});
        Result -> Result
    end.
connect(Nodes) ->
    [true = call(A, net_kernel, connect_node, [B]) || A <- Nodes, B <- Nodes, A =/= B],
    await(fun() -> lists:all(fun(N) ->
        length(call(N, syn, members, [imboy_cache, dsync_handler])) =:= length(Nodes)
    end, Nodes) end),
    [reconcile_tick = call(N, erlang, send, [imboy_cache_sync, reconcile_tick]) || N <- Nodes],
    await(fun() -> lists:all(fun(N) -> call(N, cache_v2_bench, members_confirmed, []) end, Nodes) end).

default_denied() -> owned(<<"production registry and default-deny admission">>, fun() ->
    [{_, A, _}] = cluster(1, #{whitelist => []}),
    K = {shared_cache, denied},
    %% Omitted whitelist must deny too, not only an explicitly empty list.
    ok = call(A, application, set_env, [imboy, cache_replication, #{}]),
    {error, replicate_not_authorized} = put_value(A, K, denied, 120),
    undefined = get_value(A, K),
    {error, not_authorized} = call(A, imboy_cache, put_delete, [K]),
    Sensitive = [local_cache, auth_session_epoch, qr_login, qr_login_qr, unregistered],
    ok = call(A, application, set_env,
        [imboy, cache_replication, #{whitelist => [shared_cache | Sensitive]}]),
    [{error, replicate_not_authorized} = put_value(A, {NS, denied}, denied, 120) || NS <- Sensitive],
    [{error, not_authorized} = call(A, imboy_cache, put_delete, [{NS, denied}]) || NS <- Sensitive],
    [default_empty_whitelist, default_delete_denied, sensitive_and_qr_not_overridden]
end).

matrix(Count) -> owned(iolist_to_binary(io_lib:format("~B node public API", [Count])), fun() ->
    put(phase, boot),
    Peers = cluster(Count, #{whitelist => [shared_cache]}),
    Nodes = nodes_of(Peers),
    step(basics, fun() -> basics(Nodes) end),
    step(concurrent, fun() -> concurrent(Nodes) end),
    Recovery = case Count of
        1 -> [];
        _ ->
            Delay = step(partition, fun() -> partition(Nodes) end),
            step(sync_restart_recovery, fun() -> restart_recovery(Nodes) end),
            step(node_restart_recovery, fun() -> node_recovery(Peers) end),
            [#{scenario => real_partition_rejoin, convergence_ms => Delay},
             sync_restart_snapshot, same_name_node_restart_snapshot]
    end,
    %% Admission limits are deliberately last; restore config even on failure.
    step(resources, fun() -> resources(Nodes) end),
    [production_opt_in, set_update_delete, ttl_no_reset, reordered_actual_frames,
        concurrent_writers_converge, index_budget_rejection, value_budget_rejection | Recovery]
end).

step(Name, Fun) ->
    put(phase, Name),
    Result = Fun(),
    io:format("SHARED_CACHE_PHASE ~p passed~n", [Name]),
    Result.

put_value(N, K, V, TTL) -> call(N, imboy_cache, put, [K, V, TTL, replicate]).
get_value(N, K) -> call(N, imboy_cache, get, [K]).
all_value(Nodes, K, Expected) -> all_value(Nodes, K, Expected, ?TIMEOUT).
all_value(Nodes, K, Expected, Timeout) ->
    try
        await(fun() ->
            Observed = [{N, get_value(N, K)} || N <- Nodes],
            put(last_observed, Observed),
            lists:all(fun({_, V}) -> V =:= Expected end, Observed)
        end, erlang:monotonic_time(millisecond) + Timeout)
    catch error:convergence_timeout ->
        States = [{N, call(N, imboy_cache_sync, status, []),
            call(N, cache_v21_dcheck, read_rev_full, [K])} || N <- Nodes],
        error({public_value_timeout, K, Expected, get(last_observed), States})
    end.
frame(N, K, V) -> call(N, cache_v21_dcheck, stored_set_frame, [K, V]).
replay(Nodes, Frame) ->
    [ok = call(N, cache_v21_dcheck, deliver_frame, [Frame]) || N <- Nodes], ok.

basics([A | _] = Nodes) ->
    K = {shared_cache, lifecycle},
    ok = put_value(A, K, old, 120),
    all_value(Nodes, K, {ok, old}),
    Old = frame(A, K, old),
    ok = put_value(A, K, newer, 120),
    all_value(Nodes, K, {ok, newer}),
    New = frame(A, K, newer),
    %% Actual committed envelopes, delivered in reverse order and duplicated.
    replay(Nodes, New), replay(Nodes, Old), replay(Nodes, Old),
    all_value(Nodes, K, {ok, newer}),
    ok = call(A, imboy_cache, put_delete, [K]),
    all_value(Nodes, K, undefined),
    replay(Nodes, New), replay(Nodes, Old),
    assert_absent_for(Nodes, K, 250),
    TTLKey = {shared_cache, ttl},
    %% Remote TTL subtracts the production five-second skew reserve. An 8s
    %% original lifetime remains observable remotely, unlike a 2s lifetime.
    ok = put_value(A, TTLKey, short_lived, 8),
    all_value(Nodes, TTLKey, {ok, short_lived}),
    TTLFrame = frame(A, TTLKey, short_lived),
    {ok, Expires} = call(A, cache_v21_dcheck, read_expires, [TTLKey]),
    [{ok, Expires} = call(N, cache_v21_dcheck, read_expires, [TTLKey]) || N <- Nodes],
    timer:sleep(9200),
    all_value(Nodes, TTLKey, undefined),
    replay(Nodes, TTLFrame),
    assert_absent_for(Nodes, TTLKey, 250).

concurrent(Nodes) ->
    K = {shared_cache, concurrent},
    Parent = self(),
    Jobs = [begin
        Token = make_ref(),
        Pid = spawn(fun() ->
            receive {go, Token} -> ok end,
            Result = [put_value(N, K, {writer, N}, 120) || _ <- lists:seq(1, 40)],
            Parent ! {Token, Result}
        end),
        {Pid, Token}
    end || N <- Nodes],
    [P ! {go, T} || {P, T} <- Jobs],
    %% While writers run, a version-change read may safely miss. Wrong values
    %% never qualify; after quiescence a miss is not successful convergence.
    [case get_value(N, K) of
        undefined -> ok;
        {ok, {writer, Origin}} -> true = lists:member(Origin, Nodes);
        Wrong -> error({wrong_concurrent_value, Wrong})
    end || N <- Nodes],
    [receive {T, Results} -> true = lists:all(fun(X) -> X =:= ok end, Results)
        after 35000 -> error(writer_timeout) end || {_, T} <- Jobs],
    Revs = [begin {ok, Rev} = call(N, cache_v21_dcheck, read_rev, [K]), Rev end || N <- Nodes],
    Winner = {_, Origin} = lists:max(Revs),
    all_value(Nodes, K, {ok, {writer, Origin}}),
    [{ok, Winner} = call(N, cache_v21_dcheck, read_rev, [K]) || N <- Nodes], ok.

partition([A | Rest] = Nodes) ->
    K = {shared_cache, partition_update},
    D = {shared_cache, partition_delete},
    [ok = put_value(A, Key, old, 120) || Key <- [K, D]],
    [all_value(Nodes, Key, {ok, old}) || Key <- [K, D]],
    Old = frame(A, D, old),
    Cookie = erlang:get_cookie(),
    Edges = [{A, N} || N <- Rest] ++ [{N, A} || N <- Rest],
    %% Distinct directional cookies prevent both sides authenticating each other.
    [true = call(A, erlang, set_cookie, [N, shared_partition_island]) || N <- Rest],
    [true = call(N, erlang, set_cookie, [A, shared_partition_survivors]) || N <- Rest],
    try
        [call(X, erlang, disconnect_node, [Y]) || {X, Y} <- Edges],
        await(fun() -> lists:all(fun({X, Y}) ->
            not lists:member(Y, call(X, erlang, nodes, [])) end, Edges) end),
        [false = call(X, net_kernel, connect_node, [Y]) || {X, Y} <- Edges],
        B = hd(Rest),
        ok = put_value(B, K, updated_during_partition, 120),
        ok = call(B, imboy_cache, put_delete, [D]),
        %% Reconnection must still fail after writes, not merely be absent briefly.
        [false = call(X, net_kernel, connect_node, [Y]) || {X, Y} <- Edges],
        true = lists:all(fun({X, Y}) -> not lists:member(Y, call(X, erlang, nodes, [])) end, Edges)
    after
        [true = call(X, erlang, set_cookie, [Y, Cookie]) || {X, Y} <- Edges],
        [true = call(X, net_kernel, connect_node, [Y]) || {X, Y} <- Edges]
    end,
    %% No new writes, state edits or manual recovery after rejoin.
    %% Production reconciliation runs every60s. Preserve its timer, and allow
    %% one natural interval plus15s for recovery rather than injecting a tick.
    RejoinedAt = erlang:monotonic_time(millisecond),
    all_value(Nodes, K, {ok, updated_during_partition}, 75000),
    all_value(Nodes, D, undefined),
    Delay = erlang:monotonic_time(millisecond) - RejoinedAt,
    io:format("SHARED_CACHE_REJOIN nodes=~B convergence_ms=~B~n", [length(Nodes), Delay]),
    replay(Nodes, Old),
    assert_absent_for(Nodes, D, 250),
    Delay.

restart_recovery([A | _] = Nodes) ->
    B = lists:last(Nodes),
    Keys = [{shared_cache, {snapshot, I}} || I <- lists:seq(1, 300)],
    [ok = put_value(A, K, K, 120) || K <- Keys],
    [all_value(Nodes, K, {ok, K}) || K <- Keys],
    {B, OldBoot} = call(B, imboy_cache_sync, node_boot, []),
    ok = call(B, cache_v21_dcheck, restart_sync_only, []),
    await(fun() -> call(B, imboy_cache_sync, status, []) =:= ready end),
    {B, NewBoot} = call(B, imboy_cache_sync, node_boot, []),
    true = NewBoot =/= OldBoot,
    [all_value(Nodes, K, {ok, K}) || K <- Keys],
    %% Existing helper clears depcache and restarts sync with real snapshot flow.
    ok = call(B, cache_v21_dcheck, restart_sync, []),
    await(fun() -> call(B, imboy_cache_sync, status, []) =:= ready end),
    [all_value(Nodes, K, {ok, K}) || K <- Keys], ok.

node_recovery(Peers) ->
    Nodes = nodes_of(Peers),
    [A | _] = Nodes,
    {P, B, Name} = lists:last(Peers),
    K = {shared_cache, node_restart},
    D = {shared_cache, node_restart_deleted},
    ok = put_value(A, K, survives, 120),
    ok = put_value(A, D, removed, 120),
    all_value(Nodes, D, {ok, removed}),
    Old = frame(A, D, removed),
    ok = call(A, imboy_cache, put_delete, [D]),
    all_value(Nodes, D, undefined),
    {B, Before} = call(B, imboy_cache_sync, node_boot, []),
    ok = peer:stop(P),
    {_NewPeer, B, Name} = start_peer(Name, #{whitelist => [shared_cache]}),
    connect(Nodes),
    {B, After} = call(B, imboy_cache_sync, node_boot, []),
    true = Before =/= After,
    all_value(Nodes, K, {ok, survives}),
    replay(Nodes, Old),
    assert_absent_for(Nodes, D, 250).

resources([A | _] = Nodes) ->
    Saved = [{N, call(N, application, get_env, [imboy, cache_replication, #{}])} || N <- Nodes],
    K = {shared_cache, budget_rejected},
    try
        [ok = call(N, application, set_env, [imboy, cache_replication, C#{n_max => 1}]) || {N, C} <- Saved],
        {error, index_full} = put_value(A, K, cannot_fit, 120),
        undefined = get_value(A, K),
        [ok = call(N, application, set_env,
            [imboy, cache_replication, C#{max_value_bytes => 64}]) || {N, C} <- Saved],
        {error, value_too_large} = put_value(A, K, binary:copy(<<0>>, 128), 120),
        undefined = get_value(A, K)
    after
        [ok = call(N, application, set_env, [imboy, cache_replication, C]) || {N, C} <- Saved]
    end,
    ok = put_value(A, K, admission_restored, 120),
    all_value(Nodes, K, {ok, admission_restored}).

assert_absent_for(Nodes, K, Millis) ->
    Until = erlang:monotonic_time(millisecond) + Millis,
    absent_loop(Nodes, K, Until).
absent_loop(Nodes, K, Until) ->
    [undefined = get_value(N, K) || N <- Nodes],
    case erlang:monotonic_time(millisecond) >= Until of
        true -> ok;
        false -> timer:sleep(25), absent_loop(Nodes, K, Until)
    end.
await(Pred) -> await(Pred, erlang:monotonic_time(millisecond) + ?TIMEOUT).
await(Pred, Deadline) ->
    case Pred() of
        true -> ok;
        false ->
            case erlang:monotonic_time(millisecond) >= Deadline of
                true -> error(convergence_timeout);
                false -> timer:sleep(25), await(Pred, Deadline)
            end
    end.
fmt(Term) -> iolist_to_binary(io_lib:format("~p", [Term])).
