%% C08 两真实后端入口竞争：仅 Cowboy 运输观察替身，PG consume/handler/SSE 为真实代码。
%% erl -noshell -pa <isolated-beams> ebin deps/*/ebin
%%     -eval 'cache_v221_qr_entry_dcheck:main(["/tmp/qr-entry.json"]),halt().'
-module(cache_v221_qr_entry_dcheck).
-include("error_code.hrl").
-export([main/1, init_peer/1, close_peer/0, prepare/2, spawn_entry/5, consumed/1, cleanup/1]).
-define(CONN, {?MODULE, conn}).
-define(WORKERS, {?MODULE, workers}).

main([Out]) ->
    main([Out, "config/sys.closeout-local.config"]);
main([Out, Profile]) ->
    logger:set_primary_config(level, none),
    application:ensure_all_started(crypto),
    Suffix = binary_to_list(binary:encode_hex(crypto:strong_rand_bytes(5), lowercase)),
    {ok, _} = net_kernel:start([list_to_atom("qrcheck" ++ Suffix), longnames]),
    true = erlang:set_cookie(node(), list_to_atom("qrck" ++ Suffix)),
    Args =
        [
            "+S",
            "2:2",
            "+A",
            "1",
            "-setcookie",
            atom_to_list(erlang:get_cookie()),
            "-kernel",
            "inet_dist_use_interface",
            "{127,0,0,1}",
            "prevent_overlapping_partitions",
            "false"
        ] ++
            lists:append([["-pa", filename:absname(P)] || P <- lists:reverse(code:get_path())]),
    {ok, PA, A} = peer:start(#{
        name => list_to_atom("qra" ++ Suffix), host => "127.0.0.1", args => Args
    }),
    try
        {ok, PB, B} = peer:start(#{
            name => list_to_atom("qrb" ++ Suffix), host => "127.0.0.1", args => Args
        }),
        try
            run(Out, A, B, Suffix, Profile)
        after
            peer:stop(PB)
        end
    after
        peer:stop(PA)
    end.

run(Out, A, B, Suffix, Profile) ->
    true = A =/= B,
    {ok, APid} = rpc:call(A, ?MODULE, init_peer, [Profile]),
    {ok, BPid} = rpc:call(B, ?MODULE, init_peer, [Profile]),
    {ok, Lock} = connect(Profile),
    unlink(Lock),
    Cases = [{status, status}, {sse, status}, {sse, sse}],
    Sessions = [
        iolist_to_binary(["c08_fixture_", Suffix, "_", integer_to_list(I)])
     || I <- lists:seq(1, 3)
    ],
    put(qr_schema_ready, false),
    try
        {ok, _, [{true}]} = epgsql:equery(
            Lock,
            "SELECT to_regclass($1) IS NOT NULL",
            [<<"public.qr_login_session">>]
        ),
        put(qr_schema_ready, true),
        io:format("C08_DEDICATED_QR_SCHEMA PASS existing_table=true~n"),
        Results = [
            scenario(A, B, Lock, APid, BPid, Pair, ST)
         || {Pair, ST} <- lists:zip(Cases, Sessions)
        ],
        0 = rpc:call(A, ?MODULE, cleanup, [Sessions]),
        Report = #{
            status => <<"PASS">>,
            cases => 3,
            failed => 0,
            skipped => 0,
            backend_nodes => [atom_to_binary(A), atom_to_binary(B)],
            transport => <<"Cowboy callback observer">>,
            authority => <<"real PG">>,
            fixture_rows_remaining => 0,
            scenarios => Results
        },
        ok = file:write_file(Out, jsone:encode(Report, [native_utf8])),
        io:format(
            "C08_TWO_BACKEND_ENTRIES PASS cases=3 failed=0 skipped=0 credentials_per_case=1 rows_remaining=0~n"
        ),
        ok
    after
        try
            epgsql:squery(Lock, "ROLLBACK")
        catch
            _:_ -> ok
        end,
        case get(qr_schema_ready) of
            true ->
                case rpc:call(A, ?MODULE, cleanup, [Sessions]) of
                    0 -> io:format("C08_FIXTURE_CLEANUP PASS rows_remaining=0~n");
                    _ -> erlang:error(fixture_cleanup_failed)
                end;
            false ->
                io:format("C08_CLEANUP_NOT_REQUIRED schema_unavailable=true~n")
        end,
        rpc:call(A, ?MODULE, close_peer, []),
        rpc:call(B, ?MODULE, close_peer, []),
        epgsql:close(Lock)
    end.

connect(Profile) ->
    {ok, [Terms]} = file:consult(Profile),
    Conf = proplists:get_value(pg_conf, proplists:get_value(imboy, Terms)),
    {M, F, Args} = maps:get(start_mfa, Conf),
    apply(M, F, Args).

init_peer(Profile) ->
    %% 隔离 peer 禁止默认崩溃日志打印请求/凭据；失败由脱敏结果显式上报。
    logger:set_primary_config(level, none),
    application:ensure_all_started(crypto),
    application:ensure_all_started(syn),
    syn:add_node_to_scopes([imboy_qr_login]),
    application:set_env(imboy, qr_login_authority, pg),
    application:set_env(imboy, sql_driver, pgsql),
    {ok, Conn} = connect(Profile),
    unlink(Conn),
    persistent_term:put(?CONN, Conn),
    persistent_term:put(?WORKERS, []),
    meck:new(pooler, [no_link, passthrough]),
    meck:expect(pooler, take_member, fun(pgsql) -> persistent_term:get(?CONN) end),
    meck:expect(pooler, return_member, fun(pgsql, _) -> ok end),
    install_transport_observer(),
    {ok, _, [{Pid}]} = epgsql:equery(Conn, "SELECT pg_backend_pid()", []),
    {ok, Pid}.

install_transport_observer() ->
    meck:new(cowboy_req, [no_link, passthrough]),
    meck:expect(cowboy_req, method, fun(_) -> <<"GET">> end),
    meck:expect(cowboy_req, parse_qs, fun(Req) -> maps:get(qs, Req) end),
    meck:expect(cowboy_req, reply, fun(_, _, Body, Req) ->
        Req#{observed_json => jsone:decode(Body)}
    end),
    meck:expect(cowboy_req, stream_reply, fun(_, _, Req) -> Req end),
    meck:expect(cowboy_req, stream_body, fun(Body, _, _) ->
        put(observed_chunks, [iolist_to_binary(Body) | chunks()]),
        ok
    end).
chunks() ->
    case get(observed_chunks) of
        undefined -> [];
        C -> C
    end.
close_peer() ->
    meck:unload(cowboy_req),
    meck:unload(pooler),
    epgsql:close(persistent_term:get(?CONN)),
    persistent_term:erase(?CONN),
    persistent_term:erase(?WORKERS),
    ok.

prepare(ST, Token) ->
    undefined = qr_login_session_repo:find_by_session_token(ST),
    ok = qr_login_session_repo:create(#{
        session_token => ST,
        qr_token => <<ST/binary, "_qr">>,
        device_id => <<"synthetic">>,
        device_name => <<"synthetic">>,
        platform => <<"web">>,
        purpose => <<"human">>,
        expires_at_ms => erlang:system_time(millisecond) + 60000
    }),
    {ok, _} = qr_login_session_repo:transition_scan(ST, 850000000000000001),
    {ok, _} = qr_login_session_repo:transition_confirm(ST, 850000000000000001, Token),
    ok.

scenario(A, B, Lock, APid, BPid, {EntryA, EntryB}, ST) ->
    Token = <<"invalid_synthetic_credential_", ST/binary>>,
    ok = rpc:call(A, ?MODULE, prepare, [ST, Token]),
    {ok, [], []} = epgsql:squery(Lock, "BEGIN"),
    {ok, _, [_]} = epgsql:equery(
        Lock,
        "SELECT session_token FROM public.qr_login_session WHERE session_token=$1 FOR UPDATE",
        [ST]
    ),
    Ref = make_ref(),
    P1 = rpc:call(A, ?MODULE, spawn_entry, [EntryA, ST, Token, self(), Ref]),
    P2 = rpc:call(B, ?MODULE, spawn_entry, [EntryB, ST, Token, self(), Ref]),
    ready(Ref, A, P1),
    ready(Ref, B, P2),
    P1 ! {go, Ref},
    P2 ! {go, Ref},
    ok = await_pg_waiters(Lock, APid, BPid, 200),
    {ok, [], []} = epgsql:squery(Lock, "COMMIT"),
    R1 = result(Ref, A),
    R2 = result(Ref, B),
    1 = maps:get(credential_count, R1) + maps:get(credential_count, R2),
    true = maps:get(compatible, R1),
    true = maps:get(compatible, R2),
    true = rpc:call(A, ?MODULE, consumed, [ST]),
    #{
        entries => [EntryA, EntryB],
        synchronized_pg_waiters => 2,
        consumers => [R1, R2],
        credentials => 1
    }.

ready(Ref, Node, Pid) ->
    receive
        {entry_ready, Ref, Node, Pid} -> ok
    after 10000 -> error(entry_not_ready)
    end.
result(Ref, Node) ->
    receive
        {entry_result, Ref, Node, R} -> R
    after 10000 -> error(entry_result_timeout)
    end.
await_pg_waiters(_, _, _, 0) ->
    error(pg_consumers_not_concurrent);
await_pg_waiters(C, A, B, N) ->
    {ok, _, _} = epgsql:squery(C, "SELECT pg_stat_clear_snapshot()"),
    {ok, _, [{Count}]} = epgsql:equery(
        C,
        "SELECT count(*) FROM pg_stat_activity WHERE pid IN ($1,$2) AND wait_event_type='Lock'",
        [A, B]
    ),
    case Count of
        2 ->
            ok;
        _ ->
            timer:sleep(20),
            await_pg_waiters(C, A, B, N - 1)
    end.

spawn_entry(Type, ST, Token, Controller, Ref) ->
    Pid = spawn(fun() ->
        try
            entry(Type, ST, Token, Controller, Ref)
        catch
            Class:_ ->
                Controller !
                    {entry_result, Ref, node(), #{
                        node => atom_to_binary(node()),
                        entry => Type,
                        credential_count => 0,
                        compatible => false,
                        error_class => Class
                    }}
        end
    end),
    persistent_term:put(?WORKERS, [Pid | persistent_term:get(?WORKERS)]),
    Pid.
entry(status, ST, Token, Controller, Ref) ->
    Req = #{qs => [{<<"session_token">>, ST}]},
    {cowboy_rest, _, State} = qr_login_handler:init(Req, #{action => status}),
    Controller ! {entry_ready, Ref, node(), self()},
    receive
        {go, Ref} -> ok
    after 10000 -> error(entry_start_timeout)
    end,
    {stop, ReqOut, _} = qr_login_handler:handle_request(Req, State),
    Json = maps:get(observed_json, ReqOut),
    Payload = maps:get(<<"payload">>, Json),
    Credential = maps:get(<<"token">>, Payload, undefined),
    Code = maps:get(<<"code">>, Json),
    Count =
        case Credential of
            Token -> 1;
            undefined -> 0;
            _ -> error(unexpected_credential)
        end,
    Compatible =
        (Count =:= 1 andalso Code =:= 0) orelse
            (Count =:= 0 andalso Code =:= ?ERR_QR_LOGIN_ALREADY_USED),
    Controller !
        {entry_result, Ref, node(), #{
            node => atom_to_binary(node()),
            entry => status,
            credential_count => Count,
            compatible => Compatible
        }};
entry(sse, ST, Token, Controller, Ref) ->
    Req0 = #{qs => [{<<"session_token">>, ST}]},
    {cowboy_loop, Req, State} = qr_login_sse_handler:init(Req0, []),
    try
        Controller ! {entry_ready, Ref, node(), self()},
        receive
            {go, Ref} -> ok
        after 10000 -> error(entry_start_timeout)
        end,
        {stop, _, _} = qr_login_sse_handler:info(
            #{<<"status">> => <<"confirmed_ready">>}, Req, State
        ),
        Credentials = [C || B <- chunks(), C <- [chunk_credential(B)], C =/= undefined],
        Count = length(Credentials),
        Compatible =
            Credentials =:= [Token] orelse
                (Credentials =:= [] andalso chunks() =:= [<<": claimed_elsewhere\n\n">>]),
        Controller !
            {entry_result, Ref, node(), #{
                node => atom_to_binary(node()),
                entry => sse,
                credential_count => Count,
                compatible => Compatible
            }}
    after
        qr_login_sse_handler:terminate(normal, Req, State)
    end.
chunk_credential(<<"data: ", Rest/binary>>) ->
    Body = binary:part(Rest, 0, byte_size(Rest) - 2),
    maps:get(<<"token">>, jsone:decode(Body), undefined);
chunk_credential(_) ->
    undefined.

consumed(ST) ->
    {ok, _, [{null, true}]} = epgsql:equery(
        persistent_term:get(?CONN),
        "SELECT login_token,consumed_at IS NOT NULL FROM public.qr_login_session WHERE session_token=$1",
        [ST]
    ),
    {error, not_claimable} = qr_login_session_repo:claim_login_token(ST),
    true.
cleanup(Sessions) ->
    ok = wait_workers(500),
    C = persistent_term:get(?CONN),
    [
        begin
            {ok, _} = epgsql:equery(
                C, "DELETE FROM public.qr_login_session WHERE session_token=$1", [ST]
            )
        end
     || ST <- Sessions
    ],
    lists:sum([
        begin
            {ok, _, [{N}]} = epgsql:equery(
                C,
                "SELECT count(*) FROM public.qr_login_session WHERE session_token=$1",
                [ST]
            ),
            N
        end
     || ST <- Sessions
    ]).

%% 锁释放后须等待真实入口进程完全退出，不能让 cleanup 与 claim 共用连接。
wait_workers(0) ->
    error(entry_workers_not_drained);
wait_workers(N) ->
    case lists:any(fun is_process_alive/1, persistent_term:get(?WORKERS, [])) of
        false ->
            persistent_term:put(?WORKERS, []),
            ok;
        true ->
            timer:sleep(20),
            wait_workers(N - 1)
    end.
