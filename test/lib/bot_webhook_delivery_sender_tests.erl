-module(bot_webhook_delivery_sender_tests).

-include_lib("eunit/include/eunit.hrl").

tls_options_verify_chain_and_hostname_test() ->
    Host = <<"hooks.example.com">>,
    Opts = bot_webhook_delivery_sender:ssl_opts(Host),
    ?assertEqual({verify, verify_peer}, lists:keyfind(verify, 1, Opts)),
    ?assertEqual(
        {server_name_indication, "hooks.example.com"},
        lists:keyfind(server_name_indication, 1, Opts)
    ),
    {customize_hostname_check, HostnameOpts} =
        lists:keyfind(customize_hostname_check, 1, Opts),
    {match_fun, MatchFun} = lists:keyfind(match_fun, 1, HostnameOpts),
    ?assert(is_function(MatchFun)).

transport_regression_test_() ->
    {timeout, 30, fun transport_regressions/0}.

transport_regressions() ->
    {ok, _} = application:ensure_all_started(ssl),
    Dir = filename:join(
        os:getenv("TMPDIR", "/tmp"),
        "imboy-sender-" ++ integer_to_list(erlang:system_time(nanosecond))
    ),
    ok = file:make_dir(Dir),
    Cert = filename:join(Dir, "cert.pem"),
    Key = filename:join(Dir, "key.pem"),
    CA = filename:join(Dir, "ca.pem"),
    CAKey = filename:join(Dir, "ca.key"),
    Csr = filename:join(Dir, "leaf.csr"),
    Ext = filename:join(Dir, "ext.cnf"),
    try
        ok = file:write_file(
            Ext,
            <<"subjectAltName=DNS:localhost\nbasicConstraints=CA:FALSE\nextendedKeyUsage=serverAuth\n">>
        ),
        %% Only local disposable material; no external issuer or network access.
        openssl([
            "req",
            "-x509",
            "-newkey",
            "ec",
            "-pkeyopt",
            "ec_paramgen_curve:P-256",
            "-nodes",
            "-days",
            "1",
            "-subj",
            "/CN=SenderTestCA",
            "-addext",
            "basicConstraints=critical,CA:TRUE",
            "-keyout",
            CAKey,
            "-out",
            CA
        ]),
        openssl([
            "req",
            "-new",
            "-newkey",
            "ec",
            "-pkeyopt",
            "ec_paramgen_curve:P-256",
            "-nodes",
            "-subj",
            "/CN=localhost",
            "-keyout",
            Key,
            "-out",
            Csr
        ]),
        openssl([
            "x509",
            "-req",
            "-in",
            Csr,
            "-CA",
            CA,
            "-CAkey",
            CAKey,
            "-set_serial",
            "2",
            "-days",
            "1",
            "-extfile",
            Ext,
            "-out",
            Cert
        ]),
        {ok, Pem} = file:read_file(CA),
        [{'Certificate', Der, _}] = public_key:pem_decode(Pem),
        ok = meck:new(config_ds, [passthrough, no_link]),
        ok = meck:expect(config_ds, env, fun(_, _) -> 1000 end),
        %% Only the CA lookup is mocked; all TLS and sender functions execute normally.
        ok = meck:new(public_key, [passthrough, no_link, unstick]),
        ok = meck:expect(public_key, cacerts_get, fun() -> [Der] end),
        transport_matrix(Cert, Key, Der)
    after
        try
            meck:unload(public_key)
        catch
            _:_ -> ok
        end,
        try
            meck:unload(config_ds)
        catch
            _:_ -> ok
        end,
        lists:foreach(fun file:delete/1, [Cert, Key, CA, CAKey, Csr, Ext]),
        file:del_dir(Dir)
    end.

transport_matrix(Cert, Key, Der) ->
    ok = meck:expect(public_key, cacerts_get, fun() -> [] end),
    ?assertMatch({error, {tls_connect, _}}, loopback(ssl, Cert, Key, <<"localhost">>, [])),
    ok = meck:expect(public_key, cacerts_get, fun() -> [Der] end),
    ?assertEqual(
        {ok, 204},
        loopback(
            ssl,
            Cert,
            Key,
            <<"localhost">>,
            [<<"HTTP/1.1 204 No Content\r\nContent-Length: 0\r\n\r\n">>]
        )
    ),
    ?assertMatch({error, {tls_connect, _}}, loopback(ssl, Cert, Key, <<"wrong.invalid">>, [])),
    ?assertEqual(
        {ok, 302},
        loopback(
            gen_tcp,
            Cert,
            Key,
            <<"localhost">>,
            [<<"HTTP/1.1 302 Found\r\nLocation: http://127.0.0.1:1/\r\n\r\n">>]
        )
    ),
    Cap = bot_webhook_delivery_sender:response_cap_bytes(),
    Prefix = <<"HTTP/1.1 200 OK\r\nX-Pad: ">>,
    Head =
        <<Prefix/binary, (binary:copy(<<"a">>, Cap - byte_size(Prefix) - 4))/binary, "\r\n\r\n">>,
    ?assertEqual({ok, 200}, loopback(gen_tcp, Cert, Key, <<"localhost">>, [Head])),
    First = binary:part(Head, 0, Cap - 8),
    ?assertEqual(
        {error, head_too_large},
        loopback(
            gen_tcp,
            Cert,
            Key,
            <<"localhost">>,
            [First, <<"aaaaaaaaa\r\n\r\n">>]
        )
    ),
    ?assertEqual({error, {recv, timeout}}, loopback(gen_tcp, Cert, Key, <<"localhost">>, [])).

openssl(Args) ->
    Port = open_port(
        {spawn_executable, os:find_executable("openssl")},
        [binary, exit_status, use_stdio, stderr_to_stdout, {args, Args}]
    ),
    openssl_result(Port, <<>>).

openssl_result(Port, Output) ->
    receive
        {Port, {data, Data}} -> openssl_result(Port, <<Output/binary, Data/binary>>);
        {Port, {exit_status, 0}} -> ok;
        {Port, {exit_status, Code}} -> error({openssl_failed, Code, Output})
    after 5000 ->
        port_close(Port),
        error(openssl_timeout)
    end.

loopback(Transport, Cert, Key, Host, Chunks) ->
    Options = [binary, {active, false}, {ip, {127, 0, 0, 1}}, {reuseaddr, true}],
    {ok, Listen} =
        case Transport of
            ssl -> ssl:listen(0, [{certfile, Cert}, {keyfile, Key} | Options]);
            gen_tcp -> gen_tcp:listen(0, Options)
        end,
    {ok, {_, Port}} =
        case Transport of
            ssl -> ssl:sockname(Listen);
            gen_tcp -> inet:sockname(Listen)
        end,
    {Pid, Ref} = spawn_monitor(fun() -> serve(Transport, Listen, Chunks) end),
    try
        bot_webhook_delivery_sender:post(
            {127, 0, 0, 1},
            Port,
            Transport =:= ssl,
            <<"/hook">>,
            Host,
            [],
            <<"{}">>
        )
    after
        Transport:close(Listen),
        exit(Pid, kill),
        receive
            {'DOWN', Ref, process, Pid, _} -> ok
        after 2000 -> error(server_cleanup_timeout)
        end
    end.

serve(Transport, Listen, Chunks) ->
    Accepted =
        case Transport of
            ssl ->
                case ssl:transport_accept(Listen, 2000) of
                    {ok, Socket} -> ssl:handshake(Socket, 2000);
                    Error -> Error
                end;
            gen_tcp ->
                gen_tcp:accept(Listen, 2000)
        end,
    case Accepted of
        {ok, Sock} ->
            try
                {ok, _} = Transport:recv(Sock, 0, 2000),
                lists:foreach(
                    fun(Chunk) ->
                        ok = Transport:send(Sock, Chunk),
                        timer:sleep(30)
                    end,
                    Chunks
                ),
                receive
                    stop -> ok
                after 1500 -> ok
                end
            after
                Transport:close(Sock)
            end;
        {error, _} ->
            ok
    end.
