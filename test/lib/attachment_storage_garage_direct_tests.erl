-module(attachment_storage_garage_direct_tests).
-moduledoc "direct-v1 有界控制面（step-5）：签名头绑定、真 HEAD、Copy dispatch、T44/T16 负向。".
%%%
% 协议 §3/§4（docs/architecture/attachment-direct-transfer-v1-protocol.md）：
%   * presign 绑定 content-type + x-amz-checksum-sha256（同输入同 URL）；
%   * head_bounded 真 HEAD：只消费元数据，不请求或交付正文；
%   * copy_dispatch 单次请求：autoredirect off、200+错误 XML 不算成功、
%     超限 body_too_large；redirect 不跟随；
%   * direct 无全文 GET fallback（head_bounded 不触碰 read/read_snapshot）。
%%%

-include_lib("eunit/include/eunit.hrl").

-define(ENDPOINT, <<"http://owned-storage.invalid">>).
-define(CFG, #{
    endpoint => ?ENDPOINT,
    container => <<"bucket">>,
    access_key => <<"ak">>,
    secret_key => <<"sk">>,
    region => <<"garage">>
}).

%% ---------------------------------------------------------------- presign

presign_test_() ->
    {foreach, fun setup_sign/0, fun cleanup_sign/1, [
        fun checksum_headers_bound_and_deterministic/0,
        fun host_only_legacy_url_unchanged/0
    ]}.

setup_sign() -> {application:get_env(imboy, garage)}.
cleanup_sign({G}) -> restore_env(garage, G).
restore_env(_K, {ok, V}) -> application:set_env(imboy, _K, V);
restore_env(_K, undefined) -> application:unset_env(imboy, _K).

checksum_headers_bound_and_deterministic() ->
    application:set_env(imboy, garage, ?CFG),
    Now = {{2026, 10, 19}, {12, 0, 0}},
    Checksum = base64_urlsafe_zero(),
    Url1 = elib_s3_sign:presign_signed_url_at(
        <<"PUT">>,
        ?ENDPOINT,
        <<"bucket">>,
        <<"u1/a b.png">>,
        [{<<"content-type">>, <<"image/png">>}, {<<"x-amz-checksum-sha256">>, Checksum}],
        Now,
        600,
        ?CFG
    ),
    Url2 = elib_s3_sign:presign_signed_url_at(
        <<"PUT">>,
        ?ENDPOINT,
        <<"bucket">>,
        <<"u1/a b.png">>,
        [{<<"content-type">>, <<"image/png">>}, {<<"x-amz-checksum-sha256">>, Checksum}],
        Now,
        600,
        ?CFG
    ),
    ?assertEqual(Url1, Url2),
    %% 签名头集合绑定 content-type 与 checksum（I04/T06）
    ?assert(binary:match(Url1, <<"X-Amz-SignedHeaders=">>) =/= nomatch),
    ?assert(
        binary:match(Url1, uri_encode_this(<<"content-type;host;x-amz-checksum-sha256">>)) =/=
            nomatch
    ),
    %% checksum 值进入 canonical headers 参与签名（不在 URL query）
    ?assert(
        Url1 =/=
            elib_s3_sign:presign_signed_url_at(
                <<"PUT">>,
                ?ENDPOINT,
                <<"bucket">>,
                <<"u1/a b.png">>,
                [
                    {<<"content-type">>, <<"image/png">>},
                    {<<"x-amz-checksum-sha256">>, base64_urlsafe_one()}
                ],
                Now,
                600,
                ?CFG
            )
    ),
    %% 路径空格编码（T10）
    ?assert(binary:match(Url1, <<"%20">>) =/= nomatch),
    %% 换 checksum → 不同签名（T05 换正文可拒）
    Url3 = elib_s3_sign:presign_signed_url_at(
        <<"PUT">>,
        ?ENDPOINT,
        <<"bucket">>,
        <<"u1/a b.png">>,
        [
            {<<"content-type">>, <<"image/png">>},
            {<<"x-amz-checksum-sha256">>, base64_urlsafe_one()}
        ],
        Now,
        600,
        ?CFG
    ),
    ?assertNotEqual(Url1, Url3),
    ok.

%% V5 旧语义冻结（I14）：host-only + Content-Type 进 query 且不参与签名。
host_only_legacy_url_unchanged() ->
    application:set_env(imboy, garage, ?CFG),
    Now = {{2026, 10, 19}, {12, 0, 0}},
    Url = elib_s3_sign:presign_url_at(
        <<"PUT">>,
        ?ENDPOINT,
        <<"bucket">>,
        <<"k">>,
        <<"image/png">>,
        Now,
        600,
        ?CFG
    ),
    ?assert(binary:match(Url, <<"X-Amz-SignedHeaders=host&">>) =/= nomatch),
    ?assert(binary:match(Url, <<"Content-Type=image%2Fpng">>) =/= nomatch),
    ok.

%% ---------------------------------------------------------------- head_bounded / copy_dispatch

control_test_() ->
    [
        {timeout, 35, ?_test(pure_mock_fixture:run(fun head_bounded_parses_headers/0))},
        {timeout, 35, ?_test(pure_mock_fixture:run(fun head_bounded_404/0))},
        {timeout, 35, ?_test(pure_mock_fixture:run(fun head_bounded_metadata_only/0))},
        {timeout, 35, ?_test(pure_mock_fixture:run(fun copy_dispatch_success_xml/0))},
        {timeout, 35, ?_test(pure_mock_fixture:run(fun copy_dispatch_error_xml_not_confirmed/0))},
        {timeout, 35, ?_test(pure_mock_fixture:run(fun copy_dispatch_oversized_body_rejected/0))},
        {timeout, 35, ?_test(pure_mock_fixture:run(fun copy_dispatch_redirect_not_followed/0))},
        {timeout, 35, ?_test(pure_mock_fixture:run(fun head_bounded_no_full_read/0))}
    ].

%% ---------------------------------------------------------------- TLS 控制面（C02）

tls_control_test_() ->
    [
        {timeout, 45, ?_test(pure_mock_fixture:run(fun tls_trusted_private_ca/0))},
        {timeout, 45, ?_test(pure_mock_fixture:run(fun tls_untrusted_ca_rejected/0))},
        {timeout, 45, ?_test(pure_mock_fixture:run(fun tls_hostname_mismatch_rejected/0))}
    ].

tls_trusted_private_ca() -> tls_control_case(trusted).
tls_untrusted_ca_rejected() -> tls_control_case(untrusted).
tls_hostname_mismatch_rejected() -> tls_control_case(mismatch).

%% 真实 TLS loopback：openssl 生成私有 CA/服务器证书/主机名不匹配证书，
%% ssl listener + 生产 head_bounded 全链路。受信链正向（garage_control_cacerts
%% 显式信任锚）、系统信任库负向（unknown_ca）、主机名不匹配负向三态。
tls_control_case(Mode) ->
    Dir = make_test_pki(),
    try
        {ok, _} = application:ensure_all_started(ssl),
        {ok, _} = application:ensure_all_started(gun),
        RootDer = pki_root_der(Dir),
        CertBase =
            case Mode of
                mismatch -> "mismatch";
                _ -> "server"
            end,
        {ok, LS} = ssl:listen(0, [
            binary,
            {active, false},
            {ip, {127, 0, 0, 1}},
            {certfile, Dir ++ "/" ++ CertBase ++ ".pem"},
            {keyfile, Dir ++ "/" ++ CertBase ++ ".key"},
            {verify, verify_none}
        ]),
        {ok, {_, Port}} = ssl:sockname(LS),
        Server = spawn(fun() -> serve_tls(LS, 0) end),
        EP = iolist_to_binary(["https://127.0.0.1:", integer_to_list(Port)]),
        application:set_env(imboy, garage, maps:put(endpoint, EP, ?CFG)),
        case Mode of
            trusted -> application:set_env(imboy, garage_control_cacerts, [RootDer]);
            _ -> ok
        end,
        try
            pure_mock_fixture:with_mocks(
                [
                    {attachment_storage, [
                        {resolve, 1, fun(<<"g">>) ->
                            {ok, #{
                                driver => <<"garage">>, container => <<"bucket">>, endpoint => EP
                            }}
                        end}
                    ]}
                ],
                fun() ->
                    Result =
                        attachment_storage_garage:head_bounded(<<"g">>, <<"bucket">>, <<"key">>),
                    case Mode of
                        trusted ->
                            ?assertMatch(
                                {ok, #{size := 123, content_type := <<"application/xml">>}}, Result
                            );
                        _ ->
                            %% 证书不受信/主机名不匹配必须握手拒绝，绝不回退明文 HTTP
                            ?assertMatch({error, _}, Result)
                    end
                end
            )
        after
            ssl:close(LS),
            exit(Server, kill)
        end
    after
        _ = file:del_dir_r(Dir)
    end.

make_test_pki() ->
    Dir = string:trim(os:cmd("mktemp -d /tmp/imboy_c02_pki.XXXXXXXX")),
    _ = os:cmd(
        io_lib:format(
            "openssl req -x509 -newkey rsa:2048 -nodes -keyout ~s/root.key -out ~s/root.pem "
            "-subj '/CN=closeout-c02-root' -days 2 "
            "-addext 'basicConstraints=critical,CA:TRUE' "
            "-addext 'keyUsage=critical,keyCertSign,cRLSign' 2>/dev/null",
            [Dir, Dir]
        )
    ),
    make_leaf_pki(Dir, "server", "127.0.0.1"),
    make_leaf_pki(Dir, "mismatch", "127.0.0.2"),
    true = filelib:is_regular(Dir ++ "/root.pem"),
    true = filelib:is_regular(Dir ++ "/server.pem"),
    true = filelib:is_regular(Dir ++ "/mismatch.pem"),
    Dir.

make_leaf_pki(Dir, Base, HostIp) ->
    _ = os:cmd(
        io_lib:format(
            "openssl req -newkey rsa:2048 -nodes -keyout ~s/~s.key -out ~s/~s.csr "
            "-subj '/CN=~s' 2>/dev/null",
            [Dir, Base, Dir, Base, HostIp]
        )
    ),
    ok = file:write_file(
        Dir ++ "/" ++ Base ++ ".ext",
        "subjectAltName=IP:" ++ HostIp ++
            "\nkeyUsage=critical,digitalSignature,keyEncipherment\nextendedKeyUsage=serverAuth\n"
    ),
    _ = os:cmd(
        io_lib:format(
            "openssl x509 -req -in ~s/~s.csr -CA ~s/root.pem -CAkey ~s/root.key -CAcreateserial "
            "-out ~s/~s.pem -days 2 -extfile ~s/~s.ext 2>/dev/null",
            [Dir, Base, Dir, Dir, Dir, Base, Dir, Base]
        )
    ),
    ok.

pki_root_der(Dir) ->
    {ok, Pem} = file:read_file(Dir ++ "/root.pem"),
    [{'Certificate', Der, _} | _] =
        [E || E <- public_key:pem_decode(Pem), element(1, E) =:= 'Certificate'],
    Der.

serve_tls(_LS, N) when N >= 3 ->
    ok;
serve_tls(LS, N) ->
    case ssl:transport_accept(LS, 6000) of
        {ok, S1} ->
            case ssl:handshake(S1, 6000) of
                {ok, Sock} ->
                    _ = read_tls_request(Sock, <<>>),
                    ssl:send(
                        Sock,
                        "HTTP/1.1 200 OK\r\n"
                        "Content-Length: 123\r\n"
                        "Content-Type: application/xml\r\n"
                        "Connection: close\r\n\r\n"
                    ),
                    ssl:close(Sock);
                _ ->
                    ok
            end,
            serve_tls(LS, N + 1);
        _ ->
            serve_tls(LS, N)
    end.

read_tls_request(Sock, Acc) ->
    case binary:match(Acc, <<"\r\n\r\n">>) of
        nomatch ->
            case ssl:recv(Sock, 0, 3000) of
                {ok, B} -> read_tls_request(Sock, <<Acc/binary, B/binary>>);
                _ -> Acc
            end;
        _ ->
            Acc
    end.

strict_head_length_test_() ->
    Cases = [
        {[], {error, invalid_content_length}},
        {[{"content-length", ""}], {error, invalid_content_length}},
        {[{"content-length", "garbage"}], {error, invalid_content_length}},
        {[{"content-length", "-1"}], {error, invalid_content_length}},
        {[{"content-length", "+1"}], {error, invalid_content_length}},
        {[{"content-length", "1.0"}], {error, invalid_content_length}},
        {[{"content-length", " 1"}], {error, invalid_content_length}},
        {[{"content-length", "1, 1"}], {error, invalid_content_length}},
        {[{"content-length", "1"}, {"content-length", "1"}], {error, invalid_content_length}},
        {[{"content-length", "1"}, {"Content-Length", "2"}], {error, invalid_content_length}},
        {[{"content-length", "9223372036854775808"}], {error, invalid_content_length}},
        {[{"content-length", lists:duplicate(200, $9)}], {error, invalid_content_length}},
        {[{"Content-Length", "0"}], {ok, 0}},
        {[{"content-length", "0001"}], {ok, 1}},
        {[{"content-length", "9223372036854775807"}], {ok, 9223372036854775807}}
    ],
    [
        {timeout, 35,
            ?_test(
                pure_mock_fixture:run(fun() ->
                    check_head_length(Entry, Headers, Expected)
                end)
            )}
     || Entry <- [head, bounded, snapshot, legacy], {Headers, Expected} <- Cases
    ].

check_head_length(Entry, Headers, Expected) ->
    Script = fun(head, _Url, [], Receiver, ReqId) ->
        Receiver ! {http, {ReqId, {{"HTTP/1.1", 200, "OK"}, Headers, <<>>}}}
    end,
    with_ctrl_mocks(Script, fun() ->
        Result =
            case Entry of
                head ->
                    attachment_storage_garage:head(<<"g">>, <<"bucket">>, <<"key">>);
                bounded ->
                    attachment_storage_garage:head_bounded(<<"g">>, <<"bucket">>, <<"key">>);
                snapshot ->
                    attachment_storage_garage:head_snapshot(#{
                        endpoint => ?ENDPOINT,
                        container => <<"bucket">>,
                        key => <<"key">>,
                        garage_config => ?CFG
                    });
                legacy ->
                    elib_oss:head_object(<<"bucket">>, <<"key">>)
            end,
        case Expected of
            {ok, Size} -> ?assertMatch({ok, #{size := Size}}, Result);
            _ -> ?assertEqual(Expected, Result)
        end
    end).

legacy_head_bounded_test_() ->
    [
        {timeout, 35,
            ?_test(
                pure_mock_fixture:run(fun() ->
                    Script = fun(head, Url, [], Receiver, ReqId) ->
                        ?assert(
                            binary:match(list_to_binary(Url), <<"X-Amz-Signature=">>) =/= nomatch
                        ),
                        Receiver ! {http, {ReqId, Response}}
                    end,
                    with_ctrl_mocks(Script, fun() ->
                        ?assertEqual(Expected, elib_oss:head_object(<<"bucket">>, <<"key">>))
                    end)
                end)
            )}
     || {Response, Expected} <- [
            {
                {{"HTTP/1.1", 200, "OK"}, [{"content-length", "5"}], <<"hello">>},
                {ok, #{size => 5, content_type => <<"application/octet-stream">>}}
            },
            {{{"HTTP/1.1", 404, "Not Found"}, [], <<>>}, {error, not_found}},
            {{{"HTTP/1.1", 403, "Forbidden"}, [], <<>>}, {error, {http_error, 403}}},
            {{{"HTTP/1.1", 302, "Found"}, [], <<>>}, {error, {http_error, 302}}},
            {{error, timeout}, {error, timeout}}
        ]
    ].

setup_ctrl() ->
    application:set_env(imboy, garage, ?CFG),
    ok.

http_mocks(Script) ->
    %% Script: fun(Method, Url, Headers, Receiver, ReqId) -> spawn sender
    {httpc, [
        {request, 4, fun(Method, Req, Opts, ClientOpts) ->
            Receiver = self(),
            {Url, Headers} =
                case {Method, Req} of
                    {head, {RequestUrl, RequestHeaders}} ->
                        {RequestUrl, RequestHeaders};
                    {put, {RequestUrl, RequestHeaders, "application/octet-stream", <<>>}} ->
                        {RequestUrl, RequestHeaders}
                end,
            AutoRedirect = lists:keyfind(autoredirect, 1, Opts),
            ?assertEqual({autoredirect, false}, AutoRedirect),
            ?assertEqual({stream, {self, once}}, lists:keyfind(stream, 1, ClientOpts)),
            ?assertEqual({body_format, binary}, lists:keyfind(body_format, 1, ClientOpts)),
            ReqId = erlang:unique_integer([positive]),
            spawn(fun() ->
                timer:sleep(1),
                Script(Method, Url, Headers, Receiver, ReqId)
            end),
            {ok, ReqId}
        end},
        {stream_next, 1, fun(_Pid) -> ok end},
        {cancel_request, 1, fun(_ReqId) -> ok end}
    ]}.

with_ctrl_mocks(Script, Inner) ->
    setup_ctrl(),
    pure_mock_fixture:with_mocks(
        [
            http_mocks(Script),
            head_mocks(Script),
            {attachment_storage, [
                {resolve, 1, fun(<<"g">>) ->
                    {ok, #{
                        driver => <<"garage">>,
                        container => <<"bucket">>,
                        endpoint => ?ENDPOINT
                    }}
                end}
            ]}
        ],
        Inner
    ).

head_mocks(Script) ->
    {gun, [
        {open, 3, fun(_, _, Options) ->
            ?assertEqual(0, maps:get(retry, Options)),
            ?assertEqual([http], maps:get(protocols, Options)),
            ?assertEqual(65536, maps:get(max_header_block_size, maps:get(http_opts, Options))),
            {ok, self()}
        end},
        {await_up, 2, fun(_, _) -> {ok, http} end},
        {head, 3, fun(_, Path, []) ->
            ReqId = make_ref(),
            put({gun_mock_kind, ReqId}, head),
            Script(head, binary_to_list(Path), [], self(), ReqId),
            ReqId
        end},
        {request, 5, fun(_, MethodBin, Path, Headers, _Body) ->
            %% C02：Copy 控制请求统一 Gun transport 后的请求行/头可见性。
            ReqId = make_ref(),
            put({gun_mock_kind, ReqId}, put),
            StrHeaders = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- Headers],
            Script(mock_method(MethodBin), binary_to_list(Path), StrHeaders, self(), ReqId),
            ReqId
        end},
        {await, 3, fun(_, ReqId, _) -> mock_gun_event(ReqId) end},
        {cancel, 2, fun(_, _) -> ok end},
        {close, 1, fun(_) -> ok end},
        {flush, 1, fun(_) -> ok end}
    ]}.

mock_method(<<"PUT">>) -> put;
mock_method(<<"GET">>) -> get;
mock_method(<<"DELETE">>) -> delete.

%% Script 的 httpc 风格消息 → Gun await 事件序列（response/data 两段）。
%% body 缓存在进程字典，下一次 await 以 data fin/nofin 交付，与生产
%% gun 数据流语义一致（流未终结交付 nofin，超限取消走 cancel）。
mock_gun_event(ReqId) ->
    case get({gun_mock_data, ReqId}) of
        {Bin, Fin} ->
            erase({gun_mock_data, ReqId}),
            {data, Fin, Bin};
        undefined ->
            receive
                {http, {ReqId, {{_, Status, _}, Headers, Body}}} when
                    is_binary(Body), byte_size(Body) > 0
                ->
                    case get({gun_mock_kind, ReqId}) of
                        head ->
                            %% HEAD 响应按协议无 body，违规字节忽略（真实 Gun 不发 data）
                            {response, fin, Status, mock_hdr(Headers)};
                        _ ->
                            put({gun_mock_data, ReqId}, {Body, fin}),
                            {response, nofin, Status, mock_hdr(Headers)}
                    end;
                {http, {ReqId, {{_, Status, _}, Headers, _}}} ->
                    {response, fin, Status, mock_hdr(Headers)};
                {http, {ReqId, stream_start, Headers, _}} ->
                    mock_gun_stream_event(ReqId, Headers);
                {http, {ReqId, {error, _} = Error}} ->
                    Error
            after 2000 ->
                {error, timeout}
            end
    end.

mock_gun_stream_event(ReqId, Headers) ->
    case get({gun_mock_kind, ReqId}) of
        head ->
            %% 真实 Gun 对 HEAD 请求不产生 data 事件（响应按协议无 body，
            %% 服务器违规多发的字节被忽略）：与旧 mock 语义一致。
            receive
                {http, {ReqId, stream, _}} ->
                    _ =
                        receive
                            {http, {ReqId, stream_end, _}} -> ok
                        after 0 -> ok
                        end,
                    {response, fin, 200, mock_hdr(Headers)};
                {http, {ReqId, stream_end, _}} ->
                    {response, fin, 200, mock_hdr(Headers)}
            after 2000 ->
                {error, timeout}
            end;
        _ ->
            receive
                {http, {ReqId, stream, Bin}} ->
                    Fin =
                        receive
                            {http, {ReqId, stream_end, _}} -> fin
                        after 0 -> nofin
                        end,
                    put({gun_mock_data, ReqId}, {Bin, Fin}),
                    {response, nofin, 200, mock_hdr(Headers)};
                {http, {ReqId, stream_end, _}} ->
                    {response, fin, 200, mock_hdr(Headers)}
            after 2000 ->
                {error, timeout}
            end
    end.

mock_hdr(Headers) ->
    [{list_to_binary(K), list_to_binary(V)} || {K, V} <- Headers].

head_bounded_parses_headers() ->
    Script = fun(head, _Url, [], Receiver, ReqId) ->
        Receiver !
            {http,
                {ReqId, stream_start,
                    [
                        {"content-length", "1024"},
                        {"content-type", "image/png"},
                        {"etag", "\"abc\""}
                    ],
                    fake_pid}},
        Receiver ! {http, {ReqId, stream_end, []}}
    end,
    with_ctrl_mocks(Script, fun() ->
        ?assertMatch(
            {ok, #{size := 1024, content_type := <<"image/png">>, etag := <<"\"abc\"">>}},
            attachment_storage_garage:head_bounded(<<"g">>, <<"bucket">>, <<"u1/a.png">>)
        )
    end).

head_bounded_404() ->
    Script = fun(head, _Url, [], Receiver, ReqId) ->
        Receiver ! {http, {ReqId, {{"HTTP/1.1", 404, "Not Found"}, [], <<>>}}}
    end,
    with_ctrl_mocks(Script, fun() ->
        ?assertEqual(
            {error, not_found},
            attachment_storage_garage:head_bounded(<<"g">>, <<"bucket">>, <<"missing">>)
        )
    end).

head_bounded_metadata_only() ->
    Script = fun(head, _Url, [], Receiver, ReqId) ->
        Receiver ! {http, {ReqId, stream_start, [{"content-length", "5"}], fake_pid}},
        Receiver ! {http, {ReqId, stream, <<"hello">>}}
    end,
    with_ctrl_mocks(Script, fun() ->
        ?assertEqual(
            {ok, #{size => 5, content_type => <<"application/octet-stream">>}},
            attachment_storage_garage:head_bounded(<<"g">>, <<"bucket">>, <<"u1/a.png">>)
        )
    end).

copy_dispatch_success_xml() ->
    Script = fun(put, _Url, Headers, Receiver, ReqId) ->
        ?assert(lists:keyfind("x-amz-copy-source", 1, Headers) =/= false),
        {_, CS} = lists:keyfind("x-amz-copy-source", 1, Headers),
        ?assertEqual("/bucket/staging/src", CS),
        Xml =
            <<"<CopyObjectResult><ETag>\"e1\"</ETag><LastModified>2026-10-19</LastModified></CopyObjectResult>">>,
        Receiver !
            {http,
                {ReqId, stream_start, [{"content-length", integer_to_list(byte_size(Xml))}],
                    fake_pid}},
        Receiver ! {http, {ReqId, stream, Xml}},
        Receiver ! {http, {ReqId, stream_end, []}}
    end,
    with_ctrl_mocks(Script, fun() ->
        ?assertMatch(
            {ok, #{etag := <<"\"e1\"">>}},
            attachment_storage_garage:copy_dispatch(
                <<"g">>, <<"bucket">>, <<"staging/src">>, <<"direct-c/s/1/t">>
            )
        )
    end).

copy_dispatch_error_xml_not_confirmed() ->
    %% T16：HTTP 200 但 XML 错误形态不确认
    Script = fun(put, _Url, _H, Receiver, ReqId) ->
        Xml = <<"<?xml version=\"1.0\"?><Error><Code>InternalError</Code></Error>">>,
        Receiver !
            {http,
                {ReqId, stream_start, [{"content-length", integer_to_list(byte_size(Xml))}],
                    fake_pid}},
        Receiver ! {http, {ReqId, stream, Xml}},
        Receiver ! {http, {ReqId, stream_end, []}}
    end,
    with_ctrl_mocks(Script, fun() ->
        ?assertEqual(
            {error, copy_xml_error},
            attachment_storage_garage:copy_dispatch(
                <<"g">>, <<"bucket">>, <<"staging/src">>, <<"direct-c/s/1/t">>
            )
        )
    end).

copy_dispatch_oversized_body_rejected() ->
    %% T44：超限响应被拒绝，不完整缓冲
    Script = fun(put, _Url, _H, Receiver, ReqId) ->
        Receiver ! {http, {ReqId, stream_start, [{"content-length", "999999"}], fake_pid}},
        Receiver ! {http, {ReqId, stream, binary:copy(<<"x">>, 70000)}}
    end,
    with_ctrl_mocks(Script, fun() ->
        ?assertEqual(
            {error, body_too_large},
            attachment_storage_garage:copy_dispatch(
                <<"g">>, <<"bucket">>, <<"staging/src">>, <<"direct-c/s/1/t">>
            )
        )
    end).

copy_dispatch_redirect_not_followed() ->
    %% 一次授权一次请求：redirect 状态返回为错误，不产生第二次请求
    Script = fun(put, _Url, _H, Receiver, ReqId) ->
        Receiver !
            {http,
                {ReqId, {
                    {"HTTP/1.1", 307, "Temporary Redirect"},
                    [{"location", "http://elsewhere.invalid/x"}],
                    <<>>
                }}}
    end,
    with_ctrl_mocks(Script, fun() ->
        ?assertEqual(
            {error, {storage_status, 307}},
            attachment_storage_garage:copy_dispatch(
                <<"g">>, <<"bucket">>, <<"staging/src">>, <<"direct-c/s/1/t">>
            )
        )
    end).

%% head_bounded 不落全文 read：read_snapshot 不可达（meck 未声明即 raise）
head_bounded_no_full_read() ->
    Script = fun(head, _Url, [], Receiver, ReqId) ->
        Receiver !
            {http,
                {ReqId, stream_start, [{"content-length", "999999"}, {"content-type", "video/mp4"}],
                    fake_pid}},
        Receiver ! {http, {ReqId, stream_end, []}}
    end,
    with_ctrl_mocks(Script, fun() ->
        ?assertMatch(
            {ok, #{size := 999999}},
            attachment_storage_garage:head_bounded(<<"g">>, <<"bucket">>, <<"big.bin">>)
        )
    end).

real_http_control_test_() ->
    [
        {timeout, 35, ?_test(pure_mock_fixture:run(fun() -> real_http_control(Kind) end))}
     || Kind <- [
            head_ok,
            head_informational,
            head_unexpected_body,
            head_missing,
            head_duplicate,
            head_missing_length,
            head_oversized_headers,
            head_preparse_limit,
            legacy_head_ok,
            legacy_head_zero,
            legacy_head_forbidden,
            copy_ok,
            copy_error_xml,
            copy_missing,
            copy_oversized,
            copy_non2xx_oversized
        ]
    ].

real_http_control(Kind) ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, Listener} = gen_tcp:listen(0, [binary, {active, false}, {ip, {127, 0, 0, 1}}]),
    {ok, {_, Port}} = inet:sockname(Listener),
    Endpoint = iolist_to_binary(["http://127.0.0.1:", integer_to_list(Port)]),
    Owner = self(),
    Ref = make_ref(),
    {Server, Monitor} = spawn_monitor(fun() -> serve_control(Listener, Kind, Owner, Ref) end),
    application:set_env(imboy, garage, maps:put(endpoint, Endpoint, ?CFG)),
    try
        pure_mock_fixture:with_mocks(
            [
                {attachment_storage, [
                    {resolve, 1, fun(<<"g">>) ->
                        {ok, #{
                            driver => <<"garage">>, container => <<"bucket">>, endpoint => Endpoint
                        }}
                    end}
                ]}
            ],
            fun() ->
                Result =
                    case Kind of
                        head_ok ->
                            attachment_storage_garage:head_bounded(
                                <<"g">>, <<"bucket">>, <<"key">>
                            );
                        HeadKind when
                            HeadKind =:= head_informational;
                            HeadKind =:= head_unexpected_body;
                            HeadKind =:= head_missing;
                            HeadKind =:= head_duplicate;
                            HeadKind =:= head_missing_length;
                            HeadKind =:= head_oversized_headers;
                            HeadKind =:= head_preparse_limit
                        ->
                            attachment_storage_garage:head_bounded(
                                <<"g">>, <<"bucket">>, <<"key">>
                            );
                        LegacyKind when
                            LegacyKind =:= legacy_head_ok;
                            LegacyKind =:= legacy_head_zero;
                            LegacyKind =:= legacy_head_forbidden
                        ->
                            elib_oss:head_object(<<"bucket">>, <<"key">>);
                        _ ->
                            attachment_storage_garage:copy_dispatch(
                                <<"g">>, <<"bucket">>, <<"source">>, <<"key">>
                            )
                    end,
                assert_control_result(Kind, Result)
            end
        ),
        receive
            {Ref, Method} ->
                Expected =
                    case Kind of
                        CopyKind when
                            CopyKind =:= copy_ok;
                            CopyKind =:= copy_error_xml;
                            CopyKind =:= copy_missing;
                            CopyKind =:= copy_oversized;
                            CopyKind =:= copy_non2xx_oversized
                        ->
                            <<"PUT">>;
                        _ ->
                            <<"HEAD">>
                    end,
                ?assertEqual(Expected, Method)
        after 5000 -> error(control_request_missing)
        end,
        receive
            {'DOWN', Monitor, process, Server, normal} -> ok
        after 5000 -> error(control_server_not_closed)
        end
    after
        gen_tcp:close(Listener),
        exit(Server, kill),
        erlang:demonitor(Monitor, [flush])
    end.

serve_control(Listener, Kind, Owner, Ref) ->
    {ok, Socket} = gen_tcp:accept(Listener, 5000),
    try
        Request = read_control_request(Socket, <<>>),
        [Method | _] = binary:split(Request, <<" ">>, [global]),
        case Method of
            <<"HEAD">> ->
                ?assertEqual(nomatch, binary:match(string:lowercase(Request), <<"\r\nrange:">>));
            _ ->
                ok
        end,
        Owner ! {Ref, Method},
        case Kind of
            head_preparse_limit ->
                %% C02：跨 TCP 段持续发送未终结 header 块（9×~8.2KB > 64KiB），
                %% 证明 Gun parser 在 preparse 累积路径触发收前硬限 limit_reached，
                %% 而不是等整块到达后的事后复核。
                _ = [
                    begin
                        ok = gen_tcp:send(Socket, [
                            "X-Flood-",
                            integer_to_list(I),
                            ": ",
                            lists:duplicate(8176, $x),
                            "\r\n"
                        ]),
                        timer:sleep(15)
                    end
                 || I <- lists:seq(1, 9)
                ],
                drain_until_closed(Socket),
                exit(normal);
            copy_non2xx_oversized ->
                %% C02：非 2xx + 超大错误体分块下发，证明 Copy 控制路径对
                %% 错误响应体同样逐块累计超限即取消（无服务端错误体预缓冲）。
                ok = gen_tcp:send(Socket, [
                    "HTTP/1.1 404 Not Found\r\n"
                    "Content-Type: application/xml\r\n"
                    "Content-Length: 131076\r\n"
                    "Connection: close\r\n\r\n"
                ]),
                _ = [
                    begin
                        send_ignoring_errors(Socket, binary:copy(<<"x">>, 43692)),
                        timer:sleep(15)
                    end
                 || _ <- lists:seq(1, 3)
                ],
                exit(normal);
            _ ->
                ok
        end,
        {Status, Body} =
            case Kind of
                HeadKind when
                    HeadKind =:= head_ok;
                    HeadKind =:= head_informational;
                    HeadKind =:= head_duplicate;
                    HeadKind =:= head_missing_length;
                    HeadKind =:= head_oversized_headers;
                    HeadKind =:= legacy_head_ok;
                    HeadKind =:= legacy_head_zero
                ->
                    {"200 OK", <<>>};
                head_unexpected_body ->
                    {"200 OK", <<"hello">>};
                head_missing ->
                    {"404 Not Found", <<>>};
                legacy_head_forbidden ->
                    {"403 Forbidden", <<>>};
                copy_ok ->
                    {"200 OK", <<"<CopyObjectResult><ETag>owned-etag</ETag></CopyObjectResult>">>};
                copy_error_xml ->
                    {"200 OK", <<"<Error><Code>AccessDenied</Code></Error>">>};
                copy_missing ->
                    {"404 Not Found", <<"missing">>};
                copy_oversized ->
                    {"200 OK", binary:copy(<<"x">>, 65537)}
            end,
        LengthHeader =
            case Kind of
                head_ok ->
                    "Content-Length: 123\r\n";
                head_informational ->
                    "Content-Length: 123\r\n";
                legacy_head_ok ->
                    "Content-Length: 123\r\n";
                head_duplicate ->
                    "Content-Length: 123\r\ncOnTeNt-LeNgTh: 123\r\n";
                head_missing_length ->
                    "";
                head_oversized_headers ->
                    ["Content-Length: 0\r\nX-Large: ", lists:duplicate(65537, $x), "\r\n"];
                _ ->
                    ["Content-Length: ", integer_to_list(byte_size(Body)), "\r\n"]
            end,
        case Kind of
            head_informational ->
                ok = gen_tcp:send(Socket, "HTTP/1.1 103 Early Hints\r\nLink: </owned>\r\n\r\n");
            _ ->
                ok
        end,
        ok = gen_tcp:send(Socket, [
            "HTTP/1.1 ",
            Status,
            "\r\n",
            LengthHeader,
            "Content-Type: application/xml\r\nConnection: close\r\n\r\n",
            Body
        ])
    after
        gen_tcp:close(Socket)
    end.

read_control_request(Socket, Bytes) when byte_size(Bytes) < 65536 ->
    case binary:match(Bytes, <<"\r\n\r\n">>) of
        nomatch ->
            {ok, Chunk} = gen_tcp:recv(Socket, 0, 5000),
            read_control_request(Socket, <<Bytes/binary, Chunk/binary>>);
        _ ->
            Bytes
    end.

assert_control_result(head_ok, Result) ->
    ?assertMatch({ok, #{size := 123}}, Result);
assert_control_result(head_informational, Result) ->
    ?assertMatch({ok, #{size := 123}}, Result);
assert_control_result(head_unexpected_body, Result) ->
    ?assertEqual({ok, #{size => 5, content_type => <<"application/xml">>}}, Result);
assert_control_result(head_missing, Result) ->
    ?assertEqual({error, not_found}, Result);
assert_control_result(head_duplicate, Result) ->
    ?assertEqual({error, invalid_content_length}, Result);
assert_control_result(head_missing_length, Result) ->
    ?assertEqual({error, invalid_content_length}, Result);
assert_control_result(head_oversized_headers, Result) ->
    ?assertMatch({error, _}, Result);
assert_control_result(head_preparse_limit, Result) ->
    %% Gun parser 收前硬限（非事后复核）：跨段未终结 header 块触发 limit_reached；
    %% gun:await 对连接级错误有 {connection_error, _} 外层包装（gun 2.6.0）。
    ?assertMatch(
        {error, {connection_error, {connection_error, limit_reached, _}}}, Result
    );
assert_control_result(legacy_head_ok, Result) ->
    ?assertMatch({ok, #{size := 123}}, Result);
assert_control_result(legacy_head_zero, Result) ->
    ?assertMatch({ok, #{size := 0}}, Result);
assert_control_result(legacy_head_forbidden, Result) ->
    ?assertEqual({error, {http_error, 403}}, Result);
assert_control_result(copy_ok, Result) ->
    ?assertEqual({ok, #{etag => <<"owned-etag">>}}, Result);
assert_control_result(copy_error_xml, Result) ->
    ?assertEqual({error, copy_xml_error}, Result);
assert_control_result(copy_missing, Result) ->
    ?assertEqual({error, not_found}, Result);
assert_control_result(copy_oversized, Result) ->
    ?assertEqual({error, body_too_large}, Result);
assert_control_result(copy_non2xx_oversized, Result) ->
    %% 非 2xx 错误体同样受逐块上限约束：超限即取消，不整块接收
    ?assertEqual({error, body_too_large}, Result).

%% ---------------------------------------------------------------- helpers

drain_until_closed(Socket) ->
    case gen_tcp:recv(Socket, 0, 2000) of
        {ok, _} -> drain_until_closed(Socket);
        _ -> ok
    end.

send_ignoring_errors(Socket, Bin) ->
    _ = gen_tcp:send(Socket, Bin),
    ok.

base64_urlsafe_zero() -> base64:encode(binary:copy(<<0>>, 32)).
base64_urlsafe_one() -> base64:encode(binary:copy(<<1>>, 32)).

uri_encode_this(Bin) ->
    iolist_to_binary([uri_char(C) || <<C>> <= Bin]).

uri_char(C) when C >= $A, C =< $Z -> C;
uri_char(C) when C >= $a, C =< $z -> C;
uri_char(C) when C >= $0, C =< $9 -> C;
uri_char($-) -> $-;
uri_char($/) -> $/;
uri_char(C) -> iolist_to_binary(io_lib:format("%~2.16.0B", [C])).
