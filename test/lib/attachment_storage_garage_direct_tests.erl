-module(attachment_storage_garage_direct_tests).
-moduledoc "direct-v1 有界控制面（step-5）：签名头绑定、真 HEAD、Copy dispatch、T44/T16 负向。".
%%%
% 协议 §3/§4（docs/architecture/attachment-direct-transfer-v1-protocol.md）：
%   * presign 绑定 content-type + x-amz-checksum-sha256（同输入同 URL）；
%   * head_bounded 真 HEAD：无正文；到达正文/超限即拒绝（不完整缓冲）；
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
        {timeout, 35, ?_test(pure_mock_fixture:run(fun head_bounded_rejects_body/0))},
        {timeout, 35, ?_test(pure_mock_fixture:run(fun copy_dispatch_success_xml/0))},
        {timeout, 35, ?_test(pure_mock_fixture:run(fun copy_dispatch_error_xml_not_confirmed/0))},
        {timeout, 35, ?_test(pure_mock_fixture:run(fun copy_dispatch_oversized_body_rejected/0))},
        {timeout, 35, ?_test(pure_mock_fixture:run(fun copy_dispatch_redirect_not_followed/0))},
        {timeout, 35, ?_test(pure_mock_fixture:run(fun head_bounded_no_full_read/0))}
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

head_bounded_rejects_body() ->
    %% HEAD 不允许正文（T44：到达即拒，不缓冲）
    Script = fun(head, _Url, [], Receiver, ReqId) ->
        Receiver ! {http, {ReqId, stream_start, [{"content-length", "5"}], fake_pid}},
        Receiver ! {http, {ReqId, stream, <<"hello">>}}
    end,
    with_ctrl_mocks(Script, fun() ->
        ?assertEqual(
            {error, body_too_large},
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
     || Kind <- [head_ok, head_missing, copy_ok, copy_error_xml, copy_missing, copy_oversized]
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
                        head_missing ->
                            attachment_storage_garage:head_bounded(
                                <<"g">>, <<"bucket">>, <<"key">>
                            );
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
                        head_ok -> <<"HEAD">>;
                        head_missing -> <<"HEAD">>;
                        _ -> <<"PUT">>
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
        Owner ! {Ref, Method},
        {Status, Body} =
            case Kind of
                head_ok ->
                    {"200 OK", <<>>};
                head_missing ->
                    {"404 Not Found", <<>>};
                copy_ok ->
                    {"200 OK", <<"<CopyObjectResult><ETag>owned-etag</ETag></CopyObjectResult>">>};
                copy_error_xml ->
                    {"200 OK", <<"<Error><Code>AccessDenied</Code></Error>">>};
                copy_missing ->
                    {"404 Not Found", <<"missing">>};
                copy_oversized ->
                    {"200 OK", binary:copy(<<"x">>, 65537)}
            end,
        Length =
            case Kind of
                head_ok -> 123;
                _ -> byte_size(Body)
            end,
        ok = gen_tcp:send(Socket, [
            "HTTP/1.1 ",
            Status,
            "\r\nContent-Length: ",
            integer_to_list(Length),
            "\r\nContent-Type: application/xml\r\nConnection: close\r\n\r\n",
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

assert_control_result(head_ok, Result) -> ?assertMatch({ok, #{size := 123}}, Result);
assert_control_result(head_missing, Result) -> ?assertEqual({error, not_found}, Result);
assert_control_result(copy_ok, Result) -> ?assertEqual({ok, #{etag => <<"owned-etag">>}}, Result);
assert_control_result(copy_error_xml, Result) -> ?assertEqual({error, copy_xml_error}, Result);
assert_control_result(copy_missing, Result) -> ?assertEqual({error, not_found}, Result);
assert_control_result(copy_oversized, Result) -> ?assertEqual({error, body_too_large}, Result).

%% ---------------------------------------------------------------- helpers

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
