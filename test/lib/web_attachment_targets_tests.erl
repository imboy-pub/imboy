-module(web_attachment_targets_tests).
-include_lib("eunit/include/eunit.hrl").
base_validation_test() ->
    lists:foreach(fun(V) -> ?assert(web_attachment_targets:valid_base(V)) end,
        [<<"https://s3.example.test">>, <<"https://s3.example.test/proxy/s3">>, <<"http://127.0.0.1:3900">>]),
    lists:foreach(fun(V) -> ?assertNot(web_attachment_targets:valid_base(V)) end,
        [<<>>, <<"http://example.test">>, <<"http://localhost:3900">>, <<"https://u@example.test">>,
         <<"https://example.test/?a=b">>, <<"https://example.test/#f">>,
         <<"https://example.test/a/../b">>, <<"https://example.test/a/%2e%2e/b">>,
         <<"https://example.test/a//b">>, <<"https://example.test:443">>]).
contract_test() ->
    ?assertEqual(#{version => 1, garage => null, edge => null}, web_attachment_targets:contract(<<>>, <<>>, <<>>)).
ticket_test() ->
    Base = <<"https://api.test">>, Edge = <<"https://edge.test/edge">>,
    ?assertEqual({ok, <<"https://edge.test/edge?ticket=a.b-c_1">>},
        web_attachment_targets:resolve(<<"https://api.test/api/v1/attachment/content?ticket=a.b-c_1">>, <<>>, Base, Edge)),
    lists:foreach(fun(Url) -> ?assertEqual({error, forbidden}, web_attachment_targets:resolve(Url, <<>>, Base, Edge)) end,
        [<<"https://api.test.evil/api/v1/attachment/content?ticket=x">>,
         <<"https://api.test/api/v1/attachment/content?ticket=x&other=y">>,
         <<"https://api.test/api/v1/attachment/content?ticket=x%2ey">>]).
garage_test() ->
    Garage = <<"https://s3.test/prefix">>,
    ?assertEqual({ok, <<"https://s3.test/prefix/bucket/key?X-Amz-Signature=abc">>},
        web_attachment_targets:resolve(<<"https://s3.test/prefix/bucket/key?X-Amz-Signature=abc">>, Garage, <<>>, <<>>)),
    lists:foreach(fun(Url) -> ?assertEqual({error, forbidden}, web_attachment_targets:resolve(Url, Garage, <<>>, <<>>)) end,
        [<<"https://s3.test/prefixevil/key?sig=x">>, <<"https://s3.test.evil/prefix/key?sig=x">>,
         <<"https://s3.test/prefix/a/%2e%2e/x?sig=x">>, <<"https://s3.test/prefix/a/../x?sig=x">>,
         <<"https://s3.test/prefix/bucket/key?sig=x#f">>, <<"https://s3.test/prefix/bucket/key">>]).

encoded_objects_test() ->
    G = <<"https://s3.test/prefix">>,
    Good = [<<"https://s3.test/prefix/bucket/a%20b.png?sig=ok">>,
            <<"https://s3.test/prefix/bucket/%E4%BD%A0%E5%A5%BD.png?sig=ok">>,
            <<"https://s3.test/prefix/bucket/100%25.png?sig=ok">>],
    lists:foreach(fun(U) -> ?assertEqual({ok, U}, web_attachment_targets:resolve(U, G, <<>>, <<>>)) end, Good),
    Bad = [<<"https://s3.test/prefix/bucket/a%2Fb?sig=x">>,
           <<"https://s3.test/prefix/bucket/a%5Cb?sig=x">>,
           <<"https://s3.test/prefix/bucket/%2E%2E/x?sig=x">>,
           <<"https://s3.test/prefix/bucket/%252F?sig=x">>,
           <<"https://s3.test/prefix/bucket/a%2fb?sig=x">>],
    lists:foreach(fun(U) -> ?assertEqual({error, forbidden}, web_attachment_targets:resolve(U, G, <<>>, <<>>)) end, Bad).
signer_byte_contract_test() ->
    Key = unicode:characters_to_binary("bucket/你好 test%.png"),
    Encoded = elib_s3_sign:uri_encode_path(Key),
    U = <<"https://s3.test/prefix/", Encoded/binary, "?X-Amz-Signature=abc">>,
    ?assertEqual({ok, U}, web_attachment_targets:resolve(U, <<"https://s3.test/prefix">>, <<>>, <<>>)).

api_base_trailing_slash_test() ->
    Url = <<"https://api.test/prefix/api/v1/attachment/content?ticket=a.b">>,
    Edge = <<"https://edge.test/edge">>,
    Expected = {ok, <<"https://edge.test/edge?ticket=a.b">>},
    lists:foreach(fun(Base) ->
        ?assertEqual(Expected, web_attachment_targets:resolve(Url, <<>>, Base, Edge)),
        ?assertEqual(Edge, maps:get(edge, web_attachment_targets:contract(<<>>, Base, Edge)))
    end, [<<"https://api.test/prefix">>, <<"https://api.test/prefix/">>, <<"https://api.test/prefix///">>]),
    ?assertEqual({error, forbidden}, web_attachment_targets:resolve(Url, <<>>, <<"https://api.test/prefix.evil/">>, Edge)).

typed_garage_percent_filename_test() ->
    Previous = application:get_env(imboy, garage),
    try
        application:set_env(imboy, garage, #{public_endpoint => <<"https://s3.test/prefix">>}),
        Key = <<"u1/name%2F.png">>,
        Url = <<"https://s3.test/prefix/bucket/u1/name%252F.png?X-Amz-Signature=x">>,
        ?assertEqual({ok, Url}, web_attachment_targets:resolve_garage(Url, <<"bucket">>, Key)),
        ?assertEqual({error, forbidden}, web_attachment_targets:resolve_garage(Url, <<"bucket">>, <<"u1/other.png">>)),
        ?assertEqual({error, forbidden}, web_attachment_targets:resolve_garage(Url, <<"bucket">>, <<"u1/../x">>)),
        ?assertEqual({error, forbidden}, web_attachment_targets:resolve_garage(<<"https://evil.test/prefix/bucket/u1/name%252F.png?x">>, <<"bucket">>, Key))
    after
        case Previous of {ok, V} -> application:set_env(imboy, garage, V); undefined -> application:unset_env(imboy, garage) end
    end.

ticket_domain_separation_test() ->
    Secret = <<"synthetic-test-key-do-not-use">>,
    {ok, Web} = attachment_download_ticket:issue_web(1, <<"u1/g">>, Secret),
    {ok, Native} = attachment_download_ticket:issue(1, <<"u1/g">>, Secret),
    ?assertEqual({ok, 1, <<"u1/g">>}, attachment_download_ticket:verify_web(Web, Secret)),
    ?assertEqual({error, invalid}, attachment_download_ticket:verify(Web, Secret)),
    ?assertEqual({error, invalid}, attachment_download_ticket:verify_web(Native, Secret)),
    ?assertEqual({ok, 1, <<"u1/g">>}, attachment_download_ticket:verify(Native, Secret)).
