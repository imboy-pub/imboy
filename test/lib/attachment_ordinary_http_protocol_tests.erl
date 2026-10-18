%%% Real middleware, HTTP handlers, PostgreSQL and storage; no application boot.
-module(attachment_ordinary_http_protocol_tests).
-include_lib("eunit/include/eunit.hrl").

http_protocol_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(S) ->
        [
            {atom_to_list(D), {timeout, 120, fun() -> roundtrip(S, D) end}}
         || D <- [disk, garage]
        ]
    end}.

setup() ->
    S = attachment_lifecycle_v5_w403_gateway_protocol_tests:setup(),
    erlang:put(w403_ordinary_setup, S),
    try
        Owned = setup_owned(S),
        erlang:erase(w403_ordinary_setup),
        Owned
    catch Class:Reason:Stack ->
        cleanup(erlang:erase(w403_ordinary_setup)),
        erlang:raise(Class, Reason, Stack)
    end.

setup_owned(S) ->
    application:set_env(imboy, api_auth_switch, <<"on">>),
    application:set_env(imboy, postgre_aes_key, crypto:strong_rand_bytes(32)),
    SignKey = binary:encode_hex(crypto:strong_rand_bytes(32), lowercase),
    Vsn = binary:part(maps:get(run, S), 0, 12),
    ConfigKey = <<"pub.imboy.fixture_android_", Vsn/binary>>,
    {ok, [_]} = elib_pg:query(
        <<
            "INSERT INTO config(tab,key,value,title) VALUES('sys',$1,"
            "'aes_cbc_' || encode(encrypt(encode($2::bytea,'base64')::bytea,$3,'aes-cbc/pad:pkcs'),'base64'),'fixture') RETURNING key"
        >>,
        [ConfigKey, jsone:encode(SignKey), config_ds:env(postgre_aes_key)]
    ),
    S1 = S#{owned_cache_keys => [{config5, ConfigKey}, {config5, ConfigKey, uncached_v1}]},
    erlang:put(w403_ordinary_setup, S1),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/v1/attachment/presign", attach_handler, #{action => presign}},
            {"/api/v1/attachment/confirm", attach_handler, #{action => confirm}},
            {"/api/v1/attachment/gateway/:session", attachment_storage_gateway_handler, #{
                action => put
            }}
        ]}
    ]),
    {ok, Listener} = cowboy:start_clear(
        attv5_ordinary_http,
        [{ip, {127, 0, 0, 1}}, {port, 0}],
        #{
            env => #{dispatch => Dispatch},
            middlewares => [cowboy_router, auth_middleware, cowboy_handler]
        }
    ),
    S2 = S1#{ordinary_listener_owned => Listener},
    erlang:put(w403_ordinary_setup, S2),
    Base = "http://127.0.0.1:" ++ integer_to_list(ranch:get_port(attv5_ordinary_http)),
    application:set_env(imboy, base_url, list_to_binary(Base)),
    Plain = <<"fixture-device|", Vsn/binary, "|android|pub.imboy.fixture">>,
    Headers = [
        {"did", "fixture-device"},
        {"vsn", binary_to_list(Vsn)},
        {"cos", "android"},
        {"pkg", "pub.imboy.fixture"},
        {"sk", binary_to_list(Vsn)},
        {"method", "sha256"},
        {"sign", binary_to_list(elib_hasher:hmac_sha256(Plain, SignKey))}
    ],
    %% Exercise the production legacy-token compatibility branch, never a forged UID route option.
    Token = token_ds:encrypt_token(maps:get(uid, S)),
    S2#{
        base => Base,
        headers => [{"authorization", "Bearer " ++ binary_to_list(Token)} | Headers],
        sign_headers => Headers
    }.

cleanup(S) ->
    case maps:get(ordinary_listener_owned, S, undefined) of
        Listener when is_pid(Listener) ->
            try
                case ranch:info(attv5_ordinary_http) of
                    #{pid := Listener} -> cowboy:stop_listener(attv5_ordinary_http);
                    _ -> ok
                end
            catch _:_ -> ok
            end;
        _ -> ok
    end,
    attachment_lifecycle_v5_w403_gateway_protocol_tests:cleanup(S).

roundtrip(S, D) ->
    Ns =
        case D of
            disk -> <<"w403-disk">>;
            garage -> <<"w403-garage">>
        end,
    Registry = attachment_storage:registry(),
    application:set_env(imboy, attachment_storage_registry, Registry#{
        upload_namespaces => #{<<"private">> => Ns}
    }),
    Cid = elib_uuid:gen_v7(),
    Path =
        "/api/v1/attachment/presign?filename=fixture.png&mime_type=image%2Fpng&client_upload_id=" ++
            binary_to_list(Cid),
    {_, Denied} = get(S, Path, maps:get(sign_headers, S)),
    ?assertNotEqual(0, maps:get(<<"code">>, Denied)),
    {200, Registered} = get(S, Path, maps:get(headers, S)),
    ?assertEqual(0, maps:get(<<"code">>, Registered)),
    Data = maps:get(<<"payload">>, Registered),
    Key = maps:get(<<"object_key">>, Data),
    {200, Repeated} = get(S, Path, maps:get(headers, S)),
    ?assertEqual(Key, maps:get(<<"object_key">>, maps:get(<<"payload">>, Repeated))),
    Url0 = maps:get(<<"put_url">>, Data),
    Url =
        case Url0 of
            <<"/", _/binary>> -> maps:get(base, S) ++ binary_to_list(Url0);
            _ -> binary_to_list(Url0)
        end,
    Gateway = maps:get(base, S) ++ "/api/v1/attachment/gateway/" ++ binary_to_list(Cid),
    {ok, {{_, 403, _}, _, _}} = httpc:request(
        put, {Gateway, [], "image/png", <<"unauthorized">>}, [{timeout, 30000}], []
    ),
    {ok, {{_, 403, _}, _, _}} = httpc:request(
        put,
        {Gateway ++ "?capability=invalid", [], "image/png", <<"unauthorized">>},
        [{timeout, 30000}],
        []
    ),
    {ok, {{_, 405, _}, _, _}} = httpc:request(get, {Gateway, []}, [{timeout, 30000}], []),
    {ok, I0} = attachment_storage:session_location(Key),
    Mod0 = attachment_storage:driver_module(atom_to_binary(maps:get(driver, I0))),
    ?assertEqual({error, not_found}, Mod0:read_snapshot(I0)),
    Bytes = <<137, 80, 78, 71, 13, 10, 26, 10, "synthetic immutable body">>,
    {ok, {{_, 200, _}, _, _}} = httpc:request(
        put, {Url, [], "image/png", Bytes}, [{timeout, 30000}], [{body_format, binary}]
    ),
    ?assertEqual({ok, Bytes}, Mod0:read_snapshot(I0)),
    Body = jsone:encode(#{<<"object_key">> => Key, <<"scope">> => <<"private">>}),
    Confirmed = post(S, "/api/v1/attachment/confirm", Body),
    case maps:get(<<"code">>, Confirmed) of
        0 -> ok;
        Error -> erlang:error({confirm_rejected, Error, maps:get(<<"msg">>, Confirmed)})
    end,
    Replay = post(S, "/api/v1/attachment/confirm", Body),
    ?assertEqual(maps:get(<<"payload">>, Confirmed), maps:get(<<"payload">>, Replay)),
    {ok, {{_, Rejected, _}, _, _}} = httpc:request(
        put, {Url, [], "image/png", <<"replacement">>}, [{timeout, 30000}], [{body_format, binary}]
    ),
    ?assertNotEqual(200, Rejected),
    {ok, I} = attachment_storage:session_location(Key),
    Mod = attachment_storage:driver_module(atom_to_binary(maps:get(driver, I))),
    ?assertEqual({ok, Bytes}, Mod:read_snapshot(I)).

get(S, Path, Headers) ->
    {ok, {{_, Code, _}, _, Body}} = httpc:request(
        get, {maps:get(base, S) ++ Path, Headers}, [{timeout, 30000}], [{body_format, binary}]
    ),
    {Code, jsone:decode(Body)}.

post(S, Path, Body) ->
    {ok, {{_, 200, _}, _, Resp}} = httpc:request(
        post,
        {maps:get(base, S) ++ Path, maps:get(headers, S), "application/json", Body},
        [{timeout, 30000}],
        [{body_format, binary}]
    ),
    jsone:decode(Resp).
