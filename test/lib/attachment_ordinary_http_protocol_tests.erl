%%% Real middleware, HTTP handlers, PostgreSQL and storage; no application boot.
-module(attachment_ordinary_http_protocol_tests).
-include_lib("eunit/include/eunit.hrl").

http_protocol_test_() ->
    %% 受控让位（仓库先例：attachment_lifecycle_v5_pg_tests r20_test_/
    %% v5_gate_pinned、w403_gateway_protocol_tests protocol_test_：eunit 无
    %% {skip, ...} 顶层描述符，生成器返回空清单才是等价的静默让位机制）。
    %% 本套件 setup 直调 attachment_lifecycle_v5_w403_gateway_protocol_
    %% tests:setup/0（required("W403_RESOURCE_MANIFEST") fail-closed），是
    %% r12 中该模块 required/1 抛错的三条外部调用链之一；nightly 全量裸跑
    %% （make eunit-local-legacy）不经 gate，缺钉名 env 时在生成器入口
    %% 让位，不进 setup。gate 路径（env 在场）语义与断言零删减。
    case os:getenv("W403_RESOURCE_MANIFEST") of
        V when is_list(V), V =/= "" ->
            http_protocol_units();
        _ ->
            io:format(
                user, "W403_ORDHTTP_EUNIT_YIELD missing_env=\"W403_RESOURCE_MANIFEST\"~n", []
            ),
            []
    end.

http_protocol_units() ->
    {setup, fun setup/0, fun cleanup/1, fun(S) ->
        [
            {atom_to_list(D), {timeout, 120, fun() -> roundtrip(S, D) end}}
         || %% V3-20：disk 驱动退役，disk 遍随驱动删除，garage 侧保留。
            D <- [garage]
        ]
    end}.

setup() ->
    S = attachment_w403_owned_fixture:setup(),
    erlang:put(w403_ordinary_setup, S),
    try
        Owned = setup_owned(S),
        erlang:erase(w403_ordinary_setup),
        Owned
    catch
        Class:Reason:Stack ->
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
            %% C10 S3-only：「确认后不得换体」的会话门在生产写入通道
            %% multipart 端点（session_context 无 confirmed 子句 →
            %% session_not_renewable → 409），与生产 imboy_router 同款注册。
            {"/api/v1/attachment/upload", attach_handler, #{action => upload}}
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
            catch
                _:_ -> ok
            end;
        _ ->
            ok
    end,
    attachment_w403_owned_fixture:cleanup(S).

roundtrip(S, D) ->
    %% V3-20：disk 驱动退役，上传命名空间仅剩 garage 根。
    Ns =
        case D of
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
    ?assertEqual([{Key, <<"registered">>}], pending_sessions(Cid)),
    {200, Repeated} = get(S, Path, maps:get(headers, S)),
    ?assertEqual(Key, maps:get(<<"object_key">>, maps:get(<<"payload">>, Repeated))),
    ?assertEqual([{Key, <<"registered">>}], pending_sessions(Cid)),
    OriginalSnapshot = pending_snapshot(Cid),
    ConflictPath =
        "/api/v1/attachment/presign?filename=different.png&mime_type=image%2Fpng&client_upload_id=" ++
            binary_to_list(Cid),
    {200, Conflict} = get(S, ConflictPath, maps:get(headers, S)),
    ?assertEqual(503, maps:get(<<"code">>, Conflict)),
    ?assertEqual(<<"登记未完成，请使用同一 client_upload_id 重试"/utf8>>, maps:get(<<"msg">>, Conflict)),
    ?assertEqual(#{}, maps:get(<<"payload">>, Conflict)),
    ?assertEqual([{Key, <<"registered">>}], pending_sessions(Cid)),
    ?assertEqual(OriginalSnapshot, pending_snapshot(Cid)),
    {200, Recovery} = get(S, Path, maps:get(headers, S)),
    ?assertEqual(0, maps:get(<<"code">>, Recovery)),
    ?assertEqual(Key, maps:get(<<"object_key">>, maps:get(<<"payload">>, Recovery))),
    ?assertEqual([{Key, <<"registered">>}], pending_sessions(Cid)),
    ?assertEqual(OriginalSnapshot, pending_snapshot(Cid)),
    Url0 = maps:get(<<"put_url">>, Data),
    Url =
        case Url0 of
            <<"/", _/binary>> -> maps:get(base, S) ++ binary_to_list(Url0);
            _ -> binary_to_list(Url0)
        end,
    %% C10：gateway 正文通道物理退役——合成 multipart/binary 请求落通用路由
    %% 层 404（不读正文、不给升级消息）；新附件链路以下方读回继续证明。
    Gateway = maps:get(base, S) ++ "/api/v1/attachment/gateway/" ++ binary_to_list(Cid),
    {ok, {{_, 404, _}, _, _}} = httpc:request(
        put, {Gateway, [], "image/png", <<"unauthorized">>}, [{timeout, 30000}], []
    ),
    {ok, {{_, 404, _}, _, _}} = httpc:request(
        put,
        {Gateway ++ "?capability=invalid", [], "image/png", <<"unauthorized">>},
        [{timeout, 30000}],
        []
    ),
    {ok, {{_, 404, _}, _, _}} = httpc:request(get, {Gateway, []}, [{timeout, 30000}], []),
    {ok, I0} = attachment_storage:session_location(Key),
    Mod0 = attachment_storage:driver_module(atom_to_binary(maps:get(driver, I0))),
    ?assertEqual({error, not_found}, Mod0:read_snapshot(I0)),
    Bytes = <<137, 80, 78, 71, 13, 10, 26, 10, "synthetic immutable body">>,
    {ok, {{_, 200, _}, _, _}} = httpc:request(
        put, {Url, [], "image/png", Bytes}, [{timeout, 30000}], [{body_format, binary}]
    ),
    ?assertEqual({ok, Bytes}, Mod0:read_snapshot(I0)),
    Body = jsone:encode(#{<<"object_key">> => Key, <<"scope">> => <<"private">>}),
    %% V5 pin 门（02 §14，W2-02）：S3 直传只落物理字节，upload_content 未
    %% 固定不得确认——先断言缺 pin 的 fail-closed 拒绝（400 契约文案），
    %% 再按 attachment_upload_logic:pin_and_put 的 receipt 形状补 pin（与
    %% attachment_lifecycle_v5_confirm_pg_tests seed_session 同惯例），
    %% confirm 放行。
    Pinless = post(S, "/api/v1/attachment/confirm", Body),
    ?assertEqual(400, maps:get(<<"code">>, Pinless)),
    ?assertEqual(<<"上传内容未固定，请先完成网关上传"/utf8>>, maps:get(<<"msg">>, Pinless)),
    ok = attach_pending_repo:register_content_pin(Key, #{
        <<"version">> => 1,
        <<"sha256">> => binary:encode_hex(crypto:hash(sha256, Bytes), lowercase),
        <<"size_bytes">> => integer_to_binary(byte_size(Bytes)),
        <<"mime">> => <<"image/png">>
    }),
    Confirmed = post(S, "/api/v1/attachment/confirm", Body),
    case maps:get(<<"code">>, Confirmed) of
        0 -> ok;
        Error -> erlang:error({confirm_rejected, Error, maps:get(<<"msg">>, Confirmed)})
    end,
    Replay = post(S, "/api/v1/attachment/confirm", Body),
    ?assertEqual(maps:get(<<"payload">>, Confirmed), maps:get(<<"payload">>, Replay)),
    {200, AfterConfirm} = get(S, Path, maps:get(headers, S)),
    ?assertEqual(0, maps:get(<<"code">>, AfterConfirm)),
    ConfirmedSession = maps:get(<<"payload">>, AfterConfirm),
    ?assertEqual(<<"confirmed">>, maps:get(<<"session_state">>, ConfirmedSession)),
    ?assertNot(maps:is_key(<<"put_url">>, ConfirmedSession)),
    ?assertEqual([{Key, <<"confirmed">>}], pending_sessions(Cid)),
    %% C10 S3-only：presigned put_url 是 bearer 存储直传（签名过期前技术上
    %% 仍可用，属 Garage 存储合同，无应用层会话门）；「确认后不得换体」
    %% 的会话门在生产写入通道 multipart 端点：confirmed 会话
    %% attachment_upload_logic:session_context 无子句 → session_not_renewable
    %% → 409（attach_handler upload_error 与 confirm 同口径）。
    Boundary = <<"ordinaryrepl7f3a">>,
    ReplacementBody =
        <<
            "--",
            (Boundary)/binary,
            "\r\n",
            "Content-Disposition: form-data; name=\"file\"; filename=\"replacement.png\"\r\n",
            "Content-Type: image/png\r\n\r\n",
            <<"replacement">>/binary,
            "\r\n--",
            (Boundary)/binary,
            "--\r\n"
        >>,
    UploadQ =
        uri_string:compose_query([
            {<<"object_key">>, Key}, {<<"mime_type">>, <<"image/png">>}
        ]),
    {ok, {{_, 409, _}, _, ReplacementResp}} = httpc:request(
        post,
        {
            maps:get(base, S) ++ "/api/v1/attachment/upload?" ++ binary_to_list(UploadQ),
            maps:get(headers, S),
            "multipart/form-data; boundary=" ++ binary_to_list(Boundary),
            ReplacementBody
        },
        [{timeout, 30000}],
        [{body_format, binary}]
    ),
    Replacement = jsone:decode(ReplacementResp),
    ?assertEqual(409, maps:get(<<"code">>, Replacement)),
    ?assertEqual(<<"session_not_renewable">>, maps:get(<<"msg">>, Replacement)),
    {ok, I} = attachment_storage:session_location(Key),
    Mod = attachment_storage:driver_module(atom_to_binary(maps:get(driver, I))),
    ?assertEqual({ok, Bytes}, Mod:read_snapshot(I)).

pending_sessions(Cid) ->
    {ok, Rows} = elib_pg:query(
        <<"SELECT object_key, session_state FROM attach_pending WHERE client_upload_id=$1::uuid">>,
        [Cid]
    ),
    [{maps:get(<<"object_key">>, Row), maps:get(<<"session_state">>, Row)} || Row <- Rows].

pending_snapshot(Cid) ->
    {ok, [Row]} = elib_pg:query(
        <<"SELECT upload_registration::text AS snapshot FROM attach_pending WHERE client_upload_id=$1::uuid">>,
        [Cid]
    ),
    maps:get(<<"snapshot">>, Row).

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
