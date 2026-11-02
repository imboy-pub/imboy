-module(eb_asset_edge_gate).
-moduledoc "V3-17 G17 gate：EB 域 content 读链端到端（主 BEAM + edge OS 进程 + 真 Garage）。".
%%%
%%% 断言组（G17 合同）：
%%%   1) 成功旅程：member 面 content_stream 签票 → 302 → edge GET 200
%%%      逐字节一致 + 私有缓存分区；精确 winner（frozen location presign）。
%%%   2) Range 206 / HEAD 200（edge 层语义，实际回归）。
%%%   3) 重复请求幂等（同票连发两次 200）。
%%%   4) 反例：非成员 actor 票 → 404；widget 绑定门（错 conversation）→ 404。
%%%   5) 撤权即时：member 置 suspended 后同票下一请求 404（逐请求授权重放）。
%%%   6) BEAM 零正文：content_stream 视图仅 redirect_url（无 body 字节）。
-export([main/0]).

-define(EDGE_PORT, 13992).
-define(APP_PORT, 9800).
-define(TOKEN, <<"v3run1-edge-internal-token">>).
-define(GARAGE, "http://127.0.0.1:13910").

main() ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(crypto),
    {ok, _} = application:ensure_all_started(ssl),
    case application:load(imboy) of
        ok -> ok;
        {error, {already_loaded, imboy}} -> ok
    end,
    application:set_env(imboy, edge_internal_token, ?TOKEN),
    %% redirect 指向本 gate 的 edge（base_url 覆盖为 loopback edge 基址；
    %% edge_base/0 会拼 <base>/edge?ticket= 形态）
    application:set_env(imboy, base_url, <<
        "http://127.0.0.1:",
        (integer_to_binary(?EDGE_PORT))/binary
    >>),
    {ok, _} = application:ensure_all_started(imboy),
    GarageEp = os:getenv("IMBOY_GARAGE_ENDPOINT", ?GARAGE),
    %% registry：a0-root 真 Garage 凭据；enterprise 上传命名空间映射到 a0-ns
    %% （EB asset 行 digest 与 presign 同源，位置一致性成立）。
    Record = #{
        driver => <<"garage">>,
        namespace => <<"a0-ns">>,
        endpoint => list_to_binary(GarageEp),
        container => list_to_binary(os:getenv("IMBOY_GARAGE_BUCKET", "imboy")),
        access_key => list_to_binary(os:getenv("A0_AK")),
        secret_key => list_to_binary(os:getenv("A0_SK")),
        region => list_to_binary(os:getenv("IMBOY_GARAGE_REGION", "garage"))
    },
    Base = attachment_storage:registry(),
    application:set_env(imboy, attachment_storage_registry, Base#{
        roots => (maps:get(roots, Base, #{}))#{<<"a0-root">> => Record},
        upload_namespaces => (maps:get(upload_namespaces, Base, #{}))#{
            <<"enterprise">> => <<"a0-ns">>
        }
    }),
    try
        run_assertions(Record)
    after
        stop_edge()
    end.

run_assertions(Record) ->
    %% 1) 合成租户事实（org/ws/member/conversation/assignment 全套）
    Scope = eb_pg_test_fixture:new_scope(),
    Org = maps:get(org_id, Scope),
    Ws = maps:get(workspace_id, Scope),
    Conv = maps:get(conversation_id, Scope),
    Actor = maps:get(actor_user_id, Scope),
    RunTag = integer_to_binary(os:system_time(millisecond)),
    Key =
        <<"enterprise/", (integer_to_binary(Org))/binary, "/", (integer_to_binary(Ws))/binary, "/",
            RunTag/binary, ".bin">>,
    Body = <<"G17-EB-EDGE-PAYLOAD-", 0:(48 * 8)>>,
    ok = put_object(Key, Body),
    {ok, RootRec} = attachment_storage:resolve(<<"a0-ns">>),
    Locator = attachment_storage:locator_digest(<<"a0-ns">>, RootRec),
    AssetId = eb_pg_test_fixture:id(),
    {ok, 1} = elib_pg:execute(
        <<
            "INSERT INTO enterprise_asset"
            " (id, organization_id, workspace_id, conversation_id, message_id, business_identity_id,"
            "  uploaded_by_user_id, object_key, object_hash, mime, size_bytes, status, key_version,"
            "  file_name, retain_until, version, storage_driver, storage_namespace, storage_container,"
            "  storage_locator_sha256)"
            " VALUES ($1,$2,$3,$4,NULL,$5,$6,$7,$8,'application/octet-stream',$9,'active',1,'g17.bin',"
            "         to_timestamp(extract(epoch from now())/1000 + 94608000),1,'garage','a0-ns',$10,$11)"
        >>,
        [
            AssetId,
            Org,
            Ws,
            Conv,
            maps:get(sales_identity_id, Scope),
            Actor,
            Key,
            binary:encode_hex(crypto:hash(sha256, Body), lowercase),
            byte_size(Body),
            maps:get(container, Record),
            Locator
        ]
    ),
    %% 2) member 面 content_stream → 302 视图（BEAM 零正文）
    {ok, View} = enterprise_business_facade:content_stream(Org, #{
        workspace_id => Ws, asset_id => AssetId, actor_user_id => Actor
    }),
    true = is_map_key(redirect_url, View),
    false = is_map_key(body, View),
    Url = maps:get(redirect_url, View),
    io:format("redirect_view_ok: ~ts...~n", [binary:part(Url, 0, 60)]),
    %% 3) 起 edge；GET 200 逐字节一致 + 私有缓存分区
    ok = start_edge(),
    {ok, {{_, 200, _}, H1, R1}} = httpc:request(
        get, {binary_to_list(Url), []}, [{timeout, 15000}], [{body_format, binary}]
    ),
    true = (R1 =:= Body),
    true = lists:keymember("cache-control", 1, lower(H1)),
    io:format("get_200_exact_bytes: ~pB ok~n", [byte_size(R1)]),
    %% 4) Range 206 实际回归
    {ok, {{_, 206, _}, H2, R2}} = httpc:request(
        get, {binary_to_list(Url), [{"range", "bytes=0-7"}]}, [{timeout, 15000}], [
            {body_format, binary}
        ]
    ),
    8 = byte_size(R2),
    true = lists:keymember("content-range", 1, lower(H2)),
    io:format("range_206: 8B ok~n"),
    %% 5) HEAD 200 空 body
    {ok, {{_, 200, _}, _, <<>>}} = httpc:request(
        head, {binary_to_list(Url), []}, [{timeout, 15000}], [{body_format, binary}]
    ),
    io:format("head_200: ok~n"),
    %% 6) 同票重复请求幂等（两次 200）
    {ok, {{_, 200, _}, _, R3}} = httpc:request(
        get, {binary_to_list(Url), []}, [{timeout, 15000}], [{body_format, binary}]
    ),
    true = (R3 =:= Body),
    io:format("repeat_get_idempotent: ok~n"),
    %% 7) 反例：非成员 actor 票（手工签发，claims 越权 actor）→ 404
    Other = maps:get(peer_user_id, Scope),
    {ok, BadTicket} = eb_asset_download_ticket:issue(
        #{
            org_id => Org,
            workspace_id => Ws,
            asset_id => AssetId,
            actor_kind => member,
            actor_id => Other
        },
        config_ds:env(jwt_key, <<>>)
    ),
    BadUrl = edge_url(BadTicket),
    {ok, {{_, 404, _}, _, _}} = httpc:request(
        get, {binary_to_list(BadUrl), []}, [{timeout, 15000}], [{body_format, binary}]
    ),
    io:format("non_member_hidden_404: ok~n"),
    %% 8) 反例：widget 绑定门（错 conversation 声明）→ 404
    {ok, WgTicket} = eb_asset_download_ticket:issue(
        #{
            org_id => Org,
            workspace_id => Ws,
            asset_id => AssetId,
            actor_kind => contact,
            actor_id => maps:get(contact_id, Scope),
            conversation_id => eb_pg_test_fixture:id()
        },
        config_ds:env(jwt_key, <<>>)
    ),
    WgUrl = edge_url(WgTicket),
    {ok, {{_, 404, _}, _, _}} = httpc:request(
        get, {binary_to_list(WgUrl), []}, [{timeout, 15000}], [{body_format, binary}]
    ),
    io:format("binding_gate_mismatch_404: ok~n"),
    %% 9) 撤权即时：member → suspended，同票下一请求 404（逐请求授权重放）
    {ok, 1} = elib_pg:execute(
        <<"UPDATE organization_member SET status='suspended' WHERE organization_id=$1 AND user_id=$2">>,
        [Org, Actor]
    ),
    {ok, {{_, 404, _}, _, _}} = httpc:request(
        get, {binary_to_list(Url), []}, [{timeout, 15000}], [{body_format, binary}]
    ),
    io:format("revocation_immediate_404: ok~n"),
    io:format("fixture_scope: org=~p ws=~p asset=~p (synthetic)~n", [Org, Ws, AssetId]),
    io:format("EB_ASSET_EDGE_GATE_ALL_PASS~n"),
    halt(0).

edge_url(Ticket) ->
    <<"http://127.0.0.1:", (integer_to_binary(?EDGE_PORT))/binary, "/edge?ticket=", Ticket/binary>>.

lower(Hs) ->
    [{string:lowercase(K), V} || {K, V} <- Hs].

start_edge() ->
    TestLibDir = filename:dirname(code:which(eb_asset_edge_gate)),
    Backend = filename:dirname(filename:dirname(TestLibDir)),
    Deps = filename:join(Backend, "deps"),
    CodePaths = lists:join(":", [D || D <- filelib:wildcard(filename:join(Deps, "*/ebin"))]),
    Script = filename:join([Backend, "priv", "workers", "download_edge.escript"]),
    Es = os:find_executable("escript"),
    Cmd = lists:flatten(
        io_lib:format(
            "EDGE_PORT=~p EDGE_AUTHORIZE_URL=\"http://127.0.0.1:~p/api/v1/attachment/edge/authorize\" "
            "EDGE_INTERNAL_TOKEN=\"~s\" EDGE_EBIN=\"~s\" EDGE_CODE_PATHS=\"~s\" \"~s\" \"~s\" 2>>/tmp/v3-eb-edge.log &",
            [?EDGE_PORT, ?APP_PORT, ?TOKEN, filename:join(Backend, "ebin"), CodePaths, Es, Script]
        )
    ),
    os:cmd(Cmd),
    wait_ready(20).

wait_ready(0) ->
    {error, edge_not_ready};
wait_ready(N) ->
    case gen_tcp:connect("127.0.0.1", ?EDGE_PORT, [binary, {active, false}], 500) of
        {ok, S} ->
            gen_tcp:close(S),
            timer:sleep(300),
            ok;
        _ ->
            timer:sleep(500),
            wait_ready(N - 1)
    end.

stop_edge() ->
    os:cmd("pkill -f download_edge.escript || true").

put_object(Key, Body0) ->
    Cfg = #{
        endpoint => list_to_binary(os:getenv("A0_ENDPOINT", ?GARAGE)),
        container => list_to_binary(os:getenv("IMBOY_GARAGE_BUCKET", "imboy")),
        access_key => list_to_binary(os:getenv("A0_AK")),
        secret_key => list_to_binary(os:getenv("A0_SK")),
        region => list_to_binary(os:getenv("IMBOY_GARAGE_REGION", "garage"))
    },
    Ck = base64:encode(crypto:hash(sha256, Body0)),
    Url = elib_s3_sign:presign_signed_url_at(
        <<"PUT">>,
        maps:get(endpoint, Cfg),
        <<"imboy">>,
        Key,
        [{<<"content-type">>, <<"application/octet-stream">>}, {<<"x-amz-checksum-sha256">>, Ck}],
        calendar:universal_time(),
        300,
        Cfg
    ),
    {ok, {{_, 200, _}, _, _}} = httpc:request(
        put,
        {
            binary_to_list(Url),
            [
                {"Content-Type", "application/octet-stream"},
                {"x-amz-checksum-sha256", binary_to_list(Ck)}
            ],
            "application/octet-stream",
            Body0
        },
        [{timeout, 30000}, {autoredirect, false}],
        [{body_format, binary}]
    ),
    ok.
