-module(attachment_edge_gate).
-moduledoc "V3-08 下载边缘 gate：主 BEAM(app) + 边缘 OS 进程 + 真 Garage 端到端断言".
-export([main/0]).

-define(EDGE_PORT, 13991).
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
    {ok, _} = application:ensure_all_started(imboy),
    %% V5 存储 registry：roots 指向本 run 一次性 Garage（与 a0_probe 同构）
    GarageEp = os:getenv("IMBOY_GARAGE_ENDPOINT", "http://127.0.0.1:13910"),
    application:set_env(imboy, attachment_storage_registry, #{
        version => 1,
        roots => #{
            <<"a0-root">> => #{
                driver => <<"garage">>,
                namespace => <<"a0-ns">>,
                endpoint => list_to_binary(GarageEp),
                container => <<"imboy">>
            }
        },
        upload_namespaces => #{}
    }),
    try
        run_assertions()
    after
        stop_edge()
    end.

run_assertions() ->
    %% 1) 对象入真 Garage
    RunTag = integer_to_binary(os:system_time(millisecond)),
    Key = <<"v3run1/edge-e2e-", RunTag/binary, ".bin">>,
    Body = <<"EDGE-E2E-PAYLOAD-", 0:(48 * 8)>>,
    ok = put_object(Key, Body),
    %% 2) PG public attachment 行。V5 收口：attachment 元数据删除必须走
    %% committed delete_intent（fn_v5_metadata_delete_guard），fixture 行
    %% 采用每轮唯一 id（991000000 合成段）；残留登记资源账由 run 收口
    %% 统一按 V5 流程清理——绝不绕过守卫强删。
    Uid = 990935,
    Aid = 991000000 + os:system_time(millisecond) rem 900000,
    %% locator 摘要按 registry 真实值预计算（V5 位置不可变守卫：一次成型）
    {ok, RootRec0} = attachment_storage:resolve(<<"a0-ns">>),
    Locator = attachment_storage:locator_digest(<<"a0-ns">>, RootRec0),
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO attachment (id, file_hash256, mime_type, ext, name, path, url, size, info,"
            " referer_time, creator_user_id, scope, scope_ref, cipher, status, updated_at, created_at,"
            " storage_driver, storage_namespace, storage_container, storage_locator_sha256,"
            " storage_physical_key)"
            " VALUES ($1,'edgehash','application/octet-stream','.bin','edge.bin',$2,$2,$3,'{}',1,$4,"
            " 'public',null,null,1,NOW(),NOW(),'garage','a0-ns','imboy',$5,$2)"
        >>,
        [Aid, Key, byte_size(Body), Uid, Locator]
    ),
    %% 3) ticket
    Secret = config_ds:env(jwt_key, <<>>),
    {ok, Ticket} = attachment_download_ticket:issue(Uid, Key, Secret),
    %% 4) 起边缘
    ok = start_edge(),
    Base =
        "http://127.0.0.1:" ++ integer_to_list(?EDGE_PORT) ++ "/edge?ticket=" ++
            binary_to_list(Ticket),
    %% 诊断：直调下载链（保留为可观测输出）
    io:format("download_diag: ~p~n", [attach_logic:download(Ticket)]),
    io:format("find_diag: ~p~n", [attachment_ds:find_by_path(Key)]),
    io:format("secret_diag: empty=~p~n", [Secret =:= <<>>]),
    io:format("verify_diag: ~p~n", [attachment_download_ticket:verify(Ticket, Secret)]),
    {ok, RecD} = attachment_ds:find_by_path(Key),
    io:format("frozen_diag: ~p~n", [attachment_storage:frozen_location(Key, RecD)]),
    io:format("digest_row: ~p~n", [maps:get(<<"storage_locator_sha256">>, RecD)]),
    io:format("digest_now: ~p~n", [
        attachment_storage:locator_digest(
            <<"a0-ns">>, element(2, attachment_storage:resolve(<<"a0-ns">>))
        )
    ]),
    %% 5) 断言组
    %% 5a. GET 200 内容逐字节一致 + 安全头 + 无 origin 泄漏
    {ok, {{_, 200, _}, H1, R1}} = httpc:request(get, {Base, []}, [{timeout, 15000}], [
        {body_format, binary}
    ]),
    Body = R1,
    true = lists:member({"cache-control", "private, no-store"}, [
        string:lowercase(K) =:= "cache-control" andalso {string:lowercase(K), V}
     || {K, V} <- H1
    ]),
    no_origin_leak(H1),
    io:format("get_200: ~pB ok~n", [byte_size(R1)]),
    %% 5b. Range 206
    {ok, {{_, 206, _}, H2, R2}} = httpc:request(
        get, {Base, [{"range", "bytes=0-7"}]}, [{timeout, 15000}], [{body_format, binary}]
    ),
    8 = byte_size(R2),
    true = lists:keymember("content-range", 1, lower(H2)),
    io:format("range_206: 8B ok~n"),
    %% 5c. HEAD 200 空 body
    {ok, {{_, 200, _}, _, <<>>}} = httpc:request(head, {Base, []}, [{timeout, 15000}], [
        {body_format, binary}
    ]),
    io:format("head_200: ok~n"),
    %% 5d. 坏 ticket → 统一 404（隐藏）
    Bad = Base ++ "INVALID",
    {ok, {{_, 404, _}, _, _}} = httpc:request(get, {Bad, []}, [{timeout, 15000}], [
        {body_format, binary}
    ]),
    io:format("bad_ticket_hidden_404: ok~n"),
    %% 5e. 越权（private 行 + 他人）→ 404
    PrivKey = <<"v3run1/edge-private-", RunTag/binary, ".bin">>,
    ok = put_object(PrivKey, <<"PRIVATE-DATA">>),
    LocatorP = Locator,
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO attachment (id, file_hash256, mime_type, ext, name, path, url, size, info,"
            " referer_time, creator_user_id, scope, scope_ref, cipher, status, updated_at, created_at,"
            " storage_driver, storage_namespace, storage_container, storage_locator_sha256,"
            " storage_physical_key)"
            " VALUES ($1,'ph','application/octet-stream','.bin','p.bin',$2,$2,12,'{}',1,88888888,"
            " 'private',null,null,1,NOW(),NOW(),'garage','a0-ns','imboy',$3,$2)"
        >>,
        [Aid + 1, PrivKey, LocatorP]
    ),
    OtherUid = 990937,
    {ok, PrivTicket} = attachment_download_ticket:issue(OtherUid, PrivKey, Secret),
    PrivBase =
        "http://127.0.0.1:" ++ integer_to_list(?EDGE_PORT) ++ "/edge?ticket=" ++
            binary_to_list(PrivTicket),
    {ok, {{_, 404, _}, _, _}} = httpc:request(get, {PrivBase, []}, [{timeout, 15000}], [
        {body_format, binary}
    ]),
    io:format("cross_subject_hidden_404: ok~n"),
    %% 5f. 撤权即时生效：置 status=-1（撤下；find_by_path 的 status>=0
    %% 过滤即拒）。scope/locator 等身份字段受 fn_v5 identity_immutable
    %% 守卫，status 是生命周期字段可迁移。
    {ok, _} = elib_pg:query(
        <<"UPDATE attachment SET status=-1 WHERE id=$1">>, [Aid]
    ),
    {ok, {{_, 404, _}, _, _}} = httpc:request(get, {Base, []}, [{timeout, 15000}], [
        {body_format, binary}
    ]),
    io:format("revocation_immediate_404: ok~n"),
    io:format("fixture_rows_left(v5_guard): ids=~p,~p (991000000 synthetic band)~n", [Aid, Aid + 1]),
    io:format("EDGE_GATE_ALL_PASS~n"),
    halt(0).

lower(Hs) -> [{string:lowercase(K), V} || {K, V} <- Hs].

no_origin_leak(Hs) ->
    Lowered = lower(Hs),
    Leaks = [
        X
     || X = {K, _} <- Lowered, K =:= "location" orelse string:prefix(K, "x-amz-") =/= nomatch
    ],
    case Leaks of
        [] -> ok;
        _ -> erlang:error({origin_leak, Leaks})
    end.

start_edge() ->
    TestLibDir = filename:dirname(code:which(attachment_edge_gate)),
    Backend = filename:dirname(filename:dirname(TestLibDir)),
    Deps = filename:join(Backend, "deps"),
    CodePaths = lists:join(":", [D || D <- filelib:wildcard(filename:join(Deps, "*/ebin"))]),
    Script = filename:join([Backend, "priv", "workers", "download_edge.escript"]),
    Es = os:find_executable("escript"),
    Cmd = lists:flatten(
        io_lib:format(
            "EDGE_PORT=~p EDGE_AUTHORIZE_URL=\"http://127.0.0.1:~p/api/v1/attachment/edge/authorize\" "
            "EDGE_INTERNAL_TOKEN=\"~s\" EDGE_EBIN=\"~s\" EDGE_CODE_PATHS=\"~s\" \"~s\" \"~s\" 2>>/tmp/v3-edge.log &",
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
        container => <<"imboy">>,
        access_key => list_to_binary(os:getenv("A0_AK")),
        secret_key => list_to_binary(os:getenv("A0_SK")),
        region => <<"garage">>
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
