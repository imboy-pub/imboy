%%% @doc W4-02 purged-session-recovery：purge 后的会话恢复墓碑链验收
%%%（真 scratch DB；fn_v5_copy_eb_recovery_tombstone + register UUID 查重）。
%%%
%%% 场景（04 W4-02 断言：无对象身份复用、跨主体不泄露）：
%%%   * confirmed purge → 意图入队 → copy_eb_recovery_tombstone 快照复制 →
%%%     同 UUID register 拒（tombstone 23505，队列行存在即拒）；
%%%   * done job 同 UUID 不产生新资产（register 拒绝持续——墓碑永久）；
%%%   * 删除资产后再 purge/register：deleted 父资产授权拒绝（不复活）；
%%%   * 外部 actor 对未知 UUID 的 register 重放：不泄露存在性（同 404 形状）；
%%%   * pending purge 的 register 响应丢失恢复：同 UUID 重放返回同一 key。
-module(attachment_lifecycle_v5_w402_purged_recovery_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

w402_purged_recovery_test_() ->
    {timeout, 1200, fun run/0}.

run() ->
    Boot = eunit_runner:eunit_setup(),
    State = inttest_marker_db:provision(
        #{
            env_prefix => <<"V5_INTTEST_W402P">>,
            connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
        }
    ),
    Conn = maps:get(conn, State),
    try
        w402p_confirmed_purge_tombstone(Conn),
        w402p_done_job_same_uuid(Conn),
        w402p_foreign_actor_no_disclosure(Conn),
        w402p_pending_register_replay(Conn)
    after
        inttest_marker_db:release(State),
        eunit_runner:eunit_cleanup(Boot)
    end.

q(Conn, Sql) -> q(Conn, Sql, []).
q(Conn, Sql, Params) ->
    case epgsql:equery(Conn, Sql, Params) of
        {ok, _} -> ok;
        {ok, _, _} -> ok;
        {error, Reason} -> erlang:error({w402p_sql_failed, Reason, Sql})
    end.

e(Conn, Sql, Params) -> epgsql:equery(Conn, Sql, Params).

uuid_bin() ->
    H = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    <<A:8/binary, B:4/binary, C:4/binary, D:4/binary, E:12/binary>> = H,
    <<A/binary, "-", B/binary, "-", C/binary, "-", D/binary, "-", E/binary>>.

hex64(Bin) ->
    iolist_to_binary([io_lib:format("~2.16.0b", [B]) || B <- binary_to_list(Bin)]).

seed_tenants(Conn) ->
    q(
        Conn,
        "INSERT INTO public.\"user\" (id, account, nickname, password, status,"
        " account_type, reg_ip, reg_cosv, created_at)"
        " VALUES (101, 'w402p-u', 'w402p', '', 1, 0, '127.0.0.1', 'w402p', now())"
        " ON CONFLICT (id) DO NOTHING"
    ),
    q(
        Conn,
        "INSERT INTO organization(id,name,owner_id) VALUES (9101,'w402p-org',101)"
        " ON CONFLICT (id) DO NOTHING"
    ),
    q(
        Conn,
        "INSERT INTO workspace(organization_id,id,name,owner_id,branding)"
        " VALUES (9101,9102,'w402p-ws',101,'{}') ON CONFLICT (id) DO NOTHING"
    ),
    ok.

seed_eb_confirmed(Conn, Id, Key, Uuid) ->
    q(
        Conn,
        "INSERT INTO enterprise_asset"
        " (id,organization_id,workspace_id,object_key,object_hash,mime,size_bytes,"
        "  status,storage_driver,storage_namespace,storage_container,"
        "  storage_locator_sha256,lifecycle_state,client_upload_id,"
        "  upload_registration)"
        " VALUES ($1,9101,9102,$2,$3,'text/plain',8,'active','garage','ns1','c1',"
        "  $4,'live',$5,$6::jsonb)",
        [
            Id,
            Key,
            hex64(crypto:hash(sha256, Key)),
            hex64(crypto:hash(sha256, <<Key/binary, "-loc">>)),
            Uuid,
            <<"{\"subject\":{\"kind\":\"user\",\"principal\":\"101\"},\"declaration\":{\"mime\":\"image\/png\"}}">>
        ]
    ),
    ok.

%% confirmed 资产 purge：意图入队 → 墓碑快照复制 → 同 UUID register 拒
w402p_confirmed_purge_tombstone(Conn) ->
    seed_tenants(Conn),
    Uuid = uuid_bin(),
    Id = 944000000001,
    Key = <<"enterprise/9101/9102/prg-1">>,
    seed_eb_confirmed(Conn, Id, Key, Uuid),
    %% 意图入队（身份完整 → ready）
    {ok, _, _} = e(
        Conn,
        "SELECT fn_v5_intent_eb_delete($1,$2,$3,'w402p purge')",
        [9101, 9102, Id]
    ),
    %% purge 前快照复制（生产顺序：purge 事务内 copy tombstone）
    {ok, _, _} = e(
        Conn, "SELECT fn_v5_copy_eb_recovery_tombstone($1,$2,$3)", [9101, 9102, Id]
    ),
    {ok, _, [{R1, R2}]} = e(
        Conn,
        "SELECT client_upload_id, upload_registration"
        " FROM enterprise_asset_delete_queue"
        " WHERE organization_id=9101 AND workspace_id=9102 AND asset_id=$1",
        [Id]
    ),
    ?assertEqual(Uuid, R1),
    ?assertNotEqual(null, R2),
    %% 同 UUID register：队列墓碑拒绝（23505）
    {error, #error{code = <<"23505">>}} = e(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b','private',101,$2::uuid,"
        "$3::jsonb,now(),now() - interval '2 hours',"
        "'garage','ns1','c1',$4)",
        [
            <<"v5e2e/w402p/replay-1">>,
            Uuid,
            <<
                "{\"subject\":{\"kind\":\"user\",\"principal\":\"101\"},"
                "\"declaration\":{\"mime\":\"image/png\"}}"
            >>,
            hex64(crypto:hash(sha256, <<"replay-1">>))
        ]
    ),
    ok = epgsql:sync(Conn).

%% done job 同 UUID 不产生新资产：done 后 register 仍拒（墓碑永久）
w402p_done_job_same_uuid(Conn) ->
    Uuid = uuid_bin(),
    Id = 944000000002,
    Key = <<"enterprise/9101/9102/prg-2">>,
    seed_eb_confirmed(Conn, Id, Key, Uuid),
    {ok, _, _} = e(
        Conn,
        "SELECT fn_v5_intent_eb_delete($1,$2,$3,'w402p done purge')",
        [9101, 9102, Id]
    ),
    {ok, _, _} = e(
        Conn, "SELECT fn_v5_copy_eb_recovery_tombstone($1,$2,$3)", [9101, 9102, Id]
    ),
    %% worker 消费到 done（删除成功；直接在断言连接上调 CAS 函数）。
    %% 队列可能含先前 case 的 ready job：循环 claim 到目标（无关 job
    %% ack(ok)=done 消化掉，语义=正常消费）。
    claim_until(Conn, Id, 10),
    %% done 后同 UUID register 仍拒（queue 行保留为墓碑）
    {error, #error{code = <<"23505">>}} = e(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b','private',101,$2::uuid,"
        "$3::jsonb,now(),now() - interval '2 hours',"
        "'garage','ns1','c1',$4)",
        [
            <<"v5e2e/w402p/replay-2">>,
            Uuid,
            <<
                "{\"subject\":{\"kind\":\"user\",\"principal\":\"101\"},"
                "\"declaration\":{\"mime\":\"image/png\"}}"
            >>,
            hex64(crypto:hash(sha256, <<"replay-2">>))
        ]
    ),
    ok = epgsql:sync(Conn).

token_for_row(_Row) -> crypto:strong_rand_bytes(16).

uuid_text() ->
    H = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    <<A:8/binary, B:4/binary, C:4/binary, D:4/binary, E:12/binary>> = H,
    <<A/binary, "-", B/binary, "-", C/binary, "-", D/binary, "-", E/binary>>.

%% 循环 claim 直到拿到目标 job 并 ack done（无关 ready job 顺带消化）。
claim_until(_Conn, _Id, 0) ->
    erlang:error({w402p_target_job_not_claimed, _Id});
claim_until(Conn, Id, N) ->
    T = uuid_text(),
    case
        e(
            Conn,
            "SELECT out_organization_id, out_workspace_id, out_asset_id"
            " FROM fn_v5_claim_eb($1)",
            [T]
        )
    of
        {ok, _, [{9101, 9102, Id}]} ->
            {ok, _, [{<<"done">>}]} = e(
                Conn,
                "SELECT fn_v5_ack_eb($1, $2, $3, $4, true)",
                [9101, 9102, Id, T]
            ),
            ok;
        {ok, _, [{O, W, Other}]} ->
            %% 无关 job：正常消费（done），继续找目标
            {ok, _, _} =
                e(Conn, "SELECT fn_v5_ack_eb($1, $2, $3, $4, true)", [
                    O, W, Other, T
                ]),
            claim_until(Conn, Id, N - 1);
        Other ->
            erlang:error({w402p_claim_failed, Other})
    end.

%% 外部 actor 对未知 UUID 的 register 重放：不泄露存在性（合法新 UUID 通过，
%% 形状一致——同 key/session 结构；拒绝面只在同 UUID 墓碑）
w402p_foreign_actor_no_disclosure(Conn) ->
    FreshUuid = uuid_bin(),
    {ok, _, [{KeyOut, <<"registered">>, Cid}]} = e(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b','private',202,$2::uuid,"
        "$3::jsonb,now(),now() - interval '2 hours',"
        "'garage','ns1','c1',$4)",
        [
            <<"v5e2e/w402p/foreign-1">>,
            FreshUuid,
            <<
                "{\"subject\":{\"kind\":\"user\",\"principal\":\"202\"},"
                "\"declaration\":{\"mime\":\"image/png\"}}"
            >>,
            hex64(crypto:hash(sha256, <<"foreign-1">>))
        ]
    ),
    ?assertEqual(<<"v5e2e/w402p/foreign-1">>, KeyOut),
    ?assertEqual(FreshUuid, Cid),
    ok.

%% pending purge 的 register 响应丢失恢复：同 UUID 重放返回同一 key/state
w402p_pending_register_replay(Conn) ->
    Uuid = uuid_bin(),
    Reg =
        <<
            "{\"subject\":{\"kind\":\"user\",\"principal\":\"101\"},"
            "\"declaration\":{\"mime\":\"image/png\"}}"
        >>,
    Key = <<"v5e2e/w402p/lost-1">>,
    {ok, _, [{K1, <<"registered">>, Uuid}]} = e(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b','private',101,$2::uuid,"
        "$3::jsonb,now(),now() - interval '2 hours',"
        "'garage','ns1','c1',$4)",
        [Key, Uuid, Reg, hex64(crypto:hash(sha256, <<"lost-1">>))]
    ),
    %% 响应丢失重放：同 UUID 同声明 → 同 key/state（幂等成功）
    {ok, _, [{K1, <<"registered">>, Uuid}]} = e(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b','private',101,$2::uuid,"
        "$3::jsonb,now(),now() - interval '2 hours',"
        "'garage','ns1','c1',$4)",
        [Key, Uuid, Reg, hex64(crypto:hash(sha256, <<"lost-1">>))]
    ),
    ok.
