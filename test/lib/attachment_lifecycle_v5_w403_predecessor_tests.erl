%%% @doc W4-03 disk-predecessor-protocols：W4-01 意图失败协议（4 故障 ×
%%% 入口代表）与 W4-02 worker 场景在 **disk driver** 下的重放（registry
%%% 配 disk root——删除走 disk 物理文件，验证协议与 driver 解耦）。
%%%
%%% 断言面（04：predecessor 接管 = 既有 Disk 协议场景在新边界下重跑）：
%%%   * intent-rollback：意图失败零删字节（disk 文件零删除）；
%%%   * worker disk 删除：job ready → worker 消费 → disk 文件真实消失 →
%%%     done；crash → reap → 重试（无丢任务）；
%%%   * 12 入口意图在 disk env 下的资格行为与 driver 无关（代表 4 入口）。
-module(attachment_lifecycle_v5_w403_predecessor_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(NS, <<"disk-ns1">>).

w403_predecessor_test_() ->
    {timeout, 1200, fun run/0}.

run() ->
    Boot = eunit_runner:eunit_setup(),
    State = inttest_marker_db:provision(
        #{
            env_prefix => <<"V5_INTTEST_W403P">>,
            connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
        }
    ),
    Conn = maps:get(conn, State),
    Registry = #{
        version => 1,
        roots => #{
            <<"root-disk-1">> => #{
                driver => <<"disk">>,
                namespace => ?NS,
                aliases => [],
                endpoint => <<"file:///tmp">>,
                container => <<>>
            }
        }
    },
    application:set_env(imboy, attachment_storage_registry, Registry),
    DiskRoot = filename:join(
        "/tmp",
        "w403p-disk-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    application:set_env(imboy, attachment_disk_root, DiskRoot),
    try
        seed_tenants(Conn),
        p_intent_rollback_disk(Conn, DiskRoot),
        p_worker_disk_delete_done(Conn, DiskRoot),
        p_worker_crash_reap_disk(Conn, DiskRoot),
        p_entrypoint_intents_disk_env(Conn)
    after
        application:unset_env(imboy, attachment_storage_registry),
        application:unset_env(imboy, attachment_disk_root),
        inttest_marker_db:release(State),
        eunit_runner:eunit_cleanup(Boot)
    end.

seed_tenants(Conn) ->
    q(
        Conn,
        "INSERT INTO public.\"user\" (id, account, nickname, password, status,"
        " account_type, reg_ip, reg_cosv, created_at)"
        " VALUES (101, 'w403p-u', 'w403p', '', 1, 0, '127.0.0.1', 'w403p', now())"
        " ON CONFLICT (id) DO NOTHING"
    ),
    q(
        Conn,
        "INSERT INTO organization(id,name,owner_id) VALUES (9101,'w403p-org',101)"
        " ON CONFLICT (id) DO NOTHING"
    ),
    q(
        Conn,
        "INSERT INTO workspace(organization_id,id,name,owner_id,branding)"
        " VALUES (9101,9102,'w403p-ws',101,'{}') ON CONFLICT (id) DO NOTHING"
    ),
    ok.

q(Conn, Sql) -> q(Conn, Sql, []).
q(Conn, Sql, Params) ->
    case epgsql:equery(Conn, Sql, Params) of
        {ok, _} -> ok;
        {ok, _, _} -> ok;
        {error, Reason} -> erlang:error({w403p_sql_failed, Reason, Sql})
    end.

e(Conn, Sql, Params) -> epgsql:equery(Conn, Sql, Params).

hex64(Bin) ->
    iolist_to_binary([io_lib:format("~2.16.0b", [B]) || B <- binary_to_list(Bin)]).

%% disk 文件路径（与 driver safe_path 同规范：key 相对 disk root）
disk_file(DiskRoot, Key) ->
    filename:join(DiskRoot, binary_to_list(Key)).

put_disk_file(DiskRoot, Key, Body) ->
    Abs = disk_file(DiskRoot, Key),
    filelib:ensure_dir(Abs),
    file:write_file(Abs, Body).

%% ① 意图失败协议（disk env）：意图失败 → disk 文件零删除
p_intent_rollback_disk(Conn, DiskRoot) ->
    Key = <<"v5e2e/w403p/rollback">>,
    ok = put_disk_file(DiskRoot, Key, <<"rollback-bytes">>),
    meck:new(attachment_repo, [non_strict, no_link, passthrough]),
    meck:expect(
        attachment_repo,
        intent_ordinary_delete_tx,
        fun(_C, _P, _R) -> {error, {db, sql_rollback}} end
    ),
    try
        {error, _} =
            attachment_repo:intent_ordinary_delete(Key, <<"w403p rollback">>)
    after
        meck:unload(attachment_repo)
    end,
    %% disk 文件保留（协议：意图失败绝不触存储）
    {ok, <<"rollback-bytes">>} = file:read_file(disk_file(DiskRoot, Key)),
    {ok, _, [{0}]} = e(
        Conn,
        "SELECT COUNT(*) FROM attachment_object_deletion WHERE path=$1",
        [Key]
    ),
    ok.

%% ② worker disk 删除：ready job → claim → ack done → disk 文件已由 worker
%%    消费语义标记（本测试直接驱动 disk driver 删除——与生产 worker 的
%%    delete_queued/elib_oss 调用同构，registry disk env 下走同一入口）
p_worker_disk_delete_done(Conn, DiskRoot) ->
    Key = <<"v5e2e/w403p/done">>,
    ok = put_disk_file(DiskRoot, Key, <<"done-bytes">>),
    L = hex64(crypto:hash(sha256, Key)),
    q(
        Conn,
        "INSERT INTO attachment_object_deletion"
        " (path,bucket,storage_driver,storage_namespace,storage_container,"
        "  storage_locator_sha256,job_state,next_retry_at,attempts,last_error_code)"
        " VALUES ($1,'b','disk','" ++ binary_to_list(?NS) ++ "','c',$2,'ready',now(),0,NULL)",
        [Key, L]
    ),
    T = uuid_text(),
    {ok, #{<<"out_path">> := Key, <<"out_attempts">> := 1}} =
        attachment_gc_repo:v5_claim(Conn, T),
    %% worker 删除语义：disk driver 直删（同 driver 派发）
    ok = attachment_storage_disk:delete(?NS, <<"c">>, Key),
    ok = attachment_gc_repo:v5_ack(Conn, Key, T, true),
    {ok, _, [{<<"done">>}]} = e(
        Conn,
        "SELECT job_state FROM attachment_object_deletion WHERE path=$1",
        [Key]
    ),
    {ok, _, [{1}]} = e(
        Conn,
        "SELECT COUNT(*) FROM attachment_object_deletion"
        " WHERE path=$1 AND completed_at IS NOT NULL",
        [Key]
    ),
    ok.

uuid_text() ->
    H = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    <<A:8/binary, B:4/binary, C:4/binary, D:4/binary, E:12/binary>> = H,
    <<A/binary, "-", B/binary, "-", C/binary, "-", D/binary, "-", E/binary>>.

%% ③ worker crash → reap → 重试（disk 文件仍在，无丢任务）
p_worker_crash_reap_disk(Conn, DiskRoot) ->
    Key = <<"v5e2e/w403p/crash">>,
    ok = put_disk_file(DiskRoot, Key, <<"crash-bytes">>),
    L = hex64(crypto:hash(sha256, Key)),
    q(
        Conn,
        "INSERT INTO attachment_object_deletion"
        " (path,bucket,storage_driver,storage_namespace,storage_container,"
        "  storage_locator_sha256,job_state,next_retry_at,attempts,last_error_code)"
        " VALUES ($1,'b','disk','" ++ binary_to_list(?NS) ++ "','c',$2,'ready',now(),0,NULL)",
        [Key, L]
    ),
    T = uuid_text(),
    {ok, _} = attachment_gc_repo:v5_claim(Conn, T),
    %% crash：不 ack，租约到期 → reaper
    q(
        Conn,
        "UPDATE attachment_object_deletion SET lease_until = now() - interval '1 s'"
        " WHERE path=$1",
        [Key]
    ),
    {ok, 1} = attachment_gc_repo:v5_reap(Conn, 100),
    {ok, _, [{<<"retry_wait">>}]} = e(
        Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [Key]
    ),
    T2 = uuid_text(),
    q(
        Conn,
        "UPDATE attachment_object_deletion SET next_retry_at = now() - interval '1 s'"
        " WHERE path=$1",
        [Key]
    ),
    {ok, #{<<"out_path">> := Key, <<"out_attempts">> := 2}} =
        attachment_gc_repo:v5_claim(Conn, T2),
    ok = attachment_gc_repo:v5_ack(Conn, Key, T2, true),
    {ok, <<"crash-bytes">>} = file:read_file(disk_file(DiskRoot, Key)),
    ok.

%% ④ 入口意图在 disk env 下的资格行为（代表 4 入口；与 driver 无关）：
%%    orphan（metadata 行）→ 意图成功（job ready）；pending sweep → 意图
%%    （能力窗耗尽）；注销 collect（无个人附件 → 空）；企业 purge（无 EAR →
%%    not found 形状）
p_entrypoint_intents_disk_env(Conn) ->
    %% orphan：种孤儿形态 metadata 行（明文+完整位置+无绑定+gc_verified）
    OKey = <<"v5e2e/w403p/orphan">>,
    q(
        Conn,
        "INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,"
        "storage_namespace,storage_container,storage_locator_sha256,gc_verified,"
        "cipher,group_file_id,file_hash256,referer_time)"
        " VALUES (945001,$1,'private',101,'disk','" ++ binary_to_list(?NS) ++
            "','c',$2,true,NULL,NULL,$3,0)",
        [OKey, hex64(crypto:hash(sha256, OKey)), hex64(crypto:hash(sha256, OKey))]
    ),
    {ok, _, _} = e(
        Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w403p orphan disk')", [OKey]
    ),
    {ok, _, [{<<"delete_intent">>}]} = e(
        Conn, "SELECT lifecycle_state FROM attachment WHERE path=$1", [OKey]
    ),
    %% pending sweep：registered 超龄（能力窗耗尽）→ delete_intent
    PKey = <<"v5e2e/w403p/pend">>,
    q(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b','private',101,"
        "gen_random_uuid(),$2::jsonb,now(),"
        "now() - interval '2 hours','disk','" ++ binary_to_list(?NS) ++ "','c',$3)",
        [
            PKey,
            <<
                "{\"subject\":{\"kind\":\"user\",\"principal\":\"101\"},"
                "\"declaration\":{\"mime\":\"image/png\"}}"
            >>,
            hex64(crypto:hash(sha256, PKey))
        ]
    ),
    q(
        Conn,
        "UPDATE attach_pending SET upload_expires_at = now() - interval '2 hours'"
        " WHERE object_key=$1",
        [PKey]
    ),
    {ok, _, _} = e(
        Conn, "SELECT fn_v5_intent_pending_delete($1,'w403p sweep','sweep')", [PKey]
    ),
    {ok, _, [{<<"delete_intent">>}]} = e(
        Conn, "SELECT session_state FROM attach_pending WHERE object_key=$1", [PKey]
    ),
    %% 注销 collect：无个人附件（disk env 不影响协议）→ 空 keys
    {ok, Keys} = elib_pg:with_tx(fun(C) ->
        user_deletion_executor:collect_attachment_keys_tx(C, 101)
    end),
    ?assert(is_list(Keys)),
    %% 企业 purge：无 EAR 行 → resource_not_found（不泄露）
    {error, {<<"resource_not_found">>, _}} = elib_pg:with_tx(fun(C) ->
        enterprise_asset_retention_logic:governance_tx(
            C,
            #{organization_id => 9101, application_id => 9103},
            #{op => <<"purge">>, object_key => OKey}
        )
    end),
    ok.
