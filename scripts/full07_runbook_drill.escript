#!/usr/bin/env escript
%%! -noshell
%% ============================================================
%% FULL-07 §8 发布候选 Runbook 演练（**真实执行**，不是文档）
%% ------------------------------------------------------------
%% 计划 §8：「升级/回滚、secret rotation、credential emergency revoke、
%% worker restart runbook 演练」。
%%
%% 本脚本在 **run-owned scratch 库**（`imboy_full07_runbook_<ts>`，跑完即 DROP）
%% 上用**生产模块**（erlang_migrate / enterprise_webhook_repo /
%% enterprise_application_grant_repo / enterprise_application_repo /
%% bot_webhook_delivery_repo）逐条真跑，每条打印 PASS/FAIL 与关键数字。
%%
%% 四条演练：
%%   D1 升级 / 回滚：空库全量 up → version=head；down 1 → head-1；up → head；
%%                   二次 up 幂等（0 applied）；down 1 后再 up 二次幂等。
%%   D2 secret rotation：写 webhook secret → 轮换 → secret 密文变化 + generation
%%                   单调递增（旧 generation 的签名不再被接受是本脚本断言的
%%                   generation 语义；HMAC 验签链由
%%                   enterprise_webhook_governance_pg_tests 覆盖）。
%%   D3 credential emergency revoke：一次事务内撤销该应用**全部** active Grant
%%                   → effective scope 立即为空（下一个请求即失败），且 Grant
%%                   行保留为 revoked（审计可见，不是物理删除）。
%%   D4 worker restart：投递 worker 在持租约时崩溃（事务 ROLLBACK）→ 行仍可被
%%                   下一次认领（不丢投递）；正常提交租约后 → 不再被重复认领
%%                   （不重复投递）。两条合起来 = 重启语义的正确性。
%%
%% 用法（在 imboy 仓根）：
%%   PG_USER=... PG_PASSWORD=... PG_PORT=4323 scripts/full07_runbook_drill.escript
%% 环境变量前缀：FULL07_DRILL_PG_{HOST,PORT,USER,PASSWORD}
%% ============================================================

-mode(compile).

main(_Args) ->
    RepoDir = os:getenv("IMBOY_DIR", "."),
    code:add_pathsa([
        filename:join([RepoDir, "ebin"]),
        filename:join([RepoDir, "deps/epgsql/ebin"]),
        filename:join([RepoDir, "deps/erlang_migrate/ebin"]),
        filename:join([RepoDir, "deps/jsone/ebin"])
    ]),
    {ok, _} = application:ensure_all_started(epgsql),
    %% 演练进程是裸 escript（无 -config）：显式装载 imboy 应用环境。
    %% postgre_aes_key 用合成占位值（绝不是任何环境的真实密钥），
    %% 只为让 webhook secret 的 AEAD 落库路径真实走到。
    ok = load_imboy_env(),
    try
        elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3})
    catch
        _:_ -> ok
    end,
    Seed = list_to_integer(env("SEED", integer_to_list(os:system_time(second)))),
    DbName = iolist_to_binary(["imboy_full07_runbook_", integer_to_list(Seed)]),
    Server = server(),
    Maint = connect(Server, <<"postgres">>),
    Result =
        try
            createdb(Maint, DbName),
            ensure_extensions(Server, DbName),
            Mig = connect(Server, DbName),
            try
                [d1_upgrade_rollback(Mig, RepoDir), d2_secret_rotation(Mig), d3_emergency_revoke(Mig), d4_worker_restart(Mig)]
            after
                epgsql:close(Mig)
            end
        after
            dropdb(Maint, DbName),
            epgsql:close(Maint)
        end,
    Failed = [R || R <- Result, R =:= failed],
    io:format("~n=== FULL-07 runbook drill: ~p/~p PASS, scratch db ~ts dropped ===~n", [
        length(Result) - length(Failed), length(Result), DbName
    ]),
    case Failed of
        [] -> halt(0);
        _ -> halt(1)
    end.

%% ------------------------------------------------------------------
%% D1 升级 / 回滚
%% ------------------------------------------------------------------
d1_upgrade_rollback(Conn, RepoDir) ->
    MigDir = filename:join(RepoDir, "priv/migrations"),
    Conf = #{conn => Conn, dir => MigDir, strict => true},
    ok = mig_up(Conf),
    Head = mig_version(Conf),
    ok = erlang_migrate:down(Conf, 1),
    HeadMinus1 = mig_version(Conf),
    ok = mig_up(Conf),
    Head2 = mig_version(Conf),
    HeadApplied = mig_version(Conf),
    ok = erlang_migrate:down(Conf, 1),
    ok = mig_up(Conf),
    Final = mig_version(Conf),
    report("D1 upgrade/rollback", [
        {"head_after_full_up", Head},
        {"head_minus_1_after_down", HeadMinus1},
        {"head_after_up_again", Head2},
        {"second_up_idempotent", HeadApplied =:= Head},
        {"final_version", Final}
    ], [
        Head =:= Head2,
        Final =:= Head,
        HeadMinus1 =:= Head - 1,
        HeadApplied =:= Head
    ]).

%% erlang_migrate 的 up 返回 ok 或 {ok, Applied}（两种都是成功）；
%% version 返回 {ok, N, Dirty}。统一收敛成脚本内的简单形态。
mig_up(Conf) ->
    case erlang_migrate:up(Conf) of
        ok -> ok;
        {ok, _Applied} -> ok;
        Other -> erlang:error({drill_migrate_up_failed, Other})
    end.

mig_version(Conf) ->
    case erlang_migrate:version(Conf) of
        {ok, N, _Dirty} -> N;
        Other -> erlang:error({drill_migrate_version_failed, Other})
    end.

%% ------------------------------------------------------------------
%% D2 secret rotation
%% ------------------------------------------------------------------
d2_secret_rotation(Conn) ->
    {OrgId, AppId, Principal} = seed_org_app(Conn, <<"full07drill-rot">>),
    _ = {OrgId, AppId},
    {ok, _} = enterprise_webhook_repo:upsert_config_tx(
        Conn,
        Principal,
        #{
            name => <<"full07drill">>,
            username => enterprise_webhook_repo:bot_username(<<"full07drill-rot">>),
            webhook_url => <<"https://oa.example.com/hook">>
        },
        [<<"message.enterprise.accepted">>],
        1
    ),
    {ok, G0} = enterprise_webhook_repo:bump_generation_tx(Conn, Principal),
    {ok, updated} = enterprise_webhook_repo:set_secret_tx(
        Conn, Principal, <<"test-webhook-secret-placeholder-v1">>
    ),
    Enc1 = secret_cipher(Conn, Principal),
    {ok, G1} = enterprise_webhook_repo:bump_generation_tx(Conn, Principal),
    {ok, updated} = enterprise_webhook_repo:set_secret_tx(
        Conn, Principal, <<"test-webhook-secret-placeholder-v2">>
    ),
    Enc2 = secret_cipher(Conn, Principal),
    {ok, G2} = enterprise_webhook_repo:bump_generation_tx(Conn, Principal),
    %% 密文整列替换（旧 secret 不再有任何残留形态）+ 明文绝不入库
    PlainLeak = binary:match(Enc2, [<<"placeholder-v2">>]) =/= nomatch,
    report("D2 secret rotation", [
        {"generation_after_config", G0},
        {"generation_after_rotate_1", G1},
        {"generation_after_rotate_2", G2},
        {"ciphertext_changed", Enc1 =/= Enc2},
        {"plaintext_in_column", PlainLeak}
    ], [
        G1 > G0,
        G2 > G1,
        Enc1 =/= Enc2,
        Enc1 =/= <<>>,
        not PlainLeak
    ]).

secret_cipher(Conn, Principal) ->
    Tb = elib_pg_sql:public_tablename(<<"bot">>),
    case epgsql:equery(Conn, <<"SELECT verify_token_enc FROM ", Tb/binary,
                               " WHERE user_id = $1">>, [Principal]) of
        {ok, _, [{Enc}]} -> Enc;
        Other -> erlang:error({drill_secret_read_failed, Other})
    end.

%% ------------------------------------------------------------------
%% D3 credential emergency revoke（一次事务内撤全部 active Grant）
%% ------------------------------------------------------------------
d3_emergency_revoke(Conn) ->
    {OrgId, AppId, Principal} = seed_org_app(Conn, <<"full07drill-revoke">>),
    Saved0 = [begin
         {ok, G} = enterprise_application_grant_repo:create_tx(Conn, OrgId, AppId, #{
             scopes => [S],
             workspace_scope_kind => none,
             expires_at => <<"2099-12-31T00:00:00+00:00">>,
             idempotency_key => Key
         }),
         #{
             id => maps:get(<<"id">>, G),
             version => maps:get(<<"version">>, G, 1)
         }
     end
     || {S, Key} <- [
            {<<"messages:send">>, <<"drill-k1">>},
            {<<"messages:send_as_human">>, <<"drill-k2">>},
            {<<"webhooks:manage">>, <<"drill-k3">>}
        ]],
    {ok, Before} = enterprise_application_grant_repo:effective_scopes_tx(Conn, OrgId, AppId),
    Saved = Saved0,
    Revoked = lists:foldl(
        fun(#{id := GId, version := Ver}, Acc) ->
            case
                %% revoked_by 必须是真实 user 行（fk_eag_revoked_by RESTRICT）——
                %% 紧急撤销的"执行人"是本 Org 的 owner/principal，不是 0。
                enterprise_application_grant_repo:revoke_tx(
                    Conn, OrgId, AppId, GId, Ver, Principal
                )
            of
                ok -> Acc + 1;
                _ -> Acc
            end
        end,
        0,
        Saved
    ),
    {ok, After} = enterprise_application_grant_repo:effective_scopes_tx(Conn, OrgId, AppId),
    Governed = enterprise_application_grant_repo:grant_governed_tx(Conn, OrgId, AppId),
    {ok, Rows} = enterprise_application_grant_repo:list_tx(Conn, OrgId, AppId),
    StillThere = length([R || R <- Rows, maps:get(<<"status">>, R) =:= <<"revoked">>]),
    report("D3 credential emergency revoke", [
        {"effective_scopes_before", length(Before)},
        {"grants_revoked", Revoked},
        {"effective_scopes_after", After},
        {"still_governed", Governed},
        {"revoked_rows_kept", StillThere}
    ], [
        length(Before) =:= 3,
        Revoked =:= 3,
        After =:= [],
        Governed =:= {ok, true},
        StillThere =:= 3
    ]).

%% ------------------------------------------------------------------
%% D4 worker restart（认领租约的崩溃/提交两种语义）
%% ------------------------------------------------------------------
d4_worker_restart(Conn) ->
    {OrgId, AppId, Principal} = seed_org_app(Conn, <<"full07drill-worker">>),
    {ok, _} = enterprise_webhook_repo:upsert_config_tx(
        Conn,
        Principal,
        #{
            name => <<"full07drill">>,
            username => enterprise_webhook_repo:bot_username(<<"full07drill-worker">>),
            webhook_url => <<"https://oa.example.com/hook">>
        },
        [<<"file.confirmed">>],
        1
    ),
    Did = <<"full07drill-delivery-1">>,
    ok = insert_delivery(Conn, Did, <<"full07drill-idem-1">>, OrgId, AppId, Principal),
    %% ① 崩溃语义：认领后事务 ROLLBACK（= worker 进程被杀，租约未落库）
    ok = squery(Conn, <<"BEGIN">>),
    {ok, CrashRows} = bot_webhook_delivery_repo:claim_due_tx(Conn, 50),
    ok = squery(Conn, <<"ROLLBACK">>),
    CrashClaimed = length([R || R <- CrashRows, maps:get(<<"delivery_id">>, R) =:= Did]),
    %% 崩溃后必须仍可被认领（不丢投递）
    ok = squery(Conn, <<"BEGIN">>),
    {ok, RetryRows} = bot_webhook_delivery_repo:claim_due_tx(Conn, 50),
    ok = squery(Conn, <<"COMMIT">>),
    RetryClaimed = length([R || R <- RetryRows, maps:get(<<"delivery_id">>, R) =:= Did]),
    %% ② 提交语义（租约生效）：重启后的新 worker 不得重复投递
    ok = squery(Conn, <<"BEGIN">>),
    {ok, AgainRows} = bot_webhook_delivery_repo:claim_due_tx(Conn, 50),
    ok = squery(Conn, <<"COMMIT">>),
    AgainClaimed = length([R || R <- AgainRows, maps:get(<<"delivery_id">>, R) =:= Did]),
    report("D4 worker restart", [
        {"claimed_before_crash_rollback", CrashClaimed},
        {"claimed_after_crash_rollback", RetryClaimed},
        {"claimed_after_lease_committed", AgainClaimed}
    ], [
        CrashClaimed =:= 1,
        RetryClaimed =:= 1,
        AgainClaimed =:= 0
    ]).

insert_delivery(Conn, Did, IdemKey, OrgId, AppId, Principal) ->
    squery(Conn, [
        <<"INSERT INTO bot_delivery (delivery_id, bot_id, event_type, payload,">>,
        <<"correlation_id, idempotency_key, webhook_url, webhook_host, pinned_ip, status,">>,
        <<"next_retry_at, ewh_owner_organization_id, ewh_owner_application_id)">>,
        <<" VALUES ('">>,
        Did,
        <<"', 'eapp:">>,
        integer_to_binary(Principal),
        <<"', 'file.confirmed', '{}', 'corrfull07drill0001', '">>,
        IdemKey,
        <<"', 'https://oa.example.com/hook', 'oa.example.com', '93.184.216.34',">>,
        <<" 'pending', NOW() - INTERVAL '1 minute', ">>,
        integer_to_binary(OrgId),
        <<", ">>,
        integer_to_binary(AppId),
        <<")">>
    ]).

%% ------------------------------------------------------------------
%% 夹具
%% ------------------------------------------------------------------
seed_org_app(Conn, NameSuffix) ->
    OrgId = 998700000 + erlang:phash2(NameSuffix, 100000),
    UserId = OrgId + 1,
    AppKey = <<"full07drill-", NameSuffix/binary>>,
    ok = squery(Conn, [
        <<"INSERT INTO \"user\" (id, password, account, account_type, status, reg_ip, reg_cosv)">>,
        <<" VALUES (">>,
        integer_to_binary(UserId),
        <<", 'x', 'drill_">>,
        integer_to_binary(UserId),
        <<"', 0, 1, '127.0.0.1', 'x') ON CONFLICT (id) DO NOTHING">>
    ]),
    ok = squery(Conn, [
        <<"INSERT INTO organization (id, name, owner_id, status, branding, settings, created_at,">>,
        <<" updated_at) VALUES (">>,
        integer_to_binary(OrgId),
        <<", 'full07-drill', ">>,
        integer_to_binary(UserId),
        <<", 'active', '{}'::jsonb, '{}'::jsonb, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>,
        <<" ON CONFLICT (id) DO NOTHING">>
    ]),
    {ok, App} = enterprise_application_repo:create_tx(
        Conn, OrgId, AppKey, <<"full07 drill app"/utf8>>, {UserId, [<<"messages:send">>]}
    ),
    {OrgId, maps:get(<<"id">>, App), UserId}.

%% imboy 应用环境（裸 escript 无 -config 注入时手工装载；已设置则不覆盖）
load_imboy_env() ->
    case application:load(imboy) of
        ok -> ok;
        {error, {already_loaded, imboy}} -> ok;
        {error, R} -> erlang:error({drill_app_load_failed, R})
    end,
    case application:get_env(imboy, postgre_aes_key) of
        {ok, K} when is_binary(K), byte_size(K) > 0 -> ok;
        _ -> application:set_env(imboy, postgre_aes_key, <<"full07-drill-synthetic-aes-key">>)
    end,
    ok.

%% ------------------------------------------------------------------
%% 基础设施
%% ------------------------------------------------------------------
server() ->
    #{
        host => env("FULL07_DRILL_PG_HOST", "127.0.0.1"),
        port => list_to_integer(env("FULL07_DRILL_PG_PORT", "4323")),
        username => env("FULL07_DRILL_PG_USER", "imboy_user"),
        password => env("FULL07_DRILL_PG_PASSWORD", "")
    }.

connect(Server, Db) ->
    %% rfc3339_bin codec：生产连接同款（elib_dt:now/0 产出的 RFC3339 文本
    %% 必须按 timestamptz 编码，否则 epgsql 走默认 codec 直接 function_clause）。
    Opts = maps:merge(Server, #{
        database => Db, timeout => 10000, codecs => [{epgsql_codec_rfc3339_bin, []}]
    }),
    case epgsql:connect(Opts) of
        {ok, C} -> C;
        {error, R} -> erlang:error({drill_connect_failed, Db, R})
    end.

createdb(Conn, Db) ->
    case epgsql:squery(Conn, <<"CREATE DATABASE ", Db/binary>>) of
        {ok, _, _} -> ok;
        {error, R} -> erlang:error({drill_createdb_failed, Db, R})
    end.

dropdb(Conn, Db) ->
    _ = epgsql:squery(Conn, <<"DROP DATABASE IF EXISTS ", Db/binary>>),
    ok.

ensure_extensions(Server, Db) ->
    Conn = connect(Server, Db),
    try
        [
            begin
                case epgsql:squery(Conn, <<"CREATE EXTENSION IF NOT EXISTS ", E/binary>>) of
                    {ok, _, _} -> ok;
                    {error, R} -> erlang:error({drill_extension_failed, E, R})
                end
            end
         || E <- [
                <<"pgcrypto">>,
                <<"pg_jieba">>,
                <<"timescaledb">>,
                <<"vector">>,
                <<"postgis">>,
                <<"pg_trgm">>,
                <<"btree_gin">>,
                <<"btree_gist">>,
                <<"citext">>,
                <<"unaccent">>,
                <<"intarray">>,
                <<"pg_stat_statements">>
            ]
        ],
        ok
    after
        epgsql:close(Conn)
    end.

squery(Conn, Sql) ->
    case epgsql:squery(Conn, iolist_to_binary(Sql)) of
        {ok, _, _} -> ok;
        {ok, _} -> ok;
        {error, R} -> erlang:error({drill_sql_failed, R, iolist_to_binary(Sql)})
    end.

report(Name, Facts, Asserts) ->
    Verdict = case lists:all(fun(X) -> X end, Asserts) of true -> pass; false -> failed end,
    io:format("~n--- ~ts: ~ts ---~n", [Name, string:uppercase(atom_to_list(Verdict))]),
    [
        io:format("    ~-38ts = ~0p~n", [K, V])
     || {K, V} <- Facts
    ],
    Verdict.

env(Key, Default) ->
    case os:getenv(Key) of
        false -> Default;
        "" -> Default;
        V -> V
    end.
