#!/usr/bin/env escript
%%! -noshell
%% ===================================================================
%% ORG-03 Invitation 行为矩阵 harness（一次性 PG / disposable marker）
%% -------------------------------------------------------------------
%% 用法:
%%   PGHOST=127.0.0.1 PGPORT=4397 PGUSER=imboy_user PGPASSWORD=... \
%%   PGDATABASE=org03_disposable_marker_20260916 IMBOY_DIR=<worktree> \
%%   test/lib/organization/organization_invitation_behavior_harness.escript
%%
%% 前置: 该库已应用迁移链至 00000128（drill_migrate.escript up；
%%       126/127 为 ORG-01 的前置迁移，129 属 ORG-04 在途文件，不进本验证链）。
%% 探针: P01-P15 = 应用层 organization_invitation_app 命令真调（elib_pg:with_tx）；
%%       A01-A06 = DB 不变量矩阵（C11 冻结 schema 正反例）；
%%       C01-C03 = 并发（C01 两连接 CAS 消费恰一胜；C02 应用层并发 accept；
%%                 C03 并发 create 唯一索引裁决）。
%% fixture 全部使用 81e8 段 bigint id；明文 token 只存在于进程内存与本次输出，
%% 不写入任何文件；密码只从环境读取，不打印。
%% ===================================================================
main(_) ->
    RepoDir = env("IMBOY_DIR", "."),
    code:add_pathsa([
        filename:join([RepoDir, "ebin"]),
        filename:join([RepoDir, "deps/epgsql/ebin"]),
        filename:join([RepoDir, "deps/pooler/ebin"]),
        filename:join([RepoDir, "deps/lager/ebin"]),
        filename:join([RepoDir, "deps/erlware_commons/ebin"]),
        filename:join([RepoDir, "deps/goldrush/ebin"])
    ]),
    ConnOpts = conn_opts(),
    {ok, Conn} = epgsql:connect(ConnOpts),
    {ok, Conn2} = epgsql:connect(ConnOpts),
    Counters = counters:new(1, []),
    ok = app_probes(Conn, Counters),
    ok = db_probes(Conn, Counters),
    ok = concurrency_probes(Conn, Conn2, Counters),
    cleanup(Conn),
    epgsql:close(Conn),
    epgsql:close(Conn2),
    io:format("~nPROBE-SUMMARY pass=~p~n", [counters:get(Counters, 1)]),
    halt(0).

conn_opts() ->
    #{host => env("PGHOST", "127.0.0.1"),
      port => list_to_integer(env("PGPORT", "4397")),
      username => env("PGUSER", "imboy_user"),
      password => env("PGPASSWORD", ""),
      database => env("PGDATABASE", "org03_disposable_marker_20260916")}.

-define(ORG_A, 810000101).
-define(ORG_B, 810000102).
-define(ORG_C, 810000103).
-define(OWNER, 810000001).
-define(ADMIN, 810000002).
-define(MEMBER, 810000003).
-define(TARGET, 810000004).
-define(STRANGER, 810000005).
-define(OWNER_B, 810000006).

%%--------------------------------------------------------------------
%% P01-P15: 应用层命令真调
%%--------------------------------------------------------------------
app_probes(Conn, Counters) ->
    {ok, _} = application:ensure_all_started(pooler),
    %% with_tx 的连接池名来自 config_ds:env(sql_driver)（application:env imboy）
    ok = application:set_env(imboy, sql_driver, pgsql),
    {ok, _} = pooler:new_pool(maps:merge(#{name => pgsql, init_count => 2, max_count => 6,
                                           queue_max => 20},
                                          #{start_mfa => {epgsql, connect, [conn_opts()]}})),
    ok = fixture_base(Conn),
    ok = fixture_org(Conn, ?ORG_A, ?OWNER, [{admin, ?ADMIN}, {member, ?MEMBER}]),

    %% P01 create 成功：明文只返回一次；库中只有 sha256 hex digest；无明文列
    Inv1 = 810000201,
    {ok, View1} = organization_invitation_app:create(?OWNER, ?ORG_A, ?TARGET,
                                                     #{invitation_id => Inv1}),
    Token1 = maps:get(token, View1),
    true = is_binary(Token1) andalso byte_size(Token1) =:= 64,
    #{<<"status">> := <<"pending">>} = pending_row(Conn, Inv1),
    StoredDigest = digest_of_row(Conn, Inv1),
    true = StoredDigest =:= organization_invitation:token_digest(Token1),
    true = StoredDigest =/= Token1,
    true = columns_of(Conn) =:= expected_columns(),
    pass(Counters, "P01 create success: plaintext once, only sha256 digest stored, no plaintext column"),

    %% P02 同 (org,target) 再建 pending → 409（部分唯一索引）
    case organization_invitation_app:create(?ADMIN, ?ORG_A, ?TARGET,
                                            #{invitation_id => 810000202}) of
        {error, {409, _}} -> pass(Counters, "P02 duplicate pending invitation rejected 409");
        Other2 -> fail(Counters, "P02 duplicate pending invitation rejected 409", Other2)
    end,

    %% P03 邀请已是 active 成员 → 409
    case organization_invitation_app:create(?OWNER, ?ORG_A, ?ADMIN,
                                            #{invitation_id => 810000203}) of
        {error, {409, _}} -> pass(Counters, "P03 invite existing active member rejected 409");
        Other3 -> fail(Counters, "P03 invite existing active member rejected 409", Other3)
    end,

    %% P04 普通成员（非治理）创建 → 403
    case organization_invitation_app:create(?MEMBER, ?ORG_A, ?STRANGER,
                                            #{invitation_id => 810000204}) of
        {error, {403, _}} -> pass(Counters, "P04 non-governance inviter rejected 403");
        Other4 -> fail(Counters, "P04 non-governance inviter rejected 403", Other4)
    end,

    %% P05 archived 组织 → 409（C16）
    ok = fixture_org(Conn, ?ORG_C, ?OWNER, []),
    {ok, _} = x(Conn, [<<"UPDATE organization SET status='archived' WHERE id=">>,
                       orgb(?ORG_C)]),
    case organization_invitation_app:create(?OWNER, ?ORG_C, ?TARGET,
                                            #{invitation_id => 810000205}) of
        {error, {409, _}} -> pass(Counters, "P05 archived org create rejected 409");
        Other5 -> fail(Counters, "P05 archived org create rejected 409", Other5)
    end,

    %% P06 组织不存在 → 404
    case organization_invitation_app:create(?OWNER, 810000199, ?TARGET,
                                            #{invitation_id => 810000206}) of
        {error, {404, _}} -> pass(Counters, "P06 missing org create rejected 404");
        Other6 -> fail(Counters, "P06 missing org create rejected 404", Other6)
    end,

    %% P07 accept 成功（target-only + 默认无 hook：不产生 membership，边界即止）
    case organization_invitation_app:accept(?TARGET, ?ORG_A, Token1, #{}) of
        {ok, View7} ->
            false = maps:get(already_accepted, View7),
            <<"accepted">> = maps:get(status, View7),
            RespondedAt = maps:get(responded_at, View7),
            true = is_integer(RespondedAt),
            {ok, _, [{N7}]} = q(Conn,
                [<<"SELECT count(*) FROM organization_member WHERE organization_id=">>,
                 orgb(?ORG_A), <<" AND user_id=">>, integer_to_binary(?TARGET)]),
            0 = b2i(N7),
            pass(Counters, "P07 accept success: consumed once, no membership by default hook");
        Other7 -> fail(Counters, "P07 accept success", Other7)
    end,

    %% P08 重复 accept 幂等：同一终态、responded_at 不变、already_accepted=true
    Row8 = row_by_id(Conn, Inv1),
    case organization_invitation_app:accept(?TARGET, ?ORG_A, Token1, #{}) of
        {ok, View8} ->
            true = maps:get(already_accepted, View8),
            <<"accepted">> = maps:get(status, View8),
            true = maps:get(responded_at, View8) =:= maps:get(<<"responded_at">>, Row8),
            pass(Counters, "P08 replay accept idempotent: same terminal, responded_at unchanged");
        Other8 -> fail(Counters, "P08 replay accept idempotent", Other8)
    end,

    %% P09 wrong-target：陌生用户持同一 token → 404，行不变
    Inv9 = 810000209,
    {ok, V9} = organization_invitation_app:create(?OWNER, ?ORG_A, ?STRANGER,
                                                  #{invitation_id => Inv9}),
    remember_token(Inv9, maps:get(token, V9)),
    case organization_invitation_app:accept(?TARGET, ?ORG_A, known_token(Inv9), #{}) of
        {error, {404, _}} ->
            <<"pending">> = status_by_id(Conn, Inv9),
            pass(Counters, "P09 wrong target accept rejected 404, row untouched");
        Other9 -> fail(Counters, "P09 wrong target accept rejected 404", Other9)
    end,
    %% 释放 STRANGER 的 pending 占位（P15 需为其重建邀请）
    {ok, _} = organization_invitation_app:revoke(?OWNER, ?ORG_A, Inv9),

    %% P10 过期 accept → 409；行被 lazy sweep 置为 expired
    Inv10 = 810000210,
    {ok, _} = x(Conn,
        [<<"INSERT INTO organization_invitation"
           " (id, organization_id, target_user_id, invited_by, token_digest, status, expires_at)"
           " VALUES (">>, integer_to_binary(Inv10), <<",">>, orgb(?ORG_A), <<",">>,
         integer_to_binary(?TARGET), <<",">>, integer_to_binary(?OWNER), <<",'">>,
         organization_invitation:token_digest(<<"expired-token">>),
         <<"','pending', CURRENT_TIMESTAMP - interval '10 seconds')">>]),
    case organization_invitation_app:accept(?TARGET, ?ORG_A, <<"expired-token">>, #{}) of
        {error, {409, _}} ->
            %% 过期裁决基于 classify_accept（读路径 fail-closed）；但 accept 中止 =
            %% 整个事务回滚，sweep 随之回滚——行保持 pending（占位由下一次
            %% 成功命令的 sweep-on-commit 释放，见 P10b）。
            <<"pending">> = status_by_id(Conn, Inv10),
            pass(Counters, "P10 expired accept rejected 409 (sweep rolls back with aborted tx)"),
            %% P10b: create 同 (org,target)：create_tx 先 sweep（提交）再插入 → 成功
            Inv10b = 810000214,
            case organization_invitation_app:create(?OWNER, ?ORG_A, ?TARGET,
                                                    #{invitation_id => Inv10b}) of
                {ok, _} ->
                    <<"expired">> = status_by_id(Conn, Inv10),
                    <<"pending">> = status_by_id(Conn, Inv10b),
                    %% 释放占位（同时再覆盖一次 revoke 路径），后续探针不受污染
                    {ok, _} = organization_invitation_app:revoke(?OWNER, ?ORG_A, Inv10b),
                    pass(Counters, "P10b create after expiry: sweep committed, pending slot reclaimed");
                Other10b -> fail(Counters, "P10b create after expiry", Other10b)
            end;
        Other10 -> fail(Counters, "P10 expired accept", Other10)
    end,

    %% P11 revoke 后 accept → 409；非治理 revoke → 403
    Inv11 = 810000211,
    {ok, V11} = organization_invitation_app:create(?ADMIN, ?ORG_A, ?TARGET,
                                                   #{invitation_id => Inv11}),
    remember_token(Inv11, maps:get(token, V11)),
    Token11 = known_token(Inv11),
    case organization_invitation_app:revoke(?MEMBER, ?ORG_A, Inv11) of
        {error, {403, _}} -> pass(Counters, "P11a non-governance revoke rejected 403");
        Other11a -> fail(Counters, "P11a non-governance revoke rejected 403", Other11a)
    end,
    case organization_invitation_app:revoke(?OWNER, ?ORG_A, Inv11) of
        {ok, _} ->
            <<"revoked">> = status_by_id(Conn, Inv11),
            case organization_invitation_app:accept(?TARGET, ?ORG_A, Token11, #{}) of
                {error, {409, _}} ->
                    pass(Counters, "P11b accept after revoke rejected 409");
                Other11b -> fail(Counters, "P11b accept after revoke", Other11b)
            end;
        Other11 -> fail(Counters, "P11b revoke success", Other11)
    end,

    %% P12 reject 后 accept → 409；重复 reject 幂等
    Inv12 = 810000212,
    {ok, V12} = organization_invitation_app:create(?OWNER, ?ORG_A, ?TARGET,
                                                   #{invitation_id => Inv12}),
    remember_token(Inv12, maps:get(token, V12)),
    Token12 = known_token(Inv12),
    {ok, _} = organization_invitation_app:reject(?TARGET, ?ORG_A, Inv12),
    {ok, _} = organization_invitation_app:reject(?TARGET, ?ORG_A, Inv12),
    case organization_invitation_app:accept(?TARGET, ?ORG_A, Token12, #{}) of
        {error, {409, _}} -> pass(Counters, "P12 accept after reject rejected 409, reject idempotent");
        Other12 -> fail(Counters, "P12 accept after reject", Other12)
    end,

    %% P13 cross-org 隔离：A 的 token 在 B 命中不了；B 的 token 在 B 正常
    ok = fixture_org(Conn, ?ORG_B, ?OWNER_B, []),
    Inv13b = 810000213,
    {ok, V13b} = organization_invitation_app:create(?OWNER_B, ?ORG_B, ?TARGET,
                                                    #{invitation_id => Inv13b}),
    remember_token(Inv13b, maps:get(token, V13b)),
    case organization_invitation_app:accept(?TARGET, ?ORG_B, Token1, #{}) of
        {error, {404, _}} -> pass(Counters, "P13a cross-org token rejected 404");
        Other13a -> fail(Counters, "P13a cross-org token rejected 404", Other13a)
    end,
    case organization_invitation_app:accept(?TARGET, ?ORG_B, known_token(Inv13b), #{}) of
        {ok, _} -> pass(Counters, "P13b own-org token accepted");
        Other13b -> fail(Counters, "P13b own-org token accepted", Other13b)
    end,

    %% P14 列表：org 视角（治理）与 target 视角（此节点上 pending 已被前序
    %% 探针消化，按无过滤列表断言行数与 Org 作用域）
    {ok, OrgRows} = organization_invitation_app:list_for_org(?OWNER, ?ORG_A, #{}),
    true = length(OrgRows) >= 1,
    true = lists:all(fun(R) -> maps:get(organization_id, R) =:= ?ORG_A end, OrgRows),
    {ok, TargetRows} = organization_invitation_app:list_for_target(?TARGET, #{}),
    true = length(TargetRows) >= 1,
    case organization_invitation_app:list_for_org(?MEMBER, ?ORG_A, #{}) of
        {error, {403, _}} -> pass(Counters, "P14 lists scoped, non-governance list rejected 403");
        Other14 -> fail(Counters, "P14 lists scoped", Other14)
    end,

    %% P15 membership_hook：同事务挂点真实产生成员行；重放不重复产生
    Inv15 = 810000215,
    {ok, V15} = organization_invitation_app:create(?OWNER, ?ORG_A, ?STRANGER,
                                                   #{invitation_id => Inv15}),
    remember_token(Inv15, maps:get(token, V15)),
    Token15 = known_token(Inv15),
    Hook =
        fun(HookConn, Row) ->
            OrgB = integer_to_binary(maps:get(<<"organization_id">>, Row)),
            TgtB = integer_to_binary(maps:get(<<"target_user_id">>, Row)),
            case elib_pg:execute(HookConn,
                    [<<"INSERT INTO organization_member"
                       " (organization_id,user_id,role,status,joined_at,created_at,updated_at)"
                       " VALUES (">>, OrgB, <<",">>, TgtB,
                       <<",'member','active',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP)"
                         " ON CONFLICT (organization_id,user_id) DO NOTHING">>], []) of
                {ok, _} -> ok;
                {error, _} = E -> E
            end
        end,
    case organization_invitation_app:accept(?STRANGER, ?ORG_A, Token15,
                                            #{membership_hook => Hook}) of
        {ok, _} ->
            {ok, _, [{N15}]} = q(Conn,
                [<<"SELECT count(*) FROM organization_member WHERE organization_id=">>,
                 orgb(?ORG_A), <<" AND user_id=">>, integer_to_binary(?STRANGER)]),
            1 = b2i(N15),
            {ok, _} = organization_invitation_app:accept(?STRANGER, ?ORG_A, Token15, #{}),
            {ok, _, [{N15b}]} = q(Conn,
                [<<"SELECT count(*) FROM organization_member WHERE organization_id=">>,
                 orgb(?ORG_A), <<" AND user_id=">>, integer_to_binary(?STRANGER)]),
            1 = b2i(N15b),
            pass(Counters, "P15 membership hook creates exactly one member row, replay adds none");
        Other15 -> fail(Counters, "P15 membership hook", Other15)
    end,
    ok.

%%--------------------------------------------------------------------
%% A01-A06: DB 不变量矩阵
%%--------------------------------------------------------------------
db_probes(Conn, Counters) ->
    ok = fixture_base(Conn),
    ok = fixture_org(Conn, 810000111, 810000011, []),
    %% A01 同 (org,target) 两条 pending → 部分唯一索引 23505
    {aborted, <<"23505">>} = tx(Conn, fun(C) ->
        {ok, _} = x(C, invitation_sql(810000301, 810000111, 810000012, 810000011, <<"a">>)),
        {ok, _} = x(C, invitation_sql(810000302, 810000111, 810000012, 810000011, <<"b">>)),
        commit
    end),
    pass(Counters, "A01 second pending same (org,target) rejected by partial unique index"),
    %% A02 终态行可并存（accepted + revoked 同 (org,target)）
    {ok, _} = tx(Conn, fun(C) ->
        {ok, _} = x(C, invitation_sql(810000303, 810000111, 810000013, 810000011, <<"c">>)),
        {ok, _} = x(C, [<<"UPDATE organization_invitation SET status='accepted',"
                         " responded_at=CURRENT_TIMESTAMP WHERE id=810000303">>]),
        {ok, _} = x(C, invitation_sql(810000304, 810000111, 810000013, 810000011, <<"d">>)),
        {ok, _} = x(C, [<<"UPDATE organization_invitation SET status='revoked',"
                         " responded_at=CURRENT_TIMESTAMP WHERE id=810000304">>]),
        commit
    end),
    pass(Counters, "A02 terminal rows coexist for same (org,target)"),
    %% A03 非法 status → CHECK 23514
    {aborted, <<"23514">>} = tx(Conn, fun(C) ->
        {ok, _} = x(C, [<<"INSERT INTO organization_invitation"
                         " (id, organization_id, target_user_id, invited_by, token_digest, status, expires_at)"
                         " VALUES (810000305,810000111,810000014,810000011,'", (organization_invitation:token_digest(<<"e">>))/binary,
                         "','weird',CURRENT_TIMESTAMP)">>]),
        commit
    end),
    pass(Counters, "A03 invalid status rejected by CHECK"),
    %% A04 非法 digest 形态 → CHECK 23514
    {aborted, <<"23514">>} = tx(Conn, fun(C) ->
        {ok, _} = x(C, [<<"INSERT INTO organization_invitation"
                         " (id, organization_id, target_user_id, invited_by, token_digest, status, expires_at)"
                         " VALUES (810000306,810000111,810000015,810000011,'plaintext-token','pending',CURRENT_TIMESTAMP)">>]),
        commit
    end),
    pass(Counters, "A04 non-digest token rejected by CHECK (plaintext cannot be stored)"),
    %% A05 target 用户不存在 → FK 23503
    {aborted, <<"23503">>} = tx(Conn, fun(C) ->
        {ok, _} = x(C, invitation_sql(810000307, 810000111, 819999999, 810000011, <<"f">>)),
        commit
    end),
    pass(Counters, "A05 missing target user rejected by FK"),
    %% A06 lazy expire sweep SQL：过期 pending → expired
    {ok, _} = x(Conn, [<<"INSERT INTO organization_invitation"
                        " (id, organization_id, target_user_id, invited_by, token_digest, status, expires_at)"
                        " VALUES (810000308,810000111,810000016,810000011,'">>,
                       organization_invitation:token_digest(<<"g">>),
                       <<"','pending', CURRENT_TIMESTAMP - interval '1 hour')">>]),
    {ok, _} = x(Conn, <<"UPDATE organization_invitation SET status='expired',"
                       " responded_at=CURRENT_TIMESTAMP, updated_at=CURRENT_TIMESTAMP"
                       " WHERE status='pending' AND expires_at <= CURRENT_TIMESTAMP"
                       " AND organization_id=810000111">>),
    {ok, _, [{<<"expired">>}]} = q(Conn, <<"SELECT status FROM organization_invitation"
                                           " WHERE id=810000308">>),
    pass(Counters, "A06 expire sweep transitions due pending to expired"),
    ok.

invitation_sql(Id, OrgId, TargetUid, InvitedBy, TokenTag) ->
    [<<"INSERT INTO organization_invitation"
       " (id, organization_id, target_user_id, invited_by, token_digest, status, expires_at)"
       " VALUES (">>, integer_to_binary(Id), <<",">>, integer_to_binary(OrgId),
     <<",">>, integer_to_binary(TargetUid), <<",">>, integer_to_binary(InvitedBy),
     <<",'">>, organization_invitation:token_digest(TokenTag),
     <<"','pending', CURRENT_TIMESTAMP + interval '1 day')">>].

%%--------------------------------------------------------------------
%% C01-C03: 并发
%%--------------------------------------------------------------------
concurrency_probes(Conn, Conn2, Counters) ->
    ok = fixture_base(Conn),
    ok = fixture_org(Conn, 810000121, 810000021, []),
    %% C01 并发 accept：两连接同语句 CAS 消费，恰一胜
    Inv = 810000401,
    {ok, _} = x(Conn, invitation_sql(Inv, 810000121, 810000022, 810000021, <<"c01">>)),
    Parent = self(),
    W1 = spawn(fun() -> Parent ! {td, self(), consume_sequence(Conn, Inv)} end),
    W2 = spawn(fun() -> Parent ! {td, self(), consume_sequence(Conn2, Inv)} end),
    R1 = receive {td, W1, X1} -> X1 after 15000 -> timeout end,
    R2 = receive {td, W2, X2} -> X2 after 15000 -> timeout end,
    Winners = length([ok || consumed <- [R1, R2]]),
    FinalStatus = status_by_id(Conn, Inv),
    case {Winners, FinalStatus} of
        {1, <<"accepted">>} ->
            pass(Counters, "C01 concurrent accept: exactly one CAS winner, final accepted");
        OtherC1 -> fail(Counters, "C01 concurrent accept", {R1, R2, OtherC1})
    end,

    %% C02 应用层并发 accept（两进程真过 with_tx/pool）：恰一 already_accepted=false
    Org2 = 810000122,
    ok = fixture_org(Conn, Org2, 810000023, []),
    Inv2 = 810000402,
    {ok, ViewC2} = organization_invitation_app:create(810000023, Org2, 810000024,
                                                      #{invitation_id => Inv2}),
    TokenC2 = maps:get(token, ViewC2),
    SpawnAccept = fun(P) ->
        spawn(fun() -> P ! {ar, self(),
                            organization_invitation_app:accept(810000024, Org2,
                                                               TokenC2, #{})} end)
    end,
    A1 = SpawnAccept(Parent),
    A2 = SpawnAccept(Parent),
    RA1 = receive {ar, A1, XA1} -> XA1 after 15000 -> timeout end,
    RA2 = receive {ar, A2, XA2} -> XA2 after 15000 -> timeout end,
    {Firsts, Replays} =
        lists:foldl(fun(R, {F, Rp}) ->
                        case R of
                            {ok, #{already_accepted := false}} -> {F + 1, Rp};
                            {ok, #{already_accepted := true}} -> {F, Rp + 1};
                            _ -> {F, Rp}
                        end
                    end, {0, 0}, [RA1, RA2]),
    case {Firsts, Replays} of
        {1, 1} -> pass(Counters, "C02 app-level concurrent accept: one consume + one idempotent replay");
        OtherC2 -> fail(Counters, "C02 app-level concurrent accept", {RA1, RA2, OtherC2})
    end,

    %% C03 并发 create：两连接同时插入同 (org,target) pending，恰一提交
    Org3 = 810000123,
    ok = fixture_org(Conn, Org3, 810000025, []),
    Seq = fun(C, Id, Tag) ->
        tx(C, fun(CC) ->
            {ok, _} = x(CC, invitation_sql(Id, Org3, 810000026, 810000025, Tag)),
            commit
        end)
    end,
    P3 = self(),
    W3 = spawn(fun() -> P3 ! {cd, self(), Seq(Conn, 810000403, <<"c03a">>)} end),
    W4 = spawn(fun() -> P3 ! {cd, self(), Seq(Conn2, 810000404, <<"c03b">>)} end),
    R3 = receive {cd, W3, X3} -> X3 after 15000 -> timeout end,
    R4 = receive {cd, W4, X4} -> X4 after 15000 -> timeout end,
    Commits = length([ok || {ok, committed} <- [R3, R4]]),
    Aborts = length([ok || {aborted, <<"23505">>} <- [R3, R4]]),
    case {Commits, Aborts} of
        {1, 1} -> pass(Counters, "C03 concurrent create: exactly one commit, one 23505");
        OtherC3 -> fail(Counters, "C03 concurrent create", {R3, R4, OtherC3})
    end,
    ok.

%% accept command 的 DB 语句序列（sweep + CAS 消费；与 organization_invitation_app
%% consume 路径同语义，单事务，行锁串行化下恰一个 RETURNING 命中）
consume_sequence(Conn, InvId) ->
    IdB = integer_to_binary(InvId),
    Self = self(),
    tx(Conn, fun(C) ->
        {ok, _} = x(C, <<"UPDATE organization_invitation SET status='expired',"
                         " responded_at=CURRENT_TIMESTAMP WHERE status='pending'"
                         " AND expires_at <= CURRENT_TIMESTAMP">>),
        CasRes = q(C, [<<"UPDATE organization_invitation SET status='accepted',"
                        " responded_at=CURRENT_TIMESTAMP, updated_at=CURRENT_TIMESTAMP"
                        " WHERE id=">>, IdB, <<" AND status='pending' RETURNING id">>]),
        {CasCount, CasRows} =
            case CasRes of
                {ok, N} when is_integer(N) -> {N, []};
                {ok, N, _, Rows} when is_integer(N) -> {N, Rows}
            end,
        Outcome =
            case CasRows of
                [_ | _] when CasCount >= 1 -> consumed;
                _ ->
                    {ok, _, [{St}]} = q(C, [<<"SELECT status FROM organization_invitation"
                                              " WHERE id=">>, IdB]),
                    {terminal, St}
            end,
        Self ! {c01_outcome, Outcome},
        commit
    end),
    receive {c01_outcome, O} -> O after 5000 -> outcome_lost end.

%%--------------------------------------------------------------------
%% fixture / sql helpers
%%--------------------------------------------------------------------
fixture_base(Conn) ->
    cleanup(Conn),
    {ok, _} = x(Conn, <<"INSERT INTO \"user\" (id,password,account,reg_ip,reg_cosv,account_type)"
                        " SELECT i, 'x', 'org03-u' || i, '127.0.0.1', '', 0"
                        " FROM generate_series(810000001, 810000040) AS i"
                        " ON CONFLICT (id) DO NOTHING">>),
    ok.

fixture_org(Conn, OrgId, OwnerUid, Extra) ->
    OrgB = integer_to_binary(OrgId),
    {ok, _} = x(Conn, [<<"INSERT INTO organization (id,name,owner_id) VALUES (">>,
                       OrgB, <<",'org03-org',">>, integer_to_binary(OwnerUid), <<")">>]),
    lists:foreach(
        fun({Role, Uid}) ->
            RoleB = atom_to_binary(Role),
            UidB = integer_to_binary(Uid),
            Sql = <<"INSERT INTO organization_member"
                    " (organization_id,user_id,role,status,joined_at,created_at,updated_at)"
                    " VALUES (", OrgB/binary, ",", UidB/binary, ",'", RoleB/binary,
                   "','active',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP)">>,
            {ok, _} = x(Conn, Sql)
        end,
        Extra),
    ok.

cleanup_sqls() ->
    [<<"DELETE FROM organization_invitation WHERE organization_id BETWEEN 810000101 AND 810000199;">>,
     <<"DELETE FROM organization WHERE id BETWEEN 810000101 AND 810000199;">>,
     <<"DELETE FROM \"user\" WHERE id BETWEEN 810000001 AND 810000099;">>].

cleanup(Conn) ->
    lists:foreach(fun(Sql) -> _ = x(Conn, Sql) end, cleanup_sqls()),
    ok.

orgb(OrgId) -> integer_to_binary(OrgId).

pending_row(Conn, InvId) ->
    case row_by_id(Conn, InvId) of
        #{<<"status">> := <<"pending">>} = Row -> Row
    end.

row_by_id(Conn, InvId) ->
    {ok, _, [T]} = q(Conn, [<<"SELECT id, organization_id, target_user_id, invited_by,"
                             " token_digest, status, extract(epoch from expires_at)::bigint,"
                             " extract(epoch from responded_at)::bigint AS responded_at"
                             " FROM organization_invitation WHERE id=">>,
                            integer_to_binary(InvId)]),
    {Id, OrgId, TargetUid, InvitedBy, Digest, Status, ExpiresAt, RespondedAt} = T,
    #{<<"id">> => Id,
      <<"organization_id">> => OrgId,
      <<"target_user_id">> => TargetUid,
      <<"invited_by">> => InvitedBy,
      <<"token_digest">> => Digest,
      <<"status">> => Status,
      <<"expires_at">> => ExpiresAt,
      <<"responded_at">> => RespondedAt}.

%% P09-P15 的明文 token 只能来自 create 响应（digest 不可逆）：
%% 在 create 后登记，accept 用例取回。
remember_token(InvId, Token) ->
    erlang:put({org03_token, InvId}, Token).

known_token(InvId) ->
    erlang:get({org03_token, InvId}).

digest_of_row(Conn, InvId) ->
    maps:get(<<"token_digest">>, row_by_id(Conn, InvId)).

status_by_id(Conn, InvId) ->
    {ok, _, [{St}]} = q(Conn, [<<"SELECT status FROM organization_invitation WHERE id=">>,
                               integer_to_binary(InvId)]),
    St.

columns_of(Conn) ->
    {ok, _, Rows} = q(Conn,
        <<"SELECT column_name FROM information_schema.columns"
          " WHERE table_name='organization_invitation' ORDER BY column_name">>),
    [N || {N} <- Rows].

expected_columns() ->
    [<<"created_at">>, <<"expires_at">>, <<"id">>, <<"invited_by">>,
     <<"organization_id">>, <<"responded_at">>, <<"status">>,
     <<"target_user_id">>, <<"token_digest">>, <<"updated_at">>].

b2i(B) when is_binary(B) -> binary_to_integer(B);
b2i(I) when is_integer(I) -> I.

q(Conn, Sql) -> epgsql:equery(Conn, iolist_to_binary(Sql), []).
%% 统一写语句封装：UPDATE/DELETE/INSERT 返回 {ok, Count}；SELECT 返回 {ok, 0}；
%% 失败抛异常（由 tx 捕获归类）。
x(Conn, Sql) ->
    case epgsql:equery(Conn, iolist_to_binary(Sql), []) of
        {ok, N} when is_integer(N) -> {ok, N};
        {ok, _, _} -> {ok, 0};
        {error, Reason} -> erlang:error({sql_failed, Reason})
    end.

%% 事务执行：Fun 返回 commit 则 COMMIT；语句异常/throw 则 ROLLBACK。
%% 返回 {ok, _}（提交成功）或 {aborted, SqlStateBinary}。
tx(Conn, Fun) ->
    {ok, _, _} = epgsql:squery(Conn, "BEGIN"),
    Exec =
        try {ok, Fun(Conn)}
        catch _Class:Reason -> {exception, Reason}
        end,
    case Exec of
        {ok, commit} ->
            case epgsql:squery(Conn, "COMMIT") of
                {ok, _, _} -> {ok, committed};
                {error, CommitErr} ->
                    {ok, _, _} = epgsql:squery(Conn, "ROLLBACK"),
                    {aborted, sqlstate(CommitErr)}
            end;
        {exception, ExceptionReason} ->
            {ok, _, _} = epgsql:squery(Conn, "ROLLBACK"),
            {aborted, sqlstate(ExceptionReason)};
        Other ->
            {ok, _, _} = epgsql:squery(Conn, "ROLLBACK"),
            {aborted, {rolled_back, Other}}
    end.

sqlstate({sql_failed, E}) -> sqlstate(E);
sqlstate({error, _S, Code, _Cn, _Msg, _Extra}) when is_binary(Code) -> Code;
sqlstate(Other) -> Other.

pass(Counters, Name) ->
    counters:add(Counters, 1, 1),
    io:format("PROBE ~s PASS~n", [Name]).

fail(_Counters, Name, Detail) ->
    io:format("PROBE ~s FAIL detail=~p~n", [Name, Detail]),
    halt(1).

env(K, D) -> case os:getenv(K) of false -> D; V -> V end.
