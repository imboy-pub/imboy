#!/usr/bin/env escript
%%! -noshell
%% ===================================================================
%% GZAPP-01 Organization Invite Code + Join Orchestrator 行为矩阵 harness
%% -------------------------------------------------------------------
%% 用法:
%%   PGHOST=127.0.0.1 PGPORT=4323 PGUSER=imboy_user PGPASSWORD=... \
%%   PGDATABASE=scratch_gzapp_01 IMBOY_DIR=<worktree> \
%%   test/lib/organization/organization_invite_code_behavior_harness.escript
%%
%% 前置: 该库已应用迁移链至 00000137（scripts/drill_migrate.escript up；
%%       scratch 库需预装 timescaledb/pgcrypto/postgis/pg_jieba/vector）。
%% 探针: P01-P16 = 应用层命令真调（organization_invite_code_app /
%%       organization_join_orchestrator / organization_invitation_app）；
%%       A01-A02 = DB 不变量（部分唯一索引 / 全局 code 唯一）。
%% fixture 全部使用 82e8 段 bigint id；跑完数据清理、库保留。
%% ===================================================================
main(_) ->
    RepoDir = env("IMBOY_DIR", "."),
    code:add_pathsa([
        filename:join([RepoDir, "ebin"]),
        filename:join([RepoDir, "deps/epgsql/ebin"]),
        filename:join([RepoDir, "deps/pooler/ebin"]),
        filename:join([RepoDir, "deps/lager/ebin"]),
        filename:join([RepoDir, "deps/erlware_commons/ebin"]),
        filename:join([RepoDir, "deps/goldrush/ebin"]),
        filename:join([RepoDir, "deps/depcache/ebin"]),
        filename:join([RepoDir, "deps/jsone/ebin"])
    ]),
    %% TSID：init（应用默认 dc/node 位宽）+ 注册业务生成器 + 本卡新生成器
    ok = elib_tsid:init(#{
        dc_id => 0, node_id => 9, dc_bits => 3,
        names => imboy_app:tsid_generator_names()
    }),
    ok = elib_tsid:register(organization_invite_code),
    ConnOpts = conn_opts(),
    {ok, Conn} = epgsql:connect(ConnOpts),
    Counters = counters:new(1, []),
    %% with_tx 依赖 pooler 池（池名来自 config_ds:env(sql_driver)）
    {ok, _} = application:ensure_all_started(pooler),
    ok = application:set_env(imboy, sql_driver, pgsql),
    {ok, _} = pooler:new_pool(maps:merge(
        #{name => pgsql, init_count => 2, max_count => 6, queue_max => 20},
        #{start_mfa => {epgsql, connect, [ConnOpts]}}
    )),
    %% join_group → group_ds:join 走 imboy_cache（depcache 实例）
    {ok, _} = imboy_cache:start_link([{depcache_memory_max, 100}]),
    %% log 宏（?INFO_LOG/?ERROR_LOG）走 lager：无 handler 时静默即可
    _ = (catch application:ensure_all_started(lager)),
    ok = app_probes(Conn, Counters),
    ok = db_probes(Conn, Counters),
    cleanup(Conn),
    epgsql:close(Conn),
    io:format("~nPROBE-SUMMARY pass=~p~n", [counters:get(Counters, 1)]),
    halt(0).

conn_opts() ->
    %% codecs 与 config/sys.local pg_conf 同口径：无 rfc3339_bin 时
    %% timestamptz 参数编码崩溃会杀死连接（dead_connection）
    #{host => env("PGHOST", "127.0.0.1"),
      port => list_to_integer(env("PGPORT", "4323")),
      username => env("PGUSER", "imboy_user"),
      password => env("PGPASSWORD", ""),
      database => env("PGDATABASE", "scratch_gzapp_01"),
      ssl => false,
      timeout => 4000,
      codecs => [{epgsql_codec_rfc3339_bin, []}]}.

-define(ORG_A, 820000101).  %% 主 org：owner/admin/member + template WS（General+Announcements）
-define(ORG_B, 820000201).  %% 跨 org 探测 org（active，owner_b）
-define(ORG_C, 820000301).  %% archived org
-define(ORG_D, 820000401).  %% 无默认 WS org
-define(OWNER, 820000001).
-define(ADMIN, 820000002).
-define(MEMBER, 820000003).
-define(TARGET, 820000011).  %% join_by_code 用户
-define(TARGET2, 820000012). %% invitation accept 用户
-define(OWNER_B, 820000021).
-define(INV_ID, 820000501).

%%--------------------------------------------------------------------
%% P01-P16: 应用层命令真调
%%--------------------------------------------------------------------
app_probes(Conn, Counters) ->
    ok = fixture_base(Conn),
    ok = fixture_org(Conn, ?ORG_A, ?OWNER, [{admin, ?ADMIN}, {member, ?MEMBER}]),
    ok = fixture_org(Conn, ?ORG_B, ?OWNER_B, []),
    ok = fixture_org(Conn, ?ORG_C, ?OWNER, []),
    ok = fixture_org(Conn, ?ORG_D, ?OWNER, []),
    %% 主 org 建默认 WS（General 群 + Announcements 频道 + 默认关系 + owner 成员）
    {ok, Tpl, created} = workspace_ds:create_template(?OWNER, ?ORG_A, <<"GZ01 WS">>, <<>>),
    WsId = maps:get(workspace_id, Tpl),
    Gid = maps:get(group_id, Tpl),
    Cid = maps:get(channel_id, Tpl),
    true = is_integer(WsId) andalso WsId > 0 andalso Gid > 0 andalso Cid > 0,

    %% P01 create 成功：8 位 A-Z2-9；DB 行 active；expires_at ≈ now+7d
    {ok, #{code := Code1, expires_at := Exp1}} =
        organization_invite_code_app:create(?OWNER, ?ORG_A, #{}),
    true = is_valid_code(Code1),
    true = is_integer(Exp1) andalso (Exp1 - os:system_time(second)) > 6 * 24 * 3600,
    true = active_code_of(Conn, ?ORG_A) =:= Code1,
    pass(Counters, "P01 create success: 8-char A-Z2-9 code, active row, 7d ttl"),

    %% P02 admin 重新生成 = 旧码失效（重新生成即旧码 revoke）
    {ok, #{code := Code2}} = organization_invite_code_app:create(?ADMIN, ?ORG_A, #{}),
    true = Code2 =/= Code1,
    true = row_status_of_code(Conn, Code1) =:= <<"revoked">>,
    true = active_code_of(Conn, ?ORG_A) =:= Code2,
    pass(Counters, "P02 regenerate revokes old code: one active per org"),

    %% P03 治理门负例：member 403 / 非成员 403 / org 不存在 404
    {error, {403, _}} = organization_invite_code_app:create(?MEMBER, ?ORG_A, #{}),
    {error, {403, _}} = organization_invite_code_app:create(?TARGET, ?ORG_A, #{}),
    {error, {404, _}} = organization_invite_code_app:create(?OWNER, 820000999, #{}),
    pass(Counters, "P03 governance gate: member/stranger 403, missing org 404"),

    %% P04 archived org create 409；码保留给 P10 测 join 409（archived 后建码被拒，
    %% 故码必须在 archive 之前预置；撤销放行移到 P10 尾部验证）
    {ok, #{code := CodeC0}} = organization_invite_code_app:create(?OWNER, ?ORG_C, #{}),
    {ok, _} = x(Conn, [<<"UPDATE organization SET status='archived' WHERE id=">>,
        ib(?ORG_C)]),
    {error, {409, _}} = organization_invite_code_app:create(?OWNER, ?ORG_C, #{}),
    pass(Counters, "P04 archived org: create 409"),

    %% P05 join_by_code 全链：org member + ws member + General 群 + Announcements 订阅
    {ok, #{code := CodeA}} = organization_invite_code_app:create(?OWNER, ?ORG_A, #{}),
    {ok, joined, Sum5} = organization_invite_code_app:join_by_code(?TARGET, ?ORG_A, CodeA),
    true = maps:get(workspace_id, Sum5) =:= WsId,
    true = maps:get(group_id, Sum5) =:= Gid,
    true = maps:get(channel_id, Sum5) =:= Cid,
    true = org_member_active(Conn, ?ORG_A, ?TARGET, <<"member">>),
    true = ws_member_active(Conn, WsId, ?TARGET, <<"member">>),
    true = group_member_active(Conn, Gid, ?TARGET),
    true = channel_subscribed(Conn, Cid, ?TARGET),
    pass(Counters, "P05 join full chain: org member + ws member + General + Announcements"),

    %% P06 幂等重放：unchanged，四级行数不变
    {ok, unchanged, _} = organization_invite_code_app:join_by_code(?TARGET, ?ORG_A, CodeA),
    true = count_rows(Conn, <<"organization_member">>, ?ORG_A, ?TARGET) =:= 1,
    true = count_rows(Conn, <<"workspace_member">>, WsId, ?TARGET) =:= 1,
    true = count_rows(Conn, <<"group_member">>, Gid, ?TARGET) =:= 1,
    true = count_rows(Conn, <<"channel_subscription">>, Cid, ?TARGET) =:= 1,
    pass(Counters, "P06 replay join idempotent: unchanged, no duplicate rows"),

    %% P07 跨 org 输码 981 与不存在码输出完全一致（不泄露存在性）
    {error, E1} = organization_invite_code_app:join_by_code(?TARGET, ?ORG_B, CodeA),
    {error, E2} = organization_invite_code_app:join_by_code(?TARGET, ?ORG_B, <<"ZZZZ9999">>),
    true = (E1 =:= E2) andalso (element(1, E1) =:= 981),
    pass(Counters, "P07 cross-org probe: identical 981 as missing code"),

    %% P08 过期码 982（手工把 active 码置为已过期）
    {ok, _} = x(Conn, [
        <<"UPDATE organization_invite_code SET expires_at = CURRENT_TIMESTAMP - INTERVAL '1 second'">>,
        <<" WHERE organization_id=">>, ib(?ORG_A), <<" AND status='active'">>
    ]),
    {error, {982, _}} = organization_invite_code_app:join_by_code(?TARGET2, ?ORG_A, CodeA),
    true = not org_member_exists(Conn, ?ORG_A, ?TARGET2),
    pass(Counters, "P08 expired code 982, no membership written"),

    %% P09 revoke 后 981；幂等 revoked 0
    {ok, #{code := CodeR}} = organization_invite_code_app:create(?OWNER, ?ORG_A, #{}),
    {ok, #{revoked := 1}} = organization_invite_code_app:revoke(?ADMIN, ?ORG_A),
    {error, {981, _}} = organization_invite_code_app:join_by_code(?TARGET2, ?ORG_A, CodeR),
    {ok, #{revoked := 0}} = organization_invite_code_app:revoke(?ADMIN, ?ORG_A),
    pass(Counters, "P09 revoked code 981; revoke idempotent"),

    %% P10 archived org join 409（P04 预置的 active 码，org 已归档）；
    %% 归档组织允许撤销（收紧操作放行，在码仍在时验证）
    {error, {409, _}} = organization_invite_code_app:join_by_code(?TARGET, ?ORG_C, CodeC0),
    true = not org_member_exists(Conn, ?ORG_C, ?TARGET),
    {ok, #{revoked := 1}} = organization_invite_code_app:revoke(?OWNER, ?ORG_C),
    pass(Counters, "P10 archived org join rejected 409; revoke allowed"),

    %% P11 默认 WS archived → 980 整体回滚（org member 也不落）
    {ok, #{code := CodeW}} = organization_invite_code_app:create(?OWNER, ?ORG_A, #{}),
    {ok, _} = x(Conn, [<<"UPDATE workspace SET status='archived' WHERE id=">>, ib(WsId)]),
    {error, {980, _}} = organization_invite_code_app:join_by_code(?TARGET2, ?ORG_A, CodeW),
    true = not org_member_exists(Conn, ?ORG_A, ?TARGET2),
    {ok, _} = x(Conn, [<<"UPDATE workspace SET status='active' WHERE id=">>, ib(WsId)]),
    pass(Counters, "P11 default ws archived: 980, whole tx rolled back"),

    %% P12 无默认 WS org：join 只写 org member（不阻塞）
    {ok, #{code := CodeD}} = organization_invite_code_app:create(?OWNER, ?ORG_D, #{}),
    {ok, joined, Sum12} = organization_invite_code_app:join_by_code(?TARGET, ?ORG_D, CodeD),
    true = maps:get(workspace_id, Sum12) =:= none,
    true = maps:get(group_id, Sum12) =:= none,
    true = org_member_active(Conn, ?ORG_D, ?TARGET, <<"member">>),
    pass(Counters, "P12 no default ws: org member only, not blocked"),

    %% P13 invitation accept 编排链（真 hook 注入）：四级全落
    {ok, InvView} = organization_invitation_app:create(?OWNER, ?ORG_A, ?TARGET2,
        #{invitation_id => ?INV_ID}),
    Token13 = maps:get(token, InvView),
    {ok, AccView} = organization_invitation_app:accept(?TARGET2, ?ORG_A, Token13,
        #{membership_hook => fun organization_join_orchestrator:membership_hook/2}),
    true = maps:get(already_accepted, AccView) =:= false,
    true = org_member_active(Conn, ?ORG_A, ?TARGET2, <<"member">>),
    true = ws_member_active(Conn, WsId, ?TARGET2, <<"member">>),
    true = group_member_active(Conn, Gid, ?TARGET2),
    true = channel_subscribed(Conn, Cid, ?TARGET2),
    pass(Counters, "P13 invitation accept full orchestration: 4-level membership"),

    %% P14 accept 幂等重放：already_accepted=true，行数不变
    {ok, AccView2} = organization_invitation_app:accept(?TARGET2, ?ORG_A, Token13,
        #{membership_hook => fun organization_join_orchestrator:membership_hook/2}),
    true = maps:get(already_accepted, AccView2) =:= true,
    true = count_rows(Conn, <<"group_member">>, Gid, ?TARGET2) =:= 1,
    true = count_rows(Conn, <<"channel_subscription">>, Cid, ?TARGET2) =:= 1,
    pass(Counters, "P14 accept replay idempotent: no duplicate side effects"),

    %% P15 get 治理面：active 视图 / 无码 not_found / member 403 / archived 409
    {ok, #{code := Code15}} = organization_invite_code_app:create(?OWNER, ?ORG_A, #{}),
    {ok, View15} = organization_invite_code_app:get(?OWNER, ?ORG_A),
    true = maps:get(code, View15) =:= Code15,
    true = {error, not_found} =:= organization_invite_code_app:get(?OWNER_B, ?ORG_B),
    {error, {403, _}} = organization_invite_code_app:get(?MEMBER, ?ORG_A),
    {error, {409, _}} = organization_invite_code_app:get(?OWNER, ?ORG_C),
    pass(Counters, "P15 governance read: active view / not_found / 403 / archived 409"),

    %% P16 码归一：小写/带空白输入同码生效（uppercase+trim）
    {ok, #{code := Code16}} = organization_invite_code_app:create(?OWNER, ?ORG_D, #{}),
    Padded = <<"  ", (lower_bin(Code16))/binary, "  ">>,
    {ok, unchanged, _} = organization_invite_code_app:join_by_code(?TARGET, ?ORG_D, Padded),
    pass(Counters, "P16 code normalization: lowercase+whitespace accepted"),

    ok.

%%--------------------------------------------------------------------
%% A01-A02: DB 不变量
%%--------------------------------------------------------------------
db_probes(Conn, Counters) ->
    %% A01 一 org 至多一个 active 码（部分唯一索引拒绝第二条）
    Id1 = 820000901,
    {aborted, <<"23505">>} = tx(Conn, fun(C) ->
        {ok, _} = x(C, [
            <<"INSERT INTO organization_invite_code (id, organization_id, code, created_by, expires_at)">>,
            <<" VALUES (">>, ib(Id1), <<",">>, ib(?ORG_B), <<",'ZZAAZZ99',NULL,">>,
            <<" CURRENT_TIMESTAMP + INTERVAL '7 days')">>
        ]),
        {ok, _} = x(C, [
            <<"INSERT INTO organization_invite_code (id, organization_id, code, created_by, expires_at)">>,
            <<" VALUES (">>, ib(Id1 + 1), <<",">>, ib(?ORG_B), <<",'ZZAAZZ98',NULL,">>,
            <<" CURRENT_TIMESTAMP + INTERVAL '7 days')">>
        ]),
        commit
    end),
    %% A02 code 全局唯一（跨 org 同码同样拒绝）
    {aborted, <<"23505">>} = tx(Conn, fun(C) ->
        {ok, _} = x(C, [
            <<"INSERT INTO organization_invite_code (id, organization_id, code, created_by, expires_at)">>,
            <<" VALUES (">>, ib(Id1 + 2), <<",">>, ib(?ORG_D), <<",'ZZAAZZ99',NULL,">>,
            <<" CURRENT_TIMESTAMP + INTERVAL '7 days')">>
        ]),
        commit
    end),
    pass(Counters, "A01+A02 partial unique index: one active per org, global code unique"),
    ok.

%%--------------------------------------------------------------------
%% fixture / helpers
%%--------------------------------------------------------------------
ib(N) -> integer_to_binary(N).

is_valid_code(Code) when is_binary(Code) ->
    byte_size(Code) =:= 8 andalso code_charset_ok(binary_to_list(Code), 0);
is_valid_code(_) ->
    false.

code_charset_ok(_, 8) ->
    true;
code_charset_ok([C | Rest], N) ->
    ((C >= $A andalso C =/= $I andalso C =/= $O) orelse (C >= $2 andalso C =< $9))
        andalso code_charset_ok(Rest, N + 1);
code_charset_ok([], _) ->
    false.

lower_bin(B) ->
    << <<(case C >= $A andalso C =< $Z of true -> C + 32; false -> C end)>> || <<C>> <= B >>.

fixture_base(Conn) ->
    cleanup(Conn),
    {ok, _} = x(Conn, [
        <<"INSERT INTO \"user\" (id,password,account,reg_ip,reg_cosv,account_type)">>,
        <<" SELECT i, 'x', 'gz01-u' || i, '127.0.0.1', '', 0">>,
        <<" FROM generate_series(820000001, 820000040) AS i">>,
        <<" ON CONFLICT (id) DO NOTHING">>
    ]),
    ok.

fixture_org(Conn, OrgId, OwnerUid, Extra) ->
    %% owner member 行由 trg_organization_owner_member_sync 在 organization
    %% INSERT 时自动同步（00000127+）；fixture 只补 admin/member 附加成员，
    %% 不得显式插 owner 行（PK 冲突 + 不变量双防线）。
    {ok, _} = x(Conn, [
        <<"INSERT INTO organization (id,name,owner_id) VALUES (">>,
        ib(OrgId), <<",'gz01-org',">>, ib(OwnerUid), <<")">>
    ]),
    lists:foreach(
        fun({Role, Uid}) ->
            {ok, _} = x(Conn, member_insert_sql(OrgId, Uid, atom_to_binary(Role)))
        end,
        Extra),
    ok.

member_insert_sql(OrgId, Uid, Role) ->
    [
        <<"INSERT INTO organization_member">>,
        <<" (organization_id,user_id,role,status,joined_at,created_at,updated_at)">>,
        <<" VALUES (">>, ib(OrgId), <<",">>, ib(Uid), <<",'">>, Role,
        <<"','active',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP)">>
    ].

active_code_of(Conn, OrgId) ->
    {ok, _, [{Code}]} = q(Conn, [
        <<"SELECT code FROM organization_invite_code WHERE organization_id=">>,
        ib(OrgId), <<" AND status='active'">>
    ]),
    Code.

row_status_of_code(Conn, Code) ->
    {ok, _, [{St}]} = q(Conn, [
        <<"SELECT status FROM organization_invite_code WHERE code='">>,
        Code, <<"'">>
    ]),
    St.

org_member_active(Conn, OrgId, Uid, Role) ->
    {ok, _, [{N}]} = q(Conn, [
        <<"SELECT count(*) FROM organization_member WHERE organization_id=">>,
        ib(OrgId), <<" AND user_id=">>, ib(Uid),
        <<" AND role='">>, Role, <<"' AND status='active'">>
    ]),
    b2i(N) =:= 1.

org_member_exists(Conn, OrgId, Uid) ->
    {ok, _, [{N}]} = q(Conn, [
        <<"SELECT count(*) FROM organization_member WHERE organization_id=">>,
        ib(OrgId), <<" AND user_id=">>, ib(Uid)
    ]),
    b2i(N) > 0.

ws_member_active(Conn, WsId, Uid, Role) ->
    {ok, _, [{N}]} = q(Conn, [
        <<"SELECT count(*) FROM workspace_member WHERE workspace_id=">>,
        ib(WsId), <<" AND user_id=">>, ib(Uid),
        <<" AND role='">>, Role, <<"' AND status='active'">>
    ]),
    b2i(N) =:= 1.

group_member_active(Conn, Gid, Uid) ->
    {ok, _, [{N}]} = q(Conn, [
        <<"SELECT count(*) FROM group_member WHERE group_id=">>,
        ib(Gid), <<" AND user_id=">>, ib(Uid), <<" AND status=1">>
    ]),
    b2i(N) =:= 1.

channel_subscribed(Conn, Cid, Uid) ->
    {ok, _, [{N}]} = q(Conn, [
        <<"SELECT count(*) FROM channel_subscription WHERE channel_id=">>,
        ib(Cid), <<" AND user_id=">>, ib(Uid), <<" AND status=1">>
    ]),
    b2i(N) =:= 1.

count_rows(Conn, Table, ScopeId, Uid) ->
    ScopeCol =
        case Table of
            <<"organization_member">> -> <<"organization_id">>;
            <<"workspace_member">> -> <<"workspace_id">>;
            <<"group_member">> -> <<"group_id">>;
            <<"channel_subscription">> -> <<"channel_id">>
        end,
    {ok, _, [{N}]} = q(Conn, [
        <<"SELECT count(*) FROM ">>, Table, <<" WHERE ">>,
        ScopeCol, <<"=">>, ib(ScopeId),
        <<" AND user_id=">>, ib(Uid)
    ]),
    b2i(N).

cleanup(Conn) ->
    lists:foreach(
        fun(Sql) ->
            _ = (catch x(Conn, Sql))
        end,
        cleanup_sqls()),
    ok.

cleanup_sqls() ->
    %% 顺序敏感（重跑安全）：
    %%   * fk_odw_workspace 是 RESTRICT：organization_default_workspace 必须
    %%     先于 workspace 删除；
    %%   * 00000127 owner 不变量触发器（DEFERRED）要求 org 删除前恰一个
    %%     active owner —— organization_member 禁止直删（先删 owner 即炸）。
    %%     先删 organization 行，member 随 CASCADE 删除（触发器对 org 已消失
    %%     的场景放行：v_owner_id IS NULL → RETURN NULL）。
    [<<"DELETE FROM channel_subscription WHERE user_id BETWEEN 820000001 AND 820000040">>,
     <<"DELETE FROM channel_admin WHERE user_id BETWEEN 820000001 AND 820000040">>,
     <<"DELETE FROM group_member WHERE user_id BETWEEN 820000001 AND 820000040">>,
     <<"DELETE FROM workspace_member WHERE user_id BETWEEN 820000001 AND 820000040">>,
     <<"DELETE FROM channel WHERE workspace_id IN (SELECT id FROM workspace WHERE organization_id BETWEEN 820000101 AND 820000999)">>,
     <<"DELETE FROM \"group\" WHERE workspace_id IN (SELECT id FROM workspace WHERE organization_id BETWEEN 820000101 AND 820000999)">>,
     <<"DELETE FROM organization_default_workspace WHERE organization_id BETWEEN 820000101 AND 820000999">>,
     <<"DELETE FROM workspace WHERE organization_id BETWEEN 820000101 AND 820000999">>,
     <<"DELETE FROM organization_invite_code WHERE organization_id BETWEEN 820000101 AND 820000999">>,
     <<"DELETE FROM organization_invitation WHERE organization_id BETWEEN 820000101 AND 820000999">>,
     <<"DELETE FROM organization WHERE id BETWEEN 820000101 AND 820000999">>,
     <<"DELETE FROM \"user\" WHERE id BETWEEN 820000001 AND 820000040">>].

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
%% 返回 committed 或 {aborted, SqlStateBinary}（A 探针断言 23505 用）。
tx(Conn, Fun) ->
    {ok, _, _} = epgsql:squery(Conn, "BEGIN"),
    Exec =
        try
            {ok, Fun(Conn)}
        catch
            _Class:Reason -> {exception, Reason}
        end,
    case Exec of
        {ok, commit} ->
            case epgsql:squery(Conn, "COMMIT") of
                {ok, _, _} -> committed;
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

env(K, D) -> case os:getenv(K) of false -> D; V -> V end.
