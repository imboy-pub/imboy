#!/usr/bin/env escript
%%! -noshell
%% ===================================================================
%% GZAPP-03 企业群/频道 Organization 授权行为矩阵 harness（真 PostgreSQL）
%% -------------------------------------------------------------------
%% 用法:
%%   PGHOST=127.0.0.1 PGPORT=4323 PGUSER=imboy_user PGPASSWORD=*** \
%%   PGDATABASE=scratch_gzapp_03 IMBOY_DIR=<worktree> \
%%   test/lib/organization/group_org_authority_behavior_harness.escript
%%
%% 前置: 该库已应用迁移链至 00000135（scripts/drill_migrate.escript up）。
%% 探针:
%%   P01 群主路径零行为变化（群主解散成功）
%%   P02 org owner 经第二授权源解散非本人持有的 ws 域群成功
%%   P03 解散后 group 行删除，但 msg_c2g 消息行保留（D07 保留策略）
%%   P04 解散写入 group_log type=101 审计，option_uid = 实际操作者（可追溯）
%%   P05 非群主且非 org 管理者（ws 成员）拒绝，group 未被删除
%%   P06 跨 org 管理者拒绝（不泄露存在性），group 未被删除
%%   P07 personal 域群 + org owner 拒绝（个人域语义不变）
%% fixture 全部使用 82e8/82e9 段 bigint id；跑完清理 fixture 行、库保留。
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
        filename:join([RepoDir, "deps/jsone/ebin"]),
        %% uid 库：elib_id:gen/1 → uid:g/0 + uid:encode64/1（msg_s2c_ds 建 WS 帧 id）
        filename:join([RepoDir, "deps/uid/ebin"])
    ]),
    ok = elib_tsid:init(#{
        dc_id => 0, node_id => 9, dc_bits => 3, names => imboy_app:tsid_generator_names()
    }),
    ConnOpts = conn_opts(),
    {ok, Conn} = epgsql:connect(ConnOpts),
    Counters = counters:new(1, []),
    {ok, _} = application:ensure_all_started(pooler),
    ok = application:set_env(imboy, sql_driver, pgsql),
    {ok, _} = pooler:new_pool(maps:merge(
        #{name => pgsql, init_count => 2, max_count => 6, queue_max => 20},
        #{start_mfa => {epgsql, connect, [ConnOpts]}}
    )),
    {ok, _} = imboy_cache:start_link([{depcache_memory_max, 100}]),
    _ = (catch application:ensure_all_started(lager)),
    ok = setup(Conn),
    ok = probes(Conn, Counters),
    cleanup(Conn),
    epgsql:close(Conn),
    io:format("~nPROBE-SUMMARY pass=~p~n", [counters:get(Counters, 1)]),
    halt(0).

conn_opts() ->
    #{
        host => env("PGHOST", "127.0.0.1"),
        port => list_to_integer(env("PGPORT", "4323")),
        username => env("PGUSER", "imboy_user"),
        password => env("PGPASSWORD", ""),
        database => env("PGDATABASE", "scratch_gzapp_03"),
        timeout => 4000,
        ssl => false,
        codecs => [{epgsql_codec_rfc3339_bin, []}]
    }.

env(Key, Default) ->
    case os:getenv(Key) of
        false -> Default;
        "" -> Default;
        V -> V
    end.

%% ===================================================================
%% Fixture：两个 Organization（各含 1 ws），单群多消息
%% ===================================================================
-define(ORG1, 16#82E80001).
-define(ORG2, 16#82E80002).
-define(WS1, 16#82E80011).
-define(WS2, 16#82E80012).
-define(OWNER1, 16#82E80101).   %% org1 owner（同时是 ws1 owner）
-define(OWNER2, 16#82E80102).   %% org2 owner
-define(MEMBER1, 16#82E80103).  %% org1 普通成员 + ws1 成员
-define(OUTSIDER, 16#82E80104). %% 与 org1/org2 无任何关系
-define(GROUP_OWNED, 16#82E80201).  %% 群主 = MEMBER1（非 org 管理者）
-define(GROUP_OWNER_DISSOLVE, 16#82E80202).
-define(GROUP_OUTSIDER, 16#82E80203).
-define(GROUP_CROSSORG, 16#82E80204).
-define(GROUP_PERSONAL, 16#82E80205).
-define(FORBIDDEN, <<"只有拥有者才能够解散该群，或者群已解散"/utf8>>).

setup(Conn) ->
    %% user 先行：fk_organization_owner / group_member 等 FK 需要真实 user 行
    exec(Conn, ~s{INSERT INTO "user" (id, password, account, reg_ip, reg_cosv, account_type)
        SELECT i, 'x', 'gzapp03-u' || i, '127.0.0.1', '', 0
        FROM unnest($1::bigint[]) AS i ON CONFLICT (id) DO NOTHING},
        [[?OWNER1, ?OWNER2, ?MEMBER1, ?OUTSIDER]]),
    exec(Conn, ~s|INSERT INTO organization (id, name, owner_id, status, branding, settings)
        VALUES ($1,'GZAPP03-Org1',$2,'active','{}','{}'), ($3,'GZAPP03-Org2',$4,'active','{}','{}')
        ON CONFLICT (id) DO NOTHING|, [?ORG1, ?OWNER1, ?ORG2, ?OWNER2]),
    exec(Conn, ~s|INSERT INTO organization_member (organization_id, user_id, role, status)
        VALUES ($1,$2,'owner','active'), ($1,$3,'member','active'), ($4,$5,'owner','active')
        ON CONFLICT DO NOTHING|, [?ORG1, ?OWNER1, ?MEMBER1, ?ORG2, ?OWNER2]),
    exec(Conn, ~s|INSERT INTO workspace (id, name, owner_id, organization_id, status, type, branding)
        VALUES ($1,'GZAPP03-WS1',$2,$3,'active','project','{}'),
               ($4,'GZAPP03-WS2',$5,$6,'active','project','{}')
        ON CONFLICT (id) DO NOTHING|, [?WS1, ?OWNER1, ?ORG1, ?WS2, ?OWNER2, ?ORG2]),
    exec(Conn, ~s|INSERT INTO workspace_member (workspace_id, user_id, role, status)
        VALUES ($1,$2,'owner','active'), ($1,$3,'member','active'), ($4,$5,'owner','active')
        ON CONFLICT DO NOTHING|, [?WS1, ?OWNER1, ?MEMBER1, ?WS2, ?OWNER2]),
    %% 四个群：本群主持有 / org owner 解散目标 / 越权目标 / 跨 org 目标；+ 1 个 personal 群
    Ins = fun(Gid, OwnerUid, Scope, WsId) ->
        exec(
            Conn,
            ~s|INSERT INTO "group" (id, owner_uid, creator_uid, title, scope, workspace_id, status, e2ee_mode)
               VALUES ($1,$2,$2,'GZAPP03-G',$3,$4,1,0) ON CONFLICT (id) DO NOTHING|,
            [Gid, OwnerUid, Scope, WsId]
        )
    end,
    Ins(?GROUP_OWNED, ?MEMBER1, <<"workspace">>, ?WS1),
    Ins(?GROUP_OWNER_DISSOLVE, ?MEMBER1, <<"workspace">>, ?WS1),
    Ins(?GROUP_OUTSIDER, ?OWNER1, <<"workspace">>, ?WS1),
    Ins(?GROUP_CROSSORG, ?OWNER1, <<"workspace">>, ?WS1),
    Ins(?GROUP_PERSONAL, ?MEMBER1, <<"personal">>, null),
    %% 群成员（⊆ ws 成员：触发器在提交时校验）
    [exec(Conn, ~s|INSERT INTO group_member (id, group_id, user_id, status) VALUES ($1,$2,$3,1)
        ON CONFLICT DO NOTHING|, [Gid * 10 + K, Gid, Uid]) ||
        {Gid, Uid, K} <- [
            {?GROUP_OWNED, ?MEMBER1, 1},
            {?GROUP_OWNED, ?OWNER1, 2},
            {?GROUP_OWNER_DISSOLVE, ?MEMBER1, 1},
            {?GROUP_OWNER_DISSOLVE, ?OWNER1, 2},
            {?GROUP_OUTSIDER, ?MEMBER1, 1},
            {?GROUP_OUTSIDER, ?OWNER1, 2},
            {?GROUP_CROSSORG, ?OWNER1, 1},
            {?GROUP_PERSONAL, ?MEMBER1, 1}
        ]],
    %% 目标群 2 条消息（D07：解散不删消息）
    exec(Conn, ~s|INSERT INTO msg_c2g (id, topic_id, from_id, to_id, msg_id, msg_type, payload)
        VALUES ($1,$2,$3,$4,'gzapp03-m1','text','{"t":"hi"}'::jsonb),
               ($5,$2,$6,$4,'gzapp03-m2','text','{"t":"ho"}'::jsonb)
        ON CONFLICT DO NOTHING|, [?GROUP_OWNER_DISSOLVE * 100 + 1, ?GROUP_OWNER_DISSOLVE, ?MEMBER1, ?GROUP_OWNER_DISSOLVE * 100 + 2, ?OWNER1, ?OWNER1]),
    ok.

%% ===================================================================
%% 探针
%% ===================================================================
probes(Conn, Counters) ->
    %% P01：群主路径（零行为变化）
    pass(
        Counters,
        <<"P01 群主解散自有群成功"/utf8>>,
        group_logic:dissolve(?MEMBER1, ?GROUP_OWNED) =:= ok
    ),
    %% P02：org owner（非群主、非本群成员关系无关）经第二授权源解散
    R2 = group_logic:dissolve(?OWNER1, ?GROUP_OWNER_DISSOLVE),
    pass(
        Counters,
        <<"P02 org owner 解散非本人持有 ws 群成功"/utf8>>, R2 =:= ok),
    %% P03：消息保留
    {ok, _, Rows3} = epgsql:equery(
        Conn, ~s|SELECT count(*) FROM msg_c2g WHERE topic_id = $1|, [?GROUP_OWNER_DISSOLVE]
    ),
    MsgCount = element(1, hd(Rows3)),
    pass(
        Counters,
        <<"P03 解散后 msg_c2g 消息保留（2 条）"/utf8>>, MsgCount =:= 2),
    %% P04：审计 option_uid = 实际操作者
    {ok, _, Rows4} = epgsql:equery(
        Conn,
        ~s|SELECT count(*) FROM group_log WHERE group_id = $1 AND type = 101 AND option_uid = $2|,
        [?GROUP_OWNER_DISSOLVE, ?OWNER1]
    ),
    LogCount = element(1, hd(Rows4)),
    pass(
        Counters,
        <<"P04 group_log type=101 审计含实际操作者"/utf8>>, LogCount >= 1),
    %% P05：ws 成员（非群主非管理者）拒绝
    R5 = group_logic:dissolve(?MEMBER1, ?GROUP_OUTSIDER),
    pass(
        Counters,
        <<"P05 非群主非管理者拒绝（既有文案）"/utf8>>, R5 =:= {error, ?FORBIDDEN}),
    pass(
        Counters,
        <<"P05b 拒绝后 group 行仍存在"/utf8>>,
        group_exists(Conn, ?GROUP_OUTSIDER)
    ),
    %% P06：跨 org 管理者拒绝（org2 owner 觊觎 org1 的群）
    R6 = group_logic:dissolve(?OWNER2, ?GROUP_CROSSORG),
    pass(
        Counters,
        <<"P06 跨 org 管理者拒绝（不泄露存在性）"/utf8>>, R6 =:= {error, ?FORBIDDEN}),
    pass(
        Counters,
        <<"P06b 拒绝后 group 行仍存在"/utf8>>,
        group_exists(Conn, ?GROUP_CROSSORG)
    ),
    %% P07：personal 域群语义不变（org owner 无授权）
    R7 = group_logic:dissolve(?OWNER1, ?GROUP_PERSONAL),
    pass(
        Counters,
        <<"P07 personal 域群 org owner 拒绝（个人域不变）"/utf8>>, R7 =:= {error, ?FORBIDDEN}),
    pass(
        Counters,
        <<"P07b personal group 行仍存在"/utf8>>,
        group_exists(Conn, ?GROUP_PERSONAL)
    ),
    %% P08：无任何关系的外部用户拒绝
    R8 = group_logic:dissolve(?OUTSIDER, ?GROUP_OUTSIDER),
    pass(
        Counters,
        <<"P08 外部用户拒绝"/utf8>>, R8 =:= {error, ?FORBIDDEN}),
    ok.

group_exists(Conn, Gid) ->
    {ok, _, Rows} = epgsql:equery(Conn, ~s|SELECT count(*) FROM "group" WHERE id = $1|, [Gid]),
    element(1, hd(Rows)) =:= 1.

%% ===================================================================
%% 工具
%% ===================================================================
exec(Conn, Sql, Params) ->
    case epgsql:equery(Conn, Sql, Params) of
        {ok, _C, _R} ->
            ok;
        {ok, _Count} ->
            ok;
        {error, Reason} ->
            io:format("FIXTURE-ERROR: ~p~nSQL: ~s~n", [Reason, Sql]),
            halt(3)
    end.

pass(Counters, Label, true) ->
    counters:add(Counters, 1, 1),
    io:format("  [PASS] ~ts~n", [Label]),
    ok;
pass(_Counters, Label, false) ->
    io:format("  [FAIL] ~ts~n", [Label]),
    halt(2).

cleanup(Conn) ->
    Groups = [
        ?GROUP_OWNED,
        ?GROUP_OWNER_DISSOLVE,
        ?GROUP_OUTSIDER,
        ?GROUP_CROSSORG,
        ?GROUP_PERSONAL
    ],
    %% 顺序敏感：
    %%   * organization_member 不直删——00000127 owner 不变量触发器（DEFERRED）
    %%     要求删除时仍「恰一个 active owner」，先删 organization 让其随 CASCADE
    %%     消失（触发器对 org 已消失场景放行，A1 harness 同款结论）；
    %%   * workspace 先于 organization（FK RESTRICT）。
    _ = epgsql:equery(Conn, ~s|DELETE FROM group_log WHERE group_id = ANY($1)|, [Groups]),
    _ = epgsql:equery(Conn, ~s|DELETE FROM msg_c2g WHERE id = ANY($1)|, [
        [?GROUP_OWNER_DISSOLVE * 100 + 1, ?GROUP_OWNER_DISSOLVE * 100 + 2]
    ]),
    _ = epgsql:equery(Conn, ~s|DELETE FROM group_member WHERE group_id = ANY($1)|, [Groups]),
    _ = epgsql:equery(Conn, ~s|DELETE FROM "group" WHERE id = ANY($1)|, [Groups]),
    _ = epgsql:equery(Conn, ~s|DELETE FROM workspace_member WHERE workspace_id = ANY($1)|, [
        [?WS1, ?WS2]
    ]),
    _ = epgsql:equery(Conn, ~s|DELETE FROM workspace WHERE id = ANY($1)|, [[?WS1, ?WS2]]),
    _ = epgsql:equery(Conn, ~s|DELETE FROM organization WHERE id = ANY($1)|, [[?ORG1, ?ORG2]]),
    _ = epgsql:equery(Conn, ~s|DELETE FROM organization_member WHERE organization_id = ANY($1)|, [
        [?ORG1, ?ORG2]
    ]),
    _ = epgsql:equery(Conn, ~s|DELETE FROM "user" WHERE id = ANY($1)|, [
        [?OWNER1, ?OWNER2, ?MEMBER1, ?OUTSIDER]
    ]),
    ok.
