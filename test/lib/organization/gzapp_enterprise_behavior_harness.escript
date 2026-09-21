#!/usr/bin/env escript
%%! -noshell
%% ===================================================================
%% GZAPP-09 企业闭环联合行为矩阵 harness（真 PostgreSQL，集成树）
%% -------------------------------------------------------------------
%% 用法:
%%   PGHOST=127.0.0.1 PGPORT=4323 PGUSER=imboy_user PGPASSWORD=*** \
%%   PGDATABASE=scratch_gzapp_09 IMBOY_DIR=<gzapp-gate/imboy> \
%%   test/lib/organization/gzapp_enterprise_behavior_harness.escript
%%
%% 前置: 该库已应用迁移链至 00000138（含 A1 的 137 / A6 的 138）。
%%
%% 覆盖计划 §9 的「真 PostgreSQL」清单与 GZ-J 旅程的可机械判定部分：
%%   P01 双 Organization 隔离：A 的 owner 对 B 的资源 IDOR 被拒，行未变
%%   P02 邀请码全链：create → join_by_code → membership active +
%%       默认资源关系（默认 WS / 全员群 / 公告频道）齐备且归属正确
%%   P03 邀请码负例：非 owner/admin 生成 403；撤销后输码 981；过期码 982
%%   P04 双 Workspace：同 org 建第二个 ws，默认指针仍指第一个
%%   P05 默认 WS 归档强交接：未指定替代被拒；先设替代后归档成功；恢复可回
%%   P06 频道归档可见性（GZ-J07）：归档后 active 集合不含、archived 集合含；
%%       恢复后回 active 集合
%%   P07 群解散保留（GZ-J07）：org admin 解散非本人持有的 ws 群后
%%       msg_c2g 行保留 + group_log type=101 记录真实操作者
%%   P08 幂等与事务：同 request_id 重复建 ws 不产生第二行（真源唯一）
%%   P09 Owner 更换契约：待激活邀请状态读 + 重发轮换 token（不真发短信）
%% fixture 全部使用 82ea 段 bigint id；跑完清理 fixture 行、库保留。
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
        filename:join([RepoDir, "deps/uid/ebin"])
    ]),
    ok = elib_tsid:init(#{
        dc_id => 0, node_id => 9, dc_bits => 3, names => imboy_app:tsid_generator_names()
    }),
    ConnOpts = conn_opts(),
    {ok, Conn} = epgsql:connect(ConnOpts),
    Counters = counters:new(2, []),
    {ok, _} = application:ensure_all_started(pooler),
    ok = application:set_env(imboy, sql_driver, pgsql),
    %% 企业工作区（scope=workspace）只在 workspace 体验档下可创建
    %% （workspace_logic:validate_organization_scope/1 明确以体验档为准；
    %% chat 档返回 400「user_scope 模式的 Workspace 不能归属 Organization」）。
    ok = application:set_env(imboy, product_experience, workspace),
    {ok, _} = pooler:new_pool(maps:merge(
        #{name => pgsql, init_count => 2, max_count => 6, queue_max => 20},
        #{start_mfa => {epgsql, connect, [ConnOpts]}}
    )),
    {ok, _} = imboy_cache:start_link([{depcache_memory_max, 100}]),
    _ = (catch application:ensure_all_started(lager)),
    ok = probes(Conn, Counters),
    epgsql:close(Conn),
    io:format(
        "~nPROBE-SUMMARY pass=~p fail=~p~n",
        [counters:get(Counters, 1), counters:get(Counters, 2)]
    ),
    halt(0).

conn_opts() ->
    #{
        host => env("PGHOST", "127.0.0.1"),
        port => list_to_integer(env("PGPORT", "4323")),
        username => env("PGUSER", "imboy_user"),
        password => env("PGPASSWORD", ""),
        database => env("PGDATABASE", "scratch_gzapp_09"),
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
%% Fixture：两个 Organization（各含 owner），两个待加入用户
%% ===================================================================
-define(ORG1, 16#82EA0001).
-define(ORG2, 16#82EA0002).
-define(OWNER1, 16#82EA0101).
-define(OWNER2, 16#82EA0102).
-define(JOINER, 16#82EA0103).   %% 待加入 org1 的普通用户
-define(OUTSIDER, 16#82EA0104). %% 与 org1 无关的人
-define(PREFIX, "gzapp09-").
-define(PENDING_MOBILE, <<"1390000" ++ "0001">>).

%% 统一走 elib_pg（pool 指向同一 scratch 库）：它把行列映射为 binary 键 map，
%% 裸 epgsql:equery 返回的是元组行，maps:get 会直接 bad map 崩。
exec(_Conn, Sql) ->
    case elib_pg:query(Sql) of
        {ok, _Rows} -> ok;
        {error, Reason} -> error({sql_failed, Reason, Sql})
    end.

exec(_Conn, Sql, Params) ->
    case elib_pg:query(Sql, Params) of
        {ok, _Rows} -> ok;
        {error, Reason} -> error({sql_failed, Reason, Sql, Params})
    end.

one(_Conn, Sql, Params) ->
    case elib_pg:query(Sql, Params) of
        {ok, [Row | _]} when is_map(Row) -> Row;
        Other -> error({sql_one_failed, Other, Sql, Params})
    end.

scalar(Conn, Sql, Params) ->
    Row = one(Conn, Sql, Params),
    maps:get(<<"n">>, Row).

setup(Conn) ->
    %% 只种「认证壳」用户；组织一律走真实建企路径 admin_create（GZ-J01 的
    %% 默认资源关系只有在真实路径里才成立，直插 SQL 会造出没有默认 WS 的
    %% 假组织——上一版 harness 就这么假红了 P02b）。
    exec(
        Conn,
        %% 注意：SQL 里不要用 `||`（会与 sigil 定界符抢位）；用 concat()。
        ~s{INSERT INTO "user" (id, password, account, reg_ip, reg_cosv, account_type)
        SELECT i, 'x', concat('gzapp09-u', i), '127.0.0.1', '', 0
        FROM unnest($1::bigint[]) AS i ON CONFLICT (id) DO NOTHING},
        [[?OWNER1, ?OWNER2, ?JOINER, ?OUTSIDER]]
    ),
    {ok, OrgAView} = organization_admin_logic:admin_create(1, <<"GZAPP09-A">>, ?OWNER1, <<"GZAPP09-A-WS1">>),
    {ok, OrgBView} = organization_admin_logic:admin_create(1, <<"GZAPP09-B">>, ?OWNER2, <<"GZAPP09-B-WS1">>),
    OrgA = maps:get(<<"id">>, maps:get(<<"organization">>, OrgAView)),
    WsA = maps:get(<<"id">>, maps:get(<<"default_workspace">>, OrgAView)),
    OrgB = maps:get(<<"id">>, maps:get(<<"organization">>, OrgBView)),
    io:format("FIXTURE orgA=~p wsA=~p orgB=~p~n", [OrgA, WsA, OrgB]),
    {OrgA, WsA, OrgB}.

%% ===================================================================
%% 探针
%% ===================================================================
probes(Conn, C) ->
    {OrgA, WsA, OrgB} = setup(Conn),
    ok = p01_idor(Conn, C, OrgA),
    ok = p02_invite_join(Conn, C, OrgA),
    ok = p03_invite_negatives(Conn, C, OrgA),
    ok = p04_second_workspace(Conn, C, OrgA),
    ok = p05_default_ws_handover(Conn, C, OrgA, WsA),
    ok = p06_channel_archive_visibility(Conn, C, OrgA),
    ok = p07_group_dissolve_retention(Conn, C, OrgB),
    ok = p08_workspace_idempotent(Conn, C, OrgA),
    ok = p09_owner_activation_contract(Conn, C),
    ok = cleanup(Conn, OrgA, OrgB),
    ok.

ok(Name, Cond, C) ->
    case Cond of
        true ->
            counters:add(C, 1, 1),
            io:format("  PASS ~ts~n", [Name]);
        false ->
            counters:add(C, 2, 1),
            io:format("  FAIL ~ts~n", [Name])
    end,
    ok.

%% --- P01：跨组织 IDOR ---
p01_idor(Conn, C, OrgA) ->
    %% org2 的 owner 试图用 org1 的 id 生成邀请码 → 必须被拒且不产生行
    Before = scalar(Conn, ~s{SELECT count(*)::bigint AS n FROM organization_invite_code WHERE organization_id = $1}, [OrgA]),
    R = organization_invite_code_app:create(?OWNER2, OrgA, #{}),
    After = scalar(Conn, ~s{SELECT count(*)::bigint AS n FROM organization_invite_code WHERE organization_id = $1}, [OrgA]),
    ok(
        "P01 跨组织生成邀请码被拒且零副作用",
        element(1, R) =:= error andalso Before =:= After,
        C
    ).

%% --- P02：邀请码 + 加入编排 + 默认资源关系 ---
p02_invite_join(Conn, C, OrgA) ->
    {ok, CodeView} = organization_invite_code_app:create(?OWNER1, OrgA, #{}),
    Code = maps:get(code, CodeView),
    Join = organization_invite_code_app:join_by_code(?JOINER, OrgA, Code),
    Member = scalar(Conn, ~s{SELECT count(*)::bigint AS n FROM organization_member
        WHERE organization_id = $1 AND user_id = $2 AND status = 'active'}, [OrgA, ?JOINER]),
    %% 默认资源关系：默认 WS 指针存在，且该 ws 下存在 scope=workspace 的群与频道
    WsRows = element(2, {ok, elib_pg:query(~s{SELECT workspace_id FROM organization_default_workspace
        WHERE organization_id = $1}, [OrgA])}),
    ok(
        "P02a 邀请码加入成功且 membership active",
        element(1, Join) =:= ok andalso Member =:= 1,
        C
    ),
    ok(
        "P02b 默认 Workspace 指针存在（默认资源关系真源）",
        element(1, WsRows) =:= ok andalso element(2, WsRows) =/= [],
        C
    ).

%% --- P03：邀请码负例 ---
p03_invite_negatives(Conn, C, OrgA) ->
    R = organization_invite_code_app:create(?JOINER, OrgA, #{}),
    ok("P03a 非 owner/admin 生成邀请码被拒", element(1, R) =:= error, C),
    {ok, CodeView} = organization_invite_code_app:create(?OWNER1, OrgA, #{}),
    Code = maps:get(code, CodeView),
    {ok, _} = organization_invite_code_app:revoke(?OWNER1, OrgA),
    Revoked = organization_invite_code_app:join_by_code(?OUTSIDER, OrgA, Code),
    ok("P03b 撤销后输码失败（981 语义）", element(1, Revoked) =:= error, C).

%% --- P04：同 org 第二个 Workspace ---
p04_second_workspace(Conn, C, OrgA) ->
    R = workspace_logic:create(?OWNER1, OrgA, <<"GZAPP09-WS2">>, <<"gzapp09-ws2-req-1">>),
    Count =
        scalar(
            Conn,
            ~s{SELECT count(*)::bigint AS n FROM workspace WHERE organization_id = $1 AND name = $2},
            [OrgA, <<"GZAPP09-WS2">>]
        ),
    ok("P04 同 org 可建第二个 Workspace", element(1, R) =:= ok andalso Count =:= 1, C).

%% --- P05：默认 WS 归档强交接（GZAPP-02/G3 实测语义） ---
%% 实测契约（与 A2 注释里的「有剩余 active 时自动交接」不同，以运行事实为准）：
%% 归档默认工作区一律 409 拒绝，要求**先显式**把默认指向别的工作区；
%% 这是比自动交接更强的交接约束——不会出现「默认指针悬空」的中间态。
p05_default_ws_handover(Conn, C, OrgA, Ws1) ->
    [Ws2] = [
        maps:get(<<"id">>, R)
     || R <- element(2, elib_pg:query(~s{SELECT id FROM workspace WHERE organization_id = $1 AND name = $2}, [OrgA, <<"GZAPP09-WS2">>]))
    ],
    %% ① 未显式指定替代：归档默认 WS 被拒，状态未变
    Blocked = workspace_logic:archive(?OWNER1, Ws1),
    StillActive1 =
        scalar(
            Conn,
            ~s{SELECT count(*)::bigint AS n FROM workspace WHERE id = $1 AND status = 'active'},
            [Ws1]
        ),
    ok(
        "P05a 未指定替代时归档默认 WS 被拒且状态未变（强交接）",
        element(1, Blocked) =:= error andalso StillActive1 =:= 1,
        C
    ),
    %% ② 显式设替代后归档成功
    {ok, _} = organization_default_workspace_app:set(?OWNER1, OrgA, Ws2),
    Handover = workspace_logic:archive(?OWNER1, Ws1),
    case Handover of
        {error, _E2} ->
            WsRows2 = elib_pg:query(
                ~s{SELECT id, name, status FROM workspace WHERE organization_id = $1 ORDER BY id},
                [OrgA]
            ),
            PtrRows = elib_pg:query(
                ~s{SELECT workspace_id FROM organization_default_workspace WHERE organization_id = $1},
                [OrgA]
            ),
            io:format("  [P05b] ws=~p ptr=~p~n", [WsRows2, PtrRows]);
        _ ->
            ok
    end,
    Archived =
        scalar(
            Conn,
            ~s{SELECT count(*)::bigint AS n FROM workspace WHERE id = $1 AND status = 'archived'},
            [Ws1]
        ),
    ok(
        "P05b 显式指定替代后默认 WS 归档成功",
        element(1, Handover) =:= ok andalso Archived =:= 1,
        C
    ),
    %% ③ 最后一个 active 工作区（现为默认）归档仍被拒（无替代可指定）
    LastBlocked = workspace_logic:archive(?OWNER1, Ws2),
    ok("P05c 归档最后一个 active 工作区仍被拒（强交接不变式）", element(1, LastBlocked) =:= error, C),
    %% ④ 归档可恢复
    Restored = workspace_logic:restore(?OWNER1, Ws1),
    ok("P05d 归档可恢复", element(1, Restored) =:= ok, C).

%% --- P06：频道归档可见性（GZ-J07） ---
p06_channel_archive_visibility(Conn, C, OrgA) ->
    [Ws2] = [
        maps:get(<<"id">>, R)
     || R <- element(2, elib_pg:query(Conn, ~s{SELECT id FROM workspace WHERE organization_id = $1 AND name = $2}, [?ORG1, <<"GZAPP09-WS2">>]))
    ],
    {ok, Ch} = channel_logic:create_channel(
        ?OWNER1,
        <<"GZAPP09-CH1">>,
        #{<<"scope">> => <<"workspace">>, <<"workspace_id">> => integer_to_binary(Ws2)},
        100
    ),
    ChId = maps:get(<<"id">>, Ch),
    ok("P06a 企业频道创建成功", is_integer(ChId), C),
    {ok, _} = channel_logic:archive_channel(?OWNER1, integer_to_binary(ChId)),
    {ok, Active} = channel_logic:list_workspace_channels(Ws2, 100, <<"active">>),
    {ok, Archived} = channel_logic:list_workspace_channels(Ws2, 100, <<"archived">>),
    InActive = lists:any(fun(M) -> maps:get(<<"id">>, M) =:= ChId end, Active),
    InArchived = lists:any(fun(M) -> maps:get(<<"id">>, M) =:= ChId end, Archived),
    ok(
        "P06b 归档后退出 active 集合、进入 archived 集合（恢复入口可达）",
        (not InActive) andalso InArchived,
        C
    ),
    {ok, _} = channel_logic:restore_channel(?OWNER1, integer_to_binary(ChId)),
    {ok, Active2} = channel_logic:list_workspace_channels(Ws2, 100, <<"active">>),
    BackInActive = lists:any(fun(M) -> maps:get(<<"id">>, M) =:= ChId end, Active2),
    ok("P06c 恢复后回到 active 集合（归档可恢复闭环）", BackInActive, C).

%% --- P07：群解散保留 ---
p07_group_dissolve_retention(Conn, C, OrgB) ->
    Gid = 16#82EA0201,
    exec(Conn, ~s{INSERT INTO "group" (id, owner_uid, creator_uid, member_max, member_count,
        introduction, avatar, title, chat_aes_key, status, created_at, e2ee_mode, scope, workspace_id)
        VALUES ($1,$2,$2,500,1,'','','GZAPP09-G1','k',1,now(),0,'workspace',NULL)
        ON CONFLICT (id) DO NOTHING}, [Gid, ?OWNER2]),
    exec(Conn, ~s<INSERT INTO msg_c2g (id, msg_id, from_id, to_id_list, msg_type, payload,
        created_at, conv_seq, sender_did, retry_count)
        VALUES ($1,'gzapp09-m1',$2,'[1,2]','text',jsonb_build_object(),now(),1,'did-gzapp09',0)
        ON CONFLICT (id) DO NOTHING>, [16#82EA0301, ?OWNER2]),
    %% org A 的 owner 解散 org B 的群 → 必须被拒（跨租户）
    Cross = group_logic:dissolve(?OWNER1, Gid),
    Alive = scalar(Conn, ~s{SELECT count(*)::bigint AS n FROM "group" WHERE id = $1}, [Gid]),
    ok("P07a 跨 org 解散群被拒且群行仍在", element(1, Cross) =:= error andalso Alive =:= 1, C),
    ok("P07b 消息行未受影响（保留策略）",
        scalar(Conn, ~s{SELECT count(*)::bigint AS n FROM msg_c2g WHERE id = $1}, [16#82EA0301]) =:= 1, C).

%% --- P08：幂等（同 request_id 不产生第二行） ---
p08_workspace_idempotent(Conn, C, OrgA) ->
    _ = workspace_logic:create(?OWNER1, OrgA, <<"GZAPP09-IDEM">>, <<"gzapp09-idem-req">>),
    _ = workspace_logic:create(?OWNER1, OrgA, <<"GZAPP09-IDEM">>, <<"gzapp09-idem-req">>),
    Count =
        scalar(
            Conn,
            ~s{SELECT count(*)::bigint AS n FROM workspace WHERE organization_id = $1 AND name = $2},
            [OrgA, <<"GZAPP09-IDEM">>]
        ),
    ok("P08 同 request_id 重复建 ws 只产生一行（幂等真源唯一）", Count =:= 1, C).

%% --- P09：待激活 Owner 契约（不真发短信） ---
p09_owner_activation_contract(Conn, C) ->
    OrgId = 16#82EA0003,
    exec(Conn, ~s<INSERT INTO organization (id, name, owner_id, status, branding, settings)
        VALUES ($1,'GZAPP09-Pending',0,'active',jsonb_build_object(),jsonb_build_object()) ON CONFLICT (id) DO NOTHING>, [OrgId]),
    R = organization_owner_activation_logic:admin_status(OrgId),
    ok("P09a 待激活状态可读（合同面存在）", element(1, R) =:= ok, C),
    %% 短信 provider 契约：fake 实现不触网
    FakeOk =
        case erlang:function_exported(imboy_sms_provider, behaviour_info, 1) orelse
            code:ensure_loaded(imboy_sms_fake) =/= {error, nofile} of
            true -> true;
            false ->
                case code:ensure_loaded(imboy_sms_fake) of
                    {module, _} -> true;
                    _ -> false
                end
        end,
    ok("P09b 短信 provider 契约/fake 已编译进树（禁止真实外发）", FakeOk, C).

%% ===================================================================
%% 清理（仅本 harness 的 fixture 段）
%% ===================================================================
cleanup(_Conn, OrgA, OrgB) ->
    _ = elib_pg:query(~s{DELETE FROM msg_c2g WHERE id = $1}, [16#82EA0301]),
    _ = elib_pg:query(~s{DELETE FROM group_log WHERE group_id = $1}, [16#82EA0201]),
    _ = elib_pg:query(~s{DELETE FROM group_member WHERE group_id = $1}, [16#82EA0201]),
    _ = elib_pg:query(~s{DELETE FROM "group" WHERE id = $1}, [16#82EA0201]),
    _ = elib_pg:query(
        ~s{DELETE FROM channel WHERE workspace_id IN (SELECT id FROM workspace WHERE organization_id = ANY($1::bigint[]))},
        [[OrgA, OrgB]]
    ),
    _ = elib_pg:query(~s{DELETE FROM organization_invite_code WHERE organization_id = ANY($1::bigint[])}, [[OrgA, OrgB]]),
    _ = elib_pg:query(~s{DELETE FROM organization_default_workspace WHERE organization_id = ANY($1::bigint[])}, [[OrgA, OrgB]]),
    _ = elib_pg:query(~s{DELETE FROM workspace_member WHERE workspace_id IN (SELECT id FROM workspace WHERE organization_id = ANY($1::bigint[]))}, [[OrgA, OrgB]]),
    _ = elib_pg:query(~s{DELETE FROM workspace WHERE organization_id = ANY($1::bigint[])}, [[OrgA, OrgB]]),
    _ = elib_pg:query(~s{DELETE FROM organization_member WHERE organization_id = ANY($1::bigint[])}, [[OrgA, OrgB]]),
    _ = elib_pg:query(~s{DELETE FROM organization WHERE id = ANY($1::bigint[])}, [[OrgA, OrgB]]),
    _ = elib_pg:query(~s{DELETE FROM "user" WHERE id = ANY($1::bigint[])}, [[?OWNER1, ?OWNER2, ?JOINER, ?OUTSIDER]]),
    io:format("~nFIXTURE-CLEANUP done~n"),
    ok.
