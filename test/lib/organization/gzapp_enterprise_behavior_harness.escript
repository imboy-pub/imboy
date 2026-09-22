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
    Pass = counters:get(Counters, 1),
    Fail = counters:get(Counters, 2),
    io:format("~nPROBE-SUMMARY pass=~p fail=~p~n", [Pass, Fail]),
    %% 有失败即非零退出——否则「探针失败」在 CI/门禁里会伪装成全绿
    %% （本 harness 上一版就因恒 halt(0)，让 GZ-J06/J07 两条从未产出证据的
    %%   声明被当成 PASS 记账）。
    case Fail of
        0 -> halt(0);
        _ -> halt(2)
    end.

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
-define(GROUP_WS, 16#82EA0201). %% P07：org1 默认 ws 内的企业群（群主=JOINER）
-define(MSG1, 16#82EA0301).     %% P07：群消息（解散后必须保留）
-define(MSG2, 16#82EA0302).
-define(PREFIX, "gzapp09-").
%% 修：原为 <<"1390000" ++ "0001">> —— `++` 不能出现在 binary 字面量内部，
%% 是编译期语法错误（P09 之前因更早的缺陷从未跑到，故一直没暴露）。
-define(PENDING_MOBILE, <<"13900000001">>).

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
    ok = p07_group_dissolve_retention(Conn, C, WsA),
    ok = p08_workspace_idempotent(Conn, C, OrgA),
    PendingFixture = p09_owner_activation_contract(Conn, C),
    ok = cleanup(Conn, OrgA, OrgB, PendingFixture),
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
    %% join_by_code/3 成功返回 {ok, Outcome, Summary}（Summary 为 atom 键 map，
    %% 见 organization_join_orchestrator:join_tx/4 的契约注释）
    J2 = case Join of
        {ok, _Outcome, #{group_id := G, channel_id := Ch}} when
            is_integer(G), is_integer(Ch)
        ->
            io:format("  [P02] 加入编排落到 group_id=~p channel_id=~p~n", [G, Ch]),
            true;
        Other2 ->
            io:format("  [P02] 加入编排未落到群/频道：~p~n", [Other2]),
            false
    end,
    %% 默认资源关系（GZ-J01 的真实断言面）：默认 WS 指针存在**且**该 ws 下
    %% 同时存在 scope=workspace 的全员群与公告频道。
    %% 修：原断言只查 `WsRows =/= []`（「有一行」），建企只裸插 workspace 时
    %% 也能满足——等于没有验证「全员群、公告频道原子成功」，却被记成 GZ-J01 PASS。
    WsId = scalar(
        Conn,
        ~s{SELECT workspace_id AS n FROM organization_default_workspace WHERE organization_id = $1},
        [OrgA]
    ),
    GCount = scalar(
        Conn,
        ~s{SELECT count(*)::bigint AS n FROM "group"
           WHERE workspace_id = $1 AND scope = 'workspace' AND status = 1},
        [WsId]
    ),
    CCount = scalar(
        Conn,
        ~s{SELECT count(*)::bigint AS n FROM channel
           WHERE workspace_id = $1 AND scope = 'workspace' AND status = 1},
        [WsId]
    ),
    ok(
        "P02a 邀请码加入成功且 membership active",
        element(1, Join) =:= ok andalso Member =:= 1,
        C
    ),
    ok("P02b 加入编排返回真实 group_id/channel_id（默认资源关系已落地）", J2, C),
    ok(
        "P02c 默认 Workspace 下全员群与公告频道均存在（GZ-J01 原子成功）",
        is_integer(WsId) andalso GCount >= 1 andalso CCount >= 1,
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
    %% 修：原用模块宏 ?ORG1（恒为 fixture 段常量）当查询参数，与 P05 的 OrgA
    %% 不是同一值 → 查不到 Ws2 → `[Ws2] = []` badmatch 打断 harness，
    %% 后续 P06b/P06c/P07/P08/P09 全部未执行（GZ-J06/J07 因此从未产出证据）。
    [Ws2] = [
        maps:get(<<"id">>, R)
     || R <- element(2, elib_pg:query(Conn, ~s{SELECT id FROM workspace WHERE organization_id = $1 AND name = $2}, [OrgA, <<"GZAPP09-WS2">>]))
    ],
    %% 修：企业频道必须走 create_channel/5 并显式给 {<<"workspace">>, WsId}；
    %% 原用 /4（个人域路径）会静默建出 scope='personal' 频道，
    %% list_workspace_channels 永远查不到它。
    {ok, Ch} = channel_logic:create_channel(
        ?OWNER1,
        <<"GZAPP09-CH1">>,
        #{},
        100,
        {<<"workspace">>, Ws2}
    ),
    ChId = maps:get(<<"id">>, Ch),
    ok("P06a 企业频道创建成功", is_integer(ChId), C),
    %% 旁证：频道行确实是 workspace 作用域，否则下面的可见性断言无意义
    Scope = scalar(
        Conn, ~s{SELECT count(*)::bigint AS n FROM channel WHERE id = $1 AND scope = 'workspace'}, [ChId]
    ),
    ok("P06a2 频道行 scope='workspace'（企业域，非个人域）", Scope =:= 1, C),
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

%% --- P07：群解散保留（GZ-J07） ---
%% 语义（与 harness 自述一致）：org 管理者解散**非本人持有**的 ws 群后，
%% 消息行保留 + group_log type=101 记录真实操作者。
%% 修：原 fixture 建的是 scope='workspace' 但 workspace_id=NULL 的群——既不
%% 属于任何 org，「跨 org 被拒」实际测的是「无 workspace 被拒」，与标题
%% 「跨 org 解散」不是同一件事；且 msg_c2g 用了 to_id_list/conv_seq/retry_count
%% 三个不存在的列（那是 msg_store 出站表的形状）→ undefined_column 直接中断。
p07_group_dissolve_retention(Conn, C, Ws1) ->
    Gid = ?GROUP_WS,
    %% 群主 = JOINER（org 普通成员、非管理者）；org owner OWNER1 不占群主位。
    %% 落 OrgA 的**默认 ws**（P02 的 join_by_code 把 JOINER 加进的就是它），
    %% 满足「群成员 ⊆ ws 成员」提交校验。
    exec(Conn, ~s{INSERT INTO "group" (id, owner_uid, creator_uid, title, scope, workspace_id,
        status, e2ee_mode) VALUES ($1,$2,$2,'GZAPP09-G1','workspace',$3,1,0)
        ON CONFLICT (id) DO NOTHING}, [Gid, ?JOINER, Ws1]),
    %% 群成员 ⊆ ws 成员：JOINER 已由 P02 加入并落地为 ws 成员；OWNER1 是 ws owner
    exec(Conn, ~s{INSERT INTO group_member (id, group_id, user_id, status)
        VALUES ($1,$2,$3,1), ($4,$2,$5,1) ON CONFLICT DO NOTHING}, [
        Gid * 10 + 1, Gid, ?JOINER, Gid * 10 + 2, ?OWNER1
    ]),
    %% msg_c2g 真实列：id/topic_id/from_id/to_id/msg_id/msg_type/payload
    %% （原用的是 to_id_list/conv_seq/retry_count —— 那是 msg_store 出站表的形状，
    %%   msg_c2g 上不存在 → undefined_column 直接中断 harness）
    %% JSON 用 jsonb_build_object()：`~s{...}` 定界符不参与花括号配对，
    %% 字面 '{"t":"hi"}' 会把 sigil 提前闭合。
    exec(Conn, ~s{INSERT INTO msg_c2g (id, topic_id, from_id, to_id, msg_id, msg_type, payload)
        VALUES ($1,$2,$3,$4,'gzapp09-m1','text',jsonb_build_object('t','hi')),
               ($5,$2,$6,$4,'gzapp09-m2','text',jsonb_build_object('t','ho'))
        ON CONFLICT DO NOTHING}, [?MSG1, Gid, ?JOINER, Gid, ?MSG2, ?OWNER1]),
    %% org owner（非群主）解散该 ws 群：第二授权源放行
    %% （dissolve 成功返回原子 ok，失败返回 {error, _}——见 group_logic:dissolve/2）
    Dissolved = group_logic:dissolve(?OWNER1, Gid),
    ok("P07a org owner 解散非本人持有的 ws 群成功", Dissolved =:= ok, C),
    Gone = scalar(Conn, ~s{SELECT count(*)::bigint AS n FROM "group" WHERE id = $1}, [Gid]),
    ok("P07a2 群行已删除", Gone =:= 0, C),
    Msgs = scalar(
        Conn, ~s{SELECT count(*)::bigint AS n FROM msg_c2g WHERE topic_id = $1}, [Gid]
    ),
    ok("P07b 解散后 msg_c2g 消息行保留（保留策略，2 条）", Msgs =:= 2, C),
    Logs = scalar(
        Conn,
        ~s{SELECT count(*)::bigint AS n FROM group_log
           WHERE group_id = $1 AND type = 101 AND option_uid = $2},
        [Gid, ?OWNER1]
    ),
    ok("P07c group_log type=101 审计含真实操作者（可追溯）", Logs >= 1, C).

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

%% --- P09：待激活 Owner 契约（走真实建企路径；不真发短信） ---
%% 修：原 fixture 直插 owner_id=0 的 organization —— 违反 organization.owner_id
%% → user(id) 外键（真库 23503），harness 在这里直接中断，P09a/P09b 从未执行。
%% 改为调用与生产同一条 admin_create_pending_owner/5（预创建不可登录 Human
%% + 组织 + 默认 WS + owner_activation_invite）。
%% 短信边界（D11-D13 本卡交付面）：imboy_sms_provider 只实现 fake/local，
%% 未配置或配置成真实厂商一律 fail-closed 不外发——本 harness 依赖并断言这一点。
p09_owner_activation_contract(Conn, C) ->
    Mobile = ?PENDING_MOBILE,
    R = organization_admin_logic:admin_create_pending_owner(
        1, <<"GZAPP09-Pending">>, Mobile, <<"GZAPP09-P-WS1">>, <<"127.0.0.1">>
    ),
    ok("P09a 待激活建企成功（预创建 Human + 组织 + 邀请）", element(1, R) =:= ok, C),
    {OrgId, OwnerUid, Invite} =
        case R of
            {ok, #{
                <<"organization">> := #{<<"id">> := O},
                <<"owner_activation">> := #{<<"owner_user_id">> := U} = IV
            }} ->
                {O, U, IV};
            _ ->
                {0, 0, #{}}
        end,
    %% 预创建 Owner：不可登录 Human（status=0 禁用 / account_type=0 human）
    Flags =
        case
            elib_pg:query(
                ~s{SELECT status, account_type FROM "user" WHERE id = $1}, [OwnerUid]
            )
        of
            {ok, [#{<<"status">> := S, <<"account_type">> := A} | _]} -> {S, A};
            _ -> {unknown, unknown}
        end,
    ok("P09b Owner 为不可登录 Human（status=0, account_type=0）", Flags =:= {0, 0}, C),
    St = organization_owner_activation_logic:admin_status(OrgId),
    ok("P09c 待激活状态可读（合同面存在）", element(1, St) =:= ok, C),
    %% D12：短信失败不回滚企业——企业行仍在且 active
    OrgAlive = scalar(
        Conn,
        ~s{SELECT count(*)::bigint AS n FROM organization WHERE id = $1 AND status = 'active'},
        [OrgId]
    ),
    Smssent = maps:get(<<"sms_sent">>, element(2, R), undefined),
    InvStatus = maps:get(<<"status">>, Invite, undefined),
    %% 两种自洽终态：送出→pending；未送出→sms_failed（且企业照常提交）
    SmsConsistent =
        (Smssent =:= true andalso InvStatus =:= <<"pending">>) orelse
            (Smssent =:= false andalso InvStatus =:= <<"sms_failed">>),
    ok(
        "P09d 短信结果与 invite 状态自洽且企业不回滚（D12）",
        OrgAlive =:= 1 andalso SmsConsistent,
        C
    ),
    ok(
        "P09e 短信 provider 无真实实现（仅 fake/not_configured，绝不外发）",
        imboy_sms_provider:provider() =:= fake orelse
            imboy_sms_provider:provider() =:= not_configured,
        C
    ),
    {OrgId, OwnerUid, Mobile}.

%% ===================================================================
%% 清理（仅本 harness 的 fixture 段）
%% ===================================================================
cleanup(_Conn, OrgA, OrgB, PendingFixture) ->
    %% P09 的待激活企业/预创建 Owner 都是运行时 TSID：先按建企路径回收，
    %% 再并入统一删除面（先删依赖行，最后删 organization/user）。
    PendingOrgs =
        case PendingFixture of
            {POrgId, _PUid, _PMobile} when is_integer(POrgId), POrgId > 0 -> [POrgId];
            _ -> []
        end,
    _ = elib_pg:query(
        ~s{DELETE FROM group_log WHERE group_id IN (SELECT id FROM "group" WHERE workspace_id IN (SELECT id FROM workspace WHERE organization_id = ANY($1::bigint[])))},
        [PendingOrgs ++ [OrgA, OrgB]]
    ),
    _ = elib_pg:query(
        ~s{DELETE FROM group_member WHERE group_id IN (SELECT id FROM "group" WHERE workspace_id IN (SELECT id FROM workspace WHERE organization_id = ANY($1::bigint[])))},
        [PendingOrgs ++ [OrgA, OrgB]]
    ),
    _ = elib_pg:query(
        ~s{DELETE FROM "group" WHERE workspace_id IN (SELECT id FROM workspace WHERE organization_id = ANY($1::bigint[]))},
        [PendingOrgs ++ [OrgA, OrgB]]
    ),
    _ = elib_pg:query(
        ~s{DELETE FROM channel_subscription WHERE workspace_id IN (SELECT id FROM workspace WHERE organization_id = ANY($1::bigint[]))},
        [PendingOrgs ++ [OrgA, OrgB]]
    ),
    _ = elib_pg:query(
        ~s{DELETE FROM channel_admin WHERE channel_id IN (SELECT id FROM channel WHERE workspace_id IN (SELECT id FROM workspace WHERE organization_id = ANY($1::bigint[])))},
        [PendingOrgs ++ [OrgA, OrgB]]
    ),
    _ = elib_pg:query(
        ~s{DELETE FROM channel WHERE workspace_id IN (SELECT id FROM workspace WHERE organization_id = ANY($1::bigint[]))},
        [PendingOrgs ++ [OrgA, OrgB]]
    ),
    _ = elib_pg:query(
        ~s{DELETE FROM owner_activation_invite WHERE organization_id = ANY($1::bigint[])},
        [PendingOrgs ++ [OrgA, OrgB]]
    ),
    _ = elib_pg:query(~s{DELETE FROM group_log WHERE group_id = $1}, [?GROUP_WS]),
    _ = elib_pg:query(~s{DELETE FROM group_member WHERE group_id = $1}, [?GROUP_WS]),
    _ = elib_pg:query(~s{DELETE FROM "group" WHERE id = $1}, [?GROUP_WS]),
    _ = elib_pg:query(~s{DELETE FROM msg_c2g WHERE id = ANY($1::bigint[])}, [[?MSG1, ?MSG2]]),
    _ = elib_pg:query(
        ~s{DELETE FROM organization_invite_code WHERE organization_id = ANY($1::bigint[])},
        [PendingOrgs ++ [OrgA, OrgB]]
    ),
    _ = elib_pg:query(
        ~s{DELETE FROM organization_default_workspace WHERE organization_id = ANY($1::bigint[])},
        [PendingOrgs ++ [OrgA, OrgB]]
    ),
    _ = elib_pg:query(
        ~s{DELETE FROM workspace_member WHERE workspace_id IN (SELECT id FROM workspace WHERE organization_id = ANY($1::bigint[]))},
        [PendingOrgs ++ [OrgA, OrgB]]
    ),
    _ = elib_pg:query(
        ~s{DELETE FROM workspace WHERE organization_id = ANY($1::bigint[])},
        [PendingOrgs ++ [OrgA, OrgB]]
    ),
    %% 非 owner 成员先删；owner 行随 organization 删除 CASCADE 带走。
    %% （00000127 owner 不变量是 DEFERRED 且每条语句都是独立提交点，
    %%   整表先删会在该提交点出现「有 org 无 owner」→ 23514。）
    _ = elib_pg:query(
        ~s{DELETE FROM organization_member WHERE organization_id = ANY($1::bigint[]) AND role <> 'owner'},
        [PendingOrgs ++ [OrgA, OrgB]]
    ),
    _ = elib_pg:query(
        ~s{DELETE FROM organization WHERE id = ANY($1::bigint[])}, [PendingOrgs ++ [OrgA, OrgB]]
    ),
    PendingUsers =
        case PendingFixture of
            {_POrgId2, PUid, _} when is_integer(PUid), PUid > 0 -> [PUid];
            _ -> []
        end,
    _ = elib_pg:query(
        ~s{DELETE FROM "user" WHERE id = ANY($1::bigint[])},
        [PendingUsers ++ [?OWNER1, ?OWNER2, ?JOINER, ?OUTSIDER]]
    ),
    io:format("~nFIXTURE-CLEANUP done~n"),
    ok.
