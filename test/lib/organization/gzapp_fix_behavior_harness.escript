#!/usr/bin/env escript
%%! -noshell
%% ===================================================================
%% GZAPP FIX 轮行为矩阵 harness（真 PostgreSQL）
%% -------------------------------------------------------------------
%% 覆盖本次复核修复的三处**行为变更**（此前仅有 mock 证据）：
%%   * R3-1  默认工作区 set/clear 的调用者鉴权（越权被拒且零状态变更）
%%   * F2    归档组织默认工作区必须显式指定替代项（计划 plan.snapshot.md:105）
%%   * F3    建企走模板原语（全员群 General + 公告频道 Announcements +
%%           owner workspace_member），并验证加入编排由此拿到 group_id/channel_id
%%
%% 用法:
%%   PGHOST=127.0.0.1 PGPORT=4323 PGUSER=imboy_user PGPASSWORD=*** \
%%   PGDATABASE=scratch_gzapp_fix_verify IMBOY_DIR=<worktree> \
%%   test/lib/organization/gzapp_fix_behavior_harness.escript
%%
%% 前置: 该库已应用迁移链至 00000138（scripts/drill_migrate.escript up）。
%% fixture 使用 83e9 段 bigint id（与他 run 的 82e8 段隔离）；跑完清理 fixture 行。
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
        database => env("PGDATABASE", "scratch_gzapp_fix_verify"),
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
%% Fixture：Org1（2 个 ws / owner+admin+member 角色齐全）、Org2（1 个 ws）
%% ===================================================================
-define(ORG1, 16#83E90001).
-define(ORG2, 16#83E90002).
-define(WS1, 16#83E90011).        %% org1 首个 ws（初始默认），owner=OWNER1
-define(WS2, 16#83E90012).        %% org1 次个 ws，owner=WSOWNER1
-define(WS_ORG2, 16#83E90013).    %% org2 的 ws（跨 org 替代项来源）
-define(WS3, 16#83E90014).        %% org1 第 3 个 ws，owner=OWNER1（非默认归档用）
-define(OWNER1, 16#83E90101).     %% org1 owner + ws1 owner（组织管理者）
-define(ADMIN1, 16#83E90102).     %% org1 admin（组织管理者，非任何 ws owner）
-define(MEMBER1, 16#83E90103).    %% org1 普通 member（既非管理者也非 ws owner）
-define(WSOWNER1, 16#83E90104).   %% org1 member + ws2 owner（非组织管理者）
-define(OUTSIDER, 16#83E90105).   %% org2 owner，与 org1 无关
-define(CREATE_OWNER, 16#83E90106). %% F3：admin_create 的 owner（active human）
-define(FORBIDDEN_PREFIX, <<"仅组织 Owner/Admin"/utf8>>).

setup(Conn) ->
    %% 幂等：先清上一次可能残留的 fixture（中途 FAIL 会 halt 跳过后置清理）
    _ = cleanup(Conn),
    exec(
        Conn,
        <<"INSERT INTO \"user\" (id, password, account, reg_ip, reg_cosv, account_type)"
            " SELECT i, 'x', 'gzfix-u' || i, '127.0.0.1', '', 0"
            " FROM unnest($1::bigint[]) AS i ON CONFLICT (id) DO NOTHING">>,
        [[?OWNER1, ?ADMIN1, ?MEMBER1, ?WSOWNER1, ?OUTSIDER, ?CREATE_OWNER]]
    ),
    exec(
        Conn,
        <<"INSERT INTO organization (id, name, owner_id, status, branding, settings)"
            " VALUES ($1,'GZFIX-Org1',$2,'active','{}','{}'),"
            " ($3,'GZFIX-Org2',$4,'active','{}','{}')"
            " ON CONFLICT (id) DO NOTHING">>,
        [?ORG1, ?OWNER1, ?ORG2, ?OUTSIDER]
    ),
    %% 角色矩阵：owner / admin / member / member
    exec(
        Conn,
        <<"INSERT INTO organization_member (organization_id, user_id, role, status)"
            " VALUES ($1,$2,'owner','active'), ($1,$3,'admin','active'),"
            " ($1,$4,'member','active'), ($1,$5,'member','active'),"
            " ($6,$7,'owner','active')"
            " ON CONFLICT DO NOTHING">>,
        [?ORG1, ?OWNER1, ?ADMIN1, ?MEMBER1, ?WSOWNER1, ?ORG2, ?OUTSIDER]
    ),
    exec(
        Conn,
        <<"INSERT INTO workspace (id, name, owner_id, organization_id, status, type, branding)"
            " VALUES ($1,'GZFIX-WS1',$2,$3,'active','project','{}'),"
            " ($4,'GZFIX-WS2',$5,$3,'active','project','{}'),"
            " ($6,'GZFIX-WS3',$7,$8,'active','project','{}')"
            " ON CONFLICT (id) DO NOTHING">>,
        [?WS1, ?OWNER1, ?ORG1, ?WS2, ?WSOWNER1, ?WS_ORG2, ?OUTSIDER, ?ORG2]
    ),
    exec(
        Conn,
        <<"INSERT INTO workspace_member (workspace_id, user_id, role, status)"
            " VALUES ($1,$2,'owner','active'), ($3,$4,'owner','active'), ($5,$6,'owner','active')"
            " ON CONFLICT DO NOTHING">>,
        [?WS1, ?OWNER1, ?WS2, ?WSOWNER1, ?WS_ORG2, ?OUTSIDER]
    ),
    exec(
        Conn,
        <<"INSERT INTO workspace (id, name, owner_id, organization_id, status, type, branding)"
            " VALUES ($1,'GZFIX-WS3b',$2,$3,'active','project','{}')"
            " ON CONFLICT (id) DO NOTHING">>,
        [?WS3, ?OWNER1, ?ORG1]
    ),
    exec(
        Conn,
        <<"INSERT INTO workspace_member (workspace_id, user_id, role, status)"
            " VALUES ($1,$2,'owner','active') ON CONFLICT DO NOTHING">>,
        [?WS3, ?OWNER1]
    ),
    %% 初始默认：org1 → WS1（模拟「建企时首个 ws 即默认」的稳态）
    exec(
        Conn,
        <<"INSERT INTO organization_default_workspace (organization_id, workspace_id)"
            " VALUES ($1,$2) ON CONFLICT (organization_id) DO UPDATE SET workspace_id = $2">>,
        [?ORG1, ?WS1]
    ),
    ok.

%% ===================================================================
%% 探针
%% ===================================================================
probes(Conn, Counters) ->
    io:format("~n== R3-1 默认工作区写命令鉴权（真库） ==~n"),
    %% P01：非管理者 set 被拒 403，且默认未被改动
    R1 = organization_default_workspace_app:set(?MEMBER1, ?ORG1, ?WS2),
    pass(Counters, <<"P01 普通成员 set 默认工作区 → 403"/utf8>>, is_forbidden(R1)),
    pass(
        Counters,
        <<"P01b 拒绝后默认仍为 WS1（零状态变更）"/utf8>>,
        default_of(Conn, ?ORG1) =:= ?WS1
    ),
    %% P02：外部人（org2 owner）set org1 默认被拒 403，零状态变更
    R2 = organization_default_workspace_app:set(?OUTSIDER, ?ORG1, ?WS1),
    pass(Counters, <<"P02 跨组织用户 set → 403"/utf8>>, is_forbidden(R2)),
    pass(
        Counters,
        <<"P02b 拒绝后默认仍为 WS1（零状态变更）"/utf8>>,
        default_of(Conn, ?ORG1) =:= ?WS1
    ),
    %% P03：无会话（undefined）set 被拒 403
    pass(
        Counters,
        <<"P03 无会话 operator=undefined → 403"/utf8>>,
        is_forbidden(organization_default_workspace_app:set(undefined, ?ORG1, ?WS2))
    ),
    %% P04：正控——org admin 可 set（证明拒绝不是因为路径整体坏掉）
    R4 = organization_default_workspace_app:set(?ADMIN1, ?ORG1, ?WS2),
    pass_val(Counters, <<"P04 正控 org admin set → {ok,_}"/utf8>>, R4, fun is_ok/1),
    pass(Counters, <<"P04b admin set 后默认变为 WS2"/utf8>>, default_of(Conn, ?ORG1) =:= ?WS2),
    %% P05：正控——目标 ws 的 owner 可设自己为默认（非组织管理者）
    R5 = organization_default_workspace_app:set(?WSOWNER1, ?ORG1, ?WS2),
    pass(
        Counters,
        <<"P05 正控 目标 WS owner（非组织管理者）set 自有 ws → {ok,_}"/utf8>>,
        is_ok(R5)
    ),
    %% P06：非管理者 clear 被拒 403，且行仍在（杜绝「清空默认」越权）
    R6 = organization_default_workspace_app:clear(?MEMBER1, ?ORG1),
    pass(Counters, <<"P06 普通成员 clear → 403"/utf8>>, is_forbidden(R6)),
    pass(
        Counters,
        <<"P06b 拒绝后默认行仍在且仍为 WS2"/utf8>>,
        default_of(Conn, ?ORG1) =:= ?WS2
    ),
    %% P07：非组织管理者的 ws owner clear 仍被拒——clear 仅限组织管理者
    pass(
        Counters,
        <<"P07 目标 WS owner clear → 403（clear 仅限组织管理者）"/utf8>>,
        is_forbidden(organization_default_workspace_app:clear(?WSOWNER1, ?ORG1))
    ),
    %% P08：正控——org admin clear 成功（幂等契约 {ok, cleared|already_empty}）
    R8 = organization_default_workspace_app:clear(?ADMIN1, ?ORG1),
    pass_val(Counters, <<"P08 正控 org admin clear → {ok,_}"/utf8>>, R8, fun is_ok/1),
    pass(Counters, <<"P08b clear 后默认行消失"/utf8>>, default_of(Conn, ?ORG1) =:= none),
    %% 复位到 WS1，进入 F2 段
    {ok, _} = organization_default_workspace_app:set(?OWNER1, ?ORG1, ?WS1),

    io:format("~n== F2 归档默认工作区强交接（真库） ==~n"),
    %% P09：未指定替代项 → 409，且 ws 未被归档、默认未变
    R9 = workspace_logic:archive(?OWNER1, ?WS1, #{}),
    pass_val(Counters, <<"P09 归档默认 WS 未指定替代项 → 409"/utf8>>, R9, fun(R) -> is_status(R, 409) end),
    pass(
        Counters,
        <<"P09b WS1 仍 active（归档被整体回滚）"/utf8>>,
        ws_status(Conn, ?WS1) =:= <<"active">>
    ),
    pass(Counters, <<"P09c 默认仍为 WS1（未被改指/清空）"/utf8>>, default_of(Conn, ?ORG1) =:= ?WS1),
    %% P10：显式给非法替代项（跨 org）→ 409，零状态变更
    R10 = workspace_logic:archive(?OWNER1, ?WS1, #{replacement_workspace_id => ?WS_ORG2}),
    pass_val(Counters, <<"P10 归档默认 WS 替代项跨 org → 409"/utf8>>, R10, fun(R) -> is_status(R, 409) end),
    pass(Counters, <<"P10b WS1 仍 active"/utf8>>, ws_status(Conn, ?WS1) =:= <<"active">>),
    pass(Counters, <<"P10c 默认仍为 WS1"/utf8>>, default_of(Conn, ?ORG1) =:= ?WS1),
    %% P11：替代项 = 已归档 ws → 409
    exec(Conn, <<"UPDATE workspace SET status='archived' WHERE id = $1">>, [?WS2]),
    R11 = workspace_logic:archive(?OWNER1, ?WS1, #{replacement_workspace_id => ?WS2}),
    pass_val(Counters, <<"P11 替代项为已归档 ws → 409"/utf8>>, R11, fun(R) -> is_status(R, 409) end),
    pass(Counters, <<"P11b WS1 仍 active"/utf8>>, ws_status(Conn, ?WS1) =:= <<"active">>),
    exec(Conn, <<"UPDATE workspace SET status='active' WHERE id = $1">>, [?WS2]),
    %% P12：合法替代项 → 归档成功 + 默认同事务改指（原子）
    R12 = workspace_logic:archive(?OWNER1, ?WS1, #{replacement_workspace_id => ?WS2}),
    pass_val(Counters, <<"P12 归档默认 WS 带合法替代项 → {ok,_}"/utf8>>, R12, fun is_ok/1),
    pass(Counters, <<"P12b WS1 已 archived"/utf8>>, ws_status(Conn, ?WS1) =:= <<"archived">>),
    pass(Counters, <<"P12c 默认同事务改指 WS2"/utf8>>, default_of(Conn, ?ORG1) =:= ?WS2),
    %% P13：回归——归档**非**默认 ws 仍无需替代项（不误伤既有路径）
    R13 = workspace_logic:archive(?OWNER1, ?WS3, #{}),
    pass_val(Counters, <<"P13 归档非默认 WS 无需替代项 → {ok,_}"/utf8>>, R13, fun is_ok/1),
    pass(Counters, <<"P13b WS3 已 archived"/utf8>>, ws_status(Conn, ?WS3) =:= <<"archived">>),
    pass(
        Counters,
        <<"P13c 默认仍指向 WS2（与归档无关路径不变）"/utf8>>,
        default_of(Conn, ?ORG1) =:= ?WS2
    ),
    %% 复位：WS3/WS1 回 active，默认指回 WS1（供 F3 段与清理）
    {ok, _} = workspace_logic:restore(?OWNER1, ?WS3),
    exec(Conn, <<"UPDATE workspace SET status='active' WHERE id = $1">>, [?WS1]),
    {ok, _} = organization_default_workspace_app:set(?OWNER1, ?ORG1, ?WS1),

    io:format("~n== R3-5 频道归档/恢复的双授权源（真库，判定在事务内） ==~n"),
    %% 通道由 org owner 建（他是 ws1 owner）；随后用「非创建者」身份试归档，
    %% 验证第二授权源与拒绝都真实生效。
    {ok, ChRow} = channel_logic:create_channel(
        ?OWNER1, <<"GZFIX-CH-R35"/utf8>>, #{}, 100, {<<"workspace">>, ?WS1}
    ),
    R35ChId = maps:get(<<"id">>, ChRow),
    pass(Counters, <<"P24 企业频道创建成功（R3-5 fixture）"/utf8>>, is_integer(R35ChId)),
    %% P25：非创建者且非 org 管理者（MEMBERSH 仅 org member）→ 拒绝，频道仍 active
    Deny = channel_logic:archive_channel(?MEMBER1, integer_to_binary(R35ChId)),
    pass_val(
        Counters,
        <<"P25 非创建者且非 org 管理者归档 → 既有拒绝文案"/utf8>>,
        Deny,
        fun
            ({error, Msg}) when is_binary(Msg) -> true;
            (_) -> false
        end
    ),
    pass(
        Counters,
        <<"P25b 被拒后频道仍 active（事务内判定未产生写入）"/utf8>>,
        channel_status(Conn, R35ChId) =:= 1
    ),
    %% P26：org owner（非频道创建者）经第二授权源归档成功
    OkArch = channel_logic:archive_channel(?OWNER1, integer_to_binary(R35ChId)),
    pass_val(
        Counters,
        <<"P26 org owner（非创建者）归档成功（第二授权源）"/utf8>>,
        OkArch,
        fun
            ({ok, _}) -> true;
            (_) -> false
        end
    ),
    pass(
        Counters,
        <<"P26b 归档后 status=0（archived）"/utf8>>,
        channel_status(Conn, R35ChId) =:= 0
    ),
    %% P27：恢复同源放行；跨组织/无关系者仍被拒
    OkRes = channel_logic:restore_channel(?OWNER1, integer_to_binary(R35ChId)),
    pass_val(
        Counters,
        <<"P27 org owner（非创建者）恢复成功"/utf8>>,
        OkRes,
        fun
            ({ok, _}) -> true;
            (_) -> false
        end
    ),
    pass(
        Counters,
        <<"P27b 恢复后 status=1（active）"/utf8>>,
        channel_status(Conn, R35ChId) =:= 1
    ),
    Deny2 = channel_logic:archive_channel(?OUTSIDER, integer_to_binary(R35ChId)),
    pass(
        Counters,
        <<"P28 跨组织用户归档被拒且频道仍 active"/utf8>>,
        is_error_binary(Deny2) andalso channel_status(Conn, R35ChId) =:= 1
    ),
    {ok, _} = channel_ds:archive(R35ChId),

    io:format("~n== C3 成员有权 Workspace（计划 §5.2，全员可见） ==~n"),
    %% P20：普通成员（非治理者）也能读别人的有权 Workspace——这正是 §5.2
    %% 「所有企业成员可见」的要求。此刻 OWNER1 在 org1 内 active 的 ws = WS1、WS3。
    Rw = organization_member_logic:workspaces(?MEMBER1, ?ORG1, ?OWNER1),
    pass_val(
        Counters,
        <<"P20 普通成员可读他人有权 Workspace（全员可见）"/utf8>>,
        Rw,
        fun
            ({ok, Items}) when is_list(Items) -> true;
            (_) -> false
        end
    ),
    {ok, OwnerWs} = Rw,
    OwnerWsIds = [maps:get(<<"id">>, I) || I <- OwnerWs],
    pass(
        Counters,
        <<"P20b 只含本 Org 内该成员的 active Workspace（WS1+WS3，按 id 升序）"/utf8>>,
        OwnerWsIds =:= [?WS1, ?WS3]
    ),
    pass(
        Counters,
        <<"P20c 不含跨 Org 授权（WS_ORG2 不出现）"/utf8>>,
        not lists:member(?WS_ORG2, OwnerWsIds)
    ),
    %% P21：非本 Org 成员调用 → 403（不泄露组织存在性）
    pass(
        Counters,
        <<"P21 非本 Org 成员调用 → 403"/utf8>>,
        is_status(organization_member_logic:workspaces(?OUTSIDER, ?ORG1, ?OWNER1), 403)
    ),
    %% P22：目标是外部用户 → 404
    pass(
        Counters,
        <<"P22 目标不是本 Org 成员 → 404"/utf8>>,
        is_status(organization_member_logic:workspaces(?OWNER1, ?ORG1, ?OUTSIDER), 404)
    ),
    %% P23：非法 id 形状 → 400
    pass(
        Counters,
        <<"P23 非法 user_id → 400"/utf8>>,
        is_status(organization_member_logic:workspaces(?OWNER1, ?ORG1, 0), 400)
    ),

    io:format("~n== F3 建企模板原语 + 加入编排（真库） ==~n"),
    RA = organization_admin_logic:admin_create(
        1, <<"GZFIX-建企验证"/utf8>>, ?CREATE_OWNER, <<"GZFIX-默认工作区"/utf8>>
    ),
    pass_val(Counters, <<"P14 admin_create → {ok,_}"/utf8>>, RA, fun is_ok/1),
    {OrgNew, WsNew} = created_ids(RA),
    pass(Counters, <<"P14b admin_create created=true 且拿到 org id"/utf8>>, is_integer(OrgNew)),
    pass(Counters, <<"P14c admin_create 返回默认 ws id"/utf8>>, is_integer(WsNew)),
    pass(
        Counters,
        <<"P15 owner workspace_member 行存在（此前缺失）"/utf8>>,
        member_rows(Conn, WsNew, ?CREATE_OWNER) =:= 1
    ),
    {Gid, Cid} = default_resource_ids(Conn, WsNew),
    pass(
        Counters,
        <<"P16 全员群 General 存在且 owner 为群成员（role=4）"/utf8>>,
        is_integer(Gid) andalso group_owner_role(Conn, Gid, ?CREATE_OWNER) =:= 4
    ),
    pass(Counters, <<"P17 公告频道 Announcements 存在"/utf8>>, is_integer(Cid)),
    pass(
        Counters,
        <<"P17b 频道 admin(role=3) + 订阅行存在"/utf8>>,
        channel_admin_role(Conn, Cid, ?CREATE_OWNER) =:= 3 andalso
            channel_subs(Conn, Cid, ?CREATE_OWNER) =:= 1
    ),
    pass(Counters, <<"P18 默认关系指向该 ws"/utf8>>, default_of(Conn, OrgNew) =:= WsNew),
    %% P19：F3 的业务面证据——加入编排现在能拿到 group_id/channel_id（此前恒 none）
    {ok, Sum} = elib_pg:with_tx(fun(C) ->
        {ok, _Outcome, S} = organization_join_orchestrator:join_tx(C, OrgNew, ?MEMBER1, null),
        {ok, S}
    end),
    pass(
        Counters,
        <<"P19 加入编排 workspace_id 非 none"/utf8>>,
        maps:get(workspace_id, Sum) =:= WsNew
    ),
    pass(
        Counters,
        <<"P19b 加入编排 group_id 为全员群（此前 none）"/utf8>>,
        maps:get(group_id, Sum) =:= Gid
    ),
    pass(
        Counters,
        <<"P19c 加入编排 channel_id 为公告频道（此前 none）"/utf8>>,
        maps:get(channel_id, Sum) =:= Cid
    ),
    pass(
        Counters,
        <<"P19d 加入者为该 ws 成员（role=member）"/utf8>>,
        ws_member_role(Conn, WsNew, ?MEMBER1) =:= <<"member">>
    ),
    cleanup_created(Conn, OrgNew, WsNew),
    ok.


is_error_binary({error, Msg}) when is_binary(Msg) -> true;
is_error_binary(_) -> false.

channel_status(Conn, ChannelId) ->
    one(Conn, <<"SELECT status FROM channel WHERE id = $1">>, [ChannelId]).

%% ===================================================================
%% 断言助手
%% ===================================================================
is_ok({ok, _}) -> true;
is_ok(_) -> false.

is_forbidden({error, {403, Msg}}) when is_binary(Msg) ->
    binary:match(Msg, ?FORBIDDEN_PREFIX) =/= nomatch;
is_forbidden(_) ->
    false.

is_status({error, {Code, Msg}}, Code) when is_binary(Msg) -> true;
is_status(_, _) -> false.

created_ids(
    {ok, #{
        <<"organization">> := #{<<"id">> := OrgId},
        <<"default_workspace">> := #{<<"id">> := WsId}
    }}
) ->
    {OrgId, WsId};
created_ids(Other) ->
    io:format("FIXTURE-ERROR: admin_create 返回不可解析 ~p~n", [Other]),
    halt(3).

default_of(Conn, OrgId) ->
    case
        epgsql:equery(
            Conn,
            <<"SELECT workspace_id FROM organization_default_workspace"
                " WHERE organization_id = $1">>,
            [OrgId]
        )
    of
        {ok, _, [{WsId}]} -> WsId;
        {ok, _, []} -> none
    end.

ws_status(Conn, WsId) ->
    one(Conn, <<"SELECT status FROM workspace WHERE id = $1">>, [WsId]).

member_rows(Conn, WsId, Uid) ->
    scalar(
        Conn,
        <<"SELECT count(*) FROM workspace_member WHERE workspace_id = $1 AND user_id = $2">>,
        [WsId, Uid]
    ).

ws_member_role(Conn, WsId, Uid) ->
    one(
        Conn,
        <<"SELECT role FROM workspace_member WHERE workspace_id = $1 AND user_id = $2">>,
        [WsId, Uid]
    ).

%% 与 join_orchestrator 同口径：scope=workspace 且 status=1 的最老群/频道
default_resource_ids(Conn, WsId) ->
    Gid = one_opt(
        Conn,
        <<"SELECT id FROM \"group\" WHERE workspace_id = $1 AND scope = 'workspace'"
            " AND status = 1 ORDER BY id ASC LIMIT 1">>,
        [WsId]
    ),
    Cid = one_opt(
        Conn,
        <<"SELECT id FROM channel WHERE workspace_id = $1 AND scope = 'workspace'"
            " AND status = 1 ORDER BY id ASC LIMIT 1">>,
        [WsId]
    ),
    {Gid, Cid}.

group_owner_role(Conn, Gid, Uid) ->
    one_opt(
        Conn,
        <<"SELECT role FROM group_member WHERE group_id = $1 AND user_id = $2">>,
        [Gid, Uid]
    ).

channel_admin_role(Conn, Cid, Uid) ->
    one_opt(
        Conn,
        <<"SELECT role FROM channel_admin WHERE channel_id = $1 AND user_id = $2">>,
        [Cid, Uid]
    ).

channel_subs(Conn, Cid, Uid) ->
    scalar(
        Conn,
        <<"SELECT count(*) FROM channel_subscription WHERE channel_id = $1"
            " AND user_id = $2 AND status = 1">>,
        [Cid, Uid]
    ).

one(Conn, Sql, Params) ->
    {ok, _, [{V}]} = epgsql:equery(Conn, Sql, Params),
    V.

one_opt(Conn, Sql, Params) ->
    case epgsql:equery(Conn, Sql, Params) of
        {ok, _, [{V}]} -> V;
        {ok, _, []} -> none
    end.

scalar(Conn, Sql, Params) ->
    {ok, _, [{V}]} = epgsql:equery(Conn, Sql, Params),
    V.

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
    %% halt/1 不走 lager 的异步 sink——失败前强制 flush，否则恰好丢掉
    %% 那一条 ?ERROR_LOG（定位根因的唯一线索）。
    _ = (catch lager:sync()),
    io:format("  [FAIL] ~ts~n", [Label]),
    halt(2).

%% 失败时把实际值打出来（真库调试必需：只报 FAIL 无法定位是 409 文案不符
%% 还是 500 兜底，前者是契约、后者是缺陷）。
pass_val(Counters, Label, Value, Pred) ->
    case Pred(Value) of
        true ->
            pass(Counters, Label, true);
        false ->
            _ = (catch lager:sync()),
            io:format("  [ACTUAL] ~ts => ~p~n", [Label, Value]),
            pass(Counters, Label, false)
    end.

cleanup(Conn) ->
    %% 顺序敏感：
    %%   * organization_member 只删**非 owner** 行——00000127 owner 不变量
    %%     触发器是 DEFERRED，而 epgsql 每条 equery 都是独立自动提交点，
    %%     整表删除会让该提交点出现「0 个 active owner」→ 23514 并打断 harness；
    %%     owner 行改随 organization 删除由 FK CASCADE 带走。
    %%   * workspace 先于 organization（FK RESTRICT）。
    WsIds = [?WS1, ?WS2, ?WS3],
    Groups = group_ids_of_ws(Conn, WsIds),
    _ = epgsql:equery(Conn, <<"DELETE FROM group_log WHERE group_id = ANY($1)">>, [Groups]),
    _ = epgsql:equery(Conn, <<"DELETE FROM group_member WHERE group_id = ANY($1)">>, [Groups]),
    _ = epgsql:equery(Conn, <<"DELETE FROM \"group\" WHERE workspace_id = ANY($1)">>, [WsIds]),
    _ = epgsql:equery(
        Conn, <<"DELETE FROM channel_subscription WHERE workspace_id = ANY($1)">>, [WsIds]
    ),
    _ = epgsql:equery(
        Conn,
        <<"DELETE FROM channel_admin WHERE channel_id IN"
            " (SELECT id FROM channel WHERE workspace_id = ANY($1))">>,
        [WsIds]
    ),
    _ = epgsql:equery(Conn, <<"DELETE FROM channel WHERE workspace_id = ANY($1)">>, [WsIds]),
    _ = epgsql:equery(
        Conn,
        <<"DELETE FROM organization_default_workspace WHERE organization_id = ANY($1)">>,
        [[?ORG1, ?ORG2]]
    ),
    _ = epgsql:equery(
        Conn, <<"DELETE FROM workspace_member WHERE workspace_id = ANY($1)">>, [
            [?WS1, ?WS2, ?WS3, ?WS_ORG2]
        ]
    ),
    _ = epgsql:equery(Conn, <<"DELETE FROM workspace WHERE id = ANY($1)">>, [
        [?WS1, ?WS2, ?WS3, ?WS_ORG2]
    ]),
    _ = epgsql:equery(
        Conn,
        <<"DELETE FROM organization_member WHERE organization_id = ANY($1) AND role <> 'owner'">>,
        [[?ORG1, ?ORG2]]
    ),
    %% org 删除失败（他表 FK RESTRICT / 残留引用）必须显式暴露——否则随后的
    %% 成员删除会把库留成「有 org 无 owner」的脏态，并在别处炸出难解的错误。
    case epgsql:equery(Conn, <<"DELETE FROM organization WHERE id = ANY($1)">>, [[?ORG1, ?ORG2]]) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            io:format("CLEANUP-ERROR: organization 删除失败 ~p~n", [Reason]),
            halt(3)
    end,
    _ = epgsql:equery(Conn, <<"DELETE FROM \"user\" WHERE id = ANY($1)">>, [
        [?OWNER1, ?ADMIN1, ?MEMBER1, ?WSOWNER1, ?OUTSIDER, ?CREATE_OWNER]
    ]),
    ok.

group_ids_of_ws(Conn, WsIds) ->
    {ok, _, Rows} = epgsql:equery(
        Conn, <<"SELECT id FROM \"group\" WHERE workspace_id = ANY($1)">>, [WsIds]
    ),
    [Id || {Id} <- Rows].

%% F3 段：admin_create 建出的组织/工作区（id 是运行时 TSID，须回读清理）
cleanup_created(Conn, OrgId, WsId) ->
    _ = epgsql:equery(
        Conn,
        <<"DELETE FROM group_log WHERE group_id IN"
            " (SELECT id FROM \"group\" WHERE workspace_id = $1)">>,
        [WsId]
    ),
    _ = epgsql:equery(
        Conn,
        <<"DELETE FROM group_member WHERE group_id IN"
            " (SELECT id FROM \"group\" WHERE workspace_id = $1)">>,
        [WsId]
    ),
    _ = epgsql:equery(Conn, <<"DELETE FROM \"group\" WHERE workspace_id = $1">>, [WsId]),
    _ = epgsql:equery(Conn, <<"DELETE FROM channel_subscription WHERE workspace_id = $1">>, [WsId]),
    _ = epgsql:equery(
        Conn,
        <<"DELETE FROM channel_admin WHERE channel_id IN"
            " (SELECT id FROM channel WHERE workspace_id = $1)">>,
        [WsId]
    ),
    _ = epgsql:equery(Conn, <<"DELETE FROM channel WHERE workspace_id = $1">>, [WsId]),
    _ = epgsql:equery(
        Conn, <<"DELETE FROM organization_default_workspace WHERE organization_id = $1">>, [OrgId]
    ),
    _ = epgsql:equery(Conn, <<"DELETE FROM workspace_member WHERE workspace_id = $1">>, [WsId]),
    _ = epgsql:equery(Conn, <<"DELETE FROM workspace WHERE id = $1">>, [WsId]),
    %% 同 cleanup/1：非 owner 行先删，owner 行随 organization CASCADE；
    %% org 删除失败必须暴露（否则留脏态）。
    _ = epgsql:equery(
        Conn,
        <<"DELETE FROM organization_member WHERE organization_id = $1 AND role <> 'owner'">>,
        [OrgId]
    ),
    case epgsql:equery(Conn, <<"DELETE FROM organization WHERE id = $1">>, [OrgId]) of
        {ok, _} ->
            ok;
        {error, CleanupReason} ->
            io:format("CLEANUP-ERROR: organization ~p 删除失败 ~p~n", [OrgId, CleanupReason]),
            halt(3)
    end,
    ok.
