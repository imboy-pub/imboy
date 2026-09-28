-module(adm_organization_create_tests).
-compile([nowarn_deprecated_catch]).
-include_lib("eunit/include/eunit.hrl").

%%% Platform Admin Organization 原子创建（合同 EADM-01/C2；
%%% POST /api/adm/organizations，list action GET/POST 同路径分流）
%%%
%%% 覆盖面（真库 marker 套件，镜像 adm_organization_handler_tests 配方）：
%%%   * 成功路径：org + owner membership + default workspace + 显式默认关系
%%%     全部落库；响应 TSID 全 string；审计 organization_create 落行；
%%%   * 幂等：同 owner 同名（大小写/首尾空白归一化）created=false 返回既有
%%%     org/ws，库内仍单行；
%%%   * 跨 owner 同名组织可共存；
%%%   * owner 门禁 fail-closed：不存在 404 / 非 active 400 / 非 human 400，
%%%     且零残留；
%%%   * 注入失败零残留：default 关系写入失败 → 整事务回滚，无 org/member/ws；
%%%   * 400 形状：缺/空 name、缺/坏 owner_user_id、缺 default_workspace_name；
%%%   * 权限门：无 organizations:write → 403 fail-closed。
%%%
%%% 库供给契约：inttest_marker_db 全链迁移；env 前缀 ADM_ORG_CREATE_INTTEST。

-define(WRITE_UID, 9201).
-define(RO_UID, 9203).

%% ------------------------------------------------------------------
%% Mock 套装（照 adm_organization_handler_tests 口径）
%% ------------------------------------------------------------------

-define(MOCK_NO_PLUGIN,
    {imboy_plugin_registry, [
        {'required_feature', 3, fun(_Type, _Handler, _Action) -> undefined end}
    ]}
).

-define(MOCK_RESP,
    {elib_response, [
        {'success', 2, fun(Req, Payload) -> Req#{response_status => 200, payload => Payload} end},
        {'success', 3, fun(Req, Payload, _Msg) ->
            Req#{response_status => 200, payload => Payload}
        end},
        {'error', 3, fun(Req, Msg, Code) -> Req#{response_status => Code, error_msg => Msg} end}
    ]}
).

-define(MOCK_COWBOY,
    {cowboy_req, [
        {'method', 1, fun(Req) -> maps:get(method, Req, <<"GET">>) end},
        {'binding', 3, fun(Key, Req, Default) ->
            maps:get(Key, maps:get(bindings, Req, #{}), Default)
        end},
        {'parse_qs', 1, fun(Req) -> maps:get(qs, Req, []) end},
        {'read_body', 1, fun(Req) -> {ok, maps:get(body, Req, <<>>), Req} end},
        {'path', 1, fun(_Req) -> <<"/api/adm/organizations">> end},
        {'set_resp_cookie', 4, fun(_Name, _Val, Req, _Opts) -> Req end},
        {'reply', 3, fun(Status, _Headers, Req) -> Req#{response_status => Status} end},
        {'reply', 4, fun(Status, _Headers, _Body, Req) -> Req#{response_status => Status} end}
    ]}
).

-define(MOCK_ACL,
    {adm_user_logic, [
        {'find', 3, fun
            (?WRITE_UID, <<"id,role_id">>, _Key) ->
                #{<<"id">> => ?WRITE_UID, <<"role_id">> => [1]};
            (?RO_UID, <<"id,role_id">>, _Key) ->
                #{<<"id">> => ?RO_UID, <<"role_id">> => [3]};
            (_Other, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 0, <<"role_id">> => [99]}
        end}
    ]}
).

-define(MOCK_ROLE_ACL,
    {adm_index_handler, [
        {'role_acl', 1, fun
            (1) ->
                {<<"super_admin">>, [<<"organizations:read">>, <<"organizations:write">>], []};
            (3) ->
                {<<"audit_admin">>, [<<"organizations:read">>], []};
            (_) ->
                {<<"none">>, [], []}
        end}
    ]}
).

-define(MOCK_PEER_IP,
    {elib_req, [
        {'peer_ip', 1, fun(_Req) -> <<"127.0.0.1">> end}
    ]}
).

-define(ADM_MOCKS, [
    ?MOCK_NO_PLUGIN,
    ?MOCK_RESP,
    ?MOCK_COWBOY,
    ?MOCK_ACL,
    ?MOCK_ROLE_ACL,
    ?MOCK_PEER_IP
]).

%% ===================================================================
%% 套件入口（真库 marker 套件）
%% ===================================================================

adm_org_create_test_() ->
    {timeout, 900,
        {setup, fun setup_db/0, fun close_db/1, fun(_Db) ->
            {foreach, fun mocks_on/0, fun(_S) -> mocks_off() end, [
                fun(_S) -> create_success_tests() end,
                fun(_S) -> create_idempotent_tests() end,
                fun(_S) -> create_owner_gate_tests() end,
                fun(_S) -> create_injection_rollback_tests() end,
                fun(_S) -> create_shape_tests() end,
                fun(_S) -> create_acl_tests() end
            ]}
        end}}.

setup_db() ->
    %% 纯套件（不启动 imboy app）：TSID 生成器 init+register 幂等配方。
    %% admin_op_log / organization / workspace 供本套件直写 org/ws ID 与审计行；
    %% group_info / group_member / channel / channel_admin /
    %% channel_subscription 供 workspace_ds:do_create_template/5 —— 平台默认
    %% workspace 模板同事务还要建默认 Group（group_info + group_member）与
    %% 默认 Channel（channel + channel_admin + channel_subscription）
    %% （workspace_ds.erl:194/:222 及 group_member_repo:upsert_active、
    %% channel_admin_repo:add、channel_subscription_repo:upsert_active）。
    %% 缺注册时 generate 崩 {elib_tsid_generator_not_registered, ...} →
    %% 整事务回滚 → handler 500，且 ERROR_LOG 经 lager 被吞，日志无痕迹。
    %% 全量跑时其他套件已注册同名生成器（persistent_term VM 全局）会掩盖此
    %% 缺口，单跑本套件必须自足注册。
    _ = (catch elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3})),
    ok =
        elib_tsid:register([
            admin_op_log,
            organization,
            workspace,
            group_info,
            group_member,
            channel,
            channel_admin,
            channel_subscription
        ]),
    ensure_depcache(),
    State =
        inttest_marker_db:provision(#{
            env_prefix => <<"ADM_ORG_CREATE_INTTEST">>,
            connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
        }),
    {ok, _} = application:ensure_all_started(pooler),
    #{server := Server, db := Db} = State,
    PoolConf = pgsql_pool_conf(Server, Db),
    case pooler:new_pool(PoolConf) of
        {ok, _Pid} ->
            State;
        {error, {already_started, _}} ->
            %% A1c（CP-TD-A02）：共享 VM 里 app 的 pgsql 池（指向共享库）已就位，
            %% 而产品代码 elib_pg:with_conn 硬编码 take_member(pgsql)——套件池必须
            %% 同名。接管：先停 app 池、换挂本套件 marker 库池；close_db/1 再按
            %% imboy pg_conf 重建 app 池还原共享 VM 状态，后续套件不受影响。
            ok = pool_swap('pgsql', PoolConf, 20),
            State
    end.

%% A1c：rm_pool/new_pool 均有异步窗口（rm 后名称短暂残留 already_present；
%% 成员占用中 rm 返回 running）——重试收敛，杜绝接管竞态。
%% rm_pool/new_pool 均有异步窗口：rm 后名称短暂残留（already_present）、
%% 成员占用中 rm 返回 running——单发必竞态。交替「rm→new」重试直到新池
%% （指向目标 conf）真正建立，杜绝接管/还原竞态（run19 实证）。
pool_swap(_Pool, _Conf, 0) ->
    erlang:error({pool_swap_failed, 'pgsql'});
pool_swap(Pool, Conf, N) ->
    _ = pooler:rm_pool(Pool),
    timer:sleep(200),
    case pooler:new_pool(Conf) of
        {ok, _Pid} ->
            ok;
        {error, {already_started, _}} ->
            timer:sleep(300),
            pool_swap(Pool, Conf, N - 1);
        {error, Other} ->
            erlang:error({pool_swap_failed, Other})
    end.

pgsql_pool_conf(#{host := Host, port := Port, username := User, password := Pass}, Db) ->
    #{
        name => pgsql,
        max_count => 4,
        init_count => 1,
        start_mfa =>
            {epgsql, connect, [
                #{
                    host => Host,
                    port => Port,
                    username => User,
                    password => Pass,
                    database => Db,
                    timeout => 10000,
                    codecs => [{epgsql_codec_rfc3339_bin, []}]
                }
            ]}
    }.

close_db(State) ->
    try pooler:rm_pool(pgsql) catch _:_ -> ok end, %% A1c: rm_pool/1 is the correct API (stop_pool/1 does not exist, the original try/catch had been silently swallowing undef)
    %% A1c（CP-TD-A02）：按 imboy pg_conf 重建 app pgsql 池（还原共享 VM 状态，
    %% 见 setup_db 接管注释），后续套件的 elib_pg 访问不受本套件影响。
    case application:get_env(imboy, pg_conf) of
        {ok, PgConf} when is_map(PgConf) ->
            ok = pool_swap('pgsql', PgConf, 20),
            ok;
        _ ->
            ok
    end,
    inttest_marker_db:release(State).

%% 默认 workspace 模板链（group_member_ds:join_group → group_ds:join）依赖
%% 命名 depcache 实例 imboy_cache（ETS 'm:imboy_cache'）。本套件不启动 imboy
%% app，单跑时须自足补建；全量跑时实例已由 eunit_setup 启动的 app 持有，此处
%% 为 no-op：
%%   * app 已启动（ETS 存在）→ 直接返回；
%%   * eunit_boot_coordinator 存在（eunit_runner 全量自愈协议）→ 由长驻协调
%%     进程重建，实例不随本套件 teardown 死亡；
%%   * 单跑兜底 → setup 进程直接 start_link（VM 随套件结束回收，无跨套件
%%     影响；imboy_cache:start_link 自带 already_started 收养）。
ensure_depcache() ->
    case ets:whereis('m:imboy_cache') of
        undefined ->
            case whereis(eunit_boot_coordinator) of
                undefined ->
                    _ = imboy_cache:start_link([{depcache_memory_max, 100}]),
                    ok;
                _CoordPid ->
                    Ref = make_ref(),
                    eunit_boot_coordinator ! {ensure_cache, self(), Ref},
                    receive
                        {Ref, ok} -> ok
                    after 5000 ->
                        %% 协调器超时：退化为 setup 进程自建（收养语义）
                        _ = imboy_cache:start_link([{depcache_memory_max, 100}]),
                        ok
                    end
            end;
        _Tab ->
            ok
    end.

mocks_on() ->
    lists:foreach(
        fun({Module, Expectations}) ->
            case meck_helper:setup_mock(Module, Expectations) of
                {ok, _} -> ok;
                {error, Reason} -> erlang:error({mock_setup_failed, Module, Reason})
            end
        end,
        ?ADM_MOCKS
    ).

mocks_off() ->
    lists:foreach(fun({Module, _}) -> meck_helper:cleanup_mock(Module) end, ?ADM_MOCKS).

%% ===================================================================
%% 组 1：成功路径 + TSID string + 审计 + 跨 owner 同名共存
%% ===================================================================

create_success_tests() ->
    [
        {"成功路径：created=true + org/member/ws/default 全落库 + TSID string + 审计", fun() ->
            success_path()
        end},
        {"跨 owner 同名组织可共存（各自 created=true 且 org id 不同）", fun() ->
            cross_owner_same_name()
        end}
    ].

success_path() ->
    Conn = conn(),
    Owner = new_id(),
    ok = seed_user(Conn, Owner, 1, 0),
    Name = <<"eadm-create-ok-", (integer_to_binary(Owner))/binary>>,
    RespReq = create_org(#{
        <<"name">> => Name,
        <<"owner_user_id">> => uid_bin(Owner),
        <<"default_workspace_name">> => <<"默认工作区"/utf8>>
    }),
    ?assertEqual(200, status_of(RespReq)),
    Payload = payload_of(RespReq),
    Org = maps:get(<<"organization">>, Payload),
    Ws = maps:get(<<"default_workspace">>, Payload),
    ?assertEqual(true, maps:get(<<"created">>, Payload)),
    %% TSID 全 string 下发
    ?assert(is_binary(maps:get(<<"id">>, Org))),
    ?assert(is_binary(maps:get(<<"owner_id">>, Org))),
    ?assertEqual(uid_bin(Owner), maps:get(<<"owner_id">>, Org)),
    ?assertEqual(Name, maps:get(<<"name">>, Org)),
    ?assertEqual(<<"active">>, maps:get(<<"status">>, Org)),
    OrgId = binary_to_integer(maps:get(<<"id">>, Org)),
    WsId = binary_to_integer(maps:get(<<"id">>, Ws)),
    ?assert(is_binary(maps:get(<<"name">>, Ws))),
    ?assertEqual(<<"active">>, maps:get(<<"status">>, Ws)),
    %% DB 事实：organization active
    ?assertMatch(
        {ok, [#{<<"status">> := <<"active">>, <<"owner_id">> := Owner}]},
        q(Conn, <<"SELECT status, owner_id FROM organization WHERE id = $1">>, [OrgId])
    ),
    %% owner membership：active + role=owner
    ?assertMatch(
        {ok, [#{<<"role">> := <<"owner">>, <<"status">> := <<"active">>}]},
        q(
            Conn,
            <<"SELECT role, status FROM organization_member",
                " WHERE organization_id = $1 AND user_id = $2">>,
            [OrgId, Owner]
        )
    ),
    %% Platform Admin 不写成 member：成员表只有 owner 一行
    {ok, [#{<<"count">> := MemberCount}]} =
        one(
            Conn,
            <<"SELECT count(*) AS count FROM organization_member WHERE organization_id = $1">>,
            [
                OrgId
            ]
        ),
    ?assertEqual(1, MemberCount),
    %% workspace：active + 归 org + owner 正确
    ?assertMatch(
        {ok, [#{<<"status">> := <<"active">>, <<"organization_id">> := OrgId}]},
        q(
            Conn,
            <<"SELECT status, organization_id, owner_id FROM workspace WHERE id = $1">>,
            [WsId]
        )
    ),
    %% 显式默认关系指向新 workspace
    ?assertMatch(
        {ok, [#{<<"workspace_id">> := WsId}]},
        q(
            Conn,
            <<"SELECT workspace_id FROM organization_default_workspace WHERE organization_id = $1">>,
            [OrgId]
        )
    ),
    %% 审计：organization_create 落行（幂等/新建都审计，这里只验新建）
    {ok, _, [{AuditCount}]} =
        epgsql:equery(
            Conn,
            <<"SELECT count(*) FROM admin_operation_logs",
                " WHERE target_id = $1 AND action = 'organization_create' AND adm_user_id = $2">>,
            [OrgId, ?WRITE_UID]
        ),
    ?assert(AuditCount >= 1),
    ok.

cross_owner_same_name() ->
    Conn = conn(),
    OwnerA = new_id(),
    OwnerB = new_id(),
    ok = seed_user(Conn, OwnerA, 1, 0),
    ok = seed_user(Conn, OwnerB, 1, 0),
    Name = <<"eadm-create-cross-", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    RespA =
        create_org(#{
            <<"name">> => Name,
            <<"owner_user_id">> => uid_bin(OwnerA),
            <<"default_workspace_name">> => <<"ws">>
        }),
    RespB =
        create_org(#{
            <<"name">> => Name,
            <<"owner_user_id">> => uid_bin(OwnerB),
            <<"default_workspace_name">> => <<"ws">>
        }),
    ?assertEqual(200, status_of(RespA)),
    ?assertEqual(200, status_of(RespB)),
    OrgA = maps:get(<<"organization">>, payload_of(RespA)),
    OrgB = maps:get(<<"organization">>, payload_of(RespB)),
    ?assertEqual(true, maps:get(<<"created">>, payload_of(RespA))),
    ?assertEqual(true, maps:get(<<"created">>, payload_of(RespB))),
    ?assertNotEqual(maps:get(<<"id">>, OrgA), maps:get(<<"id">>, OrgB)),
    {ok, [#{<<"count">> := Cnt}]} =
        one(
            Conn,
            <<"SELECT count(*) AS count FROM organization WHERE lower(trim(name)) = lower(trim($1))">>,
            [Name]
        ),
    ?assertEqual(2, Cnt),
    ok.

%% ===================================================================
%% 组 2：幂等（归一化命中 created=false，库内仍单行）
%% ===================================================================

create_idempotent_tests() ->
    [
        {"幂等：同 owner 同名（大小写+首尾空白变体）created=false 返回既有 org/ws，库内单行", fun() -> idempotent_hit() end}
    ].

idempotent_hit() ->
    Conn = conn(),
    Owner = new_id(),
    ok = seed_user(Conn, Owner, 1, 0),
    Name = <<"eadm-create-idem-", (integer_to_binary(Owner))/binary>>,
    Resp1 =
        create_org(#{
            <<"name">> => Name,
            <<"owner_user_id">> => uid_bin(Owner),
            <<"default_workspace_name">> => <<"ws-first">>
        }),
    ?assertEqual(200, status_of(Resp1)),
    Org1 = maps:get(<<"organization">>, payload_of(Resp1)),
    Ws1 = maps:get(<<"default_workspace">>, payload_of(Resp1)),
    ?assertEqual(true, maps:get(<<"created">>, payload_of(Resp1))),
    %% 归一化变体（大小写 + 首尾空格）命中幂等
    Variant = <<"  ", (upper_binary(Name))/binary, " ">>,
    Resp2 =
        create_org(#{
            <<"name">> => Variant,
            <<"owner_user_id">> => uid_bin(Owner),
            <<"default_workspace_name">> => <<"ws-second">>
        }),
    ?assertEqual(200, status_of(Resp2)),
    Payload2 = payload_of(Resp2),
    ?assertEqual(false, maps:get(<<"created">>, Payload2)),
    Org2 = maps:get(<<"organization">>, Payload2),
    Ws2 = maps:get(<<"default_workspace">>, Payload2),
    ?assertEqual(maps:get(<<"id">>, Org1), maps:get(<<"id">>, Org2)),
    ?assertEqual(maps:get(<<"id">>, Ws1), maps:get(<<"id">>, Ws2)),
    ?assertEqual(<<"ws-first">>, maps:get(<<"name">>, Ws2)),
    %% 库内仍单行 org / 单行 owner member / 单行 ws / 单行默认关系
    {ok, [#{<<"count">> := OrgCnt}]} =
        one(
            Conn,
            <<"SELECT count(*) AS count FROM organization WHERE owner_id = $1">>,
            [Owner]
        ),
    ?assertEqual(1, OrgCnt),
    {ok, [#{<<"count">> := WsCnt}]} =
        one(
            Conn,
            <<"SELECT count(*) AS count FROM workspace WHERE owner_id = $1 AND organization_id IS NOT NULL">>,
            [Owner]
        ),
    ?assertEqual(1, WsCnt),
    ok.

upper_binary(Bin) when is_binary(Bin) ->
    list_to_binary(string:uppercase(binary_to_list(Bin))).

%% ===================================================================
%% 组 3：owner 门禁（fail-closed，零残留）
%% ===================================================================

create_owner_gate_tests() ->
    [
        {"owner 不存在 → 404 + 零残留", fun() -> owner_missing() end},
        {"owner 非 active（status=0 禁用）→ 400 + 零残留", fun() -> owner_disabled() end},
        {"owner 非 human（account_type=1 ai_agent）→ 400 + 零残留", fun() ->
            owner_not_human()
        end}
    ].

owner_missing() ->
    Owner = new_id(),
    Name = <<"eadm-create-missing-", (integer_to_binary(Owner))/binary>>,
    RespReq =
        create_org(#{
            <<"name">> => Name,
            <<"owner_user_id">> => uid_bin(Owner),
            <<"default_workspace_name">> => <<"ws">>
        }),
    ?assertEqual(404, status_of(RespReq)),
    ?assertEqual(0, org_count(Owner, Name)),
    ok.

owner_disabled() ->
    Conn = conn(),
    Owner = new_id(),
    ok = seed_user(Conn, Owner, 0, 0),
    Name = <<"eadm-create-disabled-", (integer_to_binary(Owner))/binary>>,
    RespReq =
        create_org(#{
            <<"name">> => Name,
            <<"owner_user_id">> => uid_bin(Owner),
            <<"default_workspace_name">> => <<"ws">>
        }),
    ?assertEqual(400, status_of(RespReq)),
    ?assertEqual(0, org_count(Owner, Name)),
    ok.

owner_not_human() ->
    Conn = conn(),
    Owner = new_id(),
    ok = seed_user(Conn, Owner, 1, 1),
    Name = <<"eadm-create-agent-", (integer_to_binary(Owner))/binary>>,
    RespReq =
        create_org(#{
            <<"name">> => Name,
            <<"owner_user_id">> => uid_bin(Owner),
            <<"default_workspace_name">> => <<"ws">>
        }),
    ?assertEqual(400, status_of(RespReq)),
    ?assertEqual(0, org_count(Owner, Name)),
    ok.

%% ===================================================================
%% 组 4：注入失败零残留（真 PG 整事务回滚）
%% ===================================================================

create_injection_rollback_tests() ->
    [
        {"注入失败：default 关系写入失败 → 500 且 org/member/workspace 零残留", fun() -> injection_rollback() end},
        {"注入失败：平台审计写入失败 → 500 且 org/member/workspace/默认关系零残留（审计在同一事务）", fun() ->
            audit_injection_rollback()
        end}
    ].

%% 合同 EADM-01/C2（实施计划:109）：平台审计是创建事务的**第 8 步**，
%% 「失败必须整事务回滚」。
%%
%% 本用例注入审计写入失败，断言业务数据**一行都不留**。它是这条合同的唯一
%% 反例守卫：若审计被放在事务外（由 handler 事后补写）且吞掉写入错误，
%% 结果会是「组织创建成功、审计静默丢失」——org/member/ws 全部落库而这里
%% 期望 0，用例立刻变红。
audit_injection_rollback() ->
    Conn = conn(),
    Owner = new_id(),
    ok = seed_user(Conn, Owner, 1, 0),
    Name = <<"eadm-create-audit-inject-", (integer_to_binary(Owner))/binary>>,
    case
        meck_helper:setup_mock(adm_operation_log_ds, [
            {'insert_tx', 7, fun(_C, _Uid, _Action, _Tid, _TType, _Detail, _Ip) ->
                {error, injected_audit_failure}
            end}
        ])
    of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({mock_setup_failed, audit_inject, Reason})
    end,
    try
        RespReq =
            create_org(#{
                <<"name">> => Name,
                <<"owner_user_id">> => uid_bin(Owner),
                <<"default_workspace_name">> => <<"ws">>
            }),
        ?assertEqual(500, status_of(RespReq))
    after
        meck_helper:cleanup_mock(adm_operation_log_ds)
    end,
    %% 零残留：org / member / workspace / 默认关系全部随审计失败一起回滚
    ?assertEqual(0, org_count(Owner, Name)),
    {ok, [#{<<"count">> := MemberCnt}]} =
        one(
            Conn,
            <<"SELECT count(*) AS count FROM organization_member om",
                " JOIN organization o ON o.id = om.organization_id",
                " WHERE o.owner_id = $1 AND lower(trim(o.name)) = lower(trim($2))">>,
            [Owner, Name]
        ),
    ?assertEqual(0, MemberCnt),
    {ok, [#{<<"count">> := WsCnt}]} =
        one(
            Conn,
            <<"SELECT count(*) AS count FROM workspace w",
                " JOIN organization o ON o.id = w.organization_id",
                " WHERE o.owner_id = $1 AND lower(trim(o.name)) = lower(trim($2))">>,
            [Owner, Name]
        ),
    ?assertEqual(0, WsCnt),
    {ok, [#{<<"count">> := RelCnt}]} =
        one(
            Conn,
            <<"SELECT count(*) AS count FROM organization_default_workspace odw",
                " JOIN organization o ON o.id = odw.organization_id",
                " WHERE o.owner_id = $1 AND lower(trim(o.name)) = lower(trim($2))">>,
            [Owner, Name]
        ),
    ?assertEqual(0, RelCnt),
    ok.

injection_rollback() ->
    Conn = conn(),
    Owner = new_id(),
    ok = seed_user(Conn, Owner, 1, 0),
    Name = <<"eadm-create-inject-", (integer_to_binary(Owner))/binary>>,
    %% 默认关系的真实写入原语是 organization_default_workspace_pg:
    %% ensure_first_workspace_tx/3（链路：create_org_tx → workspace_ds
    %% create_default_template_tx → organization_default_workspace_app:
    %% ensure_first_workspace_tx → 本原语）。logic 头注释里的 upsert_tx
    %% 已非 admin_create 调用面——mock 必须打在真实链上，否则注入无效、
    %% 创建成功返回 200，本用例的 500 契约失守。
    case
        meck_helper:setup_mock(organization_default_workspace_pg, [
            {'ensure_first_workspace_tx', 3, fun(_C, _O, _W) ->
                {error, injected_failure}
            end}
        ])
    of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({mock_setup_failed, inject, Reason})
    end,
    try
        RespReq =
            create_org(#{
                <<"name">> => Name,
                <<"owner_user_id">> => uid_bin(Owner),
                <<"default_workspace_name">> => <<"ws">>
            }),
        ?assertEqual(500, status_of(RespReq))
    after
        meck_helper:cleanup_mock(organization_default_workspace_pg)
    end,
    %% 零残留：org / member / workspace / default 关系全部回滚
    ?assertEqual(0, org_count(Owner, Name)),
    {ok, [#{<<"count">> := MemberCnt}]} =
        one(
            Conn,
            <<"SELECT count(*) AS count FROM organization_member om",
                " JOIN organization o ON o.id = om.organization_id",
                " WHERE o.owner_id = $1 AND lower(trim(o.name)) = lower(trim($2))">>,
            [Owner, Name]
        ),
    ?assertEqual(0, MemberCnt),
    {ok, [#{<<"count">> := WsCnt}]} =
        one(
            Conn,
            <<"SELECT count(*) AS count FROM workspace w",
                " JOIN organization o ON o.id = w.organization_id",
                " WHERE o.owner_id = $1 AND lower(trim(o.name)) = lower(trim($2))">>,
            [Owner, Name]
        ),
    ?assertEqual(0, WsCnt),
    ok.

%% ===================================================================
%% 组 5：请求形状 400
%% ===================================================================

create_shape_tests() ->
    [
        {"缺 name → 400", fun() ->
            Owner = seed_owner(),
            ?assertEqual(
                400,
                status_of(
                    create_org(#{
                        <<"owner_user_id">> => uid_bin(Owner),
                        <<"default_workspace_name">> => <<"ws">>
                    })
                )
            )
        end},
        {"空白 name → 400", fun() ->
            Owner = seed_owner(),
            ?assertEqual(
                400,
                status_of(
                    create_org(#{
                        <<"name">> => <<"   ">>,
                        <<"owner_user_id">> => uid_bin(Owner),
                        <<"default_workspace_name">> => <<"ws">>
                    })
                )
            )
        end},
        {"缺 owner_user_id → 400", fun() ->
            ?assertEqual(
                400,
                status_of(
                    create_org(#{<<"name">> => <<"x">>, <<"default_workspace_name">> => <<"ws">>})
                )
            )
        end},
        {"非整数 owner_user_id → 400", fun() ->
            ?assertEqual(
                400,
                status_of(
                    create_org(#{
                        <<"name">> => <<"x">>,
                        <<"owner_user_id">> => <<"abc">>,
                        <<"default_workspace_name">> => <<"ws">>
                    })
                )
            )
        end},
        {"缺 default_workspace_name → 400", fun() ->
            Owner = seed_owner(),
            ?assertEqual(
                400,
                status_of(
                    create_org(#{<<"name">> => <<"x">>, <<"owner_user_id">> => uid_bin(Owner)})
                )
            )
        end}
    ].

%% ===================================================================
%% 组 6：权限门（fail-closed 403）
%% ===================================================================

create_acl_tests() ->
    [
        {"read-only 角色调 POST 创建 → 403（fail-closed）", fun() ->
            Owner = seed_owner(),
            Name = <<"eadm-create-ro-", (integer_to_binary(Owner))/binary>>,
            RespReq = call(
                ?RO_UID,
                <<"POST">>,
                #{
                    <<"name">> => Name,
                    <<"owner_user_id">> => uid_bin(Owner),
                    <<"default_workspace_name">> => <<"ws">>
                }
            ),
            ?assertEqual(403, status_of(RespReq)),
            ?assertEqual(0, org_count(Owner, Name))
        end}
    ].

%% ===================================================================
%% 调用 helper
%% ===================================================================

call(AdmUid, Method, BodyMap) ->
    Req0 = #{method => Method, bindings => #{}, body => jsone:encode(BodyMap)},
    {ok, RespReq, _} =
        adm_organization_handler:init(Req0, #{action => list, adm_user_id => AdmUid}),
    RespReq.

create_org(BodyMap) ->
    call(?WRITE_UID, <<"POST">>, BodyMap).

status_of(RespReq) -> maps:get(response_status, RespReq, undefined).

payload_of(RespReq) -> maps:get(payload, RespReq, undefined).

%% ===================================================================
%% Seed / SQL helpers（合成租户夹具；随机 TSID 隔离，不 TRUNCATE 不删行）
%% ===================================================================

seed_owner() ->
    Conn = conn(),
    Owner = new_id(),
    ok = seed_user(Conn, Owner, 1, 0),
    Owner.

seed_user(Conn, Uid, Status, AccountType) ->
    exec(
        Conn,
        <<"INSERT INTO \"user\"(id,password,account,reg_ip,reg_cosv,status,account_type)",
            " VALUES ($1,'x',$2,'127.0.0.1','x',$3,$4)">>,
        [Uid, account(Uid), Status, AccountType]
    ).

org_count(Owner, Name) ->
    {ok, [#{<<"count">> := Cnt}]} =
        one(
            conn(),
            <<"SELECT count(*) AS count FROM organization",
                " WHERE owner_id = $1 AND lower(trim(name)) = lower(trim($2))">>,
            [Owner, Name]
        ),
    Cnt.

new_id() ->
    try elib_tsid:generate() of
        Id when is_integer(Id) -> Id
    catch
        _:_ ->
            1000000000000000 + erlang:unique_integer([positive, monotonic])
    end.

uid_bin(Uid) ->
    integer_to_binary(Uid).

account(Uid) ->
    <<"eadmcr_", (integer_to_binary(Uid))/binary>>.

conn() ->
    pooler:take_member(pgsql).

exec(Conn, Sql, Params) ->
    case epgsql:equery(Conn, Sql, Params) of
        {ok, _} ->
            pooler:return_member(pgsql, Conn),
            ok;
        Other ->
            pooler:return_member(pgsql, Conn, fail),
            erlang:error({seed_failed, Sql, Other})
    end.

one(Conn, Sql, Params) ->
    case epgsql:equery(Conn, Sql, Params) of
        {ok, Cols, [Row]} ->
            pooler:return_member(pgsql, Conn),
            {ok, [tuple_to_named_map(Cols, Row)]};
        Other ->
            pooler:return_member(pgsql, Conn, fail),
            erlang:error({query_failed, Sql, Other})
    end.

q(Conn, Sql, Params) ->
    R = epgsql:equery(Conn, Sql, Params),
    pooler:return_member(pgsql, Conn),
    case R of
        {ok, Cols, Rows} when is_list(Rows) ->
            {ok, [tuple_to_named_map(Cols, Row) || Row <- Rows]};
        Other ->
            {error, Other}
    end.

tuple_to_named_map(Cols, Row) when is_list(Cols) ->
    Names = [element(2, C) || C <- Cols],
    maps:from_list(lists:zip(Names, tuple_to_list(Row))).
