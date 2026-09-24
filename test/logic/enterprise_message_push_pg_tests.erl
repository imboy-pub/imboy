%% enterprise_message_push_pg_tests
%% FULL-07 — 企业托管消息（OA 代发 INT-09/10）**离线推送**真库集成测试。
%%
%% 增量根因（A0 已核实）：`src/logic/enterprise_message_logic.erl` 此前没有任何
%% push 调用，而 `msg_c2c_logic` / `msg_c2g_logic` 都有。后果：OA 代发的企业
%% 托管消息发给**离线**用户时完全不推送——企业内部平台的核心通知链是断的。
%% 本套件把「修复后必须成立」与「修复后仍然不许发生」两侧都钉死。
%%
%% 被测物：**生产代码链**，不是副本。
%%   enterprise_message_logic:direct_tx/3 | group_tx/4   （真 SQL / 真事务）
%%   enterprise_message_logic:push_after_commit/2        （提交后独立读 + 触发）
%%   push_notification_logic:maybe_push_for_enterprise_c2c/2 | _c2g/3
%%   push_notification_ds:send_to_user/3 | send_to_users/3（多设备 fan-out）
%%   push_provider_jpush:send/3 → push_provider_jpush_http:post/3（真实 wire body）
%%
%% 一次性 marker 库（inttest_marker_db 配方，env 前缀 FULL07_INTTEST）：空库全量
%% up（erlang_migrate strict，含 00000136..00000142）→ 合同 oracle。夹具在 setup
%% 阶段 COMMIT（含 push token 行），业务消息行按用例落库、不回收（marker 库随
%% 套件结束 DROP）。
%%
%% 关键口径：
%%   * eunit VM 没有 pooler / 没有 imboy app：把 elib_pg 的**池化入口** shim 到真
%%     marker 连接（`with_tx/1`、`query/2`、`execute/2`），SQL 文本与参数仍由生产
%%     repo/logic 构造（测试不重写 SQL 副本）。
%%   * `imboy_syn:count_user/1`（在线判定）与 `elib_async` 一律 meck：在线/离线
%%     是外部事实，异步壳同步化便于「恰一次」断言。
%%   * provider HTTP **全程 meck**：绝不向 api.jpush.cn 发任何请求；任何真实出网
%%     尝试都会让用例显式失败（unexpected_real_http_post）。
%%
%% ID 段：997xxx（本 run 独立 marker 库；995/996 属 push_token_contract，991 属
%% EPGZ-04）。marker 库供给失败（环境/配置/迁移任一不可用）显式 FAIL，无静默 skip。
%%
%% 运行：make eunit-local t=enterprise_message_push_pg_tests

-module(enterprise_message_push_pg_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

%% ---- 夹具（997 段独立 ID） ----
-define(ORG_A, 997101).
-define(OWNER, 997001).
-define(H_SEND, 997011).
-define(H_R1, 997012).
-define(H_R2, 997013).
-define(PRIN, 997014).
-define(WS_A, 997201).
-define(GRP, 997301).
-define(GM_BASE, 997401).

-define(EXT_SEND, <<"ext997-send">>).
-define(EXT_R1, <<"ext997-r1">>).
-define(EXT_R2, <<"ext997-r2">>).

-define(SCOPES_FULL, [
    <<"application:read">>,
    <<"identities:read">>,
    <<"identities:write">>,
    <<"groups:write">>,
    <<"files:write">>,
    <<"messages:send">>,
    <<"messages:send_as_human">>,
    <<"webhooks:manage">>
]).

%% 推送 token 夹具（每个 device 一个 token）
-define(D_R1A, <<"did997-r1a">>).
-define(D_R1B, <<"did997-r1b">>).
-define(D_R1C, <<"did997-r1c">>).
-define(D_R2A, <<"did997-r2a">>).
-define(D_R2B, <<"did997-r2b">>).
-define(D_PRIN, <<"did997-prin">>).
-define(T_R1A, <<"rid997-r1a">>).
-define(T_R1B, <<"rid997-r1b">>).
-define(T_R1C, <<"rid997-r1c">>).
-define(T_R2A, <<"rid997-r2a">>).
-define(T_R2B, <<"rid997-r2b">>).
-define(T_PRIN, <<"rid997-prin">>).

%% 隐私常量（push_notification_logic 的 fail-closed 不变量）
-define(PUSH_TITLE, <<"新消息"/utf8>>).
-define(PUSH_BODY, <<"发来一条消息"/utf8>>).
%% 正文探针：企业消息正文里放这个串，出现在 provider 请求体即命中泄露
-define(PII_MARKER, <<"PII997-ENTERPRISE-BODY-DO-NOT-LEAK">>).

%% 占位凭证（非真实凭据）+ RFC 2606 保留域（永不解析）
-define(TEST_APP_KEY, <<"test-jpush-appkey-placeholder">>).
-define(TEST_MASTER_SECRET, <<"test-jpush-master-secret-placeholder">>).
-define(TEST_PUSH_URL, <<"https://push.invalid.test/v3/push">>).

%%%===================================================================
%%% Fixture
%%%===================================================================

setup_conn() ->
    try
        elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3})
    catch
        _:_ -> ok
    end,
    lists:foreach(
        fun(Name) ->
            case lists:member(Name, elib_tsid:registered()) of
                true -> ok;
                false -> elib_tsid:register(Name)
            end
        end,
        [
            group_info,
            group_member,
            enterprise_message,
            enterprise_audit_event,
            push_token,
            msg_c2c,
            msg_c2g
        ]
    ),
    {ok, _} = application:ensure_all_started(throttle),
    State = inttest_marker_db:provision(#{
        env_prefix => <<"FULL07_INTTEST">>,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }),
    C = maps:get(conn, State),
    %% 池化入口 shim：整个套件期间都生效（每条用例的 with_tx 收尾不 unload 它）
    install_pool_shim(C),
    ok = exec(C, <<"BEGIN">>),
    try
        seed_matrix(C),
        ok = exec(C, <<"COMMIT">>)
    catch
        Class:Reason:Stack ->
            _ = exec_quiet(C, <<"ROLLBACK">>),
            inttest_marker_db:release(State),
            erlang:raise(Class, {fixture_seed_failed, Reason}, Stack)
    end,
    {AppA, _AppARead} = app_ids(C),
    State#{app_a => AppA}.

close_conn(State) ->
    %% 全套件级桩必须在此清干净：本套件 meck 了 imboy_syn / elib_async /
    %% push_provider_jpush*，残留会让同 VM 后续套件的 meck:new 报
    %% {already_started, Pid}（全量 eunit 里会误伤他人套件）。
    uninstall_runtime_stubs(),
    try
        meck:unload(elib_pg)
    catch
        _:_ -> ok
    end,
    inttest_marker_db:release(State),
    ok.

%% ------------------------------------------------------------------
%% 池化入口 shim：elib_pg 的池化接口 → 真 marker 连接
%% ------------------------------------------------------------------
install_pool_shim(Conn) ->
    meck:new(elib_pg, [passthrough, no_link]),
    %% 提交后推送读的是「独立事务」；这里把它绑到测试连接上（同一连接可见
    %% 已落库的行），语义等价于生产里 with_tx 拿一条池化连接。
    meck:expect(elib_pg, with_tx, 1, fun(F) -> F(Conn) end),
    meck:expect(elib_pg, execute, 2, fun(Sql, Params) ->
        elib_pg:execute(Conn, Sql, Params)
    end),
    meck:expect(elib_pg, query, 2, fun(Sql, Params) ->
        elib_pg:query(Conn, Sql, Params)
    end),
    ok.

%% 异步壳同步化 + provider 全部桩化（内含「真实出网即失败」硬门）
install_runtime_stubs() ->
    meck:new(elib_async, [passthrough, no_link]),
    meck:expect(elib_async, async, 1, fun(Fun) ->
        Fun(),
        self()
    end),
    meck:expect(elib_async, async_retry, 3, fun(Fun, _Retry, _Delay) ->
        Fun(),
        self()
    end),
    meck:new(imboy_syn, [passthrough, no_link]),
    meck:new(push_provider_jpush_http, [no_link, unstick]),
    meck:expect(push_provider_jpush_http, post, 3, fun(_U, _H, _B) ->
        erlang:error({unexpected_real_http_post, blocked_by_test})
    end),
    ok.

uninstall_runtime_stubs() ->
    lists:foreach(
        fun(M) ->
            %% meck 未 mock 该模块时 unload 会报错——「已卸载」与「没装过」
            %% 对调用方同义，这里吞掉。
            try
                meck:unload(M)
            catch
                _:_ -> ok
            end
        end,
        [elib_async, imboy_syn, push_provider_jpush_http, push_provider_jpush]
    ),
    ok.

%% 每个用例一个干净桩集
prepare_case() ->
    uninstall_runtime_stubs(),
    install_runtime_stubs(),
    %% 默认：全部在线（用例显式覆盖为离线）
    meck:expect(imboy_syn, count_user, fun(_) -> 1 end),
    %% provider adapter：默认只计数（真实实现由 wire 用例单独接管）
    meck:new(push_provider_jpush, [passthrough, no_link]),
    meck:expect(push_provider_jpush, send, 3, fun(Token, Title, Body) ->
        self() ! {push_send, Token, Title, Body},
        ok
    end),
    %% 26670a37 起通道携带固定路由 Data，DS 恒走 send/4——只桩 send/3 时
    %% passthrough 会撞真实 HTTP seam（套件级 post mock 直接报错），零捕获。
    meck:expect(push_provider_jpush, send, 4, fun(Token, Title, Body, _Data) ->
        self() ! {push_send, Token, Title, Body},
        ok
    end),
    ok.

%% ---- 按用例驱动：消息落库 → 提交后推送触发 ----

%% human 模式 direct：H_SEND -> H_R1
send_direct_human(C, State, Content) ->
    Input = #{
        sender_mode => <<"human">>,
        sender_user_id => ?EXT_SEND,
        recipient_user_id => ?EXT_R1,
        msg_type => <<"text">>,
        content => Content
    },
    {ok, Result} = enterprise_message_logic:direct_tx(C, ctx_a(State), Input),
    Result.

%% application 模式 direct：principal -> H_R1
send_direct_app(C, State, Content) ->
    Input = #{
        sender_mode => <<"application">>,
        recipient_user_id => ?EXT_R1,
        msg_type => <<"text">>,
        content => Content
    },
    {ok, Result} = enterprise_message_logic:direct_tx(C, ctx_a(State), Input),
    Result.

send_group_human(C, State, Content) ->
    Input = #{
        sender_mode => <<"human">>,
        sender_user_id => ?EXT_SEND,
        msg_type => <<"text">>,
        content => Content
    },
    {ok, Result} = enterprise_message_logic:group_tx(C, ctx_a(State), ?GRP, Input),
    Result.

send_group_app(C, State, Content) ->
    Input = #{
        sender_mode => <<"application">>,
        msg_type => <<"text">>,
        content => Content
    },
    {ok, Result} = enterprise_message_logic:group_tx(C, ctx_a(State), ?GRP, Input),
    Result.

%% 触发提交后推送（生产里由 enterprise_message_handler 在 {tx_ok,...} 分支调用）
fire(C, Table, Result) ->
    _ = C,
    ok = enterprise_message_logic:push_after_commit(Table, maps:get(<<"msg_id">>, Result)).

%% 收集 provider 调用（Token/Title/Body）
take_sends() ->
    take_sends([]).

take_sends(Acc) ->
    receive
        {push_send, T, Ti, B} -> take_sends([{T, Ti, B} | Acc])
    after 0 -> lists:reverse(Acc)
    end.

drain() ->
    _ = take_sends(),
    ok.

with_tx(C, TestFun) ->
    ?_test(begin
        ok = exec(C, <<"BEGIN">>),
        try
            TestFun(C),
            ok
        after
            exec_quiet(C, <<"ROLLBACK">>)
        end
    end).

%% 不使用事务包装的用例（消息行保留在 marker 库里；库随套件 DROP）
plain(TestFun) ->
    ?_test(TestFun()).

exec(C, IoData) ->
    Sql = iolist_to_binary(IoData),
    case elib_pg:query(C, Sql, []) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({sql_error, Reason, Sql})
    end.

exec_quiet(C, IoData) ->
    _ = elib_pg:query(C, iolist_to_binary(IoData), []),
    ok.

one(C, Sql, Params) ->
    case elib_pg:query(C, Sql, Params) of
        {ok, [Row | _]} -> Row;
        {ok, []} -> #{};
        {error, Reason} -> erlang:error({sql_error, Reason, Sql})
    end.

%% ---- 夹具矩阵 ----

seed_matrix(C) ->
    seed_user(C, ?OWNER, 0, 1),
    seed_user(C, ?H_SEND, 0, 1),
    seed_user(C, ?H_R1, 0, 1),
    seed_user(C, ?H_R2, 0, 1),
    seed_user(C, ?PRIN, 0, 1),
    seed_org(C, ?ORG_A, ?OWNER, <<"full07-org-a">>),
    lists:foreach(
        fun(Uid) -> seed_org_member(C, ?ORG_A, Uid, <<"active">>) end,
        [?H_SEND, ?H_R1, ?H_R2, ?PRIN]
    ),
    seed_workspace(C, ?WS_A, ?ORG_A, ?OWNER, <<"active">>),
    lists:foreach(
        fun(Uid) -> seed_ws_member(C, ?WS_A, Uid) end,
        [?OWNER, ?H_SEND, ?H_R1, ?PRIN]
    ),
    seed_group(C, ?GRP, ?WS_A, ?OWNER, <<"workspace">>, 1),
    %% 群成员：sender / R1 / principal（R2 不是群成员——群发不得外溢）
    seed_group_member(C, ?GM_BASE, ?GRP, ?H_SEND, 1),
    seed_group_member(C, ?GM_BASE + 1, ?GRP, ?H_R1, 1),
    seed_group_member(C, ?GM_BASE + 2, ?GRP, ?PRIN, 1),
    seed_group_member(C, ?GM_BASE + 3, ?GRP, ?OWNER, 4),
    {ok, AppA} = enterprise_application_repo:create_tx(
        C, ?ORG_A, <<"full07-oa-a">>, <<"full07 org A oa"/utf8>>, {?PRIN, ?SCOPES_FULL}
    ),
    ok = seed_mapping(C, ?ORG_A, maps:get(<<"id">>, AppA), ?EXT_SEND, ?H_SEND),
    ok = seed_mapping(C, ?ORG_A, maps:get(<<"id">>, AppA), ?EXT_R1, ?H_R1),
    ok = seed_mapping(C, ?ORG_A, maps:get(<<"id">>, AppA), ?EXT_R2, ?H_R2),
    %% push token：R1 三台设备全活跃；R2 两台，其中一台已停用；principal 一台
    register_token(C, ?H_R1, ?D_R1A, ?T_R1A),
    register_token(C, ?H_R1, ?D_R1B, ?T_R1B),
    register_token(C, ?H_R1, ?D_R1C, ?T_R1C),
    register_token(C, ?H_R2, ?D_R2A, ?T_R2A),
    register_token(C, ?H_R2, ?D_R2B, ?T_R2B),
    ok = exec(C, [
        <<"UPDATE ", (push_token_repo:tablename())/binary, " SET status = 0 WHERE user_id = ">>,
        integer_to_binary(?H_R2),
        <<" AND device_id = '">>,
        ?D_R2B,
        <<"'">>
    ]),
    register_token(C, ?PRIN, ?D_PRIN, ?T_PRIN),
    ok.

register_token(C, Uid, DeviceId, Token) ->
    {ok, _} = push_token_repo:upsert(Uid, DeviceId, <<"android">>, <<"jpush">>, Token),
    _ = C,
    ok.

seed_user(C, Uid, AccountType, Status) ->
    exec(C, [
        <<"INSERT INTO \"user\" (id, password, account, account_type, status, reg_ip, reg_cosv) VALUES (">>,
        integer_to_binary(Uid),
        ", 'x', 't997_u",
        integer_to_binary(Uid),
        "', ",
        integer_to_binary(AccountType),
        ", ",
        integer_to_binary(Status),
        <<", '127.0.0.1', 'x')">>
    ]).

seed_org(C, OrgId, OwnerUid, Name) ->
    exec(C, [
        <<"INSERT INTO organization (id, name, owner_id, status, branding, settings, created_at, updated_at) VALUES (">>,
        integer_to_binary(OrgId),
        ", '",
        Name,
        "', ",
        integer_to_binary(OwnerUid),
        <<", 'active', '{}'::jsonb, '{}'::jsonb, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_org_member(C, OrgId, Uid, Status) ->
    exec(C, [
        <<"INSERT INTO organization_member (organization_id, user_id, role, status, joined_at, created_at, updated_at) VALUES (">>,
        integer_to_binary(OrgId),
        ", ",
        integer_to_binary(Uid),
        ", 'member', '",
        Status,
        <<"', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_workspace(C, WsId, OrgId, OwnerUid, Status) ->
    exec(C, [
        <<"INSERT INTO workspace (id, name, owner_id, status, organization_id, created_at, updated_at) VALUES (">>,
        integer_to_binary(WsId),
        ", 't997_ws_",
        integer_to_binary(WsId),
        "', ",
        integer_to_binary(OwnerUid),
        ", '",
        Status,
        "', ",
        integer_to_binary(OrgId),
        <<", CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_ws_member(C, WsId, Uid) ->
    exec(C, [
        <<"INSERT INTO workspace_member (workspace_id, user_id, role, status, joined_at, created_at, updated_at) VALUES (">>,
        integer_to_binary(WsId),
        ", ",
        integer_to_binary(Uid),
        <<", 'member', 'active', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_group(C, Gid, WsId, OwnerUid, Scope, Status) ->
    exec(C, [
        <<"INSERT INTO \"group\" (id, type, join_limit, owner_uid, creator_uid, member_max, member_count, title, status, scope, workspace_id, created_at, updated_at) VALUES (">>,
        integer_to_binary(Gid),
        <<", 2, 3, ">>,
        integer_to_binary(OwnerUid),
        <<", ">>,
        integer_to_binary(OwnerUid),
        <<", 500, 4, 't997_g_">>,
        integer_to_binary(Gid),
        "', ",
        integer_to_binary(Status),
        <<", '">>,
        Scope,
        <<"', ">>,
        integer_to_binary(WsId),
        <<", CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_group_member(C, GmId, Gid, Uid, Role) ->
    exec(C, [
        <<"INSERT INTO group_member (id, group_id, user_id, role, status, created_at, updated_at) VALUES (">>,
        integer_to_binary(GmId),
        <<", ">>,
        integer_to_binary(Gid),
        <<", ">>,
        integer_to_binary(Uid),
        <<", ">>,
        integer_to_binary(Role),
        <<", 1, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_mapping(C, OrgId, AppId, Ext, Uid) ->
    case enterprise_external_identity_repo:bind_tx(C, OrgId, AppId, Ext, Uid) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({seed_mapping_failed, Ext, Reason})
    end.

app_ids(C) ->
    A = one(
        C, <<"SELECT id FROM enterprise_application WHERE application_key = 'full07-oa-a'">>, []
    ),
    {maps:get(<<"id">>, A), null}.

ctx_a(State) ->
    #{
        organization_id => ?ORG_A,
        application_id => maps:get(app_a, State),
        granted_scopes => ?SCOPES_FULL,
        principal_user_id => ?PRIN
    }.

%% 离线/在线控制：count_user 桩按 uid 给答案（在 OnlineUids 里 = 1，其余 = 0；
%% 0 才是「完全离线」——与生产判定 `imboy_syn:count_user(Uid) =:= 0` 同口径）。
set_online(OnlineUids) ->
    meck:expect(imboy_syn, count_user, fun(Uid) ->
        case lists:member(Uid, OnlineUids) of
            true -> 1;
            false -> 0
        end
    end),
    ok.

%%%===================================================================
%%% ① direct：离线收件人多设备恰一次 / 在线零推送 / 停用 token 零推送
%%%===================================================================

direct_human_offline_multi_device_exactly_once(C, State) ->
    prepare_case(),
    set_online([?H_SEND]),
    drain(),
    Result = send_direct_human(C, State, <<"hello-997">>),
    fire(C, <<"msg_c2c">>, Result),
    Sends = take_sends(),
    %% 恰一条推送 per 设备（3 台活跃设备 → 3 次），且逐条为常量文案
    ?assertEqual(
        lists:sort([?T_R1A, ?T_R1B, ?T_R1C]),
        lists:sort([T || {T, _, _} <- Sends])
    ),
    ?assertEqual(3, length(Sends)),
    lists:foreach(
        fun({_T, Title, Body}) ->
            ?assertEqual(?PUSH_TITLE, Title),
            ?assertEqual(?PUSH_BODY, Body)
        end,
        Sends
    ),
    %% 正文探针：推送通道里没有消息正文的任何痕迹
    lists:foreach(
        fun({_T, Title, Body}) ->
            ?assertEqual(nomatch, binary:match(Title, [?PII_MARKER])),
            ?assertEqual(nomatch, binary:match(Body, [?PII_MARKER]))
        end,
        Sends
    ),
    ok.

direct_online_recipient_zero_push(C, State) ->
    prepare_case(),
    %% 收件人在线（单设备在线也算在线——与 C2C 同一口径）
    set_online([?H_R1]),
    drain(),
    Result = send_direct_human(C, State, <<"online-997">>),
    fire(C, <<"msg_c2c">>, Result),
    ?assertEqual([], take_sends()),
    ok.

direct_deactivated_token_zero_push(C, State) ->
    prepare_case(),
    %% R2 离线但唯一活跃 token 已停用（D_R2B 在夹具里 status=0）
    set_online([?H_SEND]),
    drain(),
    Input = #{
        sender_mode => <<"human">>,
        sender_user_id => ?EXT_SEND,
        recipient_user_id => ?EXT_R2,
        msg_type => <<"text">>,
        content => <<"deactivated-997">>
    },
    {ok, Result} = enterprise_message_logic:direct_tx(C, ctx_a(State), Input),
    fire(C, <<"msg_c2c">>, Result),
    Sends = take_sends(),
    %% 只有活跃的 D_R2A 收到；停用的 D_R2B 零投递
    ?assertEqual([?T_R2A], [T || {T, _, _} <- Sends]),
    ?assertEqual(nomatch, binary:match(term_to_binary(Sends), [?T_R2B])),
    ok.

direct_application_mode_sender_is_principal(C, State) ->
    prepare_case(),
    set_online([?H_SEND]),
    drain(),
    Result = send_direct_app(C, State, <<"app-mode-997">>),
    %% from_id = principal（应用主体），推送照常触发
    Row = one(
        C, <<"SELECT from_id, e2ee FROM msg_c2c WHERE msg_id = $1">>, [
            maps:get(<<"msg_id">>, Result)
        ]
    ),
    ?assertEqual(?PRIN, maps:get(<<"from_id">>, Row)),
    ?assertEqual(null, maps:get(<<"e2ee">>, Row)),
    fire(C, <<"msg_c2c">>, Result),
    ?assertEqual(3, length(take_sends())),
    ok.

%%%===================================================================
%%% ② group：离线成员恰一次 / 发送者永不自收 / 群成员边界
%%%===================================================================

group_offline_members_exactly_once_sender_excluded(C, State) ->
    prepare_case(),
    %% 发送者 H_SEND 与 R1、PRIN 全部离线；OWNER 在线
    set_online([?OWNER]),
    drain(),
    Result = send_group_human(C, State, <<"group-997">>),
    fire(C, <<"msg_c2g">>, Result),
    Sends = take_sends(),
    %% 群成员 = H_SEND(sender,离线) / R1(离线) / PRIN(离线) / OWNER(在线,群主 role4)
    %% 期望：R1 三设备 + PRIN 一台 = 4；发送者 H_SEND 自己**零**推送；
    %% 在线成员 OWNER 零推送；非群成员 R2 零推送。
    ?assertEqual(
        lists:sort([?T_R1A, ?T_R1B, ?T_R1C, ?T_PRIN]),
        lists:sort([T || {T, _, _} <- Sends])
    ),
    ?assertEqual(4, length(Sends)),
    lists:foreach(
        fun({_T, Title, Body}) ->
            ?assertEqual(?PUSH_TITLE, Title),
            ?assertEqual(?PUSH_BODY, Body)
        end,
        Sends
    ),
    ok.

group_application_mode_principal_never_self_receives(C, State) ->
    prepare_case(),
    %% principal 作为 from_id（application 模式），本身是群成员且离线 —— 仍不自收
    set_online([?OWNER]),
    drain(),
    Result = send_group_app(C, State, <<"group-app-997">>),
    Row = one(
        C, <<"SELECT from_id, e2ee FROM msg_c2g WHERE msg_id = $1">>, [
            maps:get(<<"msg_id">>, Result)
        ]
    ),
    ?assertEqual(?PRIN, maps:get(<<"from_id">>, Row)),
    ?assertEqual(null, maps:get(<<"e2ee">>, Row)),
    fire(C, <<"msg_c2g">>, Result),
    Sends = take_sends(),
    %% principal(=sender) 零推送；只有 R1 的三台设备
    ?assertEqual(
        lists:sort([?T_R1A, ?T_R1B, ?T_R1C]),
        lists:sort([T || {T, _, _} <- Sends])
    ),
    ?assertEqual(nomatch, binary:match(term_to_binary(Sends), [?T_PRIN])),
    ok.

group_all_online_zero_push(C, State) ->
    prepare_case(),
    set_online([?OWNER, ?H_R1, ?PRIN, ?H_SEND]),
    drain(),
    Result = send_group_human(C, State, <<"group-online-997">>),
    fire(C, <<"msg_c2g">>, Result),
    ?assertEqual([], take_sends()),
    ok.

%%%===================================================================
%%% ③ provider wire：键集封闭 + 无正文/PII/device_id/uid + 逐字节稳定
%%%===================================================================

wire_payload_closed_and_pii_free(C, State) ->
    prepare_case(),
    set_online([?H_SEND]),
    application:set_env(imboy, push, [
        {jpush_app_key, ?TEST_APP_KEY},
        {jpush_master_secret, ?TEST_MASTER_SECRET},
        {jpush_push_url, ?TEST_PUSH_URL}
    ]),
    try
        %% 让 adapter 走真实实现（默认桩只计数），只桩 HTTP seam ——
        %% 验的是 adapter 真正发出的 wire body。DS 恒走 send/4（26670a37），
        %% 故穿透桩必须桩在 4 元上。
        meck:expect(push_provider_jpush, send, 4, fun(Token, Title, Body, _Data) ->
            meck:passthrough([Token, Title, Body, #{}])
        end),
        meck:expect(push_provider_jpush_http, post, fun(Url, Headers, Body) ->
            self() ! {wire, {Url, Headers, Body}},
            {ok, 200, <<"{\"sendno\":\"1\",\"msg_id\":\"1\"}">>}
        end),
        drain(),
        %% 正文里放探针：企业消息正文绝不能进推送请求体
        Result = send_direct_human(C, State, ?PII_MARKER),
        fire(C, <<"msg_c2c">>, Result),
        Wires1 = collect_wires(3),
        ?assertEqual(3, length(Wires1)),
        lists:foreach(fun assert_wire_closed/1, Wires1),
        ByToken1 = wire_by_token(Wires1),
        %% 三台设备各一条，路由到各自的 registration_id
        ?assertEqual(lists:sort([?T_R1A, ?T_R1B, ?T_R1C]), lists:sort(maps:keys(ByToken1))),
        %% 去掉 token 之后三条 body **逐字节完全相同**：payload 是 token 的纯
        %% 函数，正文/密文/device_id/uid/origin 没有任何掺入通道。
        ?assertEqual(
            1,
            length(
                lists:usort([
                    binary:replace(B, T, <<"<TOKEN>">>, [global])
                 || {T, B} <- maps:to_list(ByToken1)
                ])
            )
        ),
        %% 同一条消息重复触发（同一 token）：wire body 逐字节一致（无时间戳/
        %% 无随机/无序号等非确定性字段）
        fire(C, <<"msg_c2c">>, Result),
        ByToken2 = wire_by_token(collect_wires(3)),
        ?assertEqual(ByToken1, ByToken2),
        ok
    after
        application:unset_env(imboy, push),
        drain()
    end.

%% 把 wire 列表按 registration_id 归成 #{Token => Body}
wire_by_token(Wires) ->
    lists:foldl(
        fun({_Url, _Headers, Body}, Acc) ->
            Json = jsone:decode(Body),
            [Token] = maps:get(<<"registration_id">>, maps:get(<<"audience">>, Json)),
            Acc#{Token => Body}
        end,
        #{},
        Wires
    ).

assert_wire_closed({Url, Headers, Body}) ->
    ?assertEqual(?TEST_PUSH_URL, Url),
    ?assert(lists:keymember(<<"authorization">>, 1, Headers)),
    Json = jsone:decode(Body),
    %% 顶层逐键封闭
    TopAllowed = [<<"platform">>, <<"audience">>, <<"notification">>, <<"options">>],
    ?assert(lists:all(fun(K) -> lists:member(K, TopAllowed) end, maps:keys(Json))),
    ?assertEqual([<<"android">>], maps:get(<<"platform">>, Json)),
    ?assertNot(maps:is_key(<<"extras">>, Json)),
    Notif = maps:get(<<"notification">>, Json),
    ?assertEqual([<<"android">>], maps:keys(Notif)),
    Android = maps:get(<<"android">>, Notif),
    ?assert(
        lists:all(
            fun(K) -> lists:member(K, [<<"title">>, <<"alert">>, <<"builder_id">>]) end,
            maps:keys(Android)
        )
    ),
    ?assertEqual(?PUSH_TITLE, maps:get(<<"title">>, Android)),
    ?assertEqual(?PUSH_BODY, maps:get(<<"alert">>, Android)),
    %% 隐私硬门：正文探针 / device_id / uid / 设计器 id / origin 全不得出现
    Probes = [
        ?PII_MARKER,
        <<"did997">>,
        <<"997012">>,
        <<"997011">>,
        <<"enterprise_application">>,
        <<"sender_kind">>
    ],
    lists:foreach(
        fun(Probe) -> ?assertEqual(nomatch, binary:match(Body, [Probe])) end,
        Probes
    ),
    ok.

collect_wires(N) -> collect_wires(N, []).
collect_wires(0, Acc) -> lists:reverse(Acc);
collect_wires(N, Acc) -> collect_wires(N - 1, [recv_wire() | Acc]).

recv_wire() ->
    receive
        {wire, V} -> V
    after 2000 -> erlang:error(wire_capture_timeout)
    end.

%%%===================================================================
%%% ④ 负例：未落库 / 未知表 / 非法输入 → 零推送且不炸（fail-safe）
%%%===================================================================

unknown_message_no_push(C, _State) ->
    prepare_case(),
    set_online([]),
    drain(),
    ?assertEqual(ok, enterprise_message_logic:push_after_commit(<<"msg_c2c">>, <<"997-nope">>)),
    ?assertEqual(ok, enterprise_message_logic:push_after_commit(<<"msg_c2g">>, <<"997-nope">>)),
    ?assertEqual(ok, enterprise_message_logic:push_after_commit(<<"msg_c2s">>, <<"997-nope">>)),
    ?assertEqual(ok, enterprise_message_logic:push_after_commit(<<"msg_c2c">>, not_a_binary)),
    ?assertEqual([], take_sends()),
    %% 数据库里也确实没有这条消息（推送不凭空发生）
    Row = one(C, <<"SELECT count(*) AS n FROM msg_c2c WHERE msg_id = '997-nope'">>, []),
    ?assertEqual(0, maps:get(<<"n">>, Row)),
    ok.

%% 推送通道故障（provider 层炸）不得影响已提交的消息结果
provider_failure_is_failsafe(C, State) ->
    prepare_case(),
    set_online([?H_SEND]),
    meck:expect(push_provider_jpush, send, 3, fun(_T, _Ti, _B) ->
        erlang:error({jpush_down, 997})
    end),
    Result = send_direct_human(C, State, <<"failsafe-997">>),
    %% 推送炸了也必须返回 ok（消息结果不受影响），且消息行已落库
    ?assertEqual(
        ok,
        enterprise_message_logic:push_after_commit(<<"msg_c2c">>, maps:get(<<"msg_id">>, Result))
    ),
    Row = one(
        C, <<"SELECT msg_id FROM msg_c2c WHERE msg_id = $1">>, [
            maps:get(<<"msg_id">>, Result)
        ]
    ),
    ?assertEqual(maps:get(<<"msg_id">>, Result), maps:get(<<"msg_id">>, Row)),
    ok.

%%%===================================================================
%%% Suite
%%%===================================================================

enterprise_message_push_pg_test_() ->
    {timeout, 900,
        {setup, fun setup_conn/0, fun close_conn/1, fun(State) ->
            C = maps:get(conn, State),
            %% inorder：全局 meck 与真库连接都不可并行复用。
            {inorder, [
                {"direct_human_offline_multi_device_exactly_once",
                    plain(fun() -> direct_human_offline_multi_device_exactly_once(C, State) end)},
                {"direct_online_recipient_zero_push",
                    plain(fun() -> direct_online_recipient_zero_push(C, State) end)},
                {"direct_deactivated_token_zero_push",
                    plain(fun() -> direct_deactivated_token_zero_push(C, State) end)},
                {"direct_application_mode_sender_is_principal",
                    plain(fun() -> direct_application_mode_sender_is_principal(C, State) end)},
                {"group_offline_members_exactly_once_sender_excluded",
                    plain(fun() ->
                        group_offline_members_exactly_once_sender_excluded(C, State)
                    end)},
                {"group_application_mode_principal_never_self_receives",
                    plain(fun() ->
                        group_application_mode_principal_never_self_receives(C, State)
                    end)},
                {"group_all_online_zero_push",
                    plain(fun() -> group_all_online_zero_push(C, State) end)},
                {"wire_payload_closed_and_pii_free",
                    plain(fun() -> wire_payload_closed_and_pii_free(C, State) end)},
                {"unknown_message_no_push", plain(fun() -> unknown_message_no_push(C, State) end)},
                {"provider_failure_is_failsafe",
                    plain(fun() -> provider_failure_is_failsafe(C, State) end)},
                %% with_tx 形式保留一条（证明事务内消息 + 提交后推送同连接可见）
                {"direct_push_inside_tx_visible_to_after_commit",
                    with_tx(C, fun(C1) ->
                        direct_human_offline_multi_device_exactly_once(C1, State)
                    end)}
            ]}
        end}}.
