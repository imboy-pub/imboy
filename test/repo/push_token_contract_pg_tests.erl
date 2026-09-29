%% push_token_contract_pg_tests
%% FULL-06 — Push token 合同完整性 + 多设备 fan-out / 离线判定 **真库**集成测试。
%%
%% 合同来源：plan-full §3.3（JPush token 生命周期 / 多设备 / 登出行为）与 §7
%% 安全硬门三条：
%%   ① Push payload 不含正文/PII；
%%   ② token 跨用户/设备不可复用；
%%   ③ logout 后旧 token 不再投递。
%%
%% 一次性 marker 库（inttest_marker_db 配方，env 前缀 FULL06_INTTEST）：空库全量
%% up（erlang_migrate strict，含本 phase 的 00000142）→ 合同 oracle。业务用例逐条
%% BEGIN ... ROLLBACK，不留数据。
%%
%% 关键口径：**生产 repo 是被测物**。eunit VM 没有 pooler，故把 elib_pg 的
%% 2 元（池化）入口 shim 到真 marker 连接——SQL 文本与参数顺序仍由
%% push_token_repo / push_notification_ds 构造，测试**不重写 SQL 副本**
%% （否则验的是副本，不是产品行为）。
%% provider 侧（JPush HTTP）一律 meck：**绝不向 api.jpush.cn 发任何请求**；
%% 凭证只用品位占位符。
%%
%% ID 段：995xxx（本 run 独立 marker 库；994 已被 enterprise_webhook_migration 占用）。
%% marker 库供给失败（环境/配置/迁移任一不可用）显式 FAIL，无静默 skip。
%%
%% 运行：make eunit-local t=push_token_contract_pg_tests

-module(push_token_contract_pg_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

%% 合成 ID（push_token 无 FK，纯隔离段）
-define(UID_A, 995001).
-define(UID_B, 995002).
-define(UID_C, 995003).
-define(D1, <<"did995-android-1">>).
-define(D2, <<"did995-android-2">>).
-define(D3, <<"did995-android-3">>).
-define(T1, <<"rid995-token-1">>).
-define(T2, <<"rid995-token-2">>).
-define(T3, <<"rid995-token-3">>).
%% GZ 期升级兼容用例专用 token（996xxx 段，会落数据不回收）
-define(T_LEGACY, <<"rid996-legacy-token">>).

%% 本 phase 新增的唯一活动 token 索引（迁移 00000142）
-define(ACTIVE_TOKEN_INDEX, <<"uq_push_token_active_token">>).
%% 既有索引（00000001）：同用户同设备仅一条活跃
-define(USER_DEVICE_INDEX, <<"uk_push_token_user_device">>).

%% 现有隐私常量（push_notification_logic 的 fail-closed 不变量）
-define(PUSH_TITLE, <<"新消息"/utf8>>).
-define(PUSH_BODY, <<"发来一条消息"/utf8>>).
%% 正文 PII 探针：任何推送正文/密文/身份片段出现在 provider 请求体即命中
-define(PII_MARKER, <<"PII995-SECRET-BODY-DO-NOT-LEAK">>).

%% 占位凭证（合同文档同款占位符，非真实凭据）
-define(TEST_APP_KEY, <<"test-jpush-appkey-placeholder">>).
-define(TEST_MASTER_SECRET, <<"test-jpush-master-secret-placeholder">>).
%% RFC 2606 保留域，永不解析
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
    case lists:member(push_token, elib_tsid:registered()) of
        true -> ok;
        false -> elib_tsid:register(push_token)
    end,
    inttest_marker_db:provision(#{
        env_prefix => <<"FULL06_INTTEST">>,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }).

close_conn(State) ->
    inttest_marker_db:release(State),
    ok.

%% ------------------------------------------------------------------
%% 池化入口 shim：elib_pg 的 2 元接口 → 真 marker 连接
%% ------------------------------------------------------------------
install_pool_shim(Conn) ->
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, execute, 2, fun(Sql, Params) ->
        elib_pg:execute(Conn, Sql, Params)
    end),
    meck:expect(elib_pg, query, 2, fun(Sql, Params) ->
        elib_pg:query(Conn, Sql, Params)
    end),
    %% list_page/2 走 elib_pg:one/2（单行投影）——池化入口一并 shim 到真连接，
    %% 否则 Admin 投影用例会掉进无 pooler 的 {noproc, {gen_server,call,[pgsql ...]}}。
    meck:expect(elib_pg, one, 2, fun(Sql, Params) ->
        elib_pg:one(Conn, Sql, Params)
    end),
    ok.

%% 逐用例桩安装（每个用例一个干净桩集；with_tx 收尾 meck:unload）
prepare_case(Conn) ->
    install_pool_shim(Conn),
    %% provider HTTP：任何真实出网调用都让用例显式失败（绝不打 api.jpush.cn）
    meck:new(push_provider_jpush_http, [no_link, unstick]),
    meck:expect(push_provider_jpush_http, post, 3, fun(_U, _H, _B) ->
        erlang:error({unexpected_real_http_post, blocked_by_test})
    end),
    %% provider adapter：捕获入参（默认成功）
    meck:new(push_provider_jpush, [passthrough, no_link]),
    capture_provider_ok(),
    %% 异步壳同步化，便于断言
    meck:new(elib_async, [passthrough, no_link]),
    meck:expect(elib_async, async_retry, 3, fun(Fun, _Retry, _Delay) ->
        Fun(),
        self()
    end),
    meck:expect(elib_async, async, 1, fun(Fun) ->
        Fun(),
        self()
    end),
    ok.

capture_provider_ok() ->
    %% CP-TD-A02/A1d：产品链路自 26670a37 起改调 send/4（Token,Title,Body,Data，
    %% Data 为固定常量路由键值）；旧 send/3 桩在 meck passthrough 下穿透真实现，
    %% eunit 无凭证 → not_configured fail-closed → 永远收不到 provider_send 消息。
    meck:expect(push_provider_jpush, send, 4, fun(Token, Title, Body, Data) ->
        self() ! {provider_send, {Token, Title, Body, Data}},
        ok
    end).

%% 逐用例事务包装（BEGIN/ROLLBACK 在最外层测试体内，不留数据）
with_tx(Conn, TestFun) ->
    ?_test(begin
        prepare_case(Conn),
        ok = exec(Conn, <<"BEGIN">>),
        try
            TestFun(Conn),
            ok
        after
            exec_quiet(Conn, <<"ROLLBACK">>),
            meck:unload()
        end
    end).

%% 预期会产生 SQL 错误的负例必须放 SAVEPOINT（失败会让事务进入 aborted 态）
in_savepoint(Conn, Fun) ->
    ok = exec(Conn, <<"SAVEPOINT push995_sp">>),
    try
        Fun()
    after
        exec_quiet(Conn, <<"ROLLBACK TO SAVEPOINT push995_sp">>),
        exec_quiet(Conn, <<"RELEASE SAVEPOINT push995_sp">>)
    end.

%% 真库 SQL 小工具
exec(C, IoData) ->
    Sql = iolist_to_binary(IoData),
    case elib_pg:query(C, Sql, []) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({sql_error, Reason, Sql})
    end.

exec_quiet(C, IoData) ->
    _ = elib_pg:query(C, iolist_to_binary(IoData), []),
    ok.

all(C, Sql, Params) ->
    case elib_pg:query(C, Sql, Params) of
        {ok, Rows} when is_list(Rows) -> Rows;
        {error, Reason} -> erlang:error({sql_error, Reason, Sql})
    end.

one(C, Sql, Params) ->
    case all(C, Sql, Params) of
        [Row | _] -> Row;
        [] -> #{}
    end.

scalar(C, Sql, Params) ->
    case maps:values(one(C, Sql, Params)) of
        [V | _] -> V;
        [] -> undefined
    end.

db_error(Result) ->
    case Result of
        {error, #error{codename = Code}} -> Code;
        Other -> Other
    end.

%% 直接 INSERT 一行（负例用，绕过 repo）
raw_insert(C, Uid, DeviceId, DeviceType, Platform, Token, Status) ->
    R = elib_pg:query(
        C,
        <<
            "INSERT INTO public.push_token"
            " (id, user_id, device_id, device_type, platform, token, status)"
            " VALUES ($1, $2, $3, $4, $5, $6, $7)"
        >>,
        [elib_tsid:generate(push_token), Uid, DeviceId, DeviceType, Platform, Token, Status]
    ),
    case db_error(R) of
        {ok, _} -> ok;
        Other -> Other
    end.

%% 该用户当前活跃 token（真库读，经生产 repo 的 SQL）
active_tokens(Uid) ->
    case push_token_repo:list_by_uid(Uid) of
        {ok, Rows} -> [maps:get(<<"token">>, R) || R <- Rows];
        {error, Reason} -> erlang:error({list_by_uid_failed, Reason})
    end.

active_token_count(C, Token) ->
    scalar(
        C,
        <<"SELECT count(*) FROM public.push_token WHERE token = $1 AND status = 1">>,
        [Token]
    ).

active_token_owner(C, Token) ->
    scalar(
        C,
        <<"SELECT user_id FROM public.push_token WHERE token = $1 AND status = 1">>,
        [Token]
    ).

active_token_device(C, Token) ->
    scalar(
        C,
        <<"SELECT device_id FROM public.push_token WHERE token = $1 AND status = 1">>,
        [Token]
    ).

%% provider 入参（drain）
take_provider_sends() ->
    drain(provider_send).

drain(Tag) ->
    receive
        {Tag, V} -> [V | drain(Tag)]
    after 0 -> []
    end.

register(Uid, DeviceId, DeviceType, Platform, Token) ->
    ?assertEqual(
        ok,
        push_notification_logic:register_token(Uid, DeviceId, DeviceType, Platform, Token)
    ).

%%%===================================================================
%%% 测试生成
%%%===================================================================

push_token_contract_test_() ->
    {timeout, 900,
        {setup, fun setup_conn/0, fun close_conn/1, fun(State) ->
            C = maps:get(conn, State),
            {inorder, [
                {"schema_head_and_active_token_index", head_schema_test(C)},
                {"field_contract_device_type_and_platform",
                    with_tx(C, fun field_contract_oracle/1)},
                {"cross_user_token_rebind_detaches_previous_owner",
                    with_tx(C, fun cross_user_rebind_oracle/1)},
                {"cross_device_same_user_token_single_active_row",
                    with_tx(C, fun cross_device_rebind_oracle/1)},
                {"db_level_unique_active_token_index", with_tx(C, fun unique_index_oracle/1)},
                {"logout_detaches_token_and_fanout_delivers_nothing",
                    with_tx(C, fun logout_oracle/1)},
                {"multi_device_fanout_and_offline_decision", with_tx(C, fun fanout_oracle/1)},
                {"jpush_wire_payload_closed_and_pii_free", with_tx(C, fun payload_oracle/1)},
                {"notify_offline_apis_unreachable_from_src", notify_reachability_test()},
                {"admin_list_page_never_projects_token_plaintext",
                    with_tx(C, fun(C1) -> admin_projection_oracle(C1) end)},
                {"migration_142_down_up_cycle", {timeout, 400, down_up_cycle_test(State)}},
                {"migration_142_gz_era_duplicate_upgrade_compat",
                    {timeout, 400, upgrade_compat_test(State)}}
            ]}
        end}}.

%%%===================================================================
%%% ⓪ head schema
%%%===================================================================

head_schema_test(C) ->
    ?_test(begin
        {ok, Version, Dirty} = erlang_migrate:version(#{
            conn => C, dir => "priv/migrations"
        }),
        ?assertEqual(false, Dirty),
        ?assertEqual(migration_head(), Version),
        %% 活动 token 唯一索引就位，且是**部分**唯一索引（WHERE status = 1）
        Def = scalar(
            C,
            <<
                "SELECT indexdef FROM pg_indexes"
                " WHERE schemaname = 'public' AND tablename = 'push_token'"
                " AND indexname = $1"
            >>,
            [?ACTIVE_TOKEN_INDEX]
        ),
        ?assert(is_binary(Def)),
        ?assertNotEqual(nomatch, binary:match(Def, <<"UNIQUE">>)),
        ?assertNotEqual(nomatch, binary:match(Def, <<"token">>)),
        ?assertNotEqual(nomatch, binary:match(Def, <<"status = 1">>)),
        %% 既有同用户同设备唯一索引仍在（本 phase 未削弱）
        ?assert(has_index(C, ?USER_DEVICE_INDEX))
    end).

%%%===================================================================
%%% ① 字段合同：device_type 是 OS，platform 是 provider（不可互换）
%%%===================================================================

field_contract_oracle(C) ->
    %% ①a 生产 repo 的 upsert 落 android + jpush 行（真库 CHECK 接受）
    register(?UID_A, ?D1, <<"android">>, <<"jpush">>, ?T1),
    Row = one(
        C,
        <<
            "SELECT user_id, device_id, device_type, platform, token, status"
            " FROM public.push_token WHERE user_id = $1 AND device_id = $2 AND status = 1"
        >>,
        [?UID_A, ?D1]
    ),
    ?assertEqual(?UID_A, maps:get(<<"user_id">>, Row)),
    ?assertEqual(<<"android">>, maps:get(<<"device_type">>, Row)),
    ?assertEqual(<<"jpush">>, maps:get(<<"platform">>, Row)),
    ?assertEqual(?T1, maps:get(<<"token">>, Row)),
    ?assertEqual(1, maps:get(<<"status">>, Row)),

    %% ①b 两字段不可互换：DB 层直接拒绝（历史错位的类型级防线）
    ?assertEqual(
        check_violation,
        in_savepoint(C, fun() ->
            raw_insert(C, ?UID_A, ?D2, <<"jpush">>, <<"android">>, ?T2, 1)
        end)
    ),
    %% ①c device_type 越界值（桌面客户端值）拒绝
    ?assertEqual(
        check_violation,
        in_savepoint(C, fun() ->
            raw_insert(C, ?UID_A, ?D2, <<"macos">>, <<"jpush">>, ?T2, 1)
        end)
    ),
    %% ①d 缺 device_type（旧版客户端 body）在 DB 层不可能成立
    ?assertEqual(
        not_null_violation,
        in_savepoint(C, fun() ->
            db_error(
                elib_pg:query(
                    C,
                    <<
                        "INSERT INTO public.push_token"
                        " (id, user_id, device_id, platform, token, status)"
                        " VALUES ($1, $2, $3, $4, $5, 1)"
                    >>,
                    [elib_tsid:generate(push_token), ?UID_A, ?D2, <<"jpush">>, ?T2]
                )
            )
        end)
    ),
    %% ①e 既有通道零回归：android+fcm、ios+apns 仍可落库
    ?assertEqual(ok, raw_insert(C, ?UID_A, ?D2, <<"android">>, <<"fcm">>, ?T2, 1)),
    ?assertEqual(ok, raw_insert(C, ?UID_A, ?D3, <<"ios">>, <<"apns">>, ?T3, 1)),
    ok.

%%%===================================================================
%%% ② token 跨用户/设备不可复用
%%%===================================================================

cross_user_rebind_oracle(C) ->
    %% uid A 在 D1 上持有 T1
    register(?UID_A, ?D1, <<"android">>, <<"jpush">>, ?T1),
    ?assertEqual([?T1], active_tokens(?UID_A)),

    %% uid B 登到同一台机器（同一 RegistrationID T1）
    register(?UID_B, ?D2, <<"android">>, <<"jpush">>, ?T1),

    %% 合同：同一 token 同时只能有一个活跃主人
    ?assertEqual(1, active_token_count(C, ?T1)),
    ?assertEqual([], active_tokens(?UID_A)),
    ?assertEqual([?T1], active_tokens(?UID_B)),
    ?assertEqual(?UID_B, active_token_owner(C, ?T1)),

    %% 端到端：给 A 推送一个请求都不能命中（A 已无活跃设备）
    ?assertEqual(ok, push_notification_ds:send_to_user(?UID_A, ?PUSH_TITLE, ?PUSH_BODY)),
    ?assertEqual([], take_provider_sends()),
    %% 给 B 推送命中且只命中 T1 一次
    ?assertEqual(ok, push_notification_ds:send_to_user(?UID_B, ?PUSH_TITLE, ?PUSH_BODY)),
    %% send/4：Data 为固定常量路由键值（不允许动态内容，键值均 binary）。
    %% assertMatch 模式变量不外溢且 drain 只能一次：先形状断言，再显式绑定 Data0。
    SendsB = take_provider_sends(),
    ?assertMatch([{?T1, ?PUSH_TITLE, ?PUSH_BODY, _Data0}], SendsB),
    [{?T1, ?PUSH_TITLE, ?PUSH_BODY, Data0}] = SendsB,
    ?assert(is_map(Data0)),
    ?assert(lists:all(fun(K) -> is_binary(K) end, maps:keys(Data0))),
    ok.

cross_device_rebind_oracle(C) ->
    %% 同一用户、同一 token、device_id 漂移（重装后新 device_id 而 rid 未变）
    register(?UID_A, ?D1, <<"android">>, <<"jpush">>, ?T1),
    register(?UID_A, ?D2, <<"android">>, <<"jpush">>, ?T1),
    ?assertEqual(1, active_token_count(C, ?T1)),
    %% 活跃行必须已切到新设备（否则断错行，推送仍发往旧设备）
    ?assertEqual(?D2, active_token_device(C, ?T1)),
    %% 多设备 fan-out：一条活跃 token 只发一次（重复 token 不造成重复推送）
    ?assertEqual(ok, push_notification_ds:send_to_user(?UID_A, ?PUSH_TITLE, ?PUSH_BODY)),
    ?assertEqual(1, length(take_provider_sends())),
    ok.

unique_index_oracle(C) ->
    %% DB 层兜底：绕过 repo 直插第二条活跃同 token 行 → 唯一索引仲裁
    ?assertEqual(ok, raw_insert(C, ?UID_A, ?D1, <<"android">>, <<"jpush">>, ?T1, 1)),
    ?assertEqual(
        unique_violation,
        in_savepoint(C, fun() ->
            raw_insert(C, ?UID_B, ?D2, <<"android">>, <<"jpush">>, ?T1, 1)
        end)
    ),
    %% 部分索引语义：非活跃历史行不受约束
    ?assertEqual(ok, raw_insert(C, ?UID_B, ?D2, <<"android">>, <<"jpush">>, ?T1, 0)),
    ?assertEqual(ok, raw_insert(C, ?UID_C, ?D3, <<"android">>, <<"jpush">>, ?T1, 0)),
    ?assertEqual(1, active_token_count(C, ?T1)),
    %% 断掉活跃行后即可重新绑定（可恢复，不死锁）
    ?assertEqual({ok, 1}, push_token_repo:deactivate_by_token(?T1)),
    ?assertEqual(ok, raw_insert(C, ?UID_B, ?D2, <<"android">>, <<"jpush">>, ?T1, 1)),
    ?assertEqual(?UID_B, active_token_owner(C, ?T1)),
    %% 既有同用户同设备唯一索引未被本 phase 削弱
    ?assertEqual(
        unique_violation,
        in_savepoint(C, fun() ->
            raw_insert(C, ?UID_B, ?D2, <<"android">>, <<"jpush">>, <<"rid995-other">>, 1)
        end)
    ),
    ok.

%%%===================================================================
%%% ③ logout 后旧 token 不再投递
%%%===================================================================

logout_oracle(C) ->
    register(?UID_A, ?D1, <<"android">>, <<"jpush">>, ?T1),
    register(?UID_A, ?D2, <<"android">>, <<"jpush">>, ?T2),
    ?assertEqual(2, length(active_tokens(?UID_A))),

    %% 单设备登出：只断该设备，另一台仍在
    ?assertEqual(ok, push_notification_logic:unregister_token(?UID_A, ?D1)),
    ?assertEqual([?T2], active_tokens(?UID_A)),
    ?assertEqual(0, active_token_count(C, ?T1)),
    ?assertEqual(ok, push_notification_ds:send_to_user(?UID_A, ?PUSH_TITLE, ?PUSH_BODY)),
    ?assertEqual([?T2], [T || {T, _, _, _} <- take_provider_sends()]),

    %% 幂等：重复登出同一设备不再改动任何行
    ?assertEqual(ok, push_notification_logic:unregister_token(?UID_A, ?D1)),
    ?assertEqual([?T2], active_tokens(?UID_A)),
    %% 未注册过的设备登出：零行受影响，也不误伤其他行
    ?assertEqual(ok, push_notification_logic:unregister_token(?UID_A, ?D3)),
    ?assertEqual([?T2], active_tokens(?UID_A)),

    %% 全部登出：完全离线，fan-out 一个请求都不发
    ?assertEqual(ok, push_notification_logic:unregister_token(?UID_A, ?D2)),
    ?assertEqual([], active_tokens(?UID_A)),
    ?assertEqual(ok, push_notification_ds:send_to_user(?UID_A, ?PUSH_TITLE, ?PUSH_BODY)),
    ?assertEqual([], take_provider_sends()),
    %% 且 token 可被下一个登录者接管（不是永久烧毁）
    register(?UID_B, ?D2, <<"android">>, <<"jpush">>, ?T2),
    ?assertEqual([?T2], active_tokens(?UID_B)),
    ok.

%%%===================================================================
%%% ④ 多设备 fan-out 与离线判定
%%%===================================================================

fanout_oracle(C) ->
    %% 显式 fail-closed：FCM/APNs 均未配置 → 除 JPush 适配器（已桩）外零出网
    application:unset_env(imboy, push),
    %% 三台活跃设备（同一 user 的 3 台 Android 设备）
    register(?UID_A, ?D1, <<"android">>, <<"jpush">>, ?T1),
    register(?UID_A, ?D2, <<"android">>, <<"jpush">>, ?T2),
    register(?UID_A, ?D3, <<"android">>, <<"jpush">>, ?T3),
    ?assertEqual(lists:sort([?T1, ?T2, ?T3]), lists:sort(active_tokens(?UID_A))),

    %% 正向：每台活跃设备恰好发一次
    ?assertEqual(ok, push_notification_ds:send_to_user(?UID_A, ?PUSH_TITLE, ?PUSH_BODY)),
    ?assertEqual(
        lists:sort([?T1, ?T2, ?T3]),
        lists:sort([T || {T, _, _, _} <- take_provider_sends()])
    ),

    %% 逐行按 platform 分派：fcm 行不得被投给 JPush 适配器（跨通道不串台）
    ?assertEqual(
        ok, raw_insert(C, ?UID_A, <<"did995-fcm">>, <<"android">>, <<"fcm">>, <<"fcm995-t">>, 1)
    ),
    ?assertEqual(ok, push_notification_ds:send_to_user(?UID_A, ?PUSH_TITLE, ?PUSH_BODY)),
    ?assertEqual(
        lists:sort([?T1, ?T2, ?T3]),
        lists:sort([T || {T, _, _, _} <- take_provider_sends()])
    ),

    %% 失效 token：T1 的 provider 报 1003 → 下线 T1，T2/T3 不受影响
    meck:expect(push_provider_jpush, send, fun
        (?T1, _T, _B, _Data) -> {error, {jpush_error, invalid_token}};
        (_Tk, _T, _B, _Data) -> ok
    end),
    ?assertEqual(ok, push_notification_ds:send_to_user(?UID_A, ?PUSH_TITLE, ?PUSH_BODY)),
    ?assertEqual(0, active_token_count(C, ?T1)),
    %% fcm 行仍在活跃（未配置 → not_configured，不当成失效 token 下线）
    ?assertEqual(
        lists:sort([?T2, ?T3, <<"fcm995-t">>]),
        lists:sort(active_tokens(?UID_A))
    ),
    capture_provider_ok(),
    %% 再发一次：失效 token 不再被重试（只剩两台）
    ?assertEqual(ok, push_notification_ds:send_to_user(?UID_A, ?PUSH_TITLE, ?PUSH_BODY)),
    ?assertEqual(
        lists:sort([?T2, ?T3]),
        lists:sort([T || {T, _, _, _} <- take_provider_sends()])
    ),

    %% 无 token 用户：no-op，不炸
    ?assertEqual(ok, push_notification_ds:send_to_user(?UID_C, ?PUSH_TITLE, ?PUSH_BODY)),
    ?assertEqual([], take_provider_sends()),

    %% 离线判定（真正触发点）：多设备在线 = 在线 → 不推送
    meck:new(imboy_syn, [passthrough, no_link]),
    meck:expect(imboy_syn, count_user, fun
        (?UID_A) -> 2;
        (?UID_B) -> 0
    end),
    ?assertEqual(ok, push_notification_logic:notify_offline_user(?UID_A, ?PUSH_TITLE, ?PUSH_BODY)),
    ?assertEqual([], take_provider_sends()),
    %% 发消息给在线接收方：同样零推送（且正文探针从未进入 provider）
    ?assertEqual(
        ok,
        push_notification_logic:maybe_push_for_c2c(?UID_B, ?UID_A, <<"text">>, ?PII_MARKER)
    ),
    ?assertEqual([], take_provider_sends()),

    %% 离线接收方：常量文案 + 该用户全部活跃设备
    register(?UID_B, ?D1, <<"android">>, <<"jpush">>, <<"rid995-b1">>),
    ?assertEqual(
        ok,
        push_notification_logic:maybe_push_for_c2c(?UID_A, ?UID_B, <<"text">>, ?PII_MARKER)
    ),
    ?assertEqual([{<<"rid995-b1">>, ?PUSH_TITLE, ?PUSH_BODY, #{}}], take_provider_sends()),
    ok.

%%%===================================================================
%%% ⑤ JPush 线上 payload：键集封闭 + 无正文/PII
%%%===================================================================

payload_oracle(C) ->
    application:set_env(imboy, push, [
        {jpush_app_key, ?TEST_APP_KEY},
        {jpush_master_secret, ?TEST_MASTER_SECRET},
        {jpush_push_url, ?TEST_PUSH_URL}
    ]),
    try
        %% 让 adapter 走真实实现（prepare_case 里为「只数调用次数」桩住了它），
        %% 只桩 HTTP seam —— 验的是 adapter 真正发出的 wire body。
        %% CP-TD-A02/A1d：产品已改调 send/4，重桩须同为 /4 passthrough；
        %% c2c 路径 Data=#{} → build_payload 不加 extras，wire body 逐字节不变。
        meck:expect(push_provider_jpush, send, 4, fun(T, Ti, B, D) ->
            meck:passthrough([T, Ti, B, D])
        end),
        meck:expect(push_provider_jpush_http, post, fun(Url, Headers, Body) ->
            self() ! {wire, {Url, Headers, Body}},
            {ok, 200, <<"{\"sendno\":\"1\",\"msg_id\":\"1\"}">>}
        end),
        register(?UID_A, ?D1, <<"android">>, <<"jpush">>, ?T1),
        %% 触发：接收方离线（count_user 桩），正文里放探针
        meck:new(imboy_syn, [passthrough, no_link]),
        meck:expect(imboy_syn, count_user, fun(_) -> 0 end),
        ?assertEqual(
            ok,
            push_notification_logic:maybe_push_for_c2c(?UID_C, ?UID_A, <<"text">>, ?PII_MARKER)
        ),
        {Url, Headers, Body} = recv_wire(),
        ?assertEqual(?TEST_PUSH_URL, Url),
        ?assert(lists:keymember(<<"authorization">>, 1, Headers)),

        %% 请求体逐键封闭：顶层仅 platform/audience/notification（+options）
        Json = jsone:decode(Body),
        TopAllowed = [<<"platform">>, <<"audience">>, <<"notification">>, <<"options">>],
        ?assert(lists:all(fun(K) -> lists:member(K, TopAllowed) end, maps:keys(Json))),
        ?assertEqual([<<"android">>], maps:get(<<"platform">>, Json)),
        ?assertEqual(
            [?T1],
            maps:get(<<"registration_id">>, maps:get(<<"audience">>, Json))
        ),
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

        %% 隐私硬门：正文/密文探针/设备标识/用户 ID 全不得出现在请求体
        lists:foreach(
            fun(Probe) -> ?assertEqual(nomatch, binary:match(Body, [Probe])) end,
            [?PII_MARKER, ?D1, <<"did995">>, <<"995001">>]
        ),

        %% 结论：body 是常量函数——重复触发产出逐字节相同的 wire body
        ?assertEqual(
            ok,
            push_notification_logic:maybe_push_for_c2c(?UID_C, ?UID_A, <<"text">>, ?PII_MARKER)
        ),
        {_U2, _H2, Body2} = recv_wire(),
        ?assertEqual(Body, Body2),
        ok
    after
        application:unset_env(imboy, push)
    end.

recv_wire() ->
    receive
        {wire, V} -> V
    after 2000 -> erlang:error(wire_capture_timeout)
    end.

%%%===================================================================
%%% ⑥ 调用面守卫：caller 自定 title/body 的 API 不得被生产代码调用
%%%===================================================================

%%%===================================================================
%%% ⑥.5 Admin 读面：token 明文零投影（FULL-06/04 follow-up）
%%%
%%% 缺陷（A0 亲核）：GET /api/adm/admin/push_token/list 只要求 settings:view，
%%% 却把原始推送 token 明文返回（adm_admin_handler.erl:712 -> push_token_ds:23
%%% -> push_token_repo:142 的 SELECT 列表里直接有 token 列）。推送 token 是设备
%%% 凭据（拿到即可向该设备推任意通知），与 plan-full §7「无 secret hydration」相抵。
%%% 修法：投影改为 SQL 侧算出的不可逆指纹（md5 前 8 位 + 原字节长度），明文根本
%%% 不进入应用进程。下面用**固定合成 token 探针**做全字面扫描。
%%%===================================================================

-define(ADMIN_TOKEN_PROBE, <<"rid995-ADMIN-PROBE-DO-NOT-LEAK-abcdef0123456789">>).

admin_projection_oracle(_C) ->
    %% 合成夹具：一台带长探针 token 的设备（本用例 BEGIN..ROLLBACK，不留数据）
    Uid = 995777,
    Did = <<"did995-admin-probe">>,
    {ok, _} = push_token_repo:upsert(Uid, Did, <<"android">>, <<"jpush">>, ?ADMIN_TOKEN_PROBE),

    {ok, #{list := Rows, total := Total}} = push_token_repo:list_page(1, 50),
    ?assert(Total >= 1),
    Row = hd([R || R <- Rows, maps:get(<<"device_id">>, R) =:= Did]),
    %% ① 行内**没有** token 键，且没有任何值等于明文
    ?assertNot(maps:is_key(<<"token">>, Row)),
    ?assertNot(
        lists:any(fun(V) -> V =:= ?ADMIN_TOKEN_PROBE end, maps:values(Row))
    ),
    %% ② 指纹口径与前端一致：md5(token) 十六进制前 8 位 + 原字节长度
    ExpectFp = binary:part(
        binary:encode_hex(crypto:hash(md5, ?ADMIN_TOKEN_PROBE), lowercase), 0, 8
    ),
    ?assertEqual(ExpectFp, maps:get(<<"token_fingerprint">>, Row)),
    ?assertEqual(byte_size(?ADMIN_TOKEN_PROBE), maps:get(<<"token_length">>, Row)),
    %% ③ 不可反推：指纹只有 8 个十六进制字符、长度只是整数；两者都不含原 token
    %%    的任何可推送形态（既非原串，也不含原串的前/后缀片段）。
    ?assertEqual(8, byte_size(maps:get(<<"token_fingerprint">>, Row))),
    ?assertEqual(nomatch, binary:match(ExpectFp, [?ADMIN_TOKEN_PROBE])),
    ?assertEqual(
        nomatch,
        binary:match(
            binary:encode_hex(crypto:hash(md5, ?ADMIN_TOKEN_PROBE), lowercase),
            [binary:part(?ADMIN_TOKEN_PROBE, 0, 8)]
        )
    ),
    %% ④ 全字面扫描：**整页响应 JSON**（含键名、嵌套、分页元数据）里探针出现 0 次。
    %%    这是对「任何响应路径都不出现明图片段」的机械断言 —— 不是只看某几个字段。
    ItemsJson = jsone:encode(#{<<"list">> => Rows, <<"total">> => Total}),
    ?assertEqual(nomatch, binary:match(ItemsJson, [?ADMIN_TOKEN_PROBE])),
    QuotedTokenKey = list_to_binary([$", "token", $"]),
    ?assertEqual(nomatch, binary:match(ItemsJson, [QuotedTokenKey])),
    %% ⑤ 长探针的多个片段也都不出现（防"部分截断展示"复活）
    lists:foreach(
        fun(Len) ->
            ?assertEqual(
                nomatch,
                binary:match(ItemsJson, [binary:part(?ADMIN_TOKEN_PROBE, 0, Len)])
            )
        end,
        [6, 8, 12, 16, 24, 32]
    ),
    %% ⑥ 推送执行链未被削弱：执行链读面（list_by_uid/1）仍拿到可推送的明文 token
    %%    （这是设备凭据的**唯一**合法消费点，明文不入 Admin 响应体）。
    {ok, ExecRows} = push_token_repo:list_by_uid(Uid),
    ?assert(
        lists:any(
            fun(R) -> maps:get(<<"token">>, R, undefined) =:= ?ADMIN_TOKEN_PROBE end,
            ExecRows
        )
    ),
    %% ⑦ 详情/单条读面同样零明文：find/list_page 之外没有第二处 Admin 投影
    %%    （list_by_uid/list_by_uids 是执行链专用，不经 handler）。
    Files = filelib:wildcard("src/adm/*.erl"),
    AdminLeak = [
        F
     || F <- Files,
        binary:match(read_file(F), [<<"push_token">>]) =/= nomatch,
        binary:match(read_file(F), [<<"list_by_uid">>]) =/= nomatch
    ],
    ?assertEqual([], AdminLeak),
    ok.

read_file(Path) ->
    case file:read_file(Path) of
        {ok, Bin} -> Bin;
        {error, Reason} -> erlang:error({read_failed, Path, Reason})
    end.

notify_reachability_test() ->
    ?_test(begin
        Files = filelib:wildcard("src/**/*.erl"),
        ?assert(length(Files) > 100),
        Hits = [
            {F, Call}
         || F <- Files,
            Call <- [<<"notify_offline_user(">>, <<"notify_offline_users(">>],
            binary:match(read_file(F), [Call]) =/= nomatch
        ],
        %% 定义方自身允许出现；生产调用方一个都不许（正文/PII 无入参通道）
        ?assertEqual(
            [],
            [F || {F, _} <- Hits, F =/= "src/logic/push_notification_logic.erl"]
        ),
        %% 生产触发点必须走常量入口
        ?assertNotEqual(
            nomatch,
            binary:match(read_file("src/logic/msg_c2c_logic.erl"), [<<"maybe_push_for_c2c(">>])
        ),
        ?assertNotEqual(
            nomatch,
            binary:match(read_file("src/logic/msg_c2g_logic.erl"), [<<"maybe_push_for_c2g(">>])
        )
    end).

%%%===================================================================
%%% ⑦ 迁移 142 down/up 对称
%%%===================================================================

down_up_cycle_test(State) ->
    ?_test(begin
        Conn = connect_marker(State),
        try
            MigConfig = #{conn => Conn, dir => "priv/migrations", strict => true},
            ?assert(has_index(Conn, ?ACTIVE_TOKEN_INDEX)),
            %% 精确退到 141（GZ 期形态：142 的活跃 token 唯一索引尚不存在），
            %% 步数从 head 推导，不随后续新迁移落地而漂移
            %% （A0-REV：原 down 1 步 + 硬编码 {ok,141,_} 只在 head=142 时成立，
            %% head=143 起断言中止并级联污染后续用例的 marker 库状态）
            Head = migration_head(),
            ?assert(Head >= 142),
            ok = erlang_migrate:down(MigConfig, Head - 141),
            ?assertMatch({ok, 141, false}, erlang_migrate:version(MigConfig)),
            ?assertNot(has_index(Conn, ?ACTIVE_TOKEN_INDEX)),
            %% 既有索引未被 down 波及
            ?assert(has_index(Conn, ?USER_DEVICE_INDEX)),
            ok = erlang_migrate:up(MigConfig),
            ?assertEqual({ok, migration_head(), false}, erlang_migrate:version(MigConfig)),
            ?assert(has_index(Conn, ?ACTIVE_TOKEN_INDEX)),
            %% 二次 up 幂等
            ok = erlang_migrate:up(MigConfig),
            ?assertEqual({ok, migration_head(), false}, erlang_migrate:version(MigConfig))
        after
            epgsql:close(Conn)
        end
    end).

connect_marker(State) ->
    #{host := Host, port := Port, username := User, password := Pass} = maps:get(server, State),
    {ok, Conn} = inttest_marker_db:safe_connect(#{
        host => Host,
        port => Port,
        username => User,
        password => Pass,
        database => maps:get(db, State),
        timeout => 10000,
        codecs => [{epgsql_codec_rfc3339_bin, []}]
    }),
    Conn.

%% ===================================================================
%% ⑦ GZ 期形态（head=141，含重复活跃 token）升级到 142 的兼容性
%% ===================================================================
%%
%% 造法：先退到 141（142 的索引随之 drop，回到 141 的真实形态），再插入
%% 「同 token 三条活跃 + 一条非活跃历史行」——这正是修复前真实可能出现的
%% 状态；然后 `up` 升到 head，验证 §1 的数据处置与索引同时生效。
%% 本用例**会落数据**（不走事务回滚），故排在最后，且 ID 段 996xxx 独立。
upgrade_compat_test(State) ->
    ?_test(begin
        Conn = connect_marker(State),
        try
            MigConfig = #{conn => Conn, dir => "priv/migrations", strict => true},
            ?assertEqual({ok, migration_head(), false}, erlang_migrate:version(MigConfig)),

            %% 退到 141：索引不存在 → 可造重复活跃行（GZ 期形态）
            ok = erlang_migrate:goto(MigConfig, 141),
            ?assertNot(has_index(Conn, ?ACTIVE_TOKEN_INDEX)),
            ?assertEqual(0, active_token_count(Conn, ?T_LEGACY)),
            ok = legacy_insert(Conn, 996001, <<"did996-a">>, <<"2026-01-01T00:00:00Z">>, 1),
            ok = legacy_insert(Conn, 996002, <<"did996-b">>, <<"2026-03-01T00:00:00Z">>, 1),
            ok = legacy_insert(Conn, 996003, <<"did996-c">>, <<"2026-02-01T00:00:00Z">>, 1),
            ok = legacy_insert(Conn, 996004, <<"did996-d">>, <<"2025-12-01T00:00:00Z">>, 0),
            ?assertEqual(3, active_token_count(Conn, ?T_LEGACY)),
            ?assertEqual(4, legacy_row_count(Conn)),

            %% 升级到 142：数据处置（只留最新活跃）+ 部分唯一索引同时生效
            ok = erlang_migrate:up(MigConfig),
            ?assertEqual({ok, migration_head(), false}, erlang_migrate:version(MigConfig)),
            ?assert(has_index(Conn, ?ACTIVE_TOKEN_INDEX)),
            ?assertEqual(1, active_token_count(Conn, ?T_LEGACY)),
            %% 保留的是 updated_at 最新那条（996002 / 2026-03-01）
            ?assertEqual(996002, active_token_owner(Conn, ?T_LEGACY)),
            ?assertEqual(<<"did996-b">>, active_token_device(Conn, ?T_LEGACY)),
            %% 不删行：全部 4 条仍在（3 条降级为 status=0）
            ?assertEqual(4, legacy_row_count(Conn)),
            %% 升级后重新受力：需要一个活动事务（SAVEPOINT）与池化 shim
            %% （register_token 走生产 repo 的 2 元池化入口）。
            install_pool_shim(Conn),
            ok = exec(Conn, <<"BEGIN">>),
            try
                %% 再来一条活跃同 token → 唯一索引仲裁
                ?assertEqual(
                    unique_violation,
                    in_savepoint(Conn, fun() ->
                        raw_insert(
                            Conn, 996005, <<"did996-e">>, <<"android">>, <<"jpush">>, ?T_LEGACY, 1
                        )
                    end)
                ),
                %% 客户端下次登录走接管式 upsert：可重新绑定到新主人
                ?assertEqual(
                    ok,
                    push_notification_logic:register_token(
                        996005, <<"did996-e">>, <<"android">>, <<"jpush">>, ?T_LEGACY
                    )
                ),
                ?assertEqual(1, active_token_count(Conn, ?T_LEGACY)),
                ?assertEqual(996005, active_token_owner(Conn, ?T_LEGACY))
            after
                exec_quiet(Conn, <<"ROLLBACK">>),
                meck:unload()
            end
        after
            epgsql:close(Conn)
        end
    end).

legacy_insert(Conn, Uid, DeviceId, UpdatedAt, Status) ->
    R = elib_pg:query(
        Conn,
        <<
            "INSERT INTO public.push_token"
            " (id, user_id, device_id, device_type, platform, token, status,"
            "  created_at, updated_at)"
            " VALUES ($1, $2, $3, 'android', 'jpush', $4, $5, $6, $7)"
        >>,
        [
            elib_tsid:generate(push_token),
            Uid,
            DeviceId,
            ?T_LEGACY,
            Status,
            <<"2025-11-01T00:00:00Z">>,
            UpdatedAt
        ]
    ),
    case db_error(R) of
        {ok, _} -> ok;
        Other -> erlang:error({legacy_insert_failed, Other})
    end.

legacy_row_count(Conn) ->
    scalar(Conn, <<"SELECT count(*) FROM public.push_token WHERE token = $1">>, [?T_LEGACY]).

has_index(C, Name) ->
    scalar(
        C,
        <<
            "SELECT count(*) FROM pg_indexes"
            " WHERE schemaname = 'public' AND indexname = $1"
        >>,
        [Name]
    ) > 0.

migration_head() ->
    Files = filelib:wildcard("priv/migrations/*.up.sql"),
    Versions = [
        V
     || F <- Files,
        [NumStr | _] <- [string:lexemes(filename:basename(F), "_")],
        {V, ""} <- [string:to_integer(NumStr)]
    ],
    ?assert(length(Versions) > 100),
    lists:max(Versions).
