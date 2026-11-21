-module(olm_shared_group_authorization_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-export([run/1]).
-export([run_revocation/1]).

%% Only an empty, controller-owned marker DB is allowed; no runtime/config fallback.
-spec run(map()) -> ok.
run(ConnectOpts) when is_map(ConnectOpts) ->
    Required = [database, host, password, port, username],
    ?assertEqual(Required, lists:sort(maps:keys(maps:with(Required, ConnectOpts)))),
    ExpectedDb = unicode:characters_to_binary(maps:get(database, ConnectOpts)),
    ?assertEqual(
        match,
        re:run(
            ExpectedDb,
            <<"\\Aimboy_olm_authorization_[a-z0-9_]+\\z">>,
            [{capture, none}]
        )
    ),
    {ok, _} = application:ensure_all_started(epgsql),
    {ok, Conn} = epgsql:connect(ConnectOpts),
    try
        run_conn(Conn, ExpectedDb)
    after
        epgsql:close(Conn)
    end.

run_conn(Conn, ExpectedDb) ->
    ?assertEqual(
        {ok, [#{<<"db">> => ExpectedDb, <<"empty">> => true}]},
        elib_pg:query(
            Conn,
            <<
                "SELECT current_database() AS db, "
                "to_regclass('public.group_member') IS NULL AS empty"
            >>,
            []
        )
    ),
    {ok, []} = elib_pg:query(Conn, <<"BEGIN">>, []),
    try
        create_fixture(Conn),
        assert_authorization(Conn)
    after
        {ok, []} = elib_pg:query(Conn, <<"ROLLBACK">>, []),
        ?assertEqual(
            {ok, [#{<<"absent">> => true}]},
            elib_pg:query(
                Conn,
                <<"SELECT to_regclass('public.group_member') IS NULL AS absent">>,
                []
            )
        )
    end.

create_fixture(Conn) ->
    %% Types/status domain: foundation migration; only status=1 is active.
    {ok, []} = elib_pg:query(
        Conn,
        <<
            "CREATE TABLE public.group_member (group_id bigint NOT NULL, "
            "user_id bigint NOT NULL, status smallint NOT NULL "
            "CHECK(status IN (-1,0,1,2)))"
        >>,
        []
    ),
    {ok, []} = elib_pg:query(
        Conn,
        <<
            "INSERT INTO public.group_member(group_id,user_id,status) VALUES "
            "(1,100,1),(1,200,1),(2,201,1),(1,202,0),(1,203,2),"
            "(1,204,-1),(1,205,0),(1,206,2),(1,207,-1)"
        >>,
        []
    ),
    ok.

assert_authorization(Conn) ->
    Modules = [friend_ds, olm_identity_ds, elib_pg, elib_metric],
    ok = meck:new(Modules, [passthrough, no_link]),
    try
        meck:expect(friend_ds, is_friend, fun(_, _) -> false end),
        meck:expect(elib_metric, increment, fun(_) -> ok end),
        %% Authorization success stops before real identity/prekey access.
        meck:expect(olm_identity_ds, find_identity, fun(_, _) -> {ok, not_found} end),
        meck:expect(elib_pg, query, fun(Sql, Params) -> elib_pg:query(Conn, Sql, Params) end),
        Cases = [
            {100, 200, true},
            {100, 201, false},
            {100, 202, false},
            {100, 203, false},
            {100, 204, false},
            {205, 200, false},
            {206, 200, false},
            {207, 200, false}
        ],
        [
            assert_claim(A, B, Allowed, Arity)
         || {A, B, Allowed} <- Cases, Arity <- [3, 4]
        ],
        [assert_database_failure(Conn, Arity) || Arity <- [3, 4]],
        assert_claim(100, 200, true, 3),
        ?assertEqual(0, meck:num_calls(olm_identity_ds, claim_one_time_key, '_')),
        ?assertEqual(0, meck:num_calls(olm_identity_ds, claim_fallback_key, '_')),
        ok
    after
        meck:unload(Modules)
    end.

assert_claim(A, B, Allowed, Arity) ->
    IdentityCalls = meck:num_calls(olm_identity_ds, find_identity, '_'),
    Result = apply(
        olm_identity_logic,
        claim_keys,
        [A, B, <<"synthetic-device">>] ++
            case Arity of
                3 -> [];
                4 -> [<<"synthetic-request">>]
            end
    ),
    Expected =
        case Allowed of
            true -> {error, <<"device_not_registered">>};
            false -> {error, <<"claim_not_authorized">>}
        end,
    ?assertEqual(Expected, Result),
    ?assertEqual(
        IdentityCalls +
            case Allowed of
                true -> 1;
                false -> 0
            end,
        meck:num_calls(olm_identity_ds, find_identity, '_')
    ).

assert_database_failure(Conn, Arity) ->
    {ok, []} = elib_pg:query(Conn, <<"SAVEPOINT missing_relation">>, []),
    try
        {ok, []} = elib_pg:query(Conn, <<"DROP TABLE public.group_member">>, []),
        assert_claim(100, 200, false, Arity)
    after
        {ok, []} = elib_pg:query(Conn, <<"ROLLBACK TO SAVEPOINT missing_relation">>, []),
        {ok, []} = elib_pg:query(Conn, <<"RELEASE SAVEPOINT missing_relation">>, [])
    end.

claim_shared_group_authorization_test_() ->
    Cases = [
        {active_shared_group, {ok, [#{<<"?column?">> => 1}]}, true},
        {no_active_shared_group, {ok, []}, false},
        {database_error, {error, unavailable}, false},
        {database_exception, throw, false},
        {unexpected_query_shape, {ok, [], [], [{1}]}, false}
    ],
    [
        {atom_to_list(Name) ++ " arity " ++ integer_to_list(Arity), fun() ->
            assert_shared_group_claim(Arity, Reply, Allowed)
        end}
     || {Name, Reply, Allowed} <- Cases, Arity <- [3, 4]
    ].

assert_shared_group_claim(Arity, Reply, Allowed) ->
    Modules = [olm_identity_ds, friend_ds, elib_pg, elib_metric],
    ok = meck:new(Modules, [passthrough, no_link]),
    try
        meck:expect(friend_ds, is_friend, fun(100, 200) -> false end),
        meck:expect(elib_metric, increment, fun(_) -> ok end),
        meck:expect(elib_pg, query, fun(_, _) ->
            case Reply of
                throw -> error(synthetic_database_failure);
                _ -> Reply
            end
        end),
        %% A successful authorization stops before any real key can be consumed.
        meck:expect(olm_identity_ds, find_identity, fun(200, <<"dev-B">>) ->
            {ok, not_found}
        end),
        Result = apply(
            olm_identity_logic,
            claim_keys,
            [100, 200, <<"dev-B">>] ++
                case Arity of
                    3 -> [];
                    4 -> [<<"request-1">>]
                end
        ),
        Expected =
            case Allowed of
                true -> {error, <<"device_not_registered">>};
                false -> {error, <<"claim_not_authorized">>}
            end,
        ?assertEqual(Expected, Result),
        %% app-booted VM 后台 worker 污染全局计数（2026-11-19 v5 实证 1→2）——钉下界
        ?assert(meck:num_calls(elib_pg, query, '_') >= 1),
        assert_shared_group_query(),
        ?assertEqual(
            case Allowed of
                true -> 1;
                false -> 0
            end,
            meck:num_calls(olm_identity_ds, find_identity, '_')
        ),
        ?assertEqual(0, meck:num_calls(olm_identity_ds, claim_one_time_key, '_'))
    after
        meck:unload(Modules)
    end.

assert_shared_group_query() ->
    Sql = meck:capture(first, elib_pg, query, ['_', '_'], 1),
    ?assertEqual([100, 200], meck:capture(first, elib_pg, query, ['_', '_'], 2)),
    ?assertEqual(
        <<
            "SELECT 1 FROM group_member a "
            "JOIN group_member b ON a.group_id = b.group_id "
            "WHERE a.user_id = $1 AND b.user_id = $2 AND a.status = 1 AND b.status = 1 "
            "LIMIT 1"
        >>,
        iolist_to_binary(Sql)
    ).

%% ===================================================================
%% S-02:LOCAL:01 撤权主体不获新密钥或越权历史（eunit 自动发现，纯 mock）
%%
%% 覆盖 C01 撤销联合门的 wire 语义：
%%   - 已撤销设备的 OTK 与 fallback 一概不可领（含 self-claim）
%%   - 已撤销设备的身份换钥（同根/换根）被 bump 0 行拒绝，键面不动
%%   - 退群后 claim 被授权门拒绝；重入后恢复
%%   - 枚举面 SQL 必须钉死 user_device status=1 JOIN（撤销设备不可枚举）
%% ===================================================================

revoked_device_claim_test_() ->
    Stages = [
        {otk_revoked, {error, <<"device_revoked">>}},
        {otk_revoked_self_claim, {error, <<"device_revoked">>}},
        {fallback_revoked_after_exhausted, {error, <<"device_revoked">>}},
        {no_prekey_after_exhausted, {error, <<"no_prekey_available">>}}
    ],
    [
        {
            atom_to_list(Stage) ++ " arity " ++ integer_to_list(Arity),
            fun() -> assert_revoked_device_claim(Stage, Expected, Arity) end
        }
     || {Stage, Expected} <- Stages, Arity <- [3, 4]
    ].

assert_revoked_device_claim(Stage, Expected, Arity) ->
    Modules = [olm_identity_ds, friend_ds, elib_metric],
    ok = meck:new(Modules, [passthrough, no_link]),
    try
        meck:expect(friend_ds, is_friend, fun(_, _) -> true end),
        meck:expect(elib_metric, increment, fun(_) -> ok end),
        meck:expect(olm_identity_ds, find_identity, fun(200, <<"dev-B">>) ->
            {ok, #{
                <<"ed25519_key">> => <<"synthetic-ed">>,
                <<"curve25519_key">> => <<"synthetic-curve">>,
                <<"signature">> => <<"synthetic-signature">>
            }}
        end),
        OtkReply =
            case Stage of
                otk_revoked -> {error, device_revoked};
                otk_revoked_self_claim -> {error, device_revoked};
                _ -> {error, exhausted}
            end,
        FbReply =
            case Stage of
                fallback_revoked_after_exhausted -> {error, device_revoked};
                no_prekey_after_exhausted -> {error, exhausted};
                %% OTK 阶段即拒的 stage 不会触达 fallback，占位即可
                _ -> {error, exhausted}
            end,
        meck:expect(olm_identity_ds, claim_one_time_key, 3, fun(_, _, _) -> OtkReply end),
        meck:expect(olm_identity_ds, claim_one_time_key, 4, fun(_, _, _, _) -> OtkReply end),
        meck:expect(olm_identity_ds, claim_fallback_key, 2, fun(_, _) -> FbReply end),
        {CurrentUid, TargetUid} =
            case Stage of
                otk_revoked_self_claim -> {200, 200};
                _ -> {100, 200}
            end,
        Result = apply(
            olm_identity_logic,
            claim_keys,
            [CurrentUid, TargetUid, <<"dev-B">>] ++
                case Arity of
                    3 -> [];
                    4 -> [<<"synthetic-request">>]
                end
        ),
        ?assertEqual(Expected, Result),
        %% 撤销设备的 fallback 同样不可达：OTK 阶段被拒时不落 fallback
        case Stage of
            otk_revoked ->
                ?assertEqual(0, meck:num_calls(olm_identity_ds, claim_fallback_key, 2));
            otk_revoked_self_claim ->
                ?assertEqual(0, meck:num_calls(olm_identity_ds, claim_fallback_key, 2));
            _ ->
                ?assertEqual(1, meck:num_calls(olm_identity_ds, claim_fallback_key, 2))
        end,
        %% 授权通过后必须先查身份键（门序）：每 claim 恰一次
        ?assertEqual(1, meck:num_calls(olm_identity_ds, find_identity, 2))
    after
        meck:unload(Modules)
    end.

revoked_device_rotation_test_() ->
    [
        {atom_to_list(Stage), fun() -> assert_revoked_rotation(Stage) end}
     || Stage <- [
            same_root_revoked,
            root_change_revoked_with_old_key_proof,
            root_change_without_old_key_proof
        ]
    ].

assert_revoked_rotation(Stage) ->
    Modules = [olm_identity_ds, user_device_ds, trust_audit_ds, elib_metric, elib_log],
    ok = meck:new(Modules, [passthrough, no_link]),
    try
        {OldPub, OldPriv} = crypto:generate_key(eddsa, ed25519),
        {NewPub, NewPriv} =
            case Stage of
                same_root_revoked -> {OldPub, OldPriv};
                _ -> crypto:generate_key(eddsa, ed25519)
            end,
        OldEd = base64:encode(OldPub),
        NewEd = base64:encode(NewPub),
        NewCurve = base64:encode(<<"synthetic-rotated-curve">>),
        Sig = base64:encode(crypto:sign(eddsa, none, NewCurve, [NewPriv, ed25519])),
        meck:expect(olm_identity_ds, find_identity, 2, fun(_, _) ->
            {ok, #{
                <<"ed25519_key">> => OldEd,
                <<"curve25519_key">> => <<"synthetic-old-curve">>,
                <<"signature">> => <<"synthetic-old-signature">>
            }}
        end),
        meck:expect(user_device_ds, bump_identity_version, 2, fun(_, _) -> {ok, 0} end),
        meck:expect(olm_identity_ds, upsert_identity, 6, fun(_, _, _, _, _, _) -> {ok, 1} end),
        meck:expect(trust_audit_ds, insert_event, 1, fun(_) -> {ok, inserted} end),
        meck:expect(elib_metric, increment, 1, fun(_) -> ok end),
        meck:expect(elib_log, internal_log, 4, fun(_, _, _, _) -> ok end),
        TransitionSig =
            case Stage of
                root_change_revoked_with_old_key_proof ->
                    Canonical =
                        <<"curve25519_key=", NewCurve/binary, "\ndevice_id=dev-B\ned25519_key=",
                            NewEd/binary, "\nuser_id=200">>,
                    base64:encode(crypto:sign(eddsa, none, Canonical, [OldPriv, ed25519]));
                _ ->
                    <<>>
            end,
        Expected =
            case Stage of
                root_change_without_old_key_proof ->
                    {error, <<"key_rotation_requires_old_key_proof">>};
                _ ->
                    {error, <<"device_revoked">>}
            end,
        ?assertEqual(
            Expected,
            olm_identity_logic:report_identity(
                200, <<"dev-B">>, NewEd, NewCurve, Sig, TransitionSig, <<"android">>
            )
        ),
        case Stage of
            root_change_without_old_key_proof ->
                %% 旧钥 PoP 前置门先拒：不触达撤销门
                ?assertEqual(0, meck:num_calls(user_device_ds, bump_identity_version, 2));
            _ ->
                ?assertEqual(1, meck:num_calls(user_device_ds, bump_identity_version, 2))
        end,
        %% 不获新密钥：撤销设备 0 次身份写入、0 次版本事件、0 条日志
        ?assertEqual(0, meck:num_calls(olm_identity_ds, upsert_identity, 6)),
        ?assertEqual(0, meck:num_calls(trust_audit_ds, insert_event, 1)),
        ?assertEqual(0, meck:num_calls(elib_log, internal_log, 4))
    after
        meck:unload(Modules)
    end.

group_exit_reentry_claim_test_() ->
    [
        {"exit_denies_claim", fun exit_denies_claim/0},
        {"reentry_allows_claim", fun reentry_allows_claim/0}
    ].

exit_denies_claim() ->
    Modules = [olm_identity_ds, friend_ds, elib_pg, elib_metric],
    ok = meck:new(Modules, [passthrough, no_link]),
    try
        meck:expect(friend_ds, is_friend, fun(_, _) -> false end),
        meck:expect(elib_metric, increment, fun(_) -> ok end),
        %% 退群后共享群查询无 status=1 行
        meck:expect(elib_pg, query, fun(_, _) -> {ok, []} end),
        [
            begin
                Result = apply(
                    olm_identity_logic,
                    claim_keys,
                    [100, 200, <<"dev-B">>] ++
                        case Arity of
                            3 -> [];
                            4 -> [<<"synthetic-request">>]
                        end
                ),
                ?assertEqual({error, <<"claim_not_authorized">>}, Result)
            end
         || Arity <- [3, 4]
        ],
        %% 授权被拒不触达身份/键查询
        ?assertEqual(0, meck:num_calls(olm_identity_ds, find_identity, '_'))
    after
        meck:unload(Modules)
    end.

reentry_allows_claim() ->
    Modules = [olm_identity_ds, friend_ds, elib_pg, elib_metric],
    ok = meck:new(Modules, [passthrough, no_link]),
    try
        meck:expect(friend_ds, is_friend, fun(_, _) -> false end),
        meck:expect(elib_metric, increment, fun(_) -> ok end),
        %% 重入（status=1）后共享群查询命中
        meck:expect(elib_pg, query, fun(_, _) -> {ok, [#{<<"?column?">> => 1}]} end),
        meck:expect(olm_identity_ds, find_identity, fun(200, <<"dev-B">>) -> {ok, not_found} end),
        [
            begin
                Result = apply(
                    olm_identity_logic,
                    claim_keys,
                    [100, 200, <<"dev-B">>] ++
                        case Arity of
                            3 -> [];
                            4 -> [<<"synthetic-request">>]
                        end
                ),
                %% 已过授权门（到达身份查询），负例终点为 device_not_registered
                ?assertEqual({error, <<"device_not_registered">>}, Result)
            end
         || Arity <- [3, 4]
        ],
        assert_shared_group_query(),
        ?assertEqual(2, meck:num_calls(olm_identity_ds, find_identity, '_'))
    after
        meck:unload(Modules)
    end.

revoked_device_enumeration_sql_test_() ->
    [{"enumeration_pins_active_device_join", fun enumeration_pins_active_device_join/0}].

enumeration_pins_active_device_join() ->
    Modules = [elib_pg],
    ok = meck:new(Modules, [passthrough, no_link]),
    try
        Rows = [#{<<"device_id">> => <<"dev-B">>, <<"ed25519_key">> => <<"synthetic-ed">>}],
        meck:expect(elib_pg, query, fun(_, _) -> {ok, Rows} end),
        %% 服务端不做二次过滤：只透传 DB 行——撤销设备的排除必须由 SQL 本身完成
        {ok, #{<<"user_id">> := 200, <<"devices">> := Rows}} =
            olm_identity_logic:list_devices(200),
        ?assertEqual({ok, Rows}, olm_identity_repo:list_identity_by_uids([200])),
        [
            begin
                Sql = iolist_to_binary(meck:capture(N, elib_pg, query, ['_', '_'], 1)),
                ?assert(binary:match(Sql, <<"user_device">>) =/= nomatch),
                ?assert(binary:match(Sql, <<"ud.status = 1">>) =/= nomatch),
                ?assertEqual(nomatch, binary:match(Sql, <<"SELECT *">>))
            end
         || N <- lists:seq(1, 2)
        ]
    after
        meck:unload(Modules)
    end.

%% ===================================================================
%% S-02:LOCAL:01/02 真库复核入口（脚本式，控制器自有隔离库）
%%
%% 与 run/1（空库合成 fixture）互补：在完整迁移链的真实 schema 上走真实
%% ds/repo 链路，验证撤销联合门、退群重入、换钥复活门与公钥侧投影。
%% 调用方（控制器配方）负责：目标库迁移到位（erlang_migrate:up 全链）、
%% 传入 epgsql ConnectOpts。库名必须为 imboy_plan_s02_olm（qa-db 租约指定
%% 的控制器自有库），防误指生产/共享库。事务自持（BEGIN/ROLLBACK）。
%% ===================================================================

-spec run_revocation(map()) -> ok.
run_revocation(ConnectOpts) when is_map(ConnectOpts) ->
    ExpectedDb = unicode:characters_to_binary(maps:get(database, ConnectOpts)),
    ?assertEqual(<<"imboy_plan_s02_olm">>, ExpectedDb),
    {ok, _} = application:ensure_all_started(epgsql),
    {ok, Conn} = epgsql:connect(ConnectOpts),
    try
        {ok, [#{<<"db">> := ExpectedDb, <<"migrated">> := true}]} =
            elib_pg:query(
                Conn,
                <<
                    "SELECT current_database() AS db, ",
                    "to_regclass('public.olm_identity') IS NOT NULL AS migrated"
                >>,
                []
            ),
        {ok, []} = elib_pg:query(Conn, <<"BEGIN">>, []),
        try
            assert_no_private_key_columns(Conn),
            assert_revocation_invariants(Conn)
        after
            {ok, []} = elib_pg:query(Conn, <<"ROLLBACK">>, []),
            %% 回滚后零残留：本复核不留下任何业务行（支持导出面同样无新增数据）
            {ok, [#{<<"n">> := 0}]} = elib_pg:query(
                Conn,
                <<
                    "SELECT (SELECT COUNT(*) FROM public.olm_identity) ",
                    "+ (SELECT COUNT(*) FROM public.olm_one_time_key) ",
                    "+ (SELECT COUNT(*) FROM public.olm_fallback_key) ",
                    "+ (SELECT COUNT(*) FROM public.trust_audit ",
                    "    WHERE method = 'identity_rotated') ",
                    "+ (SELECT COUNT(*) FROM public.user_device ",
                    "    WHERE user_id IN (100, 200)) ",
                    "+ (SELECT COUNT(*) FROM public.group_member ",
                    "    WHERE group_id = 9001) AS n"
                >>,
                []
            )
        end
    after
        epgsql:close(Conn)
    end.

%% S-02:LOCAL:02 支持导出负例（schema 面）：olm 三表不得存在私钥/凭据类
%% 列——服务端只存公钥侧是零信任承诺，任何此类列都会让导出面携带秘密。
assert_no_private_key_columns(Conn) ->
    {ok, Rows} = elib_pg:query(
        Conn,
        <<
            "SELECT table_name, column_name FROM information_schema.columns ",
            "WHERE table_schema = 'public' AND table_name IN ",
            "('olm_identity','olm_one_time_key','olm_fallback_key') ",
            "AND column_name ~ ",
            "'(private|secret|pickle|password|credential|token|seed|mnemonic|recovery)'"
        >>,
        []
    ),
    ?assertEqual([], Rows).

assert_revocation_invariants(Conn) ->
    %% imboy_cache:memo 的 60s 正结果缓存是既有吊销传播语义（撤销方负责
    %% invalidate 扇出，见 user_device_logic:delete）。测试 VM 无 depcache，
    %% 绕过 memo 让每次 is_active 直读真实白名单行——被测对象是 DB 联合门
    %% 本身，绕过缓存只会更严（不存在陈旧放行窗口），不放宽任何门。
    Modules = [friend_ds, elib_pg, elib_metric, elib_tsid, imboy_cache],
    ok = meck:new(Modules, [passthrough, no_link]),
    try
        meck:expect(friend_ds, is_friend, fun(_, _) -> false end),
        meck:expect(elib_metric, increment, fun(_) -> ok end),
        meck:expect(elib_tsid, generate, 1, fun(_) ->
            erlang:unique_integer([positive])
        end),
        meck:expect(imboy_cache, memo, 3, fun(F, _, _) -> F() end),
        meck:expect(imboy_cache, memo, 4, fun(F, _, _, _) -> F() end),
        meck:expect(imboy_cache, uncached, 1, fun(V) -> V end),
        meck:expect(imboy_cache, invalidate, 1, fun(_) -> ok end),
        meck:expect(elib_pg, query, 2, fun(Sql, Params) ->
            elib_pg:query(Conn, Sql, Params)
        end),
        meck:expect(elib_pg, execute, 2, fun(Sql, Params) ->
            elib_pg:execute(Conn, Sql, Params)
        end),
        meck:expect(elib_pg, execute_count, 2, fun(Sql, Params) ->
            elib_pg:execute_count(Conn, Sql, Params)
        end),
        %% trust_audit_repo:insert_event 在 with_tx/1 内自开事务；本复核外层
        %% 已持事务，重定向为「以当前连接直接执行 fun」（advisory xact 锁
        %% 随外层事务持有，语义等价于其锁内串行化意图）。
        meck:expect(elib_pg, with_tx, 1, fun(F) -> F(Conn) end),
        DevB = <<"dev-B">>,
        DevX = <<"dev-X">>,
        #{dev_b := BKeys, dev_x := XKeys} = create_revocation_fixture(DevB, DevX),
        %% ① 群成员对活跃设备 claim：OTK 正常领取（授权门 + 撤销门均放行），
        %%    /4 顺带验证幂等租约 SQL 对真实 schema 的兼容。
        {ok, #{<<"type">> := <<"one_time">>}} =
            olm_identity_logic:claim_keys(100, 200, DevB),
        {ok, #{<<"type">> := <<"one_time">>}} =
            olm_identity_logic:claim_keys(100, 200, DevB, <<"s02-lease-1">>),
        %% ② 已撤销设备（白名单 status=0）：残留 OTK 也领不走（撤销联合门）
        ?assertEqual(
            {error, <<"device_revoked">>}, olm_identity_logic:claim_keys(100, 200, DevX)
        ),
        ?assertEqual(
            {error, <<"device_revoked">>},
            olm_identity_logic:claim_keys(200, 200, DevX, <<"s02-lease-x">>)
        ),
        %% ③ 撤销设备不获新密钥：身份换钥（同根）被 bump 0 行拒绝，键面不动
        RotCurve = base64:encode(<<"s02-rotated-curve">>),
        RotSigX = signed(maps:get(priv, XKeys), RotCurve),
        ?assertEqual(
            {error, <<"device_revoked">>},
            olm_identity_logic:report_identity(
                200, DevX, maps:get(ed, XKeys), RotCurve, RotSigX, <<"android">>
            )
        ),
        {ok, #{<<"curve25519_key">> := XCurve0}} = olm_identity_logic:get_identity(200, DevX),
        ?assertEqual(base64:encode(<<"s02-dev-X-curve">>), XCurve0),
        %% ④ 活跃设备换钥（对照组）：放行 + 版本事件真实落库（真实 trust_audit 链）
        RotSigB = signed(maps:get(priv, BKeys), RotCurve),
        ?assertEqual(
            ok,
            olm_identity_logic:report_identity(
                200, DevB, maps:get(ed, BKeys), RotCurve, RotSigB, <<"android">>
            )
        ),
        {ok, [#{<<"n">> := 1}]} = elib_pg:query(
            Conn,
            <<
                "SELECT COUNT(*) AS n FROM public.trust_audit ",
                "WHERE method = 'identity_rotated' AND target_device_id = 'dev-B'"
            >>,
            []
        ),
        %% ⑤ 越权历史：枚举面（设备列表/身份批量查询）不含已撤销设备
        {ok, #{<<"devices">> := Devices}} = olm_identity_logic:list_devices(200),
        ?assertEqual([DevB], [maps:get(<<"device_id">>, D) || D <- Devices]),
        {ok, IdRows} = olm_identity_repo:list_identity_by_uids([200]),
        ?assertEqual(
            [DevB], lists:usort([maps:get(<<"device_id">>, D) || D <- IdRows])
        ),
        %% ⑥ OTK 耗尽回退 fallback（活跃设备）——撤销设备无此通道（②已证）
        {ok, #{<<"type">> := <<"fallback">>}} =
            olm_identity_logic:claim_keys(100, 200, DevB),
        %% ⑦ 退群：status=0 后 claim 被授权门拒绝；重入（status=1）后恢复
        {ok, 1} = elib_pg:execute(
            Conn,
            <<
                "UPDATE public.group_member SET status = 0 ",
                "WHERE group_id = 9001 AND user_id = 100"
            >>,
            []
        ),
        ?assertEqual(
            {error, <<"claim_not_authorized">>},
            olm_identity_logic:claim_keys(100, 200, DevB)
        ),
        {ok, 1} = elib_pg:execute(
            Conn,
            <<
                "UPDATE public.group_member SET status = 1 ",
                "WHERE group_id = 9001 AND user_id = 100"
            >>,
            []
        ),
        {ok, #{<<"type">> := <<"fallback">>}} =
            olm_identity_logic:claim_keys(100, 200, DevB),
        %% ⑧ 撤销级联清理 + 孤儿对账：删撤销设备 olm 材料；再撤销 dev-B 后
        %%    sweep 清扫残留，枚举面归零（撤销对 E2EE 生效的最终一致性）
        {ok, Deleted} = olm_identity_repo:delete_by_device(200, DevX),
        ?assert(Deleted >= 1),
        {ok, 1} = elib_pg:execute(
            Conn,
            <<
                "UPDATE public.user_device SET status = 0 ",
                "WHERE user_id = 200 AND device_id = 'dev-B'"
            >>,
            []
        ),
        {ok, Swept} = olm_identity_repo:sweep_orphan_olm_material(),
        ?assert(Swept >= 1),
        {ok, #{<<"devices">> := []}} = olm_identity_logic:list_devices(200),
        ok
    after
        meck:unload(Modules)
    end.

create_revocation_fixture(DevB, DevX) ->
    [IdA, IdB, IdX, Gm1, Gm2] = [erlang:unique_integer([positive]) || _ <- lists:seq(1, 5)],
    %% user_device 白名单：dev-A(100) 活跃、dev-B(200) 活跃、
    %% dev-X(200) status=0（撤销后状态——olm 残留行场景，即 C01 对账目标）
    {ok, 3} = elib_pg:execute(
        <<
            "INSERT INTO public.user_device(id, user_id, device_id, status) ",
            "VALUES($1, 100, 'dev-A', 1), ($2, 200, $3, 1), ($4, 200, $5, 0)"
        >>,
        [IdA, IdB, DevB, IdX, DevX]
    ),
    {ok, 2} = elib_pg:execute(
        <<
            "INSERT INTO public.group_member(id, group_id, user_id, status) ",
            "VALUES($1, 9001, 100, 1), ($2, 9001, 200, 1)"
        >>,
        [Gm1, Gm2]
    ),
    %% 身份注册走公开 logic API（首次注册 PoP 路径，真实 ed25519 验签）。
    %% dev-X 的身份行构成「撤销后残留」：首次注册无设备门（设计如此，
    %% 撤销防线在 claim/枚举/换钥三面，见 ②③⑤）。
    BKeys = register_identity(200, DevB, <<"s02-dev-B-curve">>),
    XKeys = register_identity(200, DevX, <<"s02-dev-X-curve">>),
    {ok, 2} = olm_identity_logic:report_one_time_keys(
        200,
        DevB,
        [
            {<<"s02-otk-1">>, base64:encode(<<"s02-otk-material-1">>)},
            {<<"s02-otk-2">>, base64:encode(<<"s02-otk-material-2">>)}
        ],
        100
    ),
    {ok, 1} = olm_identity_logic:report_one_time_keys(
        200, DevX, [{<<"s02-otk-x">>, base64:encode(<<"s02-otk-material-x">>)}], 100
    ),
    ok = report_fallback_signed(
        200,
        DevB,
        <<"s02-fb-B">>,
        <<"s02-fb-B-material">>,
        maps:get(priv, BKeys)
    ),
    ok = report_fallback_signed(
        200,
        DevX,
        <<"s02-fb-X">>,
        <<"s02-fb-X-material">>,
        maps:get(priv, XKeys)
    ),
    #{dev_b => BKeys, dev_x => XKeys}.

register_identity(Uid, Did, CurveSeed) ->
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    Ed = base64:encode(Pub),
    Curve = base64:encode(CurveSeed),
    Sig = signed(Priv, Curve),
    ok = olm_identity_logic:report_identity(Uid, Did, Ed, Curve, Sig, <<"android">>),
    #{ed => Ed, priv => Priv, curve => Curve}.

report_fallback_signed(Uid, Did, KeyId, KeySeed, Priv) ->
    KeyB64 = base64:encode(KeySeed),
    Canonical = <<
        "device_id=",
        Did/binary,
        "\nkey_base64=",
        KeyB64/binary,
        "\nkey_id=",
        KeyId/binary,
        "\nuser_id=",
        (integer_to_binary(Uid))/binary
    >>,
    Sig = base64:encode(crypto:sign(eddsa, none, Canonical, [Priv, ed25519])),
    olm_identity_logic:report_fallback_key(Uid, Did, KeyId, KeyB64, Sig).

signed(Priv, Msg) ->
    base64:encode(crypto:sign(eddsa, none, Msg, [Priv, ed25519])).
