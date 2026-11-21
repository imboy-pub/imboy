-module(olm_identity_log_privacy_tests).
-include_lib("eunit/include/eunit.hrl").

database_failure_logs_do_not_expose_identity_or_reason_test_() ->
    Cases = [
        {upsert_one_time_keys, 4,
            fun() ->
                olm_identity_logic:report_one_time_keys(
                    100,
                    <<"synthetic-device">>,
                    [{<<"synthetic-key-id">>, <<"synthetic-key">>}],
                    100
                )
            end,
            olm_report_otk_error},
        {upsert_fallback_key, 4,
            fun() ->
                olm_identity_logic:report_fallback_key(
                    100,
                    <<"synthetic-device">>,
                    <<"synthetic-key-id">>,
                    <<"synthetic-key">>
                )
            end,
            olm_report_fallback_error},
        {find_identity, 2,
            fun() ->
                olm_identity_logic:report_fallback_key(
                    100,
                    <<"synthetic-device">>,
                    <<"synthetic-key-id">>,
                    <<"synthetic-key">>,
                    <<"synthetic-signature">>
                )
            end,
            olm_fallback_identity_error},
        {find_identity, 2,
            fun() -> olm_identity_logic:claim_keys(100, 100, <<"synthetic-device">>) end,
            olm_claim_identity_error},
        {find_identity, 2,
            fun() ->
                olm_identity_logic:claim_keys(
                    100, 100, <<"synthetic-device">>, <<"synthetic-request">>
                )
            end,
            olm_claim_identity_error},
        {find_identity, 2,
            fun() ->
                olm_identity_logic:report_identity(
                    100,
                    <<"synthetic-device">>,
                    <<"synthetic-ed">>,
                    <<"synthetic-curve">>,
                    <<"synthetic-signature">>,
                    <<"android">>
                )
            end,
            olm_report_identity_lookup_error},
        {find_identity, 2,
            fun() -> olm_identity_logic:get_identity(100, <<"synthetic-device">>) end,
            olm_get_identity_error},
        {list_devices_with_identity, 1, fun() -> olm_identity_logic:list_devices(100) end,
            olm_list_devices_error},
        {count_one_time_keys, 2,
            fun() -> olm_identity_logic:count_one_time_keys(100, <<"synthetic-device">>) end,
            olm_count_otk_error},
        {cleanup_consumed_one_time_keys, 1,
            fun() -> olm_identity_logic:cleanup_consumed_one_time_keys(30) end,
            olm_cleanup_otk_error}
    ],
    [
        ?_test(check_failure_log(Function, Arity, Call, Event))
     || {Function, Arity, Call, Event} <- Cases
    ].

check_failure_log(Function, Arity, Call, Event) ->
    Modules = [olm_identity_ds, elib_log],
    ok = meck:new(Modules, [passthrough, no_link]),
    try
        Canary = #{uid => 100, device => <<"synthetic-device">>, key => <<"synthetic-key-canary">>},
        Stub =
            case Arity of
                1 -> fun(_) -> {error, Canary} end;
                2 -> fun(_, _) -> {error, Canary} end;
                4 -> fun(_, _, _, _) -> {error, Canary} end
            end,
        meck:expect(olm_identity_ds, Function, Arity, Stub),
        meck:expect(elib_log, internal_log, 4, fun(_, _, _, _) -> ok end),
        ?assertEqual({error, <<"internal_error">>}, Call()),
        ?assertEqual(1, meck:num_calls(elib_log, internal_log, [error, Event, '_', '_'])),
        ?assertEqual(1, meck:num_calls(elib_log, internal_log, 4)),
        ?assertEqual(1, meck:num_calls(olm_identity_ds, Function, Arity)),
        %% S-02:LOCAL:02 强化：失败日志的全部实参深度扫描不含 canary——
        %% 私钥/凭据/键材料/正文（含 DB 失败原因）任何形态都不得进日志。
        [
            ?assertEqual(false, term_contains(LoggedArgs, Canary))
         || {_, {elib_log, internal_log, LoggedArgs}, _} <- internal_log_history()
        ]
    after
        meck:unload(Modules)
    end.

internal_log_history() ->
    [
        Entry
     || Entry = {_, {elib_log, internal_log, _Args}, _Result} <- meck:history(elib_log)
    ].

%% @private 深度扫描：Term 内任意层级是否出现与 Needle 相等的值。
term_contains(Term, Needle) when is_map(Term) ->
    term_contains(maps:to_list(Term), Needle);
term_contains(Term, Needle) when is_tuple(Term) ->
    term_contains(tuple_to_list(Term), Needle);
term_contains(Term, Needle) when is_list(Term) ->
    lists:any(fun(Element) -> term_contains(Element, Needle) end, Term);
term_contains(Term, Needle) ->
    Term =:= Needle.

identity_write_failure_logs_do_not_expose_identity_or_reason_test_() ->
    [
        ?_test(check_identity_write_log(Stage, Event))
     || {Stage, Event} <- [
            {bump, olm_identity_bump_error},
            {upsert, olm_report_identity_error},
            {audit, olm_rotation_event_failed}
        ]
    ].

check_identity_write_log(Stage, Event) ->
    Modules = [olm_identity_ds, user_device_ds, trust_audit_ds, elib_metric, elib_tsid, elib_log],
    ok = meck:new(Modules, [passthrough, no_link]),
    try
        {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
        Ed = base64:encode(Pub),
        Curve = base64:encode(<<"synthetic-new-curve">>),
        Sig = base64:encode(crypto:sign(eddsa, none, Curve, [Priv, ed25519])),
        Canary = #{uid => 100, key => <<"synthetic-key-canary">>},
        configure_identity_write_failure(Stage, Ed, Canary),
        Expected =
            case Stage of
                audit -> ok;
                _ -> {error, <<"internal_error">>}
            end,
        ?assertEqual(
            Expected,
            olm_identity_logic:report_identity(
                100, <<"synthetic-device">>, Ed, Curve, Sig, <<"android">>
            )
        ),
        ?assertEqual(1, meck:num_calls(elib_log, internal_log, [error, Event, '_', '_'])),
        ?assertEqual(1, meck:num_calls(elib_log, internal_log, 4)),
        Metrics =
            case Stage of
                audit -> 1;
                _ -> 0
            end,
        ?assertEqual(
            Metrics, meck:num_calls(elib_metric, increment, [olm_rotation_event_failed_total])
        ),
        %% S-02:LOCAL:02 强化：换钥链路失败日志实参深度扫描不含 canary
        %% （旧/新公钥、签名、过渡签名均在本调用作用域内，均不得进日志）
        [
            ?assertEqual(false, term_contains(LoggedArgs, Canary))
         || {_, {elib_log, internal_log, LoggedArgs}, _} <- internal_log_history()
        ]
    after
        meck:unload(Modules)
    end.

configure_identity_write_failure(Stage, Ed, Canary) ->
    meck:expect(olm_identity_ds, find_identity, 2, fun(_, _) ->
        {ok, #{<<"ed25519_key">> => Ed, <<"curve25519_key">> => <<"synthetic-old-curve">>}}
    end),
    meck:expect(user_device_ds, bump_identity_version, 2, fun(_, _) ->
        case Stage of
            bump -> {error, Canary};
            _ -> {ok, 2, 1}
        end
    end),
    meck:expect(olm_identity_ds, upsert_identity, 6, fun(_, _, _, _, _, _) ->
        case Stage of
            upsert -> {error, Canary};
            _ -> {ok, 1}
        end
    end),
    meck:expect(trust_audit_ds, insert_event, 1, fun(_) -> {error, Canary} end),
    meck:expect(elib_tsid, generate, 1, fun(_) -> 990003 end),
    meck:expect(elib_metric, increment, 1, fun(_) -> ok end),
    meck:expect(elib_log, internal_log, 4, fun(_, _, _, _) -> ok end).

%% ===================================================================
%% S-02:LOCAL:02 补充断言（eunit 自动发现，纯 mock）
%%
%%   1. 撤权拒绝路径零日志：不携私钥/凭据/正文的最强形态是不记录
%%   2. repo 失败日志载荷精确钉形：事件 + 标识 + DB 原因，仅此而已
%%   3. repo 写入失败不重复记录（键/签名等材料永不进 repo 层日志）
%%   4. 密钥查询投影仅公钥侧：无 SELECT *、无私钥/凭据类列名
%% ===================================================================

revocation_rejects_do_not_log_test_() ->
    [
        {"claim_revoked_is_silent", fun claim_revoked_is_silent/0},
        {"rotation_revoked_is_silent", fun rotation_revoked_is_silent/0}
    ].

claim_revoked_is_silent() ->
    Modules = [olm_identity_ds, friend_ds, elib_metric, elib_log],
    ok = meck:new(Modules, [passthrough, no_link]),
    try
        meck:expect(friend_ds, is_friend, fun(_, _) -> true end),
        meck:expect(elib_metric, increment, fun(_) -> ok end),
        meck:expect(elib_log, internal_log, 4, fun(_, _, _, _) -> ok end),
        meck:expect(olm_identity_ds, find_identity, fun(200, <<"dev-B">>) ->
            {ok, #{
                <<"ed25519_key">> => <<"synthetic-ed">>,
                <<"curve25519_key">> => <<"synthetic-curve">>,
                <<"signature">> => <<"synthetic-signature">>
            }}
        end),
        %% 撤销门两分支：OTK 阶段拒绝 / OTK 耗尽后 fallback 阶段拒绝
        meck:expect(olm_identity_ds, claim_one_time_key, 3, fun(_, _, _) ->
            {error, device_revoked}
        end),
        ?assertEqual(
            {error, <<"device_revoked">>},
            olm_identity_logic:claim_keys(100, 200, <<"dev-B">>)
        ),
        meck:expect(olm_identity_ds, claim_one_time_key, 3, fun(_, _, _) ->
            {error, exhausted}
        end),
        meck:expect(olm_identity_ds, claim_fallback_key, 2, fun(_, _) ->
            {error, device_revoked}
        end),
        ?assertEqual(
            {error, <<"device_revoked">>},
            olm_identity_logic:claim_keys(100, 200, <<"dev-B">>)
        ),
        ?assertEqual([], internal_log_history())
    after
        meck:unload(Modules)
    end.

rotation_revoked_is_silent() ->
    Modules = [olm_identity_ds, user_device_ds, trust_audit_ds, elib_metric, elib_log],
    ok = meck:new(Modules, [passthrough, no_link]),
    try
        {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
        Ed = base64:encode(Pub),
        NewCurve = base64:encode(<<"synthetic-rotated-curve">>),
        Sig = base64:encode(crypto:sign(eddsa, none, NewCurve, [Priv, ed25519])),
        meck:expect(olm_identity_ds, find_identity, 2, fun(_, _) ->
            {ok, #{
                <<"ed25519_key">> => Ed,
                <<"curve25519_key">> => <<"synthetic-old-curve">>,
                <<"signature">> => <<"synthetic-old-signature">>
            }}
        end),
        meck:expect(user_device_ds, bump_identity_version, 2, fun(_, _) -> {ok, 0} end),
        meck:expect(olm_identity_ds, upsert_identity, 6, fun(_, _, _, _, _, _) -> {ok, 1} end),
        meck:expect(trust_audit_ds, insert_event, 1, fun(_) -> {ok, inserted} end),
        meck:expect(elib_metric, increment, 1, fun(_) -> ok end),
        meck:expect(elib_log, internal_log, 4, fun(_, _, _, _) -> ok end),
        ?assertEqual(
            {error, <<"device_revoked">>},
            olm_identity_logic:report_identity(200, <<"dev-B">>, Ed, NewCurve, Sig, <<"android">>)
        ),
        ?assertEqual([], internal_log_history())
    after
        meck:unload(Modules)
    end.

repo_failure_log_shape_test_() ->
    [
        {"claim_otk_error_tuple", fun repo_claim_otk_error_log/0},
        {"claim_lease_lookup_error_tuple", fun repo_lease_error_log/0},
        {"upsert_errors_are_silent_in_repo", fun repo_upsert_errors_silent/0}
    ].

repo_claim_otk_error_log() ->
    Modules = [elib_pg, elib_log],
    ok = meck:new(Modules, [passthrough, no_link]),
    try
        Reason = {error, {error, error, <<"42P01">>, undefined_function, undefined, undefined}},
        meck:expect(elib_pg, query, 2, fun(_, _) -> {error, Reason} end),
        meck:expect(elib_log, internal_log, 4, fun(_, _, _, _) -> ok end),
        ?assertEqual(
            {error, exhausted}, olm_identity_repo:claim_one_time_key(200, <<"dev-B">>, 100)
        ),
        Logs = internal_log_history(),
        ?assertEqual(1, length(Logs)),
        [{_, {elib_log, internal_log, [error, Msg, olm_identity_repo, _Line]}, _}] = Logs,
        %% 载荷精确钉形：事件 + 两个标识 + DB 原因。键材料/签名不进日志。
        ?assertEqual({olm_claim_otk_error, 200, <<"dev-B">>, Reason}, Msg)
    after
        meck:unload(Modules)
    end.

repo_lease_error_log() ->
    Modules = [elib_pg, elib_log],
    ok = meck:new(Modules, [passthrough, no_link]),
    try
        Reason = {synthetic, db_failure},
        meck:expect(elib_pg, query, 2, fun(_, _) -> {error, Reason} end),
        meck:expect(elib_log, internal_log, 4, fun(_, _, _, _) -> ok end),
        %% /4：租约回查失败（fail-closed 记 lease_lookup_error）→
        %% 消费路径同批失败（记 claim_otk_error）→ exhausted
        ?assertEqual(
            {error, exhausted},
            olm_identity_repo:claim_one_time_key(200, <<"dev-B">>, 100, <<"s02-req">>)
        ),
        Logs = internal_log_history(),
        ?assertEqual(2, length(Logs)),
        Messages = [
            Msg
         || {_, {elib_log, internal_log, [error, Msg, olm_identity_repo, _Line]}, _} <- Logs
        ],
        ?assertEqual(
            [
                {olm_claim_lease_lookup_error, 200, <<"dev-B">>, Reason},
                {olm_claim_otk_error, 200, <<"dev-B">>, Reason}
            ],
            Messages
        )
    after
        meck:unload(Modules)
    end.

repo_upsert_errors_silent() ->
    Modules = [elib_pg, elib_tsid, elib_log],
    ok = meck:new(Modules, [passthrough, no_link]),
    try
        Reason = {synthetic, db_failure},
        meck:expect(elib_tsid, generate, 1, fun(_) ->
            erlang:unique_integer([positive])
        end),
        meck:expect(elib_pg, query, 2, fun(_, _) -> {error, Reason} end),
        meck:expect(elib_pg, execute, 2, fun(_, _) -> {error, Reason} end),
        meck:expect(elib_log, internal_log, 4, fun(_, _, _, _) -> ok end),
        %% 写入面失败由 logic 层记事件原子；repo 层零日志——
        %% Ed25519/Curve25519/签名/OTK 材料永不在 repo 层进入日志。
        ?assertEqual(
            {error, Reason},
            olm_identity_repo:upsert_identity(
                200,
                <<"dev-B">>,
                <<"synthetic-ed">>,
                <<"synthetic-curve">>,
                <<"synthetic-signature">>,
                <<"android">>
            )
        ),
        ?assertEqual(
            {error, Reason},
            olm_identity_repo:upsert_fallback_key(200, <<"dev-B">>, <<"k">>, <<"v">>)
        ),
        ?assertEqual(
            {error, Reason},
            olm_identity_repo:upsert_one_time_keys(200, <<"dev-B">>, [{<<"k">>, <<"v">>}], 100)
        ),
        ?assertEqual([], internal_log_history())
    after
        meck:unload(Modules)
    end.

key_projection_public_only_test_() ->
    [
        {
            atom_to_list(Function),
            fun() -> assert_public_projection(Function, Args) end
        }
     || {Function, Args} <- [
            {find_identity, [200, <<"dev-B">>]},
            {list_identity_by_uids, [[200]]},
            {list_devices_with_identity, [200]},
            {claim_fallback_key, [200, <<"dev-B">>]},
            {upsert_identity, [200, <<"dev-B">>, <<"ed">>, <<"curve">>, <<"sig">>, <<"android">>]},
            {upsert_one_time_keys, [200, <<"dev-B">>, [{<<"k">>, <<"v">>}], 100]},
            {upsert_fallback_key, [200, <<"dev-B">>, <<"k">>, <<"v">>]}
        ]
    ].

assert_public_projection(Function, Args) ->
    Modules = [elib_pg, elib_tsid],
    ok = meck:new(Modules, [passthrough, no_link]),
    try
        meck:expect(elib_tsid, generate, 1, fun(_) ->
            erlang:unique_integer([positive])
        end),
        meck:expect(elib_pg, query, 2, fun(_, _) -> {ok, []} end),
        meck:expect(elib_pg, execute, 2, fun(_, _) -> {ok, 1} end),
        _ = apply(olm_identity_repo, Function, Args),
        Sqls = captured_sqls(),
        ?assert(Sqls =/= []),
        [
            begin
                %% 支持导出/查询投影负例：不得全列 SELECT，不得出现
                %% 私钥/凭据类列名（服务端只存公钥侧的 SQL 面承诺）
                ?assertEqual(nomatch, binary:match(Sql, <<"SELECT *">>)),
                [
                    ?assertEqual(nomatch, binary:match(Sql, Forbidden))
                 || Forbidden <- [
                        <<"private">>,
                        <<"secret">>,
                        <<"pickle">>,
                        <<"password">>,
                        <<"credential">>,
                        <<"token">>,
                        <<"seed">>
                    ]
                ]
            end
         || Sql <- Sqls
        ]
    after
        meck:unload(Modules)
    end.

captured_sqls() ->
    [
        iolist_to_binary(Sql)
     || {_, {elib_pg, Kind, [Sql | _]}, _} <- meck:history(elib_pg),
        Kind =:= query orelse Kind =:= execute
    ].
