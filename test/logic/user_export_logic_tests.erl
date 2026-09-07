-module(user_export_logic_tests).

%%%===================================================================
%%% @doc user_export_logic 个人数据导出测试（C0-GOV-01 + P-01）
%%%
%%% 覆盖：导出 schema 与 scope 声明、敏感字段剥离（含嵌套、jsonb 自由键、
%%%       user_info 新增列的 schema 漂移场景）、bounded 截断标记透传、
%%%       冷却限频（窗口内拒绝/窗口外放行/配置关闭/检查故障 fail-open）、
%%%       Legal Hold 显式不支持、范围限制（非法 uid 拒绝）、
%%%       导出行为写审计、审计失败不阻断导出。
%%% @end
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

-compile([nowarn_deprecated_catch]).

%%%===================================================================
%%% 敏感字段剥离（纯函数，不触库）
%%%===================================================================

sanitize_strips_sensitive_keys_test() ->
    In = #{
        <<"id">> => 1,
        <<"nickname">> => <<"n">>,
        <<"password">> => <<"hash">>,
        <<"password_salt">> => <<"s">>,
        <<"access_token">> => <<"t">>,
        <<"refresh_token">> => <<"t2">>,
        <<"api_key">> => <<"k">>,
        <<"private_key">> => <<"pk">>,
        <<"client_secret">> => <<"cs">>,
        <<"credential_id">> => <<"c">>
    },
    Out = user_export_logic:sanitize(In),
    ?assertEqual([<<"id">>, <<"nickname">>], lists:sort(maps:keys(Out))),
    ?assertEqual(1, maps:get(<<"id">>, Out)).

%% 大小写不敏感：DB 列名/驼峰键都要挡住
sanitize_is_case_insensitive_test() ->
    Out = user_export_logic:sanitize(#{
        <<"Password">> => <<"x">>,
        <<"accessToken">> => <<"x">>,
        <<"SECRET_KEY">> => <<"x">>,
        <<"ok">> => 1
    }),
    ?assertEqual([<<"ok">>], maps:keys(Out)).

%% 嵌套 map 与 list 内的敏感字段同样要剥离
sanitize_recurses_into_nested_test() ->
    Out = user_export_logic:sanitize(#{
        <<"settings">> => #{<<"theme">> => <<"dark">>, <<"push_token">> => <<"tk">>},
        <<"friends">> => [
            #{<<"to_user_id">> => 2, <<"secret">> => <<"s">>},
            #{<<"to_user_id">> => 3}
        ]
    }),
    ?assertEqual(#{<<"theme">> => <<"dark">>}, maps:get(<<"settings">>, Out)),
    ?assertEqual(
        [#{<<"to_user_id">> => 2}, #{<<"to_user_id">> => 3}],
        maps:get(<<"friends">>, Out)
    ).

%% atom 键（部分 DS 返回 atom key）也要判定
sanitize_handles_atom_keys_test() ->
    Out = user_export_logic:sanitize(#{password => <<"x">>, nickname => <<"n">>}),
    ?assertEqual([nickname], maps:keys(Out)).

sensitive_key_predicate_test() ->
    lists:foreach(
        fun(K) -> ?assert(user_export_logic:sensitive_key(K)) end,
        [<<"password">>, <<"token">>, <<"secret">>, <<"salt">>, <<"private_key">>]
    ),
    lists:foreach(
        fun(K) -> ?assertNot(user_export_logic:sensitive_key(K)) end,
        [<<"id">>, <<"nickname">>, <<"created_at">>, <<"region">>]
    ).

%%%===================================================================
%%% Legal Hold 显式不支持（不得静默省略）
%%%===================================================================

legal_hold_explicitly_unsupported_test() ->
    S = user_export_logic:legal_hold_status(),
    ?assertEqual(false, maps:get(<<"supported">>, S)),
    ?assert(byte_size(maps:get(<<"reason">>, S)) > 0).

%%%===================================================================
%%% 范围声明（P-01：范围由 legal review 决定，不声称 GDPR 完整）
%%%===================================================================

scope_declares_categories_and_exclusions_test() ->
    S = user_export_logic:scope(),
    ?assertEqual(
        [<<"user_info">>, <<"friends">>, <<"groups">>, <<"settings">>],
        maps:get(<<"categories">>, S)
    ),
    Excluded = maps:get(<<"excluded">>, S),
    %% 排除项逐项点名：静默缺失会被用户与审计方误读为已覆盖
    lists:foreach(
        fun(K) -> ?assert(lists:member(K, Excluded)) end,
        [<<"messages">>, <<"moments">>, <<"attachments">>]
    ),
    ?assert(byte_size(maps:get(<<"disclaimer">>, S)) > 0),
    ?assert(is_integer(maps:get(<<"friends_limit">>, S))),
    ?assert(is_integer(maps:get(<<"groups_limit">>, S))).

%%%===================================================================
%%% 导出主流程
%%%===================================================================

export_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun export_returns_schema_and_audits/0,
        fun export_strips_sensitive_from_ds_payload/0,
        fun export_strips_sensitive_from_user_info_schema_drift/0,
        fun export_passes_truncation_markers/0,
        fun export_rejects_invalid_uid/0,
        fun export_propagates_ds_error/0,
        fun export_survives_audit_failure/0,
        fun export_rejects_within_cool_down/0,
        fun export_allows_after_cool_down/0,
        fun cool_down_disabled_when_zero/0,
        fun cool_down_check_failure_fails_open/0
    ]}.

setup() ->
    meck:new(user_ds, [passthrough, non_strict]),
    meck:new(user_log_ds, [passthrough, non_strict]),
    meck:expect(user_log_ds, add_internal, fun(_C, _T, _U, _B, _Ts) -> {ok, 1} end),
    %% 冷却默认放行（从未导出）；各冷却用例自行覆盖
    meck:expect(user_log_ds, last_export_at, fun(_Uid) -> {ok, undefined} end),
    ok.

cleanup(_) ->
    catch meck:unload(user_log_ds),
    catch meck:unload(user_ds),
    catch application:unset_env(imboy, user_export_cool_down_ms),
    ok.

ds_payload() ->
    #{
        <<"user_info">> => #{<<"id">> => 7, <<"nickname">> => <<"n">>},
        <<"friends">> => [#{<<"to_user_id">> => 8}],
        <<"friends_truncated">> => false,
        <<"groups">> => [],
        <<"groups_truncated">> => false,
        <<"settings">> => #{<<"theme">> => <<"dark">>},
        <<"exported_at">> => 1234
    }.

export_returns_schema_and_audits() ->
    meck:expect(user_ds, export_data_bounded, fun(7) -> {ok, ds_payload()} end),
    {ok, Data} = user_export_logic:export(7, fake_req()),
    lists:foreach(
        fun(K) -> ?assert(maps:is_key(K, Data)) end,
        [
            <<"user_info">>,
            <<"friends">>,
            <<"groups">>,
            <<"settings">>,
            <<"exported_at">>,
            <<"legal_hold">>,
            <<"scope">>
        ]
    ),
    %% 导出必须留下不可变审计记录，type=130
    Calls = [A || {_P, {user_log_ds, add_internal, A}, _R} <- meck:history(user_log_ds)],
    ?assertMatch([[_, 130, 7, _, _]], Calls),
    [[_, _, _, Body, _]] = Calls,
    Decoded = jsone:decode(Body, [{object_format, map}]),
    ?assertEqual(<<"user_data_export">>, maps:get(<<"action">>, Decoded)).

%% DS 层 jsonb 自由键将来新增凭据类设置项必须被兜底剥离
export_strips_sensitive_from_ds_payload() ->
    Payload = maps:put(
        <<"settings">>,
        #{<<"theme">> => <<"dark">>, <<"push_token">> => <<"leak">>},
        ds_payload()
    ),
    meck:expect(user_ds, export_data_bounded, fun(7) -> {ok, Payload} end),
    {ok, Data} = user_export_logic:export(7, fake_req()),
    ?assertEqual(#{<<"theme">> => <<"dark">>}, maps:get(<<"settings">>, Data)).

%% schema 漂移：user_info 将来新增敏感列（列 allowlist 失误）同样被剥
export_strips_sensitive_from_user_info_schema_drift() ->
    Payload = maps:put(
        <<"user_info">>,
        #{
            <<"id">> => 7,
            <<"nickname">> => <<"n">>,
            <<"password_hash">> => <<"leak">>,
            <<"smtp_credential">> => <<"leak2">>
        },
        ds_payload()
    ),
    meck:expect(user_ds, export_data_bounded, fun(7) -> {ok, Payload} end),
    {ok, Data} = user_export_logic:export(7, fake_req()),
    ?assertEqual(
        #{<<"id">> => 7, <<"nickname">> => <<"n">>},
        maps:get(<<"user_info">>, Data)
    ).

%% bounded 截断标记必须透传给用户（诚实截断，不静默）
export_passes_truncation_markers() ->
    Base = ds_payload(),
    Payload = Base#{
        <<"friends">> => [#{<<"to_user_id">> => N} || N <- lists:seq(1, 10)],
        <<"friends_truncated">> => true,
        <<"groups_truncated">> => true
    },
    meck:expect(user_ds, export_data_bounded, fun(7) -> {ok, Payload} end),
    {ok, Data} = user_export_logic:export(7, fake_req()),
    ?assert(maps:get(<<"friends_truncated">>, Data)),
    ?assert(maps:get(<<"groups_truncated">>, Data)),
    ?assertEqual(10, length(maps:get(<<"friends">>, Data))).

%% 范围限制：uid 非法一律拒绝，不回退到任何默认账号
export_rejects_invalid_uid() ->
    meck:expect(user_ds, export_data_bounded, fun(_) ->
        erlang:error(should_not_be_called)
    end),
    lists:foreach(
        fun(Uid) ->
            ?assertEqual({error, invalid_uid}, user_export_logic:export(Uid, fake_req()))
        end,
        [0, -1, undefined, <<"7">>]
    ).

export_propagates_ds_error() ->
    meck:expect(user_ds, export_data_bounded, fun(7) -> {error, db_down} end),
    ?assertEqual({error, db_down}, user_export_logic:export(7, fake_req())).

%% 审计写失败不能吞掉用户的数据权
export_survives_audit_failure() ->
    meck:expect(user_ds, export_data_bounded, fun(7) -> {ok, ds_payload()} end),
    meck:expect(user_log_ds, add_internal, fun(_C, _T, _U, _B, _Ts) -> {error, db_down} end),
    ?assertMatch({ok, _}, user_export_logic:export(7, fake_req())).

%%%===================================================================
%%% 冷却限频（P-01：上次导出审计行后窗口内拒绝，防反复导出刷库）
%%%===================================================================

%% 窗口内（1h 前 < 24h 窗口）拒绝，剩余毫秒落在 (0, 窗口]
export_rejects_within_cool_down() ->
    HourAgo = elib_dt:to_rfc3339(elib_dt:millisecond() - 3600000),
    meck:expect(user_log_ds, last_export_at, fun(7) -> {ok, HourAgo} end),
    meck:expect(user_ds, export_data_bounded, fun(_) -> erlang:error(should_not_be_called) end),
    {error, {cool_down, Remaining}} = user_export_logic:export(7, fake_req()),
    ?assert(Remaining > 0),
    ?assert(Remaining =< 86400000),
    %% 被拒的请求不得执行导出，也不得写审计行
    ?assertEqual(0, length(meck:history(user_ds))),
    Calls = [A || {_P, {user_log_ds, add_internal, A}, _R} <- meck:history(user_log_ds)],
    ?assertEqual([], Calls).

%% 窗口外（25h 前 > 24h 窗口）放行
export_allows_after_cool_down() ->
    Hours25Ago = elib_dt:to_rfc3339(elib_dt:millisecond() - 90000000),
    meck:expect(user_log_ds, last_export_at, fun(7) -> {ok, Hours25Ago} end),
    meck:expect(user_ds, export_data_bounded, fun(7) -> {ok, ds_payload()} end),
    ?assertMatch({ok, _}, user_export_logic:export(7, fake_req())).

%% 配置 user_export_cool_down_ms=0 关闭冷却（运维/测试开关）
cool_down_disabled_when_zero() ->
    ok = application:set_env(imboy, user_export_cool_down_ms, 0),
    HourAgo = elib_dt:to_rfc3339(elib_dt:millisecond() - 3600000),
    meck:expect(user_log_ds, last_export_at, fun(7) -> {ok, HourAgo} end),
    meck:expect(user_ds, export_data_bounded, fun(7) -> {ok, ds_payload()} end),
    ?assertMatch({ok, _}, user_export_logic:export(7, fake_req())).

%% 冷却检查自身故障放行（fail-open：数据权优先，与审计失败不阻断同向）
cool_down_check_failure_fails_open() ->
    meck:expect(user_log_ds, last_export_at, fun(7) -> {error, db_down} end),
    meck:expect(user_ds, export_data_bounded, fun(7) -> {ok, ds_payload()} end),
    ?assertMatch({ok, _}, user_export_logic:export(7, fake_req())).

%% cowboy_req 只被 cowboy_req:header/3 读取，用最小 map 即可
fake_req() ->
    #{headers => #{}}.
