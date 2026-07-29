-module(user_export_logic_tests).

%%%===================================================================
%%% @doc user_export_logic 个人数据导出测试（C0-GOV-01）
%%%
%%% 覆盖：导出 schema、敏感字段剥离（含嵌套与 SELECT * 新增列场景）、
%%%       Legal Hold 显式不支持、范围限制（非法 uid 拒绝）、
%%%       导出行为写审计、审计失败不阻断导出。
%%% @end
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

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
%%% 导出主流程
%%%===================================================================

export_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun export_returns_schema_and_audits/0,
        fun export_strips_sensitive_from_ds_payload/0,
        fun export_rejects_invalid_uid/0,
        fun export_propagates_ds_error/0,
        fun export_survives_audit_failure/0
    ]}.

setup() ->
    meck:new(user_ds, [passthrough, non_strict]),
    meck:new(user_log_ds, [passthrough, non_strict]),
    meck:expect(user_log_ds, add_internal, fun(_C, _T, _U, _B, _Ts) -> {ok, 1} end),
    ok.

cleanup(_) ->
    catch meck:unload(user_log_ds),
    catch meck:unload(user_ds),
    ok.

ds_payload() ->
    #{
        <<"user_info">> => #{<<"id">> => 7, <<"nickname">> => <<"n">>},
        <<"friends">> => [#{<<"to_user_id">> => 8}],
        <<"groups">> => [],
        <<"settings">> => #{<<"theme">> => <<"dark">>},
        <<"exported_at">> => 1234
    }.

export_returns_schema_and_audits() ->
    meck:expect(user_ds, export_data, fun(7) -> {ok, ds_payload()} end),
    {ok, Data} = user_export_logic:export(7, fake_req()),
    lists:foreach(
        fun(K) -> ?assert(maps:is_key(K, Data)) end,
        [
            <<"user_info">>,
            <<"friends">>,
            <<"groups">>,
            <<"settings">>,
            <<"exported_at">>,
            <<"legal_hold">>
        ]
    ),
    %% 导出必须留下不可变审计记录，type=130
    Calls = [A || {_P, {user_log_ds, add_internal, A}, _R} <- meck:history(user_log_ds)],
    ?assertMatch([[_, 130, 7, _, _]], Calls),
    [[_, _, _, Body, _]] = Calls,
    Decoded = jsone:decode(Body, [{object_format, map}]),
    ?assertEqual(<<"user_data_export">>, maps:get(<<"action">>, Decoded)).

%% DS 用 SELECT * 取 user_setting，将来新增凭据列必须被兜底剥离
export_strips_sensitive_from_ds_payload() ->
    Payload = maps:put(
        <<"settings">>,
        #{<<"theme">> => <<"dark">>, <<"push_token">> => <<"leak">>},
        ds_payload()
    ),
    meck:expect(user_ds, export_data, fun(7) -> {ok, Payload} end),
    {ok, Data} = user_export_logic:export(7, fake_req()),
    ?assertEqual(#{<<"theme">> => <<"dark">>}, maps:get(<<"settings">>, Data)).

%% 范围限制：uid 非法一律拒绝，不回退到任何默认账号
export_rejects_invalid_uid() ->
    meck:expect(user_ds, export_data, fun(_) -> erlang:error(should_not_be_called) end),
    lists:foreach(
        fun(Uid) ->
            ?assertEqual({error, invalid_uid}, user_export_logic:export(Uid, fake_req()))
        end,
        [0, -1, undefined, <<"7">>]
    ).

export_propagates_ds_error() ->
    meck:expect(user_ds, export_data, fun(7) -> {error, db_down} end),
    ?assertEqual({error, db_down}, user_export_logic:export(7, fake_req())).

%% 审计写失败不能吞掉用户的数据权
export_survives_audit_failure() ->
    meck:expect(user_ds, export_data, fun(7) -> {ok, ds_payload()} end),
    meck:expect(user_log_ds, add_internal, fun(_C, _T, _U, _B, _Ts) -> {error, db_down} end),
    ?assertMatch({ok, _}, user_export_logic:export(7, fake_req())).

%% cowboy_req 只被 cowboy_req:header/3 读取，用最小 map 即可
fake_req() ->
    #{headers => #{}}.
