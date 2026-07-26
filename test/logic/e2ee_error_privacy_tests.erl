-module(e2ee_error_privacy_tests).
%%%===================================================================
%%% @doc S19 错误响应隐私守护测试
%%%
%%% 不变量：Logic 层返回给 Handler 的 {error, Msg} 中 Msg 必须是
%%% 不透明的 binary（如 <<"internal_error">>），绝不可是原始 Erlang
%%% 项（epgsql 错误元组、map、list 等），否则经 elib_cnv:safe_to_binary
%%% 的 ~p 兜底会将数据库 schema 元信息泄露给客户端。
%%%
%%% 模拟的 "真实" DB 错误格式：
%%%   {error, error, <<"23505">>, <<"duplicate key value violates unique constraint">>, []}
%%%   {error, #{code => <<"42P01">>, message => <<"relation \"x\" does not exist">>}}
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

-define(WITH_MECKS(Modules, Fun),
    (fun() ->
        ok = meck:new(Modules, [passthrough, no_link]),
        try
            Fun()
        after
            meck:unload(Modules)
        end
    end)()
).

%% 模拟 DS 层返回的真实 DB 错误（2-tuple 包裹 epgsql 内部元组）
-define(FAKE_DB_ERROR,
    {error,
        {error, error, <<"23505">>,
            <<"duplicate key value violates unique constraint \"user_device_pkey\"">>, []}}
).
-define(FAKE_DB_MAP_ERROR,
    {error, #{
        code => <<"42P01">>,
        message => <<"relation \"user_device\" does not exist">>,
        severity => <<"ERROR">>
    }}
).

%% ===================================================================
%% e2ee_logic:report_device_key — 设备保存失败
%% ===================================================================

report_device_key_save_error_opaque_test() ->
    ?WITH_MECKS([user_device_ds, elib_dt], fun() ->
        meck:expect(elib_dt, now, fun() -> 1700000000 end),
        %% update_public_key 返回 {ok, 0} → 走 save 分支
        meck:expect(user_device_ds, update_public_key, fun(_, _, _, _, _) -> {ok, 0} end),
        %% save 返回原始 epgsql 错误（2-tuple）
        meck:expect(user_device_ds, save, fun(_, _, _, _) -> ?FAKE_DB_ERROR end),
        Result = e2ee_logic:report_device_key(
            1, <<"did1">>, <<"android">>, <<"phone">>, <<"pk">>, <<"kid1">>
        ),
        %% 不变量：错误必须是不透明 binary，不含 schema 信息
        ?assertMatch({error, Msg} when is_binary(Msg), Result),
        {error, Msg} = Result,
        ?assertEqual(nomatch, binary:match(Msg, <<"user_device">>)),
        ?assertEqual(nomatch, binary:match(Msg, <<"23505">>)),
        ?assertEqual(nomatch, binary:match(Msg, <<"constraint">>))
    end).

report_device_key_update_error_opaque_test() ->
    ?WITH_MECKS([user_device_ds, elib_dt], fun() ->
        meck:expect(elib_dt, now, fun() -> 1700000000 end),
        %% update_public_key 直接返回原始 DB 错误（2-tuple）
        meck:expect(user_device_ds, update_public_key, fun(_, _, _, _, _) ->
            ?FAKE_DB_MAP_ERROR
        end),
        Result = e2ee_logic:report_device_key(
            1, <<"did1">>, <<"ios">>, <<"iphone">>, <<"pk">>, <<"kid1">>
        ),
        ?assertMatch({error, Msg} when is_binary(Msg), Result),
        {error, Msg} = Result,
        ?assertEqual(nomatch, binary:match(Msg, <<"42P01">>)),
        ?assertEqual(nomatch, binary:match(Msg, <<"relation">>))
    end).

%% ===================================================================
%% e2ee_logic:pull_key_changes_from_db — 查询失败
%% ===================================================================

pull_key_changes_db_error_opaque_test() ->
    ok = meck:new([elib_pg, elib_dt, friend_ds, elib_cnv], [no_link]),
    try
        meck:expect(elib_cnv, safe_to_integer, fun
            (V) when is_integer(V) -> V;
            (_) -> 0
        end),
        meck:expect(elib_dt, to_rfc3339, fun(_) -> <<"2024-01-01T00:00:00Z">> end),
        meck:expect(friend_ds, list_by_uid, fun(_) -> [10, 20, 30] end),
        %% 模拟 DB 连接失败
        meck:expect(elib_pg, query, fun(_, _) ->
            {error, {error, error, <<"08006">>, <<"connection_failure">>, []}}
        end),
        Result = e2ee_logic:pull_key_notifications(1, 0, 50),
        ?assertMatch({error, Msg} when is_binary(Msg), Result),
        {error, Msg} = Result,
        ?assertEqual(nomatch, binary:match(Msg, <<"08006">>)),
        ?assertEqual(nomatch, binary:match(Msg, <<"connection">>))
    after
        meck:unload([elib_pg, elib_dt, friend_ds, elib_cnv])
    end.

%% ===================================================================
%% e2ee_recovery_logic:start_server_backup_recovery — 备份查询失败
%% ===================================================================

recovery_backup_db_error_opaque_test() ->
    ?WITH_MECKS([e2ee_backup_ds], fun() ->
        %% 模拟 epgsql 超时错误
        meck:expect(e2ee_backup_ds, latest, fun(_) ->
            {error,
                {error, error, <<"57014">>, <<"canceling statement due to statement timeout">>, []}}
        end),
        Result = e2ee_recovery_logic:start_auto_recovery(1, <<"did1">>, <<"server_backup">>),
        ?assertMatch({error, Msg} when is_binary(Msg), Result),
        {error, Msg} = Result,
        ?assertEqual(nomatch, binary:match(Msg, <<"57014">>)),
        ?assertEqual(nomatch, binary:match(Msg, <<"timeout">>)),
        ?assertEqual(nomatch, binary:match(Msg, <<"canceling">>))
    end).

%% ===================================================================
%% 通用不变量：所有 E2EE logic 模块的 error 返回值必须是 binary
%% ===================================================================

error_msg_type_invariant_test() ->
    %% 收集所有可能的 error msg 类型，验证均为 binary
    ?WITH_MECKS([user_device_ds, elib_dt, e2ee_backup_ds], fun() ->
        meck:expect(elib_dt, now, fun() -> 1700000000 end),
        meck:expect(user_device_ds, update_public_key, fun(_, _, _, _, _) -> {ok, 0} end),
        meck:expect(user_device_ds, save, fun(_, _, _, _) -> {error, some_atom_reason} end),
        meck:expect(e2ee_backup_ds, latest, fun(_) -> {error, {tcp_error, closed}} end),

        R1 = e2ee_logic:report_device_key(1, <<"d">>, <<"a">>, <<"n">>, <<"pk">>, <<"k">>),
        R3 = e2ee_recovery_logic:start_auto_recovery(1, <<"d">>, <<"server_backup">>),

        %% 所有错误消息必须是 binary（handler 可安全序列化为 JSON string）
        lists:foreach(
            fun
                ({error, {Msg, _Code}}) ->
                    %% {Msg, ErrorCode} 格式也要求 Msg 是 binary
                    ?assert(is_binary(Msg));
                ({error, Msg}) ->
                    ?assert(is_binary(Msg))
            end,
            [R1, R3]
        )
    end).
