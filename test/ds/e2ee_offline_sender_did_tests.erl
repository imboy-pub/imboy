%% @doc A2-a：离线（decrypt-on-read）路径必须携带服务端验证过的 `sender_did`。
%%
%% == 背景 ==
%%
%% PFv3 接收侧 `_validateContextBinding` 第 6 项（ADR 15 §3.3）拿**信封顶层**的
%% `sender_did` 与受认证的 `protected_header.sender_did` 硬比对。
%% 实时投递路径已由 `message_ds:stamp_sender_device/2` + `with_sender_device/2`
%% 盖上该字段（见 `e2ee_sender_device_envelope_tests`）。
%%
%% **离线路径没有**：发送者设备标识从未被持久化——
%%   - `msg_store_staging` / `msg_c2c` 两张表都没有设备列；
%%   - `msg_c2c_ds:read_msg_filter/3` 的列集里也没有；
%%   - `message_ds:offline_envelope/2` 组装出的信封里自然也没有。
%% 后果：离线期间收到的 C2C v3 消息，重连拉取后被判
%% `context_mismatch_sender_did` 而永久不可读。
%%
%% 证据：`imboy/docs/guides/e2ee/v2/evidence/E2EE-012-024-025-029-reacceptance.md` §6.1。
%%
%% == 本文件守护的三个断点 ==
%%
%% 1. 写入：`msg_store_repo:stage/11` 必须把 `sender_did` 落进 staging 行；
%%    旧的 `stage/10` 调用方（c2g / s2c / c2s / agent）行为不得改变。
%% 2. 读取：`msg_c2c_ds:read_msg_for_device/4` 的列集必须含 `sender_did`。
%% 3. 出站：`message_ds:offline_envelope/2` 必须把该值带上信封顶层，
%%    且**没有时不得伪造空值**（空串会让接收侧把「服务端没提供」误判成
%%    「设备 ID 是空串」，两者失败语义不同）。
%%
%% 端到端（真 PostgreSQL：staging → worker → msg_c2c → 读回）由
%% `e2ee_message_pipeline_integration_tests` 的
%% `test_sender_did_survives_pipeline_to_offline_envelope/0` 闭合。
-module(e2ee_offline_sender_did_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DID, <<"dev-sender-offline-01">>).

%% ===================================================================
%% 1. 读取列集
%% ===================================================================

%% 生产入口：messaging_logic:offline/6 与 message_ds:check_and_notify_offline_msgs/2
%% 都经 msg_c2c_ds:read_msg_for_device/4 取离线行。
read_msg_for_device_selects_sender_did_test() ->
    meck:new(msg_c2c_repo, [passthrough]),
    try
        meck:expect(msg_c2c_repo, read_msg, fun(_Where, Column, _Limit, _Params) ->
            persistent_term:put({?MODULE, column}, Column),
            {ok, []}
        end),
        _ = msg_c2c_ds:read_msg_for_device(1, ?DID, 10, undefined),
        Column = persistent_term:get({?MODULE, column}),
        ?assert(binary:match(Column, <<"sender_did">>) =/= nomatch),
        %% 对照：既有列一个都不能丢（收紧列集不得误删）
        [
            ?assert(binary:match(Column, C) =/= nomatch)
         || C <- [<<"payload">>, <<"from_id">>, <<"to_id">>, <<"msg_id">>, <<"e2ee">>]
        ]
    after
        persistent_term:erase({?MODULE, column}),
        _ = (catch meck:unload(msg_c2c_repo))
    end.

%% 无 DID 的旧客户端走 read_msg/3，列集同源，同样必须带上
read_msg_selects_sender_did_test() ->
    meck:new(msg_c2c_repo, [passthrough]),
    try
        meck:expect(msg_c2c_repo, read_msg, fun(_Where, Column, _Limit, _Params) ->
            persistent_term:put({?MODULE, column2}, Column),
            {ok, []}
        end),
        _ = msg_c2c_ds:read_msg(1, 10),
        Column = persistent_term:get({?MODULE, column2}),
        ?assert(binary:match(Column, <<"sender_did">>) =/= nomatch)
    after
        persistent_term:erase({?MODULE, column2}),
        _ = (catch meck:unload(msg_c2c_repo))
    end.

%% ===================================================================
%% 2. 出站信封
%% ===================================================================

row(Extra) ->
    maps:merge(
        #{
            <<"id">> => <<"m-offline-1">>,
            <<"from_id">> => 100,
            <<"to_id">> => 200,
            <<"payload">> => <<>>,
            <<"msg_type">> => <<"text">>,
            <<"e2ee">> => #{
                <<"meta_version">> => 3,
                <<"devices">> => #{
                    ?DID => #{<<"ciphertext">> => <<"Y2lwaGVy">>}
                }
            },
            <<"created_at">> => 1750000000000,
            <<"server_ts">> => 1750000000001
        },
        Extra
    ).

offline_envelope_carries_sender_did_test() ->
    Msg = message_ds:offline_envelope(<<"C2C">>, row(#{<<"sender_did">> => ?DID})),
    ?assertEqual(?DID, maps:get(<<"sender_did">>, Msg)).

%% 不得伪造：列为 NULL（epgsql 返回 null）时信封不得出现该键
offline_envelope_null_sender_did_not_fabricated_test() ->
    Msg = message_ds:offline_envelope(<<"C2C">>, row(#{<<"sender_did">> => null})),
    ?assertEqual(error, maps:find(<<"sender_did">>, Msg)).

%% 同上：列缺失（旧 schema / 旧行）时也不得伪造
offline_envelope_absent_sender_did_not_fabricated_test() ->
    Msg = message_ds:offline_envelope(<<"C2C">>, row(#{})),
    ?assertEqual(error, maps:find(<<"sender_did">>, Msg)).

%% 空串同样视为「没提供」——落库若写了空串，不得当作有效设备标识下发
offline_envelope_empty_sender_did_not_fabricated_test() ->
    Msg = message_ds:offline_envelope(<<"C2C">>, row(#{<<"sender_did">> => <<>>})),
    ?assertEqual(error, maps:find(<<"sender_did">>, Msg)).

%% 【对照组 / 正向可用性】收紧不得以牺牲既有字段为代价：
%% payload 与 e2ee 必须逐字段原样透传（E2EE-060 不透明透传契约）。
%% 本用例在修改前后都必须绿——若它在改前就红，说明 harness 有缺陷。
offline_envelope_preserves_payload_and_e2ee_test() ->
    Row = row(#{<<"sender_did">> => ?DID}),
    Msg = message_ds:offline_envelope(<<"C2C">>, Row),
    ?assertEqual(maps:get(<<"e2ee">>, Row), maps:get(<<"e2ee">>, Msg)),
    ?assertEqual(<<>>, maps:get(<<"payload">>, Msg)),
    ?assertEqual(<<"m-offline-1">>, maps:get(<<"id">>, Msg)),
    ?assertEqual(<<"C2C">>, maps:get(<<"type">>, Msg)),
    ?assertEqual(100, maps:get(<<"from">>, Msg)),
    ?assertEqual(200, maps:get(<<"to">>, Msg)).

%% ===================================================================
%% 3. staging 写入
%% ===================================================================

capture_insert() ->
    meck:new(elib_tsid, [passthrough]),
    meck:new(elib_pg_sql, [passthrough]),
    meck:new(elib_pg, [passthrough]),
    meck:expect(elib_tsid, generate, fun(_Name) -> 4242 end),
    meck:expect(elib_pg_sql, insert, fun(_Tb, Data) ->
        persistent_term:put({?MODULE, insert_data}, Data),
        {<<"INSERT INTO t DEFAULT VALUES">>, []}
    end),
    meck:expect(elib_pg, query, fun(_Sql, _Params) -> {ok, 1} end).

release_insert() ->
    persistent_term:erase({?MODULE, insert_data}),
    _ = (catch meck:unload(elib_pg)),
    _ = (catch meck:unload(elib_pg_sql)),
    _ = (catch meck:unload(elib_tsid)),
    ok.

stage_persists_sender_did_test() ->
    capture_insert(),
    try
        _ = msg_store_repo:stage(
            <<"c2c">>,
            <<"m-1">>,
            <<"text">>,
            <<"message">>,
            #{<<"meta_version">> => 3},
            <<>>,
            100,
            200,
            <<"2026-07-28T00:00:00+08:00">>,
            <<"2026-07-28T00:00:00+08:00">>,
            ?DID
        ),
        Data = persistent_term:get({?MODULE, insert_data}),
        ?assertEqual(?DID, maps:get(sender_did, Data))
    after
        release_insert()
    end.

%% 向后兼容：旧的 stage/10（c2g / s2c / c2s / agent 主动消息仍在用）
%% 不得因本次扩参而多写一个空值列——空串不是设备标识。
stage_legacy_arity_omits_sender_did_test() ->
    capture_insert(),
    try
        _ = msg_store_repo:stage(
            <<"s2c">>,
            <<"m-2">>,
            <<"custom">>,
            <<"pull_offline_msg">>,
            null,
            <<"{}">>,
            100,
            200,
            <<"2026-07-28T00:00:00+08:00">>,
            <<"2026-07-28T00:00:00+08:00">>
        ),
        Data = persistent_term:get({?MODULE, insert_data}),
        ?assertEqual(error, maps:find(sender_did, Data))
    after
        release_insert()
    end.

%% 群聊 fan-out（to_id_list 分支）同样支持，且缺省不写空值
stage_group_arity_omits_sender_did_test() ->
    capture_insert(),
    try
        _ = msg_store_repo:stage(
            <<"c2g">>,
            <<"m-3">>,
            <<"text">>,
            <<"message">>,
            null,
            <<"{}">>,
            100,
            [200, 300],
            <<"2026-07-28T00:00:00+08:00">>,
            <<"2026-07-28T00:00:00+08:00">>
        ),
        Data = persistent_term:get({?MODULE, insert_data}),
        ?assertEqual(error, maps:find(sender_did, Data)),
        ?assertEqual([200, 300], maps:get(to_id_list, Data))
    after
        release_insert()
    end.

%% ===================================================================
%% 4. staging → 正式表：claim_pending 必须把该列读出来
%% ===================================================================

%% worker 从 claim_pending 的结果行里取 sender_did；列不在 SELECT 里
%% 等于全链路在此处断掉（且不会报错，只是静默丢字段）。
claim_pending_selects_sender_did_test() ->
    meck:new(elib_pg, [passthrough]),
    try
        meck:expect(elib_pg, with_tx, fun(Fun) -> Fun(fake_conn) end),
        meck:expect(elib_pg, query, fun(_Conn, Sql, _Params) ->
            persistent_term:put({?MODULE, claim_sql}, iolist_to_binary(Sql)),
            {ok, []}
        end),
        _ = msg_store_repo:claim_pending(10, 30),
        Sql = persistent_term:get({?MODULE, claim_sql}),
        ?assert(binary:match(Sql, <<"sender_did">>) =/= nomatch)
    after
        persistent_term:erase({?MODULE, claim_sql}),
        _ = (catch meck:unload(elib_pg))
    end.

%% ===================================================================
%% 5. 全新安装与存量部署的 schema 不得分叉
%% ===================================================================

%% staging 表不由迁移创建，而是 msg_store_repo:ensure_table_exists/0 的
%% CREATE TABLE IF NOT EXISTS。迁移只覆盖存量部署；漏改 DDL = 全新安装少一列。
ensure_table_ddl_has_sender_did_test() ->
    meck:new(elib_pg, [passthrough]),
    try
        meck:expect(elib_pg, execute, fun(Sql, _Params) ->
            Bin = iolist_to_binary(Sql),
            case binary:match(Bin, <<"CREATE TABLE IF NOT EXISTS">>) of
                nomatch -> ok;
                _ -> persistent_term:put({?MODULE, ddl}, Bin)
            end,
            {ok, 0}
        end),
        _ = msg_store_repo:ensure_table_exists(),
        Ddl = persistent_term:get({?MODULE, ddl}),
        ?assert(binary:match(Ddl, <<"sender_did">>) =/= nomatch)
    after
        persistent_term:erase({?MODULE, ddl}),
        _ = (catch meck:unload(elib_pg))
    end.
