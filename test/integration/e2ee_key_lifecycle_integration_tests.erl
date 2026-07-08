%% @doc E2EE 密钥生命周期（设备传输 / 社交恢复 / 分片审计日志）真库集成测试。
%%
%% 背景：审查发现 e2ee_transfer/e2ee_social/e2ee_shard_transmission_log
%% 三个 repo 模块的现有测试 100% 是 meck，从未有一条 SQL 真正打到过
%% PostgreSQL。真库跑通后当场复现一个此前完全未知的活跃 bug：
%% `e2ee_social_repo:add_contact/1` 的 INSERT/UPDATE 语句列名写成
%% `nickname`，而 `e2ee_trusted_contacts` 表实际列名是 `contact_nickname`
%% （00000004_social.up.sql），导致可信联系人功能对真库调用 100% 失败
%% （PG 42703 undefined_column）。已修复，此处补回归。
%%
%% 同时覆盖两个"只有真库能验证"的高危模式：
%% - CAS 更新的并发语义（update_status_and_device 第二次调用应返回 conflict）
%% - GROUP BY + COUNT(*) 的类型解码（epgsql 是否把 bigint 正确解码为
%%   Erlang integer，can_recover/2 的 is_integer(Count) 守卫依赖这一点）
-module(e2ee_key_lifecycle_integration_tests).

-include_lib("eunit/include/eunit.hrl").

e2ee_key_lifecycle_test_() ->
    _ = eunit_runner:eunit_setup(),
    application:set_env(imboy, env, test),
    case eunit_runner:eunit_try_db() of
        {ok, _Driver, _Conn} ->
            {foreach, fun setup/0, fun cleanup/1, [
                {"分片传输审计日志：JSONB metadata 真库写入不崩溃", fun test_shard_transmission_log_insert/0},
                {"设备间传输会话：创建 + CAS 状态流转（并发保护）", fun test_transfer_session_create_and_cas/0},
                {"社交恢复可行性判断：GROUP BY/COUNT 类型解码正确", fun test_social_can_recover_group_by/0},
                {"可信联系人：ON CONFLICT upsert 真正生效（回归 42703）", fun test_social_add_contact_upsert/0},
                {"社交恢复建分片（生产入口 create_shards/4）：k-of-n 多分片必须能全部写入",
                    fun test_create_shards_multi_shard_production_entrypoint/0}
            ]};
        {error, _Reason} ->
            {"Database not available", fun() -> {skip, "Database not available"} end}
    end.

setup() ->
    Uid = elib_tsid:generate(),
    ProxyUid = elib_tsid:generate(),
    Context = #{uid => Uid, proxy_uid => ProxyUid},
    persistent_term:put({?MODULE, test_context}, Context),
    Context.

cleanup(#{uid := Uid}) ->
    {ok, _} = elib_pg:execute(
        <<"DELETE FROM e2ee_shard_transmission_log WHERE uid = $1">>, [Uid]
    ),
    {ok, _} = elib_pg:execute(
        <<"DELETE FROM e2ee_transfer_sessions WHERE from_uid = $1">>, [Uid]
    ),
    {ok, _} = elib_pg:execute(<<"DELETE FROM e2ee_social_shards WHERE uid = $1">>, [Uid]),
    {ok, _} = elib_pg:execute(
        <<"DELETE FROM e2ee_trusted_contacts WHERE uid = $1 OR contact_uid = $1">>, [Uid]
    ),
    persistent_term:erase({?MODULE, test_context}),
    ok.

get_context() ->
    persistent_term:get({?MODULE, test_context}).

%% ===================================================================
%% 测试用例
%% ===================================================================

test_shard_transmission_log_insert() ->
    #{uid := Uid, proxy_uid := ProxyUid} = get_context(),
    ShardId = <<"shard_", (integer_to_binary(elib_tsid:generate()))/binary>>,
    %% 与实际调用方 e2ee_shard_validator.erl 一致：metadata 必须先
    %% jsone:encode 成 binary 再传入，不能传裸 Erlang map
    Metadata = jsone:encode(#{<<"shard_index">> => 1, <<"total_shards">> => 5}, [native_utf8]),

    Result = e2ee_shard_transmission_log_repo:insert(#{
        shard_id => ShardId,
        key_version => <<"latest">>,
        uid => Uid,
        proxy_uid => ProxyUid,
        action => <<"shard_created">>,
        direction => <<"server_to_proxy">>,
        metadata => Metadata
    }),
    ?assertMatch({ok, _}, Result),

    {ok, Logs} = e2ee_shard_transmission_log_repo:list_by_shard_id(ShardId),
    ?assertEqual(1, length(Logs)).

test_transfer_session_create_and_cas() ->
    #{uid := FromUid, proxy_uid := ToUid} = get_context(),
    SessionId = elib_uuid:gen_v7(),
    ExpiresAt = elib_dt:to_rfc3339(elib_dt:millisecond() + 300000, millisecond),

    CreateResult = e2ee_transfer_repo:create(#{
        <<"session_id">> => SessionId,
        <<"from_uid">> => FromUid,
        <<"from_device_id">> => <<"device_a">>,
        <<"to_uid">> => ToUid,
        <<"encrypted_key_bundle">> => <<"encrypted_bundle_placeholder">>,
        <<"expires_at">> => ExpiresAt
    }),
    ?assertMatch({ok, _}, CreateResult),

    {ok, Session} = e2ee_transfer_repo:get_by_session_id(SessionId),
    ?assertEqual(<<"pending">>, maps:get(<<"status">>, Session)),

    %% 第一次 accept：pending -> accepted，CAS 条件满足
    ?assertEqual(
        ok,
        e2ee_transfer_repo:update_status_and_device(SessionId, <<"accepted">>, <<"device_b">>, 300)
    ),

    %% 第二次再 accept：状态已不是 pending，CAS 应拒绝而非静默覆盖
    ?assertEqual(
        {error, conflict},
        e2ee_transfer_repo:update_status_and_device(SessionId, <<"accepted">>, <<"device_c">>, 300)
    ),

    {ok, Session2} = e2ee_transfer_repo:get_by_session_id(SessionId),
    ?assertEqual(<<"device_b">>, maps:get(<<"to_device_id">>, Session2)).

test_social_can_recover_group_by() ->
    #{uid := Uid, proxy_uid := ProxyUid} = get_context(),
    KeyVersion = <<"latest">>,
    %% 建 3 个分片（threshold=2, total=3），应可恢复
    lists:foreach(
        fun(Idx) ->
            {ok, _} = e2ee_social_repo:create(#{
                <<"uid">> => Uid,
                <<"key_version">> => KeyVersion,
                <<"shard_index">> => Idx,
                <<"total_shards">> => 3,
                <<"threshold">> => 2,
                <<"encrypted_shard">> => <<"encrypted_shard_data">>,
                <<"proxy_uid">> => ProxyUid,
                <<"shard_id">> => <<"shard_cr_", (integer_to_binary(Idx))/binary>>
            })
        end,
        [1, 2, 3]
    ),

    %% COUNT(*) 必须被 epgsql 解码为 Erlang integer，
    %% 否则 can_recover/2 的 is_integer(Count) 守卫会静默恒为 false
    ?assertEqual({ok, true}, e2ee_social_repo:can_recover(Uid, KeyVersion)),
    ?assertEqual({ok, false}, e2ee_social_repo:can_recover(Uid, <<"nonexistent_version">>)).

test_social_add_contact_upsert() ->
    #{uid := Uid, proxy_uid := ContactUid} = get_context(),

    %% 回归 42703：首次插入必须成功（此前 100% 失败）
    {ok, _} = e2ee_social_repo:add_contact(#{
        <<"uid">> => Uid,
        <<"contact_uid">> => ContactUid,
        <<"contact_nickname">> => <<"初始昵称"/utf8>>
    }),

    %% ON CONFLICT DO UPDATE 必须真正更新昵称，而非静默忽略
    {ok, _} = e2ee_social_repo:add_contact(#{
        <<"uid">> => Uid,
        <<"contact_uid">> => ContactUid,
        <<"contact_nickname">> => <<"更新后昵称"/utf8>>
    }),

    {ok, Contacts} = e2ee_social_repo:list_contacts(Uid),
    [Contact] = [C || C <- Contacts, maps:get(<<"contact_uid">>, C) =:= ContactUid],
    ?assertEqual(<<"更新后昵称"/utf8>>, maps:get(<<"contact_nickname">>, Contact)).

%% 回归：迁移 00000024 修复前，e2ee_social_shards 表 idx_e2ee_social_shards_
%% unique_active 唯一索引只按 (uid, key_version) 去重，与 create_shards/4
%% 的 k-of-n 多分片设计（threshold 强制 >= 2，必然写入 >= 2 条同 uid+
%% key_version 的分片行）结构性冲突——生产入口对任何真实调用
%% （分片数 >= 2）100% 触发 PG 23505 unique_violation，社交恢复功能
%% 完全无法使用。真库集成测试实测复现后已修复索引维度为
%% (uid, key_version, shard_index)。
test_create_shards_multi_shard_production_entrypoint() ->
    #{uid := Uid} = get_context(),
    ProxyUids = [elib_tsid:generate() || _ <- lists:seq(1, 3)],
    lists:foreach(
        fun(ProxyUid) ->
            {ok, _} = e2ee_social_repo:add_contact(#{
                <<"uid">> => Uid,
                <<"contact_uid">> => ProxyUid,
                <<"contact_nickname">> => <<"proxy"/utf8>>
            })
        end,
        ProxyUids
    ),
    Shards = [
        #{<<"proxy_uid">> => P, <<"encrypted_shard">> => <<"encrypted_placeholder">>}
     || P <- ProxyUids
    ],

    Result = e2ee_social_logic:create_shards(Uid, <<"latest">>, 2, Shards),
    ?assertMatch({ok, _}, Result),
    {ok, PersistedShards} = Result,
    ?assertEqual(3, length(PersistedShards)),

    ?assertEqual({ok, true}, e2ee_social_repo:can_recover(Uid, <<"latest">>)),

    lists:foreach(
        fun(ProxyUid) ->
            {ok, _} = elib_pg:execute(
                <<"DELETE FROM e2ee_trusted_contacts WHERE uid = $1 AND contact_uid = $2">>,
                [Uid, ProxyUid]
            )
        end,
        ProxyUids
    ).
