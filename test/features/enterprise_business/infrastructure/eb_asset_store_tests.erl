%%% @doc EB-03R P13 / A11 套件：object-store 三件能力的**正向**验证。
%%%
%%% 三项子判定（A11-1/2/3）：
%%%   1. **装配**：`eb_infra_ports:resolve(asset)` 返回实现模块（不再是
%%%      `{error, {not_implemented_yet, asset}}`）；
%%%   2. **作用域**：三者的键都带 Org/Workspace 隔离前缀，跨 Org 读/删必须失败；
%%%   3. **错误传播**：底层失败映射为契约错误元组，绝不静默 `{ok, _}`。
%%%
%%% 边界声明（硬约束 7）：对象存储侧用**本地替身**（进程内 `persistent_term` 桶）
%%% 证明契约语义。**不得**据此宣称「真实 Garage 验收通过」；本卡结论口径固定为
%%% `adapter_contract_verified_locally; real_garage_acceptance=NOT_RUN`。
-module(eb_asset_store_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

asset_store_test_() ->
    {setup, fun setup/0, fun cleanup_db/1, fun cases/1}.

setup() ->
    eunit_runner:eunit_setup_with_db().

cleanup_db({ok, Conn}) ->
    _ = eb_asset_object_stub:reset(),
    eunit_runner:eunit_cleanup_db(Conn);
cleanup_db(_Other) ->
    ok.

cases({ok, _Conn}) ->
    [
        {timeout, 60, fun a11a_asset_port_is_assembled/0},
        {timeout, 60, fun a11b_put_then_stream_round_trips_within_scope/0},
        {timeout, 60, fun a11b_cross_org_stream_and_delete_fail/0},
        {timeout, 60, fun a11c_bottom_failures_are_propagated/0},
        {timeout, 60, fun delete_private_removes_the_object_only/0},
        {timeout, 60, fun metadata_lifecycle_is_reachable_through_the_port/0}
    ];
cases(_Skipped) ->
    {skip, "asset store suite requires the scratch database connection"}.

%% A11-1：装配
a11a_asset_port_is_assembled() ->
    ?assertNotEqual({error, {not_implemented_yet, asset}}, eb_infra_ports:resolve(asset)),
    ?assertEqual({ok, eb_asset_store}, eb_infra_ports:resolve(asset)),
    {ok, Impl} = eb_infra_ports:resolve(asset),
    Exports = [N || {N, _A} <- Impl:module_info(exports)],
    lists:foreach(
        fun({Name, _Arity}) -> ?assert(lists:member(Name, Exports)) end,
        maps:get(eb_asset_port, eb_ports:contracts())
    ).

%% A11-2（正）：同作用域的 put → stream 往返
a11b_put_then_stream_round_trips_within_scope() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        AssetId = eb_pg_test_fixture:id(),
        Bytes = <<"eb03r-object-bytes">>,
        {ok, Summary} = eb_asset_store:put_private(Org, Ws, #{
            id => AssetId,
            object_hash => sha256_hex(Bytes),
            mime => <<"text/plain">>,
            payload => Bytes
        }),
        ?assertEqual(AssetId, maps:get(asset_id, Summary)),
        ?assertEqual(pending_confirm, maps:get(status, Summary)),
        %% storage_ref 是不透明引用：不是 URL、不是可下载链接
        Ref = maps:get(storage_ref, Summary),
        ?assertEqual(nomatch, re:run(Ref, "://", [{capture, none}])),
        {ok, {content_stream, Streamed}} = eb_asset_store:stream_content(Org, Ws, AssetId),
        ?assertEqual(Bytes, Streamed),
        %% 作用域键带 Org/Workspace 前缀
        Key = eb_asset_store:scope_key(Org, Ws, AssetId),
        ?assertEqual(Key, Ref)
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% A11-2（负）：跨 Org 读/删必须失败
a11b_cross_org_stream_and_delete_fail() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        OtherOrg = maps:get(other_org_id, Scope),
        OtherWs = maps:get(other_workspace_id, Scope),
        AssetId = eb_pg_test_fixture:id(),
        {ok, _} = eb_asset_store:put_private(Org, Ws, #{
            id => AssetId,
            object_hash => sha256_hex(<<"scope">>),
            payload => <<"scoped-bytes">>
        }),
        ?assertEqual({error, not_found}, eb_asset_store:stream_content(OtherOrg, OtherWs, AssetId)),
        ?assertEqual({error, not_found}, eb_asset_store:delete_private(OtherOrg, OtherWs, AssetId)),
        %% 跨 Org 删除失败后对象与元数据都还在
        {ok, {content_stream, _}} = eb_asset_store:stream_content(Org, Ws, AssetId),
        ?assertMatch({ok, _}, eb_asset_store:fetch_asset(Org, Ws, AssetId)),
        %% 替身 adapter 自身也拒绝越作用域前缀（纵深防御）
        Prefix = eb_asset_object_stub:key_prefix(Org, Ws),
        ForeignKey = <<(eb_asset_object_stub:key_prefix(OtherOrg, OtherWs))/binary, "1">>,
        ?assertEqual({error, out_of_scope}, eb_asset_object_stub:get(ForeignKey, Prefix))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% A11-3：底层失败必须显式传播
a11c_bottom_failures_are_propagated() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        %% (a) payload 非法（非 binary）⇒ 底层 {error, invalid_payload} 冒成契约错误
        ?assertEqual(
            {error, {object_store, invalid_payload}},
            eb_asset_store:put_private(Org, Ws, #{
                id => eb_pg_test_fixture:id(),
                object_hash => sha256_hex(<<"x">>),
                payload => 12345
            })
        ),
        %% (b) payload 为空 ⇒ empty_payload
        ?assertEqual(
            {error, {object_store, empty_payload}},
            eb_asset_store:put_private(Org, Ws, #{
                id => eb_pg_test_fixture:id(),
                object_hash => sha256_hex(<<"x">>),
                payload => <<>>
            })
        ),
        %% (c) descriptor 缺 id ⇒ 契约级错误（不触库）
        ?assertEqual(
            {error, invalid_descriptor},
            eb_asset_store:put_private(Org, Ws, #{payload => <<"x">>})
        ),
        %% (d) 对象被绕过清除后 stream 必须报底层错误，不得返回空流
        AssetId = eb_pg_test_fixture:id(),
        {ok, Summary} = eb_asset_store:put_private(Org, Ws, #{
            id => AssetId,
            object_hash => sha256_hex(<<"gone">>),
            payload => <<"gone-bytes">>
        }),
        ok = eb_asset_object_stub:reset(),
        ?assertEqual(
            {error, {object_store, not_found}},
            eb_asset_store:stream_content(Org, Ws, AssetId)
        ),
        ?assertEqual(
            {error, {object_store, not_found}},
            eb_asset_store:delete_private(Org, Ws, AssetId)
        ),
        _ = Summary,
        ok
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

delete_private_removes_the_object_only() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        AssetId = eb_pg_test_fixture:id(),
        {ok, _} = eb_asset_store:put_private(Org, Ws, #{
            id => AssetId,
            object_hash => sha256_hex(<<"del">>),
            payload => <<"to-be-deleted">>
        }),
        ok = eb_asset_store:delete_private(Org, Ws, AssetId),
        %% 对象没了，但元数据仍在（对象回收与元数据状态是两件事）
        ?assertEqual(
            {error, {object_store, not_found}},
            eb_asset_store:stream_content(Org, Ws, AssetId)
        ),
        ?assertEqual(
            pending_confirm,
            maps:get(status, element(2, eb_asset_store:fetch_asset(Org, Ws, AssetId)))
        )
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

metadata_lifecycle_is_reachable_through_the_port() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        AssetId = eb_pg_test_fixture:id(),
        {ok, Pending} = eb_asset_store:insert_asset(Org, Ws, #{
            id => AssetId,
            object_hash => sha256_hex(<<"meta">>),
            mime => <<"application/octet-stream">>,
            size_bytes => 8
        }),
        ?assertEqual(pending_confirm, maps:get(status, Pending)),
        {ok, Active} = eb_asset_store:confirm_asset(Org, Ws, AssetId),
        ?assertEqual(active, maps:get(status, Active)),
        ok = eb_asset_store:cleanup_asset(Org, Ws, AssetId),
        {ok, Deleted} = eb_asset_store:fetch_asset(Org, Ws, AssetId),
        ?assertEqual(deleted, maps:get(status, Deleted))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% ===================================================================
%% 辅助
%% ===================================================================

tenant(Scope) ->
    {maps:get(org_id, Scope), maps:get(workspace_id, Scope)}.

sha256_hex(Bin) ->
    binary:encode_hex(crypto:hash(sha256, Bin), lowercase).
