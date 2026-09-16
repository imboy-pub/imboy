%%% @doc EB-03R P12 套件：asset metadata 生命周期（pending_confirm → active → deleted）。
%%%
%%% 判定：写入后可读回；状态跃迁是 CAS（重放不算成功）；跨 Org 一律 not_found；
%%% 元数据里**没有**任何可下载语义（object_key 由实现派生，不接收调用方传入）。
-module(eb_asset_meta_pg_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

asset_meta_test_() ->
    {setup, fun setup/0, fun cleanup_db/1, fun cases/1}.

setup() ->
    eunit_runner:eunit_setup_with_db().

cleanup_db({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup_db(_Other) ->
    ok.

cases({ok, _Conn}) ->
    [
        {timeout, 60, fun insert_then_fetch_round_trips_pending_metadata/0},
        {timeout, 60, fun confirm_is_a_cas_transition/0},
        {timeout, 60, fun cleanup_marks_deleted_and_is_idempotent/0},
        {timeout, 60, fun cross_org_access_is_not_found/0},
        {timeout, 60, fun object_key_is_derived_and_scoped/0},
        {timeout, 60, fun descriptor_without_hash_is_rejected/0}
    ];
cases(_Skipped) ->
    {skip, "asset metadata suite requires the scratch database connection"}.

insert_then_fetch_round_trips_pending_metadata() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        AssetId = eb_pg_test_fixture:id(),
        Descriptor = #{
            id => AssetId,
            object_hash => sha256_hex(<<"eb03r-asset-1">>),
            mime => <<"application/octet-stream">>,
            size_bytes => 128,
            key_version => 1
        },
        {ok, Row} = eb_pg_asset_meta:insert_asset(Org, Ws, Descriptor),
        ?assertEqual(AssetId, maps:get(id, Row)),
        ?assertEqual(pending_confirm, maps:get(status, Row)),
        ?assertEqual(128, maps:get(size_bytes, Row)),
        {ok, Fetched} = eb_pg_asset_meta:fetch_asset(Org, Ws, AssetId),
        ?assertEqual(pending_confirm, maps:get(status, Fetched)),
        ?assertEqual(maps:get(object_key, Row), maps:get(object_key, Fetched))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

confirm_is_a_cas_transition() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        AssetId = insert_asset(Scope),
        {ok, Active} = eb_pg_asset_meta:confirm_asset(Org, Ws, AssetId),
        ?assertEqual(active, maps:get(status, Active)),
        ?assertEqual(2, maps:get(version, Active)),
        %% 重复 confirm：状态机跃迁必须可审计 ⇒ 第二次不算成功
        ?assertEqual({error, conflict}, eb_pg_asset_meta:confirm_asset(Org, Ws, AssetId)),
        ?assertEqual(
            active,
            maps:get(status, element(2, eb_pg_asset_meta:fetch_asset(Org, Ws, AssetId)))
        )
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

cleanup_marks_deleted_and_is_idempotent() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        AssetId = insert_asset(Scope),
        ok = eb_pg_asset_meta:cleanup_asset(Org, Ws, AssetId),
        {ok, Deleted} = eb_pg_asset_meta:fetch_asset(Org, Ws, AssetId),
        ?assertEqual(deleted, maps:get(status, Deleted)),
        ?assertNotEqual(undefined, maps:get(deleted_at, Deleted)),
        %% 已 deleted：再回收是 no-op（不是「成功跃迁」）
        ?assertEqual({error, not_found}, eb_pg_asset_meta:cleanup_asset(Org, Ws, AssetId))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

cross_org_access_is_not_found() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        OtherOrg = maps:get(other_org_id, Scope),
        OtherWs = maps:get(other_workspace_id, Scope),
        AssetId = insert_asset(Scope),
        ?assertEqual({error, not_found}, eb_pg_asset_meta:fetch_asset(OtherOrg, OtherWs, AssetId)),
        ?assertEqual(
            {error, not_found}, eb_pg_asset_meta:confirm_asset(OtherOrg, OtherWs, AssetId)
        ),
        ?assertEqual(
            {error, not_found}, eb_pg_asset_meta:cleanup_asset(OtherOrg, OtherWs, AssetId)
        ),
        %% 原租户未被影响
        ?assertEqual(
            pending_confirm,
            maps:get(status, element(2, eb_pg_asset_meta:fetch_asset(Org, Ws, AssetId)))
        )
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

object_key_is_derived_and_scoped() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        AssetId = insert_asset(Scope),
        {ok, Row} = eb_pg_asset_meta:fetch_asset(Org, Ws, AssetId),
        Key = maps:get(object_key, Row),
        Expected = eb_pg_asset_meta:object_key(Org, Ws, AssetId),
        ?assertEqual(Expected, Key),
        %% 作用域前缀：Org/Workspace 都在键里（跨 Org 必然落到不同键）
        ?assertNotEqual(nomatch, binary:match(Key, integer_to_binary(Org))),
        ?assertNotEqual(nomatch, binary:match(Key, integer_to_binary(Ws))),
        %% 不得是 URL（EB-01 的 ck_enterprise_asset_object_key_no_url 同口径）
        ?assertEqual(nomatch, re:run(Key, "^[a-zA-Z][a-zA-Z0-9+.\\-]*://", [{capture, none}])),
        %% 调用方无法通过入参指定 object_key（白名单外字段被忽略）
        Ignored = insert_asset_with(
            Scope, #{object_key => <<"https://evil.example/x">>}
        ),
        {ok, Row2} = eb_pg_asset_meta:fetch_asset(Org, Ws, Ignored),
        ?assertEqual(eb_pg_asset_meta:object_key(Org, Ws, Ignored), maps:get(object_key, Row2))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

descriptor_without_hash_is_rejected() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        ?assertEqual(
            {error, invalid_asset_descriptor},
            eb_pg_asset_meta:insert_asset(Org, Ws, #{id => eb_pg_test_fixture:id()})
        ),
        ?assertMatch(
            {error, {invalid_tenant, _}},
            eb_pg_asset_meta:insert_asset(undefined, Ws, #{id => 1, object_hash => <<"h">>})
        )
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% ===================================================================
%% 辅助
%% ===================================================================

tenant(Scope) ->
    {maps:get(org_id, Scope), maps:get(workspace_id, Scope)}.

insert_asset(Scope) ->
    insert_asset_with(Scope, #{}).

insert_asset_with(Scope, Extra) ->
    {Org, Ws} = tenant(Scope),
    AssetId = eb_pg_test_fixture:id(),
    Base = #{
        id => AssetId,
        object_hash => sha256_hex(integer_to_binary(AssetId)),
        mime => <<"application/octet-stream">>,
        size_bytes => 64,
        key_version => 1
    },
    {ok, _Row} = eb_pg_asset_meta:insert_asset(Org, Ws, maps:merge(Base, Extra)),
    AssetId.

sha256_hex(Bin) ->
    binary:encode_hex(crypto:hash(sha256, Bin), lowercase).
