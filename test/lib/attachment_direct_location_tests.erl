-module(attachment_direct_location_tests).
-moduledoc "direct-v1（迁移 00000200）逻辑/物理键分离的 reader 兼容测试（step-4）。".
%%%
% location_row/snapshot 的物理键解析合同（协议 §6）：
%   * 历史行 storage_physical_key = NULL → 物理键 = 逻辑 path（行为不变）；
%   * direct 行 storage_physical_key 非 NULL → 读/删命中物理 winner 键；
%   * 残缺值（空串/非 binary）→ location_unverified 失败关闭；
%   * 部分 NULL 四元组 → location_unverified（沿用既有 fail-closed）；
%   * locator digest 不匹配 → location_changed（不静默换根）。
%%%

-include_lib("eunit/include/eunit.hrl").

location_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun legacy_row_uses_logical_key/0,
        fun direct_row_uses_physical_key/0,
        fun empty_physical_key_fails_closed/0,
        fun partial_null_identity_fails_closed/0,
        fun all_null_is_legacy_unknown/0,
        fun digest_mismatch_is_location_changed/0
    ]}.

setup() ->
    Saved = {
        application:get_env(imboy, attachment_storage_registry),
        application:get_env(imboy, garage)
    },
    Root = #{
        driver => <<"garage">>,
        namespace => <<"g">>,
        endpoint => <<"http://127.0.0.1:3900">>,
        container => <<"b">>
    },
    application:set_env(imboy, attachment_storage_registry, #{
        version => 1, roots => #{<<"g-root">> => Root#{root_id => <<"g-root">>}}
    }),
    application:set_env(imboy, garage, #{endpoint => <<"http://127.0.0.1:3900">>, bucket => <<"b">>}),
    Saved.

cleanup({R, G}) ->
    restore(attachment_storage_registry, R),
    restore(garage, G).
restore(K, {ok, V}) -> application:set_env(imboy, K, V);
restore(K, undefined) -> application:unset_env(imboy, K).

digest() ->
    attachment_storage:locator_digest(
        <<"g">>, #{
            driver => <<"garage">>,
            root_id => <<"g-root">>,
            endpoint => <<"http://127.0.0.1:3900">>,
            container => <<"b">>
        }
    ).

identity_row() ->
    #{
        <<"storage_driver">> => <<"garage">>,
        <<"storage_namespace">> => <<"g">>,
        <<"storage_container">> => <<"b">>,
        <<"storage_locator_sha256">> => digest(),
        <<"upload_content">> => #{<<"mime">> => <<"image/png">>}
    }.

legacy_row_uses_logical_key() ->
    Row = identity_row(),
    ?assertMatch(
        {ok, #{key := <<"u1/abc.png">>, driver := garage, container := <<"b">>}},
        attachment_storage:frozen_location(<<"u1/abc.png">>, Row)
    ).

direct_row_uses_physical_key() ->
    Row = (identity_row())#{<<"storage_physical_key">> => <<"direct-c/s1/1/xyz">>},
    ?assertMatch(
        {ok, #{key := <<"direct-c/s1/1/xyz">>}},
        attachment_storage:frozen_location(<<"u1/abc.png">>, Row)
    ).

empty_physical_key_fails_closed() ->
    Row = (identity_row())#{<<"storage_physical_key">> => <<>>},
    ?assertMatch(
        {error, location_unverified},
        attachment_storage:frozen_location(<<"u1/abc.png">>, Row)
    ).

partial_null_identity_fails_closed() ->
    Row = (identity_row())#{<<"storage_container">> => null},
    ?assertMatch(
        {error, location_unverified},
        attachment_storage:frozen_location(<<"u1/abc.png">>, Row)
    ).

all_null_is_legacy_unknown() ->
    ?assertMatch(
        {ok, legacy_unknown},
        attachment_storage:frozen_location(<<"u1/abc.png">>, #{})
    ).

digest_mismatch_is_location_changed() ->
    Row = (identity_row())#{<<"storage_locator_sha256">> => binary:encode_hex(<<0:256>>)},
    ?assertMatch(
        {error, location_changed},
        attachment_storage:frozen_location(<<"u1/abc.png">>, Row)
    ).
