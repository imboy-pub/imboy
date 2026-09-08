-module(mcp_client_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% MCP-01：mcp_client 凭证 repo 契约（真库）。

uid() ->
    erlang:unique_integer([positive]) +
        erlang:phash2(binary:encode_hex(crypto:strong_rand_bytes(8))).

same_owner_multiple_clients_test_() ->
    ?TEST_WITH_DB(fun() ->
        {ok, _} = application:ensure_all_started(crypto),
        Owner = uid(),
        {ok, C1} = mcp_client_repo:create_client(Owner, #{name => <<"c1">>}),
        {ok, C2} = mcp_client_repo:create_client(Owner, #{name => <<"c2">>}),
        %% A01：同 owner 两个独立 client（不同 client_id/client_key/secret）
        ?assertNotEqual(maps:get(<<"client_id">>, C1), maps:get(<<"client_id">>, C2)),
        ?assertNotEqual(maps:get(<<"client_key">>, C1), maps:get(<<"client_key">>, C2)),
        ?assertNotEqual(maps:get(<<"secret">>, C1), maps:get(<<"secret">>, C2)),
        {ok, Rows} = mcp_client_repo:list_by_owner(Owner),
        ?assertEqual(2, length(Rows))
    end).

secret_shown_once_digest_in_db_test_() ->
    ?TEST_WITH_DB(fun() ->
        {ok, _} = application:ensure_all_started(crypto),
        {ok, C} = mcp_client_repo:create_client(uid(), #{name => <<"sec">>}),
        Secret = maps:get(<<"secret">>, C),
        ?assert(byte_size(Secret) >= 32),
        %% 明文不入库：库中只有摘要（digest 查找命中）与前缀
        {ok, Row} = mcp_client_repo:find_by_digest(digest(Secret)),
        ?assertNotEqual(maps:get(<<"credential_digest">>, Row), Secret),
        ?assertEqual(Secret, maps:get(<<"secret">>, C)),
        Prefix = maps:get(<<"credential_prefix">>, Row),
        ?assertEqual(Prefix, binary:part(Secret, 0, 8)),
        %% 明文本身绝不出现在行内
        ?assertEqual(undefined, maps:get(<<"secret">>, Row, undefined))
    end).

find_by_digest_unknown_test_() ->
    ?TEST_WITH_DB(fun() ->
        {ok, _} = application:ensure_all_started(crypto),
        {error, notfound} = mcp_client_repo:find_by_digest(digest(<<"nope">>))
    end).

set_disabled_flag_test_() ->
    ?TEST_WITH_DB(fun() ->
        {ok, _} = application:ensure_all_started(crypto),
        {ok, C} = mcp_client_repo:create_client(uid(), #{name => <<"dis">>}),
        ClientId = maps:get(<<"client_id">>, C),
        {ok, _} = mcp_client_repo:set_disabled(ClientId, true),
        {ok, Row} = mcp_client_repo:find_by_client_key(maps:get(<<"client_key">>, C)),
        ?assertEqual(true, maps:get(<<"disabled">>, Row)),
        {ok, _} = mcp_client_repo:set_disabled(ClientId, false),
        {ok, Row2} = mcp_client_repo:find_by_client_key(maps:get(<<"client_key">>, C)),
        ?assertEqual(false, maps:get(<<"disabled">>, Row2))
    end).

%% 摘要确定性（同 token 同摘要；异 token 异摘要）
digest_test_() ->
    [
        ?_assertEqual(digest(<<"abc">>), digest(<<"abc">>)),
        ?_assertNotEqual(digest(<<"abc">>), digest(<<"abd">>))
    ].

%% 回归（EXT-01 实测教训）：digest_hex 必须保持导出——
%% mcp_governance_logic:authenticate_secret/1 的 HTTP 认证路径跨层调用它，
%% 曾因未导出在运行时 undef（mock 测试掩盖了跨模块契约）。
digest_hex_exported_test_() ->
    ?_assert(erlang:function_exported(mcp_client_repo, digest_hex, 1)).

%% 导出函数与本地摘要实现必须同构（防止导出版与存储版漂移）
digest_hex_consistent_test_() ->
    ?_assertEqual(digest(<<"abc">>), mcp_client_repo:digest_hex(<<"abc">>)).

digest(Bin) -> binary:encode_hex(crypto:hash(sha256, Bin), lowercase).
