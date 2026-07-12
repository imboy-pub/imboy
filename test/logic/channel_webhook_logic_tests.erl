-module(channel_webhook_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% ===================================================================
%% incoming/3
%% ===================================================================

%% ① token 无效 → 统一 not_found
incoming_invalid_token_test_() ->
    ?WITH_MECKS(
        [
            {agent_rate_limiter, [{'allow', 2, fun(_, 0) -> allow end}]},
            {channel_webhook_ds, [
                {'find_by_token', 1, fun(<<"badtoken">>) -> {error, not_found} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, not_found},
                channel_webhook_logic:incoming(<<"badtoken">>, <<"hello">>, <<"1.2.3.4">>)
            )
        end
    ).

%% ② 停用 webhook → 与无效 token 同样 not_found（不泄露存在性差异）
incoming_disabled_webhook_test_() ->
    ?WITH_MECKS(
        [
            {agent_rate_limiter, [{'allow', 2, fun(_, 0) -> allow end}]},
            {channel_webhook_ds, [
                {'find_by_token', 1, fun(<<"disabledtok">>) ->
                    {ok, #{
                        <<"status">> => 2,
                        <<"channel_id">> => 77,
                        <<"bot_uid">> => 555
                    }}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, not_found},
                channel_webhook_logic:incoming(<<"disabledtok">>, <<"hello">>, <<"1.2.3.4">>)
            )
        end
    ).

%% ③ 限流拒绝 → rate_limited（在查库之前，不触达 DS）
incoming_rate_limited_test_() ->
    ?WITH_MECKS(
        [
            {agent_rate_limiter, [
                {'allow', 2, fun
                    (<<"webhook_ip:", _/binary>>, 0) -> allow;
                    (_, 0) -> {deny, requester_rate}
                end}
            ]},
            {channel_webhook_ds, [
                {'find_by_token', 1, fun(_) -> erlang:error(should_not_hit_ds) end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, rate_limited},
                channel_webhook_logic:incoming(<<"sometoken">>, <<"hello">>, <<"1.2.3.4">>)
            )
        end
    ).

%% ④ 有效 token → publish 被调且作者=bot_uid、频道=绑定频道、payload 带 is_bot
incoming_valid_token_publishes_as_bot_test_() ->
    ?WITH_MECKS(
        [
            {agent_rate_limiter, [{'allow', 2, fun(_, 0) -> allow end}]},
            {channel_webhook_ds, [
                {'find_by_token', 1, fun(<<"goodtoken">>) ->
                    {ok, #{
                        <<"status">> => 1,
                        <<"channel_id">> => 77,
                        <<"bot_uid">> => 555
                    }}
                end}
            ]},
            {channel_logic_message, [
                %% fun 头部即断言：作者必须是 bot_uid=555，频道必须是绑定的 77，
                %% payload 必须带 is_bot=true；参数不符 meck 抛错 → 测试失败
                {'publish_message', 5, fun(
                    555, <<"77">>, <<"hello">>, <<"text">>, #{<<"is_bot">> := true}
                ) ->
                    {ok, #{<<"id">> => 999}}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                ok,
                channel_webhook_logic:incoming(<<"goodtoken">>, <<"hello">>, <<"1.2.3.4">>)
            ),
            ?assert(meck:called(channel_logic_message, publish_message, '_'))
        end
    ).

%% ===================================================================
%% create/3 权限
%% ===================================================================

%% ⑤ 非管理员（role=1 编辑）不能创建 webhook
create_rejects_non_admin_test_() ->
    ?WITH_MECKS(
        [
            {channel_logic_common, [
                {'resolve_channel_id', 1, fun(<<"77">>) -> 77 end},
                {'get_user_role', 2, fun(77, 1001) -> 1 end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"只有频道管理员可以管理 webhook"/utf8>>},
                channel_webhook_logic:create(1001, <<"77">>, <<"CI Bot">>)
            )
        end
    ).

%% ===================================================================
%% list/2 token 掩码
%% ===================================================================

%% ⑥ list 不泄露完整 token（只回前 8 位 + ***）
list_masks_token_test_() ->
    FullToken = <<"abcdefgh0123456789abcdef0123456789abcdef01234567">>,
    ?WITH_MECKS(
        [
            {channel_logic_common, [
                {'resolve_channel_id', 1, fun(<<"77">>) -> 77 end},
                {'get_user_role', 2, fun(77, 1001) -> 2 end}
            ]},
            {channel_webhook_ds, [
                {'list_by_channel', 1, fun(77) ->
                    {ok, [
                        #{
                            <<"id">> => 1,
                            <<"token">> => FullToken,
                            <<"status">> => 1
                        }
                    ]}
                end}
            ]}
        ],
        fun() ->
            {ok, [Row]} = channel_webhook_logic:list(1001, <<"77">>),
            ?assertEqual(<<"abcdefgh***">>, maps:get(<<"token">>, Row))
        end
    ).

%% ⑦ IP 维度限流是第一道闸门：换 token 枚举/空 body 均被 IP 限流覆盖，
%% 不触达 token 限流与 DS（security-review H1/M1）
incoming_ip_rate_limited_test_() ->
    ?WITH_MECKS(
        [
            {agent_rate_limiter, [
                {'allow', 2, fun
                    (<<"webhook_ip:", _/binary>>, 0) -> {deny, requester_rate};
                    (_, 0) -> erlang:error(should_not_hit_token_limiter)
                end}
            ]},
            {channel_webhook_ds, [
                {'find_by_token', 1, fun(_) -> erlang:error(should_not_hit_ds) end}
            ]}
        ],
        fun() ->
            %% 正常 body 与空 body 都先过 IP 闸门
            ?assertEqual(
                {error, rate_limited},
                channel_webhook_logic:incoming(<<"any">>, <<"hello">>, <<"6.6.6.6">>)
            ),
            ?assertEqual(
                {error, rate_limited},
                channel_webhook_logic:incoming(<<"any">>, <<>>, <<"6.6.6.6">>)
            )
        end
    ).
