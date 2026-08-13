-module(channel_logic_message_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% @doc BUG#125 回归：editor(role=1) 退订后 is_subscribed 必须为 false，
%% 不能因 admin 表历史角色记录而恒返回 true。
get_channel_editor_unsubscribed_returns_false_test_() ->
    ?WITH_MECKS(
        [
            {channel_ds, [
                {'find_by_id_with_price', 1, fun(_ChannelId) ->
                    #{<<"id">> => 42, <<"name">> => <<"干饭"/utf8>>}
                end}
            ]},
            {channel_admin_ds, [
                {'get_role', 2, fun(_ChannelId, _Uid) -> 1 end}
            ]},
            {channel_subscription_ds, [
                {'is_subscribed', 2, fun(_ChannelId, _Uid) -> false end}
            ]}
        ],
        fun() ->
            Result = channel_logic_message:get_channel(<<"42">>, 1001),
            ?assertMatch({ok, _}, Result),
            {ok, Channel} = Result,
            ?assertEqual(false, maps:get(is_subscribed, Channel)),
            ?assertEqual(1, maps:get(user_role, Channel))
        end
    ).

%% @doc 创建者(role=3)恒为已订阅，即使 subscription 表无记录。
get_channel_creator_always_subscribed_test_() ->
    ?WITH_MECKS(
        [
            {channel_ds, [
                {'find_by_id_with_price', 1, fun(_ChannelId) ->
                    #{<<"id">> => 42, <<"name">> => <<"干饭"/utf8>>}
                end}
            ]},
            {channel_admin_ds, [
                {'get_role', 2, fun(_ChannelId, _Uid) -> 3 end}
            ]},
            {channel_subscription_ds, [
                {'is_subscribed', 2, fun(_ChannelId, _Uid) -> false end}
            ]}
        ],
        fun() ->
            Result = channel_logic_message:get_channel(<<"42">>, 1001),
            ?assertMatch({ok, _}, Result),
            {ok, Channel} = Result,
            ?assertEqual(true, maps:get(is_subscribed, Channel)),
            ?assertEqual(3, maps:get(user_role, Channel))
        end
    ).

%% @doc 普通用户(role=0)走真实订阅查询。
get_channel_role_zero_queries_subscription_test_() ->
    ?WITH_MECKS(
        [
            {channel_ds, [
                {'find_by_id_with_price', 1, fun(_ChannelId) ->
                    #{<<"id">> => 42, <<"name">> => <<"干饭"/utf8>>}
                end},
                {'find_by_id', 2, fun(_ChannelId, _Columns) ->
                    #{<<"id">> => 42}
                end}
            ]},
            {channel_admin_ds, [
                {'get_role', 2, fun(_ChannelId, _Uid) -> 0 end}
            ]},
            {channel_subscription_ds, [
                {'is_subscribed', 2, fun(_ChannelId, _Uid) -> true end}
            ]}
        ],
        fun() ->
            Result = channel_logic_message:get_channel(<<"42">>, 1001),
            ?assertMatch({ok, _}, Result),
            {ok, Channel} = Result,
            ?assertEqual(true, maps:get(is_subscribed, Channel)),
            ?assertEqual(0, maps:get(user_role, Channel))
        end
    ).

%% @doc 付费频道详情必须返回购买状态；历史订阅不能被当成内容权益。
get_paid_channel_returns_purchase_state_test_() ->
    ?WITH_MECKS(
        [
            {channel_ds, [
                {'find_by_id_with_price', 1, fun(_ChannelId) ->
                    #{
                        <<"id">> => 42,
                        <<"type">> => 2,
                        <<"price">> => 990
                    }
                end},
                {'find_by_id', 2, fun(_ChannelId, _Columns) ->
                    #{<<"id">> => 42}
                end}
            ]},
            {channel_admin_ds, [
                {'get_role', 2, fun(_ChannelId, _Uid) -> 0 end}
            ]},
            {channel_subscription_ds, [
                {'is_subscribed', 2, fun(_ChannelId, _Uid) -> true end}
            ]},
            {channel_order_ds, [
                {'has_purchased', 2, fun(_ChannelId, _Uid) -> false end}
            ]}
        ],
        fun() ->
            {ok, Channel} = channel_logic_message:get_channel(<<"42">>, 1001),
            ?assertEqual(true, maps:get(is_subscribed, Channel)),
            ?assertEqual(false, maps:get(has_purchased, Channel))
        end
    ).

get_paid_channel_returns_true_for_purchased_user_test_() ->
    ?WITH_MECKS(
        [
            {channel_ds, [
                {'find_by_id_with_price', 1, fun(_ChannelId) ->
                    #{<<"id">> => 42, <<"type">> => 2, <<"price">> => 990}
                end},
                {'find_by_id', 2, fun(_ChannelId, _Columns) -> #{<<"id">> => 42} end}
            ]},
            {channel_admin_ds, [
                {'get_role', 2, fun(_ChannelId, _Uid) -> 0 end}
            ]},
            {channel_subscription_ds, [
                {'is_subscribed', 2, fun(_ChannelId, _Uid) -> false end}
            ]},
            {channel_order_ds, [
                {'has_purchased', 2, fun(_ChannelId, _Uid) -> true end}
            ]}
        ],
        fun() ->
            {ok, Channel} = channel_logic_message:get_channel(<<"42">>, 1001),
            ?assertEqual(true, maps:get(has_purchased, Channel))
        end
    ).

get_paid_custom_channel_includes_price_and_purchase_state_test_() ->
    ?WITH_MECKS(
        [
            {channel_ds, [
                {'find_by_custom_id', 1, fun(<<"paid_daily">>) ->
                    #{<<"id">> => 42, <<"type">> => 2}
                end},
                {'find_by_id_with_price', 1, fun(42) ->
                    #{<<"id">> => 42, <<"type">> => 2, <<"price">> => 990}
                end},
                {'find_by_id', 2, fun(42, _Columns) -> #{<<"id">> => 42} end}
            ]},
            {channel_admin_ds, [
                {'get_role', 2, fun(42, 1001) -> 0 end}
            ]},
            {channel_subscription_ds, [
                {'is_subscribed', 2, fun(42, 1001) -> true end}
            ]},
            {channel_order_ds, [
                {'has_purchased', 2, fun(42, 1001) -> false end}
            ]}
        ],
        fun() ->
            {ok, Channel} = channel_logic_message:get_channel_by_custom_id(
                <<"paid_daily">>,
                1001
            ),
            ?assertEqual(990, maps:get(<<"price">>, Channel)),
            ?assertEqual(false, maps:get(has_purchased, Channel))
        end
    ).

%% ===================================================================
%% update_channel/3 + custom_id（BUG#134）测试
%% ===================================================================

%% @doc BUG#134：未设过 custom_id 的频道，更新时传入 custom_id 应落库。
update_channel_custom_id_first_set_persists_test_() ->
    ?WITH_MECKS(
        [
            {channel_admin_ds, [
                {'get_role', 2, fun(_ChannelId, _Uid) -> 2 end}
            ]},
            {channel_ds, [
                {'find_by_id', 2, fun
                    (_ChannelId, <<"custom_id">>) ->
                        #{<<"id">> => 42, <<"custom_id">> => <<>>};
                    (_ChannelId, <<"*">>) ->
                        #{
                            <<"id">> => 42,
                            <<"name">> => <<"干饭"/utf8>>,
                            <<"custom_id">> => <<"ganfan">>
                        }
                end},
                % 真实 repo 无行时返回空 map #{}（elib_pg:one 的 Default=#{}），
                % 并非 {error, not_found}——mock 必须匹配真实形状，否则掩盖回归
                {'find_by_custom_id', 1, fun(_CustomId) -> #{} end},
                {'update', 2, fun(_ChannelId, Data) ->
                    put(captured_update, Data),
                    {ok, 1}
                end},
                {'subscriber_uids', 1, fun(_ChannelId) -> [] end}
            ]}
        ],
        fun() ->
            Result = channel_logic_message:update_channel(1001, <<"42">>, #{
                <<"name">> => <<"新名字"/utf8>>,
                <<"custom_id">> => <<"ganfan">>
            }),
            ?assertMatch({ok, _}, Result),
            Captured = get(captured_update),
            ?assertEqual(<<"ganfan">>, maps:get(<<"custom_id">>, Captured)),
            ?assertEqual(<<"新名字"/utf8>>, maps:get(<<"name">>, Captured))
        end
    ).

%% @doc BUG#134：已设过 custom_id（锁定）时再传不同值，必须忽略且整体更新仍成功。
update_channel_custom_id_locked_ignored_test_() ->
    ?WITH_MECKS(
        [
            {channel_admin_ds, [
                {'get_role', 2, fun(_ChannelId, _Uid) -> 2 end}
            ]},
            {channel_ds, [
                {'find_by_id', 2, fun
                    (_ChannelId, <<"custom_id">>) ->
                        #{<<"id">> => 42, <<"custom_id">> => <<"old_id">>};
                    (_ChannelId, <<"*">>) ->
                        #{
                            <<"id">> => 42,
                            <<"name">> => <<"干饭"/utf8>>,
                            <<"custom_id">> => <<"old_id">>
                        }
                end},
                {'find_by_custom_id', 1, fun(_CustomId) ->
                    erlang:error(should_not_check_uniqueness_when_locked)
                end},
                {'update', 2, fun(_ChannelId, Data) ->
                    put(captured_update, Data),
                    {ok, 1}
                end},
                {'subscriber_uids', 1, fun(_ChannelId) -> [] end}
            ]}
        ],
        fun() ->
            Result = channel_logic_message:update_channel(1001, <<"42">>, #{
                <<"name">> => <<"新名字"/utf8>>,
                <<"custom_id">> => <<"new_id">>
            }),
            ?assertMatch({ok, _}, Result),
            Captured = get(captured_update),
            ?assertEqual(false, maps:is_key(<<"custom_id">>, Captured)),
            ?assertEqual(<<"新名字"/utf8>>, maps:get(<<"name">>, Captured))
        end
    ).

%% @doc BUG#134：未设过但传入的 custom_id 已被其他频道占用，返回明确错误且不落库。
update_channel_custom_id_duplicate_returns_error_test_() ->
    ?WITH_MECKS(
        [
            {channel_admin_ds, [
                {'get_role', 2, fun(_ChannelId, _Uid) -> 2 end}
            ]},
            {channel_ds, [
                {'find_by_id', 2, fun(_ChannelId, <<"custom_id">>) ->
                    #{<<"id">> => 42, <<"custom_id">> => <<>>}
                end},
                {'find_by_custom_id', 1, fun(_CustomId) ->
                    #{<<"id">> => 99, <<"custom_id">> => <<"taken">>}
                end},
                {'update', 2, fun(_, _) ->
                    erlang:error(should_not_update_when_custom_id_duplicate)
                end}
            ]}
        ],
        fun() ->
            Result = channel_logic_message:update_channel(1001, <<"42">>, #{
                <<"custom_id">> => <<"taken">>
            }),
            ?assertEqual({error, <<"自定义ID已被使用"/utf8>>}, Result)
        end
    ).

%% @doc BUG#134：显式传入非 binary 或空 custom_id 视为格式非法，返回明确错误，
%% 不静默过滤（静默过滤正是本 bug 的失败模式）。
update_channel_custom_id_invalid_format_returns_error_test_() ->
    ?WITH_MECKS(
        [
            {channel_admin_ds, [
                {'get_role', 2, fun(_ChannelId, _Uid) -> 2 end}
            ]},
            {channel_ds, [
                {'update', 2, fun(_, _) ->
                    erlang:error(should_not_update_when_custom_id_invalid)
                end}
            ]}
        ],
        fun() ->
            Result1 = channel_logic_message:update_channel(1001, <<"42">>, #{
                <<"custom_id">> => 12345
            }),
            ?assertEqual({error, <<"自定义ID格式无效"/utf8>>}, Result1),
            Result2 = channel_logic_message:update_channel(1001, <<"42">>, #{
                <<"custom_id">> => <<>>
            }),
            ?assertEqual({error, <<"自定义ID格式无效"/utf8>>}, Result2)
        end
    ).

%% @doc 无权限（role<2）时 update_channel 行为不变。
update_channel_returns_permission_denied_for_role_below_two_test_() ->
    ?WITH_MECKS(
        [
            {channel_admin_ds, [
                {'get_role', 2, fun(_ChannelId, _Uid) -> 1 end}
            ]},
            {channel_ds, [
                {'update', 2, fun(_, _) ->
                    erlang:error(should_not_update_when_no_permission)
                end}
            ]}
        ],
        fun() ->
            Result = channel_logic_message:update_channel(1001, <<"42">>, #{
                <<"name">> => <<"新名字"/utf8>>,
                <<"custom_id">> => <<"ganfan">>
            }),
            ?assertEqual({error, <<"无权限操作"/utf8>>}, Result)
        end
    ).

%% @doc BUG#125 回归（custom_id 路径）：editor 退订后 is_subscribed 必须为 false。
get_channel_by_custom_id_editor_unsubscribed_returns_false_test_() ->
    ?WITH_MECKS(
        [
            {channel_ds, [
                {'find_by_custom_id', 1, fun(_CustomId) ->
                    #{<<"id">> => 42, <<"name">> => <<"干饭"/utf8>>}
                end}
            ]},
            {channel_admin_ds, [
                {'get_role', 2, fun(_ChannelId, _Uid) -> 1 end}
            ]},
            {channel_subscription_ds, [
                {'is_subscribed', 2, fun(_ChannelId, _Uid) -> false end}
            ]}
        ],
        fun() ->
            Result = channel_logic_message:get_channel_by_custom_id(<<"ganfan">>, 1001),
            ?assertMatch({ok, _}, Result),
            {ok, Channel} = Result,
            ?assertEqual(false, maps:get(is_subscribed, Channel)),
            ?assertEqual(1, maps:get(user_role, Channel))
        end
    ).
