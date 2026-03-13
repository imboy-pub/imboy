-module(channel_subscribe_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% channel_subscribe_ds 模块 EUnit
%%%
%%% 目标：
%%% - 私有/付费订阅激活统一走 channel_ds:subscribe/2
%%% - 支付竞态下可补偿激活订阅
%%%===================================================================

subscribe_paid_activates_subscription_via_channel_ds_test_() ->
    ?WITH_MECKS([
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD001">>) ->
                {ok, #{
                    <<"channel_id">> => 11,
                    <<"user_id">> => 2002,
                    <<"status">> => 1
                }}
            end}
        ]},
        {channel_ds, [
            {'subscribe', 2, fun(11, 2002) -> ok end}
        ]}
    ], fun() ->
        ?assertEqual(ok, channel_subscribe_ds:subscribe_paid(11, 2002, <<"ORD001">>)),
        ?assertEqual(1, meck:num_calls(channel_ds, subscribe, 2))
    end).

subscribe_paid_rejects_unpaid_order_test_() ->
    ?WITH_MECKS([
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD002">>) ->
                {ok, #{
                    <<"channel_id">> => 11,
                    <<"user_id">> => 2002,
                    <<"status">> => 0
                }}
            end}
        ]},
        {channel_ds, [
            {'subscribe', 2, fun(_, _) ->
                erlang:error(should_not_subscribe_for_unpaid_order)
            end}
        ]}
    ], fun() ->
        ?assertEqual(
            {error, <<"订单未支付"/utf8>>},
            channel_subscribe_ds:subscribe_paid(11, 2002, <<"ORD002">>)
        ),
        ?assertEqual(0, meck:num_calls(channel_ds, subscribe, 2))
    end).

subscribe_paid_returns_error_when_order_missing_test_() ->
    ?WITH_MECKS([
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD404">>) ->
                {error, not_found}
            end}
        ]},
        {channel_ds, [
            {'subscribe', 2, fun(_, _) ->
                erlang:error(should_not_subscribe_when_order_missing)
            end}
        ]}
    ], fun() ->
        ?assertEqual(
            {error, <<"订单不存在"/utf8>>},
            channel_subscribe_ds:subscribe_paid(11, 2002, <<"ORD404">>)
        ),
        ?assertEqual(0, meck:num_calls(channel_ds, subscribe, 2))
    end).

subscribe_paid_propagates_order_lookup_error_as_binary_test_() ->
    ?WITH_MECKS([
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD_DB_ERR">>) ->
                {error, db_down}
            end}
        ]},
        {channel_ds, [
            {'subscribe', 2, fun(_, _) ->
                erlang:error(should_not_subscribe_when_order_lookup_failed)
            end}
        ]}
    ], fun() ->
        ?assertEqual(
            {error, <<"db_down">>},
            channel_subscribe_ds:subscribe_paid(11, 2002, <<"ORD_DB_ERR">>)
        ),
        ?assertEqual(0, meck:num_calls(channel_ds, subscribe, 2))
    end).

subscribe_paid_rejects_mismatched_channel_test_() ->
    ?WITH_MECKS([
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD_CH_MISMATCH">>) ->
                {ok, #{
                    <<"channel_id">> => 12,
                    <<"user_id">> => 2002,
                    <<"status">> => 1
                }}
            end}
        ]},
        {channel_ds, [
            {'subscribe', 2, fun(_, _) ->
                erlang:error(should_not_subscribe_when_channel_mismatch)
            end}
        ]}
    ], fun() ->
        ?assertEqual(
            {error, <<"订单频道不匹配"/utf8>>},
            channel_subscribe_ds:subscribe_paid(11, 2002, <<"ORD_CH_MISMATCH">>)
        ),
        ?assertEqual(0, meck:num_calls(channel_ds, subscribe, 2))
    end).

subscribe_paid_rejects_mismatched_user_test_() ->
    ?WITH_MECKS([
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD_UID_MISMATCH">>) ->
                {ok, #{
                    <<"channel_id">> => 11,
                    <<"user_id">> => 3003,
                    <<"status">> => 1
                }}
            end}
        ]},
        {channel_ds, [
            {'subscribe', 2, fun(_, _) ->
                erlang:error(should_not_subscribe_when_user_mismatch)
            end}
        ]}
    ], fun() ->
        ?assertEqual(
            {error, <<"订单用户不匹配"/utf8>>},
            channel_subscribe_ds:subscribe_paid(11, 2002, <<"ORD_UID_MISMATCH">>)
        ),
        ?assertEqual(0, meck:num_calls(channel_ds, subscribe, 2))
    end).

subscribe_private_returns_ok_when_already_subscribed_test_() ->
    ?WITH_MECKS([
        {channel_ds, [
            {'is_subscribed', 2, fun(11, 2002) -> true end}
        ]},
        {channel_invitation_repo, [
            {'is_invited', 2, fun(_, _) ->
                erlang:error(should_not_check_invitation_when_already_subscribed)
            end}
        ]}
    ], fun() ->
        ?assertEqual(ok, channel_subscribe_ds:subscribe_private(11, 2002, undefined)),
        ?assertEqual(1, meck:num_calls(channel_ds, is_subscribed, 2)),
        ?assertEqual(0, meck:num_calls(channel_invitation_repo, is_invited, 2))
    end).

check_subscription_permission_private_returns_can_subscribe_when_already_subscribed_test_() ->
    ?WITH_MECKS([
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"id,type">>) ->
                #{<<"id">> => 11, <<"type">> => 1}
            end}
        ]},
        {channel_ds, [
            {'is_subscribed', 2, fun(11, 2002) -> true end}
        ]},
        {channel_invitation_repo, [
            {'is_invited', 2, fun(_, _) ->
                erlang:error(should_not_check_invitation_when_already_subscribed)
            end}
        ]}
    ], fun() ->
        ?assertEqual(
            {ok, can_subscribe},
            channel_subscribe_ds:check_subscription_permission(11, 2002)
        ),
        ?assertEqual(1, meck:num_calls(channel_ds, is_subscribed, 2)),
        ?assertEqual(0, meck:num_calls(channel_invitation_repo, is_invited, 2))
    end).

check_subscription_permission_paid_returns_can_subscribe_when_already_subscribed_test_() ->
    ?WITH_MECKS([
        {channel_repo, [
            {'find_by_id', 2, fun(12, <<"id,type">>) ->
                #{<<"id">> => 12, <<"type">> => 2}
            end}
        ]},
        {channel_ds, [
            {'is_subscribed', 2, fun(12, 2002) -> true end}
        ]},
        {channel_order_repo, [
            {'has_purchased', 2, fun(_, _) ->
                erlang:error(should_not_check_purchase_when_already_subscribed)
            end}
        ]}
    ], fun() ->
        ?assertEqual(
            {ok, can_subscribe},
            channel_subscribe_ds:check_subscription_permission(12, 2002)
        ),
        ?assertEqual(1, meck:num_calls(channel_ds, is_subscribed, 2)),
        ?assertEqual(0, meck:num_calls(channel_order_repo, has_purchased, 2))
    end).

check_subscription_permission_private_returns_need_invitation_when_not_subscribed_and_not_invited_test_() ->
    ?WITH_MECKS([
        {channel_repo, [
            {'find_by_id', 2, fun(13, <<"id,type">>) ->
                #{<<"id">> => 13, <<"type">> => 1}
            end}
        ]},
        {channel_ds, [
            {'is_subscribed', 2, fun(13, 2002) -> false end}
        ]},
        {channel_invitation_repo, [
            {'is_invited', 2, fun(13, 2002) -> false end}
        ]}
    ], fun() ->
        ?assertEqual(
            {ok, need_invitation},
            channel_subscribe_ds:check_subscription_permission(13, 2002)
        ),
        ?assertEqual(1, meck:num_calls(channel_ds, is_subscribed, 2)),
        ?assertEqual(1, meck:num_calls(channel_invitation_repo, is_invited, 2))
    end).

check_subscription_permission_returns_error_when_channel_missing_test_() ->
    ?WITH_MECKS([
        {channel_repo, [
            {'find_by_id', 2, fun(99, <<"id,type">>) ->
                {error, not_found}
            end}
        ]},
        {channel_ds, [
            {'is_subscribed', 2, fun(_, _) ->
                erlang:error(should_not_check_subscription_when_channel_missing)
            end}
        ]}
    ], fun() ->
        ?assertEqual(
            {error, <<"频道不存在"/utf8>>},
            channel_subscribe_ds:check_subscription_permission(99, 2002)
        ),
        ?assertEqual(0, meck:num_calls(channel_ds, is_subscribed, 2))
    end).

check_subscription_permission_propagates_lookup_error_as_binary_test_() ->
    ?WITH_MECKS([
        {channel_repo, [
            {'find_by_id', 2, fun(98, <<"id,type">>) ->
                {error, db_down}
            end}
        ]},
        {channel_ds, [
            {'is_subscribed', 2, fun(_, _) ->
                erlang:error(should_not_check_subscription_when_lookup_failed)
            end}
        ]}
    ], fun() ->
        ?assertEqual(
            {error, <<"db_down">>},
            channel_subscribe_ds:check_subscription_permission(98, 2002)
        ),
        ?assertEqual(0, meck:num_calls(channel_ds, is_subscribed, 2))
    end).

check_subscription_permission_private_returns_can_subscribe_when_invited_test_() ->
    ?WITH_MECKS([
        {channel_repo, [
            {'find_by_id', 2, fun(14, <<"id,type">>) ->
                #{<<"id">> => 14, <<"type">> => 1}
            end}
        ]},
        {channel_ds, [
            {'is_subscribed', 2, fun(14, 2002) -> false end}
        ]},
        {channel_invitation_repo, [
            {'is_invited', 2, fun(14, 2002) -> true end}
        ]}
    ], fun() ->
        ?assertEqual(
            {ok, can_subscribe},
            channel_subscribe_ds:check_subscription_permission(14, 2002)
        ),
        ?assertEqual(1, meck:num_calls(channel_ds, is_subscribed, 2)),
        ?assertEqual(1, meck:num_calls(channel_invitation_repo, is_invited, 2))
    end).

check_subscription_permission_paid_returns_need_purchase_when_not_purchased_test_() ->
    ?WITH_MECKS([
        {channel_repo, [
            {'find_by_id', 2, fun(15, <<"id,type">>) ->
                #{<<"id">> => 15, <<"type">> => 2}
            end}
        ]},
        {channel_ds, [
            {'is_subscribed', 2, fun(15, 2002) -> false end}
        ]},
        {channel_order_repo, [
            {'has_purchased', 2, fun(15, 2002) -> false end}
        ]}
    ], fun() ->
        ?assertEqual(
            {ok, need_purchase},
            channel_subscribe_ds:check_subscription_permission(15, 2002)
        ),
        ?assertEqual(1, meck:num_calls(channel_order_repo, has_purchased, 2))
    end).

check_subscription_permission_paid_returns_can_subscribe_when_purchased_test_() ->
    ?WITH_MECKS([
        {channel_repo, [
            {'find_by_id', 2, fun(16, <<"id,type">>) ->
                #{<<"id">> => 16, <<"type">> => 2}
            end}
        ]},
        {channel_ds, [
            {'is_subscribed', 2, fun(16, 2002) -> false end}
        ]},
        {channel_order_repo, [
            {'has_purchased', 2, fun(16, 2002) -> true end}
        ]}
    ], fun() ->
        ?assertEqual(
            {ok, can_subscribe},
            channel_subscribe_ds:check_subscription_permission(16, 2002)
        ),
        ?assertEqual(1, meck:num_calls(channel_order_repo, has_purchased, 2))
    end).

check_subscription_permission_public_returns_can_subscribe_test_() ->
    ?WITH_MECKS([
        {channel_repo, [
            {'find_by_id', 2, fun(17, <<"id,type">>) ->
                #{<<"id">> => 17, <<"type">> => 0}
            end}
        ]},
        {channel_ds, [
            {'is_subscribed', 2, fun(17, 2002) -> false end}
        ]},
        {channel_invitation_repo, [
            {'is_invited', 2, fun(_, _) ->
                erlang:error(should_not_check_invitation_for_public_channel)
            end}
        ]},
        {channel_order_repo, [
            {'has_purchased', 2, fun(_, _) ->
                erlang:error(should_not_check_purchase_for_public_channel)
            end}
        ]}
    ], fun() ->
        ?assertEqual(
            {ok, can_subscribe},
            channel_subscribe_ds:check_subscription_permission(17, 2002)
        ),
        ?assertEqual(1, meck:num_calls(channel_ds, is_subscribed, 2)),
        ?assertEqual(0, meck:num_calls(channel_invitation_repo, is_invited, 2)),
        ?assertEqual(0, meck:num_calls(channel_order_repo, has_purchased, 2))
    end).

subscribe_private_returns_error_when_not_invited_test_() ->
    ?WITH_MECKS([
        {channel_ds, [
            {'is_subscribed', 2, fun(11, 2002) -> false end}
        ]},
        {channel_invitation_repo, [
            {'is_invited', 2, fun(11, 2002) -> false end},
            {'find_pending_by_channel_and_invitee', 2, fun(_, _) ->
                erlang:error(should_not_find_pending_when_not_invited)
            end}
        ]}
    ], fun() ->
        ?assertEqual(
            {error, <<"私有频道需要邀请才能订阅"/utf8>>},
            channel_subscribe_ds:subscribe_private(11, 2002, undefined)
        ),
        ?assertEqual(1, meck:num_calls(channel_invitation_repo, is_invited, 2)),
        ?assertEqual(0, meck:num_calls(channel_invitation_repo, find_pending_by_channel_and_invitee, 2))
    end).

accept_invitation_subscribes_after_accepting_latest_invitation_test_() ->
    ?WITH_MECKS([
        {channel_invitation_repo, [
            {'find_by_id', 1, fun(501) ->
                {ok, #{
                    <<"id">> => 501,
                    <<"channel_id">> => 11,
                    <<"invitee_uid">> => 2002
                }}
            end},
            {'find_pending_by_channel_and_invitee', 2, fun(11, 2002) ->
                {ok, #{<<"id">> => 501}}
            end},
            {'accept', 2, fun(501, 2002) -> ok end}
        ]},
        {channel_ds, [
            {'subscribe', 2, fun(11, 2002) -> ok end}
        ]}
    ], fun() ->
        ?assertEqual(ok, channel_subscribe_ds:accept_invitation(501, 2002)),
        ?assertEqual(1, meck:num_calls(channel_ds, subscribe, 2))
    end).

accept_invitation_fallback_id_path_still_subscribes_test_() ->
    ?WITH_MECKS([
        {channel_invitation_repo, [
            {'find_by_id', 1, fun(502) ->
                {ok, #{
                    <<"id">> => 502,
                    <<"channel_id">> => 11,
                    <<"invitee_uid">> => 2002
                }}
            end},
            {'find_pending_by_channel_and_invitee', 2, fun(11, 2002) ->
                {error, not_found}
            end},
            {'accept', 2, fun(502, 2002) -> ok end}
        ]},
        {channel_ds, [
            {'subscribe', 2, fun(11, 2002) -> ok end}
        ]}
    ], fun() ->
        ?assertEqual(ok, channel_subscribe_ds:accept_invitation(502, 2002)),
        ?assertEqual(1, meck:num_calls(channel_ds, subscribe, 2))
    end).

accept_invitation_returns_pending_lookup_error_without_id_fallback_test_() ->
    ?WITH_MECKS([
        {channel_invitation_repo, [
            {'find_by_id', 1, fun(505) ->
                {ok, #{
                    <<"id">> => 505,
                    <<"channel_id">> => 11,
                    <<"invitee_uid">> => 2002
                }}
            end},
            {'find_pending_by_channel_and_invitee', 2, fun(11, 2002) ->
                {error, db_down}
            end},
            {'accept', 2, fun(_, _) ->
                erlang:error(should_not_accept_when_pending_lookup_failed)
            end}
        ]},
        {channel_ds, [
            {'subscribe', 2, fun(_, _) ->
                erlang:error(should_not_subscribe_when_pending_lookup_failed)
            end}
        ]}
    ], fun() ->
        ?assertEqual(
            {error, <<"db_down">>},
            channel_subscribe_ds:accept_invitation(505, 2002)
        ),
        ?assertEqual(1, meck:num_calls(channel_invitation_repo, find_by_id, 1)),
        ?assertEqual(0, meck:num_calls(channel_invitation_repo, accept, 2)),
        ?assertEqual(0, meck:num_calls(channel_ds, subscribe, 2))
    end).

accept_invitation_returns_error_when_invitation_missing_test_() ->
    ?WITH_MECKS([
        {channel_invitation_repo, [
            {'find_by_id', 1, fun(590) -> {error, not_found} end}
        ]},
        {channel_ds, [
            {'subscribe', 2, fun(_, _) ->
                erlang:error(should_not_subscribe_when_invitation_missing)
            end}
        ]}
    ], fun() ->
        ?assertEqual(
            {error, <<"邀请不存在"/utf8>>},
            channel_subscribe_ds:accept_invitation(590, 2002)
        ),
        ?assertEqual(0, meck:num_calls(channel_ds, subscribe, 2))
    end).

accept_invitation_propagates_find_by_id_error_test_() ->
    ?WITH_MECKS([
        {channel_invitation_repo, [
            {'find_by_id', 1, fun(592) -> {error, db_down} end}
        ]},
        {channel_ds, [
            {'subscribe', 2, fun(_, _) ->
                erlang:error(should_not_subscribe_when_find_by_id_failed)
            end}
        ]}
    ], fun() ->
        ?assertEqual(
            {error, <<"db_down">>},
            channel_subscribe_ds:accept_invitation(592, 2002)
        ),
        ?assertEqual(1, meck:num_calls(channel_invitation_repo, find_by_id, 1)),
        ?assertEqual(0, meck:num_calls(channel_ds, subscribe, 2))
    end).

accept_invitation_rejects_invitee_mismatch_before_accepting_test_() ->
    ?WITH_MECKS([
        {channel_invitation_repo, [
            {'find_by_id', 1, fun(591) ->
                {ok, #{
                    <<"id">> => 591,
                    <<"channel_id">> => 11,
                    <<"invitee_uid">> => 3003
                }}
            end},
            {'accept', 2, fun(_, _) ->
                erlang:error(should_not_accept_mismatched_invitee)
            end}
        ]},
        {channel_ds, [
            {'subscribe', 2, fun(_, _) ->
                erlang:error(should_not_subscribe_mismatched_invitee)
            end}
        ]}
    ], fun() ->
        ?assertEqual(
            {error, <<"无权接受此邀请"/utf8>>},
            channel_subscribe_ds:accept_invitation(591, 2002)
        ),
        ?assertEqual(0, meck:num_calls(channel_invitation_repo, accept, 2)),
        ?assertEqual(0, meck:num_calls(channel_ds, subscribe, 2))
    end).

accept_invitation_fallback_rejects_mismatched_channel_test_() ->
    ?WITH_MECKS([
        {channel_invitation_repo, [
            {'is_invited', 2, fun(12, 2002) -> true end},
            {'find_by_id', 1, fun(503) ->
                {ok, #{
                    <<"id">> => 503,
                    <<"channel_id">> => 11,
                    <<"invitee_uid">> => 2002
                }}
            end},
            {'find_pending_by_channel_and_invitee', 2, fun(12, 2002) ->
                {error, not_found}
            end},
            {'accept', 2, fun(_, _) ->
                erlang:error(should_not_accept_mismatched_invitation)
            end}
        ]},
        {channel_ds, [
            {'is_subscribed', 2, fun(12, 2002) -> false end},
            {'subscribe', 2, fun(_, _) ->
                erlang:error(should_not_subscribe_on_mismatched_invitation)
            end}
        ]}
    ], fun() ->
        ?assertEqual(
            {error, <<"邀请频道不匹配"/utf8>>},
            channel_subscribe_ds:subscribe_private(12, 2002, 503)
        ),
        ?assertEqual(0, meck:num_calls(channel_invitation_repo, accept, 2)),
        ?assertEqual(0, meck:num_calls(channel_ds, subscribe, 2))
    end).

accept_invitation_already_accepted_is_idempotent_without_reaccept_test_() ->
    ?WITH_MECKS([
        {channel_invitation_repo, [
            {'find_by_id', 1, fun(504) ->
                {ok, #{
                    <<"id">> => 504,
                    <<"channel_id">> => 11,
                    <<"invitee_uid">> => 2002,
                    <<"status">> => 1
                }}
            end},
            {'find_pending_by_channel_and_invitee', 2, fun(11, 2002) ->
                {error, not_found}
            end},
            {'accept', 2, fun(504, 2002) -> {error, not_found_or_expired} end}
        ]},
        {channel_ds, [
            {'subscribe', 2, fun(11, 2002) -> ok end}
        ]}
    ], fun() ->
        ?assertEqual(
            {error, already_accepted},
            channel_subscribe_ds:accept_invitation(504, 2002)
        ),
        ?assertEqual(1, meck:num_calls(channel_invitation_repo, accept, 2)),
        ?assertEqual(1, meck:num_calls(channel_ds, subscribe, 2))
    end).

reject_invitation_passes_through_ok_test_() ->
    ?WITH_MECKS([
        {channel_invitation_repo, [
            {'reject', 2, fun(601, 2002) -> ok end}
        ]}
    ], fun() ->
        ?assertEqual(ok, channel_subscribe_ds:reject_invitation(601, 2002)),
        ?assertEqual(1, meck:num_calls(channel_invitation_repo, reject, 2))
    end).

reject_invitation_passes_through_error_test_() ->
    ?WITH_MECKS([
        {channel_invitation_repo, [
            {'reject', 2, fun(602, 2002) -> {error, not_found} end}
        ]}
    ], fun() ->
        ?assertEqual(
            {error, not_found},
            channel_subscribe_ds:reject_invitation(602, 2002)
        ),
        ?assertEqual(1, meck:num_calls(channel_invitation_repo, reject, 2))
    end).

create_order_returns_error_when_already_purchased_test_() ->
    ?WITH_MECKS([
        {channel_order_repo, [
            {'has_purchased', 2, fun(11, 2002) -> true end},
            {'create_order', 1, fun(_) ->
                erlang:error(should_not_create_order_when_already_purchased)
            end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_, _) ->
                erlang:error(should_not_load_price_when_already_purchased)
            end}
        ]}
    ], fun() ->
        ?assertEqual(
            {error, <<"您已购买此频道"/utf8>>},
            channel_subscribe_ds:create_order(11, 2002, #{})
        ),
        ?assertEqual(0, meck:num_calls(channel_order_repo, create_order, 1)),
        ?assertEqual(0, meck:num_calls(elib_pg, query, 2))
    end).

create_order_returns_error_when_price_not_configured_test_() ->
    ?WITH_MECKS([
        {channel_order_repo, [
            {'has_purchased', 2, fun(11, 2002) -> false end},
            {'create_order', 1, fun(_) ->
                erlang:error(should_not_create_order_without_price)
            end}
        ]},
        {elib_pg, [
            {'query', 2, fun(Sql, [11]) ->
                SqlBin = iolist_to_binary(Sql),
                ?assert(re:run(SqlBin, <<"FROM channel_price">>) =/= nomatch),
                {ok, []}
            end}
        ]}
    ], fun() ->
        ?assertEqual(
            {error, <<"频道价格未配置"/utf8>>},
            channel_subscribe_ds:create_order(11, 2002, #{})
        ),
        ?assertEqual(0, meck:num_calls(channel_order_repo, create_order, 1)),
        ?assertEqual(1, meck:num_calls(elib_pg, query, 2))
    end).

create_order_propagates_price_query_error_test_() ->
    ?WITH_MECKS([
        {channel_order_repo, [
            {'has_purchased', 2, fun(11, 2002) -> false end},
            {'create_order', 1, fun(_) ->
                erlang:error(should_not_create_order_when_price_query_failed)
            end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, [11]) ->
                {error, db_down}
            end}
        ]}
    ], fun() ->
        ?assertEqual(
            {error, <<"db_down">>},
            channel_subscribe_ds:create_order(11, 2002, #{})
        ),
        ?assertEqual(0, meck:num_calls(channel_order_repo, create_order, 1)),
        ?assertEqual(1, meck:num_calls(elib_pg, query, 2))
    end).

create_order_creates_order_with_price_defaults_test_() ->
    ?WITH_MECKS([
        {channel_order_repo, [
            {'has_purchased', 2, fun(11, 2002) -> false end},
            {'create_order', 1, fun(Data) ->
                ?assertEqual(11, maps:get(channel_id, Data)),
                ?assertEqual(2002, maps:get(user_id, Data)),
                ?assertEqual(19.9, maps:get(amount, Data)),
                ?assertEqual(<<"USD">>, maps:get(currency, Data)),
                {ok, <<"ORD_NEW_001">>}
            end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, [11]) ->
                {ok, [#{
                    <<"id">> => 801,
                    <<"channel_id">> => 11,
                    <<"price">> => 19.9,
                    <<"currency">> => <<"USD">>
                }]}
            end}
        ]}
    ], fun() ->
        ?assertEqual(
            {ok, <<"ORD_NEW_001">>},
            channel_subscribe_ds:create_order(11, 2002, #{})
        ),
        ?assertEqual(1, meck:num_calls(channel_order_repo, create_order, 1)),
        ?assertEqual(1, meck:num_calls(elib_pg, query, 2))
    end).

pay_order_subscribes_after_successful_payment_test_() ->
    ?WITH_MECKS([
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD003">>) ->
                {ok, #{
                    <<"channel_id">> => 11,
                    <<"user_id">> => 2002,
                    <<"status">> => 0
                }}
            end},
            {'pay', 2, fun(<<"ORD003">>, _PaymentData) -> ok end}
        ]},
        {channel_ds, [
            {'subscribe', 2, fun(11, 2002) -> ok end}
        ]}
    ], fun() ->
        ?assertEqual(ok, channel_subscribe_ds:pay_order(<<"ORD003">>, #{})),
        ?assertEqual(1, meck:num_calls(channel_order_repo, pay, 2)),
        ?assertEqual(1, meck:num_calls(channel_ds, subscribe, 2))
    end).

pay_order_returns_order_not_found_when_initial_lookup_fails_test_() ->
    ?WITH_MECKS([
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD_MISSING">>) ->
                {error, not_found}
            end},
            {'pay', 2, fun(_, _) ->
                erlang:error(should_not_call_pay_when_order_missing)
            end}
        ]},
        {channel_ds, [
            {'subscribe', 2, fun(_, _) ->
                erlang:error(should_not_subscribe_when_order_missing)
            end}
        ]}
    ], fun() ->
        ?assertEqual(
            {error, <<"订单不存在"/utf8>>},
            channel_subscribe_ds:pay_order(<<"ORD_MISSING">>, #{})
        ),
        ?assertEqual(0, meck:num_calls(channel_order_repo, pay, 2)),
        ?assertEqual(0, meck:num_calls(channel_ds, subscribe, 2))
    end).

pay_order_propagates_initial_lookup_error_test_() ->
    ?WITH_MECKS([
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD_DB_ERR">>) ->
                {error, db_down}
            end},
            {'pay', 2, fun(_, _) ->
                erlang:error(should_not_call_pay_when_lookup_failed)
            end}
        ]},
        {channel_ds, [
            {'subscribe', 2, fun(_, _) ->
                erlang:error(should_not_subscribe_when_lookup_failed)
            end}
        ]}
    ], fun() ->
        ?assertEqual(
            {error, <<"db_down">>},
            channel_subscribe_ds:pay_order(<<"ORD_DB_ERR">>, #{})
        ),
        ?assertEqual(0, meck:num_calls(channel_order_repo, pay, 2)),
        ?assertEqual(0, meck:num_calls(channel_ds, subscribe, 2))
    end).

pay_order_when_already_paid_still_ensures_subscription_test_() ->
    ?WITH_MECKS([
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD004">>) ->
                {ok, #{
                    <<"channel_id">> => 11,
                    <<"user_id">> => 2002,
                    <<"status">> => 1
                }}
            end},
            {'pay', 2, fun(_, _) ->
                erlang:error(should_not_call_pay_for_paid_order)
            end}
        ]},
        {channel_ds, [
            {'subscribe', 2, fun(11, 2002) -> ok end}
        ]}
    ], fun() ->
        ?assertEqual(
            {error, already_paid},
            channel_subscribe_ds:pay_order(<<"ORD004">>, #{})
        ),
        ?assertEqual(0, meck:num_calls(channel_order_repo, pay, 2)),
        ?assertEqual(1, meck:num_calls(channel_ds, subscribe, 2))
    end).

pay_order_recovers_when_update_reports_not_found_but_order_is_paid_test_() ->
    ?WITH_MECKS([
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD005">>) ->
                case erlang:get(ord005_find_calls) of
                    undefined ->
                        erlang:put(ord005_find_calls, 1),
                        {ok, #{
                            <<"channel_id">> => 11,
                            <<"user_id">> => 2002,
                            <<"status">> => 0
                        }};
                    _ ->
                        {ok, #{
                            <<"channel_id">> => 11,
                            <<"user_id">> => 2002,
                            <<"status">> => 1
                        }}
                end
            end},
            {'pay', 2, fun(<<"ORD005">>, _PaymentData) ->
                {error, not_found_or_expired}
            end}
        ]},
        {channel_ds, [
            {'subscribe', 2, fun(11, 2002) -> ok end}
        ]}
    ], fun() ->
        erase(ord005_find_calls),
        ?assertEqual(
            {error, already_paid},
            channel_subscribe_ds:pay_order(<<"ORD005">>, #{})
        ),
        ?assertEqual(1, meck:num_calls(channel_order_repo, pay, 2)),
        ?assertEqual(2, meck:num_calls(channel_order_repo, find_by_order_no, 1)),
        ?assertEqual(1, meck:num_calls(channel_ds, subscribe, 2))
    end).

pay_order_returns_not_found_or_expired_when_recheck_cannot_load_order_test_() ->
    ?WITH_MECKS([
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD006">>) ->
                case erlang:get(ord006_find_calls) of
                    undefined ->
                        erlang:put(ord006_find_calls, 1),
                        {ok, #{
                            <<"channel_id">> => 11,
                            <<"user_id">> => 2002,
                            <<"status">> => 0
                        }};
                    _ ->
                        {error, not_found}
                end
            end},
            {'pay', 2, fun(<<"ORD006">>, _PaymentData) ->
                {error, not_found_or_expired}
            end}
        ]},
        {channel_ds, [
            {'subscribe', 2, fun(_, _) ->
                erlang:error(should_not_subscribe_when_recheck_order_missing)
            end}
        ]}
    ], fun() ->
        erase(ord006_find_calls),
        ?assertEqual(
            {error, not_found_or_expired},
            channel_subscribe_ds:pay_order(<<"ORD006">>, #{})
        ),
        ?assertEqual(1, meck:num_calls(channel_order_repo, pay, 2)),
        ?assertEqual(2, meck:num_calls(channel_order_repo, find_by_order_no, 1)),
        ?assertEqual(0, meck:num_calls(channel_ds, subscribe, 2))
    end).
