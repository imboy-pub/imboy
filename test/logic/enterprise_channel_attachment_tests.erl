-module(enterprise_channel_attachment_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("eunit_setup.hrl").

%% 旧管理员/订阅权益不能覆盖父级企业失权；三条签发/转正路径都拒绝。
channel_parent_scope_denied_blocks_all_attachment_paths_test_() ->
    ?WITH_MECKS(
        [
            {attachment_ds, [
                {'authorize_channel_scope', 2, fun(9, 7) -> false end},
                {'find_by_path', 1, fun(_K) ->
                    {ok, #{<<"scope">> => <<"channel">>, <<"scope_ref">> => <<"9">>}}
                end}
            ]},
            {channel_admin_ds, [{'get_role', 2, fun(_, _) -> 3 end}]},
            {elib_oss, [
                {'presign_put_for_key', 4, fun(_, _, _, _) -> <<"unexpected">> end},
                {'presign_get_for_key', 3, fun(_, _, _) -> <<"unexpected">> end},
                {'head_object', 2, fun(_, _) -> {error, unexpected} end}
            ]}
        ],
        fun() ->
            Key = elib_oss:build_object_key(7, <<"channel">>, <<"9">>, <<"a.png">>),
            ?assertEqual(
                {error, forbidden},
                attach_logic:presign(7, <<"a.png">>, <<"image/png">>, <<"channel">>, <<"9">>)
            ),
            ?assertEqual(
                {error, forbidden},
                attach_logic:confirm(7, Key, <<"channel">>, <<"9">>, #{})
            ),
            ?assertEqual({error, forbidden}, attach_logic:view_url(7, Key)),
            ?assertEqual(0, meck:num_calls(channel_admin_ds, get_role, 2)),
            ?assertEqual(0, meck:num_calls(elib_oss, presign_put_for_key, 4)),
            ?assertEqual(0, meck:num_calls(elib_oss, presign_get_for_key, 3)),
            ?assertEqual(0, meck:num_calls(elib_oss, head_object, 2))
        end
    ).

active_channel_admin_keeps_attachment_access_test_() ->
    ?WITH_MECKS(
        [
            {attachment_ds, [
                {'authorize_channel_scope', 2, fun(9, 7) -> true end},
                {'find_by_path', 1, fun(_) ->
                    {ok, #{<<"scope">> => <<"channel">>, <<"scope_ref">> => <<"9">>}}
                end}
            ]},
            {channel_admin_ds, [{'get_role', 2, fun(9, 7) -> 3 end}]},
            {channel_subscription_ds, [{'is_subscribed', 2, fun(_, _) -> false end}]},
            {elib_oss, [
                {'get_bucket', 1, fun(_) -> <<"bucket">> end},
                {'presign_get_for_key', 3, fun(_, _, _) -> <<"https://signed">> end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, <<"https://signed">>},
                attach_logic:view_url(7, <<"u7/channel/a.png">>)
            ),
            ?assertEqual(0, meck:num_calls(channel_subscription_ds, is_subscribed, 2))
        end
    ).
