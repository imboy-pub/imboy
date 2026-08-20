-module(attach_logic_tests).
%%%
% attach_logic 单元测试（资源访问控制版）
% 覆盖：
%   presign/5  类型白名单 + can_upload 上传权（public/private/c2c/group + channel/moment 占位）
%   confirm/5  越权守卫(skip head) + HEAD 真实性核实 + scope 落库
%   view_url/2 authorize/2 六分支全覆盖（public/private/c2c/group/channel/moment）+ fail-closed
%%%

-include_lib("eunit/include/eunit.hrl").
-include_lib("eunit_setup.hrl").

%% 与 elib_oss:?MAX_FILE_SIZE 保持一致（100MB）
-define(MAX_SIZE, 100 * 1024 * 1024).

set_garage_env() ->
    application:set_env(imboy, garage, #{
        endpoint => <<"http://127.0.0.1:3900">>,
        bucket => <<"imboy">>,
        public_bucket => <<"imboy-public">>,
        public_base_url => <<"https://s3.imboy.pub">>,
        access_key => <<"GKtest">>,
        secret_key => <<"testsecret">>
    }).

%% ===================================================================
%% presign：MIME 白名单 + can_upload 上传权
%% ===================================================================

presign_rejects_invalid_mime_test() ->
    ?assertEqual(
        {error, invalid_file_type},
        attach_logic:presign(
            1, <<"a.exe">>, <<"application/x-msdownload">>, <<"private">>, undefined
        )
    ).

presign_private_ok_returns_bound_key_test() ->
    set_garage_env(),
    {ok, Data} = attach_logic:presign(789, <<"a.png">>, <<"image/png">>, <<"private">>, undefined),
    ?assertMatch(<<"u789/file/", _/binary>>, maps:get(<<"object_key">>, Data)),
    ?assert(is_binary(maps:get(<<"put_url">>, Data))),
    ?assert(is_integer(maps:get(<<"expires_at">>, Data))),
    %% 非 public 不返回 public_url
    ?assertEqual(error, maps:find(<<"public_url">>, Data)).

presign_public_returns_public_url_test() ->
    set_garage_env(),
    {ok, Data} = attach_logic:presign(
        52278, <<"head.jpg">>, <<"image/jpeg">>, <<"public">>, undefined
    ),
    ?assertMatch(<<"u52278/avatar/", _/binary>>, maps:get(<<"object_key">>, Data)),
    ?assertMatch(
        <<"https://s3.imboy.pub/u52278/avatar/", _/binary>>, maps:get(<<"public_url">>, Data)
    ).

presign_c2c_member_ok_test() ->
    set_garage_env(),
    {ok, Data} = attach_logic:presign(1, <<"a.png">>, <<"image/png">>, <<"c2c">>, <<"c2c:1:2">>),
    ?assertMatch(<<"u1/c2c/", _/binary>>, maps:get(<<"object_key">>, Data)).

presign_c2c_non_member_forbidden_test() ->
    set_garage_env(),
    ?assertEqual(
        {error, forbidden},
        attach_logic:presign(3, <<"a.png">>, <<"image/png">>, <<"c2c">>, <<"c2c:1:2">>)
    ).

presign_group_member_ok_test_() ->
    ?WITH_MECK(
        group_member_ds,
        [{'is_member', 2, fun(66, 1) -> true end}],
        fun() ->
            set_garage_env(),
            {ok, Data} = attach_logic:presign(
                1, <<"a.png">>, <<"image/png">>, <<"group">>, <<"66">>
            ),
            ?assertMatch(<<"u1/g66/", _/binary>>, maps:get(<<"object_key">>, Data))
        end
    ).

presign_group_non_member_forbidden_test_() ->
    ?WITH_MECK(
        group_member_ds,
        [{'is_member', 2, fun(_, _) -> false end}],
        fun() ->
            set_garage_env(),
            ?assertEqual(
                {error, forbidden},
                attach_logic:presign(1, <<"a.png">>, <<"image/png">>, <<"group">>, <<"66">>)
            )
        end
    ).

presign_channel_subscriber_ok_test_() ->
    ?WITH_MECKS(
        [
            %% 非频道管理角色（get_role → 0），走订阅判定
            {channel_admin_ds, [{'get_role', 2, fun(_, _) -> 0 end}]},
            {channel_ds, [
                {'find_by_id', 2, fun(9, _Fields) -> #{<<"type">> => 0} end}
            ]},
            {channel_subscription_ds, [{'is_subscribed', 2, fun(9, 1) -> true end}]}
        ],
        fun() ->
            set_garage_env(),
            {ok, Data} = attach_logic:presign(
                1, <<"a.png">>, <<"image/png">>, <<"channel">>, <<"9">>
            ),
            ?assertMatch(<<"u1/channel/", _/binary>>, maps:get(<<"object_key">>, Data))
        end
    ).

presign_channel_non_subscriber_forbidden_test_() ->
    ?WITH_MECKS(
        [
            {channel_admin_ds, [{'get_role', 2, fun(_, _) -> 0 end}]},
            {channel_ds, [
                {'find_by_id', 2, fun(9, _Fields) -> #{<<"type">> => 0} end}
            ]},
            {channel_subscription_ds, [{'is_subscribed', 2, fun(_, _) -> false end}]}
        ],
        fun() ->
            set_garage_env(),
            ?assertEqual(
                {error, forbidden},
                attach_logic:presign(1, <<"a.png">>, <<"image/png">>, <<"channel">>, <<"9">>)
            )
        end
    ).

presign_paid_channel_subscription_without_purchase_forbidden_test_() ->
    ?WITH_MECKS(
        [
            {channel_ds, [
                {'find_by_id', 2, fun(9, _Fields) ->
                    #{
                        <<"id">> => 9, <<"type">> => 2, <<"status">> => 1
                    }
                end}
            ]},
            {channel_admin_ds, [{'get_role', 2, fun(9, 1) -> 0 end}]},
            {channel_order_ds, [{'has_purchased', 2, fun(9, 1) -> false end}]},
            {channel_subscription_ds, [{'is_subscribed', 2, fun(9, 1) -> true end}]}
        ],
        fun() ->
            set_garage_env(),
            ?assertEqual(
                {error, forbidden},
                attach_logic:presign(1, <<"a.png">>, <<"image/png">>, <<"channel">>, <<"9">>)
            ),
            ?assertEqual(0, meck:num_calls(channel_subscription_ds, is_subscribed, 2))
        end
    ).

%% moment 上传设计上放行（scope_ref 发帖时未知，可见性在读时按帖子 ACL 卡）
presign_moment_ok_test() ->
    set_garage_env(),
    {ok, Data} = attach_logic:presign(1, <<"a.png">>, <<"image/png">>, <<"moment">>, undefined),
    ?assertMatch(<<"u1/moment/", _/binary>>, maps:get(<<"object_key">>, Data)).

%% ===================================================================
%% confirm：越权守卫（上报他人命名空间 key 应被拒，不触达 DB/存储）
%% ===================================================================

confirm_rejects_cross_uid_key_test() ->
    OtherKey = elib_oss:build_object_key(2, <<"private">>, undefined, <<"a.png">>),
    Meta = #{<<"md5">> => <<"abc">>, <<"mime_type">> => <<"image/png">>, <<"size">> => 10},
    ?assertEqual(
        {error, forbidden_key},
        attach_logic:confirm(1, OtherKey, <<"private">>, undefined, Meta)
    ).

confirm_rejects_invalid_key_test() ->
    Meta = #{<<"md5">> => <<"abc">>, <<"mime_type">> => <<"image/png">>, <<"size">> => 10},
    ?assertEqual(
        {error, invalid_key},
        attach_logic:confirm(1, <<"bad-key">>, <<"private">>, undefined, Meta)
    ).

%% 越权 key 在 HEAD 之前就被拒，绝不触达存储（HEAD）
confirm_cross_uid_skips_head_test_() ->
    ?WITH_MECKS(
        [
            {elib_oss, [
                {'head_object', 2, fun(_B, _K) ->
                    {ok, #{size => 1, content_type => <<"image/png">>}}
                end}
            ]}
        ],
        fun() ->
            OtherKey = <<"u2/file/20260620/file_x/a.png">>,
            Meta = #{<<"md5">> => <<"abc">>, <<"mime_type">> => <<"image/png">>, <<"size">> => 10},
            ?assertEqual(
                {error, forbidden_key},
                attach_logic:confirm(1, OtherKey, <<"private">>, undefined, Meta)
            ),
            ?assertEqual(0, meck:num_calls(elib_oss, head_object, 2))
        end
    ).

%% group 非成员：can_upload 在 HEAD 之前拒绝
confirm_group_non_member_forbidden_test_() ->
    ?WITH_MECKS(
        [
            {group_member_ds, [{'is_member', 2, fun(_, _) -> false end}]},
            {elib_oss, [
                {'head_object', 2, fun(_B, _K) ->
                    {ok, #{size => 1, content_type => <<"image/png">>}}
                end}
            ]}
        ],
        fun() ->
            Key = <<"u1/g66/20260620/file_x/a.png">>,
            Meta = #{<<"md5">> => <<"abc">>, <<"mime_type">> => <<"image/png">>, <<"size">> => 10},
            ?assertEqual(
                {error, forbidden},
                attach_logic:confirm(1, Key, <<"group">>, <<"66">>, Meta)
            ),
            ?assertEqual(0, meck:num_calls(elib_oss, head_object, 2))
        end
    ).

%% ===================================================================
%% confirm：服务端真实性校验（HEAD 核实存在性/真实大小/真实类型）
%% ===================================================================

confirm_rejects_missing_object_test_() ->
    ?WITH_MECKS(
        [
            {elib_oss, [{'head_object', 2, fun(_B, _K) -> {error, not_found} end}]},
            {elib_pg, [{'with_tx', 1, fun(F) -> F(fake_conn) end}]},
            {attachment_ds, [{'save', 4, fun(_, _, _, _) -> ok end}]}
        ],
        fun() ->
            Key = <<"u789/file/20260620/file_x/a.png">>,
            Meta = #{<<"md5">> => <<"abc">>, <<"mime_type">> => <<"image/png">>, <<"size">> => 10},
            ?assertEqual(
                {error, object_not_found},
                attach_logic:confirm(789, Key, <<"private">>, undefined, Meta)
            ),
            ?assertEqual(0, meck:num_calls(attachment_ds, save, 4))
        end
    ).

confirm_rejects_oversize_object_test_() ->
    ?WITH_MECKS(
        [
            {elib_oss, [
                {'head_object', 2, fun(_B, _K) ->
                    {ok, #{size => ?MAX_SIZE + 1, content_type => <<"image/png">>}}
                end},
                {'delete_object', 2, fun(_B, _K) -> ok end}
            ]},
            {elib_pg, [{'with_tx', 1, fun(F) -> F(fake_conn) end}]},
            {attachment_ds, [{'save', 4, fun(_, _, _, _) -> ok end}]}
        ],
        fun() ->
            Key = <<"u789/file/20260620/file_x/big.png">>,
            Meta = #{<<"md5">> => <<"abc">>, <<"mime_type">> => <<"image/png">>, <<"size">> => 10},
            ?assertEqual(
                {error, file_too_large},
                attach_logic:confirm(789, Key, <<"private">>, undefined, Meta)
            ),
            ?assertEqual(1, meck:num_calls(elib_oss, delete_object, 2)),
            ?assertEqual(0, meck:num_calls(attachment_ds, save, 4))
        end
    ).

confirm_rejects_invalid_type_object_test_() ->
    ?WITH_MECKS(
        [
            {elib_oss, [
                {'head_object', 2, fun(_B, _K) ->
                    {ok, #{size => 1024, content_type => <<"application/x-msdownload">>}}
                end},
                {'delete_object', 2, fun(_B, _K) -> ok end}
            ]},
            {elib_pg, [{'with_tx', 1, fun(F) -> F(fake_conn) end}]},
            {attachment_ds, [{'save', 4, fun(_, _, _, _) -> ok end}]}
        ],
        fun() ->
            Key = <<"u789/file/20260620/file_x/evil.png">>,
            Meta = #{<<"md5">> => <<"abc">>, <<"mime_type">> => <<"image/png">>, <<"size">> => 10},
            ?assertEqual(
                {error, invalid_file_type},
                attach_logic:confirm(789, Key, <<"private">>, undefined, Meta)
            ),
            ?assertEqual(1, meck:num_calls(elib_oss, delete_object, 2)),
            ?assertEqual(0, meck:num_calls(attachment_ds, save, 4))
        end
    ).

%% 校验通过：用 HEAD 真实 size/mime 落库，且写入 scope/scope_ref
confirm_persists_real_metadata_and_scope_test_() ->
    ?WITH_MECKS(
        [
            {elib_oss, [
                {'head_object', 2, fun(_B, _K) ->
                    {ok, #{size => 2048, content_type => <<"image/png">>}}
                end}
            ]},
            {elib_pg, [{'with_tx', 1, fun(F) -> F(fake_conn) end}]},
            {attachment_ds, [{'save', 4, fun(_, _, _, _) -> ok end}]}
        ],
        fun() ->
            Key = <<"u789/file/20260620/file_x/a.png">>,
            Meta = #{
                <<"md5">> => <<"deadbeef">>, <<"mime_type">> => <<"text/plain">>, <<"size">> => 10
            },
            ?assertEqual(
                {ok, #{<<"object_key">> => Key}},
                attach_logic:confirm(789, Key, <<"private">>, undefined, Meta)
            ),
            [Attach] = meck:capture(first, attachment_ds, save, ['_', '_', '_', '_'], 4),
            ?assertEqual(2048, maps:get(<<"size">>, Attach)),
            ?assertEqual(<<"image/png">>, maps:get(<<"mime_type">>, Attach)),
            ?assertEqual(<<"deadbeef">>, maps:get(<<"file_hash256">>, Attach)),
            ?assertEqual(<<"private">>, maps:get(<<"scope">>, Attach)),
            ?assertEqual(null, maps:get(<<"scope_ref">>, Attach))
        end
    ).

%% public confirm 落库 scope=public 并返回 public_url
confirm_public_returns_public_url_test_() ->
    ?WITH_MECKS(
        [
            {elib_oss, [
                {'head_object', 2, fun(_B, _K) ->
                    {ok, #{size => 100, content_type => <<"image/jpeg">>}}
                end}
            ]},
            {elib_pg, [{'with_tx', 1, fun(F) -> F(fake_conn) end}]},
            {attachment_ds, [{'save', 4, fun(_, _, _, _) -> ok end}]}
        ],
        fun() ->
            set_garage_env(),
            Key = <<"u52278/avatar/20260620/abc.jpg">>,
            Meta = #{<<"md5">> => <<"x">>, <<"mime_type">> => <<"image/jpeg">>, <<"size">> => 1},
            {ok, Data} = attach_logic:confirm(52278, Key, <<"public">>, undefined, Meta),
            ?assertEqual(Key, maps:get(<<"object_key">>, Data)),
            ?assertEqual(<<"https://s3.imboy.pub/", Key/binary>>, maps:get(<<"public_url">>, Data)),
            [Attach] = meck:capture(first, attachment_ds, save, ['_', '_', '_', '_'], 4),
            ?assertEqual(<<"public">>, maps:get(<<"scope">>, Attach))
        end
    ).

%% ===================================================================
%% view_url + authorize/2 六分支
%% ===================================================================

%% public：直读公开 URL，不签名
authorize_public_direct_url_test_() ->
    ?WITH_MECKS(
        [
            {attachment_ds, [
                {'find_by_path', 1, fun(_K) ->
                    {ok, #{<<"scope">> => <<"public">>, <<"creator_user_id">> => 2}}
                end}
            ]},
            {elib_oss, [{'public_url_for_key', 1, fun(_K) -> <<"https://pub">> end}]}
        ],
        fun() ->
            %% 任何 uid（含非 owner）都能读
            ?assertEqual({ok, <<"https://pub">>}, attach_logic:view_url(999, <<"u2/avatar/x.jpg">>))
        end
    ).

%% private：owner 放行 / 非 owner 拒绝
authorize_private_owner_grants_test_() ->
    ?WITH_MECKS(
        [
            {attachment_ds, [
                {'find_by_path', 1, fun(_K) ->
                    {ok, #{<<"scope">> => <<"private">>, <<"creator_user_id">> => 1}}
                end}
            ]},
            {elib_oss, [{'presign_get_for_key', 3, fun(_B, _K, _E) -> <<"https://sig">> end}]}
        ],
        fun() ->
            ?assertEqual({ok, <<"https://sig">>}, attach_logic:view_url(1, <<"u1/file/a.png">>))
        end
    ).

authorize_private_non_owner_denies_test_() ->
    ?WITH_MECKS(
        [
            {attachment_ds, [
                {'find_by_path', 1, fun(_K) ->
                    {ok, #{<<"scope">> => <<"private">>, <<"creator_user_id">> => 1}}
                end}
            ]},
            {elib_oss, [{'presign_get_for_key', 3, fun(_B, _K, _E) -> <<"https://sig">> end}]}
        ],
        fun() ->
            ?assertEqual({error, forbidden}, attach_logic:view_url(2, <<"u1/file/a.png">>)),
            ?assertEqual(0, meck:num_calls(elib_oss, presign_get_for_key, 3))
        end
    ).

%% c2c：会话方放行 / 第三方拒绝（conv_key_vo 真实解析）
authorize_c2c_member_grants_test_() ->
    ?WITH_MECKS(
        [
            {attachment_ds, [
                {'find_by_path', 1, fun(_K) ->
                    {ok, #{<<"scope">> => <<"c2c">>, <<"scope_ref">> => <<"c2c:1:2">>}}
                end}
            ]},
            {elib_oss, [{'presign_get_for_key', 3, fun(_B, _K, _E) -> <<"https://sig">> end}]}
        ],
        fun() ->
            ?assertEqual({ok, <<"https://sig">>}, attach_logic:view_url(2, <<"u1/c2c/a.png">>))
        end
    ).

authorize_c2c_third_party_denies_test_() ->
    ?WITH_MECKS(
        [
            {attachment_ds, [
                {'find_by_path', 1, fun(_K) ->
                    {ok, #{<<"scope">> => <<"c2c">>, <<"scope_ref">> => <<"c2c:1:2">>}}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual({error, forbidden}, attach_logic:view_url(3, <<"u1/c2c/a.png">>))
        end
    ).

%% group：成员放行 / 非成员拒绝（is_member(Gid, Uid) Gid 在前）
authorize_group_member_grants_test_() ->
    ?WITH_MECKS(
        [
            {attachment_ds, [
                {'find_by_path', 1, fun(_K) ->
                    {ok, #{<<"scope">> => <<"group">>, <<"scope_ref">> => <<"66">>}}
                end}
            ]},
            {group_member_ds, [{'is_member', 2, fun(66, 7) -> true end}]},
            {elib_oss, [{'presign_get_for_key', 3, fun(_B, _K, _E) -> <<"https://sig">> end}]}
        ],
        fun() ->
            ?assertEqual({ok, <<"https://sig">>}, attach_logic:view_url(7, <<"u1/g66/a.png">>))
        end
    ).

authorize_group_non_member_denies_test_() ->
    ?WITH_MECKS(
        [
            {attachment_ds, [
                {'find_by_path', 1, fun(_K) ->
                    {ok, #{<<"scope">> => <<"group">>, <<"scope_ref">> => <<"66">>}}
                end}
            ]},
            {group_member_ds, [{'is_member', 2, fun(_, _) -> false end}]}
        ],
        fun() ->
            ?assertEqual({error, forbidden}, attach_logic:view_url(7, <<"u1/g66/a.png">>))
        end
    ).

%% channel：订阅者放行 / 未订阅拒绝（is_subscribed(ChannelId, Uid) ChannelId 在前）
authorize_channel_subscriber_grants_test_() ->
    ?WITH_MECKS(
        [
            {attachment_ds, [
                {'find_by_path', 1, fun(_K) ->
                    {ok, #{<<"scope">> => <<"channel">>, <<"scope_ref">> => <<"9">>}}
                end}
            ]},
            {channel_admin_ds, [{'get_role', 2, fun(_, _) -> 0 end}]},
            {channel_ds, [
                {'find_by_id', 2, fun(9, _Fields) -> #{<<"type">> => 0} end}
            ]},
            {channel_subscription_ds, [{'is_subscribed', 2, fun(9, 7) -> true end}]},
            {elib_oss, [{'presign_get_for_key', 3, fun(_B, _K, _E) -> <<"https://sig">> end}]}
        ],
        fun() ->
            ?assertEqual({ok, <<"https://sig">>}, attach_logic:view_url(7, <<"u1/ch9/a.png">>))
        end
    ).

authorize_channel_non_subscriber_denies_test_() ->
    ?WITH_MECKS(
        [
            {attachment_ds, [
                {'find_by_path', 1, fun(_K) ->
                    {ok, #{<<"scope">> => <<"channel">>, <<"scope_ref">> => <<"9">>}}
                end}
            ]},
            {channel_admin_ds, [{'get_role', 2, fun(_, _) -> 0 end}]},
            {channel_ds, [
                {'find_by_id', 2, fun(9, _Fields) -> #{<<"type">> => 0} end}
            ]},
            {channel_subscription_ds, [{'is_subscribed', 2, fun(_, _) -> false end}]}
        ],
        fun() ->
            ?assertEqual({error, forbidden}, attach_logic:view_url(7, <<"u1/ch9/a.png">>))
        end
    ).

%% 付费频道：有效购买放行，即使订阅记录不是访问权来源。
authorize_paid_channel_purchased_grants_test_() ->
    ?WITH_MECKS(
        [
            {attachment_ds, [
                {'find_by_path', 1, fun(_K) ->
                    {ok, #{<<"scope">> => <<"channel">>, <<"scope_ref">> => <<"9">>}}
                end}
            ]},
            {channel_ds, [
                {'find_by_id', 2, fun(9, _Fields) ->
                    #{
                        <<"id">> => 9, <<"type">> => 2, <<"status">> => 1
                    }
                end}
            ]},
            {channel_admin_ds, [{'get_role', 2, fun(9, 7) -> 0 end}]},
            {channel_order_ds, [{'has_purchased', 2, fun(9, 7) -> true end}]},
            {elib_oss, [{'presign_get_for_key', 3, fun(_B, _K, _E) -> <<"https://sig">> end}]}
        ],
        fun() ->
            ?assertEqual({ok, <<"https://sig">>}, attach_logic:view_url(7, <<"u7/ch9/a.png">>))
        end
    ).

%% 付费频道：历史订阅不能替代购买，附件下载必须与正文同样拒绝。
authorize_paid_channel_subscription_without_purchase_denies_test_() ->
    ?WITH_MECKS(
        [
            {attachment_ds, [
                {'find_by_path', 1, fun(_K) ->
                    {ok, #{<<"scope">> => <<"channel">>, <<"scope_ref">> => <<"9">>}}
                end}
            ]},
            {channel_ds, [
                {'find_by_id', 2, fun(9, _Fields) ->
                    #{
                        <<"id">> => 9, <<"type">> => 2, <<"status">> => 1
                    }
                end}
            ]},
            {channel_admin_ds, [{'get_role', 2, fun(9, 7) -> 0 end}]},
            {channel_order_ds, [{'has_purchased', 2, fun(9, 7) -> false end}]},
            {channel_subscription_ds, [{'is_subscribed', 2, fun(9, 7) -> true end}]}
        ],
        fun() ->
            ?assertEqual({error, forbidden}, attach_logic:view_url(7, <<"u7/ch9/a.png">>)),
            ?assertEqual(0, meck:num_calls(channel_subscription_ds, is_subscribed, 2))
        end
    ).

%% 上传者本人：channel scope 下「订阅者可访问」语义优先于 creator 归属，
%% 上传者（creator=查看者）若仍订阅则该频道附件可访问（退订后不可访问，同上例）。
authorize_channel_uploader_subscribed_grants_test_() ->
    ?WITH_MECKS(
        [
            {attachment_ds, [
                {'find_by_path', 1, fun(_K) ->
                    {ok, #{
                        <<"scope">> => <<"channel">>,
                        <<"scope_ref">> => <<"9">>,
                        <<"creator_user_id">> => 7
                    }}
                end}
            ]},
            {channel_admin_ds, [{'get_role', 2, fun(_, _) -> 0 end}]},
            {channel_ds, [
                {'find_by_id', 2, fun(9, _Fields) -> #{<<"type">> => 0} end}
            ]},
            {channel_subscription_ds, [{'is_subscribed', 2, fun(9, 7) -> true end}]},
            {elib_oss, [{'presign_get_for_key', 3, fun(_B, _K, _E) -> <<"https://sig">> end}]}
        ],
        fun() ->
            ?assertEqual({ok, <<"https://sig">>}, attach_logic:view_url(7, <<"u7/ch9/a.png">>))
        end
    ).

%% moment：可见放行 / 不可见拒绝 / post 不存在拒绝
authorize_moment_visible_grants_test_() ->
    ?WITH_MECKS(
        [
            {attachment_ds, [
                {'find_by_path', 1, fun(_K) ->
                    {ok, #{<<"scope">> => <<"moment">>, <<"scope_ref">> => <<"555">>}}
                end}
            ]},
            {moment_ds, [
                {'get_post', 1, fun(555) -> #{<<"author_uid">> => 1, <<"status">> => 1} end},
                {'can_view_post', 2, fun(7, _Post) -> true end}
            ]},
            {elib_oss, [{'presign_get_for_key', 3, fun(_B, _K, _E) -> <<"https://sig">> end}]}
        ],
        fun() ->
            ?assertEqual({ok, <<"https://sig">>}, attach_logic:view_url(7, <<"u1/m555/a.png">>))
        end
    ).

authorize_moment_invisible_denies_test_() ->
    ?WITH_MECKS(
        [
            {attachment_ds, [
                {'find_by_path', 1, fun(_K) ->
                    {ok, #{<<"scope">> => <<"moment">>, <<"scope_ref">> => <<"555">>}}
                end}
            ]},
            {moment_ds, [
                {'get_post', 1, fun(555) -> #{<<"author_uid">> => 1, <<"status">> => 1} end},
                {'can_view_post', 2, fun(_, _) -> false end}
            ]}
        ],
        fun() ->
            ?assertEqual({error, forbidden}, attach_logic:view_url(7, <<"u1/m555/a.png">>))
        end
    ).

authorize_moment_missing_post_denies_test_() ->
    ?WITH_MECKS(
        [
            {attachment_ds, [
                {'find_by_path', 1, fun(_K) ->
                    {ok, #{<<"scope">> => <<"moment">>, <<"scope_ref">> => <<"555">>}}
                end}
            ]},
            {moment_ds, [{'get_post', 1, fun(555) -> {error, not_found} end}]}
        ],
        fun() ->
            ?assertEqual({error, forbidden}, attach_logic:view_url(7, <<"u1/m555/a.png">>))
        end
    ).

%% ===================================================================
%% view_url：not_found 与 fail-closed
%% ===================================================================

view_url_denies_when_not_found_test_() ->
    ?WITH_MECKS(
        [{attachment_ds, [{'find_by_path', 1, fun(_K) -> {error, not_found} end}]}],
        fun() ->
            ?assertEqual({error, forbidden}, attach_logic:view_url(1, <<"u2/file/a.png">>))
        end
    ).

view_url_fails_closed_on_query_error_test_() ->
    ?WITH_MECKS(
        [
            {attachment_ds, [{'find_by_path', 1, fun(_K) -> {error, db_timeout} end}]},
            {elib_oss, [{'presign_get_for_key', 3, fun(_B, _K, _E) -> <<"https://sig">> end}]}
        ],
        fun() ->
            ?assertEqual({error, forbidden}, attach_logic:view_url(1, <<"u1/file/a.png">>)),
            ?assertEqual(0, meck:num_calls(elib_oss, presign_get_for_key, 3))
        end
    ).
