-module(attach_logic_tests).
%%%
% attach_logic 单元测试
% 覆盖：presign（MIME 白名单 + key 绑 uid）、confirm 越权守卫、ObjectKey 归属往返
%       confirm 服务端真实性校验（HEAD 核实存在性/真实大小/真实类型）
%       view_url fail-closed（归属校验失败不再降级放行）
%%%

-include_lib("eunit/include/eunit.hrl").
-include_lib("eunit_setup.hrl").

%% 与 elib_oss:?MAX_FILE_SIZE 保持一致（100MB）
-define(MAX_SIZE, 100 * 1024 * 1024).

%% ===================================================================
%% elib_oss: ObjectKey 绑定 uid 与归属反解
%% ===================================================================

build_object_key_has_uid_prefix_test() ->
    Key = elib_oss:build_object_key(123, <<"photo.jpg">>),
    ?assertMatch(<<"u123/", _/binary>>, Key),
    %% 以原始文件名结尾
    ?assertEqual(<<"photo.jpg">>, filename:basename(Key)).

owner_of_key_roundtrip_test() ->
    Key = elib_oss:build_object_key(456, <<"a.png">>),
    ?assertEqual({ok, 456}, elib_oss:owner_of_key(Key)).

owner_of_key_rejects_bad_key_test() ->
    ?assertEqual({error, invalid_key}, elib_oss:owner_of_key(<<"file_x/a.png">>)),
    ?assertEqual({error, invalid_key}, elib_oss:owner_of_key(<<"uabc/file/a.png">>)),
    ?assertEqual({error, invalid_key}, elib_oss:owner_of_key(<<>>)).

build_object_key_is_unique_test() ->
    %% 强随机：连续两次生成的 key 不相同
    K1 = elib_oss:build_object_key(1, <<"x.png">>),
    K2 = elib_oss:build_object_key(1, <<"x.png">>),
    ?assertNotEqual(K1, K2).

%% ===================================================================
%% presign：MIME 白名单 + 返回结构
%% ===================================================================

presign_rejects_invalid_mime_test() ->
    ?assertEqual(
        {error, invalid_file_type},
        attach_logic:presign(1, <<"a.exe">>, <<"application/x-msdownload">>)
    ).

presign_ok_returns_bound_key_test() ->
    {ok, Data} = attach_logic:presign(789, <<"a.png">>, <<"image/png">>),
    ?assertMatch(<<"u789/", _/binary>>, maps:get(<<"object_key">>, Data)),
    ?assert(is_binary(maps:get(<<"put_url">>, Data))),
    ?assert(is_integer(maps:get(<<"expires_at">>, Data))).

%% ===================================================================
%% confirm：越权守卫（上报他人命名空间 key 应被拒，且不触达 DB）
%% ===================================================================

confirm_rejects_cross_uid_key_test() ->
    %% uid=1 试图上报 uid=2 的 key
    OtherKey = elib_oss:build_object_key(2, <<"a.png">>),
    Meta = #{<<"md5">> => <<"abc">>, <<"mime_type">> => <<"image/png">>, <<"size">> => 10},
    ?assertEqual({error, forbidden_key}, attach_logic:confirm(1, OtherKey, Meta)).

confirm_rejects_invalid_key_test() ->
    Meta = #{<<"md5">> => <<"abc">>, <<"mime_type">> => <<"image/png">>, <<"size">> => 10},
    ?assertEqual({error, invalid_key}, attach_logic:confirm(1, <<"bad-key">>, Meta)).

%% ===================================================================
%% confirm：服务端真实性校验（缺口 1+2 加固）
%% 不再信任客户端自报的 size/mime_type，而是向 Garage 发 HEAD 核实真实值。
%% ===================================================================

%% 客户端 confirm 一个根本不存在的 ObjectKey（从未 PUT）→ 拒绝，不落孤儿元数据
confirm_rejects_missing_object_test_() ->
    ?WITH_MECKS(
        [
            {elib_oss, [
                {'head_object', 1, fun(_Key) -> {error, not_found} end}
            ]},
            {elib_pg, [{'with_tx', 1, fun(F) -> F(fake_conn) end}]},
            {attachment_ds, [{'save', 4, fun(_, _, _, _) -> ok end}]}
        ],
        fun() ->
            Key = <<"u789/file_x/a.png">>,
            Meta = #{
                <<"md5">> => <<"abc">>,
                <<"mime_type">> => <<"image/png">>,
                <<"size">> => 10
            },
            ?assertEqual({error, object_not_found}, attach_logic:confirm(789, Key, Meta)),
            %% 对象不存在时绝不落库
            ?assertEqual(0, meck:num_calls(attachment_ds, save, 4))
        end
    ).

%% 客户端谎报 size=10，但对象真实大小超过 100MB → 拒绝并删除对象
confirm_rejects_oversize_object_test_() ->
    ?WITH_MECKS(
        [
            {elib_oss, [
                {'head_object', 1, fun(_Key) ->
                    {ok, #{size => ?MAX_SIZE + 1, content_type => <<"image/png">>}}
                end},
                {'delete_object', 1, fun(_Key) -> ok end}
            ]},
            {elib_pg, [{'with_tx', 1, fun(F) -> F(fake_conn) end}]},
            {attachment_ds, [{'save', 4, fun(_, _, _, _) -> ok end}]}
        ],
        fun() ->
            Key = <<"u789/file_x/big.png">>,
            Meta = #{
                <<"md5">> => <<"abc">>,
                <<"mime_type">> => <<"image/png">>,
                <<"size">> => 10
            },
            ?assertEqual({error, file_too_large}, attach_logic:confirm(789, Key, Meta)),
            %% 超限对象必须被清理，且不得落库
            ?assertEqual(1, meck:num_calls(elib_oss, delete_object, 1)),
            ?assertEqual(0, meck:num_calls(attachment_ds, save, 4))
        end
    ).

%% 客户端谎报 mime=image/png 拿到签名，但对象真实类型不在白名单 → 拒绝并删除对象
confirm_rejects_invalid_type_object_test_() ->
    ?WITH_MECKS(
        [
            {elib_oss, [
                {'head_object', 1, fun(_Key) ->
                    {ok, #{size => 1024, content_type => <<"application/x-msdownload">>}}
                end},
                {'delete_object', 1, fun(_Key) -> ok end}
            ]},
            {elib_pg, [{'with_tx', 1, fun(F) -> F(fake_conn) end}]},
            {attachment_ds, [{'save', 4, fun(_, _, _, _) -> ok end}]}
        ],
        fun() ->
            Key = <<"u789/file_x/evil.png">>,
            Meta = #{
                <<"md5">> => <<"abc">>,
                <<"mime_type">> => <<"image/png">>,
                <<"size">> => 10
            },
            ?assertEqual({error, invalid_file_type}, attach_logic:confirm(789, Key, Meta)),
            ?assertEqual(1, meck:num_calls(elib_oss, delete_object, 1)),
            ?assertEqual(0, meck:num_calls(attachment_ds, save, 4))
        end
    ).

%% 校验通过：用 HEAD 返回的真实 size/mime_type 落库，忽略客户端自报值
confirm_persists_real_metadata_test_() ->
    ?WITH_MECKS(
        [
            {elib_oss, [
                {'head_object', 1, fun(_Key) ->
                    {ok, #{size => 2048, content_type => <<"image/png">>}}
                end}
            ]},
            {elib_pg, [{'with_tx', 1, fun(F) -> F(fake_conn) end}]},
            {attachment_ds, [{'save', 4, fun(_, _, _, _) -> ok end}]}
        ],
        fun() ->
            Key = <<"u789/file_x/a.png">>,
            %% 客户端谎报 size=10、mime=text/plain，应被真实值覆盖
            Meta = #{
                <<"md5">> => <<"deadbeef">>,
                <<"mime_type">> => <<"text/plain">>,
                <<"size">> => 10
            },
            ?assertEqual(
                {ok, #{<<"object_key">> => Key}},
                attach_logic:confirm(789, Key, Meta)
            ),
            [Attach] =
                meck:capture(first, attachment_ds, save, ['_', '_', '_', '_'], 4),
            ?assertEqual(2048, maps:get(<<"size">>, Attach)),
            ?assertEqual(<<"image/png">>, maps:get(<<"mime_type">>, Attach)),
            %% md5 仍取客户端上报（不作安全边界）
            ?assertEqual(<<"deadbeef">>, maps:get(<<"md5">>, Attach))
        end
    ).

%% 越权 key 在 HEAD 之前就被拒，绝不触达存储（HEAD）与 DB
confirm_cross_uid_skips_head_test_() ->
    ?WITH_MECKS(
        [
            {elib_oss, [
                {'head_object', 1, fun(_Key) ->
                    {ok, #{size => 1, content_type => <<"image/png">>}}
                end}
            ]}
        ],
        fun() ->
            OtherKey = <<"u2/file_x/a.png">>,
            Meta = #{
                <<"md5">> => <<"abc">>,
                <<"mime_type">> => <<"image/png">>,
                <<"size">> => 10
            },
            ?assertEqual({error, forbidden_key}, attach_logic:confirm(1, OtherKey, Meta)),
            ?assertEqual(0, meck:num_calls(elib_oss, head_object, 1))
        end
    ).

%% ===================================================================
%% view_url：fail-closed（缺口 3 加固）
%% 归属校验查询失败时不再降级放行，必须拒绝。
%% ===================================================================

view_url_grants_when_owner_test_() ->
    ?WITH_MECKS(
        [
            {attachment_ds, [{'find_by_path_and_uid', 2, fun(_K, _U) -> {ok, #{}} end}]},
            {elib_oss, [{'presign_get_for_key', 2, fun(_K, _E) -> <<"https://signed-get">> end}]}
        ],
        fun() ->
            ?assertEqual(
                {ok, <<"https://signed-get">>},
                attach_logic:view_url(1, <<"u1/file_x/a.png">>)
            )
        end
    ).

view_url_denies_when_not_found_test_() ->
    ?WITH_MECKS(
        [
            {attachment_ds, [{'find_by_path_and_uid', 2, fun(_K, _U) -> {error, not_found} end}]}
        ],
        fun() ->
            ?assertEqual(
                {error, forbidden},
                attach_logic:view_url(1, <<"u2/file_x/a.png">>)
            )
        end
    ).

%% 关键：DB/存储查询失败时必须 fail-closed（拒绝），而非降级放行
view_url_fails_closed_on_query_error_test_() ->
    ?WITH_MECKS(
        [
            {attachment_ds, [
                {'find_by_path_and_uid', 2, fun(_K, _U) -> {error, db_timeout} end}
            ]},
            {elib_oss, [{'presign_get_for_key', 2, fun(_K, _E) -> <<"https://signed-get">> end}]}
        ],
        fun() ->
            ?assertEqual(
                {error, forbidden},
                attach_logic:view_url(1, <<"u1/file_x/a.png">>)
            ),
            %% fail-closed：绝不签发下载 URL
            ?assertEqual(0, meck:num_calls(elib_oss, presign_get_for_key, 2))
        end
    ).
