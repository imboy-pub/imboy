-module(attach_logic_tests).
%%%
% attach_logic 单元测试
% 覆盖：presign（MIME 白名单 + key 绑 uid）、confirm 越权守卫、ObjectKey 归属往返
% confirm 的成功落库路径依赖 DB，不在此纯单元测试覆盖（由集成/E2E 覆盖）
%%%

-include_lib("eunit/include/eunit.hrl").

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
