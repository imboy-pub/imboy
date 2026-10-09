-module(group_album_read_authorization_tests).
-include_lib("eunit/include/eunit.hrl").

read_authorization_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun(_) -> ?_test(member_list_signs_response()) end,
        fun(_) -> ?_test(member_detail_signs_response()) end,
        fun(_) -> ?_test(nonmember_list_has_no_signature()) end,
        fun(_) -> ?_test(nonmember_detail_has_no_signature()) end,
        fun(_) -> ?_test(legacy_url_preserved()) end
    ]}.

setup() ->
    lists:foreach(
        fun(M) -> meck:new(M, [non_strict, no_link]) end,
        [group_album_repo, group_ds, elib_oss]
    ),
    meck:expect(
        group_album_repo,
        find_album_by_album_id,
        fun(_) -> #{<<"group_id">> => 20} end
    ),
    meck:expect(
        group_album_repo,
        list_photos,
        fun(_, _, _, _) -> {ok, #{list => [photo()], total => 1}} end
    ),
    meck:expect(group_album_repo, find_photo_by_id, fun(_) -> photo() end),
    meck:expect(group_album_repo, is_liked, fun(_, _) -> false end),
    meck:expect(group_ds, is_member, fun(Uid, 20) -> Uid =:= 100 end),
    meck:expect(elib_oss, object_key_from_url, fun(_) -> <<"file_1_1/a.png">> end),
    meck:expect(
        elib_oss,
        presign_get_for_key,
        fun(<<"file_1_1/a.png">>, 600) -> <<"https://storage/signed?X-Amz-Signature=token">> end
    ).

cleanup(_) ->
    lists:foreach(fun meck:unload/1, [group_album_repo, group_ds, elib_oss]).

photo() ->
    #{
        <<"id">> => 1,
        <<"group_id">> => 20,
        <<"photo_id">> => <<"file_1_1">>,
        <<"photo_url">> => <<"http://storage/bucket/file_1_1/a.png">>,
        <<"thumbnail_url">> => <<"http://storage/bucket/file_1_1/a.png?thumbnail=1">>
    }.

member_list_signs_response() ->
    {ok, #{<<"items">> := [Photo]}} = group_album_ds:list_photos(<<"album">>, 100, 1, 10),
    assert_signed(Photo),
    ?assertEqual(1, meck:num_calls(elib_oss, presign_get_for_key, '_')).

member_detail_signs_response() ->
    {ok, Photo} = group_album_ds:get_photo_detail(1, 100),
    assert_signed(Photo),
    ?assertEqual(false, maps:get(<<"is_liked">>, Photo)).

assert_signed(Photo) ->
    ?assertEqual(
        <<"https://storage/signed?X-Amz-Signature=token">>, maps:get(<<"photo_url">>, Photo)
    ),
    ?assertEqual(maps:get(<<"photo_url">>, Photo), maps:get(<<"thumbnail_url">>, Photo)).

nonmember_list_has_no_signature() ->
    ?assertMatch({error, _}, group_album_ds:list_photos(<<"album">>, 101, 1, 10)),
    ?assertEqual(0, meck:num_calls(elib_oss, presign_get_for_key, '_')).

nonmember_detail_has_no_signature() ->
    ?assertMatch({error, _}, group_album_ds:get_photo_detail(1, 101)),
    ?assertEqual(0, meck:num_calls(elib_oss, presign_get_for_key, '_')).

legacy_url_preserved() ->
    meck:expect(elib_oss, object_key_from_url, fun(_) -> undefined end),
    {ok, Photo} = group_album_ds:get_photo_detail(1, 100),
    ?assertEqual(maps:get(<<"photo_url">>, photo()), maps:get(<<"photo_url">>, Photo)),
    ?assertEqual(0, meck:num_calls(elib_oss, presign_get_for_key, '_')).
