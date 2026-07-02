-module(group_album_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% @doc 删除相册：创建者可删除
group_album_logic_test_() ->
    {inorder, [
        delete_album_by_creator_case(),
        delete_album_by_admin_case(),
        delete_album_permission_denied_case(),
        delete_album_not_found_case()
    ]}.

delete_album_by_creator_test_() ->
    delete_album_by_creator_case().

delete_album_by_admin_test_() ->
    delete_album_by_admin_case().

delete_album_permission_denied_test_() ->
    delete_album_permission_denied_case().

delete_album_not_found_test_() ->
    delete_album_not_found_case().

delete_album_by_creator_case() ->
    ?WITH_MECKS(
        [
            {group_album_repo, [
                {'find_album_by_album_id', 1, fun(<<"album_1">>) ->
                    #{
                        <<"id">> => 11,
                        <<"group_id">> => 20,
                        <<"creator_id">> => 100
                    }
                end},
                {'delete_album', 1, fun(11) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, group_album_logic:delete_album(<<"album_1">>, 100))
        end
    ).

%% @doc 删除相册：管理员可删除
delete_album_by_admin_case() ->
    ?WITH_MECKS(
        [
            {group_album_repo, [
                {'find_album_by_album_id', 1, fun(<<"album_2">>) ->
                    #{
                        <<"id">> => 12,
                        <<"group_id">> => 21,
                        <<"creator_id">> => 101
                    }
                end},
                {'delete_album', 1, fun(12) -> {ok, 1} end}
            ]},
            {group_member_repo, [
                {'find', 3, fun(21, 100, <<"role">>) ->
                    #{<<"role">> => 3}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, group_album_logic:delete_album(<<"album_2">>, 100))
        end
    ).

%% @doc 删除相册：权限不足
delete_album_permission_denied_case() ->
    ?WITH_MECKS(
        [
            {group_album_repo, [
                {'find_album_by_album_id', 1, fun(<<"album_3">>) ->
                    #{
                        <<"id">> => 13,
                        <<"group_id">> => 22,
                        <<"creator_id">> => 101
                    }
                end}
            ]},
            {group_member_repo, [
                {'find', 3, fun(22, 100, <<"role">>) ->
                    #{<<"role">> => 1}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"相册权限不足"/utf8>>},
                group_album_logic:delete_album(<<"album_3">>, 100)
            )
        end
    ).

%% @doc 删除相册：相册不存在
delete_album_not_found_case() ->
    ?WITH_MECK(
        group_album_repo,
        [
            {'find_album_by_album_id', 1, fun(<<"album_4">>) -> #{} end}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"相册不存在"/utf8>>},
                group_album_logic:delete_album(<<"album_4">>, 100)
            )
        end
    ).
