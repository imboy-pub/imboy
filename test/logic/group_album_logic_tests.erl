-module(group_album_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

batch_upload_results_test_() ->
    [?_test(batch_upload_results_case(Mode)) || Mode <- [success, mixed, denied, archived]].

batch_upload_results_case(Mode) ->
    Modules = [group_ds, workspace_guard, elib_oss, group_album_repo, elib_dt],
    lists:foreach(fun(Module) -> meck:new(Module, [non_strict]) end, Modules),
    try
        meck:expect(group_ds, is_member, fun(100, 20) -> Mode =/= denied end),
        meck:expect(workspace_guard, ensure_writable, fun({group, 20}) ->
            case Mode of
                archived -> {error, ?ERR_WORKSPACE_ARCHIVED};
                _ -> ok
            end
        end),
        meck:expect(workspace_guard, write_tx, fun({group, 20}, Callback) -> Callback(conn) end),
        meck:expect(elib_oss, upload, fun(Data, Name, #{mime_type := <<"image/jpeg">>}) ->
            case {Mode, Data} of
                {mixed, <<"second">>} -> {error, upload_failed};
                _ -> {ok, <<"https://example.invalid/photo.jpg">>, Name}
            end
        end),
        meck:expect(group_album_repo, insert_photo_tx, fun(conn, #{album_id := <<"album">>}) ->
            {ok, 42}
        end),
        meck:expect(group_album_repo, find_album_by_album_id, fun(<<"album">>) ->
            %% review P2 相册归属校验读取 group_id（gid=20 本群）
            #{<<"id">> => 11, <<"group_id">> => 20}
        end),
        meck:expect(group_album_repo, increment_photo_count_tx, fun(conn, 11) -> {ok, 1} end),
        meck:expect(elib_dt, timestamp, fun() -> 123 end),
        Photos = [
            {<<"album">>, <<"first">>, <<"first.jpg">>},
            {<<"album">>, <<"second">>, <<"second.jpg">>}
        ],
        {ok, Results} = group_album_logic:batch_upload_photos(<<"20">>, 100, Photos),
        ?assertEqual(2, length(Results)),
        batch_upload_results_assert(Mode, Results)
    after
        lists:foreach(fun meck:unload/1, Modules)
    end.

batch_upload_results_assert(success, [{ok, First}, {ok, Second}]) ->
    ?assertMatch(
        #{<<"id">> := 42, <<"gid">> := <<"20">>, <<"photo_id">> := <<"first.jpg">>}, First
    ),
    ?assertMatch(
        #{<<"id">> := 42, <<"gid">> := <<"20">>, <<"photo_id">> := <<"second.jpg">>}, Second
    );
batch_upload_results_assert(mixed, [{ok, First}, {error, upload_failed}]) ->
    ?assertMatch(#{<<"id">> := 42, <<"gid">> := <<"20">>}, First);
batch_upload_results_assert(denied, Results) ->
    ?assertEqual([{error, <<"非群组成员"/utf8>>}, {error, <<"非群组成员"/utf8>>}], Results),
    ?assertEqual([], meck:history(workspace_guard)),
    ?assertEqual([], meck:history(elib_oss));
batch_upload_results_assert(archived, Results) ->
    ?assertEqual([{error, ?ERR_WORKSPACE_ARCHIVED}, {error, ?ERR_WORKSPACE_ARCHIVED}], Results),
    ?assertEqual([], meck:history(elib_oss)),
    ?assertEqual([], meck:history(group_album_repo)).

photo_id_authorization_test_() ->
    [
        ?_test(photo_id_authorization_case(Action, Member))
     || Action <- [like_photo, unlike_photo, add_comment, list_comments], Member <- [false, true]
    ].

photo_id_authorization_case(Action, Member) ->
    Modules = [group_album_repo, group_ds, workspace_guard],
    lists:foreach(fun(Module) -> meck:new(Module, [non_strict]) end, Modules),
    try
        meck:expect(group_album_repo, find_photo_by_id, fun(42) ->
            #{<<"id">> => 42, <<"group_id">> => 20}
        end),
        meck:expect(group_ds, is_member, fun(100, 20) -> Member end),
        meck:expect(group_album_repo, is_liked, fun(42, 100) -> false end),
        meck:expect(group_album_repo, list_comments, fun(42, 20) -> {ok, []} end),
        meck:expect(workspace_guard, write_tx, fun({group_album_photo, Id}, _) ->
            ?assertEqual(42, Id),
            {error, {?ERR_WORKSPACE_ARCHIVED, <<"archived">>}}
        end),
        Actual =
            case Action of
                add_comment -> group_album_logic:add_comment(42, 100, <<"comment">>);
                list_comments -> group_album_logic:list_comments(42, 100, 20);
                _ -> apply(group_album_logic, Action, [42, 100])
            end,
        case Member of
            false ->
                ?assertEqual({error, <<"你不是该群成员"/utf8>>}, Actual),
                ?assertEqual([], meck:history(workspace_guard)),
                ?assertEqual(0, meck:num_calls(group_album_repo, list_comments, '_'));
            true when Action =:= list_comments ->
                ?assertEqual({ok, []}, Actual),
                ?assertEqual(1, meck:num_calls(group_album_repo, list_comments, '_'));
            true ->
                ?assertEqual({error, ?ERR_WORKSPACE_ARCHIVED}, Actual),
                ?assertEqual(1, meck:num_calls(workspace_guard, write_tx, '_'))
        end
    after
        lists:foreach(fun meck:unload/1, Modules)
    end.

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
                {'delete_album_tx', 2, fun(_Conn, 11) -> {ok, 1} end}
            ]},
            %% T7 归档写守卫收口适配：personal 直通 + with_tx 直跑
            {workspace_resolver, [{'resolve_workspace', 1, fun(_) -> personal end}]},
            {elib_pg, [{'with_tx', 1, fun(TxFun) -> TxFun(fake_conn) end}]}
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
                {'delete_album_tx', 2, fun(_Conn, 12) -> {ok, 1} end}
            ]},
            {group_member_repo, [
                {'find', 3, fun(21, 100, <<"role">>) ->
                    #{<<"role">> => 3}
                end}
            ]},
            %% T7 归档写守卫收口适配：personal 直通 + with_tx 直跑
            {workspace_resolver, [{'resolve_workspace', 1, fun(_) -> personal end}]},
            {elib_pg, [{'with_tx', 1, fun(TxFun) -> TxFun(fake_conn) end}]}
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

%% 回归（IDOR）：list_comments/3 非该图片所属群成员 → 拒绝，
%% 防止任意登录用户传任意 photo_id 跨群窥探评论内容
list_comments_non_member_rejected_test_() ->
    ?WITH_MECKS(
        [
            {group_album_repo, [
                {'find_photo_by_id', 1, fun(1) ->
                    #{<<"id">> => 1, <<"group_id">> => 30}
                end}
            ]},
            {group_member_repo, [
                {'find', 3, fun(30, 999, <<"id">>) -> #{} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"你不是该群成员"/utf8>>},
                group_album_logic:list_comments(1, 999, 20)
            )
        end
    ).

%% 正常路径：list_comments/3 群成员可正常查询评论
list_comments_member_success_test_() ->
    ?WITH_MECKS(
        [
            {group_album_repo, [
                {'find_photo_by_id', 1, fun(2) ->
                    #{<<"id">> => 2, <<"group_id">> => 31}
                end},
                {'list_comments', 2, fun(2, 20) ->
                    {ok, [#{<<"id">> => 1, <<"content">> => <<"nice"/utf8>>}]}
                end}
            ]},
            {group_member_repo, [
                {'find', 3, fun(31, 100, <<"id">>) -> #{<<"id">> => 1} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, [#{<<"id">> => 1, <<"content">> => <<"nice"/utf8>>}]},
                group_album_logic:list_comments(2, 100, 20)
            )
        end
    ).
