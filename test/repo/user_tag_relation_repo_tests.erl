-module(user_tag_relation_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_tag_relation_repo 模块的 EUnit 测试
%%%
%%% 目标：验证用户标签关系数据访问层功能
%%% 覆盖：标签关系查询、添加、删除、更新
%%%===================================================================

%% 测试常量定义
-define(TEST_UID, 12345).
-define(TEST_TAG_ID, 999).
-define(TEST_SCENE, 1).
-define(TEST_OBJECT_ID, <<"obj456">>).

%% 辅助函数：模拟数据库连接
mock_connection() ->
    {connection, self()}.

%% 测试表名获取
tablename_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 1, fun(sql_driver) -> pgsql end}
            ]}
        ],
        fun() ->
            Result = user_tag_relation_repo:tablename(),
            ?assertEqual(<<"public.user_tag_relation">>, Result)
        end
    ).

%% 测试删除操作（二进制 Scene, 整数 Uid, 二进制 ObjectId）
delete_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_pg, [no_link]),
        meck:new(elib_log, [no_link]),

        meck:expect(elib_pg, execute, fun(_Sql, _Params) ->
            {ok, 1}
        end),
        meck:expect(elib_log, info, fun(_Fmt) -> ok end),
        meck:expect(elib_log, info, fun(_Fmt, _Args) -> ok end),

        try
            Result = user_tag_relation_repo:delete(<<"1">>, ?TEST_UID, ?TEST_OBJECT_ID),
            ?assertEqual({ok, 1}, Result),
            ?assert(meck:called(elib_pg, execute, 2))
        after
            meck:unload(elib_pg),
            meck:unload(elib_log)
        end
    end).

%% 测试删除操作（整数UID自动转换为二进制）
delete_integer_uid_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_pg, [no_link]),
        meck:new(elib_log, [no_link]),

        meck:expect(elib_pg, execute, fun(_Sql, Params) ->
            [_Scene, Uid, _ObjectId] = Params,
            % 验证整数 UID 被转换为二进制
            ?assertMatch(<<_/binary>>, Uid),
            ?assertEqual(integer_to_binary(?TEST_UID), Uid),
            {ok, 1}
        end),
        meck:expect(elib_log, info, fun(_Fmt) -> ok end),
        meck:expect(elib_log, info, fun(_Fmt, _Args) -> ok end),

        try
            Result = user_tag_relation_repo:delete(<<"1">>, ?TEST_UID, ?TEST_OBJECT_ID),
            ?assertMatch({ok, _}, Result)
        after
            meck:unload(elib_pg),
            meck:unload(elib_log)
        end
    end).

%% 测试删除操作（整数ObjectId自动转换为二进制）
delete_integer_object_id_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_pg, [no_link]),
        meck:new(elib_log, [no_link]),

        meck:expect(elib_pg, execute, fun(_Sql, Params) ->
            [_Scene, _Uid, ObjectId] = Params,
            ?assertMatch(<<_/binary>>, ObjectId),
            ?assertEqual(integer_to_binary(789), ObjectId),
            {ok, 1}
        end),
        meck:expect(elib_log, info, fun(_Fmt) -> ok end),
        meck:expect(elib_log, info, fun(_Fmt, _Args) -> ok end),

        try
            Result = user_tag_relation_repo:delete(<<"1">>, ?TEST_UID, 789),
            ?assertMatch({ok, _}, Result)
        after
            meck:unload(elib_pg),
            meck:unload(elib_log)
        end
    end).

%% 测试根据 tag_id 删除关联记录
delete_by_tag_id_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_pg, [no_link]),

        meck:expect(elib_pg, execute, fun(_Sql, Params) ->
            [TagId] = Params,
            ?assertEqual(?TEST_TAG_ID, TagId),
            {ok, 5}
        end),

        try
            Result = user_tag_relation_repo:delete_by_tag_id(?TEST_TAG_ID),
            ?assertMatch({ok, _}, Result),
            ?assert(meck:called(elib_pg, execute, 2))
        after
            meck:unload(elib_pg)
        end
    end).

%% 测试移除用户标签关系
remove_user_tag_relation_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_pg, [no_link]),
        meck:new(elib_log, [no_link]),

        meck:expect(elib_pg, execute, fun(_Conn, _Sql, _Params) ->
            {ok, 1}
        end),
        meck:expect(elib_log, info, fun(_Fmt) -> ok end),
        meck:expect(elib_log, info, fun(_Fmt, _Args) -> ok end),

        try
            Conn = mock_connection(),
            Result = user_tag_relation_repo:remove_user_tag_relation(
                Conn, <<"1">>, ?TEST_UID, ?TEST_TAG_ID, ?TEST_OBJECT_ID
            ),
            ?assertEqual(ok, Result),
            ?assert(meck:called(elib_pg, execute, 3))
        after
            meck:unload(elib_pg),
            meck:unload(elib_log)
        end
    end).

%% 测试替换对象标签（场景1：用户收藏）
replace_object_tag_scene1_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_pg, [no_link]),
        meck:new(elib_log, [no_link]),

        meck:expect(elib_pg, execute, fun(_Conn, _Sql, _Params) ->
            {ok, 1}
        end),
        meck:expect(elib_log, error, fun(_Fmt) -> ok end),
        meck:expect(elib_log, error, fun(_Fmt, _Args) -> ok end),

        try
            Conn = mock_connection(),
            Result = user_tag_relation_repo:replace_object_tag(
                Conn, 1, ?TEST_UID, ?TEST_OBJECT_ID, "old_tag", "new_tag"
            ),
            ?assertEqual(ok, Result),
            ?assert(meck:called(elib_pg, execute, 3))
        after
            meck:unload(elib_pg),
            meck:unload(elib_log)
        end
    end).

%% 测试替换对象标签（场景2：用户好友）
%% to_user_id 为 bigint：ObjectId 必须传 integer（binary 编码致 int8
%% integer_overflow 崩连接，生产 500 实证）。logic 层 remove/set 已保证
%% 传 integer，此处断言 Params 全 integer 做回归保护。
replace_object_tag_scene2_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_pg, [no_link]),
        meck:new(elib_log, [no_link]),

        meck:expect(elib_pg, execute, fun(_Conn, _Sql, Params) ->
            % Params = [Uid, ObjectId, FromName++",", ToName++","]，仅前两个
            % 是 int8 参数位；FromName/ToName 本就是 list（text 参数）
            [Uid, ObjId | _] = Params,
            true = is_integer(Uid),
            true = is_integer(ObjId),
            {ok, 1}
        end),
        meck:expect(elib_log, error, fun(_Fmt) -> ok end),
        meck:expect(elib_log, error, fun(_Fmt, _Args) -> ok end),

        try
            Conn = mock_connection(),
            Result = user_tag_relation_repo:replace_object_tag(
                Conn, 2, ?TEST_UID, 99999, "old_tag", "new_tag"
            ),
            ?assertEqual(ok, Result),
            ?assert(meck:called(elib_pg, execute, 3))
        after
            meck:unload(elib_pg),
            meck:unload(elib_log)
        end
    end).

%% 测试替换对象标签（整数ObjectId自动转换）
replace_object_tag_int_objectid_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_pg, [no_link]),
        meck:new(elib_log, [no_link]),

        % integer ObjectId 必须直通（不转 binary）：execute 推断 int8，
        % binary 编码致 integer_overflow 崩连接（生产 500 实证）
        meck:expect(elib_pg, execute, fun(_Conn, _Sql, Params) ->
            [Uid, ObjId | _] = Params,
            true = is_integer(Uid),
            true = is_integer(ObjId),
            {ok, 1}
        end),
        meck:expect(elib_log, error, fun(_Fmt) -> ok end),
        meck:expect(elib_log, error, fun(_Fmt, _Args) -> ok end),

        try
            Conn = mock_connection(),
            Result = user_tag_relation_repo:replace_object_tag(
                Conn, 1, ?TEST_UID, 99999, "old", "new"
            ),
            ?assertEqual(ok, Result)
        after
            meck:unload(elib_pg),
            meck:unload(elib_log)
        end
    end).

%% 测试保存标签（成功路径）
save_tag_success_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_pg, [no_link]),
        meck:new(elib_tsid, [no_link]),

        meck:expect(elib_tsid, generate, fun(user_tag) -> 10001 end),
        meck:expect(elib_pg, execute, fun(_Conn, _Sql, _Params) ->
            {ok, 1, [{10001}]}
        end),

        try
            Conn = mock_connection(),
            CreatedAt = 1700000000,
            Result = user_tag_relation_repo:save_tag(
                Conn, ?TEST_UID, ?TEST_SCENE, CreatedAt, <<"test_tag">>
            ),
            ?assertEqual({10001, <<"test_tag">>}, Result),
            ?assert(meck:called(elib_pg, execute, 3))
        after
            meck:unload(elib_pg),
            meck:unload(elib_tsid)
        end
    end).

%% 测试保存标签（冲突更新路径）
save_tag_conflict_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_pg, [no_link]),
        meck:new(elib_tsid, [no_link]),

        meck:expect(elib_tsid, generate, fun(user_tag) -> 10002 end),
        meck:expect(elib_pg, execute, fun(_Conn, _Sql, _Params) ->
            {ok, 1, [{500}]}
        end),

        try
            Conn = mock_connection(),
            CreatedAt = 1700000000,
            Result = user_tag_relation_repo:save_tag(
                Conn, ?TEST_UID, ?TEST_SCENE, CreatedAt, <<"existing_tag">>
            ),
            ?assertEqual({500, <<"existing_tag">>}, Result)
        after
            meck:unload(elib_pg),
            meck:unload(elib_tsid)
        end
    end).

%% 测试保存标签（失败路径）
save_tag_error_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_pg, [no_link]),
        meck:new(elib_tsid, [no_link]),

        meck:expect(elib_tsid, generate, fun(user_tag) -> 10003 end),
        meck:expect(elib_pg, execute, fun(_Conn, _Sql, _Params) ->
            {error, unique_violation}
        end),

        try
            Conn = mock_connection(),
            CreatedAt = 1700000000,
            Result = user_tag_relation_repo:save_tag(
                Conn, ?TEST_UID, ?TEST_SCENE, CreatedAt, <<"bad_tag">>
            ),
            ?assertEqual({0, <<"bad_tag">>}, Result)
        after
            meck:unload(elib_pg),
            meck:unload(elib_tsid)
        end
    end).

%% 测试更新标签（成功路径）
update_tag_success_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_pg, [no_link]),

        meck:expect(elib_pg, execute, fun(_Conn, _Sql, _Params) ->
            {ok, 1}
        end),

        try
            Conn = mock_connection(),
            CreatedAt = 1700000000,
            Result = user_tag_relation_repo:update_tag(
                Conn, ?TEST_TAG_ID, <<"updated_tag">>, ?TEST_UID, CreatedAt
            ),
            ?assertEqual({?TEST_TAG_ID, <<"updated_tag">>}, Result),
            ?assert(meck:called(elib_pg, execute, 3))
        after
            meck:unload(elib_pg)
        end
    end).

%% 测试更新标签（未找到记录）
update_tag_not_found_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_pg, [no_link]),

        meck:expect(elib_pg, execute, fun(_Conn, _Sql, _Params) ->
            {ok, 0}
        end),

        try
            Conn = mock_connection(),
            CreatedAt = 1700000000,
            Result = user_tag_relation_repo:update_tag(
                Conn, ?TEST_TAG_ID, <<"updated_tag">>, ?TEST_UID, CreatedAt
            ),
            ?assertEqual({0, <<"updated_tag">>}, Result)
        after
            meck:unload(elib_pg)
        end
    end).

%% 测试更新标签（数据库错误）
update_tag_error_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_pg, [no_link]),

        meck:expect(elib_pg, execute, fun(_Conn, _Sql, _Params) ->
            {error, connection_lost}
        end),

        try
            Conn = mock_connection(),
            CreatedAt = 1700000000,
            Result = user_tag_relation_repo:update_tag(
                Conn, ?TEST_TAG_ID, <<"updated_tag">>, ?TEST_UID, CreatedAt
            ),
            ?assertEqual({0, <<"updated_tag">>}, Result)
        after
            meck:unload(elib_pg)
        end
    end).

%% 测试保存用户标签关系（成功路径）
save_user_tag_relation_success_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_pg, [no_link]),
        meck:new(elib_tsid, [no_link]),

        meck:expect(elib_tsid, generate, fun(user_tag_relation) -> 20001 end),
        meck:expect(elib_pg, execute, fun(_Conn, _Sql, _Params) ->
            {ok, 1, [{20001}]}
        end),

        try
            Conn = mock_connection(),
            CreatedAt = 1700000000,
            Result = user_tag_relation_repo:save_user_tag_relation(
                Conn, ?TEST_SCENE, ?TEST_UID, ?TEST_TAG_ID, ?TEST_OBJECT_ID, CreatedAt
            ),
            ?assertEqual(20001, Result),
            ?assert(meck:called(elib_pg, execute, 3))
        after
            meck:unload(elib_pg),
            meck:unload(elib_tsid)
        end
    end).

%% 测试保存用户标签关系（失败路径）
save_user_tag_relation_error_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_pg, [no_link]),
        meck:new(elib_tsid, [no_link]),

        meck:expect(elib_tsid, generate, fun(user_tag_relation) -> 20002 end),
        meck:expect(elib_pg, execute, fun(_Conn, _Sql, _Params) ->
            {error, unique_violation}
        end),

        try
            Conn = mock_connection(),
            CreatedAt = 1700000000,
            Result = user_tag_relation_repo:save_user_tag_relation(
                Conn, ?TEST_SCENE, ?TEST_UID, ?TEST_TAG_ID, ?TEST_OBJECT_ID, CreatedAt
            ),
            ?assertEqual(0, Result)
        after
            meck:unload(elib_pg),
            meck:unload(elib_tsid)
        end
    end).

%% 测试查询标签
select_tag_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_pg, [no_link]),

        meck:expect(elib_pg, query, fun(_Sql, _Params) ->
            {ok, [{1, <<"test_tag">>}, {2, <<"another_tag">>}]}
        end),

        try
            Result = user_tag_relation_repo:select_tag(
                <<"scene = $1">>, [?TEST_SCENE], <<"id, name">>
            ),
            ?assertMatch({ok, [_ | _]}, Result),

            {ok, Rows} = Result,
            ?assertEqual(2, length(Rows))
        after
            meck:unload(elib_pg)
        end
    end).

%% 测试查询用户标签关系
select_user_tag_relation_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_pg, [no_link]),
        meck:new(elib_log, [no_link]),

        meck:expect(elib_pg, query, fun(_Sql, _Params) ->
            {ok, [{1, ?TEST_UID, ?TEST_SCENE, ?TEST_OBJECT_ID, ?TEST_TAG_ID}]}
        end),
        meck:expect(elib_log, info, fun(_Fmt) -> ok end),
        meck:expect(elib_log, info, fun(_Fmt, _Args) -> ok end),

        try
            Result = user_tag_relation_repo:select_user_tag_relation(
                <<"user_id = $1 AND scene = $2">>, [?TEST_UID, ?TEST_SCENE], <<"*">>
            ),
            ?assertMatch({ok, [_ | _]}, Result),

            {ok, Rows} = Result,
            ?assertEqual(1, length(Rows))
        after
            meck:unload(elib_pg),
            meck:unload(elib_log)
        end
    end).

%% 测试标签副标题（场景2，有数据）
tag_subtitle_scene2_with_data_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_pg, [no_link]),
        meck:new(imboy_cache, [no_link]),

        MemoFun = fun(Fun, _Key, _TTL) -> Fun() end,
        meck:expect(imboy_cache, memo, MemoFun),

        meck:expect(elib_pg, query, fun(_Sql, _Params) ->
            {ok, [
                #{<<"subtitle">> => <<"Alice">>},
                #{<<"subtitle">> => <<"Bob">>}
            ]}
        end),

        try
            Result = user_tag_relation_repo:tag_subtitle(2, ?TEST_TAG_ID, 5),
            ?assert(is_binary(Result)),
            ?assert(string:str(binary_to_list(Result), "Alice") > 0)
        after
            meck:unload(elib_pg),
            meck:unload(imboy_cache)
        end
    end).

%% 测试标签副标题（场景2，无数据）
tag_subtitle_scene2_no_data_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_pg, [no_link]),
        meck:new(imboy_cache, [no_link]),

        MemoFun = fun(Fun, _Key, _TTL) -> Fun() end,
        meck:expect(imboy_cache, memo, MemoFun),

        meck:expect(elib_pg, query, fun(_Sql, _Params) ->
            {ok, []}
        end),

        try
            Result = user_tag_relation_repo:tag_subtitle(2, ?TEST_TAG_ID, 0),
            ?assertEqual(<<>>, Result)
        after
            meck:unload(elib_pg),
            meck:unload(imboy_cache)
        end
    end).

%% 测试标签副标题（场景1，直接返回空）
tag_subtitle_scene1_test_() ->
    ?TEST_WITH_APP(fun() ->
        Result = user_tag_relation_repo:tag_subtitle(1, ?TEST_TAG_ID, 10),
        ?assertEqual(<<>>, Result)
    end).

%% 测试刷新副标题
flush_subtitle_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(imboy_cache, [no_link]),

        meck:expect(imboy_cache, flush, fun(_Key) -> ok end),

        try
            Result = user_tag_relation_repo:flush_subtitle(?TEST_TAG_ID),
            ?assertEqual(ok, Result),
            ?assert(meck:called(imboy_cache, flush, 1))
        after
            meck:unload(imboy_cache)
        end
    end).

%% 测试SQL注入防护
sql_injection_protection_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(elib_pg, [no_link]),
        meck:new(elib_log, [no_link]),

        meck:expect(elib_pg, execute, fun(Sql, Params) ->
            SqlStr = binary_to_list(Sql),
            ?assert(string:str(SqlStr, "$1") > 0),
            ?assert(string:str(SqlStr, "$2") > 0),
            ?assert(string:str(SqlStr, "$3") > 0),
            [Scene, Uid, ObjectId] = Params,
            ?assertMatch(<<_/binary>>, Scene),
            ?assertMatch(<<_/binary>>, Uid),
            ?assertMatch(<<_/binary>>, ObjectId),
            {ok, 1}
        end),
        meck:expect(elib_log, info, fun(_Fmt) -> ok end),
        meck:expect(elib_log, info, fun(_Fmt, _Args) -> ok end),

        try
            MaliciousScene = <<"1'; DROP TABLE user_tag_relation; --">>,
            MaliciousUid = 12345,
            MaliciousObjectId = <<"obj'; UPDATE user_tag_relation SET tag='hacked'; --">>,

            Result = user_tag_relation_repo:delete(
                MaliciousScene, MaliciousUid, MaliciousObjectId
            ),
            ?assertMatch({ok, _}, Result),
            ?assert(meck:called(elib_pg, execute, 2))
        after
            meck:unload(elib_pg),
            meck:unload(elib_log)
        end
    end).
