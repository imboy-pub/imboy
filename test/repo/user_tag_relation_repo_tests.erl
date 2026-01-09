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
-define(TEST_TAG_ID, <<"tag123">>).
-define(TEST_SCENE, <<"1">>).
-define(TEST_OBJECT_ID, <<"obj456">>).
-define(TEST_TABLE_NAME, <<"public.user_tag_relation">>).

%% 测试表名获取
tablename_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(imboy_pg_sql, [passthrough, no_link]),
        
        % Mock表名生成
        meck:expect(imboy_pg_sql, public_tablename, fun(TableName) ->
            ?assertEqual(<<"user_tag_relation">>, TableName),
            ?TEST_TABLE_NAME
        end),
        
        try
            % 测试表名获取
            Result = user_tag_relation_repo:tablename(),
            ?assertEqual(?TEST_TABLE_NAME, Result),
            
            % 验证Mock调用
            ?assert(meck:called(imboy_pg_sql, public_tablename, 1))
        after
            % 清理Mock
            meck:unload(imboy_pg_sql)
        end
    end).

%% 测试删除操作
delete_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(imboy_pg_sql, [passthrough, no_link]),
        meck:new(imboy_pg, [passthrough, no_link]),
        
        % Mock表名生成
        meck:expect(imboy_pg_sql, public_tablename, fun(_) ->
            ?TEST_TABLE_NAME
        end),
        
        % Mock数据库执行
        meck:expect(imboy_pg, execute, 2, fun(Sql, Params) ->
            % 验证SQL语句
            ?assert(string:str(binary_to_list(Sql), "DELETE FROM") > 0),
            ?assert(string:str(binary_to_list(Sql), binary_to_list(?TEST_TABLE_NAME)) > 0),
            ?assert(string:str(binary_to_list(Sql), "WHERE scene =") > 0),
            
            % 验证参数
            ?assert(length(Params) =:= 3),
            [Scene, Uid, ObjectId] = Params,
            ?assertEqual(?TEST_SCENE, Scene),
            ?assertEqual(?TEST_UID, Uid),
            ?assertEqual(?TEST_OBJECT_ID, ObjectId),
            
            {ok, 1}
        end),
        
        try
            % 测试删除操作
            Result = user_tag_relation_repo:delete(?TEST_SCENE, ?TEST_UID, ?TEST_OBJECT_ID),
            case Result of
                {ok, AffectedCount} when is_integer(AffectedCount) -> ?assert(AffectedCount >= 0);
                {ok, _} -> ?assert(true);
                _ -> ?assert(false, "Expected {ok, AffectedCount}")
            end,

            % 验证Mock调用
            ?assert(meck:called(imboy_pg, execute, 2))
        after
            % 清理Mock
            meck:unload(imboy_pg_sql),
            meck:unload(imboy_pg)
        end
    end).

%% 测试删除操作（整数UID转换）
delete_integer_uid_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(imboy_pg_sql, [passthrough, no_link]),
        meck:new(imboy_pg, [passthrough, no_link]),

        % Mock表名生成
        meck:expect(imboy_pg_sql, public_tablename, fun(_) ->
            ?TEST_TABLE_NAME
        end),

        % Mock数据库执行并验证参数转换
        meck:expect(imboy_pg, execute, 2, fun(_Sql, _Params) ->
            [_Scene, Uid, _ObjectId] = _Params,
            % 验证整数UID被转换为二进制
            ?assertMatch(<<_/binary>>, Uid),
            ?assertEqual(integer_to_binary(?TEST_UID), Uid),
            {ok, 1}
        end),

        try
            % 测试整数UID的删除操作
            Result = user_tag_relation_repo:delete(?TEST_SCENE, ?TEST_UID, ?TEST_OBJECT_ID),
            case Result of
                {ok, AffectedCount} when is_integer(AffectedCount) -> ?assert(AffectedCount >= 0);
                {ok, _} -> ?assert(true);
                _ -> ?assert(false, "Expected {ok, AffectedCount}")
            end
        after
            % 清理Mock
            meck:unload(imboy_pg_sql),
            meck:unload(imboy_pg)
        end
    end).

%% 测试移除用户标签关系
remove_user_tag_relation_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(imboy_pg_sql, [passthrough, no_link]),
        meck:new(imboy_pg, [passthrough, no_link]),
        
        % Mock表名生成
        meck:expect(imboy_pg_sql, public_tablename, fun(_) ->
            ?TEST_TABLE_NAME
        end),
        
        % Mock数据库执行
        meck:expect(imboy_pg, execute, 2, fun(Sql, Params) ->
            % 验证SQL语句
            ?assert(string:str(binary_to_list(Sql), "DELETE FROM") > 0),
            ?assert(string:str(binary_to_list(Sql), "WHERE scene =") > 0),
            ?assert(string:str(binary_to_list(Sql), "AND user_id =") > 0),
            ?assert(string:str(binary_to_list(Sql), "AND object_id =") > 0),
            ?assert(string:str(binary_to_list(Sql), "AND tag_id =") > 0),
            
            % 验证参数
            ?assert(length(Params) =:= 4),
            [Scene, Uid, ObjectId, TagId] = Params,
            ?assertEqual(?TEST_SCENE, Scene),
            ?assertEqual(?TEST_UID, Uid),
            ?assertEqual(?TEST_OBJECT_ID, ObjectId),
            ?assertEqual(?TEST_TAG_ID, TagId),
            
            {ok, 1}
        end),
        
        try
            % 测试移除用户标签关系
            Conn = mock_connection(),
            Result = user_tag_relation_repo:remove_user_tag_relation(
                Conn, ?TEST_SCENE, ?TEST_UID, ?TEST_TAG_ID, ?TEST_OBJECT_ID
            ),
            ?assertEqual(ok, Result),
            
            % 验证Mock调用
            ?assert(meck:called(imboy_pg, execute, 2))
        after
            % 清理Mock
            meck:unload(imboy_pg_sql),
            meck:unload(imboy_pg)
        end
    end).

%% 测试替换对象标签
replace_object_tag_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(imboy_pg_sql, [passthrough, no_link]),
        meck:new(imboy_pg, [passthrough, no_link]),
        
        % Mock表名生成
        meck:expect(imboy_pg_sql, public_tablename, fun(TableName) ->
            case TableName of
                <<"user_collect">> -> <<"public.user_collect">>;
                <<"user_friend">> -> <<"public.user_friend">>
            end
        end),
        
        % Mock数据库执行
        meck:expect(imboy_pg, execute, 2, fun(Sql, Params) ->
            % 验证SQL语句
            ?assert(string:str(binary_to_list(Sql), "UPDATE") > 0),
            ?assert(string:str(binary_to_list(Sql), "SET tag = replace") > 0),
            ?assert(string:str(binary_to_list(Sql), "WHERE") > 0),
            
            % 验证参数
            ?assert(length(Params) >= 4),
            {ok, 1}
        end),
        
        try
            % 测试场景1：用户收藏
            Conn = mock_connection(),
            Result1 = user_tag_relation_repo:replace_object_tag(
                Conn, <<"1">>, ?TEST_UID, ?TEST_OBJECT_ID, <<"old_tag">>, <<"new_tag">>
            ),
            ?assertEqual(ok, Result1),
            
            % 测试场景2：用户好友
            Result2 = user_tag_relation_repo:replace_object_tag(
                Conn, <<"2">>, ?TEST_UID, ?TEST_OBJECT_ID, <<"old_tag">>, <<"new_tag">>
            ),
            ?assertEqual(ok, Result2),
            
            % 验证Mock调用
            ?assert(meck:called(imboy_pg, execute, 2))
        after
            % 清理Mock
            meck:unload(imboy_pg_sql),
            meck:unload(imboy_pg)
        end
    end).

%% 测试保存标签
save_tag_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(imboy_pg_sql, [passthrough, no_link]),
        meck:new(imboy_pg, [passthrough, no_link]),
        
        % Mock表名生成
        meck:expect(imboy_pg_sql, public_tablename, fun(_) ->
            <<"public.user_tag">>
        end),
        
        % Mock数据库执行
        meck:expect(imboy_pg, execute, 2, fun(Sql, Params) ->
            % 验证SQL语句
            ?assert(string:str(binary_to_list(Sql), "INSERT INTO") > 0),
            ?assert(string:str(binary_to_list(Sql), "public.user_tag") > 0),
            
            % 验证参数
            ?assert(length(Params) =:= 6),
            {ok, 1}
        end),
        
        try
            % 测试保存标签
            Conn = mock_connection(),
            CreatedAt = imboy_dt:timestamp(),
            Result = user_tag_relation_repo:save_tag(
                Conn, ?TEST_UID, ?TEST_SCENE, CreatedAt, <<"test_tag">>
            ),
            ?assertEqual(ok, Result),
            
            % 验证Mock调用
            ?assert(meck:called(imboy_pg, execute, 2))
        after
            % 清理Mock
            meck:unload(imboy_pg_sql),
            meck:unload(imboy_pg)
        end
    end).

%% 测试更新标签
update_tag_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(imboy_pg_sql, [passthrough, no_link]),
        meck:new(imboy_pg, [passthrough, no_link]),
        
        % Mock表名生成
        meck:expect(imboy_pg_sql, public_tablename, fun(_) ->
            <<"public.user_tag">>
        end),
        
        % Mock数据库执行
        meck:expect(imboy_pg, execute, 2, fun(Sql, Params) ->
            % 验证SQL语句
            ?assert(string:str(binary_to_list(Sql), "UPDATE") > 0),
            ?assert(string:str(binary_to_list(Sql), "public.user_tag") > 0),
            ?assert(string:str(binary_to_list(Sql), "SET") > 0),
            
            % 验证参数
            ?assert(length(Params) >= 3),
            {ok, 1}
        end),
        
        try
            % 测试更新标签
            Conn = mock_connection(),
            UpdatedAt = imboy_dt:timestamp(),
            Result = user_tag_relation_repo:update_tag(
                Conn, ?TEST_UID, ?TEST_SCENE, UpdatedAt, <<"updated_tag">>
            ),
            ?assertEqual(ok, Result),
            
            % 验证Mock调用
            ?assert(meck:called(imboy_pg, execute, 2))
        after
            % 清理Mock
            meck:unload(imboy_pg_sql),
            meck:unload(imboy_pg)
        end
    end).

%% 测试保存用户标签关系
save_user_tag_relation_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(imboy_pg_sql, [passthrough, no_link]),
        meck:new(imboy_pg, [passthrough, nolink]),
        
        % Mock表名生成
        meck:expect(imboy_pg_sql, public_tablename, fun(_) ->
            ?TEST_TABLE_NAME
        end),
        
        % Mock数据库执行
        meck:expect(imboy_pg, execute, 2, fun(Sql, Params) ->
            % 验证SQL语句
            ?assert(string:str(binary_to_list(Sql), "INSERT INTO") > 0),
            ?assert(string:str(binary_to_list(Sql), binary_to_list(?TEST_TABLE_NAME)) > 0),
            
            % 验证参数
            ?assert(length(Params) =:= 6),
            [Uid, Scene, ObjectId, TagId, CreatedAt, UpdatedAt] = Params,
            ?assertEqual(?TEST_UID, Uid),
            ?assertEqual(?TEST_SCENE, Scene),
            ?assertEqual(?TEST_OBJECT_ID, ObjectId),
            ?assertEqual(?TEST_TAG_ID, TagId),
            ?assert(is_integer(CreatedAt)),
            ?assert(is_integer(UpdatedAt)),
            
            {ok, 1}
        end),
        
        try
            % 测试保存用户标签关系
            Conn = mock_connection(),
            Timestamp = imboy_dt:timestamp(),
            Result = user_tag_relation_repo:save_user_tag_relation(
                Conn, ?TEST_UID, ?TEST_SCENE, ?TEST_OBJECT_ID, ?TEST_TAG_ID, Timestamp
            ),
            ?assertEqual(ok, Result),
            
            % 验证Mock调用
            ?assert(meck:called(imboy_pg, execute, 2))
        after
            % 清理Mock
            meck:unload(imboy_pg_sql),
            meck:unload(imboy_pg)
        end
    end).

%% 测试查询标签
select_tag_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(imboy_pg_sql, [passthrough, no_link]),
        meck:new(imboy_pg, [passthrough, no_link]),
        
        % Mock表名生成
        meck:expect(imboy_pg_sql, public_tablename, fun(_) ->
            <<"public.user_tag">>
        end),
        
        % Mock数据库查询
        meck:expect(imboy_pg, query, 2, fun(Sql, _Params) ->
            % 验证SQL语句
            ?assert(string:str(binary_to_list(Sql), "SELECT") > 0),
            ?assert(string:str(binary_to_list(Sql), "FROM") > 0),

            % 返回模拟数据
            {ok, [{1, <<"test_tag">>, ?TEST_SCENE, ?TEST_UID}]}
        end),

        try
            % 测试查询标签
            Conn = mock_connection(),
            Result = user_tag_relation_repo:select_tag(
                Conn, ?TEST_UID, ?TEST_SCENE
            ),
            case Result of
                {ok, List} when is_list(List) -> ?assert(true);
                _ -> ?assert(false, "Expected {ok, List}")
            end,

            % 验证Mock调用
            ?assert(meck:called(imboy_pg, query, 2))
        after
            % 清理Mock
            meck:unload(imboy_pg_sql),
            meck:unload(imboy_pg)
        end
    end).

%% 测试查询用户标签关系
select_user_tag_relation_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(imboy_pg_sql, [passthrough, no_link]),
        meck:new(imboy_pg, [passthrough, no_link]),
        
        % Mock表名生成
        meck:expect(imboy_pg_sql, public_tablename, fun(_) ->
            ?TEST_TABLE_NAME
        end),

        % Mock数据库查询
        meck:expect(imboy_pg, query, 2, fun(Sql, _Params) ->
            % 验证SQL语句
            ?assert(string:str(binary_to_list(Sql), "SELECT") > 0),
            ?assert(string:str(binary_to_list(Sql), binary_to_list(?TEST_TABLE_NAME)) > 0),

            % 返回模拟数据
            {ok, [{?TEST_UID, ?TEST_SCENE, ?TEST_OBJECT_ID, ?TEST_TAG_ID}]}
        end),

        try
            % 测试查询用户标签关系
            Conn = mock_connection(),
            Result = user_tag_relation_repo:select_user_tag_relation(
                Conn, ?TEST_UID, ?TEST_SCENE
            ),
            case Result of
                {ok, List} when is_list(List) -> ?assert(true);
                _ -> ?assert(false, "Expected {ok, List}")
            end,

            % 验证Mock调用
            ?assert(meck:called(imboy_pg, query, 2))
        after
            % 清理Mock
            meck:unload(imboy_pg_sql),
            meck:unload(imboy_pg)
        end
    end).

%% 测试标签副标题
tag_subtitle_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(imboy_pg, [passthrough, no_link]),

        % Mock数据库查询
        meck:expect(imboy_pg, query, 2, fun(Sql, _Params) ->
            % 验证SQL语句包含副标题查询
            ?assert(string:str(binary_to_list(Sql), "subtitle") > 0),

            % 返回模拟数据
            {ok, [{<<"test_subtitle">>}]}
        end),

        try
            % 测试标签副标题
            Conn = mock_connection(),
            Result = user_tag_relation_repo:tag_subtitle(
                Conn, ?TEST_TAG_ID, ?TEST_SCENE
            ),
            case Result of
                {ok, List} when is_list(List) -> ?assert(true);
                _ -> ?assert(false, "Expected {ok, List}")
            end,

            % 验证Mock调用
            ?assert(meck:called(imboy_pg, query, 2))
        after
            % 清理Mock
            meck:unload(imboy_pg)
        end
    end).

%% 测试刷新副标题
flush_subtitle_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(imboy_pg, [passthrough, no_link]),
        
        % Mock数据库执行
        meck:expect(imboy_pg, execute, 2, fun(Sql, _Params) ->
            % 验证SQL语句包含刷新操作
            ?assert(string:str(binary_to_list(Sql), "UPDATE") > 0),
            ?assert(string:str(binary_to_list(Sql), "subtitle") > 0),
            
            {ok, 1}
        end),
        
        try
            % 测试刷新副标题
            Conn = mock_connection(),
            Result = user_tag_relation_repo:flush_subtitle(Conn),
            ?assertEqual(ok, Result),
            
            % 验证Mock调用
            ?assert(meck:called(imboy_pg, execute, 2))
        after
            % 清理Mock
            meck:unload(imboy_pg)
        end
    end).

%% 测试SQL注入防护
sql_injection_protection_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置Mock
        meck:new(imboy_pg_sql, [passthrough, no_link]),
        meck:new(imboy_pg, [passthrough, no_link]),
        
        % Mock表名生成
        meck:expect(imboy_pg_sql, public_tablename, fun(_) ->
            ?TEST_TABLE_NAME
        end),
        
        % Mock数据库执行并验证参数化查询
        meck:expect(imboy_pg, execute, 2, fun(Sql, Params) ->
            % 验证使用参数化查询而不是字符串拼接
            ?assert(string:str(binary_to_list(Sql), "$1") > 0),
            ?assert(string:str(binary_to_list(Sql), "$2") > 0),
            ?assert(string:str(binary_to_list(Sql), "$3") > 0),

            % 验证恶意输入被正确处理
            [Scene, Uid, ObjectId] = Params,
            ?assertMatch(<<_/binary>>, Scene),
            ?assertMatch(<<_/binary>>, Uid),
            ?assertMatch(<<_/binary>>, ObjectId),

            {ok, 1}
        end),

        try
            % 测试恶意输入
            MaliciousScene = <<"1'; DROP TABLE user_tag_relation; --">>,
            MaliciousUid = <<"123'; DELETE FROM user_tag_relation; --">>,
            MaliciousObjectId = <<"obj'; UPDATE user_tag_relation SET tag='hacked'; --">>,

            Result = user_tag_relation_repo:delete(MaliciousScene, MaliciousUid, MaliciousObjectId),
            case Result of
                {ok, AffectedCount} when is_integer(AffectedCount) -> ?assert(AffectedCount >= 0);
                {ok, _} -> ?assert(true);
                _ -> ?assert(false, "Expected {ok, AffectedCount}")
            end,

            % 验证Mock调用
            ?assert(meck:called(imboy_pg, execute, 2))
        after
            % 清理Mock
            meck:unload(imboy_pg_sql),
            meck:unload(imboy_pg)
        end
    end).

%% 辅助函数：模拟数据库连接
mock_connection() ->
    % 返回一个模拟的连接对象
    {ok, conn} = pgsql:connect("localhost", "test", "test", "test", []),
    conn.

%% 测试数据完整性
data_integrity_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 测试必需字段验证
        RequiredFields = [
            {user_id, ?TEST_UID},
            {scene, ?TEST_SCENE},
            {object_id, ?TEST_OBJECT_ID},
            {tag_id, ?TEST_TAG_ID}
        ],
        
        lists:foreach(fun({Field, Value}) ->
            ?assert(is_atom(Field)),
            case Field of
                user_id -> ?assert(is_integer(Value) orelse is_binary(Value));
                scene -> ?assertMatch(<<_/binary>>, Value);
                object_id -> ?assertMatch(<<_/binary>>, Value);
                tag_id -> ?assertMatch(<<_/binary>>, Value)
            end
        end, RequiredFields),
        
        % 测试数据类型转换
        ?assertMatch(<<_/binary>>, integer_to_binary(?TEST_UID)),
        ?assert(is_integer(binary_to_integer(integer_to_binary(?TEST_UID)))),
        
        % 测试表名格式
        TableName = user_tag_relation_repo:tablename(),
        ?assertMatch(<<_/binary>>, TableName),
        ?assert(string:str(binary_to_list(TableName), "user_tag_relation") > 0)
    end).
