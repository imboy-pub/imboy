-module(app_ddl_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% app_ddl_repo 模块的 EUnit 测试
%%%
%%% 目标：验证 imboy_db → imboy_pg 迁移的语义正确性
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_public_prefix_test_() ->
    ?TEST_WITH_APP(fun() ->
        Result = app_ddl_repo:tablename(),
        ?assertEqual(<<"public.app_ddl">>, Result)
    end).

tablename_is_binary_test_() ->
    ?TEST_WITH_APP(fun() ->
        Result = app_ddl_repo:tablename(),
        ?assertMatch(<<_/binary>>, Result),
        ?assert(byte_size(Result) > 0),
        % 验证表名包含 app_ddl 字符串，符合命名规范
        ?assert(string:find(binary_to_list(Result), "app_ddl") =/= nomatch)
    end).


%% ===================================================================
%% add/1 测试
%% ===================================================================

add_valid_data_test_() ->
    ?TEST_WITH_DB(fun() ->
        Data = #{
            app_key => <<"test_app">>,
            version => <<"1.0.0">>,
            ddl_sql => <<"CREATE TABLE test (id INT);">>,
            status => 1,
            created_at => imboy_dt:now()
        },
        Result = app_ddl_repo:add(Data),
        ?ASSERT_OK(Result),
        {ok, InsertResult} = Result,
        % 验证返回的插入结果包含必要信息
        case InsertResult of
            InsertId when is_integer(InsertId) ->
                ?assert(InsertId > 0);
            InsertMap when is_map(InsertMap) ->
                #{<<"id">> := Id} = InsertMap,
                ?ASSERT_MATCH(#{<<"app_key">> := <<"test_app">>, <<"version">> := <<"1.0.0">>}, InsertMap),
                ?assert(Id > 0);
            _ ->
                ?assert(false, "Expected positive integer or map with id field")
        end
    end).

add_empty_map_test_() ->
    ?TEST_WITH_DB(fun() ->
        Data = #{},
        Result = app_ddl_repo:add(Data),
        case Result of
            {error, Reason} when is_atom(Reason); is_binary(Reason) ->
                ?assert(true);
            _ ->
                ?assert(false, "Expected {error, Reason}")
        end
    end).

add_with_required_fields_test_() ->
    ?TEST_WITH_DB(fun() ->
        Data = #{
            app_key => <<"minimal_app">>,
            version => <<"1.0">>,
            created_at => imboy_dt:now()
        },
        Result = app_ddl_repo:add(Data),
        ?ASSERT_OK(Result),
        {ok, InsertResult} = Result,
        % 验证最小字段集合也能成功插入
        case InsertResult of
            InsertId when is_integer(InsertId) ->
                ?assert(InsertId > 0);
            InsertMap when is_map(InsertMap) ->
                #{<<"id">> := Id} = InsertMap,
                ?ASSERT_MATCH(#{<<"app_key">> := <<"minimal_app">>, <<"version">> := <<"1.0">>}, InsertMap),
                ?assert(Id > 0);
            _ ->
                ?assert(false, "Expected positive integer or map with id field")
        end
    end).

add_with_timestamp_field_test_() ->
    ?TEST_WITH_DB(fun() ->
        Now = imboy_dt:now(),
        Data = #{
            app_key => <<"timestamp_test">>,
            version => <<"2.0">>,
            created_at => Now,
            updated_at => Now
        },
        Result = app_ddl_repo:add(Data),
        case Result of
            {ok, Ddl} -> 
                ?ASSERT_MATCH(#{<<"id">> := _, <<"app_key">> := <<"test_app">>, <<"version">> := <<"2.0">>}, Ddl);
            {error, Reason} -> 
                ?assert(is_atom(Reason), "Expected atom error reason");
            _ -> ?assert(false, "Unexpected result type")
        end
    end).

add_with_large_ddl_sql_test_() ->
    ?TEST_WITH_DB(fun() ->
        LargeSQL = list_to_binary(lists:duplicate(1000, $a)),
        Data = #{
            app_key => <<"large_sql_app">>,
            version => <<"1.0">>,
            ddl_sql => LargeSQL,
            created_at => imboy_dt:now()
        },
        Result = app_ddl_repo:add(Data),
        case Result of
            {ok, Ddl} -> 
                ?ASSERT_MATCH(#{<<"id">> := _, <<"app_key">> := <<"large_sql_app">>, <<"status">> := 0}, Ddl);
            {error, Reason} -> 
                ?assert(is_atom(Reason), "Expected atom error reason");
            _ -> ?assert(false, "Unexpected result type")
        end
    end).

add_with_status_zero_test_() ->
    ?TEST_WITH_DB(fun() ->
        Data = #{
            app_key => <<"status_zero_app">>,
            version => <<"1.0">>,
            status => 0,
            created_at => imboy_dt:now()
        },
        Result = app_ddl_repo:add(Data),
        case Result of
            {ok, Ddl} -> 
                ?ASSERT_MATCH(#{<<"id">> := _, <<"app_key">> := <<"status_zero_app">>, <<"status">> := 0}, Ddl);
            {error, Reason} -> 
                ?assert(is_atom(Reason), "Expected atom error reason");
            _ -> ?assert(false, "Unexpected result type")
        end
    end).

add_with_negative_status_test_() ->
    ?TEST_WITH_DB(fun() ->
        Data = #{
            app_key => <<"negative_status_app">>,
            version => <<"1.0">>,
            status => -1,
            created_at => imboy_dt:now()
        },
        Result = app_ddl_repo:add(Data),
        case Result of
            {ok, Ddl} -> 
                ?ASSERT_MATCH(#{<<"id">> := _, <<"app_key">> := <<"negative_status_app">>, <<"status">> := -1}, Ddl);
            {error, Reason} -> 
                ?assert(is_atom(Reason), "Expected atom error reason");
            _ -> ?assert(false, "Unexpected result type")
        end
    end).

%% ===================================================================
%% 边界和异常路径测试
%% ===================================================================

add_with_nil_app_key_test_() ->
    ?TEST_WITH_DB(fun() ->
        Data = #{
            app_key => <<>>,
            version => <<"1.0">>,
            created_at => imboy_dt:now()
        },
        Result = app_ddl_repo:add(Data),
        case Result of
            {error, Reason} when is_atom(Reason); is_binary(Reason) ->
                ?assert(true);
            _ ->
                ?assert(false, "Expected {error, Reason}")
        end
    end).

add_with_nil_version_test_() ->
    ?TEST_WITH_DB(fun() ->
        Data = #{
            app_key => <<"no_version_app">>,
            version => <<>>,
            created_at => imboy_dt:now()
        },
        Result = app_ddl_repo:add(Data),
        case Result of
            {error, Reason} when is_atom(Reason); is_binary(Reason) ->
                ?assert(true);
            _ ->
                ?assert(false, "Expected {error, Reason}")
        end
    end).
