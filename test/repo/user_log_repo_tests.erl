-module(user_log_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_log_repo 模块的 EUnit 测试
%%%
%%% 目标：验证用户日志数据访问层功能
%%% 覆盖：日志查询、创建
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_table_test_() ->
    ?TEST_WITH_APP(fun() ->
        Result = user_log_repo:tablename(),
        ?assertMatch(<<_/binary>>, Result),
        ?assert(<<>> =/= Result)
    end).

%% ===================================================================
%% 日志查询测试
%% ===================================================================

%% user_log_repo 仅导出 add/1 和 add/2，无查询函数

add_log_test_() ->
    ?WITH_MECKS([
        {elib_pg_sql, [
            {'public_tablename', 1, fun(_Table) -> <<"public.user_log">> end},
            {'insert', 2, fun(Table, Data) ->
                ?assertEqual(<<"public.user_log">>, Table),
                ?assertEqual(#{
                    uid => 1,
                    action => <<"login">>,
                    detail => <<"User logged in from iOS">>
                }, Data),
                {<<"INSERT INTO public.user_log (uid,action,detail) VALUES ($1,$2,$3)">>,
                 [1, <<"login">>, <<"User logged in from iOS">>]}
            end}
        ]},
        {elib_pg, [
            {'with_tx', 1, fun(Fun) ->
                ?assert(is_function(Fun, 1)),
                Fun(mock_conn)
            end},
            {'execute', 3, fun(mock_conn, Sql, Params) ->
                SqlBin = iolist_to_binary(Sql),
                ?assert(binary:match(SqlBin, <<"INSERT INTO public.user_log">>) =/= nomatch),
                ?assertEqual([1, <<"login">>, <<"User logged in from iOS">>], Params),
                {ok, 1}
            end}
        ]}
    ], fun() ->
        Data = #{
            uid => 1,
            action => <<"login">>,
            detail => <<"User logged in from iOS">>
        },
        Result = user_log_repo:add(Data),
        ?assertEqual({ok, 1}, Result)
    end).
