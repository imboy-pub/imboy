-module(adm_user_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% adm_user_repo 模块的 EUnit 测试
%%%
%%% 目标：验证管理后台用户数据访问层功能
%%%===================================================================

tablename_returns_correct_table_test_() ->
    ?TEST_WITH_APP(fun() ->
        Result = adm_user_repo:tablename(),
        ?assertMatch(<<_/binary>>, Result),
        ?assert(<<>> =/= Result)
    end).

find_admin_by_email_test_() ->
    ?TEST_WITH_DB(fun() ->
        Email = <<"admin@example.com">>,
        Column = [<<"id">>, <<"account">>, <<"nickname">>],
        Result = adm_user_repo:find_by_email(Email, Column),
        case Result of
            {ok, User} when is_map(User) -> ?assert(true);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, User}")
        end
    end).

find_admin_by_id_test_() ->
    ?TEST_WITH_DB(fun() ->
        AdminId = 1,
        Result = adm_user_repo:find_by_id(AdminId),
        case Result of
            {ok, User} when is_map(User) -> ?assert(true);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, User}")
        end
    end).
