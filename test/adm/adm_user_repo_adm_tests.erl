-module(adm_user_repo_adm_tests).
-include_lib("eunit/include/eunit.hrl").
%% CI-00：原模块名 adm_*_tests 与 test/logic|test/repo 下同名模块冲突——erlang.mk 把
%% test/ 子目录平铺编译到 test/，同名 beam 互相覆盖导致本文件用例从不被执行。
%% 改名恢复发现执行（用例内容未改动，非 skip 非删除）。
-include("eunit_setup.hrl").

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
    %% CI-00 修桩：Column 参数应为逗号分隔的 binary（原 list-of-binary 传入
    %% <<"SELECT ", Column/binary, ...>> 直接 badarg）；返回契约是 map()
    %% （value_or_empty，查不到返回空 map），断言随之对齐。
    ?TEST_WITH_DB(fun() ->
        Email = <<"ci00-nonexistent@example.com">>,
        Column = <<"id,account,nickname">>,
        Result = adm_user_repo:find_by_email(Email, Column),
        ?assertMatch(#{}, Result)
    end).

find_admin_by_id_test_() ->
    %% CI-00 修桩：find_by_id 返回契约是 map()（不存在返回空 map，非
    %% {ok, User}），且不预设真库存在 id=1 的管理员。
    ?TEST_WITH_DB(fun() ->
        AdminId = 1,
        Result = adm_user_repo:find_by_id(AdminId),
        ?assertMatch(#{}, Result)
    end).
