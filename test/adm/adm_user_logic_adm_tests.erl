-module(adm_user_logic_adm_tests).
-include_lib("eunit/include/eunit.hrl").
%% CI-00：原模块名 adm_*_tests 与 test/logic|test/repo 下同名模块冲突——erlang.mk 把
%% test/ 子目录平铺编译到 test/，同名 beam 互相覆盖导致本文件用例从不被执行。
%% 改名恢复发现执行（用例内容未改动，非 skip 非删除）。
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% adm_user_logic 模块的 EUnit 测试
%%%
%%% 目标：验证管理员用户逻辑功能
%%% 覆盖：用户查询、缓存机制
%%%===================================================================

%% ===================================================================
%% find/3 测试
%% ===================================================================

find_user_test_() ->
    ?WITH_MECKS(
        [
            {adm_user_repo, [
                {'find_by_id', 2, fun(_Uid, _Column) ->
                    #{
                        <<"id">> => 1,
                        <<"account">> => <<"admin@test.com">>,
                        <<"nickname">> => <<"Admin User">>
                    }
                end}
            ]},
            {imboy_cache, [
                {'memo', 3, fun(Fun, _Key, _Ttl) ->
                    Fun()
                end}
            ]}
        ],
        fun() ->
            Uid = 1,
            Column = <<"id">>,
            Result = adm_user_logic:find(Column, Uid, {adm_user, Column, Uid}),
            ?assertEqual(
                #{
                    <<"id">> => 1,
                    <<"account">> => <<"admin@test.com">>,
                    <<"nickname">> => <<"Admin User">>
                },
                Result
            )
        end
    ).
