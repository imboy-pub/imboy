-module(adm_user_logic_tests).
-include_lib("eunit/include/eunit.hrl").
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
    ?WITH_MECKS([
        {adm_user_repo, [
            {'find_by_id', 2, fun(_Uid, _Column) ->
                #{<<"id">> => 1, <<"account">> => <<"admin@test.com">>, <<"nickname">> => <<"Admin User">>}
            end}
        ]},
        {imboy_cache, [
            {'memo', 3, fun(Fun, _Key, _Ttl) ->
                Fun()
            end}
        ]}
    ], fun() ->
        Uid = 1,
        Column = <<"id">>,
        Result = adm_user_logic:find(Column, Uid, {adm_user, Column, Uid}),
        ?assertEqual(#{<<"id">> => 1, <<"account">> => <<"admin@test.com">>, <<"nickname">> => <<"Admin User">>}, Result)
    end).
