-module(wallet_repo_page_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% @doc wallet_repo:page/3 运营分页查询测试
%%% 验证：透传 WhereMap、固定列、id desc 排序，经 elib_pg:page_with_total/6。

page_passes_wheremap_and_columns_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'page_with_total', 6, fun(Table, Column, WhereMap, Order, Page, Size) ->
                ?assert(is_binary(Table)),
                %% 列包含钱包关键字段
                ?assertNotEqual(nomatch, binary:match(Column, <<"balance">>)),
                ?assertNotEqual(nomatch, binary:match(Column, <<"frozen">>)),
                ?assertEqual(#{user_id => 100}, WhereMap),
                ?assertEqual(<<"id desc">>, Order),
                ?assertEqual(1, Page),
                ?assertEqual(20, Size),
                {ok, #{total => 0, page => 1, size => 20, list => []}}
            end}
        ],
        fun() ->
            ?assertMatch({ok, #{list := []}}, wallet_repo:page(#{user_id => 100}, 1, 20))
        end
    ).

page_error_passthrough_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'page_with_total', 6, fun(_T, _C, _W, _O, _P, _S) -> {error, boom} end}
        ],
        fun() ->
            ?assertEqual({error, boom}, wallet_repo:page(#{}, 1, 20))
        end
    ).
