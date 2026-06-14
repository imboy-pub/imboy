-module(recharge_order_repo_page_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% @doc recharge_order_repo:page/3 运营分页查询测试
%%% 验证：透传 WhereMap（status/payment_method/user_id/order_no）、固定列、排序。

page_passes_wheremap_and_columns_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'page_with_total', 6, fun(Table, Column, WhereMap, Order, Page, Size) ->
                ?assert(is_binary(Table)),
                ?assertNotEqual(nomatch, binary:match(Column, <<"order_no">>)),
                ?assertNotEqual(nomatch, binary:match(Column, <<"payment_method">>)),
                ?assertEqual(#{status => 1, user_id => 100}, WhereMap),
                ?assertEqual(<<"id desc">>, Order),
                ?assertEqual(1, Page),
                ?assertEqual(20, Size),
                {ok, #{total => 0, page => 1, size => 20, list => []}}
            end}
        ],
        fun() ->
            ?assertMatch(
                {ok, #{list := []}},
                recharge_order_repo:page(#{status => 1, user_id => 100}, 1, 20)
            )
        end
    ).

page_error_passthrough_test_() ->
    ?WITH_MECK(
        elib_pg,
        [
            {'page_with_total', 6, fun(_T, _C, _W, _O, _P, _S) -> {error, boom} end}
        ],
        fun() ->
            ?assertEqual({error, boom}, recharge_order_repo:page(#{}, 1, 20))
        end
    ).
