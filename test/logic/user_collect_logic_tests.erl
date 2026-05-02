-module(user_collect_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% @doc user_collect_logic 模块测试
add_collect_success_test_() ->
    ?WITH_MECKS([
        {user_collect_ds, [
            {'count_by_uid_kind_id', 2, fun(_Uid, _KindId) -> 0 end},
            {'update', 3, fun(_Uid, _KindId, _Data) -> {ok, 1} end}
        ]},
        {elib_log, [
            {'info', 1, fun(_Msg) -> ok end},
            {'info', 2, fun(_Fmt, _Args) -> ok end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> 1000000 end}
        ]},
        {elib_pg, [
            {'with_tx', 1, fun(_Fun) -> ok end}
        ]}
    ], fun() ->
        % add/6: Uid, Kind, KindId, Info, Source, Remark
        Result = user_collect_logic:add(100, <<"1">>, <<"article1">>,
                                        #{<<"title">> => <<"标题"/utf8>>},
                                        <<"web">>, <<"备注"/utf8>>),
        ?assertMatch({ok, _}, Result)
    end).

remove_collect_success_test_() ->
    ?WITH_MECK(user_collect_ds, [
        {'delete', 2, fun(_Uid, _KindId) -> {ok, 1} end}
    ], fun() ->
        Result = user_collect_logic:remove(100, <<"article1">>),
        ?assertEqual({ok, 1}, Result)
    end).

add_collect_with_empty_source_fails_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = user_collect_logic:add(100, <<"1">>, <<"article1">>,
                                        #{<<"title">> => <<"标题"/utf8>>},
                                        <<"">>, <<"备注"/utf8>>),
        ?assertMatch({error, _}, Result)
    end).

add_collect_with_empty_kind_id_fails_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = user_collect_logic:add(100, <<"1">>, <<"">>,
                                        #{<<"title">> => <<"标题"/utf8>>},
                                        <<"web">>, <<"备注"/utf8>>),
        ?assertMatch({error, _}, Result)
    end).

add_collect_with_empty_info_fails_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = user_collect_logic:add(100, <<"1">>, <<"article1">>,
                                        #{},
                                        <<"web">>, <<"备注"/utf8>>),
        ?assertMatch({error, _}, Result)
    end).
