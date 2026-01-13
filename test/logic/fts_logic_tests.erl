-module(fts_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% @doc fts_logic 模块测试
user_search_success_test_() ->
    ?WITH_MECK(fts_user_ds, [
        {'user_search_page', 3, fun(_Keyword, _Limit, _Offset) ->
            {ok, [#{<<"id">> => 1, <<"nickname">> => <<"用户1"/utf8>>}]}
        end}
    ], fun() ->
        Result = fts_logic:user_search(<<"用户"/utf8>>, 10, 0),
        ?assertMatch({ok, [_ | _]}, Result)
    end).
