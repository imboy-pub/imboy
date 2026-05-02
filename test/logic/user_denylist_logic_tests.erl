-module(user_denylist_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% @doc user_denylist_logic 模块测试
add_to_denylist_success_test_() ->
    ?WITH_MECKS([
        {user_denylist_ds, [
            {'add', 3, fun(_Uid, _DeniedUid, _Remark) -> ok end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> 1000000 end}
        ]},
        {imboy_cache, [
            {'flush', 1, fun(_Key) -> ok end}
        ]}
    ], fun() ->
        Result = user_denylist_logic:add(100, 101, <<"原因"/utf8>>),
        ?assertEqual(ok, Result)
    end).

remove_from_denylist_success_test_() ->
    ?WITH_MECKS([
        {user_denylist_ds, [
            {'remove', 2, fun(_Uid, _DeniedUid) -> ok end}
        ]},
        {imboy_cache, [
            {'flush', 1, fun(_Key) -> ok end}
        ]}
    ], fun() ->
        Result = user_denylist_logic:remove(100, 101),
        ?assertEqual(ok, Result)
    end).

in_denylist_true_test_() ->
    ?WITH_MECKS([
        {user_denylist_ds, [
            {'in_denylist', 2, fun(_Uid, _DeniedUid) -> 1 end}
        ]},
        {imboy_cache, [
            {'memo', 3, fun(Fun, _Key, _TTL) -> Fun() end}
        ]}
    ], fun() ->
        Result = user_denylist_logic:in_denylist(100, 101),
        ?assertEqual(1, Result)
    end).
