-module(user_collect_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% @doc user_collect_logic 模块测试
add_collect_success_test_() ->
    ?WITH_MECKS(
        [
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
        ],
        fun() ->
            % add/6: Uid, Kind, KindId, Info, Source, Remark
            Result = user_collect_logic:add(
                100,
                <<"1">>,
                <<"article1">>,
                #{<<"title">> => <<"标题"/utf8>>},
                <<"web">>,
                <<"备注"/utf8>>
            ),
            ?assertMatch({ok, _}, Result)
        end
    ).

%% 迁移 227 起 remove 分流（§9）：legacy 行/无行 → 物理 DELETE（现状语义）；
%% sealed 行 → 服务端代执行墓碑。
remove_collect_success_test_() ->
    ?WITH_MECK(
        user_collect_ds,
        [
            {'find_any_by_uid_kind_id', 2, fun(_Uid, _KindId) -> not_found end},
            {'delete', 2, fun(_Uid, _KindId) -> {ok, 1} end}
        ],
        fun() ->
            Result = user_collect_logic:remove(100, <<"article1">>),
            ?assertEqual({ok, deleted}, Result)
        end
    ).

remove_legacy_row_deletes_physically_test_() ->
    Row = #{<<"status">> => 1, <<"generation">> => 0, <<"key_version">> => null},
    ?WITH_MECK(
        user_collect_ds,
        [
            {'find_any_by_uid_kind_id', 2, fun(_Uid, _KindId) -> {ok, Row} end},
            {'delete', 2, fun(_Uid, _KindId) -> {ok, 1} end}
        ],
        fun() ->
            Result = user_collect_logic:remove(100, <<"article1">>),
            ?assertEqual({ok, 1}, Result)
        end
    ).

remove_sealed_row_tombstones_test_() ->
    Row = #{<<"status">> => 1, <<"generation">> => 3, <<"key_version">> => 1},
    ?WITH_MECK(
        user_collect_ds,
        [
            {'find_any_by_uid_kind_id', 2, fun(_Uid, _KindId) -> {ok, Row} end},
            {'tombstone_sealed', 2, fun(_Uid, _KindId) -> {ok, tombstoned} end}
        ],
        fun() ->
            %% sealed 行绝不物理删（§9：服务端代执行墓碑）
            Result = user_collect_logic:remove(100, <<"article1">>),
            ?assertEqual({ok, tombstoned}, Result)
        end
    ).

add_collect_with_empty_source_fails_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = user_collect_logic:add(
            100,
            <<"1">>,
            <<"article1">>,
            #{<<"title">> => <<"标题"/utf8>>},
            <<"">>,
            <<"备注"/utf8>>
        ),
        ?assertMatch({error, _}, Result)
    end).

add_collect_with_empty_kind_id_fails_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = user_collect_logic:add(
            100,
            <<"1">>,
            <<"">>,
            #{<<"title">> => <<"标题"/utf8>>},
            <<"web">>,
            <<"备注"/utf8>>
        ),
        ?assertMatch({error, _}, Result)
    end).

add_collect_with_empty_info_fails_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = user_collect_logic:add(
            100,
            <<"1">>,
            <<"article1">>,
            #{},
            <<"web">>,
            <<"备注"/utf8>>
        ),
        ?assertMatch({error, _}, Result)
    end).
