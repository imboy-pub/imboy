-module(group_category_handler_contract_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_category_handler 契约兼容测试
%%%===================================================================

rename_numeric_category_id_compatible_test_() ->
    ?WITH_MECKS([
        {auth_ds, [
            {'current_uid', 1, fun(_State) -> 321 end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"id">> => 7,
                    <<"category_name">> => <<"工作"/utf8>>
                }
            end}
        ]},
        {group_category_logic, [
            {'rename', 3, fun(321, 7, <<"工作"/utf8>>) -> ok end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Payload, _Msg) -> req_ok end}
        ]}
    ], fun() ->
        {ok, Req1, _State} = group_category_handler:init(req_mock(), #{action => rename, current_uid => 321}),
        ?assertEqual(req_ok, Req1)
    end).

move_group_numeric_ids_compatible_test_() ->
    ?WITH_MECKS([
        {auth_ds, [
            {'current_uid', 1, fun(_State) -> 321 end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"gid">> => 1001,
                    <<"category_id">> => 8
                }
            end}
        ]},
        {group_category_logic, [
            {'move_group', 3, fun(321, 1001, 8) -> ok end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Payload, _Msg) -> req_ok end}
        ]}
    ], fun() ->
        {ok, Req1, _State} = group_category_handler:init(req_mock(), #{action => move_group, current_uid => 321}),
        ?assertEqual(req_ok, Req1)
    end).

sort_numeric_ids_compatible_test_() ->
    ?WITH_MECKS([
        {auth_ds, [
            {'current_uid', 1, fun(_State) -> 321 end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"sort_orders">> => [
                        #{<<"id">> => 5, <<"sort_order">> => 1},
                        #{<<"id">> => 9, <<"sort_order">> => 2}
                    ]
                }
            end}
        ]},
        {group_category_logic, [
            {'update_sort_order', 2, fun(321, [{5, 1}, {9, 2}]) -> ok end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Payload, _Msg) -> req_ok end}
        ]}
    ], fun() ->
        {ok, Req1, _State} = group_category_handler:init(req_mock(), #{action => sort, current_uid => 321}),
        ?assertEqual(req_ok, Req1)
    end).

delete_legacy_id_still_supported_test_() ->
    ?WITH_MECKS([
        {auth_ds, [
            {'current_uid', 1, fun(_State) -> 321 end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"id">> => <<"hash_cat">>}
            end}
        ]},
        {group_category_logic, [
            {'delete', 2, fun(321, 88) -> ok end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Payload, _Msg) -> req_ok end}
        ]}
    ], fun() ->
        {ok, Req1, _State} = group_category_handler:init(req_mock(), #{action => delete, current_uid => 321}),
        ?assertEqual(req_ok, Req1)
    end).

req_mock() ->
    #{mock_req => true}.
