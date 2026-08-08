-module(conversation_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

req_mock() -> req.

ensure_imboy_loaded() ->
    case application:load(imboy) of
        ok -> ok;
        {error, {already_loaded, imboy}} -> ok;
        {error, {already_loaded, _}} -> ok;
        _ -> ok
    end.

online_list_action_success_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) -> [{<<"type">>, <<"list">>}, {<<"limit">>, <<"3">>}] end}
            ]},
            {imboy_syn, [
                {'count_user', 0, fun() -> 12 end},
                {'count', 0, fun() -> 20 end},
                {'list_by_limit', 1, fun(3) ->
                    [
                        {{1001, self()}, {ios, <<"did_1">>}, 1700000000, ref1, node()},
                        {{1002, self()}, {android, <<"did_2">>}, 1700000001, ref2, node()}
                    ]
                end}
            ]},
            {elib_dt, [
                {'to_rfc3339', 1, fun(_Nano) -> <<"2026-02-24T00:00:00Z">> end}
            ]},
            {elib_response, [
                {'success', 3, fun(_Req, Data, _Msg) ->
                    self() ! {resp_data, Data},
                    req_ok
                end}
            ]}
        ],
        fun() ->
            ensure_imboy_loaded(),
            {ok, Req, State} = conversation_handler:init(req_mock(), #{
                action => online, current_uid => 100
            }),
            ?assertEqual(req_ok, Req),
            ?assertEqual(#{current_uid => 100}, State),
            Data =
                receive
                    {resp_data, D} -> D
                after 1000 ->
                    timeout
                end,
            ?assertNotEqual(timeout, Data),
            ?assertEqual(2, length(Data))
        end
    ).

mine_action_returns_server_authoritative_conversation_list_test_() ->
    %% mine/2 现已是薄适配器：直接委托 conversation_logic:list/2（服务端权威列表），
    %% 会话聚合/去重/置顶逻辑下沉到 logic 层（由 conversation_logic_tests 覆盖）。
    %% 此处只验证 handler 透传 logic 返回的列表。
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) -> [{<<"last_server_ts">>, <<"1700000000">>}] end}
            ]},
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 100 end}
            ]},
            {conversation_logic, [
                {'list', 2, fun(100, #{last_server_ts := <<"1700000000">>}) ->
                    {ok, [
                        #{
                            <<"conversation_id">> => 3001,
                            <<"conversation_type">> => <<"c2g">>,
                            <<"last_msg">> => #{<<"content">> => <<"group-hi">>}
                        },
                        #{
                            <<"conversation_id">> => 2001,
                            <<"conversation_type">> => <<"c2c">>,
                            <<"last_msg">> => #{<<"content">> => <<"hello">>}
                        }
                    ]}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Data) ->
                    self() ! {resp_data, Data},
                    req_ok
                end}
            ]}
        ],
        fun() ->
            {ok, Req, _State} = conversation_handler:init(req_mock(), #{
                action => mine, current_uid => 100
            }),
            ?assertEqual(req_ok, Req),
            Data =
                receive
                    {resp_data, D} -> D
                after 1000 ->
                    timeout
                end,
            ?assertNotEqual(timeout, Data),
            ?assertEqual(2, length(Data)),
            [FirstItem | _] = Data,
            ?assertEqual(3001, maps:get(<<"conversation_id">>, FirstItem)),
            ?assertEqual(<<"c2g">>, maps:get(<<"conversation_type">>, FirstItem)),
            LastMsg = maps:get(<<"last_msg">>, FirstItem),
            ?assertEqual(<<"group-hi">>, maps:get(<<"content">>, LastMsg))
        end
    ).

delete_conversation_success_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'read_body', 1, fun(_Req) ->
                    {ok, <<"{\"conversation_id\":\"c1\",\"type\":\"c2c\"}">>, req_after_body}
                end}
            ]},
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 100 end}
            ]},
            {conversation_logic, [
                {'delete', 3, fun(100, <<"c1">>, <<"c2c">>) -> ok end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, _Data) -> req_ok end}
            ]}
        ],
        fun() ->
            {ok, Req, _State} = conversation_handler:init(req_mock(), #{
                action => delete_conversation, current_uid => 100
            }),
            ?assertEqual(req_ok, Req)
        end
    ).

delete_conversation_error_maps_operation_failed_code_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'read_body', 1, fun(_Req) ->
                    {ok, <<"{\"conversation_id\":\"c1\",\"type\":\"c2c\"}">>, req_after_body}
                end}
            ]},
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 100 end}
            ]},
            {conversation_logic, [
                {'delete', 3, fun(100, <<"c1">>, <<"c2c">>) -> {error, <<"failed">>} end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) ->
                    self() ! {resp_code, Code},
                    req_error
                end}
            ]}
        ],
        fun() ->
            {ok, Req, _State} = conversation_handler:init(req_mock(), #{
                action => delete_conversation, current_uid => 100
            }),
            ?assertEqual(req_error, Req),
            Code =
                receive
                    {resp_code, C} -> C
                after 1000 ->
                    timeout
                end,
            ?assertEqual(?ERR_OPERATION_FAILED, Code)
        end
    ).

pin_and_restore_actions_success_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'read_body', 1, fun(_Req) ->
                    {ok, <<"{\"conversation_id\":\"c1\",\"type\":\"c2c\"}">>, req_after_body}
                end}
            ]},
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 100 end}
            ]},
            {conversation_pin_logic, [
                {'pin', 3, fun(100, <<"c1">>, <<"c2c">>) -> ok end}
            ]},
            {conversation_logic, [
                {'restore', 3, fun(100, <<"c1">>, <<"c2c">>) -> ok end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, _Data) -> req_ok end}
            ]}
        ],
        fun() ->
            {ok, PinReq, _S1} = conversation_handler:init(req_mock(), #{
                action => pin_conversation, current_uid => 100
            }),
            ?assertEqual(req_ok, PinReq),
            {ok, RestoreReq, _S2} = conversation_handler:init(req_mock(), #{
                action => restore_conversation, current_uid => 100
            }),
            ?assertEqual(req_ok, RestoreReq)
        end
    ).

%% @doc BUG#129 回归：客户端按 TSID 契约以 string 传 conversation_id，
%% handler 系统边界必须转回 integer 再交给 logic（logic 层 is_integer 守卫会拒绝 string）
tsid_string_conversation_id_converted_to_integer_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'read_body', 1, fun(_Req) ->
                    {ok,
                        <<"{\"conversation_id\":\"8601234567890123\",\"type\":\"c2c\"}">>,
                        req_after_body}
                end}
            ]},
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 100 end}
            ]},
            {conversation_pin_logic, [
                {'pin', 3, fun(100, 8601234567890123, <<"c2c">>) -> ok end}
            ]},
            {conversation_logic, [
                {'delete', 3, fun(100, 8601234567890123, <<"c2c">>) -> ok end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, _Data) -> req_ok end}
            ]}
        ],
        fun() ->
            {ok, PinReq, _S1} = conversation_handler:init(req_mock(), #{
                action => pin_conversation, current_uid => 100
            }),
            ?assertEqual(req_ok, PinReq),
            {ok, DeleteReq, _S2} = conversation_handler:init(req_mock(), #{
                action => delete_conversation, current_uid => 100
            }),
            ?assertEqual(req_ok, DeleteReq)
        end
    ).
