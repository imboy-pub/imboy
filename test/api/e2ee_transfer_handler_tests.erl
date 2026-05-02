-module(e2ee_transfer_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc e2ee_transfer_handler 基础行为测试
%%%===================================================================

module_loaded_test_() ->
    ?TEST_SIMPLE(fun() ->
        code:ensure_loaded(e2ee_transfer_handler),
        ?assertMatch({file, _}, code:is_loaded(e2ee_transfer_handler))
    end).

handle_action_false_returns_original_req_test_() ->
    ?TEST_SIMPLE(fun() ->
        Req0 = cowboy_req_h:new(#{uri => <<"/v1/e2ee/transfer/unknown">>}),
        Result = e2ee_transfer_handler:handle_action(false, Req0, #{}),
        ?assertEqual(Req0, Result)
    end).

init_with_false_action_removes_action_from_state_test_() ->
    ?TEST_SIMPLE(fun() ->
        Req0 = cowboy_req_h:new(#{}),
        {ok, Req1, State1} = e2ee_transfer_handler:init(Req0, #{action => false, keep => 2}),
        ?assertEqual(Req0, Req1),
        ?assertEqual(#{keep => 2}, State1)
    end).

create_transfer_accepts_legacy_to_uid_test_() ->
    ?WITH_MECKS([
        {imboy_policy, [
            {'e2ee_enabled', 0, fun() -> true end}
        ]},
        {cowboy_req, [
            {'read_body', 1, fun(_Req) ->
                {ok, <<"{\"to_uid\":\"12345\"}">>, req_after_body}
            end}
        ]},
        {user_ds, [
            {'may_exist', 1, fun(12345) -> true end}
        ]},
        {user_device_ds, [
            {'get_public_by_uid', 1, fun(Uid) ->
                case Uid of
                    100 ->
                        {ok, [#{
                            <<"device_id">> => <<"sender_device_1">>,
                            <<"public_key">> => <<"sender_public">>
                        }]};
                    12345 ->
                        {ok, [#{
                            <<"device_id">> => <<"receiver_device_1">>,
                            <<"public_key">> => <<"receiver_public">>
                        }]}
                end
            end},
            {'get_private_key', 2, fun(100, <<"sender_device_1">>) ->
                {ok, <<"sender_private">>}
            end}
        ]},
        {e2ee_transfer_logic, [
            {'create_transfer', 5, fun(100, <<"sender_device_1">>, 12345, <<"sender_private">>, <<"receiver_public">>) ->
                {ok, #{
                    <<"session_id">> => <<"session-1">>,
                    <<"expires_at">> => <<"2026-02-25T00:00:00Z">>
                }}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Data) ->
                self() ! {resp_data, Data},
                req_ok
            end}
        ]}
    ], fun() ->
        Req0 = cowboy_req_h:new(#{}),
        Result = e2ee_transfer_handler:create_transfer(Req0, #{current_uid => 100}),
        ?assertEqual(req_ok, Result),
        ?assertEqual(
            #{<<"session_id">> => <<"session-1">>, <<"expires_at">> => <<"2026-02-25T00:00:00Z">>},
            receive_resp_data()
        )
    end).

receive_resp_data() ->
    receive
        {resp_data, Data} -> Data
    after 1000 ->
        timeout
    end.
