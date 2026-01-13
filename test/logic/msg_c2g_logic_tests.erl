-module(msg_c2g_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% @doc msg_c2g_logic 模块测试
send_message_success_test_() ->
    ?WITH_MECKS([
        {msg_c2g_ds, [
            {'save', 1, fun(_Data) -> {ok, <<"msg_id">>} end}
        ]},
        {message_ds, [
            {'send_next', 2, fun(_Uid, _Msg) -> ok end}
        ]}
    ], fun() ->
        Result = msg_c2g_logic:send(#{<<"from_id">> => 100, <<"group_id">> => 1, <<"body">> => <<"消息"/utf8>>}),
        ?assertMatch({ok, _}, Result)
    end).
