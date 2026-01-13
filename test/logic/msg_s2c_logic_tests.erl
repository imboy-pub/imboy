-module(msg_s2c_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% @doc msg_s2c_logic 模块测试
send_system_message_success_test_() ->
    ?WITH_MECKS([
        {msg_s2c_ds, [
            {'save', 1, fun(_Data) -> {ok, <<"msg_id">>} end}
        ]},
        {message_ds, [
            {'send_next', 2, fun(_Uid, _Msg) -> ok end}
        ]}
    ], fun() ->
        Result = msg_s2c_logic:send(#{<<"to_id">> => 100, <<"body">> => <<"系统消息"/utf8>>}),
        ?assertMatch({ok, _}, Result)
    end).
