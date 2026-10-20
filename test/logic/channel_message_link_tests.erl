-module(channel_message_link_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

message_link_access_test_() ->
    [
        {setup,
            fun() ->
                %% 纯单元测试：所有外部读均 mock，不启动数据库和整套应用。
                lists:foreach(
                    fun(M) -> meck:new(M, [passthrough, no_link]) end,
                    [channel_logic_common, channel_message_ds, channel_ds]
                ),
                meck:expect(channel_logic_common, resolve_channel_id, fun binary_to_integer/1),
                meck:expect(channel_logic_common, decode_positive_id, fun binary_to_integer/1),
                meck:expect(
                    channel_logic_common,
                    ensure_channel_content_access,
                    fun(_, _) -> Access end
                ),
                meck:expect(channel_message_ds, find_by_id, fun(_) -> Message end),
                meck:expect(channel_ds, list_user_reactions, fun(_, _) -> {ok, []} end)
            end,
            fun(_) ->
                lists:foreach(
                    fun meck:unload/1,
                    [channel_logic_common, channel_message_ds, channel_ds]
                )
            end,
            fun(_) ->
                ?_test(begin
                    Result = channel_logic_message:get_message(7, <<"42">>, <<"100">>),
                    case Expected of
                        allowed -> ?assertMatch({ok, #{<<"id">> := 100}}, Result);
                        denied -> ?assertMatch({error, _}, Result)
                    end,
                    case Access of
                        ok -> ok;
                        _ -> ?assertEqual(0, meck:num_calls(channel_message_ds, find_by_id, '_'))
                    end
                end)
            end}
     || {Access, Message, Expected} <- [
            {ok, #{<<"id">> => 100, <<"channel_id">> => 42}, allowed},
            {ok, #{<<"id">> => 100, <<"channel_id">> => 43}, denied},
            {ok, #{<<"id">> => 100, <<"channel_id">> => 42, <<"revoked">> => true}, denied},
            {ok, #{}, denied},
            {{error, <<"forbidden">>}, #{<<"id">> => 100, <<"channel_id">> => 42}, denied}
        ]
    ].
