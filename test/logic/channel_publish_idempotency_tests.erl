-module(channel_publish_idempotency_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

publish_message_duplicate_request_skips_side_effects_test_() ->
    ?WITH_MECKS(
        [
            {moderation_business, [
                {'inspect_channel', 4, fun(_, _, _, _) -> {queued, []} end},
                %% duplicate 幂等短路不应触达 bind_version——直通便于意外调用即红。
                {'bind_version', 2, fun(V, _) -> V end}
            ]},
            {channel_admin_repo, [
                {'get_role', 2, fun(11, 1001) -> 2 end}
            ]},
            {channel_ds, [
                %% GZAPP-02/G4 状态门：do_publish 前 find_by_id/2 查 status。
                {'find_by_id', 2, fun(_, _) -> #{<<"status">> => 1} end},
                {'publish_message', 6, fun(
                    11, 1001, <<"hello"/utf8>>, <<"text">>, #{}, <<"req-1">>
                ) ->
                    {ok, 99, duplicate}
                end}
            ]},
            {channel_message_ds, [
                {'find_by_id', 1, fun(99) ->
                    #{
                        <<"id">> => 99,
                        <<"channel_id">> => 11,
                        <<"author_id">> => 1001,
                        <<"content">> => <<"hello"/utf8>>,
                        <<"msg_type">> => <<"text">>,
                        <<"payload">> => <<"{}">>
                    }
                end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(_, _, _, _, _, _, _) ->
                    erlang:error(duplicate_must_not_broadcast)
                end}
            ]}
        ],
        fun() ->
            Result = channel_logic:publish_message(
                1001,
                <<"11">>,
                <<"hello"/utf8>>,
                <<"text">>,
                #{},
                <<"req-1">>
            ),

            ?assertMatch({ok, #{<<"id">> := 99}}, Result),
            ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
        end
    ).
