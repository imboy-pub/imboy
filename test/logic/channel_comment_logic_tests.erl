-module(channel_comment_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

create_parent_id_contract_test_() ->
    ?WITH_MECKS(
        [
            {channel_logic_common, [
                {resolve_channel_id, 1, fun(_) -> 42 end},
                {ensure_channel_content_access, 2, fun(_, _) -> ok end}
            ]},
            {channel_message_ds, [{find_by_id, 1, fun(_) -> #{<<"channel_id">> => 42} end}]},
            {user_ds, [{find_by_id, 2, fun(_, _) -> #{} end}]},
            {channel_comment_ds, [
                {add, 1, fun(Data) -> {ok, maps:get(<<"parent_id">>, Data)} end},
                {find_by_id, 1, fun(Id) -> #{<<"parent_id">> => Id} end}
            ]}
        ],
        fun() ->
            Valid = [
                {0, 0},
                {<<"0">>, 0},
                {9007199254740993, 9007199254740993},
                {<<"9007199254740993">>, 9007199254740993},
                {<<"9223372036854775807">>, 9223372036854775807}
            ],
            lists:foreach(
                fun({Input, Expected}) ->
                    ?assertEqual(
                        {ok, #{<<"parent_id">> => Expected}},
                        channel_comment_logic:create(1001, <<"42">>, <<"99">>, <<"ok">>, Input)
                    )
                end,
                Valid
            ),
            Invalid = [
                -1,
                <<"-1">>,
                9223372036854775808,
                <<"9223372036854775808">>,
                1.5,
                <<"1.5">>,
                <<"abc">>,
                <<>>,
                null
            ],
            lists:foreach(
                fun(Input) ->
                    ?assertMatch(
                        {error, {400, _}},
                        channel_comment_logic:create(1001, <<"42">>, <<"99">>, <<"ok">>, Input)
                    )
                end,
                Invalid
            ),
            ?assertEqual(length(Valid), meck:num_calls(channel_comment_ds, add, 1))
        end
    ).

paid_comment_like_requires_content_access_test_() ->
    ?WITH_MECKS(
        [
            {channel_comment_ds, [
                {'find_by_id', 1, fun(7001) -> #{<<"channel_id">> => 42} end},
                {'like', 1, fun(_CommentId) -> {ok, 1} end}
            ]},
            {channel_logic_common, [
                {'ensure_channel_content_access', 2, fun(1001, 42) ->
                    {error, <<"付费频道需要先购买"/utf8>>}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"付费频道需要先购买"/utf8>>},
                channel_comment_logic:like(1001, 7001)
            ),
            ?assertEqual(0, meck:num_calls(channel_comment_ds, like, 1))
        end
    ).

paid_comment_unlike_requires_content_access_test_() ->
    ?WITH_MECKS(
        [
            {channel_comment_ds, [
                {'find_by_id', 1, fun(7001) -> #{<<"channel_id">> => 42} end},
                {'unlike', 1, fun(_CommentId) -> {ok, 0} end}
            ]},
            {channel_logic_common, [
                {'ensure_channel_content_access', 2, fun(1001, 42) ->
                    {error, <<"付费频道需要先购买"/utf8>>}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"付费频道需要先购买"/utf8>>},
                channel_comment_logic:unlike(1001, 7001)
            ),
            ?assertEqual(0, meck:num_calls(channel_comment_ds, unlike, 1))
        end
    ).

comment_like_after_content_access_updates_count_test_() ->
    ?WITH_MECKS(
        [
            {channel_comment_ds, [
                {'find_by_id', 1, fun(7001) -> #{<<"channel_id">> => 42} end},
                {'like', 1, fun(7001) -> {ok, 3} end}
            ]},
            {channel_logic_common, [
                {'ensure_channel_content_access', 2, fun(1001, 42) -> ok end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, channel_comment_logic:like(1001, 7001)),
            ?assertEqual(1, meck:num_calls(channel_comment_ds, like, 1))
        end
    ).

comment_like_missing_comment_is_rejected_test_() ->
    ?WITH_MECKS(
        [
            {channel_comment_ds, [
                {'find_by_id', 1, fun(7001) -> {error, not_found} end},
                {'like', 1, fun(_CommentId) -> {ok, 1} end}
            ]},
            {channel_logic_common, [
                {'ensure_channel_content_access', 2, fun(_, _) -> ok end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"评论不存在"/utf8>>},
                channel_comment_logic:like(1001, 7001)
            ),
            ?assertEqual(0, meck:num_calls(channel_logic_common, ensure_channel_content_access, 2)),
            ?assertEqual(0, meck:num_calls(channel_comment_ds, like, 1))
        end
    ).
