-module(channel_logic_message_reactions_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% attach_my_reactions/2：为一页消息批量补充 my_reactions 字段

attach_my_reactions_empty_list_test() ->
    ?assertEqual([], channel_logic_message:attach_my_reactions(1001, [])).

attach_my_reactions_marks_only_reacted_messages_test_() ->
    ?WITH_MECKS(
        [
            {channel_ds, [
                {'list_user_reactions', 2, fun(1001, [11, 12, 13]) ->
                    {ok, [
                        #{<<"message_id">> => 11, <<"reaction_type">> => <<"like">>},
                        #{<<"message_id">> => 13, <<"reaction_type">> => <<"like">>},
                        #{<<"message_id">> => 13, <<"reaction_type">> => <<"heart">>}
                    ]}
                end}
            ]}
        ],
        fun() ->
            Messages = [
                #{<<"id">> => 11, <<"content">> => <<"a">>},
                #{<<"id">> => 12, <<"content">> => <<"b">>},
                #{<<"id">> => 13, <<"content">> => <<"c">>}
            ],
            Result = channel_logic_message:attach_my_reactions(1001, Messages),
            ?assertMatch(
                [
                    #{<<"id">> := 11, <<"my_reactions">> := [<<"like">>]},
                    #{<<"id">> := 12, <<"my_reactions">> := []},
                    #{<<"id">> := 13, <<"my_reactions">> := [<<"like">>, <<"heart">>]}
                ],
                Result
            )
        end
    ).

attach_my_reactions_degrades_to_empty_on_db_error_test_() ->
    ?WITH_MECKS(
        [
            {channel_ds, [
                {'list_user_reactions', 2, fun(_, _) -> {error, db_down} end}
            ]}
        ],
        fun() ->
            Messages = [#{<<"id">> => 21, <<"content">> => <<"x">>}],
            Result = channel_logic_message:attach_my_reactions(1001, Messages),
            ?assertMatch([#{<<"id">> := 21, <<"my_reactions">> := []}], Result)
        end
    ).

attach_my_reactions_skips_rows_with_bad_shape_test_() ->
    ?WITH_MECKS(
        [
            {channel_ds, [
                {'list_user_reactions', 2, fun(_, _) ->
                    {ok, [
                        #{<<"message_id">> => 31, <<"reaction_type">> => <<"like">>},
                        #{<<"unexpected">> => true}
                    ]}
                end}
            ]}
        ],
        fun() ->
            Messages = [#{<<"id">> => 31}],
            Result = channel_logic_message:attach_my_reactions(1001, Messages),
            ?assertMatch([#{<<"id">> := 31, <<"my_reactions">> := [<<"like">>]}], Result)
        end
    ).

%% channel_repo:list_user_reactions/2 空 id 列表短路，不访问数据库
list_user_reactions_empty_ids_shortcircuit_test() ->
    ?assertEqual({ok, []}, channel_repo:list_user_reactions(1001, [])).
