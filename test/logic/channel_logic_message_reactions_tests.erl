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

%% get_message_reactions/3 回归：MessageId 必须解码为整数传 find_by_id，
%% 数据源必须是 channel_reaction 聚合（elib_pg:query）。
%% 背景：曾把 binary MessageId 直接传 find_by_id（恒落"消息不属于该频道"），
%% 且误走 msg_reaction_ds（msg_reaction 表，msg_type='channel' 恒空）。
get_message_reactions_decodes_id_and_reads_channel_reaction_test_() ->
    ?WITH_MECKS(
        [
            {channel_logic_common, [
                {'ensure_channel_content_access', 2, fun(_Uid, _Cid) -> ok end}
            ]},
            {channel_message_ds, [
                %% guard 限定 integer 且等值：binary MessageId 直接传入即 red
                {'find_by_id', 1, fun(Id) when is_integer(Id), Id =:= 900 ->
                    #{<<"channel_id">> => 42}
                end}
            ]},
            {elib_pg, [
                {'query', 2, fun(Sql, [Id]) when is_binary(Sql), Id =:= 900 ->
                    true = nomatch =/= binary:match(Sql, <<"FROM channel_reaction">>),
                    {ok, [#{<<"reaction_type">> => <<"like">>, <<"cnt">> => 1}]}
                end}
            ]}
        ],
        fun() ->
            Result = channel_logic:get_message_reactions(1001, <<"42">>, <<"900">>),
            ?assertMatch(
                {ok, [#{<<"reaction_type">> := <<"like">>, <<"cnt">> := 1}]},
                Result
            )
        end
    ).

%% 消息不属于该频道（IDOR 防御分支保持）
get_message_reactions_rejects_foreign_message_test_() ->
    ?WITH_MECKS(
        [
            {channel_logic_common, [
                {'ensure_channel_content_access', 2, fun(_Uid, _Cid) -> ok end}
            ]},
            {channel_message_ds, [
                {'find_by_id', 1, fun(900) -> #{<<"channel_id">> => 999} end}
            ]}
        ],
        fun() ->
            Result = channel_logic:get_message_reactions(1001, <<"42">>, <<"900">>),
            ?assertEqual({error, <<"消息不属于该频道"/utf8>>}, Result)
        end
    ).

%% MessageId 非正整数短路，不触库
get_message_reactions_rejects_bad_message_id_test() ->
    ?assertEqual(
        {error, <<"消息不存在"/utf8>>},
        channel_logic:get_message_reactions(1001, <<"42">>, <<"abc">>)
    ).
