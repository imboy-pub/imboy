-module(msg_reaction_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

encoded_uid(Uid) ->
    list_to_binary(["uid:", integer_to_list(Uid)]).

find_reaction_group(Emoji, Reactions) ->
    {Emoji, Data} = lists:keyfind(Emoji, 1, Reactions),
    Data.

find_reaction_stat(Emoji, Stats) ->
    [Stat] = [Item || Item <- Stats, maps:get(<<"emoji">>, Item) =:= Emoji],
    Stat.

add_reaction_returns_encoded_user_and_timestamp_test_() ->
    MsgId = <<"test_msg_ds_001">>,
    MsgType = <<"c2c">>,
    UserId = 999999,
    Emoji = <<"👍"/utf8>>,
    CreatedAt = <<"2026-03-16T00:00:00Z">>,
    EncodedUid = encoded_uid(UserId),
    CacheKey = {msg_reaction, MsgId, MsgType},
    ?WITH_MECKS([
        {msg_reaction_repo, [
            {'add', 4, fun(MsgId, MsgType, UserId, Emoji) -> ok end}
        ]},
        {imboy_cache, [
            {'delete', 1, fun(CacheKey) -> ok end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> fixed_now end},
            {'to_rfc3339', 1, fun(fixed_now) -> CreatedAt end}
        ]}
    ], fun() ->
        {ok, Result} = msg_reaction_ds:add_reaction(MsgId, MsgType, UserId, Emoji),
        ?assertEqual(MsgId, maps:get(<<"msg_id">>, Result)),
        ?assertEqual(MsgType, maps:get(<<"msg_type">>, Result)),
        ?assertEqual(UserId, maps:get(<<"user_id">>, Result)),
        ?assertEqual(Emoji, maps:get(<<"emoji">>, Result)),
        ?assertEqual(CreatedAt, maps:get(<<"created_at">>, Result)),
        ?assert(meck:called(imboy_cache, delete, [CacheKey]))
    end).

remove_reaction_clears_cache_test_() ->
    MsgId = <<"test_msg_ds_002">>,
    MsgType = <<"c2c">>,
    UserId = 999999,
    Emoji = <<"❤️"/utf8>>,
    CacheKey = {msg_reaction, MsgId, MsgType},
    ?WITH_MECKS([
        {msg_reaction_repo, [
            {'remove', 4, fun(MsgId, MsgType, UserId, Emoji) -> ok end}
        ]},
        {imboy_cache, [
            {'delete', 1, fun(CacheKey) -> ok end}
        ]}
    ], fun() ->
        ?assertEqual(ok, msg_reaction_ds:remove_reaction(MsgId, MsgType, UserId, Emoji)),
        ?assert(meck:called(imboy_cache, delete, [CacheKey]))
    end).

get_reactions_groups_by_emoji_and_encodes_users_test_() ->
    MsgId = <<"test_msg_ds_003">>,
    MsgType = <<"c2g">>,
    ThumbsUp = <<"👍"/utf8>>,
    Heart = <<"❤️"/utf8>>,
    ?WITH_MECKS([
        {msg_reaction_repo, [
            {'find_by_msg', 2, fun(MsgId, MsgType) ->
                {ok, [
                    #{<<"emoji">> => ThumbsUp, <<"user_id">> => 999999},
                    #{<<"emoji">> => ThumbsUp, <<"user_id">> => 999998},
                    #{<<"emoji">> => Heart, <<"user_id">> => 999997}
                ]}
            end}
        ]}
    ], fun() ->
        {ok, Reactions} = msg_reaction_ds:get_reactions(MsgId, MsgType),
        ?assertEqual(2, length(Reactions)),

        ThumbsUpData = find_reaction_group(ThumbsUp, Reactions),
        HeartData = find_reaction_group(Heart, Reactions),
        ?assertEqual(2, maps:get(<<"count">>, ThumbsUpData)),
        ?assertEqual(
            lists:sort([999998, 999999]),
            lists:sort(maps:get(<<"users">>, ThumbsUpData))
        ),
        ?assertEqual(1, maps:get(<<"count">>, HeartData))
    end).

get_reaction_stats_returns_map_entries_test_() ->
    MsgId = <<"test_msg_ds_004">>,
    MsgType = <<"c2c">>,
    ThumbsUp = <<"👍"/utf8>>,
    Heart = <<"❤️"/utf8>>,
    ?WITH_MECKS([
        {msg_reaction_repo, [
            {'find_by_msg', 2, fun(MsgId, MsgType) ->
                {ok, [
                    #{<<"emoji">> => ThumbsUp, <<"user_id">> => 999999},
                    #{<<"emoji">> => ThumbsUp, <<"user_id">> => 999998},
                    #{<<"emoji">> => Heart, <<"user_id">> => 999997}
                ]}
            end}
        ]}
    ], fun() ->
        {ok, Stats} = msg_reaction_ds:get_reaction_stats(MsgId, MsgType),
        ?assertEqual(2, length(Stats)),

        ThumbsUpStat = find_reaction_stat(ThumbsUp, Stats),
        HeartStat = find_reaction_stat(Heart, Stats),
        ?assertEqual(2, maps:get(<<"count">>, ThumbsUpStat)),
        ?assertEqual(
            lists:sort([999998, 999999]),
            lists:sort(maps:get(<<"users">>, ThumbsUpStat))
        ),
        ?assertEqual(1, maps:get(<<"count">>, HeartStat))
    end).

is_reacted_returns_false_when_repo_is_empty_test_() ->
    MsgId = <<"test_msg_ds_005">>,
    MsgType = <<"c2c">>,
    UserId = 999999,
    Emoji = <<"😄"/utf8>>,
    ?WITH_MECK(msg_reaction_repo, [
        {'find_by_msg_emoji', 3, fun(MsgId, MsgType, Emoji) -> {ok, []} end}
    ], fun() ->
        ?assertEqual(false, msg_reaction_ds:is_reacted(MsgId, MsgType, UserId, Emoji))
    end).

is_reacted_returns_true_when_user_reaction_exists_test_() ->
    MsgId = <<"test_msg_ds_006">>,
    MsgType = <<"c2g">>,
    UserId = 999999,
    Emoji = <<"🎉"/utf8>>,
    ?WITH_MECK(msg_reaction_repo, [
        {'find_by_msg_emoji', 3, fun(MsgId, MsgType, Emoji) ->
            {ok, [
                #{<<"emoji">> => Emoji, <<"user_id">> => 999998},
                #{<<"emoji">> => Emoji, <<"user_id">> => UserId}
            ]}
        end}
    ], fun() ->
        ?assertEqual(true, msg_reaction_ds:is_reacted(MsgId, MsgType, UserId, Emoji))
    end).
