-module(channel_logic_stats).
-compile([nowarn_deprecated_catch]).

-export([get_channel_stats/1]).
-export([record_message_view/3]).
-export([add_reaction/4]).
-export([remove_reaction/4]).
-export([get_daily_stats/2]).

-spec get_channel_stats(binary()) -> {ok, map()} | {error, binary()}.
get_channel_stats(ChannelIdBin) ->
    ChannelId = decode_positive_id(ChannelIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            case channel_ds:find_by_id(ChannelId, <<"id,name,subscriber_count">>) of
                {error, _} ->
                    {error, <<"频道不存在"/utf8>>};
                Channel when is_map(Channel) ->
                    case get_message_stats(ChannelId) of
                        {ok, TotalMessages, TotalViews} ->
                            case channel_ds:get_reaction_count(ChannelId) of
                                {ok, Reactions} ->
                                    Stats = #{
                                        <<"channel_id">> => ChannelIdBin,
                                        <<"subscriber_count">> => maps:get(
                                            <<"subscriber_count">>, Channel, 0
                                        ),
                                        <<"total_messages">> => TotalMessages,
                                        <<"total_views">> => TotalViews,
                                        <<"total_reactions">> => Reactions
                                    },
                                    {ok, Stats};
                                {error, Reason} ->
                                    {error, elib_cnv:safe_to_binary(Reason)}
                            end;
                        {error, Reason} ->
                            {error, elib_cnv:safe_to_binary(Reason)}
                    end;
                _ ->
                    {error, <<"频道不存在"/utf8>>}
            end
    end.

-spec get_message_stats(integer()) ->
    {ok, non_neg_integer(), non_neg_integer()} | {error, term()}.
get_message_stats(ChannelId) ->
    channel_message_ds:get_stats(ChannelId).

-spec record_message_view(integer(), binary(), binary()) -> ok | {error, binary()}.
record_message_view(Uid, ChannelIdBin, MessageIdBin) ->
    ChannelId = channel_logic_common:resolve_channel_id(ChannelIdBin),
    MessageId = decode_positive_id(MessageIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ when MessageId =:= 0 ->
            {error, <<"消息不存在"/utf8>>};
        _ ->
            case channel_logic_common:ensure_channel_content_access(Uid, ChannelId) of
                ok ->
                    case channel_ds:has_viewed_message(MessageId, Uid) of
                        true ->
                            ok;
                        false ->
                            Now = elib_dt:now(),
                            case channel_ds:insert_message_view(ChannelId, MessageId, Uid, Now) of
                                {ok, _} -> ok;
                                {error, Reason} -> {error, elib_cnv:safe_to_binary(Reason)}
                            end;
                        {error, Reason} ->
                            {error, elib_cnv:safe_to_binary(Reason)}
                    end;
                {error, Reason} ->
                    {error, elib_cnv:safe_to_binary(Reason)}
            end
    end.

-spec add_reaction(integer(), binary(), binary(), binary()) -> ok | {error, binary()}.
add_reaction(Uid, ChannelIdBin, MessageIdBin, ReactionType) ->
    ChannelId = channel_logic_common:resolve_channel_id(ChannelIdBin),
    MessageId = decode_positive_id(MessageIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ when MessageId =:= 0 ->
            {error, <<"消息不存在"/utf8>>};
        _ ->
            case channel_logic_common:ensure_channel_content_access(Uid, ChannelId) of
                ok ->
                    Now = elib_dt:now(),
                    case channel_ds:insert_reaction(ChannelId, MessageId, Uid, ReactionType, Now) of
                        {ok, _} -> ok;
                        {error, Reason} -> {error, elib_cnv:safe_to_binary(Reason)}
                    end;
                {error, Reason} ->
                    {error, elib_cnv:safe_to_binary(Reason)}
            end
    end.

-spec remove_reaction(integer(), binary(), binary(), binary()) -> ok | {error, binary()}.
remove_reaction(Uid, ChannelIdBin, MessageIdBin, ReactionType) ->
    ChannelId = channel_logic_common:resolve_channel_id(ChannelIdBin),
    MessageId = decode_positive_id(MessageIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ when MessageId =:= 0 ->
            {error, <<"消息不存在"/utf8>>};
        _ ->
            case channel_logic_common:ensure_channel_content_access(Uid, ChannelId) of
                ok ->
                    case channel_ds:delete_reaction(ChannelId, MessageId, Uid, ReactionType) of
                        {ok, _} -> ok;
                        {error, Reason} -> {error, elib_cnv:safe_to_binary(Reason)}
                    end;
                {error, Reason} ->
                    {error, elib_cnv:safe_to_binary(Reason)}
            end
    end.

-spec get_daily_stats(binary(), integer()) -> {ok, list(map())} | {error, binary()}.
get_daily_stats(ChannelIdBin, Days) ->
    ChannelId = decode_positive_id(ChannelIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            case channel_ds:get_daily_stats(ChannelId, Days) of
                {ok, Stats} when is_list(Stats) ->
                    {ok, [S || S <- Stats, is_map(S)]};
                {ok, Other} ->
                    {error, elib_cnv:safe_to_binary(Other)};
                {error, Reason} ->
                    {error, elib_cnv:safe_to_binary(Reason)}
            end
    end.

-spec decode_positive_id(term()) -> integer().
decode_positive_id(Value) ->
    case catch ec_cnv:to_integer(Value) of
        Id when is_integer(Id), Id > 0 ->
            Id;
        _ ->
            0
    end.
