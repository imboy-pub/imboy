-module(channel_logic_stats).

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
            case channel_repo:find_by_id(ChannelId, <<"id,name,subscriber_count">>) of
                {error, _} ->
                    {error, <<"频道不存在"/utf8>>};
                Channel when is_map(Channel) ->
                    case get_message_stats(ChannelId) of
                        {ok, TotalMessages, TotalViews} ->
                            case channel_repo:get_reaction_count(ChannelId) of
                                {ok, Reactions} ->
                                    Stats = #{
                                        <<"channel_id">> => ChannelIdBin,
                                        <<"subscriber_count">> => maps:get(<<"subscriber_count">>, Channel, 0),
                                        <<"total_messages">> => TotalMessages,
                                        <<"total_views">> => TotalViews,
                                        <<"total_reactions">> => Reactions
                                    },
                                    {ok, Stats};
                                {error, Reason} ->
                                    {error, elib_cnv:safe_to_binary(Reason)};
                                Other ->
                                    {error, elib_cnv:safe_to_binary(Other)}
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
    Tb = channel_message_repo:tablename(),
    Sql = <<"SELECT COUNT(*) as total_messages, COALESCE(SUM(view_count), 0) as total_views "
            "FROM ", Tb/binary, " WHERE channel_id = $1 AND status = 1">>,
    case elib_pg:one(Sql, [ChannelId]) of
        {ok, Row} when is_map(Row) ->
            TotalMsgs = maps:get(<<"total_messages">>, Row, 0),
            TotalViews = maps:get(<<"total_views">>, Row, 0),
            {ok, TotalMsgs, TotalViews};
        {error, Reason} ->
            {error, Reason};
        Other ->
            {error, Other}
    end.

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
                    case channel_repo:has_viewed_message(MessageId, Uid) of
                        true ->
                            ok;
                        false ->
                            Now = elib_dt:now(),
                            case channel_repo:insert_message_view(ChannelId, MessageId, Uid, Now) of
                                {ok, _} -> ok;
                                {error, Reason} -> {error, elib_cnv:safe_to_binary(Reason)};
                                Other -> {error, elib_cnv:safe_to_binary(Other)}
                            end;
                        {error, Reason} ->
                            {error, elib_cnv:safe_to_binary(Reason)};
                        Other ->
                            {error, elib_cnv:safe_to_binary(Other)}
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
                    case channel_repo:insert_reaction(ChannelId, MessageId, Uid, ReactionType, Now) of
                        {ok, _} -> ok;
                        {error, Reason} -> {error, elib_cnv:safe_to_binary(Reason)};
                        Other -> {error, elib_cnv:safe_to_binary(Other)}
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
                    case channel_repo:delete_reaction(ChannelId, MessageId, Uid, ReactionType) of
                        {ok, _} -> ok;
                        {error, Reason} -> {error, elib_cnv:safe_to_binary(Reason)};
                        Other -> {error, elib_cnv:safe_to_binary(Other)}
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
            case channel_repo:get_daily_stats(ChannelId, Days) of
                {ok, Stats} when is_list(Stats) ->
                    {ok, [S || S <- Stats, is_map(S)]};
                {ok, Other} ->
                    {error, elib_cnv:safe_to_binary(Other)};
                {error, Reason} ->
                    {error, elib_cnv:safe_to_binary(Reason)};
                Other ->
                    {error, elib_cnv:safe_to_binary(Other)}
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
