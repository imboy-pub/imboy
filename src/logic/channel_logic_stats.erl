-module(channel_logic_stats).
-compile([nowarn_deprecated_catch]).

-export([get_channel_stats/2]).
-export([get_channel_stats_admin/1]).
-export([record_message_view/3]).
-export([add_reaction/4]).
-export([remove_reaction/4]).
-export([get_daily_stats/3]).

-spec get_channel_stats(integer(), binary()) -> {ok, map()} | {error, binary()}.
get_channel_stats(Uid, ChannelIdBin) ->
    ChannelId = decode_positive_id(ChannelIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            case channel_logic_common:ensure_channel_content_access(Uid, ChannelId) of
                {error, Reason} ->
                    {error, Reason};
                ok ->
                    fetch_channel_stats(ChannelId, ChannelIdBin)
            end
    end.

%% @doc 管理后台统计：跳过频道订阅者访问校验（adm 侧已做管理员鉴权，
%% 管理员通常无频道角色，走 ensure_channel_content_access 恒被拒）。
-spec get_channel_stats_admin(binary()) -> {ok, map()} | {error, binary()}.
get_channel_stats_admin(ChannelIdBin) ->
    ChannelId = decode_positive_id(ChannelIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            fetch_channel_stats(ChannelId, ChannelIdBin)
    end.

fetch_channel_stats(ChannelId, ChannelIdBin) ->
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
                            {error, elib_cnv:safe_to_binary(Reason)};
                        UnexpectedReactions ->
                            {error, elib_cnv:safe_to_binary(UnexpectedReactions)}
                    end;
                {error, Reason} ->
                    {error, elib_cnv:safe_to_binary(Reason)};
                UnexpectedStats ->
                    {error, elib_cnv:safe_to_binary(UnexpectedStats)}
            end;
        _Unexpected ->
            {error, <<"频道不存在"/utf8>>}
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
                    case ensure_message_in_channel(MessageId, ChannelId) of
                        {error, Reason} ->
                            {error, Reason};
                        ok ->
                            case channel_ds:has_viewed_message(MessageId, Uid) of
                                true ->
                                    ok;
                                false ->
                                    Now = elib_dt:millisecond(),
                                    case
                                        channel_ds:insert_message_view(
                                            ChannelId, MessageId, Uid, Now
                                        )
                                    of
                                        {ok, _} -> ok;
                                        {error, Reason} -> {error, elib_cnv:safe_to_binary(Reason)};
                                        Unexpected -> {error, elib_cnv:safe_to_binary(Unexpected)}
                                    end;
                                {error, Reason} ->
                                    {error, elib_cnv:safe_to_binary(Reason)};
                                Unexpected ->
                                    {error, elib_cnv:safe_to_binary(Unexpected)}
                            end
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
                    case ensure_message_in_channel(MessageId, ChannelId) of
                        {error, Reason} ->
                            {error, Reason};
                        ok ->
                            Now = elib_dt:millisecond(),
                            case
                                channel_ds:insert_reaction(
                                    ChannelId, MessageId, Uid, ReactionType, Now
                                )
                            of
                                {ok, _} -> ok;
                                {error, Reason} -> {error, elib_cnv:safe_to_binary(Reason)};
                                Unexpected -> {error, elib_cnv:safe_to_binary(Unexpected)}
                            end
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
                    case ensure_message_in_channel(MessageId, ChannelId) of
                        {error, Reason} ->
                            {error, Reason};
                        ok ->
                            case
                                channel_ds:delete_reaction(ChannelId, MessageId, Uid, ReactionType)
                            of
                                {ok, _} -> ok;
                                {error, Reason} -> {error, elib_cnv:safe_to_binary(Reason)};
                                Unexpected -> {error, elib_cnv:safe_to_binary(Unexpected)}
                            end
                    end;
                {error, Reason} ->
                    {error, elib_cnv:safe_to_binary(Reason)}
            end
    end.

%% @doc IDOR 防御：校验 MessageId 确实属于 ChannelId，防止调用者用自己有权访问
%% 的频道 A 的 ChannelId，配合猜测/枚举到的另一频道 B 的 MessageId，对 B 里
%% 自己无权访问的消息插入反应/浏览记录（channel_ds:insert_reaction /
%% insert_message_view 本身不做归属校验，需要调用方在此收口）。
%% 参考 get_message_reactions/3、channel_logic_message:revoke_message/3
%% 已有的同款校验模式。
-spec ensure_message_in_channel(integer(), integer()) -> ok | {error, binary()}.
ensure_message_in_channel(MessageId, ChannelId) ->
    case channel_message_ds:find_by_id(MessageId) of
        #{<<"channel_id">> := MsgChannelId} when MsgChannelId =:= ChannelId ->
            ok;
        _ ->
            {error, <<"消息不属于该频道"/utf8>>}
    end.

-spec get_daily_stats(integer(), binary(), integer()) -> {ok, list(map())} | {error, binary()}.
get_daily_stats(Uid, ChannelIdBin, Days) ->
    ChannelId = decode_positive_id(ChannelIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            case channel_logic_common:ensure_channel_content_access(Uid, ChannelId) of
                {error, Reason} ->
                    {error, Reason};
                ok ->
                    case channel_ds:get_daily_stats(ChannelId, Days) of
                        {ok, Stats} when is_list(Stats) ->
                            {ok, [S || S <- Stats, is_map(S)]};
                        {ok, UnexpectedStats} ->
                            {error, elib_cnv:safe_to_binary(UnexpectedStats)};
                        {error, Reason} ->
                            {error, elib_cnv:safe_to_binary(Reason)};
                        Unexpected ->
                            {error, elib_cnv:safe_to_binary(Unexpected)}
                    end
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
