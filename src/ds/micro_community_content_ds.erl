-module(micro_community_content_ds).
-moduledoc "Content authorization and writes share one transaction and locked source rows.".
-export([search/4, set_essence/4, set_tags/4]).

search(Uid, ChannelId, Filters, Pagination) ->
    transaction(fun(Conn) ->
        case context(Conn, Uid, ChannelId, read) of
            {ok, _} -> micro_community_content_repo:search_tx(Conn, ChannelId, Filters, Pagination);
            Error -> Error
        end
    end).

set_essence(Uid, ChannelId, MessageId, Value) ->
    mutate(Uid, ChannelId, MessageId, essence, Value).

set_tags(Uid, ChannelId, MessageId, Tags) ->
    case channel_message_tags:normalize_payload(#{<<"tags">> => Tags}) of
        {ok, #{<<"tags">> := Normalized}} -> mutate(Uid, ChannelId, MessageId, tags, Normalized);
        _ -> {error, {400, <<"标签格式无效"/utf8>>}}
    end.

mutate(Uid, ChannelId, MessageId, Kind, Value) ->
    transaction(fun(Conn) ->
        case context(Conn, Uid, ChannelId, write) of
            {ok, Channel} ->
                case micro_community_content_repo:message_tx(Conn, ChannelId, MessageId) of
                    {ok, Message} -> authorized_mutation(Conn, Uid, Channel, Message, Kind, Value);
                    {error, not_found} -> missing()
                end;
            Error ->
                Error
        end
    end).

authorized_mutation(Conn, Uid, Channel, Message, Kind, Value) ->
    Role = maps:get(role, Channel),
    Manager = is_integer(Role) andalso Role >= 2,
    Author = maps:get(<<"author_id">>, Message) =:= Uid,
    Allowed =
        case Kind of
            essence -> Manager;
            tags -> Manager orelse (Author andalso within_edit_window(Message))
        end,
    case Allowed of
        false ->
            denied();
        true ->
            ChannelId = maps:get(<<"id">>, Channel),
            MessageId = maps:get(<<"id">>, Message),
            case Kind of
                essence ->
                    micro_community_content_repo:essence_tx(Conn, ChannelId, MessageId, Value);
                tags ->
                    micro_community_content_repo:tags_tx(Conn, ChannelId, MessageId, Value)
            end
    end.

context(Conn, Uid, ChannelId, Mode) ->
    %% Keep the established workspace -> channel lock order for every transaction.
    case workspace_guard:ensure_writable_tx(Conn, {channel, ChannelId}) of
        ok -> locked_context(Conn, Uid, ChannelId, Mode);
        {error, {980, _}} when Mode =:= read -> locked_context(Conn, Uid, ChannelId, Mode);
        Error -> Error
    end.

locked_context(Conn, Uid, ChannelId, Mode) ->
    case micro_community_content_repo:context_tx(Conn, Uid, ChannelId) of
        {error, not_found} ->
            missing();
        {ok, Channel} ->
            case maps:get(<<"status">>, Channel) =:= 1 of
                false when Mode =:= write -> {error, {980, <<"频道已归档或禁用"/utf8>>}};
                false -> missing();
                true -> access_context(Channel)
            end
    end.

access_context(Channel) ->
    Role = maps:get(role, Channel),
    Content =
        case Role > 0 of
            true ->
                true;
            false ->
                case maps:get(<<"access_type">>, Channel) of
                    1 ->
                        maps:get(purchased, Channel);
                    _ ->
                        maps:get(<<"visibility">>, Channel) =:= 0 orelse
                            maps:get(subscribed, Channel)
                end
        end,
    case maps:get(workspace_allowed, Channel) andalso Content of
        false -> denied();
        true -> {ok, Channel}
    end.

within_edit_window(Message) ->
    Window = edit_window_seconds(),
    case Window =< 0 of
        true ->
            true;
        false ->
            Created = maps:get(<<"created_at">>, Message, undefined),
            CreatedMs =
                try
                    elib_dt:rfc3339_to(Created, millisecond)
                catch
                    _:_ -> invalid
                end,
            case CreatedMs of
                Ms when is_integer(Ms), Ms > 0 ->
                    Age = elib_dt:millisecond() - Ms,
                    Age >= 0 andalso Age =< Window * 1000;
                _ ->
                    false
            end
    end.

edit_window_seconds() ->
    case application:get_env(imboy, channel_edit_window_seconds) of
        {ok, Seconds} when is_integer(Seconds) -> Seconds;
        _ -> 86400
    end.

transaction(Fun) ->
    try elib_pg:with_tx(Fun, []) of
        {ok, _} = Ok -> Ok;
        {error, {Code, Msg}} = Error when is_integer(Code), is_binary(Msg) -> Error;
        _ -> unavailable()
    catch
        throw:{abort_tx, {Code, Msg}} when is_integer(Code), is_binary(Msg) -> {error, {Code, Msg}};
        _:_ -> unavailable()
    end.

missing() -> {error, {404, <<"内容不存在"/utf8>>}}.
denied() -> {error, {403, <<"无权限访问内容"/utf8>>}}.
unavailable() -> {error, {503, <<"内容服务暂不可用"/utf8>>}}.
