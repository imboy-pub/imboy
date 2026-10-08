-module(micro_community_content_logic).
-moduledoc "Micro-community search and message metadata; no changes to phase-one delivery locks.".
-export([search/3, set_essence/4, set_tags/4]).

search(Uid, ChannelId0, Options) when is_map(Options) ->
    with_enabled(fun() ->
        case {positive_id(ChannelId0), search_options(Options)} of
            {ChannelId, {ok, Filters, Pagination}} when ChannelId > 0 ->
                micro_community_content_ds:search(Uid, ChannelId, Filters, Pagination);
            _ ->
                invalid()
        end
    end);
search(_, _, _) ->
    invalid().

set_essence(Uid, ChannelId0, MessageId0, Value) when is_boolean(Value) ->
    with_enabled(fun() ->
        case {positive_id(ChannelId0), positive_id(MessageId0)} of
            {ChannelId, MessageId} when ChannelId > 0, MessageId > 0 ->
                micro_community_content_ds:set_essence(Uid, ChannelId, MessageId, Value);
            _ ->
                invalid()
        end
    end);
set_essence(_, _, _, _) ->
    invalid().

set_tags(Uid, ChannelId0, MessageId0, Tags) ->
    with_enabled(fun() ->
        case
            {
                positive_id(ChannelId0),
                positive_id(MessageId0),
                channel_message_tags:normalize_payload(#{<<"tags">> => Tags})
            }
        of
            {ChannelId, MessageId, {ok, #{<<"tags">> := Normalized}}} when
                ChannelId > 0, MessageId > 0
            ->
                micro_community_content_ds:set_tags(Uid, ChannelId, MessageId, Normalized);
            _ ->
                invalid()
        end
    end).

with_enabled(Fun) ->
    try {imboy_policy:current_profile(), imboy_feature:enabled(channel)} of
        {phase1_delivery, _} -> {error, {403, <<"一期交付不开放微社群能力"/utf8>>}};
        {_, true} -> Fun();
        _ -> {error, {403, <<"频道功能未开放"/utf8>>}}
    catch
        _:_ -> {error, {503, <<"内容服务暂不可用"/utf8>>}}
    end.

search_options(Options) ->
    Page = positive_id(maps:get(<<"page">>, Options, 1)),
    Size = positive_id(maps:get(<<"size">>, Options, 10)),
    Q = maps:get(<<"q">>, Options, <<>>),
    Tag0 = maps:get(<<"tag">>, Options, <<>>),
    Essence = bool_filter(maps:get(<<"essence">>, Options, null)),
    case {Page > 0, Size >= 1 andalso Size =< 50, valid_query(Q), normalize_tag(Tag0), Essence} of
        {true, true, true, {ok, Tag}, {ok, Flag}} ->
            {ok, #{q => Q, tag => Tag, essence => Flag}, {Page, Size}};
        _ ->
            invalid()
    end.

normalize_tag(<<>>) ->
    {ok, <<>>};
normalize_tag(Tag) ->
    case channel_message_tags:normalize_payload(#{<<"tags">> => [Tag]}) of
        {ok, #{<<"tags">> := [Value]}} -> {ok, Value};
        _ -> invalid()
    end.

valid_query(Q) when is_binary(Q) ->
    case unicode:characters_to_list(Q, utf8) of
        Chars when is_list(Chars) -> length(Chars) =< 200;
        _ -> false
    end;
valid_query(_) ->
    false.

bool_filter(null) -> {ok, null};
bool_filter(true) -> {ok, true};
bool_filter(false) -> {ok, false};
bool_filter(<<"true">>) -> {ok, true};
bool_filter(<<"false">>) -> {ok, false};
bool_filter(_) -> invalid().

positive_id(Value) when is_integer(Value), Value > 0, Value =< 9223372036854775807 -> Value;
positive_id(Value) when is_binary(Value), byte_size(Value) =< 19 ->
    case re:run(Value, <<"^[1-9][0-9]*$">>, [{capture, none}]) of
        match -> positive_id(binary_to_integer(Value));
        _ -> 0
    end;
positive_id(_) ->
    0.

invalid() -> {error, {400, <<"参数格式无效"/utf8>>}}.
