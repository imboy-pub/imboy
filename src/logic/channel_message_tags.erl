-module(channel_message_tags).
-moduledoc "Pure channel message tag validation and patch merging.".

-export([normalize_payload/1, merge_tags/2]).

-spec normalize_payload(map()) -> {ok, map()} | {error, invalid_tags | invalid_payload}.
normalize_payload(Payload) when is_map(Payload) ->
    case maps:find(<<"tags">>, Payload) of
        error ->
            {ok, Payload};
        {ok, Tags} when is_list(Tags), length(Tags) =< 5 ->
            case normalize_tags(Tags, []) of
                {ok, Normalized} -> {ok, Payload#{<<"tags">> => Normalized}};
                {error, _} = Error -> Error
            end;
        {ok, _} ->
            {error, invalid_tags}
    end;
normalize_payload(_) ->
    {error, invalid_payload}.

-spec merge_tags(map(), map()) -> {ok, map()} | {error, invalid_tags | invalid_payload}.
merge_tags(Base, Patch) when is_map(Base), is_map(Patch) ->
    case maps:find(<<"tags">>, Patch) of
        error ->
            {ok, Base};
        {ok, Tags} ->
            case normalize_payload(#{<<"tags">> => Tags}) of
                {ok, Normalized} -> {ok, maps:merge(Base, Normalized)};
                {error, _} = Error -> Error
            end
    end;
merge_tags(_, _) ->
    {error, invalid_payload}.

normalize_tags([], Acc) ->
    {ok, lists:reverse(Acc)};
normalize_tags([Tag | Rest], Acc) when is_binary(Tag) ->
    case unicode:characters_to_list(Tag, utf8) of
        Chars when is_list(Chars) ->
            Trimmed = string:trim(Chars),
            case length(Trimmed) of
                Size when Size >= 1, Size =< 20 ->
                    Value = unicode:characters_to_binary(Trimmed),
                    Next =
                        case lists:member(Value, Acc) of
                            true -> Acc;
                            false -> [Value | Acc]
                        end,
                    normalize_tags(Rest, Next);
                _ ->
                    {error, invalid_tags}
            end;
        _ ->
            {error, invalid_tags}
    end;
normalize_tags(_, _) ->
    {error, invalid_tags}.
