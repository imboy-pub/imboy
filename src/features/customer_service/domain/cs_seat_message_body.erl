-module(cs_seat_message_body).

-export([validate/1]).

-define(MAX_CODEPOINTS, 4000).

validate(#{sender_type := <<"business_identity">>} = Params) ->
    validate_body(maps:get(body, Params, <<>>));
validate(_) ->
    ok.

validate_body(Body) when is_binary(Body), byte_size(Body) =< ?MAX_CODEPOINTS * 4 ->
    case unicode:characters_to_list(Body, utf8) of
        Characters when is_list(Characters), length(Characters) =< ?MAX_CODEPOINTS ->
            ok;
        Characters when is_list(Characters) ->
            {error, {invalid_argument, body_too_long}};
        _ ->
            {error, {invalid_argument, body_invalid_utf8}}
    end;
validate_body(Body) when is_binary(Body) ->
    {error, {invalid_argument, body_too_long}};
validate_body(_) ->
    {error, {invalid_argument, body}}.
