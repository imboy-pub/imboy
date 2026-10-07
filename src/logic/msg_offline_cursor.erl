-module(msg_offline_cursor).
-export([encode/1, decode/1]).

%% Preserve the database timestamp precision; the legacy millisecond cursor
%% cannot distinguish messages sharing the same timestamp.
encode(undefined) -> <<>>;
encode({At, Id}) -> base64:encode(jsone:encode(#{<<"v">> => 1, <<"at">> => At, <<"id">> => Id})).

decode(<<>>) ->
    {ok, undefined};
decode(Token) when is_binary(Token), byte_size(Token) =< 512 ->
    try
        decode_data(jsone:decode(base64:decode(Token)))
    catch
        _:_ -> {error, invalid_offline_cursor}
    end;
decode(_) ->
    {error, invalid_offline_cursor}.

decode_data(#{<<"v">> := 1, <<"at">> := At, <<"id">> := Id} = Data) when
    map_size(Data) =:= 3,
    is_binary(At),
    byte_size(At) =< 64,
    is_binary(Id),
    byte_size(Id) > 0,
    byte_size(Id) =< 40
->
    _ = calendar:rfc3339_to_system_time(binary_to_list(At), [{unit, microsecond}]),
    {ok, {At, Id}};
decode_data(_) ->
    {error, invalid_offline_cursor}.
