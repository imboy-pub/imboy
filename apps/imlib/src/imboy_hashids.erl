-module(imboy_hashids).

%%%
% hashids 转换器
%
% imboy_hashids:encode(12345)
% imboy_hashids:decode(<<"bxyxr9">>)
% imboy_hashids:replace_id(list())
%%%

-export([encode/1]).
-export([decode/1]).
-export([replace_id/1, replace_id/2]).

-define(uid_alphabet, "123456789abcdefghijkmnpqrstuvwxyz").



replace_id(Li)->
    replace_id(Li, <<"id">>).

-spec replace_id(list() | map(), binary()) -> list() | map().
replace_id(Li, K) when is_list(Li) ->
    case proplists:get_value(K, Li) of
        undefined ->
            Li;
        _ ->
            Id = proplists:get_value(K, Li),
            [{K, imboy_hashids:encode(Id)} | proplists:delete(K, Li)]
    end;
replace_id(M, K) when is_map(M) ->
    case maps:is_key(K, M) of
        true ->
            Id = maps:get(K, M),
            maps:put(K, imboy_hashids:encode(Id), M);
        _ ->
            M
    end.


-spec encode(integer() | binary() | list()) -> binary().
encode(Id) when is_binary(Id) ->
    encode(binary_to_integer(Id));
encode(Id) when is_list(Id) ->
    encode(list_to_integer(Id));
encode(Id) ->
    Salt = config_ds:env(hashids_salt, ""),
    Ctx = hashids:new([{min_hash_length, 6}, {default_alphabet, ?uid_alphabet}, {salt, Salt}]),
    list_to_binary(hashids:encode(Ctx, [Id])).


-spec decode(list() | binary()) -> integer().
decode(Id) when is_binary(Id) ->
    decode(binary_to_list(Id));
decode(Id) ->
    try
        Salt = config_ds:env(hashids_salt, ""),
        Ctx = hashids:new([{min_hash_length, 6}, {default_alphabet, ?uid_alphabet}, {salt, Salt}]),
        hashids:decode(Ctx, Id)
    of
        [Id2] ->
            Id2;
        [] ->
            0
    catch
        _:_ ->
            0
    end.
