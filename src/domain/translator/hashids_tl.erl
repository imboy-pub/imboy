-module (hashids_tl).
%%%
% hashids_tl 是 hashids translator 缩写
% hashids_tl:uid_encode(12345)
% hashids_tl:uid_decode(<<"bxyxr9">>)
% hashids_tl:encode_id(list())
%%%

-export ([uid_encode/1]).
-export ([uid_decode/1]).
-export ([encode_id/1]).

-include("hashids.hrl").

-spec encode_id(list()) -> list().
encode_id(Li) ->
    Id = proplists:get_value(<<"id">>, Li),
    [{<<"id">>, hashids_tl:uid_encode(Id)} | proplists:delete(<<"id">>, Li)].

-spec uid_encode(integer()) -> binary().
uid_encode(Id) when is_binary(Id)  ->
    uid_encode(binary_to_integer(Id));
uid_encode(Id) when is_list(Id)  ->
    uid_encode(list_to_integer(Id));
uid_encode(Id) ->
    Ctx = hashids:new([
        {min_hash_length, 6},
        {default_alphabet, ?uid_alphabet},
        {salt, ?hashids_salt}
    ]),
    list_to_binary(hashids:encode(Ctx, [Id])).

-spec uid_decode(list() | binary()) -> integer().
uid_decode(Id) when is_binary(Id) ->
    uid_decode(binary_to_list(Id));
uid_decode(Id) ->
    Ctx = hashids:new([
        {min_hash_length, 6},
        {default_alphabet, ?uid_alphabet},
        {salt, ?hashids_salt}
    ]),
    [Uid] = hashids:decode(Ctx, Id),
    Uid.
