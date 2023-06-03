-module(imboy_hashids).

%%%
% hashids 转换器
%
% imboy_hashids:uid_encode(12345)
% imboy_hashids:uid_decode(<<"bxyxr9">>)
% imboy_hashids:replace_id(list())
%%%

-export([uid_encode/1]).
-export([uid_decode/1]).
-export([replace_id/1]).

-define (uid_alphabet, "123456789abcdefghijkmnpqrstuvwxyz").

-spec replace_id(list()) -> list().
replace_id(Li) ->
    Id = proplists:get_value(<<"id">>, Li),
    [{<<"id">>, imboy_hashids:uid_encode(Id)} |
     proplists:delete(<<"id">>, Li)].


-spec uid_encode(integer() | binary() | list()) -> binary().
uid_encode(Id) when is_binary(Id) ->
    uid_encode(binary_to_integer(Id));
uid_encode(Id) when is_list(Id) ->
    uid_encode(list_to_integer(Id));
uid_encode(Id) ->
    Salt = imboy_func:env(hashids_salt, ""),
    Ctx = hashids:new([{min_hash_length, 6},
                       {default_alphabet, ?uid_alphabet},
                       {salt, Salt}]),
    list_to_binary(hashids:encode(Ctx, [Id])).


-spec uid_decode(list() | binary()) -> integer().
uid_decode(Id) when is_binary(Id) ->
    uid_decode(binary_to_list(Id));
uid_decode(Id) ->
    try
        Salt = imboy_func:env(hashids_salt, ""),
        Ctx = hashids:new([{min_hash_length, 6},
                           {default_alphabet, ?uid_alphabet},
                           {salt, Salt}]),
        hashids:decode(Ctx, Id)
    of
        [Uid] ->
            Uid;
        [] ->
            0
    catch
        _ ->
            0
    end.
