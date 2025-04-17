
-module(imboy_hashids).

%%%
% hashids 转换器
%
% imboy_hashids:encode(12345)
% imboy_hashids:decode(<<"522dzx">>).
% imboy_hashids:replace_id(list())
%%%

-export([encode/1]).
-export([decode/1]).
-export([replace_id/1, replace_id/2]).


% persistent_term:get({imboy_hashids, salt}).
% persistent_term:get({imboy_hashids, ctx}).
%% 定义宏时直接转换避免运行时计算
-define(UID_ALPHABET, "123456789abcdefghijkmnpqrstuvwxyz").
-define(SALT, (persistent_term:get({?MODULE, salt}))). % 盐值缓存
-define(CTX,  (persistent_term:get({?MODULE, ctx}))).  % 上下文缓存

%% 模块初始化时预计算上下文
-on_load(init/0).
init() ->
    Salt = config_ds:env(hashids_salt),
    Ctx = hashids:new([
        {min_hash_length, 6},
        {default_alphabet, ?UID_ALPHABET},
        {salt, Salt}
    ]),
    % 使用persistent_term实现全局缓存
    persistent_term:put({?MODULE, salt}, Salt),
    persistent_term:put({?MODULE, ctx}, Ctx),
    ok.

%% 统一类型处理接口
-spec encode(integer() | binary() | list()) -> binary().
encode(Id) when is_binary(Id) ->
    encode(binary_to_integer(Id));
encode(Id) when is_list(Id) ->
    encode(list_to_integer(Id));
encode(Id) ->
    list_to_binary(hashids:encode(?CTX, [Id])).

-spec decode(binary() | list()) -> integer().
decode(Id) when is_binary(Id) ->
    decode(binary_to_list(Id));
decode(Id) ->
    try
        case hashids:decode(?CTX, Id) of
            [Num] -> Num;
            _     -> 0
        end
    catch
        _:_ -> 0
    end.

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
