
-module(imboy_hashids).

%%%
% hashids 转换器
%
% imboy_hashids:encode(12345)
% imboy_hashids:decode(<<"522dzx">>).
% imboy_hashids:replace_id(list())
%%%

-export([encode/1, encode/4]).
-export([decode/1]).
-export([encode_hex/1, decode_hex/1]).
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
    Salt = config_ds:env(hashids_salt, "imboy"),
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

%% @doc 带自定义参数的编码函数
%% @param Id 要编码的数字
%% @param Salt 自定义盐值
%% @param Alphabet 自定义字母表
%% @param MinLength 最小长度
%% @return 编码后的二进制字符串
-spec encode(integer(), string() | binary(), string(), integer()) -> binary().
encode(Id, Salt, Alphabet, MinLength) when is_integer(Id) ->
    CustomCtx = hashids:new([
        {min_hash_length, MinLength},
        {default_alphabet, Alphabet},
        {salt, Salt}
    ]),
    list_to_binary(hashids:encode(CustomCtx, [Id])).

%% @doc 十六进制编码函数（与encode/1功能相同，接口兼容）
%% @param Id 要编码的正整数或非空列表
%% @return 编码后的二进制字符串
%% @throws function_clause 当 Ids 为空列表或负数时
-spec encode_hex(integer() | list()) -> binary().
encode_hex(Id) when is_integer(Id), Id >= 0 ->
    encode(Id);
encode_hex([]) ->
    erlang:error(function_clause);  % 空列表不被支持
encode_hex(Ids) when is_list(Ids), length(Ids) > 0 ->
    list_to_binary(hashids:encode(?CTX, Ids)).

%% @doc 十六进制解码函数（与decode/1功能相同，支持列表）
%% @param Encoded 编码后的字符串
%% @return 解码后的整数或列表
-spec decode_hex(binary() | list()) -> integer() | list().
decode_hex(Encoded) when is_binary(Encoded) ->
    decode_hex(binary_to_list(Encoded));
decode_hex(Encoded) ->
    try
        case hashids:decode(?CTX, Encoded) of
            [Num] -> Num;
            List when is_list(List) -> List;
            _     -> 0
        end
    catch
        _:_ -> 0
    end.
