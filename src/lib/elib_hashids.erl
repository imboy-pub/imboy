
-module(elib_hashids).

%%%
% hashids 转换器
%
% elib_hashids:encode(12345)
% elib_hashids:decode(<<"522dzx">>).
% elib_hashids:replace_id(list())
%%%

-export([encode/1, encode/4]).
-export([decode/1]).
-export([encode_hex/1, decode_hex/1]).
-export([replace_id/1, replace_id/2]).
-export([replace_fields/2]).


% persistent_term:get({elib_hashids, salt}).
% persistent_term:get({elib_hashids, ctx}).
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
% elib_hashids:encode(67)
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

-spec replace_id(list() | map()) -> list() | map().
replace_id(Li)->
    replace_id(Li, <<"id">>).

-spec replace_id(list(), binary()) -> list(); (map(), binary()) -> map().
replace_id(Li, K) when is_list(Li) ->
    case lists:keytake(K, 1, Li) of
        false ->
            Li;
        {value, {K, Id}, Rest} ->
            [{K, maybe_encode(Id)} | Rest]
    end;
replace_id(M, K) when is_map(M) ->
    case maps:is_key(K, M) of
        true ->
            Id = maps:get(K, M),
            maps:put(K, maybe_encode(Id), M);
        _ ->
            M
    end.

%% @doc 批量替换 map 中多个字段的 ID 为编码后的值
%%
%% 对 map 中的指定字段进行批量 ID 编码替换，只对存在的字段进行处理。
%% 如果字段不存在或值为 0，则跳过该字段。
%%
%% @param Map 输入的 map
%% @param Fields 要替换的字段列表
%% @returns 替换后的 map
%%
%% @example
%% > G = #{id => 1, creator_uid => 2, owner_uid => 3, name => <<"test">>}.
%% > elib_hashids:replace_fields(G, [<<"id">>, <<"creator_uid">>, <<"owner_uid">>]).
%% #{id => <<"522dzx">>, creator_uid => <<"3k9mn2">>, owner_uid => <<"4j8np3">>, name => <<"test">>}.
-spec replace_fields(map(), [binary()]) -> map().
replace_fields(Map, Fields) when is_map(Map), is_list(Fields) ->
    lists:foldl(fun(Field, AccMap) ->
        case maps:is_key(Field, AccMap) of
            true ->
                Id = maps:get(Field, AccMap),
                % 仅对可安全转换为整数的字段进行编码，避免重复编码导致崩溃
                case to_integer(Id) of
                    {ok, 0} ->
                        AccMap;
                    {ok, IntId} ->
                        maps:put(Field, encode(IntId), AccMap);
                    error ->
                        AccMap
                end;
            false ->
                AccMap
        end
    end, Map, Fields).

-spec maybe_encode(term()) -> term().
maybe_encode(Id) ->
    case to_integer(Id) of
        {ok, IntId} ->
            encode(IntId);
        error ->
            Id
    end.

-spec to_integer(term()) -> {ok, integer()} | error.
to_integer(Id) when is_integer(Id) ->
    {ok, Id};
to_integer(Id) when is_binary(Id) ->
    try binary_to_integer(Id) of
        IntId ->
            {ok, IntId}
    catch
        _:_ ->
            error
    end;
to_integer(Id) when is_list(Id) ->
    try list_to_integer(Id) of
        IntId ->
            {ok, IntId}
    catch
        _:_ ->
            error
    end;
to_integer(_) ->
    error.

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
            List when is_list(List) -> List
        end
    catch
        _:_ -> 0
    end.
