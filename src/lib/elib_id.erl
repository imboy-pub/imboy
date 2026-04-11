-module(elib_id).

%%% @doc ID 工具模块
%%% 提供唯一标识符生成和 TSID 字段序列化功能

-export([gen/0, gen/1]).
-export([tsid_to_bin/1, tsid_keys_to_bin/2]).

%% @doc 生成唯一标识符（无前缀）
-spec gen() -> binary().
gen() ->
    gen("").

%% @doc 生成带前缀的唯一标识符
-spec gen(integer() | list() | binary()) -> binary().
gen(Prefix) ->
    U1 = uid:encode64(uid:g()),
    iolist_to_binary([ec_cnv:to_binary(Prefix), U1]).

%% @doc 将 TSID 整数转为 binary 字符串；非整数值原样返回。
%%
%% Admin API 返回 TSID 时使用此函数，避免 JS 精度丢失。
%%
%% @example
%%   elib_id:tsid_to_bin(84442613760000001) => <<"84442613760000001">>
%%   elib_id:tsid_to_bin(<<"already_bin">>) => <<"already_bin">>
-spec tsid_to_bin(integer() | any()) -> binary() | any().
tsid_to_bin(Id) when is_integer(Id) ->
    integer_to_binary(Id);
tsid_to_bin(Value) ->
    Value.

%% @doc 将 map 中指定键的 TSID 整数值批量转为 binary 字符串。
%%
%% Keys 支持 atom 或 binary，与 map 实际键类型一致即可。
%% 非整数值或不存在的键跳过（不报错）。
%%
%% @example
%%   elib_id:tsid_keys_to_bin(
%%     #{<<"id">> => 84442613760000001, <<"name">> => <<"alice">>},
%%     [<<"id">>]
%%   ) => #{<<"id">> => <<"84442613760000001">>, <<"name">> => <<"alice">>}
%%
%%   elib_id:tsid_keys_to_bin(
%%     #{from_id => 84442613760000002, to_id => 84442613760000003},
%%     [from_id, to_id]
%%   ) => #{from_id => <<"84442613760000002">>, to_id => <<"84442613760000003">>}
-spec tsid_keys_to_bin(map(), [atom() | binary()]) -> map().
tsid_keys_to_bin(Map, Keys) when is_map(Map) ->
    lists:foldl(fun(Key, Acc) ->
        case maps:find(Key, Acc) of
            {ok, Value} when is_integer(Value) ->
                maps:put(Key, integer_to_binary(Value), Acc);
            _ ->
                Acc
        end
    end, Map, Keys).
