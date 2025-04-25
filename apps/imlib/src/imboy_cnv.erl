-module(imboy_cnv).

%%%
% cnv 是 convert 的缩写，概念上基础 ec_cnv 模块
% 如果 /deps/erlware_commons/src/ec_cnv.erl 模块有的方法就，使用它
%
% imboy_cnv:json_maybe(12345).
% imboy_cnv:json_maybe(<<"[1,2,3]">>).
%%%

-export([json_maybe/1]).

-export([zipwith_equery/1]).
-export([implode/2]).
-export([remove_dups/1]).
-export([vsn_major/1]).
-export([map_to_query/1]).
-export([list_to_binary_string/1]).

% imboy_cnv:convert_at_timestamps(List).
-export([convert_at_timestamps/1]).



% imboy_cnv:map_to_query(#{d=>4, a => 1, b => 2, c => 3}).
map_to_query(Map) ->
    Pairs = [[ec_cnv:to_list(Key), "=", ec_cnv:to_list(Value)] || {Key, Value} <- maps:to_list(Map)],
    list_to_binary(string:join(Pairs, "&")).


% imboy_cnv:list_to_binary_string([513251,62829,62825]).
list_to_binary_string(IntList) ->
    % 将整数列表转换为字符串列表
    StringList = lists:map(fun(I) -> integer_to_list(I) end, IntList),
    % 使用逗号连接字符串列表
    JoinedString = lists:join(",", StringList),
    % 将字符串转换为二进制
    list_to_binary(JoinedString).

zipwith_equery(Res) ->
    case Res of
        {ok, _, []} ->
            [];
        {ok, ColumnLi, Items0} ->
            Items1 = [tuple_to_list(Item) || Item <- Items0],
            [lists:zipwith(fun(X, Y) -> {X, Y} end, ColumnLi, Row) || Row <- Items1];
        _ ->
            []
    end.

% 如果是json类型的字符串，就decode，否则保持原数据类型
json_maybe(Val) ->
    case jsx:is_json(Val) of
        true ->
            jsx:decode(Val);
        false ->
            Val
    end.


% 用字符串连接数组元素，类似 php 的 implode/2 方法
% imboy_cnv:implode(",", [<<"a">>, "b"]).
% imboy_cnv:implode("','", [<<"a">>, "b"]).
% imboy_cnv:implode(",", [1,2,3.3]).   // <<"1,2,3.3">>
-spec implode([binary() | list() | float() | integer()], list()) -> binary().
implode(S, Li) when is_float(S) ->
    implode(io_lib:format("~p", [S]), Li);
implode(S, Li) when is_integer(S) ->
    implode(integer_to_binary(S), Li);
implode(Separator, Li) ->
    Li2 = [ [Separator, ec_cnv:to_binary(I)] || I <- Li ],
    iolist_to_binary(string:replace(iolist_to_binary(Li2), Separator, "")).


% 从 list 中移除重复的元素
remove_dups([]) ->
    [];
remove_dups([H | T]) ->
    [H | [ X || X <- remove_dups(T), X /= H ]].


vsn_major(Vsn) ->
    Major2 = case ec_semver:parse(Vsn) of
        {{Major, _, _, _}, _} ->
            Major;
        {{Major, _, _}, _} ->
            Major;
        {{Major, _}, _} ->
            Major;
        {Major, _} when is_integer(Major) ->
            Major;
        {Major, _} ->
            Major
    end,
    ec_cnv:to_binary(Major2).

%% @doc 递归处理数据结构，转换以_at结尾的字段时间格式
%% 支持处理Map、Proplist和嵌套结构
-spec convert_at_timestamps(any()) -> any().
convert_at_timestamps([]) ->
    [];
convert_at_timestamps(Map) when is_map(Map) andalso map_size(Map) == 0 ->
    #{};
convert_at_timestamps(Map) when is_map(Map) ->
    maps:fold(fun(K, V, Acc) ->
        Acc#{K => process_key_value(K, V)}
    end, #{}, Map);

convert_at_timestamps([{_, _}|_] = Proplist) ->
    [{K, process_key_value(K, V)} || {K, V} <- Proplist];

convert_at_timestamps(List) when is_list(List) ->
    [process_item(Elem) || Elem <- List];

convert_at_timestamps(Value) -> Value.

%% @doc 处理列表中的单个元素
process_item({K, V}) -> {K, process_key_value(K, V)};
process_item(Item) when is_map(Item); is_list(Item) ->
    convert_at_timestamps(Item);
process_item(Item) -> Item.

%% @doc 处理键值对
process_key_value(Key, Value) ->
    K = ec_cnv:to_binary(Key),
    case imboy_str:endswith(<<"_at">>, K) or imboy_str:endswith(<<"_ts">>, K) of
        true  -> convert_timestamp(Value);       % 时间字段转换
        false -> convert_structured(Value)      % 结构化数据处理
    end.

%% @doc 处理结构化数据的递归转换
convert_structured(V) when is_map(V); is_list(V) ->
    convert_at_timestamps(V);
convert_structured(V) -> V.

%% @doc 时间格式转换（RFC3339 -> 毫秒时间戳）
convert_timestamp(Value) when is_binary(Value); is_list(Value) ->
    case imboy_dt:rfc3339_to(Value, millisecond) of
        {error, _} ->
            Value;
        Num -> Num
    end;
convert_timestamp(Value) when is_tuple(Value) ->
    case imboy_dt:datetime_to(Value, millisecond) of
        {error, _} ->
            Value;
        Num -> Num
    end;
convert_timestamp(Value) -> Value.  % 非时间字符串保持原样
