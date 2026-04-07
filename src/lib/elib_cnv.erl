-module(elib_cnv).

%%%
% cnv 是 convert 的缩写，概念上基础 ec_cnv 模块
% 如果 /deps/erlware_commons/src/ec_cnv.erl 模块有的方法就，使用它
%
% elib_cnv:json_maybe(12345).
% elib_cnv:json_maybe(<<"[1,2,3]">>).
%%%

-export([json_maybe/1]).
-export([id_to_binary/1]).

-export([implode/2]).
-export([remove_dups/1]).
-export([vsn_major/1]).
-export([map_to_query/1]).
-export([list_to_binary_string/1]).
-export([safe_to_binary/1]).

% elib_cnv:convert_at_timestamps(List).
-export([convert_at_timestamps/1]).

%% @doc 将 ID 转换为 binary 字符串，用于 API 响应
%%
%% @deprecated 不再使用。ID 现在直接以 integer 返回客户端，不做字符串转换。
%% 保留仅用于向后兼容。
%%
-spec id_to_binary(integer() | binary()) -> binary().
id_to_binary(Id) when is_integer(Id) ->
    integer_to_binary(Id);
id_to_binary(Id) when is_binary(Id) ->
    Id;
id_to_binary(Id) when is_list(Id) ->
    list_to_binary(Id);
id_to_binary(Id) ->
    ec_cnv:to_binary(Id).

%% @doc 将 map 转换为 URL 查询字符串
%% @param Map 键值对映射
%% @returns URL 查询字符串（如 <<"a=1&b=2">>）
-spec map_to_query(map()) -> binary().



%% @example
%% elib_cnv:map_to_query(#{d=>4, a => 1, b => 2, c => 3}).
map_to_query(Map) ->
    Pairs = [[ec_cnv:to_list(Key), "=", ec_cnv:to_list(Value)] || {Key, Value} <- maps:to_list(Map)],
    list_to_binary(string:join(Pairs, "&")).


%% @doc 将整数列表转换为逗号分隔的二进制字符串
%% @param IntList 整数列表
%% @returns 逗号分隔的二进制字符串（如 <<"513251,62829,62825">>）
-spec list_to_binary_string(list(integer())) -> binary().
list_to_binary_string(IntList) ->
    % 将整数列表转换为字符串列表
    StringList = lists:map(fun(I) -> integer_to_list(I) end, IntList),
    % 使用逗号连接字符串列表
    JoinedString = lists:join(",", StringList),
    % 将字符串转换为二进制
    list_to_binary(JoinedString).

%% @doc 如果是 JSON 类型的字符串则解码，否则保持原数据类型
%% @param B 输入数据（binary、list 或其他类型）
%% @returns 解码后的 map 或原始数据
-spec json_maybe(binary() | list() | term()) -> map() | binary() | list() | term().
json_maybe(B) when is_binary(B) ->
    case B of
        <<${, _/binary>> -> jsone:decode(B, [{object_format, map}]);
        <<$[, _/binary>> -> jsone:decode(B, [{object_format, map}]);
        _ -> B
    end;
json_maybe(S) when is_list(S) ->
    B = iolist_to_binary(S),
    case B of
        <<${, _/binary>> -> jsone:decode(B, [{object_format, map}]);
        <<$[, _/binary>> -> jsone:decode(B, [{object_format, map}]);
        _ -> S
    end;
json_maybe(Val) ->
    Val.


%% @doc 用字符串连接数组元素，类似 PHP 的 implode 函数
%% @param S 分隔符（支持 float、integer、binary）
%% @param Li 要连接的列表
%% @returns 连接后的二进制字符串
%% @example
%% elib_cnv:implode(",", [<<"a">>, "b"]).
%% elib_cnv:implode("','", [<<"a">>, "b"]).
%% elib_cnv:implode(",", [1,2,3.3]).  %% <<"1,2,3.3">>
-spec implode(binary() | [binary() | list() | float() | integer()], list()) -> binary().
implode(S, Li) when is_float(S) ->
    implode(io_lib:format("~p", [S]), Li);
implode(S, Li) when is_integer(S) ->
    implode(integer_to_binary(S), Li);
implode(Separator, Li) ->
    Li2 = [ [Separator, ec_cnv:to_binary(I)] || I <- Li ],
    iolist_to_binary(string:replace(iolist_to_binary(Li2), Separator, "")).


%% @doc 从 list 中移除重复的元素
%% @param List 输入列表
%% @returns 去重后的列表
-spec remove_dups(list()) -> list().
remove_dups([]) ->
    [];
remove_dups([H | T]) ->
    [H | [ X || X <- remove_dups(T), X /= H ]].


%% @doc 从版本号中提取主版本号
%% @param Vsn 版本号（binary、integer 或 tuple 格式）
%% @returns 主版本号的二进制表示
-spec vsn_major(binary() | integer() | tuple()) -> binary().
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
-spec process_item(any()) -> any().
process_item({K, V}) -> {K, process_key_value(K, V)};
process_item(Item) when is_map(Item); is_list(Item) ->
    convert_at_timestamps(Item);
process_item(Item) -> Item.

%% @doc 处理键值对
-spec process_key_value(any(), any()) -> any().
process_key_value(Key, Value) ->
    K = ec_cnv:to_binary(Key),
    case elib_str:endswith(<<"_at">>, K) or elib_str:endswith(<<"_ts">>, K) of
        true  -> elib_dt:rfc3339_to(Value);       % 时间字段转换
        false -> convert_structured(Value)      % 结构化数据处理
    end.

%% @doc 处理结构化数据的递归转换
-spec convert_structured(any()) -> any().
convert_structured(V) when is_map(V); is_list(V) ->
    convert_at_timestamps(V);
convert_structured(V) -> V.

%% @doc 安全地将任意类型转换为二进制，支持复杂的错误结构
%% 优先使用 ec_cnv:to_binary 处理基本类型，遇到复杂结构时特殊处理
%% @param Term 任意类型的数据
%% @returns 二进制格式的错误消息
-spec safe_to_binary(any()) -> binary().
safe_to_binary(Term) ->
    try
        % 优先使用成熟的 ec_cnv:to_binary 处理基本类型
        ec_cnv:to_binary(Term)
    catch
        error:function_clause ->
            % 直接将复杂结构转换为字符串表示，简洁实用
            erlang:iolist_to_binary(io_lib:format("~p", [Term]));
        error:badarg ->
            % 处理 iolist_to_binary 的 badarg 错误，使用兜底转换方案
            erlang:list_to_binary(lists:flatten(io_lib:format("~p", [Term])));
        Class:Reason:Stacktrace ->
            % 兜底处理其他所有异常情况
            error_logger:warning_msg("safe_to_binary unexpected error: ~p:~p~nStacktrace: ~p~nInput: ~p~n",
                                   [Class, Reason, Stacktrace, Term]),
            erlang:list_to_binary(lists:flatten(io_lib:format("~p", [Term])))
    end.
