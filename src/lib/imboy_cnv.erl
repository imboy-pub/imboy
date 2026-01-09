-module(imboy_cnv).

%%%
% cnv 是 convert 的缩写，概念上基础 ec_cnv 模块
% 如果 /deps/erlware_commons/src/ec_cnv.erl 模块有的方法就，使用它
%
% imboy_cnv:json_maybe(12345).
% imboy_cnv:json_maybe(<<"[1,2,3]">>).
%%%

-export([json_maybe/1]).

-export([implode/2]).
-export([remove_dups/1]).
-export([vsn_major/1]).
-export([map_to_query/1]).
-export([list_to_binary_string/1]).
-export([safe_to_binary/1]).

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

% 如果是json类型的字符串，就decode，否则保持原数据类型
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


% 用字符串连接数组元素，类似 php 的 implode/2 方法
% imboy_cnv:implode(",", [<<"a">>, "b"]).
% imboy_cnv:implode("','", [<<"a">>, "b"]).
% imboy_cnv:implode(",", [1,2,3.3]).   // <<"1,2,3.3">>
-spec implode(binary() | [binary() | list() | float() | integer()], list()) -> binary().
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
        true  -> imboy_dt:rfc3339_to(Value);       % 时间字段转换
        false -> convert_structured(Value)      % 结构化数据处理
    end.

%% @doc 处理结构化数据的递归转换
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
