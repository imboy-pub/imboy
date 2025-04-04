-module(imboy_response).
%%%
% API响应JSON数据构造模块
%%%
-export([success/4, success/1, success/2, success/3]).
-export([error/4, error/1, error/2, error/3]).
-export([page_payload/4]).

% imboy_response:convert_at_timestamps(List).
% -export([convert_at_timestamps/1]).

%% ===================================================================
%% API Functions
%% ===================================================================

%% @doc 构建分页数据结构
%% Total: 总记录数 | Page: 当前页码 | Size: 每页数量 | List: 数据列表
-spec page_payload(integer(), integer(), integer(), list()) -> list().
page_payload(Total, Page, Size, List) ->
    % io:format("List1 ~p~n", [List]),
    List2 = convert_at_timestamps(List),
    % io:format("List2 ~p~n", [List2]),
    [{<<"total">>, Total}, {<<"page">>, Page}, {<<"size">>, Size}, {<<"list">>, List2}].

%% Success系列函数
success(Req) ->
    reply_json(0, "success", #{}, Req).

success(Req, Payload0) ->
    Payload = convert_at_timestamps(Payload0),
    io:format("Payload0 ~p~n", [Payload0]),
    reply_json(0, "success", Payload, Req).

success(Req, Payload0, Msg) ->
    Payload = convert_at_timestamps(Payload0),
    reply_json(0, Msg, Payload, Req).

success(Req, Payload0, Msg, Options) ->
    %% 转换时间字段
    Payload = convert_at_timestamps(Payload0),
    reply_json(0, Msg, Payload, Req, Options).

%% Error系列函数
error(Req) ->
    reply_json(1, "error", #{}, Req).

error(Req, Msg) ->
    reply_json(1, Msg, #{}, Req).

error(Req, Msg, Code) ->
    reply_json(Code, Msg, #{}, Req).

error(Req, Msg, Code, Options) ->
    reply_json(Code, Msg, #{}, Req, Options).

%% ===================================================================
%% Internal Functions
%% ===================================================================

%% @doc 生成JSON响应核心函数
reply_json(Code, Msg, Payload, Req) ->
    reply_json(Code, Msg, Payload, Req, []).

reply_json(Code, Msg, Payload, Req, Options) ->
    Msg2 = if
         is_list(Msg) == false ->
            ec_cnv:to_binary(Msg);
        true ->
            unicode:characters_to_binary(Msg)
    end,
    io:format("reply_json Payload ~p~n", [Payload]),
    %% 构造响应主体
    BasePayload = [
        {<<"code">>, Code},
        {<<"msg">>, Msg2},
        {<<"payload">>, Payload}
    ],

    %% 合并额外选项并编码
    JsonBody = jsone:encode(BasePayload ++ Options, [native_utf8]),

    %% 发送响应
    cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json; charset=utf-8">>},
        JsonBody,
        Req).

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
    % io:format("~p, ~p, ~p~n", [Key, imboy_type:is_at_key(Key), Value]),
    case imboy_type:is_at_key(Key) of
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
