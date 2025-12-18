-module(imboy_response).
%%%
% API响应JSON数据构造模块
%%%
-export([success/4, success/1, success/2, success/3]).
-export([error/4, error/1, error/2, error/3]).
-export([page_payload/4]).
-export([json_decode_field/2]).

%% ===================================================================
%% API Functions
%% ===================================================================

%% @doc 构建分页数据结构
%% Total: 总记录数 | Page: 当前页码 | Size: 每页数量 | List: 数据列表
-spec page_payload(integer(), integer(), integer(), list()) -> list().
page_payload(Total, Page, Size, List) ->
    % io:format("List1 ~p~n", [List]),
    List2 = imboy_cnv:convert_at_timestamps(List),
    % io:format("List2 ~p~n", [List2]),
    [{<<"total">>, Total}, {<<"page">>, Page}, {<<"size">>, Size}, {<<"list">>, List2}].

%% Success系列函数

%% @doc 返回成功响应，使用默认消息
%% @param Req cowboy请求对象
%% @returns cowboy_req:req() 更新后的请求对象
-spec success(cowboy_req:req()) -> cowboy_req:req().
success(Req) ->
    reply_json(0, "success", #{}, Req).

%% @doc 返回成功响应，带自定义负载
%% @param Req cowboy请求对象
%% @param Payload0 响应数据
%% @returns cowboy_req:req() 更新后的请求对象
-spec success(cowboy_req:req(), map() | list()) -> cowboy_req:req().
success(Req, Payload0) ->
    Payload = imboy_cnv:convert_at_timestamps(Payload0),
    % io:format("Payload0 ~p~n", [Payload0]),
    reply_json(0, "success", Payload, Req).

%% @doc 返回成功响应，带自定义负载和消息
%% @param Req cowboy请求对象
%% @param Payload0 响应数据
%% @param Msg 响应消息
%% @returns cowboy_req:req() 更新后的请求对象
-spec success(cowboy_req:req(), map() | list(), binary() | list()) -> cowboy_req:req().
success(Req, Payload0, Msg) ->
    Payload = imboy_cnv:convert_at_timestamps(Payload0),
    reply_json(0, Msg, Payload, Req).

%% @doc 返回成功响应，带自定义负载、消息和选项
%% @param Req cowboy请求对象
%% @param Payload0 响应数据
%% @param Msg 响应消息
%% @param Options 额外选项
%% @returns cowboy_req:req() 更新后的请求对象
-spec success(cowboy_req:req(), map() | list(), binary() | list(), list()) -> cowboy_req:req().
success(Req, Payload0, Msg, Options) ->
    %% 转换时间字段
    Payload = imboy_cnv:convert_at_timestamps(Payload0),
    reply_json(0, Msg, Payload, Req, Options).

%% Error系列函数

%% @doc 返回错误响应，使用默认错误消息
%% @param Req cowboy请求对象
%% @returns cowboy_req:req() 更新后的请求对象
-spec error(cowboy_req:req()) -> cowboy_req:req().
error(Req) ->
    reply_json(1, "error", #{}, Req).

%% @doc 返回错误响应，带自定义消息
%% @param Req cowboy请求对象
%% @param Msg 错误消息
%% @returns cowboy_req:req() 更新后的请求对象
-spec error(cowboy_req:req(), binary() | list()) -> cowboy_req:req().
error(Req, Msg) ->
    reply_json(1, Msg, #{}, Req).

%% @doc 返回错误响应，带自定义消息和错误码
%% @param Req cowboy请求对象
%% @param Msg 错误消息
%% @param Code 错误码
%% @returns cowboy_req:req() 更新后的请求对象
-spec error(cowboy_req:req(), binary() | list(), integer()) -> cowboy_req:req().
error(Req, Msg, Code) ->
    reply_json(Code, Msg, #{}, Req).

%% @doc 返回错误响应，带自定义消息、错误码和选项
%% @param Req cowboy请求对象
%% @param Msg 错误消息
%% @param Code 错误码
%% @param Options 额外选项
%% @returns cowboy_req:req() 更新后的请求对象
-spec error(cowboy_req:req(), binary() | list(), integer(), list()) -> cowboy_req:req().
error(Req, Msg, Code, Options) ->
    reply_json(Code, Msg, #{}, Req, Options).

%% @doc 尝试解析指定字段的JSON字符串为结构化数据
%% @param Row 数据行（属性列表格式）
%% @param Field 要解析的字段名
%% @returns 解析后的数据行，如果解析失败则保持原样
-spec json_decode_field(proplists:proplist(), any()) -> proplists:proplist().
json_decode_field(Row, Field) ->
    case lists:keyfind(Field, 1, Row) of
        {Field, Payload} when is_binary(Payload) ->
            try
                % 尝试解析 JSON 字符串为 map
                DecodedPayload = jsone:decode(Payload, [{object_format, proplist}]),
                % 替换原来的 payload 字段
                lists:keyreplace(Field, 1, Row, {Field, DecodedPayload})
            catch
                _:_ ->
                    % 如果解析失败，保持原样
                    Row
            end;
        _ ->
            % 如果没有 payload 字段或不是二进制，保持原样
            Row
    end.

%% ===================================================================
%% Internal Functions
%% ===================================================================

%% @doc 生成JSON响应核心函数（不带额外选项）
%% @param Code 响应码
%% @param Msg 响应消息
%% @param Payload 响应数据
%% @param Req cowboy请求对象
%% @returns cowboy_req:req() 更新后的请求对象
-spec reply_json(integer(), binary() | list(), map() | list(), cowboy_req:req()) -> cowboy_req:req().
reply_json(Code, Msg, Payload, Req) ->
    reply_json(Code, Msg, Payload, Req, []).

%% @doc 生成JSON响应核心函数（带额外选项）
%% @param Code 响应码
%% @param Msg 响应消息
%% @param Payload 响应数据
%% @param Req cowboy请求对象
%% @param Options 额外选项
%% @returns cowboy_req:req() 更新后的请求对象
-spec reply_json(integer(), binary() | list(), map() | list(), cowboy_req:req(), list()) -> cowboy_req:req().
reply_json(Code, Msg, Payload, Req, Options) ->
    % io:format("reply_json Payload ~p~n", [Payload]),
    %% 构造响应主体
    BasePayload = [
        {<<"code">>, Code},
        {<<"msg">>, imboy_cnv:safe_to_binary(Msg)},
        {<<"sv_ts">>, imboy_dt:millisecond()},
%%        {<<"request_id">>, imboy_dt:millisecond()},
        {<<"payload">>, Payload}
    ],

    %% 合并额外选项并编码
    JsonBody = jsone:encode(BasePayload ++ Options, [native_utf8]),

    %% 发送响应
    cowboy_req:reply(200,
        #{
            <<"content-type">> => <<"application/json; charset=utf-8">>,
            <<"Referrer-Policy">> => <<"strict-origin-when-cross-origin">>
        },
        JsonBody,
        Req).
