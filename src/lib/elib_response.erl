-module(elib_response).
%%%
% API响应JSON数据构造模块
%%%
-export([success/4, success/1, success/2, success/3]).
-export([error/4, error/1, error/2, error/3]).
-export([error_with_status/4]).
-export([error_with_code/2, error_with_code/3]).
-export([handle_logic_result/2, handle_logic_result_with/4]).
-export([json_decode_field/2, json_decode_list_field/2]).

%% ===================================================================
%% API Functions
%% ===================================================================

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
-spec success(cowboy_req:req(), map()) -> cowboy_req:req().
success(Req, Payload0) ->
    Payload = elib_cnv:convert_at_timestamps(Payload0),
    reply_json(0, "success", Payload, Req).

%% @doc 返回成功响应，带自定义负载和消息
%% @param Req cowboy请求对象
%% @param Payload0 响应数据
%% @param Msg 响应消息
%% @returns cowboy_req:req() 更新后的请求对象
-spec success(cowboy_req:req(), map(), binary() | list()) -> cowboy_req:req().
success(Req, Payload0, Msg) ->
    Payload = elib_cnv:convert_at_timestamps(Payload0),
    reply_json(0, Msg, Payload, Req).

%% @doc 返回成功响应，带自定义负载、消息和选项
%% @param Req cowboy请求对象
%% @param Payload0 响应数据
%% @param Msg 响应消息
%% @param Options 额外选项
%% @returns cowboy_req:req() 更新后的请求对象
-spec success(cowboy_req:req(), map(), binary() | list(), map()) -> cowboy_req:req().
success(Req, Payload0, Msg, Options) ->
    %% 转换时间字段
    Payload = elib_cnv:convert_at_timestamps(Payload0),
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
-spec error(cowboy_req:req(), binary() | list(), integer(), map()) -> cowboy_req:req().
error(Req, Msg, Code, Options) ->
    reply_json(Code, Msg, #{}, Req, Options).

%% @doc 返回带 HTTP 状态码的错误响应。
%% 普通业务错误继续使用 HTTP 200 + envelope code；认证/协议边界错误
%% 可使用此函数发送真实 HTTP 状态码，同时保持相同的 JSON envelope。
%% @param Req cowboy 请求对象
%% @param HttpStatus HTTP 状态码
%% @param Msg 错误消息
%% @param Code envelope 中的业务错误码
%% @returns cowboy_req:req() 更新后的请求对象
-spec error_with_status(cowboy_req:req(), pos_integer(), binary() | list(), integer()) ->
    cowboy_req:req().
error_with_status(Req, HttpStatus, Msg, Code) ->
    reply_json_with_status(HttpStatus, Code, Msg, #{}, Req, #{}).

%% @doc 尝试解析指定字段的JSON字符串为结构化数据
%% @param Row 数据行（map 或 proplists:proplist() 格式）
%% @param Field 要解析的字段名
%% @returns 解析后的数据行，如果解析失败则保持原样
-spec json_decode_field(map(), any()) -> map().
json_decode_field(Row, Field) when is_map(Row) ->
    case maps:get(Field, Row, undefined) of
        Payload when is_binary(Payload), Payload =/= <<>> ->
            % 【改进】快速检查是否可能是 JSON，避免无效解析
            case is_potential_json(Payload) of
                true ->
                    try
                        % 尝试解析 JSON 字符串为 map
                        DecodedPayload = jsone:decode(Payload, [{object_format, map}]),
                        % 替换原来的 payload 字段
                        maps:put(Field, DecodedPayload, Row)
                    catch
                        Class:Reason:_ ->
                            % 【改进】降级日志级别，添加更少冗余信息
                            logger:warning(
                                "Failed to decode JSON field ~p: ~p:~p~nPreview: ~p",
                                [Field, Class, Reason, preview_binary(Payload, 100)]
                            ),
                            % 如果解析失败，保持原样
                            Row
                    end;
                false ->
                    % 不是 JSON 格式，直接返回原数据
                    Row
            end;
        _ ->
            % 如果没有 payload 字段或不是二进制，保持原样
            Row
    end.

%% @doc 批量解析列表中所有记录的指定 JSON 字段
%% @param Rows 数据行列表（map 或 proplists:proplist() 格式）
%% @param Field 要解析的字段名
%% @returns 解析后的数据行列表
-spec json_decode_list_field
    (list(), any()) -> list();
    (any(), any()) -> any().
json_decode_list_field(Rows, Field) when is_list(Rows) ->
    [json_decode_field(Row, Field) || Row <- Rows];
json_decode_list_field(Rows, _Field) ->
    Rows.

%% ===================================================================
%% Internal Functions
%% ===================================================================

%% @doc 生成JSON响应核心函数（不带额外选项）
%% @param Code 响应码
%% @param Msg 响应消息
%% @param Payload 响应数据
%% @param Req cowboy请求对象
%% @returns cowboy_req:req() 更新后的请求对象
-spec reply_json(integer(), binary() | list(), map() | list(), cowboy_req:req()) ->
    cowboy_req:req().
reply_json(Code, Msg, Payload, Req) ->
    reply_json(Code, Msg, Payload, Req, #{}).

%% @doc 生成JSON响应核心函数（带额外选项）
%% @param Code 响应码
%% @param Msg 响应消息
%% @param Payload 响应数据
%% @param Req cowboy请求对象
%% @param Options 额外选项
%% @returns cowboy_req:req() 更新后的请求对象
-spec reply_json(integer(), binary() | list(), map() | list(), cowboy_req:req(), map()) ->
    cowboy_req:req().
reply_json(Code, Msg, Payload, Req, Options) ->
    reply_json_with_status(200, Code, Msg, Payload, Req, Options).

%% @doc 生成带指定 HTTP 状态码的 JSON 响应。
-spec reply_json_with_status(
    pos_integer(), integer(), binary() | list(), map() | list(), cowboy_req:req(), map()
) -> cowboy_req:req().
reply_json_with_status(HttpStatus, Code, Msg, Payload, Req, Options) ->
    Msg2 =
        if
            is_list(Msg) ->
                unicode:characters_to_binary(Msg);
            true ->
                elib_cnv:safe_to_binary(Msg)
        end,
    BasePayload = #{
        <<"code">> => Code,
        <<"msg">> => Msg2,
        <<"sv_ts">> => elib_dt:millisecond(),
        <<"payload">> => Payload
    },

    OptionsMap =
        case Options of
            Map when is_map(Map) ->
                Map;
            _ ->
                #{}
        end,
    %% 固化 envelope 核心字段，避免外部 options 覆盖协议字段
    ExtraOptions = maps:without(
        [<<"code">>, <<"msg">>, <<"sv_ts">>, <<"payload">>],
        OptionsMap
    ),
    JsonBody = jsone:encode(maps:merge(BasePayload, ExtraOptions), [native_utf8]),

    %% 发送响应
    cowboy_req:reply(
        HttpStatus,
        #{
            <<"content-type">> => <<"application/json; charset=utf-8">>,
            <<"Referrer-Policy">> => <<"strict-origin-when-cross-origin">>
        },
        JsonBody,
        Req
    ).

%% ===================================================================
%% Internal functions
%% ===================================================================

%% @doc 检查二进制数据是否可能是 JSON 格式
%% @param Bin 二进制数据
%% @returns true 如果可能是 JSON，false 如果明显不是
-spec is_potential_json(binary()) -> boolean().
is_potential_json(<<>>) ->
    false;
% 超过 100KB，不太可能是 JSON
is_potential_json(Bin) when byte_size(Bin) > 1024 * 100 ->
    false;
is_potential_json(<<First:8, _Rest/binary>>) when First =:= ${; First =:= $[ ->
    true;
% 允许前导空格
is_potential_json(<<"  ", Rest/binary>>) ->
    is_potential_json(Rest);
% 允许前导换行
is_potential_json(<<"\n", Rest/binary>>) ->
    is_potential_json(Rest);
% 允许前导 CRLF
is_potential_json(<<"\r\n", Rest/binary>>) ->
    is_potential_json(Rest);
is_potential_json(_) ->
    false.

%% @doc 生成二进制数据的预览（避免日志过长）
%% @param Bin 二进制数据
%% @param MaxLen 最大长度
%% @returns 预览字符串
-spec preview_binary(binary(), pos_integer()) -> binary().
preview_binary(Bin, MaxLen) when byte_size(Bin) =< MaxLen ->
    Bin;
preview_binary(Bin, MaxLen) ->
    <<Prefix:MaxLen/binary, _Rest/binary>> = Bin,
    <<Prefix/binary, "...">>.

%% ===================================================================
%% Logic 层结果处理辅助函数
%% ===================================================================

%% @doc 统一处理 Logic 层的返回结果
%% 自动匹配 {ok, Data} 或 {error, Msg} 并返回对应的响应
%% @param Req cowboy请求对象
%% @param Result Logic层返回结果 {ok, map()} | {error, binary()}
%% @returns cowboy_req:req() 更新后的请求对象
-spec handle_logic_result(
    cowboy_req:req(),
    {ok, map()} | {error, binary() | list()} | {error, binary() | list(), integer()}
) -> cowboy_req:req().
handle_logic_result(Req, {ok, Data}) ->
    ?MODULE:success(Req, Data);
handle_logic_result(Req, {error, Msg, Code}) when is_integer(Code) ->
    elib_response:error(Req, Msg, Code);
handle_logic_result(Req, {error, Msg}) ->
    elib_response:error(Req, Msg).

%% @doc 统一处理 Logic 层的返回结果，支持数据增强
%% 自动匹配 {ok, Data} 或 {error, Msg} 并返回对应的响应
%% @param Req cowboy请求对象
%% @param Result Logic层返回结果 {ok, map()} | {error, binary()}
%% @param EnrichFun 数据增强函数，用于在成功时额外处理数据
%% @param Options 额外选项
%% @returns cowboy_req:req() 更新后的请求对象
-spec handle_logic_result_with(
    cowboy_req:req(),
    {ok, map()} | {error, binary() | list()},
    function(),
    map()
) -> cowboy_req:req().
handle_logic_result_with(Req, {ok, Data}, EnrichFun, Options) ->
    EnrichedData = EnrichFun(Data),
    ?MODULE:success(Req, EnrichedData, <<"success.">>, Options);
handle_logic_result_with(Req, {error, Msg}, _EnrichFun, _Options) ->
    elib_response:error(Req, Msg).

%% @doc 使用错误码宏返回错误响应
%% @param Req cowboy请求对象
%% @param ErrorCode 错误码（使用 error_code.hrl 中定义的宏）
%% @returns cowboy_req:req() 更新后的请求对象
-spec error_with_code(cowboy_req:req(), integer()) -> cowboy_req:req().
error_with_code(Req, ErrorCode) ->
    elib_response:error(Req, <<"操作失败"/utf8>>, ErrorCode).

%% @doc 使用错误码宏返回错误响应（带自定义消息）
%% @param Req cowboy请求对象
%% @param Msg 错误消息
%% @param ErrorCode 错误码（使用 error_code.hrl 中定义的宏）
%% @returns cowboy_req:req() 更新后的请求对象
-spec error_with_code(cowboy_req:req(), binary() | list(), integer()) -> cowboy_req:req().
error_with_code(Req, Msg, ErrorCode) ->
    elib_response:error(Req, Msg, ErrorCode).
