-module(imboy_response).
%%%
% API响应JSON数据构造模块
%%%
-export([success/4, success/1, success/2, success/3]).
-export([error/4, error/1, error/2, error/3]).
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
-spec success(cowboy_req:req(), map() | list(), binary() | list(), map()) -> cowboy_req:req().
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
-spec error(cowboy_req:req(), binary() | list(), integer(), map()) -> cowboy_req:req().
error(Req, Msg, Code, Options) ->
    reply_json(Code, Msg, #{}, Req, Options).

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
                            logger:warning("Failed to decode JSON field ~p: ~p:~p~nPreview: ~p",
                                       [Field, Class, Reason, preview_binary(Payload, 100)]),
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
-spec json_decode_list_field(list(), any()) -> list(); (any(), any()) -> any().
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
-spec reply_json(integer(), binary() | list(), map() | list(), cowboy_req:req()) -> cowboy_req:req().
reply_json(Code, Msg, Payload, Req) ->
    reply_json(Code, Msg, Payload, Req, #{}).

%% @doc 生成JSON响应核心函数（带额外选项）
%% @param Code 响应码
%% @param Msg 响应消息
%% @param Payload 响应数据
%% @param Req cowboy请求对象
%% @param Options 额外选项
%% @returns cowboy_req:req() 更新后的请求对象
-spec reply_json(integer(), binary() | list(), map() | list(), cowboy_req:req(), map()) -> cowboy_req:req().
reply_json(Code, Msg, Payload, Req, Options) ->
    Msg2 = if
         is_list(Msg) ->
            unicode:characters_to_binary(Msg);
         true ->
            imboy_cnv:safe_to_binary(Msg)
    end,
    % io:format("reply_json Payload ~p~n", [Payload]),
    BasePayload = #{
        <<"code">> => Code,
        <<"msg">> => Msg2,
        <<"sv_ts">> => imboy_dt:millisecond(),
        <<"payload">> => Payload
    },

    OptionsMap = case Options of
        Map when is_map(Map) ->
            Map;
        _ ->
            #{}
    end,
    JsonBody = jsone:encode(maps:merge(BasePayload, OptionsMap), [native_utf8]),

    %% 发送响应
    cowboy_req:reply(200,
        #{
            <<"content-type">> => <<"application/json; charset=utf-8">>,
            <<"Referrer-Policy">> => <<"strict-origin-when-cross-origin">>
        },
        JsonBody,
        Req).

%% ===================================================================
%% Internal functions
%% ===================================================================

%% @doc 检查二进制数据是否可能是 JSON 格式
%% @param Bin 二进制数据
%% @returns true 如果可能是 JSON，false 如果明显不是
-spec is_potential_json(binary()) -> boolean().
is_potential_json(<<>>) ->
    false;
is_potential_json(Bin) when byte_size(Bin) > 1024 * 100 ->  % 超过 100KB，不太可能是 JSON
    false;
is_potential_json(<<First:8, _Rest/binary>>) when First =:= ${; First =:= $[ ->
    true;
is_potential_json(<<"  ", Rest/binary>>) ->  % 允许前导空格
    is_potential_json(Rest);
is_potential_json(<<"\n", Rest/binary>>) ->  % 允许前导换行
    is_potential_json(Rest);
is_potential_json(<<"\r\n", Rest/binary>>) ->  % 允许前导 CRLF
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
