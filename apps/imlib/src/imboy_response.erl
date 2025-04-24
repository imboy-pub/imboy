-module(imboy_response).
%%%
% API响应JSON数据构造模块
%%%
-export([success/4, success/1, success/2, success/3]).
-export([error/4, error/1, error/2, error/3]).
-export([page_payload/4]).


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
success(Req) ->
    reply_json(0, "success", #{}, Req).

success(Req, Payload0) ->
    Payload = imboy_cnv:convert_at_timestamps(Payload0),
    io:format("Payload0 ~p~n", [Payload0]),
    reply_json(0, "success", Payload, Req).

success(Req, Payload0, Msg) ->
    Payload = imboy_cnv:convert_at_timestamps(Payload0),
    reply_json(0, Msg, Payload, Req).

success(Req, Payload0, Msg, Options) ->
    %% 转换时间字段
    Payload = imboy_cnv:convert_at_timestamps(Payload0),
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
