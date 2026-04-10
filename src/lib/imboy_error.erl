%%%-------------------------------------------------------------------
%%% @doc
%%% 统一错误处理和参数验证模块
%%%
%%% 本模块提供统一的错误处理和参数验证功能，遵循 DRY 原则。
%%% 封装了 API 层常见的验证和错误响应模式，减少重复代码。
%%%
%%% @author Imboy Team
%%% @copyright 2026 Imboy Project
%%% @end
%%%-------------------------------------------------------------------

-module(imboy_error).

%% 参数验证导出
-export([validate_required/2, validate_required/3]).
-export([validate_id/2]).
-export([validate_id_list/2]).

%% 错误响应导出
-export([missing_param/2]).
-export([invalid_id/2]).
-export([not_found/3]).

%% 错误消息导出
-export([error_msg/1]).

-include("error_code.hrl").
-include("log.hrl").

%% ===================================================================
%% 参数验证函数
%% ===================================================================

%% @doc 验证必需参数是否存在且非空
%%
%% 检查参数是否存在且非空（非 undefined 且非空二进制/字符串）。
%% 如果验证失败，直接返回错误响应。
%%
%% @param Req Cowboy 请求对象
%% @param Field 字段名（atom），对应 query string 中的键
%% @returns {ok, Value} | {error, cowboy_req:req()}
%%
%% @example
%% case imboy_error:validate_required(Req, account) of
%%     {ok, Account} -> ...
%% end.
-spec validate_required(cowboy_req:req(), atom()) ->
    {ok, binary() | term()} | {error, cowboy_req:req()}.
validate_required(Req, Field) ->
    validate_required(Req, Field, fun(Value) -> Value =/= undefined andalso Value =/= <<>> end).

%% @doc 验证必需参数是否满足条件
%%
%% 检查参数是否存在且满足指定的验证函数。
%% 如果验证失败，直接返回错误响应。
%%
%% @param Req Cowboy 请求对象
%% @param Field 字段名（atom），对应 query string 中的键
%% @param ValidateFun 验证函数，接收参数值返回 boolean()
%% @returns {ok, Value} | {error, cowboy_req:req()}
%%
%% @example
%% % 验证参数必须为数字
%% case imboy_error:validate_required(Req, age, fun is_integer/1) of
%%     {ok, Age} -> ...
%% end.
-spec validate_required(cowboy_req:req(), atom(), fun((term()) -> boolean())) ->
    {ok, term()} | {error, cowboy_req:req()}.
validate_required(Req, Field, ValidateFun) ->
    Value = cowboy_req:match_qs([{Field, [], undefined}], Req),
    FieldValue = maps:get(Field, Value),
    case ValidateFun(FieldValue) of
        true -> {ok, FieldValue};
        false -> {error, missing_param(Req, Field)}
    end.

%% @doc 验证 ID 格式
%%
%% 验证 ID 是否为有效格式，并转换为整数。
%% 如果验证失败，直接返回错误响应。
%%
%% @param Req Cowboy 请求对象
%% @param Id 要验证的 ID
%% @returns {ok, DecodedId} | {error, cowboy_req:req()}
%% @returns DecodedId 转换后的整数 ID
%%
%% @example
%% case imboy_error:validate_id(Req, Gid) of
%%     {ok, Gid2} -> ...
%% end.
-spec validate_id(cowboy_req:req(), binary()) ->
    {ok, integer()} | {error, cowboy_req:req()}.
validate_id(Req, Id) when is_binary(Id) ->
    case ec_cnv:to_integer(Id) of
        0 -> {error, invalid_id(Req, Id)};
        DecodedId -> {ok, DecodedId}
    end;
validate_id(Req, _Id) ->
    {error, invalid_id(Req, <<"invalid">>)}.

%% @doc 验证 ID 列表
%%
%% 验证 ID 列表中的每个 ID 是否为有效格式。
%% 如果有任何一个 ID 无效，返回错误响应。
%%
%% @param Req Cowboy 请求对象
%% @param IdList ID 列表
%% @returns {ok, DecodedIdList} | {error, cowboy_req:req()}
%%
%% @example
%% case imboy_error:validate_id_list(Req, [<<"522dzx">>, <<"3k9mn2">>]) of
%%     {ok, [1, 2]} -> ...
%% end.
-spec validate_id_list(cowboy_req:req(), [binary()]) ->
    {ok, [integer()]} | {error, cowboy_req:req()}.
validate_id_list(Req, IdList) when is_list(IdList) ->
    DecodedList = lists:map(fun(Id) -> ec_cnv:to_integer(Id) end, IdList),
    case lists:member(0, DecodedList) of
        true -> {error, invalid_id(Req, <<"invalid_id_in_list">>)};
        false -> {ok, DecodedList}
    end.

%% ===================================================================
%% 错误响应函数
%% ===================================================================

%% @doc 返回缺少参数错误响应
%%
%% @param Req Cowboy 请求对象
%% @param Field 缺失的字段名（atom 或 binary）
%% @returns 更新后的 Cowboy 请求对象
-spec missing_param(cowboy_req:req(), atom() | binary()) -> cowboy_req:req().
missing_param(Req, Field) when is_atom(Field) ->
    FieldBin = atom_to_binary(Field, utf8),
    Msg = <<FieldBin/binary, " 必须"/utf8>>,
    elib_response:error(Req, elib_cnv:safe_to_binary(Msg));
missing_param(Req, Field) when is_binary(Field) ->
    Msg = <<Field/binary, " 必须"/utf8>>,
    elib_response:error(Req, elib_cnv:safe_to_binary(Msg)).

%% @doc 返回 ID 格式错误响应
%%
%% @param Req Cowboy 请求对象
%% @param Id 无效的 ID
%% @returns 更新后的 Cowboy 请求对象
-spec invalid_id(cowboy_req:req(), binary()) -> cowboy_req:req().
invalid_id(Req, _Id) ->
    elib_response:error(Req, <<"ID 格式有误"/utf8>>).

%% @doc 返回资源未找到错误响应
%%
%% @param Req Cowboy 请求对象
%% @param ResourceType 资源类型（如 <<"用户"/utf8>>, <<"群组"/utf8>>）
%% @param Id 资源 ID
%% @returns 更新后的 Cowboy 请求对象
-spec not_found(cowboy_req:req(), binary(), binary()) -> cowboy_req:req().
not_found(Req, ResourceType, Id) ->
    Msg = <<ResourceType/binary, " 不存在: "/utf8, Id/binary>>,
    elib_response:error(Req, elib_cnv:safe_to_binary(Msg)).

%% @doc 根据错误码获取错误消息
%%
%% 从 ERROR_MSG_MAP 中查找对应的错误消息。
%% 如果未找到，返回默认错误消息。
%%
%% @param Code 错误码（整数）
%% @returns 错误消息（binary）
%%
%% @example
%% Msg = imboy_error:error_msg(?ERR_BAD_REQUEST).
-spec error_msg(integer()) -> binary().
error_msg(Code) when is_integer(Code) ->
    case maps:get(Code, ?ERROR_MSG_MAP, undefined) of
        undefined ->
            <<"未知错误"/utf8>>;
        Msg ->
            Msg
    end.
