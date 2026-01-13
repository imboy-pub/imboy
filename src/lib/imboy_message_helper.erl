%%%-------------------------------------------------------------------
%%% @doc
%%% 消息构建与发送辅助函数
%%%
%%% 本模块统一消息构建、编码和发送的常见模式，遵循 DRY 原则。
%%% 封装了 JSON 编码、消息发送等重复性操作。
%%%
%%% @author Imboy Team
%%% @copyright 2026 Imboy Project
%%% @end
%%%-------------------------------------------------------------------

-module(imboy_message_helper).

-include("log.hrl").

%% JSON 编码
-export([encode_json/1]).

%% 消息构建与发送
-export([build_and_send/4]).
-export([build_and_send/5]).
-export([encode_and_send/4]).
-export([encode_and_send/5]).

%% ===================================================================
%% API Functions
%% ===================================================================

%% @doc 将 map 编码为 JSON（使用 native_utf8）
%%
%% 统一 JSON 编码方式，确保 UTF-8 字符串正确处理。
%% 替代直接调用 `jsone:encode(Map, [native_utf8])` 的重复模式。
%%
%% @param Map 要编码的数据
%% @returns JSON 二进制字符串
%%
%% @example
%% Msg = #{<<"hello">> => <<"世界"/utf8>>},
%% Json = imboy_message_helper:encode_json(Msg).
-spec encode_json(map()) -> binary().
encode_json(Map) when is_map(Map) ->
    jsone:encode(Map, [native_utf8]).

%% @doc 构建并发送消息（简化版）
%%
%% 统一消息发送流程：构建消息 → 编码JSON → 获取重试配置 → 发送。
%% 适用于需要立即构建并发送消息的场景。
%%
%% @param ToUid 目标用户ID
%% @param MsgId 消息ID
%% @param Msg 消息内容（map）
%% @param RetryType 重试类型（如 <<"c2c">>, <<"c2s">>）
%% @returns ok
%%
%% @example
%% Msg = message_ds:assemble_msg(<<"C2C">>, From, To, Payload, MsgId),
%% imboy_message_helper:build_and_send(ToId, MsgId, Msg, <<"c2c">>).
-spec build_and_send(pos_integer() | binary(), binary(), map(), binary()) -> ok.
build_and_send(ToUid, MsgId, Msg, RetryType) ->
    build_and_send(ToUid, MsgId, Msg, RetryType, []).

%% @doc 构建并发送消息（支持自定义重试间隔）
%%
%% 统一消息发送流程：构建消息 → 编码JSON → 发送。
%% 支持自定义重试间隔列表，不使用默认配置。
%%
%% @param ToUid 目标用户ID
%% @param MsgId 消息ID
%% @param Msg 消息内容（map）
%% @param RetryTypeOrIntervals 重试类型或自定义间隔列表
%% @param Options 选项（预留，暂未使用）
%% @returns ok
%%
%% @example
%% % 使用默认重试配置
%% imboy_message_helper:build_and_send(ToId, MsgId, Msg, <<"c2c">>, []).
%%
%% % 使用自定义重试间隔
%% Intervals = [2000, 5000, 7000],
%% imboy_message_helper:build_and_send(ToId, MsgId, Msg, Intervals, []).
-spec build_and_send(pos_integer() | binary(), binary(), map(),
                    binary() | [non_neg_integer()], list()) -> ok.
build_and_send(ToUid, MsgId, Msg, RetryTypeOrIntervals, _Options) when is_list(RetryTypeOrIntervals) ->
    % 自定义重试间隔
    MsgJson = encode_json(Msg),
    message_ds:send_next(ToUid, MsgId, MsgJson, RetryTypeOrIntervals);
build_and_send(ToUid, MsgId, Msg, RetryType, _Options) when is_binary(RetryType) ->
    % 使用配置的重试间隔
    MsgJson = encode_json(Msg),
    MsLi = elib_retry_config:intervals(RetryType),
    message_ds:send_next(ToUid, MsgId, MsgJson, MsLi).

%% @doc 编码并发送已构建的消息（使用默认重试配置）
%%
%% 对已构建好的消息进行 JSON 编码并发送。
%% 使用指定重试类型的默认配置。
%%
%% @param ToUid 目标用户ID
%% @param MsgId 消息ID
%% @param Msg 消息内容（map）
%% @param RetryType 重试类型（如 <<"c2c">>, <<"c2s">>）
%% @returns ok
%%
%% @example
%% Msg = message_ds:assemble_msg(<<"C2C">>, From, To, Payload, MsgId),
%% imboy_message_helper:encode_and_send(ToId, MsgId, Msg, <<"c2c">>).
-spec encode_and_send(pos_integer() | binary(), binary(), map(), binary()) -> ok.
encode_and_send(ToUid, MsgId, Msg, RetryType) ->
    encode_and_send(ToUid, MsgId, Msg, RetryType, []).

%% @doc 编码并发送已构建的消息（支持选项）
%%
%% 对已构建好的消息进行 JSON 编码并发送。
%% 支持自定义重试间隔列表。
%%
%% @param ToUid 目标用户ID
%% @param MsgId 消息ID
%% @param Msg 消息内容（map）
%% @param RetryTypeOrIntervals 重试类型或自定义间隔列表
%% @param Options 选项（预留，暂未使用）
%% @returns ok
%%
%% @example
%% % 使用默认重试配置
%% imboy_message_helper:encode_and_send(ToId, MsgId, Msg, <<"c2c">>, []).
%%
%% % 使用自定义重试间隔
%% Intervals = [2000, 5000, 7000],
%% imboy_message_helper:encode_and_send(ToId, MsgId, Msg, Intervals, []).
-spec encode_and_send(pos_integer() | binary(), binary(), map(),
                     binary() | [non_neg_integer()], list()) -> ok.
encode_and_send(ToUid, MsgId, Msg, RetryTypeOrIntervals, _Options) when is_list(RetryTypeOrIntervals) ->
    MsgJson = encode_json(Msg),
    message_ds:send_next(ToUid, MsgId, MsgJson, RetryTypeOrIntervals);
encode_and_send(ToUid, MsgId, Msg, RetryType, _Options) when is_binary(RetryType) ->
    MsgJson = encode_json(Msg),
    MsLi = elib_retry_config:intervals(RetryType),
    message_ds:send_next(ToUid, MsgId, MsgJson, MsLi).

%% ===================================================================
%% Internal Functions
%% ===================================================================
