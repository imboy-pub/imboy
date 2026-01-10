-module(imboy_retry_config).
%%%
% imboy_retry_config 是消息重试间隔配置模块
% 提供统一的重试间隔管理，支持通过宏定义默认值
%%%

-include("chat.hrl").

-export([intervals/1]).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 获取指定类型的重试间隔列表
%% @param Type 消息类型 (<<"c2c">>, <<"c2g">>, <<"c2s">>, <<"pull">>, <<"notice">>)
%% @returns 重试间隔列表（毫秒）
-spec intervals(binary()) -> [non_neg_integer()].
intervals(<<"c2c">>) -> ?MSG_RETRY_DELAYS_C2C;
intervals(<<"c2g">>) -> ?MSG_RETRY_DELAYS_C2G;
intervals(<<"c2s">>) -> ?MSG_RETRY_DELAYS_C2S;
intervals(<<"pull">>) -> ?MSG_RETRY_DELAYS_PULL;
intervals(<<"notice">>) -> ?MSG_RETRY_DELAYS_NOTICE;
intervals(_) -> [0, 5000, 7000, 11000].  % 默认值
