-module(elib_retry_config).
%%%
% elib_retry_config 是消息重试间隔配置模块
% 提供统一的重试间隔管理，支持通过宏定义默认值
%%%

-include("chat.hrl").

-export([intervals/1]).

%%% 消息重试间隔配置（单位：毫秒）
%%% 默认策略：0ms立即投递 -> 5s -> 7s -> 11s -> 17s停止
-define(MSG_RETRY_DELAYS_C2C, [0, 5000, 7000, 11000, 17000]).
-define(MSG_RETRY_DELAYS_C2G, [0, 3500, 7000, 11000, 17000]).
-define(MSG_RETRY_DELAYS_C2S, [0, 5000, 7000, 11000]).
-define(MSG_RETRY_DELAYS_S2C, [0, 1500, 1500, 3000, 5000, 7000]).
-define(MSG_RETRY_DELAYS_PULL, [8000, 10000, 20000]).
-define(MSG_RETRY_DELAYS_NOTICE, [0, 5000, 10000]).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 获取指定类型的重试间隔列表
% elib_retry_config:intervals(<<"s2c">>)
%% @param Type 消息类型 (<<"c2c">>, <<"c2g">>, <<"c2s">>, <<"pull">>, <<"notice">>)
%% @returns 重试间隔列表（毫秒）
-spec intervals(binary()) -> [non_neg_integer()].
intervals(<<"c2c">>) -> ?MSG_RETRY_DELAYS_C2C;
intervals(<<"c2g">>) -> ?MSG_RETRY_DELAYS_C2G;
intervals(<<"c2s">>) -> ?MSG_RETRY_DELAYS_C2S;
intervals(<<"s2c">>) -> ?MSG_RETRY_DELAYS_S2C;
intervals(<<"pull">>) -> ?MSG_RETRY_DELAYS_PULL;
intervals(<<"notice">>) -> ?MSG_RETRY_DELAYS_NOTICE;
intervals(_) -> [0, 5000, 7000, 11000].  % 默认值
