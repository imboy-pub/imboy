-module(e2ee_shard_validator).

%% E2EE 分片传输审计日志桩（stub）。
%% 真实实现由插件或外部模块提供；此处仅保证调用不崩溃，dialyzer 可分析返回类型。

-export([log_shard_transmission/3]).

-spec log_shard_transmission(atom(), term(), map()) -> ok.
log_shard_transmission(_Event, _ShardId, _Meta) ->
    ok.
