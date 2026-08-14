-module(imboy_plugin_audit_ds).

%%%-------------------------------------------------------------------
%%% @doc
%%% 插件审计日志数据服务层 / Plugin audit log data service
%%% lifecycle.md §11
%%% 异步写入，不阻塞 statem / Async write, non-blocking to statem.
%%%
%%% @author Imboy Team
%%% @copyright 2026 Imboy Project
%%%-------------------------------------------------------------------

-export([write/1, list/3, count/1]).

-include("log.hrl").

%% @doc 异步写入审计日志（fire-and-forget）。
%% 失败仅打日志，不阻塞调用方。
-spec write(map()) -> ok.
write(Event) when is_map(Event) ->
    _ = elib_async:async(fun() ->
        case write_sync(Event) of
            {ok, _} -> ok;
            {error, Reason} -> ?WARN_LOG([audit_write_failed, Reason])
        end
    end),
    ok.

%% @doc 同步写入审计日志。
-spec write_sync(map()) -> {ok, pos_integer()} | {error, term()}.
write_sync(Event) ->
    Now = os:system_time(millisecond),
    Row = maps:merge(#{started_at => Now, operator => <<"system">>}, Event),
    imboy_plugin_audit_repo:insert(Row).

%% @doc 查询审计日志。
-spec list(binary(), non_neg_integer(), non_neg_integer()) -> {ok, list()} | {error, term()}.
list(PluginName, Limit, Offset) ->
    imboy_plugin_audit_repo:list(PluginName, Limit, Offset).

%% @doc 统计审计日志总数。
-spec count(binary()) -> {ok, non_neg_integer()} | {error, term()}.
count(PluginName) ->
    imboy_plugin_audit_repo:count(PluginName).
