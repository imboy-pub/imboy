-module(elib_metric).
%%%===================================================================
%%% @doc
%%% elib_metric - 简单的指标收集模块
%%%
%%% 功能：
%%% - 计数器（Counter）
%%% - 直方图（Histogram）
%%% - 指标导出
%%%
%%% 使用示例：
%%% ```
%%% ok = elib_metric:increment(request_count),
%%% ok = elib_metric:increment(request_count, 5),
%%% ok = elib_metric:record(response_time_ms, 125),
%%% Metrics = elib_metric:get_all_metrics()
%%% ```
%%%===================================================================

-behaviour(gen_server).

%% API 函数
-export([start_link/0]).
-export([increment/1]).
-export([increment/2]).
-export([increment/3]).
-export([record/2]).
-export([get_all_metrics/0]).
-export([reset/0]).
-export([reset_counter/1]).
-export([reset_histogram/1]).
-export([log_message_format/2]).

%% gen_server 回调
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ETS 表名
-define(METRICS_ETS, elib_metrics_ets).

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc 启动 elib_metric gen_server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 增加计数器（默认增加 1）
-spec increment(atom()) -> ok.
increment(Name) ->
    increment(Name, 1).

%% @doc 增加计数器（指定增量）
-spec increment(atom(), integer()) -> ok.
increment(Name, Delta) when is_atom(Name), is_integer(Delta), Delta > 0 ->
    gen_server:cast(?MODULE, {increment, Name, Delta}).

%% @doc 增加带标签的计数器
%% Labels 是 map，如 #{plugin => channel}
%% Delta 固定为 1；自定义 delta 请用 increment/3
-spec increment(atom(), integer(), map()) -> ok | {error, invalid_delta}.
increment(Name, Delta, Labels) when is_atom(Name), is_integer(Delta), Delta > 0, is_map(Labels) ->
    gen_server:cast(?MODULE, {increment_labeled, Name, Delta, Labels});
increment(_Name, Delta, _Labels) when is_integer(Delta), Delta =< 0 ->
    {error, invalid_delta}.

%% @doc 记录直方图数据
-spec record(atom(), number()) -> ok.
record(Name, Value) when is_atom(Name), is_number(Value) ->
    gen_server:cast(?MODULE, {record, Name, Value}).

%% @doc 获取所有指标
-spec get_all_metrics() -> #{
    counters => #{atom() => integer()},
    histograms => #{atom() => [map()]}
}.
get_all_metrics() ->
    gen_server:call(?MODULE, get_all_metrics).

%% @doc 重置所有指标
-spec reset() -> ok.
reset() ->
    gen_server:call(?MODULE, reset).

%% @doc 重置指定计数器
-spec reset_counter(atom()) -> ok.
reset_counter(Name) ->
    gen_server:call(?MODULE, {reset_counter, Name}).

%% @doc 重置指定直方图
-spec reset_histogram(atom()) -> ok.
reset_histogram(Name) ->
    gen_server:call(?MODULE, {reset_histogram, Name}).

%% @doc 记录 WebSocket 消息格式统计
%% 用于跟踪消息方向、类型、action、E2EE 状态分布
%%
%% @param Direction 消息方向：<<"in">> (接收) 或 <<"out">> (发送)
%% @param Msg 消息 map
%% @returns ok
%%
%% @example
%% ok = elib_metric:log_message_format(<<"in">>, IncomingMessage).
-spec log_message_format(binary(), map()) -> ok.
log_message_format(Direction, Msg) when is_binary(Direction), is_map(Msg) ->
    Type = maps:get(<<"type">>, Msg, <<"unknown">>),
    MsgType = maps:get(<<"msg_type">>, Msg, none),
    Action = maps:get(<<"action">>, Msg, none),
    E2EE = case maps:get(<<"e2ee">>, Msg, null) of
               null -> no_e2ee;
               _ -> has_e2ee
           end,

    % 记录消息类型计数
    TypeAtom = binary_to_existing_atom(Type, utf8),
    increment({msg_type, TypeAtom}),

    % 记录 msg_type �计数
    case MsgType of
        none -> ok;
        _ when is_binary(MsgType) ->
            MsgTypeAtom = binary_to_existing_atom(MsgType, utf8),
            increment({msg_content_type, MsgTypeAtom});
        _ -> ok
    end,

    % 记录 action 计数
    case Action of
        none -> ok;
        _ when is_binary(Action) ->
            ActionAtom = binary_to_existing_atom(Action, utf8),
            increment({msg_action, ActionAtom});
        _ -> ok
    end,

    % 记录 E2EE 状态计数
    increment({msg_e2ee, E2EE}),

    % 记录方向计数
    increment({msg_direction, Direction}),

    % 记录综合指标
    increment({msg_metric, Type, Direction, E2EE}),

    ok.

%%%===================================================================
%%% gen_server 回调函数
%%%===================================================================

%% @private
init([]) ->
    % 创建 ETS 表用于存储指标
    _ = ets:new(?METRICS_ETS, [
        named_table,
        set,
        public,
        {read_concurrency, true}
    ]),
    {ok, #{}}.

%% @private
handle_call(get_all_metrics, _From, State) ->
    % 获取所有计数器（无标签）
    Counters = ets:foldl(
        fun({{counter, Name}, Value}, Acc) ->
            Acc#{Name => Value};
           (_, Acc) ->
            Acc
        end,
        #{},
        ?METRICS_ETS
    ),

    % 获取所有带标签计数器（将排序列表转回 map 作为 key）
    LabeledCounters = ets:foldl(
        fun({{labeled_counter, Name, SortedLabels}, Value}, Acc) ->
            LabelsMap = maps:from_list(SortedLabels),
            Acc#{{Name, LabelsMap} => Value};
           (_, Acc) ->
            Acc
        end,
        #{},
        ?METRICS_ETS
    ),

    AllCounters = maps:merge(Counters, LabeledCounters),

    % 获取所有直方图
    Histograms = ets:foldl(
        fun({{histogram, Name}, Buckets}, Acc) ->
            Acc#{Name => lists:reverse(Buckets)};
           (_, Acc) ->
            Acc
        end,
        #{},
        ?METRICS_ETS
    ),

    {reply, #{counters => AllCounters, histograms => Histograms}, State};

handle_call(reset, _From, State) ->
    ets:delete_all_objects(?METRICS_ETS),
    {reply, ok, State};

handle_call({reset_counter, Name}, _From, State) ->
    ets:delete(?METRICS_ETS, {counter, Name}),
    {reply, ok, State};

handle_call({reset_histogram, Name}, _From, State) ->
    ets:delete(?METRICS_ETS, {histogram, Name}),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast({increment, Name, Delta}, State) ->
    Key = {counter, Name},
    NewValue = case ets:lookup(?METRICS_ETS, Key) of
        [{Key, Value}] -> Value + Delta;
        [] -> Delta
    end,
    ets:insert(?METRICS_ETS, {Key, NewValue}),
    {noreply, State};

handle_cast({increment_labeled, Name, Delta, Labels}, State) ->
    Key = {labeled_counter, Name, sort_labels(Labels)},
    NewValue = case ets:lookup(?METRICS_ETS, Key) of
        [{Key, Value}] -> Value + Delta;
        [] -> Delta
    end,
    ets:insert(?METRICS_ETS, {Key, NewValue}),
    {noreply, State};

handle_cast({record, Name, Value}, State) ->
    Key = {histogram, Name},
    % 简单的桶实现：记录值和计数
    Bucket = #{
        value => Value,
        count => 1,
        timestamp => erlang:system_time(millisecond)
    },
    NewBuckets = case ets:lookup(?METRICS_ETS, Key) of
        [{Key, Buckets}] -> [Bucket | Buckets];
        [] -> [Bucket]
    end,
    ets:insert(?METRICS_ETS, {Key, NewBuckets}),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================

%% @doc 将 labels map 排序为规范形式（用于 ETS key 的一致性）
-spec sort_labels(map()) -> [{atom(), atom()}].
sort_labels(Labels) ->
    lists:sort(maps:to_list(Labels)).
