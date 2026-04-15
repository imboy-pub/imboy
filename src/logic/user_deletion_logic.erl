-module(user_deletion_logic).
%%%===================================================================
%%% @doc 账号注销自动清理 GenServer
%%%
%%% 功能：
%%% - 定期扫描 status=2（申请注销中）且超过 retention_days 的用户
%%% - 自动删除用户的所有关联数据
%%% - 支持手动触发清理
%%% - 用户数据导出（注销前提供数据快照）
%%%
%%% 配置 (sys.config):
%%% - {user_deletion_enabled, true}              % 启用自动删除（默认 false）
%%% - {user_deletion_interval, 86400000}          % 扫描间隔，默认 24 小时
%%% - {user_deletion_retention_days, 60}          % 保留天数，默认 60 天
%%% - {user_deletion_batch_size, 10}              % 每批处理上限
%%%===================================================================

-behaviour(gen_server).

-include("log.hrl").

%% API
-export([start_link/0]).
-export([cleanup_now/0]).
-export([get_status/0]).
-export([export_user_data/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(DEFAULT_INTERVAL, 86400000).   % 24 小时
-define(DEFAULT_RETENTION_DAYS, 60).
-define(DEFAULT_BATCH_SIZE, 10).

-record(state, {
    interval :: non_neg_integer(),
    retention_days :: pos_integer(),
    batch_size :: pos_integer(),
    last_cleanup :: erlang:timestamp() | undefined,
    total_deleted :: non_neg_integer()
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 手动触发清理
-spec cleanup_now() -> {ok, non_neg_integer()} | {error, term()}.
cleanup_now() ->
    gen_server:call(?MODULE, cleanup_now, 60000).

%% @doc 获取清理状态
-spec get_status() -> map().
get_status() ->
    gen_server:call(?MODULE, get_status).

%% @doc 导出用户数据（用于注销前的数据快照）
%% @param Uid 用户ID
%% @return {ok, ExportData} | {error, Reason}
-spec export_user_data(integer()) -> {ok, map()} | {error, term()}.
export_user_data(Uid) ->
    user_ds:export_data(Uid).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Enabled = application:get_env(imboy, user_deletion_enabled, false),
    case Enabled of
        true ->
            Interval = application:get_env(imboy, user_deletion_interval, ?DEFAULT_INTERVAL),
            RetDays = application:get_env(imboy, user_deletion_retention_days, ?DEFAULT_RETENTION_DAYS),
            BatchSize = application:get_env(imboy, user_deletion_batch_size, ?DEFAULT_BATCH_SIZE),
            State = #state{
                interval = Interval,
                retention_days = RetDays,
                batch_size = BatchSize,
                last_cleanup = undefined,
                total_deleted = 0
            },
            %% 首次延迟 60 秒执行
            erlang:send_after(60000, self(), do_cleanup),
            ok = ?INFO_LOG([user_deletion_logic, started, #{
                interval => Interval,
                retention_days => RetDays,
                batch_size => BatchSize
            }]),
            {ok, State};
        false ->
            ok = ?INFO_LOG([user_deletion_logic, disabled]),
            {ok, #state{
                interval = 0,
                retention_days = ?DEFAULT_RETENTION_DAYS,
                batch_size = 0,
                last_cleanup = undefined,
                total_deleted = 0
            }}
    end.

handle_call(cleanup_now, _From, State) ->
    {Result, NewState} = do_cleanup_internal(State),
    {reply, Result, NewState};

handle_call(get_status, _From, State) ->
    Status = #{
        interval => State#state.interval,
        retention_days => State#state.retention_days,
        batch_size => State#state.batch_size,
        last_cleanup => State#state.last_cleanup,
        total_deleted => State#state.total_deleted
    },
    {reply, Status, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(do_cleanup, #state{interval = 0} = State) ->
    {noreply, State};
handle_info(do_cleanup, State) ->
    {_Result, NewState} = do_cleanup_internal(State),
    erlang:send_after(State#state.interval, self(), do_cleanup),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

do_cleanup_internal(State) ->
    try
        RetDays = State#state.retention_days,
        BatchSize = State#state.batch_size,
        Deleted = delete_expired_users(RetDays, BatchSize),
        case Deleted > 0 of
            true ->
                ok = ?INFO_LOG([user_deletion_cleanup, #{deleted => Deleted, retention_days => RetDays}]);
            false ->
                ok
        end,
        NewState = State#state{
            last_cleanup = erlang:timestamp(),
            total_deleted = State#state.total_deleted + Deleted
        },
        {{ok, Deleted}, NewState}
    catch
        _:Error ->
            ok = ?ERROR_LOG([user_deletion_cleanup_error, Error]),
            {{error, Error}, State}
    end.

%% @doc 查找并删除超过保留期的注销申请用户
delete_expired_users(RetentionDays, BatchSize) ->
    case user_ds:find_expired_logout_users(integer_to_binary(RetentionDays), BatchSize) of
        {ok, Rows} when is_list(Rows), length(Rows) > 0 ->
            lists:foldl(fun(#{<<"id">> := Uid}, Acc) ->
                try
                    ok = user_ds:delete_all_related_data(Uid),
                    ok = ?INFO_LOG([user_deleted, #{uid => Uid}]),
                    Acc + 1
                catch
                    _:Err ->
                        ok = ?ERROR_LOG([user_deletion_failed, #{uid => Uid, error => Err}]),
                        Acc
                end
            end, 0, Rows);
        {ok, []} ->
            0;
        {error, Reason} ->
            ok = ?ERROR_LOG([user_deletion_query_failed, #{retention_days => RetentionDays, error => Reason}]),
            0
    end.
