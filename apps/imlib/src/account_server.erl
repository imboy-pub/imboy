-module(account_server).

%% 自定义日志宏（替换原imlib中的日志宏）
-define(LOG_ERROR(Format), error_logger:error_msg(Format)).
-define(LOG_ERROR(Format, Args), error_logger:error_msg(Format, Args)).
-define(LOG_INFO(Format), error_logger:info_msg(Format)).
-define(LOG_INFO(Format, Args), error_logger:info_msg(Format, Args)).
-define(LOG_WARNING(Format), error_logger:warning_msg(Format)).
-define(LOG_WARNING(Format, Args), error_logger:warning_msg(Format, Args)).
-define(LOG_DEBUG(Format), error_logger:info_msg("[DEBUG] " ++ Format)).
-define(LOG_DEBUG(Format, Args), error_logger:info_msg("[DEBUG] " ++ Format, Args)).

-behaviour(gen_server).

%% API
-export([start_link/0, allocate/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% 定义常量
-define(DEFAULT_START_ID, 50000).        % 默认起始账号ID
-define(DEFAULT_BATCH_SIZE, 1000).       % 默认批量预分配数量
-define(MAX_RETRIES, 3).                 % 最大重试次数
-define(ALLOC_TIMEOUT, 5000).            % 分配超时时间(毫秒)
-define(DB_RETRY_INTERVAL, 30000).      % 数据库重试间隔(毫秒)

%% 服务器状态记录
-record(state, {
    start = ?DEFAULT_START_ID,           % 当前起始ID
    batch_size = ?DEFAULT_BATCH_SIZE,    % 批量大小
    ids = [],                            % 预分配的ID池(乱序)
    db_available = true,                 % 数据库是否可用
    db_check_ref = undefined             % 数据库检查定时器引用
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

allocate() ->
    try
        gen_server:call(?MODULE, allocate, ?ALLOC_TIMEOUT)
    catch
        exit:{timeout, _} ->
            ?LOG_ERROR("Account allocation timeout"),
            {error, timeout};
        Exit:Reason ->
            ?LOG_ERROR("Account allocation failed: ~p:~p", [Exit, Reason]),
            {error, Reason}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    rand:seed(exsplus),
    case safe_get_max_account_id() of
        {ok, StartId} ->
            ?LOG_INFO("Account server started with DB connection, start_id=~p", [StartId]),
            {ok, #state{
                start = StartId,
                ids = create_rand_list(StartId, ?DEFAULT_BATCH_SIZE),
                db_available = true
            }};
        {error, Reason} ->
            ?LOG_WARNING("Failed to connect to DB, using default start_id. Reason: ~p", [Reason]),
            {ok, #state{
                start = ?DEFAULT_START_ID,
                ids = create_rand_list(?DEFAULT_START_ID, ?DEFAULT_BATCH_SIZE),
                db_available = false,
                db_check_ref = schedule_db_check()
            }}
    end.

handle_call(allocate, _From, State = #state{ids = [], start = Start, batch_size = Size}) ->
    NewStart = Start + Size,
    NewIds = create_rand_list(NewStart, Size),
    ?LOG_DEBUG("Allocated new ID batch: ~p IDs starting from ~p", [Size, NewStart]),
    {reply, NewStart, State#state{start = NewStart, ids = tl(NewIds)}};

handle_call(allocate, _From, State = #state{ids = [Id|Rest]}) ->
    {reply, Id, State#state{ids = Rest}};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(stop, State) ->
    cancel_db_check_timer(State#state.db_check_ref),
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_db_availability, State) ->
    case safe_get_max_account_id() of
        {ok, MaxId} when not State#state.db_available ->
            ?LOG_INFO("DB connection restored. Max account id: ~p", [MaxId]),
            NewStart = max(MaxId + 1, State#state.start),
            NewIds = create_rand_list(NewStart, State#state.batch_size),
            {noreply, State#state{
                start = NewStart,
                ids = NewIds,
                db_available = true,
                db_check_ref = undefined
            }};
        {ok, _} ->
            {noreply, State};
        {error, Reason} ->
            ?LOG_WARNING("DB still unavailable: ~p", [Reason]),
            {noreply, State#state{
                db_check_ref = schedule_db_check()
            }}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    cancel_db_check_timer(State#state.db_check_ref),
    ?LOG_INFO("Account server stopping"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

safe_get_max_account_id() ->
    try
        case imboy_db:pluck(<<"user">>, <<"max(CAST(account as integer)) as max">>, ?DEFAULT_START_ID) of
            null ->
                {ok, ?DEFAULT_START_ID};
            Num when is_integer(Num) andalso Num >= ?DEFAULT_START_ID ->
                {ok, Num};
            _ ->
                {ok, ?DEFAULT_START_ID}
        end
    catch
        Exit:Reason ->
            ?LOG_ERROR("Database query failed: ~p:~p", [Exit, Reason]),
            {error, Reason}
    end.

create_rand_list(Start, Len) ->
    List = lists:seq(Start, Start + Len - 1),
    Shuffled = [X || {_,X} <- lists:sort([{rand:uniform(), N} || N <- List])],
    Shuffled.

schedule_db_check() ->
    erlang:send_after(?DB_RETRY_INTERVAL, self(), check_db_availability).

cancel_db_check_timer(undefined) ->
    ok;
cancel_db_check_timer(Ref) ->
    erlang:cancel_timer(Ref),
    ok.
