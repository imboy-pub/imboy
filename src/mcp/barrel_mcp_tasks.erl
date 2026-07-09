%%%-------------------------------------------------------------------
%%% @doc Long-running operation registry (MCP tasks).
%%%
%%% Tools registered with `long_running => true' return immediately
%%% with a `taskId' instead of synchronously producing a result. The
%%% worker continues in the background; clients poll via
%%% `tasks/get', enumerate via `tasks/list', and abort via
%%% `tasks/cancel'. State transitions emit
%%% `notifications/tasks/status' on the session's SSE channel.
%%%
%%% Tasks live in a `protected' ETS table keyed by
%%% `{SessionId, TaskId}'. A periodic sweep evicts terminal tasks
%%% (success / error / cancelled) older than `?TASK_TTL'.
%%% @end
%%%-------------------------------------------------------------------
-module(barrel_mcp_tasks).

-behaviour(gen_server).

-export([
    start_link/0,
    create/3,
    get/2,
    list/2,
    cancel/2,
    finish/3,
    fail/3,
    set_worker/3
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-define(TABLE, barrel_mcp_tasks_table).
%% 1 hour
-define(TASK_TTL, 3600 * 1000).
%% 1 minute
-define(SWEEP_INTERVAL, 60 * 1000).

-record(task, {
    id :: binary(),
    session_id :: binary() | undefined,
    method :: binary(),
    %% Spec vocabulary (MCP 2025-11-25):
    %%   submitted | working | completed | failed | cancelled
    %% We don't model `submitted' today (workers start immediately),
    %% so the initial state is `working' and terminal states are
    %% `completed', `failed', `cancelled'.
    status :: working | completed | failed | cancelled,
    result :: term(),
    error :: term(),
    created_at :: integer(),
    updated_at :: integer(),
    worker_pid :: pid() | undefined,
    request_id :: integer() | binary() | undefined
}).

%%====================================================================
%% Public API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Create a new running task. Returns the task id.
-spec create(
    SessionId :: binary() | undefined,
    Method :: binary(),
    Opts :: map()
) -> {ok, binary()}.
create(SessionId, Method, _Opts) ->
    gen_server:call(?MODULE, {create, SessionId, Method}).

-spec get(SessionId :: binary() | undefined, TaskId :: binary()) ->
    {ok, map()} | {error, not_found}.
get(SessionId, TaskId) ->
    case ets:lookup(?TABLE, {SessionId, TaskId}) of
        [{_, Task}] -> {ok, task_to_map(Task)};
        [] -> {error, not_found}
    end.

-spec list(SessionId :: binary() | undefined, map()) -> {ok, [map()]}.
list(SessionId, _Opts) ->
    Tasks = ets:foldl(
        fun
            ({{S, _}, T}, Acc) when S =:= SessionId -> [task_to_map(T) | Acc];
            (_, Acc) -> Acc
        end,
        [],
        ?TABLE
    ),
    {ok, Tasks}.

%% @doc Mark a task as cancelled and notify the client. Sends
%% `{cancel, RequestId}' to the worker pid (if recorded) so
%% cooperative arity-2 handlers can abort.
-spec cancel(binary() | undefined, binary()) -> ok | {error, not_found}.
cancel(SessionId, TaskId) ->
    gen_server:call(?MODULE, {cancel, SessionId, TaskId}).

%% @doc Record the worker pid (and optional originating request id)
%% on a running task so a later `tasks/cancel' can stop it.
-spec set_worker(
    binary() | undefined,
    binary(),
    #{
        worker := pid(),
        request_id => integer() | binary()
    }
) ->
    ok | {error, not_found}.
set_worker(SessionId, TaskId, Info) ->
    gen_server:call(?MODULE, {set_worker, SessionId, TaskId, Info}).

%% @doc Record success: store the result and emit notifications/tasks/status.
-spec finish(binary() | undefined, binary(), term()) -> ok | {error, not_found}.
finish(SessionId, TaskId, Result) ->
    gen_server:call(?MODULE, {finish, SessionId, TaskId, Result}).

%% @doc Record failure: store the error and emit notification.
-spec fail(binary() | undefined, binary(), term()) -> ok | {error, not_found}.
fail(SessionId, TaskId, Reason) ->
    gen_server:call(?MODULE, {fail, SessionId, TaskId, Reason}).

%%====================================================================
%% gen_server
%%====================================================================

init([]) ->
    _ = ensure_table(),
    erlang:send_after(?SWEEP_INTERVAL, self(), sweep),
    {ok, #{}}.

handle_call({create, SessionId, Method}, _From, State) ->
    Now = erlang:system_time(millisecond),
    TaskId = generate_id(),
    Task = #task{
        id = TaskId,
        session_id = SessionId,
        method = Method,
        status = working,
        created_at = Now,
        updated_at = Now
    },
    true = ets:insert(?TABLE, {{SessionId, TaskId}, Task}),
    notify_changed(SessionId, Task),
    {reply, {ok, TaskId}, State};
handle_call({cancel, SessionId, TaskId}, _From, State) ->
    %% Best-effort: send the worker a cooperative cancel signal so
    %% arity-2 tool handlers can short-circuit. Then transition
    %% the stored status. Arity-1 handlers run to completion;
    %% their result is dropped because the task is already in a
    %% terminal state.
    _ =
        case ets:lookup(?TABLE, {SessionId, TaskId}) of
            [{_, #task{worker_pid = Pid, request_id = ReqId}}] when
                is_pid(Pid)
            ->
                try
                    Pid ! {cancel, ReqId}
                catch
                    _:_ -> ok
                end;
            _ ->
                ok
        end,
    Reply = transition(SessionId, TaskId, cancelled, undefined, undefined),
    {reply, Reply, State};
handle_call({set_worker, SessionId, TaskId, Info}, _From, State) ->
    Reply =
        case ets:lookup(?TABLE, {SessionId, TaskId}) of
            [{_, #task{} = Task}] ->
                Updated = Task#task{
                    worker_pid = maps:get(worker, Info),
                    request_id = maps:get(request_id, Info, undefined)
                },
                true = ets:insert(?TABLE, {{SessionId, TaskId}, Updated}),
                ok;
            [] ->
                {error, not_found}
        end,
    {reply, Reply, State};
handle_call({finish, SessionId, TaskId, Result}, _From, State) ->
    Reply = transition(SessionId, TaskId, completed, Result, undefined),
    {reply, Reply, State};
handle_call({fail, SessionId, TaskId, Reason}, _From, State) ->
    Reply = transition(SessionId, TaskId, failed, undefined, Reason),
    {reply, Reply, State};
handle_call(_, _, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(sweep, State) ->
    Now = erlang:system_time(millisecond),
    Cutoff = Now - ?TASK_TTL,
    Drop = ets:foldl(
        fun
            ({_, #task{status = working}}, Acc) -> Acc;
            ({Key, #task{updated_at = U}}, Acc) when U < Cutoff -> [Key | Acc];
            (_, Acc) -> Acc
        end,
        [],
        ?TABLE
    ),
    lists:foreach(fun(K) -> ets:delete(?TABLE, K) end, Drop),
    erlang:send_after(?SWEEP_INTERVAL, self(), sweep),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.

%%====================================================================
%% Internal
%%====================================================================

ensure_table() ->
    case ets:whereis(?TABLE) of
        undefined ->
            ets:new(?TABLE, [
                named_table,
                protected,
                set,
                {read_concurrency, true}
            ]);
        _ ->
            ok
    end.

generate_id() ->
    Rand = crypto:strong_rand_bytes(16),
    Hex = binary:encode_hex(Rand, lowercase),
    <<"task_", Hex/binary>>.

transition(SessionId, TaskId, Status, Result, Reason) ->
    case ets:lookup(?TABLE, {SessionId, TaskId}) of
        [{_, #task{status = working} = Task}] ->
            Now = erlang:system_time(millisecond),
            Updated = Task#task{
                status = Status,
                result = Result,
                error = Reason,
                updated_at = Now
            },
            true = ets:insert(?TABLE, {{SessionId, TaskId}, Updated}),
            notify_changed(SessionId, Updated),
            ok;
        [{_, _}] ->
            %% Already terminal — idempotent.
            ok;
        [] ->
            {error, not_found}
    end.

notify_changed(undefined, _) ->
    ok;
notify_changed(SessionId, #task{} = Task) ->
    case barrel_mcp_session:get_sse_pid(SessionId) of
        {ok, Pid} when is_pid(Pid) ->
            Pid !
                {sse_send_message, #{
                    <<"jsonrpc">> => <<"2.0">>,
                    <<"method">> => <<"notifications/tasks/status">>,
                    <<"params">> => task_to_map(Task)
                }},
            ok;
        _ ->
            ok
    end.

task_to_map(#task{
    id = Id,
    session_id = Sid,
    method = M,
    status = St,
    result = R,
    error = E,
    created_at = C,
    updated_at = U
}) ->
    Base = #{
        <<"taskId">> => Id,
        <<"method">> => M,
        <<"status">> => atom_to_binary(St, utf8),
        <<"createdAt">> => to_rfc3339(C),
        <<"lastUpdatedAt">> => to_rfc3339(U),
        %% `ttl' is the requested retention duration in milliseconds,
        %% null when unlimited. We don't yet honour a client-supplied
        %% TTL, so we report null.
        <<"ttl">> => null
    },
    Base1 =
        case Sid of
            undefined -> Base;
            _ -> Base#{<<"sessionId">> => Sid}
        end,
    Base2 =
        case St =:= completed of
            true when R =/= undefined -> Base1#{<<"result">> => R};
            _ -> Base1
        end,
    case St =:= failed of
        true when E =/= undefined ->
            Base2#{<<"error">> => format_error(E)};
        _ ->
            Base2
    end.

to_rfc3339(Ms) when is_integer(Ms) ->
    iolist_to_binary(
        calendar:system_time_to_rfc3339(
            Ms,
            [
                {unit, millisecond},
                {offset, "Z"}
            ]
        )
    ).

format_error(B) when is_binary(B) -> B;
format_error(T) -> iolist_to_binary(io_lib:format("~p", [T])).
