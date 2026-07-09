%%%-------------------------------------------------------------------
%%% @author Benoit Chesneau
%%% @copyright 2024-2026 Benoit Chesneau
%%% @doc MCP Session Management.
%%%
%%% Provides ETS-based session management for MCP Streamable HTTP transport.
%%% Sessions track client connections, protocol versions, and activity.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(barrel_mcp_session).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    create/1,
    get/1,
    update_activity/1,
    delete/1,
    generate_id/0,
    list/0,
    cleanup_expired/1,
    %% Capability tracking (set during MCP `initialize').
    set_client_capabilities/2,
    has_sampling/1,
    list_sampling_capable/0,
    has_elicitation/1,
    list_elicitation_capable/0,
    has_roots/1,
    list_roots_capable/0,
    %% Per-session log level (`logging/setLevel').
    set_log_level/2,
    get_log_level/1,
    log_level_priority/1,
    %% Negotiated protocol version (after `initialize').
    set_protocol_version/2,
    get_protocol_version/1,
    %% sse_pid management.
    set_sse_pid/2,
    get_sse_pid/1,
    %% Resource subscription tracking (server-side, used to emit
    %% notifications/resources/updated when an exposed resource changes).
    subscribe_resource/2,
    unsubscribe_resource/2,
    subscribers_for/1,
    %% Server -> client request via the session's SSE channel.
    sampling_create_message/3,
    elicit_create/3,
    roots_list/2,
    deliver_response/2,
    %% Server -> client notifications.
    broadcast_list_changed/1,
    notify_progress/4,
    %% In-flight tool tracking (used by `notifications/cancelled').
    record_in_flight/4,
    cancel_in_flight/2,
    clear_in_flight/2,
    %% SSE replay (Last-Event-ID).
    record_sse_event/3,
    events_since/2,
    set_sse_buffer_max/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include("barrel_mcp.hrl").

-define(SESSION_TABLE, barrel_mcp_sessions).
-define(SUBSCRIPTIONS_TABLE, barrel_mcp_resource_subs).
-define(PENDING_TABLE, barrel_mcp_pending_requests).
%% In-flight tool calls per session: {{SessionId, RequestId} => #in_flight{}}
-define(INFLIGHT_TABLE, barrel_mcp_inflight).
%% 1 minute
-define(CLEANUP_INTERVAL, 60000).
-define(DEFAULT_SAMPLING_TIMEOUT, 30000).

-record(mcp_session, {
    id :: binary(),
    created_at :: integer(),
    last_activity :: integer(),
    client_info :: map(),
    client_capabilities :: map(),
    protocol_version :: binary(),
    %% Process handling SSE stream
    sse_pid :: pid() | undefined,
    %% Recent SSE events (newest first) for `Last-Event-ID' replay.
    sse_buffer = [] :: [{binary(), map()}],
    sse_buffer_max = 256 :: pos_integer(),
    %% Per-session log level set by `logging/setLevel'. Default
    %% `info' per the MCP spec. Filters `notifications/message' on
    %% emit.
    log_level = info :: log_level()
}).

-type log_level() ::
    debug
    | info
    | notice
    | warning
    | error
    | critical
    | alert
    | emergency.

%% Async tool call in-flight tracking.
-record(in_flight, {
    session_id :: binary(),
    request_id :: integer() | binary(),
    worker_pid :: pid(),
    waiter_pid :: pid()
}).

-record(pending, {
    id :: binary(),
    session_id :: binary(),
    caller :: pid(),
    caller_ref :: reference(),
    expires_at :: integer(),
    tag = sampling_response :: atom()
}).

%%====================================================================
%% API
%%====================================================================

%% @doc Start the session manager.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Create a new session.
-spec create(Opts) -> {ok, binary()} when
    Opts :: #{
        client_info => map(),
        protocol_version => binary()
    }.
create(Opts) ->
    gen_server:call(?MODULE, {create, Opts}).

%% @doc Get a session by ID.
-spec get(binary()) -> {ok, map()} | {error, not_found}.
get(SessionId) ->
    case ets:lookup(?SESSION_TABLE, SessionId) of
        [{_, Session}] ->
            {ok, session_to_map(Session)};
        [] ->
            {error, not_found}
    end.

%% @doc Update last activity timestamp.
-spec update_activity(binary()) -> ok | {error, not_found}.
update_activity(SessionId) ->
    gen_server:call(?MODULE, {update_activity, SessionId}).

%% @doc Delete a session.
-spec delete(binary()) -> ok.
delete(SessionId) ->
    gen_server:call(?MODULE, {delete, SessionId}).

%% @doc Generate a unique session ID.
-spec generate_id() -> binary().
generate_id() ->
    Rand = crypto:strong_rand_bytes(16),
    Hex = binary:encode_hex(Rand, lowercase),
    <<"mcp_", Hex/binary>>.

%% @doc List all sessions.
-spec list() -> [map()].
list() ->
    ets:foldl(
        fun({_, Session}, Acc) ->
            [session_to_map(Session) | Acc]
        end,
        [],
        ?SESSION_TABLE
    ).

%% @doc Set the client_capabilities map for a session. Called from the
%% protocol handler after parsing the `initialize' request.
-spec set_client_capabilities(binary(), map()) -> ok | {error, not_found}.
set_client_capabilities(SessionId, Capabilities) when is_map(Capabilities) ->
    gen_server:call(?MODULE, {set_client_capabilities, SessionId, Capabilities}).

%% @doc Record the negotiated protocol version on a session. Called
%% by the HTTP transport after a successful `initialize' so later
%% requests on the same session can fall back to it when the client
%% omits the `MCP-Protocol-Version' header.
-spec set_protocol_version(binary(), binary()) -> ok | {error, not_found}.
set_protocol_version(SessionId, Version) when is_binary(Version) ->
    gen_server:call(?MODULE, {set_protocol_version, SessionId, Version}).

%% @doc Look up the negotiated protocol version for a session.
-spec get_protocol_version(binary()) -> {ok, binary()} | {error, not_found}.
get_protocol_version(SessionId) ->
    case ets:lookup(?SESSION_TABLE, SessionId) of
        [{_, #mcp_session{protocol_version = V}}] when is_binary(V) ->
            {ok, V};
        [{_, _}] ->
            {ok, ?MCP_PROTOCOL_VERSION};
        [] ->
            {error, not_found}
    end.

%% @doc Whether a session declared sampling capability in its initialize
%% request.
-spec has_sampling(binary()) -> boolean().
has_sampling(SessionId) ->
    case ets:lookup(?SESSION_TABLE, SessionId) of
        [{_, #mcp_session{client_capabilities = Caps}}] ->
            maps:is_key(<<"sampling">>, Caps);
        [] ->
            false
    end.

%% @doc List session ids whose client declared sampling capability.
-spec list_sampling_capable() -> [binary()].
list_sampling_capable() ->
    ets:foldl(
        fun({Id, #mcp_session{client_capabilities = Caps}}, Acc) ->
            case maps:is_key(<<"sampling">>, Caps) of
                true -> [Id | Acc];
                false -> Acc
            end
        end,
        [],
        ?SESSION_TABLE
    ).

%% @doc Whether a session declared elicitation capability in its
%% initialize request.
-spec has_elicitation(binary()) -> boolean().
has_elicitation(SessionId) ->
    case ets:lookup(?SESSION_TABLE, SessionId) of
        [{_, #mcp_session{client_capabilities = Caps}}] ->
            maps:is_key(<<"elicitation">>, Caps);
        [] ->
            false
    end.

%% @doc List session ids whose client declared elicitation capability.
-spec list_elicitation_capable() -> [binary()].
list_elicitation_capable() ->
    ets:foldl(
        fun({Id, #mcp_session{client_capabilities = Caps}}, Acc) ->
            case maps:is_key(<<"elicitation">>, Caps) of
                true -> [Id | Acc];
                false -> Acc
            end
        end,
        [],
        ?SESSION_TABLE
    ).

%% @doc Whether a session declared roots capability in its initialize
%% request.
-spec has_roots(binary()) -> boolean().
has_roots(SessionId) ->
    case ets:lookup(?SESSION_TABLE, SessionId) of
        [{_, #mcp_session{client_capabilities = Caps}}] ->
            maps:is_key(<<"roots">>, Caps);
        [] ->
            false
    end.

%% @doc List session ids whose client declared roots capability.
-spec list_roots_capable() -> [binary()].
list_roots_capable() ->
    ets:foldl(
        fun({Id, #mcp_session{client_capabilities = Caps}}, Acc) ->
            case maps:is_key(<<"roots">>, Caps) of
                true -> [Id | Acc];
                false -> Acc
            end
        end,
        [],
        ?SESSION_TABLE
    ).

%% @doc Set the per-session log level (driven by `logging/setLevel').
%% `Level' is one of the eight RFC 5424 levels accepted by the MCP
%% spec; rejects anything else with `{error, invalid_level}'.
-spec set_log_level(binary(), log_level() | binary()) ->
    ok | {error, not_found | invalid_level}.
set_log_level(SessionId, Level) ->
    case parse_level(Level) of
        {ok, L} ->
            gen_server:call(?MODULE, {set_log_level, SessionId, L});
        error ->
            {error, invalid_level}
    end.

%% @doc Read the current log level for a session. Defaults to `info'
%% before any `logging/setLevel' is received.
-spec get_log_level(binary()) -> {ok, log_level()} | {error, not_found}.
get_log_level(SessionId) ->
    case ets:lookup(?SESSION_TABLE, SessionId) of
        [{_, #mcp_session{log_level = L}}] -> {ok, L};
        [] -> {error, not_found}
    end.

%% @doc Numeric priority for the eight RFC 5424 levels (debug=0,
%% emergency=7). Higher = more severe. Used for filtering: a
%% notification at priority N is delivered iff N >= configured level.
-spec log_level_priority(log_level() | binary()) -> 0..7 | error.
log_level_priority(Level) ->
    case parse_level(Level) of
        {ok, L} -> level_priority(L);
        error -> error
    end.

level_priority(debug) -> 0;
level_priority(info) -> 1;
level_priority(notice) -> 2;
level_priority(warning) -> 3;
level_priority(error) -> 4;
level_priority(critical) -> 5;
level_priority(alert) -> 6;
level_priority(emergency) -> 7.

parse_level(L) when is_atom(L) ->
    case
        lists:member(L, [
            debug,
            info,
            notice,
            warning,
            error,
            critical,
            alert,
            emergency
        ])
    of
        true -> {ok, L};
        false -> error
    end;
parse_level(<<"debug">>) ->
    {ok, debug};
parse_level(<<"info">>) ->
    {ok, info};
parse_level(<<"notice">>) ->
    {ok, notice};
parse_level(<<"warning">>) ->
    {ok, warning};
parse_level(<<"error">>) ->
    {ok, error};
parse_level(<<"critical">>) ->
    {ok, critical};
parse_level(<<"alert">>) ->
    {ok, alert};
parse_level(<<"emergency">>) ->
    {ok, emergency};
parse_level(_) ->
    error.

%% @doc Set the SSE process pid for a session.
-spec set_sse_pid(binary(), pid() | undefined) -> ok | {error, not_found}.
set_sse_pid(SessionId, Pid) ->
    gen_server:call(?MODULE, {set_sse_pid, SessionId, Pid}).

-spec get_sse_pid(binary()) -> {ok, pid()} | {error, not_found | no_sse}.
get_sse_pid(SessionId) ->
    case ets:lookup(?SESSION_TABLE, SessionId) of
        [{_, #mcp_session{sse_pid = undefined}}] -> {error, no_sse};
        [{_, #mcp_session{sse_pid = Pid}}] when is_pid(Pid) -> {ok, Pid};
        [] -> {error, not_found}
    end.

%% @doc Subscribe a session to resource updates for a given URI.
-spec subscribe_resource(binary(), binary()) -> ok.
subscribe_resource(SessionId, Uri) when
    is_binary(SessionId), is_binary(Uri)
->
    gen_server:call(?MODULE, {subscribe_resource, SessionId, Uri}).

-spec unsubscribe_resource(binary(), binary()) -> ok.
unsubscribe_resource(SessionId, Uri) ->
    gen_server:call(?MODULE, {unsubscribe_resource, SessionId, Uri}).

%% @doc Return all session ids that subscribed to a URI.
-spec subscribers_for(binary()) -> [binary()].
subscribers_for(Uri) when is_binary(Uri) ->
    _ = ensure_subs_table(),
    %% match-spec to find all {SessionId, Uri} for the given Uri
    Pattern = {{'$1', Uri}},
    Match = [{Pattern, [], ['$1']}],
    ets:select(?SUBSCRIPTIONS_TABLE, Match).

%% @doc Send `sampling/createMessage' to the client behind a session and
%% wait for the response. The session must (a) exist, (b) have an active
%% sse_pid, and (c) have declared sampling capability in initialize.
-spec sampling_create_message(binary(), map(), map()) ->
    {ok, map(), map()}
    | {error, timeout | not_supported | no_sse | not_found | term()}.
sampling_create_message(SessionId, Params, Opts) ->
    case has_sampling(SessionId) of
        false ->
            {error, not_supported};
        true ->
            case get_sse_pid(SessionId) of
                {error, _} = E -> E;
                {ok, Pid} -> do_sampling(SessionId, Pid, Params, Opts)
            end
    end.

%% @doc Send `elicitation/create' to the client behind a session and wait
%% for the response. The session must (a) exist, (b) have an active
%% sse_pid, and (c) have declared elicitation capability in initialize.
-spec elicit_create(binary(), map(), map()) ->
    {ok, map()}
    | {error, timeout | not_supported | no_sse | not_found | term()}.
elicit_create(SessionId, Params, Opts) ->
    case has_elicitation(SessionId) of
        false ->
            {error, not_supported};
        true ->
            case get_sse_pid(SessionId) of
                {error, _} = E -> E;
                {ok, Pid} -> do_elicit(SessionId, Pid, Params, Opts)
            end
    end.

%% @doc Send `roots/list' to the client behind a session and wait for
%% the response. The session must (a) exist, (b) have an active sse_pid,
%% and (c) have declared roots capability in initialize.
-spec roots_list(binary(), map()) ->
    {ok, [map()]}
    | {error, timeout | not_supported | no_sse | not_found | term()}.
roots_list(SessionId, Opts) ->
    case has_roots(SessionId) of
        false ->
            {error, not_supported};
        true ->
            case get_sse_pid(SessionId) of
                {error, _} = E -> E;
                {ok, Pid} -> do_roots_list(SessionId, Pid, Opts)
            end
    end.

%% @doc Deliver a JSON-RPC response from the client back to the waiting
%% caller. Called by the HTTP handler when an inbound POST contains a
%% `result' or `error' for a server-initiated id.
-spec deliver_response(binary() | integer(), map()) -> ok | {error, unknown_id}.
deliver_response(Id, Response) ->
    gen_server:call(?MODULE, {deliver_response, id_to_binary(Id), Response}).

%% @doc Push a `notifications/<kind>/list_changed' envelope to every
%% session that has an active SSE channel. Tolerates a missing
%% session manager (e.g. during stdio-only operation).
-spec broadcast_list_changed(handler_type()) -> ok.
broadcast_list_changed(Kind) ->
    case {whereis(?MODULE), list_changed_method(Kind)} of
        {undefined, _} ->
            ok;
        %% kind has no list_changed notification
        {_, undefined} ->
            ok;
        {_, Method} ->
            Notif = #{
                <<"jsonrpc">> => <<"2.0">>,
                <<"method">> => Method,
                <<"params">> => #{}
            },
            broadcast_to_sse_sessions(Notif)
    end.

list_changed_method(tool) -> <<"notifications/tools/list_changed">>;
list_changed_method(resource) -> <<"notifications/resources/list_changed">>;
list_changed_method(resource_template) -> <<"notifications/resources/list_changed">>;
list_changed_method(prompt) -> <<"notifications/prompts/list_changed">>;
list_changed_method(completion) -> undefined.

broadcast_to_sse_sessions(Notification) ->
    %% Reads from a `protected' ETS via direct ets:foldl/3 work fine
    %% from any process. We only need the gen_server when we mutate
    %% the table.
    case ets:whereis(?SESSION_TABLE) of
        undefined ->
            ok;
        _ ->
            ets:foldl(
                fun
                    ({_Id, #mcp_session{sse_pid = Pid}}, Acc) when is_pid(Pid) ->
                        Pid ! {sse_send_message, Notification},
                        Acc;
                    (_, Acc) ->
                        Acc
                end,
                ok,
                ?SESSION_TABLE
            )
    end.

%% @doc Record an in-flight tool call so a later
%% `notifications/cancelled' can find the worker and waiter.
-spec record_in_flight(binary(), integer() | binary(), pid(), pid()) -> ok.
record_in_flight(SessionId, RequestId, WorkerPid, WaiterPid) ->
    gen_server:call(
        ?MODULE,
        {record_in_flight, SessionId, RequestId, WorkerPid, WaiterPid}
    ).

%% @doc Cancel an in-flight tool call. Sends `{cancel, RequestId}'
%% to the worker and `{cancelled, RequestId}' to the waiter, then
%% drops the entry. Idempotent: a missing entry returns `ok'.
-spec cancel_in_flight(binary(), integer() | binary()) -> ok.
cancel_in_flight(SessionId, RequestId) ->
    gen_server:call(?MODULE, {cancel_in_flight, SessionId, RequestId}).

%% @doc Drop an in-flight entry (called by the waiter after a normal
%% completion).
-spec clear_in_flight(binary(), integer() | binary()) -> ok.
clear_in_flight(SessionId, RequestId) ->
    gen_server:call(?MODULE, {clear_in_flight, SessionId, RequestId}).

%% @doc Append an SSE event to the session's ring buffer for later
%% replay via `Last-Event-ID'.
-spec record_sse_event(binary(), binary(), map()) -> ok.
record_sse_event(SessionId, EventId, Payload) ->
    gen_server:call(
        ?MODULE,
        {record_sse_event, SessionId, EventId, Payload}
    ).

%% @doc Return SSE events newer than `LastId' (oldest first), or
%% `truncated' when `LastId' is older than the oldest buffered event.
-spec events_since(binary(), binary()) ->
    {ok, [{binary(), map()}]} | truncated | {error, not_found}.
events_since(SessionId, LastId) ->
    case ets:lookup(?SESSION_TABLE, SessionId) of
        [{_, #mcp_session{sse_buffer = Buf}}] ->
            collect_after(Buf, LastId);
        [] ->
            {error, not_found}
    end.

%% Buffer is newest-first. Return events after `LastId' in
%% chronological order (oldest first), or `truncated' if LastId
%% isn't in the window.
collect_after(Buf, LastId) ->
    case lists:splitwith(fun({Id, _}) -> Id =/= LastId end, Buf) of
        {_, []} ->
            %% LastId not found — buffer rolled over.
            truncated;
        {Newer, [_ | _]} ->
            {ok, lists:reverse(Newer)}
    end.

%% @doc Configure the maximum number of SSE events buffered per
%% session for replay.
-spec set_sse_buffer_max(binary(), pos_integer()) -> ok | {error, not_found}.
set_sse_buffer_max(SessionId, Max) when is_integer(Max), Max > 0 ->
    gen_server:call(?MODULE, {set_sse_buffer_max, SessionId, Max}).

%% @doc Push a `notifications/progress' envelope to a specific
%% session over its SSE channel. `Token' is the progressToken the
%% client supplied on the originating request.
-spec notify_progress(binary(), term(), number(), number() | undefined) -> ok.
notify_progress(SessionId, Token, Progress, Total) ->
    case get_sse_pid(SessionId) of
        {ok, Pid} ->
            Params0 = #{
                <<"progressToken">> => Token,
                <<"progress">> => Progress
            },
            Params =
                case Total of
                    undefined -> Params0;
                    _ -> Params0#{<<"total">> => Total}
                end,
            Pid !
                {sse_send_message, #{
                    <<"jsonrpc">> => <<"2.0">>,
                    <<"method">> => <<"notifications/progress">>,
                    <<"params">> => Params
                }},
            ok;
        _ ->
            ok
    end.

%% @doc Cleanup sessions older than TTL milliseconds. Routes through
%% the gen_server (the table owner under the new `protected'
%% visibility); the handler deletes expired entries inline.
-spec cleanup_expired(pos_integer()) -> non_neg_integer().
cleanup_expired(TTL) ->
    gen_server:call(?MODULE, {cleanup_expired, TTL}).

%% Trim a newest-first list to at most `Max' entries.
trim(List, Max) when length(List) =< Max -> List;
trim(List, Max) -> lists:sublist(List, Max).

%% Inline session delete, only called from inside the gen_server.
delete_inline(SessionId) ->
    case ets:lookup(?SESSION_TABLE, SessionId) of
        [{_, #mcp_session{sse_pid = Pid}}] when is_pid(Pid) ->
            Pid ! session_terminated;
        _ ->
            ok
    end,
    true = ets:delete(?SESSION_TABLE, SessionId),
    ok.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Create ETS tables if they don't exist
    _ = ensure_session_table(),
    _ = ensure_subs_table(),
    _ = ensure_pending_table(),
    _ = ensure_inflight_table(),
    %% Schedule periodic cleanup
    _ = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup),
    {ok, #{}}.

handle_call({create, Opts}, _From, State) ->
    SessionId = generate_id(),
    Now = erlang:system_time(millisecond),
    Session = #mcp_session{
        id = SessionId,
        created_at = Now,
        last_activity = Now,
        client_info = maps:get(client_info, Opts, #{}),
        client_capabilities = maps:get(client_capabilities, Opts, #{}),
        protocol_version = maps:get(protocol_version, Opts, <<"2025-03-26">>),
        sse_pid = undefined
    },
    true = ets:insert(?SESSION_TABLE, {SessionId, Session}),
    {reply, {ok, SessionId}, State};
handle_call({update_activity, SessionId}, _From, State) ->
    Reply =
        case ets:lookup(?SESSION_TABLE, SessionId) of
            [{_, Session}] ->
                Now = erlang:system_time(millisecond),
                Updated = Session#mcp_session{last_activity = Now},
                true = ets:insert(?SESSION_TABLE, {SessionId, Updated}),
                ok;
            [] ->
                {error, not_found}
        end,
    {reply, Reply, State};
handle_call({delete, SessionId}, _From, State) ->
    case ets:lookup(?SESSION_TABLE, SessionId) of
        [{_, #mcp_session{sse_pid = Pid}}] when is_pid(Pid) ->
            Pid ! session_terminated;
        _ ->
            ok
    end,
    true = ets:delete(?SESSION_TABLE, SessionId),
    {reply, ok, State};
handle_call({set_client_capabilities, SessionId, Caps}, _From, State) ->
    Reply =
        case ets:lookup(?SESSION_TABLE, SessionId) of
            [{_, Session}] ->
                Updated = Session#mcp_session{client_capabilities = Caps},
                true = ets:insert(?SESSION_TABLE, {SessionId, Updated}),
                ok;
            [] ->
                {error, not_found}
        end,
    {reply, Reply, State};
handle_call({set_log_level, SessionId, Level}, _From, State) ->
    Reply =
        case ets:lookup(?SESSION_TABLE, SessionId) of
            [{_, Session}] ->
                Updated = Session#mcp_session{log_level = Level},
                true = ets:insert(?SESSION_TABLE, {SessionId, Updated}),
                ok;
            [] ->
                {error, not_found}
        end,
    {reply, Reply, State};
handle_call({set_protocol_version, SessionId, Version}, _From, State) ->
    Reply =
        case ets:lookup(?SESSION_TABLE, SessionId) of
            [{_, Session}] ->
                Updated = Session#mcp_session{protocol_version = Version},
                true = ets:insert(?SESSION_TABLE, {SessionId, Updated}),
                ok;
            [] ->
                {error, not_found}
        end,
    {reply, Reply, State};
handle_call({set_sse_pid, SessionId, Pid}, _From, State) ->
    Reply =
        case ets:lookup(?SESSION_TABLE, SessionId) of
            [{_, Session}] ->
                Updated = Session#mcp_session{sse_pid = Pid},
                true = ets:insert(?SESSION_TABLE, {SessionId, Updated}),
                ok;
            [] ->
                {error, not_found}
        end,
    {reply, Reply, State};
handle_call({subscribe_resource, SessionId, Uri}, _From, State) ->
    true = ets:insert(?SUBSCRIPTIONS_TABLE, {{SessionId, Uri}}),
    {reply, ok, State};
handle_call({unsubscribe_resource, SessionId, Uri}, _From, State) ->
    true = ets:delete(?SUBSCRIPTIONS_TABLE, {SessionId, Uri}),
    {reply, ok, State};
handle_call({register_pending, RequestId, Pending}, _From, State) ->
    true = ets:insert(?PENDING_TABLE, {RequestId, Pending}),
    {reply, ok, State};
handle_call({discard_pending, RequestId}, _From, State) ->
    true = ets:delete(?PENDING_TABLE, RequestId),
    {reply, ok, State};
handle_call({deliver_response, Key, Response}, _From, State) ->
    Reply =
        case ets:lookup(?PENDING_TABLE, Key) of
            [{_, #pending{caller = Caller, caller_ref = Ref, tag = Tag}}] ->
                true = ets:delete(?PENDING_TABLE, Key),
                Caller ! {Tag, Ref, Response},
                ok;
            [] ->
                {error, unknown_id}
        end,
    {reply, Reply, State};
handle_call(
    {record_in_flight, SessionId, RequestId, Worker, Waiter},
    _From,
    State
) ->
    InFlight = #in_flight{
        session_id = SessionId,
        request_id = RequestId,
        worker_pid = Worker,
        waiter_pid = Waiter
    },
    true = ets:insert(?INFLIGHT_TABLE, {{SessionId, RequestId}, InFlight}),
    {reply, ok, State};
handle_call({cancel_in_flight, SessionId, RequestId}, _From, State) ->
    case ets:lookup(?INFLIGHT_TABLE, {SessionId, RequestId}) of
        [{_, #in_flight{worker_pid = W, waiter_pid = Wt}}] ->
            _ =
                (try
                    W ! {cancel, RequestId}
                catch
                    _:_ -> ok
                end),
            _ =
                (try
                    Wt ! {cancelled, RequestId}
                catch
                    _:_ -> ok
                end),
            true = ets:delete(?INFLIGHT_TABLE, {SessionId, RequestId});
        [] ->
            ok
    end,
    {reply, ok, State};
handle_call({clear_in_flight, SessionId, RequestId}, _From, State) ->
    true = ets:delete(?INFLIGHT_TABLE, {SessionId, RequestId}),
    {reply, ok, State};
handle_call({record_sse_event, SessionId, EventId, Payload}, _From, State) ->
    case ets:lookup(?SESSION_TABLE, SessionId) of
        [{_, #mcp_session{sse_buffer = Buf, sse_buffer_max = Max} = S}] ->
            NewBuf = trim([{EventId, Payload} | Buf], Max),
            true = ets:insert(
                ?SESSION_TABLE,
                {SessionId, S#mcp_session{sse_buffer = NewBuf}}
            ),
            ok;
        [] ->
            ok
    end,
    {reply, ok, State};
handle_call({set_sse_buffer_max, SessionId, Max}, _From, State) ->
    Reply =
        case ets:lookup(?SESSION_TABLE, SessionId) of
            [{_, S}] ->
                true = ets:insert(
                    ?SESSION_TABLE,
                    {SessionId, S#mcp_session{sse_buffer_max = Max}}
                ),
                ok;
            [] ->
                {error, not_found}
        end,
    {reply, Reply, State};
handle_call({cleanup_expired, TTL}, _From, State) ->
    Now = erlang:system_time(millisecond),
    Cutoff = Now - TTL,
    Expired = ets:foldl(
        fun
            ({Id, #mcp_session{last_activity = LA}}, Acc) when
                LA < Cutoff
            ->
                [Id | Acc];
            (_, Acc) ->
                Acc
        end,
        [],
        ?SESSION_TABLE
    ),
    lists:foreach(fun delete_inline/1, Expired),
    {reply, length(Expired), State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup, State) ->
    %% Inline the cleanup. We can't call the public `cleanup_expired/1'
    %% because it goes through `gen_server:call(?MODULE, …)' — a
    %% self-call that would deadlock.
    TTL = application:get_env(barrel_mcp, session_ttl, 1800000),
    Now = erlang:system_time(millisecond),
    Cutoff = Now - TTL,
    Expired = ets:foldl(
        fun
            ({Id, #mcp_session{last_activity = LA}}, Acc) when
                LA < Cutoff
            ->
                [Id | Acc];
            (_, Acc) ->
                Acc
        end,
        [],
        ?SESSION_TABLE
    ),
    lists:foreach(fun delete_inline/1, Expired),
    case Expired of
        [] ->
            ok;
        _ ->
            logger:debug(
                "Cleaned up ~p expired MCP sessions",
                [length(Expired)]
            )
    end,
    erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

session_to_map(#mcp_session{
    id = Id,
    created_at = CreatedAt,
    last_activity = LastActivity,
    client_info = ClientInfo,
    client_capabilities = Caps,
    protocol_version = ProtocolVersion,
    sse_pid = SsePid
}) ->
    #{
        id => Id,
        created_at => CreatedAt,
        last_activity => LastActivity,
        client_info => ClientInfo,
        client_capabilities => Caps,
        protocol_version => ProtocolVersion,
        sse_pid => SsePid
    }.

%% ============================================================================
%% Internal helpers (table init + sampling implementation)
%% ============================================================================

ensure_session_table() ->
    case ets:whereis(?SESSION_TABLE) of
        undefined ->
            ets:new(?SESSION_TABLE, [
                named_table,
                protected,
                set,
                {read_concurrency, true},
                {write_concurrency, true}
            ]);
        _ ->
            ok
    end.

ensure_subs_table() ->
    case ets:whereis(?SUBSCRIPTIONS_TABLE) of
        undefined ->
            ets:new(?SUBSCRIPTIONS_TABLE, [
                named_table,
                protected,
                set,
                {read_concurrency, true}
            ]);
        _ ->
            ok
    end.

ensure_pending_table() ->
    case ets:whereis(?PENDING_TABLE) of
        undefined ->
            ets:new(?PENDING_TABLE, [
                named_table,
                protected,
                set,
                {read_concurrency, true}
            ]);
        _ ->
            ok
    end.

ensure_inflight_table() ->
    case ets:whereis(?INFLIGHT_TABLE) of
        undefined ->
            ets:new(?INFLIGHT_TABLE, [
                named_table,
                protected,
                set,
                {read_concurrency, true}
            ]);
        _ ->
            ok
    end.

do_sampling(SessionId, SsePid, Params, Opts) ->
    Timeout = maps:get(timeout_ms, Opts, ?DEFAULT_SAMPLING_TIMEOUT),
    RequestId = generate_request_id(<<"sampling-">>),
    Ref = make_ref(),
    ok = gen_server:call(
        ?MODULE,
        {register_pending, RequestId, #pending{
            id = RequestId,
            session_id = SessionId,
            caller = self(),
            caller_ref = Ref,
            expires_at = erlang:system_time(millisecond) + Timeout,
            tag = sampling_response
        }}
    ),
    Request = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => RequestId,
        <<"method">> => <<"sampling/createMessage">>,
        <<"params">> => Params
    },
    SsePid ! {sse_send_message, Request},
    receive
        {sampling_response, Ref, #{<<"result">> := Result} = R} ->
            Usage = maps:get(<<"usage">>, Result, maps:get(usage, R, #{})),
            {ok, Result, Usage};
        {sampling_response, Ref, #{<<"error">> := Err}} ->
            {error, {client_error, Err}}
    after Timeout ->
        _ = gen_server:call(?MODULE, {discard_pending, RequestId}),
        {error, timeout}
    end.

do_elicit(SessionId, SsePid, Params, Opts) ->
    Timeout = maps:get(timeout_ms, Opts, ?DEFAULT_SAMPLING_TIMEOUT),
    RequestId = generate_request_id(<<"elicit-">>),
    Ref = make_ref(),
    ok = gen_server:call(
        ?MODULE,
        {register_pending, RequestId, #pending{
            id = RequestId,
            session_id = SessionId,
            caller = self(),
            caller_ref = Ref,
            expires_at = erlang:system_time(millisecond) + Timeout,
            tag = elicitation_response
        }}
    ),
    Request = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => RequestId,
        <<"method">> => <<"elicitation/create">>,
        <<"params">> => Params
    },
    SsePid ! {sse_send_message, Request},
    receive
        {elicitation_response, Ref, #{<<"result">> := Result}} ->
            {ok, Result};
        {elicitation_response, Ref, #{<<"error">> := Err}} ->
            {error, {client_error, Err}}
    after Timeout ->
        _ = gen_server:call(?MODULE, {discard_pending, RequestId}),
        {error, timeout}
    end.

do_roots_list(SessionId, SsePid, Opts) ->
    Timeout = maps:get(timeout_ms, Opts, ?DEFAULT_SAMPLING_TIMEOUT),
    RequestId = generate_request_id(<<"roots-">>),
    Ref = make_ref(),
    ok = gen_server:call(
        ?MODULE,
        {register_pending, RequestId, #pending{
            id = RequestId,
            session_id = SessionId,
            caller = self(),
            caller_ref = Ref,
            expires_at = erlang:system_time(millisecond) + Timeout,
            tag = roots_response
        }}
    ),
    Request = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => RequestId,
        <<"method">> => <<"roots/list">>,
        <<"params">> => #{}
    },
    SsePid ! {sse_send_message, Request},
    receive
        {roots_response, Ref, #{<<"result">> := Result}} ->
            Roots = maps:get(<<"roots">>, Result, []),
            {ok, Roots};
        {roots_response, Ref, #{<<"error">> := Err}} ->
            {error, {client_error, Err}}
    after Timeout ->
        _ = gen_server:call(?MODULE, {discard_pending, RequestId}),
        {error, timeout}
    end.

generate_request_id(Prefix) ->
    <<Prefix/binary, (integer_to_binary(erlang:unique_integer([positive])))/binary>>.

id_to_binary(Id) when is_binary(Id) -> Id;
id_to_binary(Id) when is_integer(Id) -> integer_to_binary(Id);
id_to_binary(Id) -> iolist_to_binary(io_lib:format("~p", [Id])).
