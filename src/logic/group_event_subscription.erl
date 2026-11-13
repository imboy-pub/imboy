-module(group_event_subscription).
-behaviour(gen_server).

-export([start_link/0, ready/0, attach/0]).
-export([supervise_handler/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-spec start_link() -> gen_server:start_ret().
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec ready() -> boolean().
ready() ->
    try
        gen_server:call(?MODULE, ready, 100)
    catch
        exit:_ -> false
    end.

-spec attach() -> ok | {error, not_ready}.
attach() ->
    case ready() of
        true -> ok;
        false -> {error, not_ready}
    end.

init([]) ->
    process_flag(trap_exit, true),
    {ok,
        install(#{
            installer => undefined,
            monitor => undefined,
            deadline => undefined,
            retry => undefined,
            ready => false
        })}.

handle_call(ready, _From, State) -> {reply, maps:get(ready, State), State}.

handle_cast(_, State) -> {noreply, State}.

handle_info({installed, Pid}, #{installer := Pid} = State) ->
    cancel(maps:get(deadline, State)),
    {noreply, State#{deadline := undefined, ready := true}};
handle_info({timeout, Timer, install}, #{deadline := Timer, installer := Pid} = State) ->
    %% Public gen_event installation waits indefinitely; isolate and cancel its caller.
    exit(Pid, kill),
    {noreply, State#{deadline := undefined, ready := false}};
handle_info({'DOWN', Ref, process, Pid, _}, #{monitor := Ref, installer := Pid} = State) ->
    cancel(maps:get(deadline, State)),
    {noreply,
        retry(State#{
            installer := undefined,
            monitor := undefined,
            deadline := undefined,
            ready := false
        })};
handle_info({timeout, Timer, retry}, #{retry := Timer} = State) ->
    {noreply, install(State#{retry := undefined})};
handle_info(_, State) ->
    {noreply, State}.

install(State) ->
    case whereis(imboy_domain_event) of
        undefined ->
            retry(State);
        Bus ->
            %% MFA spawn: the installer is intentionally resident (it only
            %% exits on handler removal / bus shutdown); a fun form would
            %% trip "created fun has no local return" on its none() body.
            {Pid, Ref} = spawn_opt(
                ?MODULE, supervise_handler, [self(), Bus], [link, monitor]
            ),
            Timer = erlang:start_timer(1000, self(), install),
            State#{installer := Pid, monitor := Ref, deadline := Timer}
    end.

%% Resident installer process: gen_event removes supervised handlers when
%% their installing process exits, so this loop never returns normally —
%% it exits handler_removed/manager_stopped instead.
-spec supervise_handler(pid(), pid()) -> no_return().
supervise_handler(Owner, Bus) ->
    Ref = monitor(process, Bus),
    ok = gen_event:swap_sup_handler(
        Bus,
        {group_event_handler, reinstall},
        {group_event_handler, []}
    ),
    Owner ! {installed, self()},
    %% Stay alive: gen_event removes supervised handlers when their installer exits.
    receive
        {gen_event_EXIT, group_event_handler, _} -> exit(handler_removed);
        {'DOWN', Ref, process, Bus, _} -> exit(manager_stopped)
    end.

retry(#{retry := undefined} = State) ->
    State#{retry := erlang:start_timer(100, self(), retry)};
retry(State) ->
    State.

cancel(undefined) ->
    ok;
cancel(Timer) ->
    _ = erlang:cancel_timer(Timer),
    ok.

terminate(_, State) ->
    cancel(maps:get(deadline, State)),
    cancel(maps:get(retry, State)),
    case maps:get(installer, State) of
        undefined ->
            ok;
        Pid ->
            exit(Pid, kill),
            Ref = maps:get(monitor, State),
            receive
                {'DOWN', Ref, process, Pid, _} -> ok
            end
    end.
