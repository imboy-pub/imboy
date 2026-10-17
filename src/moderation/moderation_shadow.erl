-module(moderation_shadow).
-behaviour(gen_server).
-export([start_link/0, set_mode/1, status/0, submit/3, classify/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(TABLE, moderation_shadow_samples).
-define(QUEUE, 64).
-define(WORKERS, 2).
-define(TTL, 1000).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set_mode(Mode) when Mode =:= legacy; Mode =:= shadow; Mode =:= rust ->
    gen_server:call(?MODULE, {mode, Mode}).

status() -> gen_server:call(?MODULE, status).

submit(Scene, Fields, Snapshot) ->
    try
        case valid_sample(Scene, Fields) of
            false ->
                count(oversize),
                dropped;
            true ->
                admit(Scene, Fields, Snapshot)
        end
    catch
        _:_ ->
            elib_metric:increment(textguard_shadow, 1, #{error_code => worker_unavailable}),
            dropped
    end.

admit(Scene, Fields, Snapshot) ->
    case ets:lookup(?TABLE, mode) of
        [{mode, shadow, Epoch}] ->
            Sample = {Epoch, now_ms() + ?TTL, Scene, Fields, Snapshot},
            insert_sample(Sample, 1);
        _ ->
            count(mode_switch),
            dropped
    end.

insert_sample(_, Slot) when Slot > ?QUEUE ->
    count(queue_full),
    dropped;
insert_sample({Epoch, _, _, _, _} = Sample, Slot) ->
    Object = {{job, Slot}, Sample},
    case ets:insert_new(?TABLE, Object) of
        false ->
            insert_sample(Sample, Slot + 1);
        true ->
            case ets:lookup(?TABLE, mode) of
                [{mode, shadow, Epoch}] ->
                    accepted;
                _ ->
                    ets:delete_object(?TABLE, Object),
                    count(mode_switch),
                    dropped
            end
    end.

valid_sample(Scene, Fields) when is_list(Fields), length(Fields) >= 1, length(Fields) =< 8 ->
    lists:member(Scene, [channel_message, moment_post, profile_field]) andalso
        lists:all(fun valid_field/1, Fields) andalso
        lists:sum([byte_size(Text) || {_, Text} <- Fields]) =< 32768;
valid_sample(_, _) ->
    false.

valid_field({Id, Text}) when is_binary(Id), is_binary(Text) ->
    byte_size(Id) >= 1 andalso byte_size(Id) =< 64 andalso byte_size(Text) =< 16384;
valid_field(_) ->
    false.

classify(unavailable, _) ->
    legacy_unavailable;
classify(_, {error, _}) ->
    facility_failure;
classify(Snapshot, {ok, Response}) ->
    case
        {
            maps:get(base_rules_digest, Snapshot),
            maps:get(base_rules_digest, Response),
            maps:get(normalization_id, Snapshot),
            maps:get(normalization_id, Response)
        }
    of
        {Base, Base, Normalization, Normalization} -> classify_semantics(Snapshot, Response);
        {Base, Base, _, _} -> normalization_incomparable;
        _ -> version_incomparable
    end.

classify_semantics(Snapshot, Response) ->
    Same = maps:get(decision, Snapshot) =:= maps:get(decision, Response),
    case {maps:get(semantics_id, Snapshot), maps:get(semantics_id, Response), Same} of
        {<<"compat-v1">>, <<"compat-v1">>, true} ->
            comparable_match;
        {<<"compat-v1">>, <<"compat-v1">>, false} ->
            comparable_difference;
        {_, <<"enhanced-v1">>, true} ->
            enhanced_observation;
        {_, <<"enhanced-v1">>, false} ->
            case maps:get(decision, Snapshot) =:= allow andalso enhanced_hit(Response) of
                true -> enhanced_new_hit;
                false -> enhanced_difference
            end;
        _ ->
            semantics_incomparable
    end.

enhanced_hit(Response) ->
    lists:any(
        fun(Hit) ->
            maps:get(variant_type, Hit, <<"canonical">>) =/= <<"canonical">> orelse
                lists:any(
                    fun(Method) -> lists:member(Method, [<<"t2s">>, <<"gap">>, <<"regex">>]) end,
                    maps:get(match_methods, Hit, [])
                )
        end,
        maps:get(hits, Response, [])
    ).

init([]) ->
    process_flag(trap_exit, true),
    ets:new(?TABLE, [named_table, public, set, {write_concurrency, true}]),
    Mode = proplists:get_value(mode, config_ds:env(moderation_detector, []), legacy),
    ets:insert(?TABLE, {mode, Mode, make_ref()}),
    erlang:send_after(10, self(), poll),
    {ok, #{running => #{}}}.

handle_call({mode, Mode}, _, State) ->
    Conf = config_ds:env(moderation_detector, []),
    ok = application:set_env(
        imboy, moderation_detector, lists:keystore(mode, 1, Conf, {mode, Mode})
    ),
    ets:insert(?TABLE, {mode, Mode, make_ref()}),
    lists:foreach(
        fun({Key, _}) ->
            ets:delete(?TABLE, Key),
            count(mode_switch)
        end,
        jobs()
    ),
    Running = maps:map(
        fun(Pid, Entry) ->
            exit(Pid, kill),
            case maps:is_key(reported, Entry) of
                false -> count(mode_switch);
                true -> ok
            end,
            Entry#{reported => true}
        end,
        maps:get(running, State)
    ),
    {reply, ok, State#{running => Running}};
handle_call(status, _, State) ->
    [{mode, Mode, _}] = ets:lookup(?TABLE, mode),
    Counters = maps:from_list([
        {Name, Value}
     || {{counter, Name}, Value} <-
            ets:match_object(?TABLE, {{counter, '_'}, '_'})
    ]),
    {reply,
        #{
            mode => Mode,
            queued => length(jobs()),
            running => map_size(maps:get(running, State)),
            counters => Counters
        },
        State};
handle_call(_, _, State) ->
    {reply, {error, unsupported}, State}.

handle_cast(_, State) -> {noreply, State}.

handle_info(poll, State) ->
    Current = now_ms(),
    maps:foreach(
        fun(Pid, #{deadline := Deadline}) ->
            case Deadline =< Current of
                true -> exit(Pid, kill);
                false -> ok
            end
        end,
        maps:get(running, State)
    ),
    Next = lists:foldl(fun dispatch/2, State, jobs()),
    erlang:send_after(10, self(), poll),
    {noreply, Next};
handle_info({grpc_guard, Owner, Guard, Ref}, State) ->
    Running = maps:get(running, State),
    case maps:find(Owner, Running) of
        {ok, Entry} when not is_map_key(guard, Entry) ->
            Monitor = monitor(process, Guard),
            Guard ! {Ref, true},
            {noreply, State#{
                running => Running#{
                    Owner => Entry#{
                        guard => Guard,
                        guard_monitor => Monitor
                    }
                }
            }};
        _ ->
            Guard ! {Ref, false},
            {noreply, State}
    end;
handle_info({grpc_connection, Owner, Guard, Connection, Ref}, State) ->
    Running = maps:get(running, State),
    case maps:find(Owner, Running) of
        {ok, #{guard := Guard} = Entry} when not is_map_key(connection, Entry) ->
            Guard ! {Ref, true},
            {noreply, State#{
                running => Running#{
                    Owner => Entry#{
                        connection => Connection,
                        connection_monitor => monitor(process, Connection)
                    }
                }
            }};
        _ ->
            Guard ! {Ref, false},
            {noreply, State}
    end;
handle_info({grpc_cleanup, Owner, Guard}, State) ->
    Running = maps:get(running, State),
    case maps:find(Owner, Running) of
        {ok, #{guard := Guard} = Entry} ->
            {noreply, State#{
                running => release_slot(Owner, Entry#{cleanup_confirmed => true}, Running)
            }};
        _ ->
            {noreply, State}
    end;
handle_info({result, Pid, Category}, State) ->
    Running = maps:get(running, State),
    case maps:find(Pid, Running) of
        {ok, #{epoch := Epoch, deadline := Deadline} = Entry} when
            not is_map_key(reported, Entry)
        ->
            case ets:lookup(?TABLE, mode) of
                [{mode, shadow, Epoch}] ->
                    case now_ms() < Deadline of
                        true -> count(Category);
                        false -> count(expired)
                    end;
                _ ->
                    ok
            end;
        _ ->
            ok
    end,
    Next =
        case maps:find(Pid, Running) of
            {ok, CurrentEntry} ->
                State#{running => Running#{Pid => CurrentEntry#{reported => true}}};
            error ->
                State
        end,
    {noreply, Next};
handle_info({'DOWN', Monitor, process, Pid, Reason}, State) ->
    Running = maps:get(running, State),
    case maps:find(Pid, Running) of
        {ok, #{monitor := Monitor, deadline := Deadline, epoch := Epoch} = Entry} ->
            case ets:lookup(?TABLE, mode) of
                [{mode, shadow, Epoch}] when Reason =/= normal, not is_map_key(reported, Entry) ->
                    case now_ms() >= Deadline of
                        true -> count(expired);
                        false -> count(worker_unavailable)
                    end;
                _ ->
                    ok
            end,
            Next = release_slot(Pid, Entry#{worker_down => true, reported => true}, Running),
            {noreply, State#{running => Next}};
        _ ->
            Next = maps:fold(
                fun(Worker, Entry, Acc) ->
                    Updated =
                        case Entry of
                            #{guard := Pid, guard_monitor := Monitor} ->
                                Entry#{guard_down => true};
                            #{connection := Pid, connection_monitor := Monitor} ->
                                Entry#{cleanup_confirmed => true};
                            _ ->
                                Entry
                        end,
                    release_slot(Worker, Updated, Acc)
                end,
                Running,
                Running
            ),
            {noreply, State#{running => Next}}
    end;
handle_info(_, State) ->
    {noreply, State}.

release_slot(Worker, Entry, Running) ->
    WorkerDown = maps:get(worker_down, Entry, false),
    GuardDone =
        not maps:is_key(guard, Entry) orelse
            (maps:get(guard_down, Entry, false) andalso
                (maps:get(cleanup_confirmed, Entry, false) orelse
                    not maps:is_key(connection, Entry))),
    case WorkerDown andalso GuardDone of
        true ->
            case maps:find(connection_monitor, Entry) of
                {ok, Ref} -> demonitor(Ref, [flush]);
                error -> ok
            end,
            maps:remove(Worker, Running);
        false ->
            Running#{Worker => Entry}
    end.

dispatch({Key, {Epoch, Deadline, Scene, Fields, Snapshot}}, State) ->
    [{mode, Mode, CurrentEpoch}] = ets:lookup(?TABLE, mode),
    case {Epoch =:= CurrentEpoch andalso Mode =:= shadow, Deadline > now_ms()} of
        {false, _} ->
            ets:delete(?TABLE, Key),
            count(mode_switch),
            State;
        {_, false} ->
            ets:delete(?TABLE, Key),
            count(expired),
            State;
        {true, true} ->
            dispatch_ready(Key, Epoch, Deadline, Scene, Fields, Snapshot, State)
    end.

dispatch_ready(Key, Epoch, Deadline, Scene, Fields, Snapshot, State) ->
    Running = maps:get(running, State),
    case map_size(Running) < ?WORKERS of
        false ->
            State;
        true ->
            ets:delete(?TABLE, Key),
            Parent = self(),
            {Pid, Monitor} = spawn_opt(
                fun() ->
                    put(textguard_shadow_coordinator, Parent),
                    Category =
                        try
                            classify(Snapshot, check(Scene, Fields, Deadline))
                        catch
                            _:_ -> facility_failure
                        end,
                    Parent ! {result, self(), Category}
                end,
                [link, monitor]
            ),
            State#{
                running => Running#{
                    Pid => #{monitor => Monitor, deadline => Deadline, epoch => Epoch}
                }
            }
    end.

check(Scene, Fields, Deadline) ->
    RequestId = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    Transport = moderation_detector_transport:select(),
    case Transport of
        moderation_detector_transport ->
            {error, not_ready};
        _ ->
            Configured = proplists:get_value(
                timeout_ms, config_ds:env(moderation_detector, []), 150
            ),
            Budget = min(Configured, max(1, Deadline - now_ms())),
            Transport:check(
                #{request_id => RequestId, scene => Scene, fields => Fields},
                #{request_id => RequestId, deadline_ms => Budget}
            )
    end.

jobs() -> ets:match_object(?TABLE, {{job, '_'}, '_'}).

count(Category) ->
    ets:update_counter(?TABLE, {counter, Category}, 1, {{counter, Category}, 0}),
    elib_metric:increment(textguard_shadow, 1, #{error_code => Category}),
    ok.

now_ms() -> erlang:monotonic_time(millisecond).

terminate(_, State) ->
    maps:foreach(fun(Pid, _) -> exit(Pid, kill) end, maps:get(running, State)),
    ok.
