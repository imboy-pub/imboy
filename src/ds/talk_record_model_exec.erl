-module(talk_record_model_exec).
-moduledoc "有界本机模型执行监护；短事务续租，取消不强杀BEAM跳过finally。".
-export([run/4]).

run(Job, Config, Prepared, Fun) ->
    Parent = self(),
    Ref = make_ref(),
    {Pid, Mon} = spawn_monitor(fun() ->
        put(talk_model_job, Job),
        put(talk_model_parent_pid, Parent),
        put(talk_model_parent_monitor, erlang:monitor(process, Parent)),
        Result =
            try
                Fun()
            catch
                throw:processor_cleanup_unconfirmed -> {error, processor_cleanup_unconfirmed};
                _:_ -> {error, processor_execution_unconfirmed}
            end,
        Confirmed = receipt(Job, Result),
        talk_record_model_job:release_confirmed(Job, Prepared, Confirmed),
        Parent ! {Ref, Confirmed}
    end),
    Deadline = erlang:monotonic_time(millisecond) + maps:get(timeout_ms, Config) + 330000,
    watch(Job, Config, Prepared, Pid, Mon, Ref, Deadline).

watch(J, C, P, Pid, Mon, Ref, Deadline) ->
    Left = max(0, Deadline - erlang:monotonic_time(millisecond)),
    Wait = min(Left, maps:get(heartbeat_ms, C)),
    receive
        {Ref, Result} ->
            erlang:demonitor(Mon, [flush]),
            receipt(J, Result);
        {'DOWN', Mon, process, Pid, _} ->
            unknown(J, processor_execution_unconfirmed)
    after Wait ->
        case Left > 0 of
            false ->
                cancel(J, Pid, Mon, Ref, processor_timeout);
            true ->
                case talk_record_model_job:renew(J, C, P) of
                    ok -> watch(J, C, P, Pid, Mon, Ref, Deadline);
                    {error, R} -> cancel(J, Pid, Mon, Ref, R);
                    _ -> cancel(J, Pid, Mon, Ref, lease_lost)
                end
        end
    end.

cancel(J, Pid, Mon, Ref, Reason) ->
    Pid ! {talk_model_cancel, Reason},
    receive
        {Ref, Result} ->
            erlang:demonitor(Mon, [flush]),
            case receipt(J, Result) of
                {ok, published} = Success -> Success;
                {error, processor_stop_unconfirmed} = Unknown -> Unknown;
                {error, processor_execution_unconfirmed} = Unknown -> Unknown;
                {error, processor_cleanup_unconfirmed} = Unknown -> Unknown;
                _ -> {error, Reason}
            end;
        {'DOWN', Mon, process, Pid, _} ->
            unknown(J, processor_execution_unconfirmed)
    after 330000 -> unknown(J, processor_execution_unconfirmed)
    end.

receipt(J, {error, R}) when
    R =:= processor_stop_unconfirmed;
    R =:= processor_cleanup_unconfirmed;
    R =:= processor_execution_unconfirmed
->
    unknown(J, R);
receipt(J, Result) ->
    case talk_record_processor_run_repo:finish(J, <<"stopped">>) of
        ok -> Result;
        _ -> unknown(J, processor_execution_unconfirmed)
    end.
unknown(J, Reason) ->
    _ = talk_record_processor_run_repo:finish(J, <<"unknown">>),
    {error, Reason}.
