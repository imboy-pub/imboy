%% IMBOY_ERLANG_QUALITY P1-03：root 终结事件协议（缺失/重复/零计数拒绝）。
%% 草稿（LP-03）正式化；事件按 root 域，跨分片身份稳定（EUNIT_120S §1.1）。
-module(imboy_quality_events).
-export([new/1, event/2, result/2]).

new([_ | _] = Expected) ->
    true = length(Expected) =:= length(lists:usort(Expected)),
    #{expected => Expected, started => [], finished => #{}, errors => []};
new(_) ->
    error(empty_or_invalid_manifest).

event({start, Root}, #{expected := Expected, started := Started} = State) ->
    case lists:member(Root, Expected) andalso not lists:member(Root, Started) of
        true -> State#{started => [Root | Started]};
        false -> reject({unexpected_or_duplicate_start, Root}, State)
    end;
event(
    {finish, Root, Status, Count, Cleanup},
    #{started := Started, finished := Finished} = State
) ->
    case
        lists:member(Root, Started) andalso not maps:is_key(Root, Finished) andalso
            Status =:= pass andalso is_integer(Count) andalso Count > 0 andalso
            Cleanup =:= ok
    of
        true -> State#{finished => Finished#{Root => Count}};
        false -> reject({invalid_finish, Root, Status, Count, Cleanup}, State)
    end;
event(Event, State) ->
    reject({unknown_event, Event}, State).

result(
    #{
        expected := Expected,
        started := Started,
        finished := Finished,
        errors := []
    },
    0
) ->
    case
        lists:sort(Expected) =:= lists:sort(Started) andalso
            lists:sort(Expected) =:= lists:sort(maps:keys(Finished))
    of
        true -> {ok, lists:sum(maps:values(Finished))};
        false -> {error, missing_terminal_events}
    end;
result(State, RawExit) ->
    {error, {raw_exit, RawExit, maps:get(errors, State)}}.

reject(Reason, #{errors := Errors} = State) -> State#{errors => [Reason | Errors]}.
