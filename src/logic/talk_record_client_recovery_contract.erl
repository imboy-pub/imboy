-module(talk_record_client_recovery_contract).
-moduledoc "No-grant discovery view and whole-job unprepared recovery predicate.".
-export([safe/1, view/5]).
safe(#{<<"has_task">> := false, <<"has_provider_fact">> := false, <<"has_uncertain_run">> := false}) ->
    true;
safe(_) ->
    false.
view(Run, Parts, Task, Ctx, Facts) ->
    R =
        case Run of
            null -> null;
            _ -> talk_record_client_ds:view(Run, Parts)
        end,
    T =
        case Task of
            null -> null;
            _ -> talk_record_provider_contract:view(Task)
        end,
    {Status, Actions} = decision(Run, Task, Ctx, Facts),
    #{
        <<"run">> => R,
        <<"task">> => T,
        <<"allowed_actions">> => Actions,
        <<"recovery_status">> => Status
    }.
decision(_, #{<<"state">> := <<"published">>}, _, _) ->
    {<<"published">>, []};
decision(_, #{<<"cancelled_at">> := C}, _, _) when C =/= null -> {<<"cancelled">>, []};
decision(Run, Task, Ctx, Facts) ->
    case busy(Run, Ctx) of
        true -> {<<"device_busy">>, []};
        false -> available(Run, Task, Facts)
    end.
available(_, Task, _) when Task =/= null ->
    {<<"task_available">>,
        maps:get(<<"allowed_actions">>, talk_record_provider_contract:view(Task))};
available(null, null, Facts) ->
    case safe(Facts) of
        true -> {<<"no_execution">>, [<<"claim">>]};
        false -> {<<"reconciliation_required">>, []}
    end;
available(_, null, Facts) ->
    case safe(Facts) of
        true -> {<<"ready">>, [<<"recover_unprepared">>]};
        false -> {<<"reconciliation_required">>, []}
    end.
busy(#{<<"state">> := <<"active">>, <<"lease_valid">> := true} = Run, #{uid := U, did := D}) ->
    maps:get(<<"actor_user_id">>, Run) =/= U orelse maps:get(<<"device_session_id">>, Run) =/= D;
busy(_, _) ->
    false.
