-module(talk_record_client_recovery_logic).
-export([execute/5]).
execute(A, #{uid := U, did := D} = Ctx, Rid, Jid, {H, B}) when
    is_integer(U), U > 0, is_binary(D), byte_size(D) > 0, byte_size(D) =< 128
->
    case user_device_ds:is_active(U, D) of
        true ->
            case body(A, B) of
                true -> talk_record_client_recovery_ds:execute(A, Ctx, Rid, Jid, {H, B});
                false -> {error, client_schema_invalid}
            end;
        _ ->
            {error, forbidden}
    end;
execute(_, _, _, _, _) ->
    {error, forbidden}.
body(latest, B) ->
    B =:= #{};
body(recover_unprepared, B) ->
    is_map(B) andalso map_size(B) =:= 1 andalso
        is_map(maps:get(<<"processing_consent">>, B, invalid)) andalso
        byte_size(jsone:encode(B)) =< 8192;
body(_, _) ->
    false.
