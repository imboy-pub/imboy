-module(talk_record_provider_logic).
-moduledoc "Bounded JWT-only provider control entry. Provider HTTP stays on the owning device.".
-export([execute/5]).
execute(A, #{uid := U, did := D} = Context, Ids, H, B) when
    is_integer(U), U > 0, is_binary(D), byte_size(D) > 0, byte_size(D) =< 128
->
    case user_device_ds:is_active(U, D) of
        true ->
            case talk_record_provider_contract:validate(A, B) of
                ok -> talk_record_provider_ds:execute(A, Context, Ids, H, B);
                E -> E
            end;
        _ ->
            {error, forbidden}
    end;
execute(_, _, _, _, _) ->
    {error, forbidden}.
