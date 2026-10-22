-module(talk_record_cleanup_retry_logic).
-moduledoc "Authenticated record-scoped cleanup recovery; no generic queue permission.".
%% elvis 5.0.4: no_operation_on_same_value crashes on this module's AST
%% ({badkey, attrs}); the rule is unrelated to the transaction control here.
-elvis([{elvis_style, no_operation_on_same_value, disable}]).
-export([retry/5, parse/1]).
retry(Uid, Rid, IfMatch, Key, Body) ->
    case
        {
            talk_record_logic:parse_if_match(IfMatch),
            talk_record_logic:valid_idempotency_key(Key),
            parse(Body)
        }
    of
        {{error, R}, _, _} ->
            {error, R};
        {_, false, _} ->
            {error, idempotency_key_required};
        {_, _, {error, R}} ->
            {error, R};
        {{ok, Rev}, true, {ok, Generation, Reason}} ->
            Resource =
                <<"/api/v1/talk-records/", (integer_to_binary(Rid))/binary, "/cleanup/retry">>,
            case talk_record_logic:request_digest(<<"POST">>, Resource, Body) of
                {ok, Digest} -> retry_valid(Uid, Rid, Rev, Generation, Key, Digest, Reason);
                _ -> {error, invalid_body}
            end
    end.
retry_valid(Uid, Rid, Rev, Generation, Key, Digest, Reason) ->
    Result = elib_pg:with_tx(
        fun(C) ->
            case
                talk_record_cleanup_retry_repo:retry_tx(
                    C, Uid, Rid, Rev, Generation, Key, Digest, Reason
                )
            of
                {ok, _} = Ok -> Ok;
                {error, R} -> exit({talk_record_rollback, {error, R}})
            end
        end,
        [{reraise, false}]
    ),
    case Result of
        {ok, P} ->
            ok = talk_record_job_worker:kick(),
            {ok, P};
        {rollback, {talk_record_rollback, {error, R}}} ->
            {error, R};
        {rollback, _} ->
            {error, db_error};
        E ->
            E
    end.
parse(B) when is_map(B) ->
    case
        {
            lists:sort(maps:keys(B)),
            maps:get(<<"cleanup_generation">>, B, undefined),
            maps:get(<<"reason">>, B, <<>>)
        }
    of
        {Keys, G, R} when
            is_integer(G), G >= 0, G =< 9223372036854775807, is_binary(R), byte_size(R) =< 800
        ->
            case
                Keys =:= [<<"cleanup_generation">>] orelse
                    Keys =:= [<<"cleanup_generation">>, <<"reason">>]
            of
                true ->
                    case unicode:characters_to_list(R, utf8) of
                        L when is_list(L), length(L) =< 200 -> {ok, G, R};
                        _ -> {error, invalid_body}
                    end;
                false ->
                    {error, invalid_body}
            end;
        _ ->
            {error, invalid_body}
    end;
parse(_) ->
    {error, invalid_body}.
