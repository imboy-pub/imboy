-module(talk_record_cleanup_retry_repo).
-export([retry_tx/8, capability_tx/3]).

retry_tx(C, Uid, Rid, Rev, Generation, Key, Digest, Reason) ->
    JobId = elib_tsid:generate(talk_record),
    case
        attachment_gc_repo:retry_talk_record_tx(
            C, Uid, Rid, Rev, Generation, Key, Digest, Reason, JobId
        )
    of
        {ok, [#{<<"result">> := Result}]} -> decode(Result);
        {ok, _} -> {error, db_error};
        {error, _} -> {error, db_error}
    end.

decode(B) when is_binary(B) ->
    try
        decode(jsone:decode(B))
    catch
        _:_ -> {error, db_error}
    end;
decode(#{<<"error">> := Code}) ->
    {error, error_code(Code)};
decode(#{<<"job_id">> := J, <<"reopened_resources">> := N} = P) when
    is_integer(J), J > 0, is_integer(N), N > 0
->
    {ok, P};
decode(_) ->
    {error, db_error}.

error_code(<<"not_found">>) -> not_found;
error_code(<<"invalid_body">>) -> invalid_body;
error_code(<<"record_not_deleted">>) -> record_not_deleted;
error_code(<<"idempotency_conflict">>) -> idempotency_conflict;
error_code(<<"revision_mismatch">>) -> revision_mismatch;
error_code(<<"cleanup_generation_conflict">>) -> cleanup_generation_conflict;
error_code(<<"cleanup_not_retryable">>) -> cleanup_not_retryable;
error_code(<<"cleanup_protected">>) -> cleanup_protected;
error_code(<<"cleanup_identity_unverified">>) -> cleanup_identity_unverified;
error_code(_) -> db_error.

%% A missing/unreadable projection is never permission. No resource paths escape.
capability_tx(C, Uid, Rid) ->
    Denied = #{
        <<"allowed">> => false,
        <<"generation">> => null,
        <<"exhausted_resources">> => 0,
        <<"reason">> => <<"unknown">>
    },
    try
        elib_pg:query(
            C, <<"SELECT public.fn_talk_record_cleanup_retry_capability($1,$2) AS capability">>, [
                Uid, Rid
            ]
        )
    of
        {ok, [#{<<"capability">> := B}]} when is_binary(B) ->
            try
                normalize_cap(jsone:decode(B), Denied)
            catch
                _:_ -> Denied
            end;
        {ok, [#{<<"capability">> := M}]} when is_map(M) -> normalize_cap(M, Denied);
        _ ->
            Denied
    catch
        _:_ -> Denied
    end.

normalize_cap(
    #{
        <<"allowed">> := A,
        <<"generation">> := G,
        <<"exhausted_resources">> := N,
        <<"reason">> := R
    } = M,
    Denied
) when
    is_boolean(A), is_integer(N), N >= 0
->
    Known = lists:member(R, [
        <<"eligible">>,
        <<"partial_protection">>,
        <<"protected">>,
        <<"not_exhausted">>,
        <<"identity_unverified">>,
        <<"permission_denied">>,
        <<"not_deleted">>
    ]),
    ValidGen = is_integer(G) andalso G >= 0,
    case
        Known andalso
            ((A andalso ValidGen andalso N > 0 andalso
                lists:member(R, [<<"eligible">>, <<"partial_protection">>])) orelse
                (not A andalso (G =:= null orelse ValidGen)))
    of
        true ->
            maps:with(
                [<<"allowed">>, <<"generation">>, <<"exhausted_resources">>, <<"reason">>], M
            );
        false ->
            Denied
    end;
normalize_cap(_, Denied) ->
    Denied.
