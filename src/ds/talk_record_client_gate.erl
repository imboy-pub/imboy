-module(talk_record_client_gate).
-moduledoc "Locked live ACL, organization, consent, input and authenticated-device fences.".
-export([record_tx/4, job_tx/3, run_tx/5, pins_tx/3, require/1, abort/1, ensure/2, mode/2, json/1]).

record_tx(C, #{uid := Uid, did := Did}, Rid, Access) ->
    true_or(
        is_integer(Uid) andalso Uid > 0 andalso is_binary(Did) andalso
            byte_size(Did) > 0 andalso byte_size(Did) =< 128,
        forbidden
    ),
    Observed = require(talk_record_repo:find_visible_tx(C, Uid, Rid)),
    Context = record_context(Observed),
    ok = require(talk_record_context_ds:validate_context_tx(C, Uid, Context)),
    R = require(talk_record_client_repo:lock_record_tx(C, Rid)),
    true_or(record_context(R) =:= Context, context_changed),
    true_or(maps:get(<<"deleted_at">>, R) =:= null, not_found),
    true_or(maps:get(<<"custody_status">>, R) =:= <<"active">>, context_changed),
    _ = require(talk_record_repo:find_visible_tx(C, Uid, Rid)),
    live_user(C, Uid),
    editable(C, Uid, R, Access),
    R.

record_context(R) ->
    #{
        scope => maps:get(<<"scope">>, R),
        organization_id => maps:get(<<"organization_id">>, R),
        workspace_id => null_zero(maps:get(<<"workspace_id">>, R))
    }.

editable(_, _, _, read) ->
    ok;
editable(_, Uid, #{<<"owner_user_id">> := Uid}, write) ->
    ok;
editable(C, Uid, #{<<"id">> := Rid}, write) ->
    case
        elib_pg:query(
            C,
            <<
                "SELECT id FROM talk_record_grant WHERE record_id=$1 AND grantee_user_id=$2"
                " AND role='editor' AND revoked_at IS NULL AND (expires_at IS NULL OR expires_at>clock_timestamp()) FOR SHARE"
            >>,
            [Rid, Uid]
        )
    of
        {ok, [_]} -> ok;
        _ -> abort(forbidden)
    end.

job_tx(C, R, Jid) ->
    J = require(talk_record_client_repo:lock_job_tx(C, maps:get(<<"id">>, R), Jid)),
    true_or(
        lists:member(maps:get(<<"kind">>, J), [<<"transcribe">>, <<"summarize">>]),
        invalid_processing_mode
    ),
    {J, pins_tx(C, R, J)}.

pins_tx(C, R, J) ->
    mode(R, J),
    true_or(maps:get(<<"media_status">>, R) =:= <<"available">>, media_not_ready),
    Config = require(talk_record_client_policy:get()),
    true_or(talk_record_client_policy:allowed(R, Config), forbidden),
    Public = maps:get(public, Config),
    Pin = json(maps:get(<<"processor_input">>, J)),
    true_or(
        maps:get(<<"config_version">>, Pin, undefined) =:= maps:get(<<"config_version">>, Public),
        processing_consent_required
    ),
    true_or(
        maps:get(<<"policy_version">>, Pin, undefined) =:= maps:get(<<"policy_version">>, Public),
        processing_consent_required
    ),
    true_or(
        maps:get(<<"input_content_revision">>, J) =:= maps:get(<<"content_revision">>, R),
        input_stale
    ),
    A = require(talk_record_assembly_logic:find_tx(C, maps:get(<<"id">>, R))),
    true_or(
        maps:get(<<"input_manifest_hash">>, J) =:= maps:get(<<"manifest_hash">>, A), input_stale
    ),
    check_transcript(C, R, J),
    check_summary(C, R, J, Pin),
    Config.

check_summary(C, R, #{<<"kind">> := <<"summarize">>}, Pin) ->
    true_or(
        maps:get(<<"summary_snapshot">>, Pin, undefined) =:=
            talk_record_job_support:summary_snapshot_tx(C, maps:get(<<"id">>, R)),
        input_stale
    );
check_summary(_, _, _, _) ->
    ok.

check_transcript(C, R, #{<<"kind">> := <<"summarize">>} = J) ->
    T = require(
        talk_record_transcript_repo:find_current_tx(
            C, maps:get(<<"id">>, R), <<"t.version,t.status">>
        )
    ),
    true_or(
        maps:get(<<"version">>, T) =:= maps:get(<<"input_transcript_version">>, J) andalso
            maps:get(<<"status">>, T) =/= <<"no_speech">>,
        input_stale
    );
check_transcript(_, _, _) ->
    ok.

run_tx(C, Context, J, Run, Token) ->
    true_or(
        maps:get(<<"actor_user_id">>, Run) =:= maps:get(uid, Context) andalso
            maps:get(<<"device_session_id">>, Run) =:= maps:get(did, Context),
        forbidden
    ),
    true_or(
        is_binary(Token) andalso byte_size(Token) >= 32 andalso byte_size(Token) =< 128, lease_lost
    ),
    Hash = binary:encode_hex(crypto:hash(sha256, Token), lowercase),
    true_or(
        talk_record_client_contract:constant_equal(Hash, maps:get(<<"lease_token_hash">>, Run)),
        lease_lost
    ),
    true_or(maps:get(<<"attempt">>, Run) =:= maps:get(<<"attempt">>, J), lease_lost),
    true_or(lists:member(maps:get(<<"state">>, J), [<<"running">>, <<"succeeded">>]), lease_lost),
    consent(C, Context, Run, maps:get(<<"kind">>, J)),
    ok.

consent(C, #{uid := Uid}, Run, Kind) ->
    case
        elib_pg:query(
            C,
            <<
                "SELECT id FROM talk_record_consent_event WHERE id=$1 AND record_id=$2 AND actor_id=$3"
                " AND kind='processing_opt_in' AND policy_version=$4 AND processor_config_version=$5 AND processing_mode=$6 FOR SHARE"
            >>,
            [
                maps:get(<<"consent_event_id">>, Run),
                maps:get(<<"record_id">>, Run),
                Uid,
                maps:get(<<"policy_version">>, Run),
                maps:get(<<"config_version">>, Run),
                Kind
            ]
        )
    of
        {ok, [_]} -> ok;
        _ -> abort(processing_consent_required)
    end.

require({ok, V}) -> V;
require({ok, A, B}) -> {A, B};
require(ok) -> ok;
require({error, R}) -> abort(R).
abort(R) -> throw({talk_client_abort, R}).
true_or(true, _) -> ok;
true_or(false, R) -> abort(R).
null_zero(null) -> 0;
null_zero(V) -> V.
json(B) when is_binary(B) -> jsone:decode(B);
json(V) -> V.

ensure(true, _) -> ok;
ensure(false, R) -> abort(R).

live_user(C, Uid) ->
    case
        require(
            elib_pg:query(
                C,
                <<"SELECT id FROM public.\"user\" WHERE id=$1 AND status=1 AND account_type=0 FOR SHARE">>,
                [Uid]
            )
        )
    of
        [_] -> ok;
        [] -> abort(forbidden)
    end.

mode(R, J) ->
    Kind = maps:get(<<"kind">>, J),
    Pin = json(maps:get(<<"processor_input">>, J)),
    ensure(
        lists:member(Kind, [<<"transcribe">>, <<"summarize">>]) andalso
            maps:get(<<"processing_mode">>, R, undefined) =:= Kind andalso
            maps:get(<<"processing_mode">>, Pin, undefined) =:= Kind,
        processing_consent_required
    ).
