-module(talk_record_governance_logic).
-moduledoc "组织管理员面聊附件治理；独立内容ACL，仅治理metadata，审计与幂等同事务。".
-export([read/3, mutate/6]).

read(Uid, Rid, Aid) ->
    tx(fun(C) ->
        {Record, Asset, Ear} = value(talk_record_governance_repo:lock_target_tx(C, Uid, Rid, Aid)),
        {ok, payload(Record, Asset, Ear)}
    end).
mutate(Uid, Rid, Aid, IfMatch, Key, Body) ->
    case
        {
            talk_record_logic:valid_idempotency_key(Key),
            talk_record_logic:parse_if_match(IfMatch),
            parse(Body)
        }
    of
        {false, _, _} ->
            {error, idempotency_key_required};
        {_, {error, R}, _} ->
            {error, R};
        {_, _, {error, R}} ->
            {error, R};
        {true, {ok, Rev}, {ok, Op}} ->
            Resource = resource(Rid, Aid),
            case talk_record_logic:request_digest(<<"POST">>, Resource, Body) of
                {ok, Digest} ->
                    tx(fun(C) -> mutate_tx(C, Uid, Rid, Aid, Rev, Key, Resource, Digest, Op) end);
                _ ->
                    {error, invalid_body}
            end
    end.
mutate_tx(C, Uid, Rid, Aid, Rev, Key, Resource, Digest, Op) ->
    {Record, Asset, Ear} = value(talk_record_governance_repo:lock_target_tx(C, Uid, Rid, Aid)),
    Exp = elib_dt:add(elib_dt:now(), {talk_record_job_support:idempotency_ttl_seconds(), second}),
    case
        value(
            talk_record_idempotency_repo:record_tx(C, Uid, <<"POST">>, Resource, Key, Digest, Exp)
        )
    of
        inserted ->
            case maps:get(<<"revision">>, Record) =:= Rev of
                true -> ok;
                false -> abort(revision_mismatch)
            end,
            apply_op(C, Aid, Ear, Op),
            {ok, NewRev} = require(talk_record_governance_repo:bump_revision_tx(C, Rid, Rev)),
            Scheduled = value(talk_record_governance_repo:requeue_approved_tx(C, Record)),
            {ok, _} = require(
                talk_record_repo:insert_audit_tx(
                    C,
                    Rid,
                    Uid,
                    <<"attachment_governance">>,
                    Aid,
                    maps:from_list([{atom_to_binary(K, utf8), V} || {K, V} <- maps:to_list(Op)])
                )
            ),
            {ok, NewEar} = require(enterprise_attachment_retention_repo:find_tx(C, Aid)),
            Status =
                case Scheduled of
                    true -> <<"pending">>;
                    false -> maps:get(<<"cleanup_status">>, Record)
                end,
            P = (payload(
                Record#{<<"revision">> => NewRev, <<"cleanup_status">> => Status}, Asset, NewEar
            ))#{
                <<"cleanup_scheduled">> => Scheduled
            },
            ok = require(
                talk_record_idempotency_repo:complete_tx(
                    C,
                    Uid,
                    <<"POST">>,
                    Resource,
                    Key,
                    jsone:encode(#{<<"payload">> => P})
                )
            ),
            {ok, created, P};
        {existing, Row} ->
            replay(Row, Digest)
    end.

apply_op(C, Aid, Ear, #{op := <<"hold">>, reason := Reason}) ->
    live_ear(Ear),
    live_asset(C, Aid),
    ok = require(enterprise_attachment_retention_repo:hold_tx(C, Aid, Reason));
apply_op(C, Aid, Ear, #{op := <<"release_hold">>}) ->
    live_ear(Ear),
    case maps:get(<<"hold_state">>, Ear) of
        <<"held">> -> ok = require(enterprise_attachment_retention_repo:release_tx(C, Aid));
        _ -> abort(hold_not_set)
    end;
apply_op(C, Aid, Ear, #{op := <<"extend_retention">>, until := Until}) ->
    live_ear(Ear),
    live_asset(C, Aid),
    {ok, _} = require(enterprise_attachment_retention_repo:extend_tx(C, Aid, Until)),
    ok;
apply_op(C, Aid, #{<<"purge_state">> := <<"live">>}, #{op := <<"purge">>}) ->
    {ok, _} = require(enterprise_attachment_retention_repo:purge_tx(C, Aid)),
    ok;
apply_op(_, _, #{<<"purge_state">> := <<"purged">>}, #{op := <<"purge">>}) ->
    ok.
live_ear(#{<<"purge_state">> := <<"live">>}) -> ok;
live_ear(_) -> abort(already_purged).
live_asset(C, Aid) ->
    case elib_pg:query(C, <<"SELECT lifecycle_state FROM attachment WHERE id=$1">>, [Aid]) of
        {ok, [#{<<"lifecycle_state">> := <<"live">>}]} -> ok;
        {ok, _} -> abort(already_deleting);
        E -> require(E)
    end.

payload(R, A, E) ->
    #{
        <<"record_id">> => maps:get(<<"id">>, R),
        <<"attachment_id">> => maps:get(<<"id">>, A),
        <<"organization_id">> => maps:get(<<"organization_id">>, R),
        <<"workspace_id">> => maps:get(<<"workspace_id">>, R),
        <<"revision">> => maps:get(<<"revision">>, R),
        <<"retention_until">> => maps:get(<<"retention_until">>, E),
        <<"hold_state">> => maps:get(<<"hold_state">>, E),
        <<"hold_reason">> => maps:get(<<"hold_reason">>, E),
        <<"purge_state">> => maps:get(<<"purge_state">>, E),
        <<"purged_at">> => maps:get(<<"purged_at">>, E),
        <<"lifecycle_state">> => maps:get(<<"lifecycle_state">>, A),
        <<"cleanup_status">> => maps:get(<<"cleanup_status">>, R),
        <<"logical_deleted">> => maps:get(<<"deleted_at">>, R) =/= null,
        <<"retention_policy_scope">> => <<"instance">>,
        <<"enterprise_governance_integrated">> => true
    }.

parse(B) when is_map(B) ->
    Op = maps:get(<<"op">>, B, undefined),
    case {Op, lists:sort(maps:keys(B))} of
        {<<"hold">>, [<<"op">>, <<"reason">>]} ->
            parse_reason(Op, maps:get(<<"reason">>, B));
        {<<"release_hold">>, [<<"op">>]} ->
            {ok, #{op => Op}};
        {<<"purge">>, [<<"op">>]} ->
            {ok, #{op => Op}};
        {<<"extend_retention">>, [<<"op">>, <<"retention_until">>]} ->
            parse_until(Op, maps:get(<<"retention_until">>, B));
        _ ->
            {error, invalid_body}
    end;
parse(_) ->
    {error, invalid_body}.
parse_reason(Op, B) when is_binary(B), byte_size(B) =< 800 ->
    case unicode:characters_to_list(B, utf8) of
        Chars when is_list(Chars) ->
            case string:trim(Chars) of
                [] ->
                    {error, invalid_body};
                Trimmed when length(Trimmed) =< 200 ->
                    {ok, #{op => Op, reason => unicode:characters_to_binary(Trimmed)}};
                _ ->
                    {error, invalid_body}
            end;
        _ ->
            {error, invalid_body}
    end;
parse_reason(_, _) ->
    {error, invalid_body}.
parse_until(Op, B) when is_binary(B), byte_size(B) =< 64 ->
    try calendar:rfc3339_to_system_time(binary_to_list(B), [{unit, second}]) of
        N when N > 0 ->
            case N =< erlang:system_time(second) + 3650 * 86400 of
                true -> {ok, #{op => Op, until => B}};
                false -> {error, invalid_body}
            end
    catch
        _:_ -> {error, invalid_body}
    end;
parse_until(_, _) ->
    {error, invalid_body}.

resource(R, A) ->
    <<"/api/v1/talk-records/", (integer_to_binary(R))/binary, "/attachments/",
        (integer_to_binary(A))/binary, "/governance">>.
replay(Row, Digest) ->
    case maps:get(<<"payload_hash">>, Row) =:= Digest of
        false ->
            abort(idempotency_conflict);
        true ->
            case maps:get(<<"result_ref">>, Row, null) of
                #{<<"payload">> := P} ->
                    {ok, replay, P};
                B when is_binary(B) ->
                    case jsone:decode(B) of
                        #{<<"payload">> := P} -> {ok, replay, P};
                        _ -> abort(idempotency_pending)
                    end;
                _ ->
                    abort(idempotency_pending)
            end
    end.
require({error, R}) -> abort(R);
require(V) -> V.
value({ok, V}) -> V;
value({ok, existing, Row}) -> {existing, Row};
value(E) -> require(E).
abort(R) -> throw({talk_governance, R}).
tx(F) ->
    case elib_pg:with_tx(F, [{reraise, false}]) of
        {rollback, {talk_governance, R}} -> {error, R};
        {rollback, _} -> {error, db_error};
        {error, _} -> {error, db_error};
        V -> V
    end.
