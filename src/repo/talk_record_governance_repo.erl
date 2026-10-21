-module(talk_record_governance_repo).
-moduledoc "企业面聊持久附件真实治理登记；处理租约不重复延长保留期。".
-export([register_tx/3, facts_tx/2, lock_target_tx/4, bump_revision_tx/3, requeue_approved_tx/2]).

register_tx(C, Rid, Aid) ->
    case
        elib_pg:query(
            C, <<"SELECT scope,organization_id,workspace_id FROM talk_record WHERE id=$1">>, [Rid]
        )
    of
        {ok, [#{<<"scope">> := <<"personal">>}]} ->
            ok;
        {ok, [
            #{
                <<"scope">> := <<"organization">>,
                <<"organization_id">> := Org,
                <<"workspace_id">> := Ws
            }
        ]} ->
            case enterprise_attachment_retention_repo:upsert_talk_record_tx(C, Aid, Org, Rid) of
                {ok, _} -> workspace_tx(C, Rid, Aid, Org, Ws);
                E -> E
            end;
        {ok, []} ->
            {error, record_not_found};
        E ->
            E
    end.
workspace_tx(_, _, _, _, null) ->
    ok;
workspace_tx(C, Rid, Aid, Org, Ws) ->
    case
        elib_pg:execute(
            C,
            <<
                "INSERT INTO workspace_attachment_retention(attachment_id,organization_id,"
                "workspace_id,scope,scope_ref) VALUES($1,$2,$3,'talk_record',$4) ON CONFLICT(attachment_id) DO NOTHING"
            >>,
            [Aid, Org, Ws, integer_to_binary(Rid)]
        )
    of
        {ok, _} ->
            case
                elib_pg:query(
                    C,
                    <<
                        "SELECT 1 FROM workspace_attachment_retention WHERE attachment_id=$1"
                        " AND organization_id=$2 AND workspace_id=$3 AND scope='talk_record' AND scope_ref=$4"
                    >>,
                    [Aid, Org, Ws, integer_to_binary(Rid)]
                )
            of
                {ok, [_]} -> ok;
                {ok, []} -> {error, workspace_governance_conflict};
                E -> E
            end;
        E ->
            E
    end.
facts_tx(C, Rid) ->
    elib_pg:query(
        C,
        <<
            "SELECT r.scope,r.organization_id,r.retention_until,(r.retention_until>now()) AS retention_active,"
            " count(e.attachment_id) AS ear_rows,count(w.attachment_id) AS war_rows,"
            " count(*) FILTER(WHERE e.hold_state='held') AS held_count,"
            " count(*) FILTER(WHERE e.purge_state<>'purged') AS unpurged_count,"
            " count(*) FILTER(WHERE e.purge_state='live' AND e.retention_until>now()) AS retained_count,"
            " max(e.retention_until) FILTER(WHERE e.purge_state='live') AS effective_retention_until"
            " FROM talk_record r LEFT JOIN enterprise_attachment_retention e ON e.source_kind='talk_record'"
            " AND e.talk_record_id=r.id AND e.organization_id=r.organization_id"
            " LEFT JOIN workspace_attachment_retention w ON w.attachment_id=e.attachment_id AND w.scope='talk_record'"
            " WHERE r.id=$1 GROUP BY r.id"
        >>,
        [Rid]
    ).

%% Lock order matches organization membership/transfer: org -> member -> record -> asset -> EAR.
lock_target_tx(C, Uid, Rid, Aid) ->
    case
        elib_pg:query(
            C, <<"SELECT organization_id FROM talk_record WHERE id=$1 AND scope='organization'">>, [
                Rid
            ]
        )
    of
        {ok, [#{<<"organization_id">> := Org}]} ->
            case lock_admin_tx(C, Uid, Org) of
                ok -> lock_record_asset_tx(C, Rid, Aid, Org);
                E -> E
            end;
        {ok, []} ->
            {error, not_found};
        E ->
            E
    end.
lock_admin_tx(C, Uid, Org) ->
    case
        elib_pg:query(
            C, <<"SELECT id FROM organization WHERE id=$1 AND status='active' FOR SHARE">>, [Org]
        )
    of
        {ok, [_]} ->
            case
                elib_pg:query(
                    C,
                    <<
                        "SELECT role FROM organization_member WHERE organization_id=$1"
                        " AND user_id=$2 AND status='active' FOR SHARE"
                    >>,
                    [Org, Uid]
                )
            of
                {ok, [#{<<"role">> := Role}]} when Role =:= <<"owner">>; Role =:= <<"admin">> -> ok;
                {ok, _} -> {error, not_found};
                E -> E
            end;
        {ok, []} ->
            {error, not_found};
        E ->
            E
    end.
lock_record_asset_tx(C, Rid, Aid, Org) ->
    case
        elib_pg:query(
            C,
            <<
                "SELECT id,organization_id,workspace_id,revision,deleted_at,cleanup_status"
                " FROM talk_record WHERE id=$1 AND organization_id=$2 AND scope='organization' FOR UPDATE"
            >>,
            [Rid, Org]
        )
    of
        {ok, [R]} ->
            case
                elib_pg:query(
                    C,
                    <<
                        "SELECT id,lifecycle_state FROM attachment WHERE id=$1"
                        " AND scope='talk_record' AND scope_ref=$2 FOR UPDATE"
                    >>,
                    [Aid, integer_to_binary(Rid)]
                )
            of
                {ok, [A]} ->
                    case
                        elib_pg:query(
                            C,
                            <<
                                "SELECT * FROM enterprise_attachment_retention WHERE attachment_id=$1"
                                " AND organization_id=$2 AND talk_record_id=$3 AND source_kind='talk_record' FOR UPDATE"
                            >>,
                            [Aid, Org, Rid]
                        )
                    of
                        {ok, [E]} -> {ok, {R, A, E}};
                        {ok, []} -> {error, not_found};
                        E -> E
                    end;
                {ok, []} ->
                    {error, not_found};
                E ->
                    E
            end;
        {ok, []} ->
            {error, not_found};
        E ->
            E
    end.
bump_revision_tx(C, Rid, Rev) ->
    case
        elib_pg:query(
            C,
            <<
                "UPDATE talk_record SET revision=revision+1,updated_at=now()"
                " WHERE id=$1 AND revision=$2 RETURNING revision"
            >>,
            [Rid, Rev]
        )
    of
        {ok, [#{<<"revision">> := New}]} -> {ok, New};
        {ok, []} -> {error, revision_mismatch};
        E -> E
    end.
requeue_approved_tx(C, #{<<"id">> := Rid, <<"deleted_at">> := Deleted}) when Deleted =/= null ->
    case
        {
            facts_tx(C, Rid),
            talk_record_delete_repo:write_blocker_tx(C, Rid),
            talk_record_attachment_adapter:evaluate_release_blockers_tx(C, Rid)
        }
    of
        {{ok, [#{<<"unpurged_count">> := 0}]}, {ok, null}, {ok, none}} ->
            requeue_deleted_tx(C, Rid);
        {{error, _} = E, _, _} ->
            E;
        {_, {error, _} = E, _} ->
            E;
        {_, _, {error, _} = E} ->
            E;
        _ ->
            {ok, false}
    end;
requeue_approved_tx(_, _) ->
    {ok, false}.
requeue_deleted_tx(C, Rid) ->
    case
        elib_pg:query(
            C,
            <<
                "UPDATE talk_record SET cleanup_status='pending',updated_at=now() WHERE id=$1"
                " AND deleted_at IS NOT NULL AND cleanup_status IN ('blocked_hold','blocked_retention','retained_by_other_reference')"
                " RETURNING id"
            >>,
            [Rid]
        )
    of
        {ok, [_]} ->
            case talk_record_delete_repo:count_purge_jobs_tx(C, Rid) of
                {ok, N} ->
                    case talk_record_delete_repo:enqueue_purge_tx(C, Rid, N) of
                        {ok, _, _} -> {ok, true};
                        E -> E
                    end;
                E ->
                    E
            end;
        {ok, []} ->
            {ok, false};
        E ->
            E
    end.
