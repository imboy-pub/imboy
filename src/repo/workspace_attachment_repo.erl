-module(workspace_attachment_repo).
-export([scan/2, scan_organization/2, can_govern/2]).

%% One attachment-id cursor across all sources, before application ACL filtering.
scan(Ws, Filters) -> scan_context(Ws, Filters, false).
scan_organization(Org, Filters) -> scan_context(Org, Filters, true).

scan_context(Id, Filters, IsOrganization) ->
    Sql = <<
        "SELECT a.id,a.name,a.size,a.mime_type,a.created_at,a.creator_user_id,"
        " a.scope,a.scope_ref,a.path,a.status,COALESCE(g.title,c.name,tr.title) AS source_name,"
        " u.nickname AS creator_name,a.cipher,COALESCE(wr.retained_at,ear.created_at) AS retained_at,"
        " CASE WHEN wr.attachment_id IS NOT NULL OR ear.attachment_id IS NOT NULL"
        " THEN 'retained' ELSE 'unmanaged' END AS retention_state,"
        " COALESCE(g.workspace_id,c.workspace_id,tr.workspace_id) AS workspace_id,"
        " COALESCE(tr.organization_id,w.organization_id) AS organization_id,"
        " tr.id AS record_id,trr.role AS attachment_role"
        " FROM attachment a"
        " LEFT JOIN \"group\" g ON a.scope='group' AND a.scope_ref=g.id::text"
        " LEFT JOIN channel c ON a.scope='channel' AND a.scope_ref=c.id::text"
        " LEFT JOIN talk_record tr ON a.scope='talk_record' AND a.scope_ref=tr.id::text"
        " LEFT JOIN LATERAL (SELECT r.role FROM talk_record_attachment_ref r"
        " WHERE r.attachment_id=a.id AND r.record_id=tr.id AND r.state='bound'"
        " AND (r.lease_until IS NULL OR r.lease_until>now()) LIMIT 1) trr ON true"
        " LEFT JOIN workspace w ON w.id=COALESCE(g.workspace_id,c.workspace_id,tr.workspace_id)"
        " LEFT JOIN workspace_attachment_retention wr ON wr.attachment_id=a.id"
        " AND wr.workspace_id=COALESCE(g.workspace_id,c.workspace_id,tr.workspace_id)"
        " LEFT JOIN enterprise_attachment_retention ear ON ear.attachment_id=a.id"
        " AND ear.organization_id=tr.organization_id AND ear.talk_record_id=tr.id"
        " AND ear.source_kind='talk_record'"
        " LEFT JOIN \"user\" u ON u.id=a.creator_user_id"
        " WHERE a.status>=0 AND ("
        " (g.scope='workspace' AND g.status=1 AND w.status='active'"
        " AND (CASE WHEN $12::boolean THEN w.organization_id=$1 ELSE g.workspace_id=$1 END)) OR"
        " (c.scope='workspace' AND c.status=1 AND w.status='active'"
        " AND (CASE WHEN $12::boolean THEN w.organization_id=$1 ELSE c.workspace_id=$1 END)) OR"
        " (tr.scope='organization' AND tr.deleted_at IS NULL AND trr.role IS NOT NULL"
        " AND (CASE WHEN $12::boolean THEN tr.organization_id=$1 ELSE tr.workspace_id=$1 END)))"
        " AND ($2::text='' OR a.name ILIKE $2 ESCAPE '!' OR tr.title ILIKE $2 ESCAPE '!')"
        " AND ($3::bigint IS NULL OR (a.scope='group' AND g.id=$3))"
        " AND ($4::bigint IS NULL OR (a.scope='channel' AND c.id=$4))"
        " AND ($5::bigint IS NULL OR a.creator_user_id=$5)"
        " AND ($6::bigint IS NULL OR a.created_at>=to_timestamp($6/1000.0))"
        " AND ($7::bigint IS NULL OR a.created_at<to_timestamp($7/1000.0))"
        " AND ($8::bigint IS NULL OR a.id<$8)"
        " AND ($9::bigint IS NULL OR a.id=$9)"
        " AND ($10::text IS NULL OR $10='' OR u.nickname ILIKE $10 ESCAPE '!'"
        " OR u.account ILIKE $10 ESCAPE '!' OR a.creator_user_id::text ILIKE $10 ESCAPE '!')"
        " AND ($11::text='all' OR a.scope=$11) ORDER BY a.id DESC LIMIT 100"
    >>,
    Keys = [
        keyword,
        group_id,
        channel_id,
        user_id,
        from_ms,
        until_ms,
        cursor,
        attachment_id,
        user_keyword
    ],
    Params =
        [Id | [maps:get(Key, Filters, null) || Key <- Keys]] ++
            [maps:get(source, Filters, <<"all">>), IsOrganization],
    elib_pg:query(Sql, Params).

%% Metadata governance never grants content access; caller applies record ACL first.
can_govern(Uid, Org) ->
    case
        elib_pg:query(
            <<
                "SELECT 1 AS allowed FROM organization o"
                " JOIN organization_member m ON m.organization_id=o.id"
                " WHERE o.id=$1 AND o.status='active' AND m.user_id=$2"
                " AND m.status='active' AND m.role IN ('owner','admin')"
            >>,
            [Org, Uid]
        )
    of
        {ok, [_]} -> true;
        _ -> false
    end.
