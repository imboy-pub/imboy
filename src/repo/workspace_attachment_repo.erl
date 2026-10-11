-module(workspace_attachment_repo).
-export([scan/2]).

%% Metadata candidates only. Application authorization precedes every projection.
scan(Ws, Filters) ->
    Sql = <<
        "SELECT a.id,a.name,a.size,a.mime_type,a.created_at,a.creator_user_id,"
        " a.scope,a.scope_ref,a.path,a.status,COALESCE(g.title,c.name) AS source_name,"
        " u.nickname AS creator_name,a.cipher,wr.retained_at,"
        " CASE WHEN wr.attachment_id IS NOT NULL THEN 'retained'"
        " ELSE 'unmanaged' END AS retention_state"
        " FROM attachment a"
        " LEFT JOIN workspace_attachment_retention wr"
        " ON wr.attachment_id=a.id AND wr.workspace_id=$1"
        " LEFT JOIN \"group\" g ON a.scope='group' AND a.scope_ref=g.id::text"
        " LEFT JOIN channel c ON a.scope='channel' AND a.scope_ref=c.id::text"
        " LEFT JOIN \"user\" u ON u.id=a.creator_user_id"
        " WHERE a.status>=0 AND ("
        " (g.workspace_id=$1 AND g.scope='workspace' AND g.status=1) OR"
        " (c.workspace_id=$1 AND c.scope='workspace' AND c.status=1))"
        " AND ($2::text='' OR a.name ILIKE $2 ESCAPE '!')"
        " AND ($3::bigint IS NULL OR (a.scope='group' AND g.id=$3))"
        " AND ($4::bigint IS NULL OR (a.scope='channel' AND c.id=$4))"
        " AND ($5::bigint IS NULL OR a.creator_user_id=$5)"
        " AND ($6::bigint IS NULL OR a.created_at>=to_timestamp($6/1000.0))"
        " AND ($7::bigint IS NULL OR a.created_at<to_timestamp($7/1000.0))"
        " AND ($8::bigint IS NULL OR a.id<$8)"
        " AND ($9::bigint IS NULL OR a.id=$9)"
        " AND ($10::text IS NULL OR $10='' OR u.nickname ILIKE $10 ESCAPE '!'"
        " OR u.account ILIKE $10 ESCAPE '!' OR a.creator_user_id::text ILIKE $10 ESCAPE '!')"
        " ORDER BY a.id DESC LIMIT 100"
    >>,
    Params = [
        Ws
        | [
            maps:get(Key, Filters, null)
         || Key <-
                [
                    keyword,
                    group_id,
                    channel_id,
                    user_id,
                    from_ms,
                    until_ms,
                    cursor,
                    attachment_id,
                    user_keyword
                ]
        ]
    ],
    elib_pg:query(Sql, Params).
