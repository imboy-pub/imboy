-module(cs_pg_inbox).

-export([seat_session_page/6, page_args/6, filters/1, sql_statements/0]).

-define(ROW_KEYS, [
    id,
    organization_id,
    workspace_id,
    contact_id,
    conversation_id,
    business_identity_id,
    visit_token_id,
    created_by_user_id,
    status,
    version,
    queued_at,
    claimed_at,
    closed_at,
    contact_subject_mask,
    last_message_id,
    last_message_sender_type,
    last_message_created_at,
    unread_count,
    needs_reply
]).
-define(COUNT_KEYS, [total, queued_total, active_total, closed_total]).

-define(SQL_PAGE, <<
    "WITH scoped AS ("
    " SELECT s.id, s.organization_id, s.workspace_id, s.contact_id,"
    " s.conversation_id, s.business_identity_id, s.visit_token_id,"
    " s.created_by_user_id, s.status, s.version,"
    " extract(epoch from s.queued_at)::bigint AS queued_at,"
    " extract(epoch from s.claimed_at)::bigint AS claimed_at,"
    " extract(epoch from s.closed_at)::bigint AS closed_at,"
    " COALESCE(NULLIF(ci.subject_mask, ''),"
    " CASE char_length(COALESCE(c.display_name, ''))"
    " WHEN 0 THEN 'guest#' || lpad((s.contact_id % 100000)::text, 5, '0')"
    " WHEN 1 THEN '*' WHEN 2 THEN left(c.display_name, 1) || '*'"
    " ELSE left(c.display_name, 1) || '***' || right(c.display_name, 1) END"
    " ) AS contact_subject_mask,"
    " CASE WHEN s.visit_token_id IS NOT NULL THEN 'widget'"
    " WHEN s.created_by_user_id IS NOT NULL THEN 'seat' ELSE 'shop_key' END AS source"
    " FROM customer_service_session s"
    " JOIN enterprise_contact c ON c.organization_id=s.organization_id AND c.id=s.contact_id"
    " LEFT JOIN LATERAL (SELECT i.subject_mask FROM enterprise_contact_identity i"
    " WHERE i.organization_id=s.organization_id AND i.contact_id=s.contact_id"
    " ORDER BY i.id LIMIT 1) ci ON true"
    " WHERE s.organization_id=$1 AND ($4::bigint=0 OR s.workspace_id=$4)"
    " AND (NOT $7::boolean OR s.business_identity_id=$6)"
    "), filtered AS MATERIALIZED ("
    " SELECT id, organization_id, workspace_id, contact_id, conversation_id,"
    " business_identity_id, visit_token_id, created_by_user_id, status, version,"
    " queued_at, claimed_at, closed_at, contact_subject_mask FROM scoped"
    " WHERE $8::text='' OR strpos(lower(contact_subject_mask), lower($8))>0"
    " OR strpos(lower(source), lower($8))>0 OR strpos(id::text, $8)>0"
    "), counts AS ("
    " SELECT count(*) FILTER (WHERE status=$2) AS total,"
    " count(*) FILTER (WHERE status='queued') AS queued_total,"
    " count(*) FILTER (WHERE status='active') AS active_total,"
    " count(*) FILTER (WHERE status='closed') AS closed_total FROM filtered"
    "), page AS ("
    " SELECT id, organization_id, workspace_id, contact_id, conversation_id,"
    " business_identity_id, visit_token_id, created_by_user_id, status, version,"
    " queued_at, claimed_at, closed_at, contact_subject_mask FROM filtered"
    " WHERE status=$2 AND ($3::bigint=0 OR id<$3) ORDER BY id DESC LIMIT $5"
    ") SELECT p.id, p.organization_id, p.workspace_id, p.contact_id, p.conversation_id,"
    " p.business_identity_id, p.visit_token_id, p.created_by_user_id, p.status, p.version,"
    " p.queued_at, p.claimed_at, p.closed_at, p.contact_subject_mask,"
    " lm.id AS last_message_id, lm.sender_type AS last_message_sender_type,"
    " extract(epoch from lm.created_at)::bigint AS last_message_created_at,"
    " facts.unread_count,"
    " (p.status <> 'closed' AND COALESCE(facts.contact_id, 0)>COALESCE(facts.business_id, 0)) AS needs_reply,"
    " counts.total, counts.queued_total, counts.active_total, counts.closed_total"
    " FROM counts LEFT JOIN page p ON true"
    " LEFT JOIN customer_service_read_cursor rc ON rc.organization_id=p.organization_id"
    " AND rc.workspace_id=p.workspace_id AND rc.session_id=p.id AND rc.business_identity_id=$6"
    " LEFT JOIN LATERAL ("
    " SELECT count(*) FILTER (WHERE m.sender_type='contact'"
    " AND m.id>COALESCE(rc.last_read_message_id, 0)) AS unread_count,"
    " max(m.id) FILTER (WHERE m.sender_type='contact') AS contact_id,"
    " max(m.id) FILTER (WHERE m.sender_type='business_identity') AS business_id"
    " FROM enterprise_message m WHERE m.organization_id=p.organization_id"
    " AND m.workspace_id=p.workspace_id AND m.conversation_id=p.conversation_id"
    " AND m.visibility='visible') facts ON true"
    " LEFT JOIN enterprise_message lm ON lm.organization_id=p.organization_id"
    " AND lm.workspace_id=p.workspace_id AND lm.conversation_id=p.conversation_id"
    " AND lm.id=GREATEST(facts.contact_id, facts.business_id) AND lm.visibility='visible'"
    " ORDER BY p.id DESC"
>>).

seat_session_page(OrgId, Status, AfterId, Limit, WorkspaceId, Filters) ->
    case page_args(OrgId, Status, AfterId, Limit, WorkspaceId, Filters) of
        {error, _} = Error ->
            Error;
        {ok, Args} ->
            case cs_pg_common:fetch_many(?SQL_PAGE, Args, ?ROW_KEYS ++ ?COUNT_KEYS) of
                {error, _} = Error -> Error;
                {ok, Rows} -> {ok, page_result(Rows)}
            end
    end.

page_args(OrgId, Status, AfterId, Limit, WorkspaceId, Filters) when
    is_integer(OrgId),
    OrgId > 0,
    OrgId =< 9223372036854775807,
    is_integer(WorkspaceId),
    WorkspaceId >= 0,
    WorkspaceId =< 9223372036854775807
->
    case
        {
            cs_app_support:session_status(Status),
            cs_app_support:page_cursor(#{after_id => AfterId, limit => Limit}),
            filters(Filters)
        }
    of
        {{ok, undefined}, _, _} ->
            {error, {invalid_status, undefined}};
        {{error, _} = Error, _, _} ->
            Error;
        {_, {error, _} = Error, _} ->
            Error;
        {_, _, {error, _} = Error} ->
            Error;
        {{ok, StatusBin}, {ok, Cursor, Size}, {ok, Normalized}} when
            Cursor =< 9223372036854775807
        ->
            {ok, [
                OrgId,
                StatusBin,
                Cursor,
                WorkspaceId,
                Size,
                maps:get(business_identity_id, Normalized),
                maps:get(mine, Normalized),
                maps:get(search, Normalized)
            ]};
        _ ->
            {error, {invalid_after_id, AfterId}}
    end;
page_args(_OrgId, _Status, _AfterId, _Limit, _WorkspaceId, _Filters) ->
    {error, {invalid_argument, seat_session_page}}.

filters(Filters) when is_map(Filters) ->
    case maps:keys(maps:without([business_identity_id, mine, search], Filters)) of
        [Key | _] -> {error, {unexpected_argument, Key}};
        [] -> identity_filters(Filters)
    end;
filters(_Filters) ->
    {error, {invalid_argument, inbox_filters}}.

identity_filters(#{business_identity_id := IdentityId} = Filters) when
    is_integer(IdentityId), IdentityId > 0, IdentityId =< 9223372036854775807
->
    case {mine(maps:get(mine, Filters, false)), search(maps:get(search, Filters, <<>>))} of
        {{ok, Mine}, {ok, Search}} ->
            {ok, #{business_identity_id => IdentityId, mine => Mine, search => Search}};
        {{error, _} = Error, _} ->
            Error;
        {_, {error, _} = Error} ->
            Error
    end;
identity_filters(_Filters) ->
    {error, {invalid_param_value, business_identity_id}}.

mine(true) -> {ok, true};
mine(false) -> {ok, false};
mine(<<"true">>) -> {ok, true};
mine(<<"false">>) -> {ok, false};
mine(_Mine) -> {error, {invalid_param_value, mine}}.

%% ponytail: preview search requires the enterprise shared owner's batch-read implementation.
search(Search) when is_binary(Search), byte_size(Search) =< 512 ->
    case unicode:characters_to_list(Search, utf8) of
        Chars when is_list(Chars), length(Chars) =< 128 ->
            {ok, unicode:characters_to_binary(string:trim(Chars))};
        _ ->
            {error, {invalid_param_value, search}}
    end;
search(_Search) ->
    {error, {invalid_param_value, search}}.

page_result([Counts | _] = Rows) ->
    #{
        rows => [maps:without(?COUNT_KEYS, Row) || #{id := Id} = Row <- Rows, is_integer(Id)],
        total => maps:get(total, Counts),
        total_by_status => #{
            queued => maps:get(queued_total, Counts),
            active => maps:get(active_total, Counts),
            closed => maps:get(closed_total, Counts)
        }
    }.

sql_statements() -> [?SQL_PAGE].
