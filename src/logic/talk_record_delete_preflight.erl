-module(talk_record_delete_preflight).
-moduledoc "Mandatory read-only deletion facts; retained identity references fail closed until"
" an approved anonymization flow exists.".
-export([facts/1, facts_tx/2]).
-include("log.hrl").

facts(Uid) when is_integer(Uid), Uid > 0 ->
    build(Uid, elib_pg:one(sql(), [Uid]));
facts(_) ->
    {error, unavailable}.

%% Same-transaction final guard, in addition to the explanatory provider read.
facts_tx(Conn, Uid) when is_integer(Uid), Uid > 0 ->
    case elib_pg:query(Conn, sql(), [Uid]) of
        {ok, [Row]} -> build(Uid, {ok, Row});
        _ -> {error, unavailable}
    end.

sql() ->
    Ws = organization_resource_access_repo:workspace_predicate_sql(
        <<"tr.workspace_id">>, <<"$1">>
    ),
    <<"SELECT count(*) FILTER (WHERE scope='personal' AND deleted_at IS NULL) AS personal_active,",
        " count(*) FILTER (WHERE scope='personal' AND deleted_at IS NOT NULL",
        " AND cleanup_status <> 'complete') AS personal_cleanup,",
        " count(*) FILTER (WHERE scope='organization' AND deleted_at IS NULL) AS organization_owned,",
        " count(*) FILTER (WHERE scope='organization' AND deleted_at IS NULL AND (",
        " custody_status='handover_required' OR NOT (",
        " EXISTS (SELECT 1 FROM organization o WHERE o.id=tr.organization_id AND o.status='active')",
        " AND EXISTS (SELECT 1 FROM organization_member om WHERE om.organization_id=tr.organization_id",
        " AND om.user_id=$1 AND om.status='active') AND (tr.workspace_id IS NULL OR (", Ws/binary,
        "))", "))) AS handover_count, (", (identity_sql())/binary, ") AS identity_refs",
        ", ARRAY(SELECT id::text FROM talk_record WHERE owner_user_id=$1",
        " AND scope='personal' AND deleted_at IS NOT NULL AND cleanup_status <> 'complete'",
        " ORDER BY id LIMIT 20) AS cleanup_record_ids",
        " FROM talk_record tr WHERE owner_user_id=$1">>.

%% These NOT NULL references are RESTRICT in migration 209. Do not replace the
%% immutable creator, claim anonymization, or cascade-delete retained records.
identity_sql() ->
    <<"(SELECT count(*) FROM talk_record WHERE owner_user_id=$1 OR created_by=$1)",
        "+(SELECT count(*) FROM talk_record_grant WHERE grantee_user_id=$1 OR granted_by=$1)",
        "+(SELECT count(*) FROM talk_record_capture WHERE started_by=$1)",
        "+(SELECT count(*) FROM talk_record_mark WHERE author_id=$1)",
        "+(SELECT count(*) FROM talk_record_export WHERE requester_id=$1)",
        "+(SELECT count(*) FROM talk_record_consent_event WHERE actor_id=$1)",
        "+(SELECT count(*) FROM talk_record_idempotency WHERE actor_id=$1)">>.

build(
    Uid,
    {ok, #{
        <<"personal_active">> := Active,
        <<"personal_cleanup">> := Cleanup,
        <<"organization_owned">> := Owned,
        <<"handover_count">> := Handover,
        <<"identity_refs">> := Identity,
        <<"cleanup_record_ids">> := CleanupIds
    }}
) ->
    {ok, #{
        subject_user_id => Uid,
        domain => talk_record,
        observed_at => os:system_time(millisecond),
        fact_version => 1,
        resources => [#{record_id => binary_to_integer(Id)} || Id <- CleanupIds],
        cleanup_resources_truncated => Cleanup > length(CleanupIds),
        blockers => lists:append([
            blockers(Uid, Active, <<"TALK_RECORD_PERSONAL_PENDING">>),
            blockers(Uid, Cleanup, <<"TALK_RECORD_CLEANUP_PENDING">>),
            blockers(Uid, Owned, <<"TALK_RECORD_ORGANIZATION_OWNER_PENDING">>),
            blockers(Uid, Identity, <<"TALK_RECORD_RETAINED_IDENTITY_PENDING">>)
        ]),
        counts => #{
            personal_active => Active,
            personal_cleanup_pending => Cleanup,
            organization_owned => Owned,
            organization_handover_required => Handover,
            retained_identity_references => Identity
        }
    }};
build(_Uid, _) ->
    {error, unavailable}.

blockers(_Uid, 0, _Code) ->
    [];
blockers(Uid, Count, Code) when is_integer(Count), Count > 0 ->
    [
        #{
            code => Code,
            resource_type => <<"account">>,
            resource_id => integer_to_binary(Uid),
            organization_id => null
        }
    ].
