-module(enterprise_oa_login_repo).
-moduledoc "Inbound OA login ticket persistence; all operations use the caller's transaction.".
-export([
    authority_signed_tx/3,
    issue_tx/6,
    issue_tx/7,
    lock_tx/3,
    pending_tx/4,
    consume_tx/2,
    authority_tx/3,
    person_tx/3,
    human_tx/2,
    device_tx/4,
    epoch_tx/2
]).

issue_tx(C, Digest, App, Cred, External, Spec) ->
    issue_tx(C, Digest, App, Cred, External, Spec, <<"bearer">>).

issue_tx(C, Digest, App, Cred, External, {Intent, Challenge}, Authentication) ->
    one(
        elib_pg:query(
            C,
            <<
                "INSERT INTO enterprise_oa_login_ticket "
                "(digest,application_id,credential_id,external_user_id,intent,challenge,issuer_authentication) "
                "VALUES($1,$2,$3,$4,$5,$6,$7) RETURNING expires_at"
            >>,
            [Digest, App, Cred, External, Intent, Challenge, Authentication]
        )
    ).

lock_tx(C, Digest, Stage) ->
    one(
        elib_pg:query(
            C,
            <<
                "SELECT * FROM enterprise_oa_login_ticket WHERE digest=$1 "
                "AND stage=$2 AND expires_at>clock_timestamp() FOR UPDATE"
            >>,
            [Digest, Stage]
        )
    ).

pending_tx(C, Old, New, Did) ->
    one(
        elib_pg:query(
            C,
            <<
                "UPDATE enterprise_oa_login_ticket SET digest=$2,stage='pending',device_id=$3, expires_at=clock_timestamp()+interval '5 minutes' "
                "WHERE digest=$1 AND stage='issued' AND expires_at>clock_timestamp() RETURNING expires_at"
            >>,
            [Old, New, Did]
        )
    ).

consume_tx(C, Digest) ->
    one(
        elib_pg:query(
            C,
            <<
                "UPDATE enterprise_oa_login_ticket SET stage='consumed' "
                "WHERE digest=$1 AND stage<>'consumed' AND expires_at>clock_timestamp() RETURNING digest"
            >>,
            [Digest]
        )
    ).

%% Lock issuer and relevant grants; re-run before returning to check wall-clock expiry.
authority_tx(C, App, Cred) ->
    case
        one(
            elib_pg:query(
                C,
                <<
                    "SELECT a.organization_id,a.application_key FROM enterprise_application a "
                    "JOIN enterprise_application_credential c ON c.application_id=a.id "
                    "WHERE a.id=$1 AND c.id=$2 AND a.status='active' AND c.status='active' "
                    "AND (c.expires_at IS NULL OR c.expires_at>clock_timestamp()) "
                    "AND a.allowed_scopes ? 'oa:login:issue' FOR UPDATE OF a FOR SHARE OF c"
                >>,
                [App, Cred]
            )
        )
    of
        {ok, Row} ->
            Grants = elib_pg:query(
                C,
                <<
                    "SELECT g.id FROM enterprise_application_grant g "
                    "JOIN enterprise_application_credential c ON c.application_id=g.application_id "
                    "JOIN enterprise_application a ON a.id=g.application_id "
                    "JOIN enterprise_application_grant_scope s ON s.grant_id=g.id "
                    "WHERE g.application_id=$1 AND c.id=$2 "
                    "AND g.organization_id IS NOT DISTINCT FROM a.organization_id "
                    "AND g.status='active' AND g.valid_from<=clock_timestamp() AND g.expires_at>clock_timestamp() "
                    "AND s.scope='oa:login:issue' FOR SHARE OF g,s"
                >>,
                [App, Cred]
            ),
            case Grants of
                {ok, [_ | _]} -> {ok, Row};
                {ok, []} -> {error, insufficient_scope};
                Error -> Error
            end;
        Error ->
            Error
    end.

person_tx(C, App, External) ->
    elib_pg:query(
        C,
        <<
            "SELECT user_id,status,binding_retired FROM enterprise_source_person "
            "WHERE application_id=$1 AND external_user_id=$2 FOR UPDATE"
        >>,
        [App, External]
    ).

human_tx(C, Uid) ->
    one(
        elib_pg:query(
            C,
            <<
                "SELECT id,account,nickname,email,mobile,avatar,gender,region,sign,status "
                "FROM \"user\" WHERE id=$1 AND account_type=0 AND status=1 FOR SHARE"
            >>,
            [Uid]
        )
    ).

device_tx(C, Uid, Did, Device) ->
    one(
        elib_pg:query(
            C,
            <<
                "INSERT INTO user_device "
                "(id,user_id,device_id,device_type,device_name,device_vsn,status,login_count) "
                "VALUES($1,$2,$3,$4,$5,$6,1,1) ON CONFLICT(user_id,device_id) DO UPDATE SET "
                "login_count=user_device.login_count+1,last_login_at=clock_timestamp() "
                "WHERE user_device.status=1 RETURNING id"
            >>,
            [
                elib_tsid:generate(user_device),
                Uid,
                Did,
                maps:get(<<"cos">>, Device, <<>>),
                maps:get(<<"dname">>, Device, <<>>),
                maps:get(<<"dvsn">>, Device, <<>>)
            ]
        )
    ).

epoch_tx(C, Uid) ->
    case
        elib_pg:execute(
            C,
            <<
                "INSERT INTO user_auth_epoch(user_id,epoch,updated_at) "
                "VALUES($1,1,CURRENT_TIMESTAMP) ON CONFLICT(user_id) DO NOTHING"
            >>,
            [Uid]
        )
    of
        {ok, _} ->
            one(
                elib_pg:query(
                    C, <<"SELECT epoch FROM user_auth_epoch WHERE user_id=$1 FOR SHARE">>, [Uid]
                )
            );
        Error ->
            Error
    end.

one({ok, [Row]}) -> {ok, Row};
one({ok, []}) -> {error, invalid_ticket};
one({error, Reason}) -> {error, Reason}.

%% Same issuer lifecycle and locking as the legacy flow, without Scope/Grant.
authority_signed_tx(C, App, Cred) ->
    one(
        elib_pg:query(
            C,
            <<
                "SELECT a.organization_id,a.application_key FROM enterprise_application a "
                "JOIN enterprise_application_credential c ON c.application_id=a.id "
                "WHERE a.id=$1 AND c.id=$2 AND a.status='active' AND c.status='active' "
                "AND (c.expires_at IS NULL OR c.expires_at>clock_timestamp()) "
                "FOR UPDATE OF a FOR SHARE OF c"
            >>,
            [App, Cred]
        )
    ).
