-module(eb_asset_visitor_access).
-export([authorize/1]).

authorize(#{
    org_id := Org,
    workspace_id := Ws,
    actor_id := Contact,
    conversation_id := Conv,
    visitor_session_id := Session,
    visitor_token_id := Token,
    visitor_installation_id := Installation
}) ->
    Sql = <<
        "SELECT 1 FROM customer_service_visit_token t "
        "JOIN customer_service_widget_installation i ON i.organization_id=t.organization_id "
        "AND i.id=t.widget_installation_id "
        "JOIN customer_service_session s ON s.organization_id=t.organization_id "
        "AND s.workspace_id=$2 AND s.id=$5 AND s.contact_id=t.contact_id "
        "AND s.conversation_id=$4 "
        "JOIN customer_service_visit_token original ON original.organization_id=s.organization_id "
        "AND original.id=s.visit_token_id AND original.widget_installation_id=i.id "
        "AND original.contact_id=s.contact_id "
        "WHERE t.organization_id=$1 AND t.id=$6 AND t.contact_id=$3 "
        "AND i.id=$7 AND i.status='active' AND t.revoked_at IS NULL "
        "AND t.expires_at>clock_timestamp()"
    >>,
    case elib_pg:query(Sql, [Org, Ws, Contact, Conv, Session, Token, Installation]) of
        {ok, [_]} -> ok;
        _ -> {error, not_found}
    end;
authorize(_) ->
    {error, not_found}.
