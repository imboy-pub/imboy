%%% 访客逐消息收据事务：凭证/安装 → 原安装会话 → 消息锁 → DB时钟复核 → 事实。
-module(cs_pg_visitor_read).
-export([confirm_visitor_message_reads/5, fetch_visitor_message_reads/4, sql_statements/0]).

sql_statements() ->
    #{
        credential =>
            <<"SELECT t.id,i.allowed_origins FROM customer_service_widget_installation i JOIN customer_service_visit_token t ON t.organization_id=i.organization_id AND t.widget_installation_id=i.id WHERE i.organization_id=$1 AND i.id=$2 AND t.id=$3 AND t.contact_id=$4 FOR SHARE OF i,t">>,
        session =>
            <<"SELECT s.id FROM customer_service_session s JOIN customer_service_visit_token original ON original.organization_id=s.organization_id AND original.id=s.visit_token_id WHERE s.organization_id=$1 AND s.workspace_id=$2 AND s.id=$3 AND s.contact_id=$4 AND s.conversation_id=$6 AND original.widget_installation_id=$5 AND original.contact_id=s.contact_id FOR UPDATE OF s FOR SHARE OF original">>,
        messages =>
            <<"SELECT id FROM enterprise_message WHERE organization_id=$1 AND workspace_id=$2 AND conversation_id=$3 AND id=ANY($4::bigint[]) AND sender_type='business_identity' AND visibility='visible' ORDER BY id FOR SHARE">>,
        clock =>
            <<"WITH instant AS MATERIALIZED (SELECT clock_timestamp() AS at) SELECT i.status AS installation_status,(t.revoked_at IS NOT NULL AND t.revoked_at<=instant.at) AS revoked,(t.expires_at<=instant.at) AS expired,to_char(instant.at AT TIME ZONE 'UTC','YYYY-MM-DD\"T\"HH24:MI:SS.US\"Z\"') AS read_at FROM customer_service_widget_installation i JOIN customer_service_visit_token t ON t.organization_id=i.organization_id AND t.widget_installation_id=i.id CROSS JOIN instant WHERE i.organization_id=$1 AND i.id=$2 AND t.id=$3 AND t.contact_id=$4">>,
        insert =>
            <<"INSERT INTO customer_service_visitor_message_read (organization_id,workspace_id,session_id,contact_id,conversation_id,message_id,read_at) SELECT $1,$2,$3,$4,$5,id,$7::timestamptz FROM unnest($6::bigint[]) AS requested(id) ON CONFLICT DO NOTHING">>,
        cursor =>
            <<"INSERT INTO customer_service_visitor_read_cursor AS current (organization_id,workspace_id,session_id,contact_id,conversation_id,highest_confirmed_id,last_advanced_at) VALUES ($1,$2,$3,$4,$5,$6,$7::timestamptz) ON CONFLICT (organization_id,workspace_id,session_id,contact_id) DO UPDATE SET highest_confirmed_id=GREATEST(current.highest_confirmed_id,EXCLUDED.highest_confirmed_id),last_advanced_at=CASE WHEN EXCLUDED.highest_confirmed_id>current.highest_confirmed_id THEN EXCLUDED.last_advanced_at ELSE current.last_advanced_at END RETURNING highest_confirmed_id">>,
        receipts =>
            <<"SELECT message_id,to_char(read_at AT TIME ZONE 'UTC','YYYY-MM-DD\"T\"HH24:MI:SS.US\"Z\"') AS read_at FROM customer_service_visitor_message_read WHERE organization_id=$1 AND workspace_id=$2 AND session_id=$3 AND contact_id=$4 AND conversation_id=$5 AND message_id=ANY($6::bigint[]) ORDER BY message_id">>
    }.

confirm_visitor_message_reads(Org, Ws, Scope, Ids, _At) ->
    case valid_scope(Org, Ws, Scope) andalso valid_ids(Ids) of
        false ->
            {error, {invalid_argument, visitor_read}};
        true ->
            case
                elib_pg:with_tx(fun(Conn) -> confirm_in(Conn, Org, Ws, Scope, lists:usort(Ids)) end)
            of
                {rollback, {error, Reason}} -> {error, Reason};
                {rollback, Reason} -> {error, Reason};
                Other -> Other
            end
    end.
confirm_in(Conn, Org, Ws, Scope, Ids) ->
    Credential = [
        Org,
        maps:get(installation_id, Scope),
        maps:get(token_id, Scope),
        maps:get(contact_id, Scope)
    ],
    CredentialRow = require_one(query(Conn, credential, Credential), visit_token_invalid),
    case
        cs_widget:origin_allowed(
            maps:get(origin, Scope, undefined),
            cs_pg_common:jsonb_read(maps:get(<<"allowed_origins">>, CredentialRow, [])),
            maps:get(request_host, Scope, undefined)
        )
    of
        {error, OriginReason} -> rollback(OriginReason);
        _ -> ok
    end,
    Base = base(Org, Ws, Scope),
    [_, _, Session, Contact, Conversation] = Base,
    require_one(
        query(Conn, session, [
            Org, Ws, Session, Contact, maps:get(installation_id, Scope), Conversation
        ]),
        not_found
    ),
    Messages = query(Conn, messages, [Org, Ws, Conversation, Ids]),
    case lists:sort([maps:get(<<"id">>, Row) || Row <- Messages]) =:= Ids of
        true -> ok;
        false -> rollback(not_found)
    end,
    Clock = require_one(query(Conn, clock, Credential), visit_token_invalid),
    At = credential_time(Clock),
    _ = query(Conn, insert, Base ++ [Ids, At]),
    Cursor = require_one(
        query(Conn, cursor, Base ++ [lists:last(Ids), At]), visitor_read_storage_failure
    ),
    Receipts = receipts(query(Conn, receipts, Base ++ [Ids])),
    case [maps:get(message_id, Row) || Row <- Receipts] =:= Ids of
        true ->
            Hint = #{
                workspace_id => Ws,
                session_id => Session,
                actor_kind => <<"visitor">>,
                action => <<"message.read">>,
                detail => #{<<"message_id">> => lists:last(Ids)}
            },
            case cs_pg_seat:insert_event_in(Conn, Org, Hint) of
                {ok, _} ->
                    {ok, #{
                        session_id => Session,
                        highest_confirmed_id => maps:get(<<"highest_confirmed_id">>, Cursor),
                        receipts => Receipts
                    }};
                {error, Reason} ->
                    rollback({visitor_read_hint_failed, Reason})
            end;
        false ->
            rollback(visitor_read_storage_failure)
    end.
credential_time(#{<<"installation_status">> := Status}) when Status =/= <<"active">> ->
    rollback(installation_revoked);
credential_time(#{<<"revoked">> := true}) ->
    rollback(token_revoked);
credential_time(#{<<"expired">> := true}) ->
    rollback(token_expired);
credential_time(#{
    <<"installation_status">> := <<"active">>,
    <<"revoked">> := false,
    <<"expired">> := false,
    <<"read_at">> := At
}) when is_binary(At) ->
    At;
credential_time(_) ->
    rollback(visit_token_invalid).
query(Conn, Key, Params) ->
    case elib_pg:query(Conn, maps:get(Key, sql_statements()), Params) of
        {ok, Rows} -> Rows;
        {error, Reason} -> rollback(cs_pg_common:normalize_error(Reason))
    end.
require_one([Row], _) when is_map(Row) -> Row;
require_one(_, Reason) -> rollback(Reason).
rollback(Reason) -> throw({rollback, {error, Reason}}).
base(Org, Ws, Scope) ->
    [
        Org,
        Ws,
        maps:get(session_id, Scope),
        maps:get(contact_id, Scope),
        maps:get(conversation_id, Scope)
    ].
receipts(Rows) ->
    [
        #{message_id => maps:get(<<"message_id">>, Row), read_at => maps:get(<<"read_at">>, Row)}
     || Row <- Rows
    ].
fetch_visitor_message_reads(Org, Ws, Scope, []) ->
    case valid_read_scope(Org, Ws, Scope) of
        true -> {ok, []};
        false -> {error, {invalid_argument, visitor_read}}
    end;
fetch_visitor_message_reads(Org, Ws, Scope, Ids) ->
    case valid_read_scope(Org, Ws, Scope) andalso valid_read_ids(Ids) of
        false ->
            {error, {invalid_argument, visitor_read}};
        true ->
            case
                elib_pg:query(
                    maps:get(receipts, sql_statements()), base(Org, Ws, Scope) ++ [lists:usort(Ids)]
                )
            of
                {ok, Rows} -> {ok, receipts(Rows)};
                {error, Reason} -> {error, cs_pg_common:normalize_error(Reason)}
            end
    end.
valid_scope(Org, Ws, Scope) ->
    valid_read_scope(Org, Ws, Scope) andalso
        lists:all(fun pos_id/1, [
            maps:get(installation_id, Scope, undefined), maps:get(token_id, Scope, undefined)
        ]).
valid_read_scope(Org, Ws, Scope) when is_map(Scope) ->
    lists:all(fun pos_id/1, [
        Org,
        Ws,
        maps:get(session_id, Scope, undefined),
        maps:get(contact_id, Scope, undefined),
        maps:get(conversation_id, Scope, undefined)
    ]);
valid_read_scope(_, _, _) ->
    false.
valid_ids(Ids) when is_list(Ids), length(Ids) >= 1, length(Ids) =< 200 ->
    lists:all(fun pos_id/1, Ids);
valid_ids(_) ->
    false.
valid_read_ids(Ids) when is_list(Ids), length(Ids) =< 200 -> lists:all(fun pos_id/1, Ids);
valid_read_ids(_) -> false.
pos_id(Id) -> is_integer(Id) andalso Id > 0 andalso Id =< 9223372036854775807.
