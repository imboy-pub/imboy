%%% Durable purge intents: capture keys before metadata deletion, never perform I/O in its transaction.
-module(eb_pg_asset_delete_queue).
-export([enqueue_in/5, drain/3]).

enqueue_in(Conn, Org, Ws, MessageIds, AssetIds) ->
    case
        elib_pg:execute(
            Conn,
            <<
                "INSERT INTO enterprise_asset_delete_queue(organization_id,workspace_id,asset_id,object_key)"
                " SELECT organization_id,workspace_id,id,object_key FROM enterprise_asset"
                " WHERE organization_id=$1 AND workspace_id=$2"
                " AND (message_id=ANY($3::bigint[]) OR id=ANY($4::bigint[]))"
                " ON CONFLICT (organization_id,workspace_id,asset_id) DO NOTHING"
            >>,
            [Org, Ws, MessageIds, AssetIds]
        )
    of
        {ok, _} -> ok;
        {error, Reason} -> throw({rollback, {object_intent_failed, Reason}})
    end.

drain(Org, Ws, Limit) ->
    case
        elib_pg:query(
            <<
                "SELECT asset_id,object_key FROM enterprise_asset_delete_queue"
                " WHERE organization_id=$1 AND workspace_id=$2 ORDER BY created_at,asset_id LIMIT $3"
            >>,
            [Org, Ws, Limit]
        )
    of
        {ok, Rows} -> {ok, lists:filtermap(fun(Row) -> drain_one(Org, Ws, Row) end, Rows)};
        {error, Reason} -> {error, {object_queue_query_failed, Reason}}
    end.

drain_one(Org, Ws, #{<<"asset_id">> := Id, <<"object_key">> := Key}) ->
    case eb_asset_store:delete_queued_object(Org, Ws, Key) of
        ok -> finish(Org, Ws, Id);
        {error, not_found} -> finish(Org, Ws, Id);
        {error, Reason} -> {true, {undefined, Id, {object_delete_failed, Reason}}}
    end.

finish(Org, Ws, Id) ->
    case
        elib_pg:execute(
            <<
                "DELETE FROM enterprise_asset_delete_queue WHERE organization_id=$1 AND workspace_id=$2 AND asset_id=$3"
            >>,
            [Org, Ws, Id]
        )
    of
        {ok, _} -> false;
        {error, Reason} -> {true, {undefined, Id, {object_intent_finish_failed, Reason}}}
    end.
