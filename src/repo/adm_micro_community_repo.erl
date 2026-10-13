-module(adm_micro_community_repo).
-export([transaction/1, authority_tx/2, channel_tx/2, essence_tx/5]).

transaction(Fun) -> elib_pg:with_tx(Fun).

authority_tx(Conn, AdminId) ->
    case
        query(Conn, <<"SELECT role_id FROM public.adm_user WHERE id=$1 AND status=1 FOR SHARE">>, [
            AdminId
        ])
    of
        [#{<<"role_id">> := RoleIds}] -> RoleIds;
        _ -> throw({rollback, forbidden})
    end.

channel_tx(Conn, Cid) ->
    case
        query(
            Conn,
            <<
                "SELECT id,status,scope,creator_uid,discussion_group_id FROM public.channel "
                "WHERE id=$1 AND status=1 FOR UPDATE"
            >>,
            [Cid]
        )
    of
        [C] -> C;
        _ -> throw({rollback, not_found})
    end.

essence_tx(Conn, AdminId, Cid, Mid, Value) ->
    case micro_community_content_repo:essence_tx(Conn, Cid, Mid, Value) of
        {ok, Row} ->
            audit_tx(Conn, AdminId, Cid, Mid, Value),
            {ok, Row};
        {error, _} = Error ->
            throw({rollback, Error})
    end.

audit_tx(Conn, AdminId, Cid, Mid, Value) ->
    Body = jsone:encode(#{
        actor_kind => <<"adm_user">>,
        admin_id => AdminId,
        channel_id => Cid,
        message_id => Mid,
        is_essence => Value
    }),
    Log = #{
        type => 901,
        uid => AdminId,
        body => Body,
        remark => <<"adm_channel_essence">>,
        created_at => elib_dt:now()
    },
    case user_log_repo:add(Conn, Log) of
        {ok, 1} -> ok;
        _ -> throw({rollback, audit_failed})
    end.

query(Conn, Sql, Params) ->
    case elib_pg:query(Conn, Sql, Params) of
        {ok, Rows} when is_list(Rows) -> Rows;
        {error, Reason} -> throw({rollback, {query_failed, Reason}})
    end.
