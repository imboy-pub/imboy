-module(forward_attachment_source_repo).
-moduledoc "事务内转发源消息资格；历史来源缺可信事实时失败关闭。".
-export([c2c_tx/3, c2g_tx/4]).

%% The receipt writer takes this same lock before accepting a revoke.
c2c_tx(Conn, MsgId, Uid) when is_binary(MsgId), is_integer(Uid), Uid > 0 ->
    Lock = <<"c2c_forward_revoke:", MsgId/binary>>,
    case lock(Conn, Lock) of
        ok ->
            exists(
                Conn,
                <<
                    "SELECT EXISTS (SELECT 1 FROM public.c2c_forward_source_fact f"
                    " JOIN public.msg_c2c m ON m.msg_id=f.msg_id"
                    " AND m.from_id=f.sender_id AND m.to_id=f.recipient_id AND m.conv_seq=f.conv_seq"
                    " WHERE f.msg_id=$1 AND ($2=f.sender_id OR $2=f.recipient_id)"
                    " AND (m.expire_at IS NULL OR m.expire_at>now())"
                    " AND NOT EXISTS (SELECT 1 FROM public.c2c_action_receipt r"
                    " WHERE r.original_msg_id=f.msg_id AND r.action='revoke')) AS allowed"
                >>,
                [MsgId, Uid]
            );
        Error ->
            Error
    end;
c2c_tx(_, _, _) ->
    {error, forbidden}.

%% The group revoke writer uses the same message lock before its fact commits.
c2g_tx(Conn, MsgId, Gid, Uid) when
    is_binary(MsgId), is_integer(Gid), Gid > 0, is_integer(Uid), Uid > 0
->
    case source_group_locks(Conn, Gid, Uid) of
        ok ->
            Lock = <<"c2g_forward_revoke:", (integer_to_binary(Gid))/binary, ":", MsgId/binary>>,
            case lock(Conn, Lock) of
                ok -> group_source(Conn, MsgId, Gid, Uid);
                Error -> Error
            end;
        Error ->
            Error
    end;
c2g_tx(_, _, _, _) ->
    {error, forbidden}.

source_group_locks(Conn, Gid, Uid) ->
    case attachment_repo:lock_upload_organization_tx(Conn, {group, Gid}, Uid) of
        ok ->
            case workspace_guard:ensure_writable_tx_retryable(Conn, {group, Gid}) of
                ok ->
                    case micro_community_access_repo:lock_group_tx(Conn, Gid) of
                        ok -> lock_group_member(Conn, Gid, Uid);
                        Error -> Error
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

lock_group_member(Conn, Gid, Uid) ->
    case
        elib_pg:query(
            Conn,
            <<
                "SELECT gm.user_id FROM public.group_member gm"
                " JOIN public.group_member_generation g ON g.group_id=gm.group_id AND g.user_id=gm.user_id"
                " WHERE gm.group_id=$1 AND gm.user_id=$2 AND gm.status=1 AND g.end_seq IS NULL"
                " FOR SHARE OF gm,g"
            >>,
            [Gid, Uid]
        )
    of
        {ok, [_]} -> ok;
        {ok, _} -> {error, forbidden};
        {error, _} = Error -> Error
    end.

group_source(Conn, MsgId, Gid, Uid) ->
    case msg_forward_repo:source_group_visible_tx(Conn, MsgId, Gid, Uid) of
        true ->
            exists(
                Conn,
                <<
                    "SELECT EXISTS (SELECT 1 FROM public.c2g_forward_source_fact f"
                    " JOIN public.msg_c2g_recipient_snapshot s ON s.msg_id=f.msg_id"
                    " AND s.to_gid=f.group_id AND s.from_id=f.sender_id AND s.conv_seq=f.conv_seq"
                    " WHERE f.msg_id=$1 AND f.group_id=$2"
                    " AND NOT EXISTS (SELECT 1 FROM public.c2g_revoke_fact r"
                    " WHERE r.group_id=f.group_id AND r.original_msg_id=f.msg_id)) AS allowed"
                >>,
                [MsgId, Gid]
            );
        false ->
            {error, forbidden}
    end.

lock(Conn, Key) ->
    case elib_pg:query(Conn, <<"SELECT pg_advisory_xact_lock(hashtextextended($1,0))">>, [Key]) of
        {ok, _} -> ok;
        {error, _} = Error -> Error
    end.

exists(Conn, Sql, Params) ->
    case elib_pg:query(Conn, Sql, Params) of
        {ok, [#{<<"allowed">> := true}]} -> ok;
        {ok, _} -> {error, forbidden};
        {error, _} = Error -> Error
    end.
