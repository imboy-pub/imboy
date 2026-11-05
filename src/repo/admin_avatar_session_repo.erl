-module(admin_avatar_session_repo).
-moduledoc "管理员头像上传会话的服务端归属".
-export([reserve_tx/3, owned/2]).

reserve_tx(C, Cid, AdminId) when is_binary(Cid), is_integer(AdminId), AdminId > 0 ->
    case
        re:run(
            Cid,
            <<"^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$">>,
            [{capture, none}]
        )
    of
        match ->
            case
                elib_pg:query(
                    C, <<"SELECT pg_advisory_xact_lock(hashtextextended(($1::uuid)::text,169))">>, [
                        Cid
                    ]
                )
            of
                {ok, _} -> find_reservation(C, Cid, AdminId);
                {error, _} = Error -> Error
            end;
        nomatch ->
            {error, invalid_upload_id}
    end;
reserve_tx(_, _, _) ->
    {error, invalid_upload_id}.

find_reservation(C, Cid, AdminId) ->
    case
        elib_pg:query(
            C,
            <<"SELECT admin_user_id FROM public.admin_avatar_session_owner WHERE client_upload_id=$1::uuid">>,
            [Cid]
        )
    of
        {ok, [#{<<"admin_user_id">> := AdminId}]} -> ok;
        {ok, [_]} -> {error, not_session_owner};
        {ok, []} -> reserve_new(C, Cid, AdminId);
        {error, _} = Error -> Error
    end.

reserve_new(C, Cid, AdminId) ->
    case
        elib_pg:query(
            C, <<"SELECT 1 FROM public.attach_pending WHERE client_upload_id=$1::uuid">>, [Cid]
        )
    of
        {ok, []} ->
            case
                elib_pg:execute(
                    C,
                    <<"INSERT INTO public.admin_avatar_session_owner (client_upload_id,admin_user_id) VALUES($1::uuid,$2)">>,
                    [Cid, AdminId]
                )
            of
                {ok, 1} -> ok;
                {error, _} = Error -> Error
            end;
        {ok, _} ->
            {error, unowned_session};
        {error, _} = Error ->
            Error
    end.

owned(Key, AdminId) when is_binary(Key), is_integer(AdminId), AdminId > 0 ->
    case
        elib_pg:query(
            <<"SELECT 1 FROM public.attach_pending p JOIN public.admin_avatar_session_owner o ON o.client_upload_id=p.client_upload_id WHERE p.object_key=$1 AND o.admin_user_id=$2">>,
            [Key, AdminId]
        )
    of
        {ok, [_]} -> ok;
        {ok, _} -> {error, not_session_owner};
        {error, _} = Error -> Error
    end;
owned(_, _) ->
    {error, not_session_owner}.
