-module(attachment_avatar_alias_repo).
-moduledoc "公开头像别名：先检查私有 winner，再以数据库状态发布。".
-export([reserve/1, publish/2, resolve/1, bind_tx/6, public_url/1, revoke_other_tx/3]).

reserve(Key) ->
    elib_pg:with_tx(
        fun(C) ->
            case attachment_repo:find_by_path_for_update_tx(C, Key) of
                {ok, #{<<"status">> := 1, <<"lifecycle_state">> := <<"live">>} = Row} ->
                    case attachment_repo:verify_direct_winner_tx(C, Key) of
                        ok ->
                            case winner_fence_tx(C, Key) of
                                {ok, Fence} -> reserve_tx(C, Row#{avatar_winner_fence => Fence});
                                Error -> Error
                            end;
                        Error ->
                            Error
                    end;
                {ok, _} ->
                    {error, avatar_not_validatable};
                Error ->
                    Error
            end
        end,
        [{reraise, false}]
    ).

reserve_tx(C, #{<<"id">> := Id} = Row) ->
    case
        elib_pg:query(
            C, <<"SELECT state FROM attachment_avatar_publication WHERE attachment_id=$1">>, [Id]
        )
    of
        {ok, []} ->
            reserve_alias_tx(C, Row);
        {ok, [#{<<"state">> := <<"complete">>}]} ->
            {ok, Row, #{<<"state">> => <<"legacy_complete">>}};
        {ok, [_]} ->
            {error, legacy_avatar_publication_requires_reconciliation};
        {error, _} = Error ->
            Error
    end.

reserve_alias_tx(C, #{<<"id">> := Id} = Row) ->
    case
        elib_pg:query(
            C,
            <<
                "SELECT alias_id::text,CASE WHEN state='published' AND bound_user_id IS NULL "
                "AND preview_expires_at<=clock_timestamp() THEN 'unpublished' ELSE state END AS state "
                "FROM attachment_avatar_alias WHERE attachment_id=$1"
            >>,
            [Id]
        )
    of
        {ok, []} -> {ok, Row, #{<<"state">> => <<"unpublished">>}};
        {ok, [Alias]} -> {ok, Row, Alias};
        {error, _} = Error -> Error
    end.

publish(Key, Snapshot) ->
    Fence = maps:get(avatar_winner_fence, Snapshot, undefined),
    Source = maps:remove(avatar_winner_fence, Snapshot),
    elib_pg:with_tx(
        fun(C) ->
            case attachment_repo:find_by_path_for_update_tx(C, Key) of
                {ok, Row} when Row =:= Source ->
                    case
                        {attachment_repo:verify_direct_winner_tx(C, Key), winner_fence_tx(C, Key)}
                    of
                        {ok, {ok, Fence}} when Fence =/= undefined -> publish_tx(C, Row);
                        {{error, _} = Error, _} -> Error;
                        {_, {error, _} = Error} -> Error;
                        _ -> {error, avatar_source_changed}
                    end;
                {ok, _} ->
                    {error, avatar_source_changed};
                Error ->
                    Error
            end
        end,
        [{reraise, false}]
    ).

winner_fence_tx(C, Key) ->
    Sql = <<"SELECT id,attempt_no,fence FROM public.fn_avatar_winner_fence($1)">>,
    case elib_pg:query(C, Sql, [Key]) of
        {ok, [Fence]} -> {ok, Fence};
        {ok, _} -> {error, avatar_winner_changed};
        {error, _} = Error -> Error
    end.

publish_tx(C, #{<<"id">> := Id, <<"path">> := Key}) ->
    case
        elib_pg:query(
            C,
            <<
                "INSERT INTO attachment_avatar_alias(attachment_id,object_key,state)"
                " VALUES($1,$2,'published') ON CONFLICT (attachment_id) DO UPDATE"
                " SET alias_id=CASE WHEN attachment_avatar_alias.bound_user_id IS NULL"
                " AND attachment_avatar_alias.preview_expires_at<=clock_timestamp()"
                " THEN EXCLUDED.alias_id ELSE attachment_avatar_alias.alias_id END,"
                " preview_expires_at=CASE WHEN attachment_avatar_alias.bound_user_id IS NULL"
                " AND attachment_avatar_alias.preview_expires_at<=clock_timestamp()"
                " THEN EXCLUDED.preview_expires_at ELSE attachment_avatar_alias.preview_expires_at END"
                " WHERE attachment_avatar_alias.state='published'"
                " RETURNING alias_id::text"
            >>,
            [Id, Key]
        )
    of
        {ok, [#{<<"alias_id">> := Alias}]} -> {ok, Alias};
        {ok, _} -> {error, avatar_alias_revoked};
        {error, _} = Error -> Error
    end.

resolve(Alias) ->
    case public_url(Alias) of
        {ok, Url} -> resolve_url(Alias, Url);
        {error, _} = Error -> Error
    end.

resolve_url(Alias, Url) ->
    elib_pg:with_conn(fun(C) ->
        case
            elib_pg:query(
                C,
                <<
                    "SELECT a.path FROM attachment_avatar_alias v"
                    " JOIN attachment a ON a.id=v.attachment_id"
                    " WHERE v.alias_id=$1::uuid AND v.state='published'"
                    " AND a.status=1 AND a.lifecycle_state='live'"
                    " AND public.fn_avatar_alias_bound_visible(v.alias_id,$2)"
                >>,
                [Alias, Url]
            )
        of
            {ok, [#{<<"path">> := Key}]} ->
                case attachment_repo:verify_direct_winner_tx(C, Key) of
                    ok -> attachment_repo:find_by_path_for_update_tx(C, Key);
                    Error -> Error
                end;
            {ok, _} ->
                {error, not_found};
            {error, _} = Error ->
                Error
        end
    end).

bind_tx(C, Key, Cid, UserId, IsCreate, AdminId) ->
    case attachment_repo:find_by_path_for_update_tx(C, Key) of
        {ok, #{<<"status">> := 1, <<"lifecycle_state">> := <<"live">>}} ->
            case attachment_repo:verify_direct_winner_tx(C, Key) of
                ok -> bind_locked_tx(C, Key, Cid, UserId, IsCreate, AdminId);
                Error -> Error
            end;
        {ok, _} ->
            {error, avatar_source_changed};
        Error ->
            Error
    end.

bind_locked_tx(C, Key, Cid, UserId, IsCreate, AdminId) ->
    Sql = <<
        "UPDATE attachment_avatar_alias v SET bound_user_id=$4"
        " FROM attach_pending p, admin_avatar_session_owner o"
        " WHERE v.object_key=$1 AND p.object_key=v.object_key AND p.client_upload_id=$2::uuid"
        " AND o.client_upload_id=p.client_upload_id AND o.admin_user_id=$5"
        " AND p.direct_state='CONFIRMED' AND p.session_state='confirmed'"
        " AND p.confirmed_attachment_id=v.attachment_id AND v.state='published'"
        " AND (v.bound_user_id IS NOT NULL OR v.preview_expires_at>clock_timestamp())"
        " AND p.upload_registration->'subject'->>'kind'='application'"
        " AND p.upload_registration->'subject'->>'org_id'='0'"
        " AND (CASE WHEN $6 THEN CASE WHEN p.upload_registration->'subject'->>'principal'"
        " ~* '^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$'"
        " THEN (p.upload_registration->'subject'->>'principal')::uuid=$2::uuid ELSE false END"
        " ELSE p.upload_registration->'subject'->>'principal'=$3 END)"
        " AND (v.bound_user_id IS NULL OR v.bound_user_id=$4) RETURNING v.alias_id::text"
    >>,
    Principal =
        case IsCreate of
            true -> Cid;
            false -> integer_to_binary(UserId)
        end,
    case elib_pg:query(C, Sql, [Key, Cid, Principal, UserId, AdminId, IsCreate]) of
        {ok, [#{<<"alias_id">> := Alias}]} -> {ok, Alias};
        {ok, _} -> {error, avatar_session_mismatch};
        {error, _} = Error -> Error
    end.

public_url(Alias) ->
    Base = config_ds:env(avatar_edge_base_url, <<>>),
    case valid_base(Base) of
        true -> {ok, <<(string:trim(Base, trailing, "/"))/binary, "/_avatar/", Alias/binary>>};
        false -> {error, edge_not_configured}
    end.

valid_base(Base) when is_binary(Base), byte_size(Base) > 0 ->
    case uri_string:parse(Base) of
        #{scheme := Scheme, host := Host} = Uri when
            (Scheme =:= <<"https">> orelse Scheme =:= <<"http">>), byte_size(Host) > 0
        ->
            not maps:is_key(userinfo, Uri) andalso not maps:is_key(query, Uri) andalso
                not maps:is_key(fragment, Uri) andalso
                lists:member(maps:get(path, Uri, <<>>), [<<>>, <<"/">>]);
        _ ->
            false
    end;
valid_base(_) ->
    false.

revoke_other_tx(C, UserId, Key) ->
    case
        elib_pg:execute(
            C,
            <<
                "UPDATE attachment_avatar_alias SET state='revoked'"
                " WHERE bound_user_id=$1 AND object_key<>$2 AND state='published'"
            >>,
            [UserId, Key]
        )
    of
        {ok, _} -> ok;
        {error, _} = Error -> Error
    end.
