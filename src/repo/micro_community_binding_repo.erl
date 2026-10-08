-module(micro_community_binding_repo).
-export([
    transaction/1,
    channel_tx/3,
    read_tx/3,
    read_sql/0,
    group_tx/4,
    pristine_tx/3,
    write_tx/4,
    candidates_tx/5,
    guard_group_tx/2,
    guard_channel_tx/2,
    pristine_sql/0,
    lock_tables_tx/1,
    channel_manager_sql/0,
    platform_channel_tx/2,
    platform_group_tx/3,
    write_platform_tx/4,
    platform_candidates_tx/4
]).

transaction(Fun) -> elib_pg:with_tx(Fun).

%% Rare management operation: serialize even legacy writers without per-group guards.
%% Sorted global table locks precede all entity row locks. Failures abort the transaction.
lock_tables_tx(Conn) ->
    Tables = [
        <<"agent_task">>,
        <<"attachment">>,
        <<"channel">>,
        <<"channel_admin">>,
        <<"\"group\"">>,
        <<"group_album">>,
        <<"group_album_photo">>,
        <<"group_file">>,
        <<"group_member">>,
        <<"group_notice">>,
        <<"group_schedule">>,
        <<"group_task">>,
        <<"group_vote">>,
        <<"msg_c2g">>,
        <<"msg_c2g_request_ledger">>,
        <<"msg_store">>,
        <<"msg_store_seq">>,
        <<"msg_store_staging">>
    ],
    [lock_table(Conn, Table) || Table <- lists:sort(Tables)],
    ok.

lock_table(Conn, Table) ->
    Sql = <<"LOCK TABLE public.", Table/binary, " IN SHARE ROW EXCLUSIVE MODE">>,
    case elib_pg:execute(Conn, Sql, []) of
        {ok, _} -> ok;
        {error, Reason} -> throw({rollback, {binding_lock_failed, Reason}});
        Other -> throw({rollback, {binding_lock_failed, Other}})
    end.

channel_manager_sql() ->
    <<
        "COALESCE((SELECT ca.role FROM public.channel_admin ca WHERE ca.channel_id=c.id "
        "AND ca.user_id=$2 AND ca.role>0 LIMIT 1), CASE WHEN c.creator_uid=$2 "
        "THEN 3 ELSE 0 END)>=2"
    >>.

%% ID and entitlement are read in one statement; never return a separately cached ID.
read_tx(Conn, Uid, Cid) ->
    _ = rows(Conn, <<"SELECT id FROM public.channel WHERE id=$1 FOR SHARE">>, [Cid]),
    _ = rows(
        Conn,
        <<
            "SELECT user_id FROM public.channel_admin WHERE channel_id=$1 "
            "AND user_id=$2 FOR SHARE"
        >>,
        [Cid, Uid]
    ),
    _ = rows(
        Conn,
        <<
            "SELECT gm.user_id FROM public.group_member gm JOIN public.channel c "
            "ON c.discussion_group_id=gm.group_id WHERE c.id=$1 AND gm.user_id=$2 FOR SHARE OF gm"
        >>,
        [Cid, Uid]
    ),
    case rows(Conn, read_sql(), [Cid, Uid]) of
        [#{<<"discussion_group_id">> := Gid}] -> {ok, #{discussion_group_id => Gid}};
        _ -> throw({rollback, forbidden})
    end.

read_sql() ->
    Manager = channel_manager_sql(),
    <<
        "SELECT c.discussion_group_id FROM public.channel c "
        "LEFT JOIN public.\"group\" g ON g.id=c.discussion_group_id "
        "WHERE c.id=$1 AND c.status=1 AND c.scope='personal' "
        "AND (c.discussion_group_id IS NULL OR (g.status=1 AND g.scope='personal' "
        "AND g.owner_uid=c.creator_uid AND g.type=2 AND g.join_limit=3 AND g.e2ee_mode=0)) "
        "AND (((",
        Manager/binary,
        ") "
        "AND (c.discussion_group_id IS NULL OR g.owner_uid=$2 OR EXISTS(SELECT 1 "
        "FROM public.group_member gm WHERE gm.group_id=g.id AND gm.user_id=$2 "
        "AND gm.status=1 AND gm.role>=3))) OR (EXISTS(SELECT 1 FROM public.channel_subscription cs "
        "WHERE cs.channel_id=c.id AND cs.user_id=$2 AND cs.status=1) "
        "AND (c.access_type<>1 OR EXISTS(SELECT 1 FROM public.channel_order co "
        "WHERE co.channel_id=c.id AND co.user_id=$2 AND co.status=1 "
        "AND (co.subscription_end_at IS NULL OR co.subscription_end_at>clock_timestamp())))))"
    >>.

channel_tx(Conn, Uid, Cid) ->
    _ = rows(Conn, <<"SELECT id FROM public.channel WHERE id=$1 FOR UPDATE">>, [Cid]),
    _ = rows(
        Conn,
        <<
            "SELECT user_id FROM public.channel_admin WHERE channel_id=$1 "
            "AND user_id=$2 FOR UPDATE"
        >>,
        [Cid, Uid]
    ),
    Manager = channel_manager_sql(),
    Sql =
        <<"SELECT c.*, (", Manager/binary,
            ") AS manages "
            "FROM public.channel c WHERE c.id=$1 FOR UPDATE OF c">>,
    row(Conn, Sql, [Cid, Uid]).

group_tx(Conn, Uid, Gid, Owner) ->
    _ = rows(Conn, <<"SELECT id FROM public.\"group\" WHERE id=$1 FOR UPDATE">>, [Gid]),
    _ = rows(
        Conn,
        <<
            "SELECT user_id FROM public.group_member WHERE group_id=$1 "
            "AND user_id=$2 FOR UPDATE"
        >>,
        [Gid, Uid]
    ),
    Sql = <<
        "SELECT g.*, (g.owner_uid=$2 OR EXISTS (SELECT 1 FROM public.group_member gm "
        "WHERE gm.group_id=g.id AND gm.user_id=$2 AND gm.status=1 AND gm.role>=3)) AS manages "
        "FROM public.\"group\" g WHERE g.id=$1 FOR UPDATE OF g"
    >>,
    G = row(Conn, Sql, [Gid, Uid]),
    case valid_group(G, Owner) of
        true ->
            lock_sequence(Conn, Gid),
            G;
        false ->
            throw({rollback, invalid_group})
    end.

valid_group(
    #{
        <<"manages">> := true,
        <<"owner_uid">> := Owner,
        <<"scope">> := <<"personal">>,
        <<"type">> := 2,
        <<"join_limit">> := 3,
        <<"status">> := 1,
        <<"e2ee_mode">> := 0
    },
    Owner
) ->
    true;
valid_group(_, _) ->
    false.

%% These structural entry points require fresh platform ACL checks by the caller.
%% They do not impersonate a personal creator or infer admin identity from a user ID.
platform_channel_tx(Conn, Cid) ->
    case row(Conn, <<"SELECT * FROM public.channel WHERE id=$1 FOR UPDATE">>, [Cid]) of
        #{<<"scope">> := <<"personal">>, <<"status">> := 1} = C -> C;
        _ -> throw({rollback, forbidden})
    end.

platform_group_tx(Conn, Gid, Owner) ->
    G = row(Conn, <<"SELECT * FROM public.\"group\" WHERE id=$1 FOR UPDATE">>, [Gid]),
    case G of
        #{
            <<"owner_uid">> := Owner,
            <<"scope">> := <<"personal">>,
            <<"type">> := 2,
            <<"join_limit">> := 3,
            <<"status">> := 1,
            <<"e2ee_mode">> := 0
        } ->
            lock_sequence(Conn, Gid),
            G;
        _ ->
            throw({rollback, invalid_group})
    end.

lock_sequence(Conn, Gid) ->
    Sql = <<
        "INSERT INTO public.msg_store_seq(conv_key,seq) VALUES('c2g:'||$1::bigint::text,0) "
        "ON CONFLICT(conv_key) DO UPDATE SET seq=public.msg_store_seq.seq RETURNING seq"
    >>,
    _ = rows(Conn, Sql, [Gid]),
    ok.

pristine_tx(Conn, Gid, Owner) ->
    case rows(Conn, pristine_sql(), [Gid, Owner]) of
        [#{<<"pristine">> := true}] -> ok;
        _ -> throw({rollback, group_not_pristine})
    end.

%% Deleted content is deliberately included: this is an unused-group predicate.
pristine_sql() ->
    Checks =
        [
            <<"SELECT 1 FROM public.group_member WHERE group_id=$1 AND user_id<>$2">>,
            <<"SELECT 1 FROM public.msg_c2g WHERE to_id=$1">>,
            <<"SELECT 1 FROM public.msg_store_staging WHERE type='c2g' AND to_id=$1">>,
            <<"SELECT 1 FROM public.msg_store WHERE conv_key='c2g:'||$1::bigint::text">>,
            <<"SELECT 1 FROM public.msg_c2g_request_ledger WHERE to_gid=$1">>,
            <<"SELECT 1 FROM public.attachment WHERE scope='group' AND scope_ref=$1::bigint::text">>
        ] ++
            [
                <<"SELECT 1 FROM public.", T/binary, " WHERE group_id=$1">>
             || T <- [
                    <<"group_notice">>,
                    <<"group_file">>,
                    <<"group_album">>,
                    <<"group_album_photo">>,
                    <<"group_task">>,
                    <<"group_schedule">>,
                    <<"group_vote">>,
                    <<"agent_task">>
                ]
            ],
    Conditions = lists:join(<<" AND ">>, [<<"NOT EXISTS (", Q/binary, ")">> || Q <- Checks]),
    iolist_to_binary([
        <<
            "SELECT (EXISTS (SELECT 1 FROM public.group_member WHERE group_id=$1 "
            "AND user_id=$2 AND status=1 AND role>=3) AND "
        >>,
        Conditions,
        <<") AS pristine">>
    ]).

write_tx(Conn, Uid, Cid, {Old, New}) ->
    update_binding_tx(Conn, Cid, New),
    audit_tx(Conn, Uid, Cid, Old, New).

write_platform_tx(Conn, AdminId, Cid, {Old, New}) ->
    update_binding_tx(Conn, Cid, New),
    Gid =
        case New of
            null -> Old;
            _ -> New
        end,
    Body = jsone:encode(#{
        actor_kind => <<"adm_user">>,
        admin_id => AdminId,
        channel_id => Cid,
        old_group_id => Old,
        new_group_id => New
    }),
    case
        group_log_repo:add(Conn, #{
            type => 302,
            option_uid => AdminId,
            group_id => Gid,
            body => Body,
            created_at => elib_dt:now()
        })
    of
        {ok, _} -> ok;
        _ -> throw({rollback, binding_audit_failed})
    end.

update_binding_tx(Conn, Cid, New) ->
    case New of
        null ->
            ok;
        _ ->
            case
                rows(
                    Conn,
                    <<"SELECT id FROM public.channel WHERE discussion_group_id=$1 AND id<>$2">>,
                    [New, Cid]
                )
            of
                [] -> ok;
                _ -> throw({rollback, binding_conflict})
            end
    end,
    case
        elib_pg:execute(
            Conn,
            <<
                "UPDATE public.channel SET discussion_group_id=$2, "
                "updated_at=CURRENT_TIMESTAMP WHERE id=$1"
            >>,
            [Cid, New]
        )
    of
        {ok, 1} -> ok;
        _ -> throw({rollback, binding_write_failed})
    end.

audit_tx(Conn, Uid, Cid, Old, New) ->
    Gid =
        case New of
            null -> Old;
            _ -> New
        end,
    Body = jsone:encode(#{channel_id => Cid, old_group_id => Old, new_group_id => New}),
    case
        group_log_repo:add(Conn, #{
            type => 301,
            option_uid => Uid,
            group_id => Gid,
            body => Body,
            created_at => elib_dt:now()
        })
    of
        {ok, _} -> ok;
        _ -> throw({rollback, binding_audit_failed})
    end.

candidates_tx(Conn, Uid, Owner, Page, Size) ->
    Pristine = binary:replace(
        binary:replace(pristine_sql(), <<"$1">>, <<"g.id">>, [global]),
        <<"$2">>,
        <<"g.owner_uid">>,
        [global]
    ),
    Sql = <<
        "SELECT g.id,g.title FROM public.\"group\" g WHERE g.owner_uid=$1 "
        "AND g.scope='personal' AND g.type=2 AND g.join_limit=3 AND g.status=1 AND g.e2ee_mode=0 "
        "AND NOT EXISTS(SELECT 1 FROM public.channel c WHERE c.discussion_group_id=g.id) "
        "AND (g.owner_uid=$2 OR EXISTS(SELECT 1 FROM public.group_member gm "
        "WHERE gm.group_id=g.id AND gm.user_id=$2 AND gm.status=1 AND gm.role>=3)) "
        "AND (",
        Pristine/binary,
        ") "
        "ORDER BY g.id DESC LIMIT $3 OFFSET $4"
    >>,
    CountSql = binary:replace(
        binary:replace(
            Sql,
            <<"SELECT g.id,g.title">>,
            <<"SELECT count(*) AS total">>
        ),
        <<"ORDER BY g.id DESC LIMIT $3 OFFSET $4">>,
        <<>>
    ),
    [#{<<"total">> := Total}] = rows(Conn, CountSql, [Owner, Uid]),
    #{list => rows(Conn, Sql, [Owner, Uid, Size, (Page - 1) * Size]), total => Total}.

platform_candidates_tx(Conn, Owner, Page, Size) ->
    Pristine = binary:replace(
        binary:replace(pristine_sql(), <<"$1">>, <<"g.id">>, [global]),
        <<"$2">>,
        <<"g.owner_uid">>,
        [global]
    ),
    Where = <<
        " FROM public.\"group\" g WHERE g.owner_uid=$1 AND g.scope='personal' "
        "AND g.type=2 AND g.join_limit=3 AND g.status=1 AND g.e2ee_mode=0 "
        "AND NOT EXISTS(SELECT 1 FROM public.channel c WHERE c.discussion_group_id=g.id) "
        "AND (",
        Pristine/binary,
        ")"
    >>,
    [#{<<"total">> := Total}] = rows(Conn, <<"SELECT count(*) AS total", Where/binary>>, [Owner]),
    List = rows(
        Conn, <<"SELECT g.id,g.title", Where/binary, " ORDER BY g.id DESC LIMIT $2 OFFSET $3">>, [
            Owner, Size, (Page - 1) * Size
        ]
    ),
    #{list => List, total => Total, page => Page, size => Size}.

%% Deletion/owner transfer callers invoke these inside their existing transaction.
guard_group_tx(Conn, Gid) ->
    case
        rows(Conn, <<"SELECT id FROM public.channel WHERE discussion_group_id=$1 FOR UPDATE">>, [
            Gid
        ])
    of
        [] -> ok;
        _ -> {error, binding_conflict}
    end.

guard_channel_tx(Conn, Cid) ->
    case
        row(Conn, <<"SELECT discussion_group_id FROM public.channel WHERE id=$1 FOR UPDATE">>, [Cid])
    of
        #{<<"discussion_group_id">> := null} -> ok;
        _ -> {error, binding_conflict}
    end.

row(Conn, Sql, Params) ->
    case rows(Conn, Sql, Params) of
        [R] -> R;
        _ -> throw({rollback, not_found})
    end.

rows(Conn, Sql, Params) ->
    case elib_pg:query(Conn, Sql, Params) of
        {ok, Rows} when is_list(Rows) -> Rows;
        _ -> throw({rollback, binding_query_failed})
    end.
