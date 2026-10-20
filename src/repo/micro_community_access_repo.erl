-module(micro_community_access_repo).
-moduledoc "Additional discussion entitlement; existing group ACLs remain required.".
-export([
    predicate_sql/2, predicate_sql/3,
    eligible/2,
    eligible_tx/3,
    filter_uids/2,
    lock_group_tx/2,
    lock_channel_tx/2,
    delivery_allowed/2,
    deactivate_tx/3
]).

%% Expressions are fixed source SQL identifiers or placeholders, never request input.
predicate_sql(GroupExpr, UidExpr) ->
    Enabled =
        try
            imboy_policy:current_profile() =/= phase1_delivery andalso
                imboy_feature:enabled(channel)
        catch
            _:_ -> false
        end,
    predicate_sql(GroupExpr, UidExpr, Enabled).

predicate_sql(GroupExpr, UidExpr, Enabled) ->
    Parent = organization_resource_access_repo:group_predicate_sql(GroupExpr, UidExpr),
    Discussion = discussion_predicate_sql(GroupExpr, UidExpr, Enabled),
    <<"(", Parent/binary, " AND (", Discussion/binary, "))">>.

discussion_predicate_sql(GroupExpr, _UidExpr, false) ->
    <<"NOT EXISTS (SELECT 1 FROM public.channel mc WHERE mc.discussion_group_id = ",
        GroupExpr/binary, ")">>;
discussion_predicate_sql(GroupExpr, UidExpr, true) ->
    Manager = manager_sql(UidExpr),
    <<"(NOT EXISTS (SELECT 1 FROM public.channel mc WHERE mc.discussion_group_id = ",
        GroupExpr/binary, ") OR EXISTS (SELECT 1 FROM public.channel mc ",
        "JOIN public.\"group\" mg ON mg.id = mc.discussion_group_id ",
        "WHERE mc.discussion_group_id = ", GroupExpr/binary,
        " AND mc.status = 1 AND mc.scope = 'personal' AND mg.scope = 'personal'",
        " AND mg.owner_uid = mc.creator_uid AND mg.status = 1 AND mg.type = 2 AND mg.join_limit = 3 AND mg.e2ee_mode = 0",
        " AND (", Manager/binary, " OR (EXISTS (SELECT 1 FROM public.channel_subscription ms ",
        "WHERE ms.channel_id = mc.id AND ms.user_id = ", UidExpr/binary,
        " AND ms.status = 1) AND (mc.access_type <> 1 OR EXISTS (",
        "SELECT 1 FROM public.channel_order mo WHERE mo.channel_id = mc.id ", "AND mo.user_id = ",
        UidExpr/binary, " AND mo.status = 1 ",
        "AND (mo.subscription_end_at IS NULL OR mo.subscription_end_at > clock_timestamp())))))))">>.

manager_sql(UidExpr) ->
    <<"(COALESCE(NULLIF((SELECT ma.role FROM public.channel_admin ma ",
        "WHERE ma.channel_id = mc.id AND ma.user_id = ", UidExpr/binary,
        " LIMIT 1), 0), CASE WHEN mc.creator_uid = ", UidExpr/binary,
        " THEN 3 ELSE 0 END) >= 2 AND (mg.owner_uid = ", UidExpr/binary,
        " OR EXISTS (SELECT 1 FROM public.group_member mm WHERE mm.group_id = mg.id ",
        "AND mm.user_id = ", UidExpr/binary, " AND mm.status = 1 AND mm.role >= 3)))">>.

eligible(Gid, Uid) when is_integer(Gid), Gid > 0, is_integer(Uid), Uid > 0 ->
    Sql = query_sql(),
    case elib_pg:one(Sql, [Gid, Uid]) of
        {ok, #{<<"allowed">> := true}} -> true;
        _ -> false
    end;
eligible(_, _) ->
    false.

eligible_tx(Conn, Gid, Uid) ->
    ok = lock_group_tx(Conn, Gid),
    case elib_pg:query(Conn, query_sql(), [Gid, Uid]) of
        {ok, [#{<<"allowed">> := true}]} -> true;
        _ -> false
    end.

%% The management binding writer uses a conflicting table lock. Take this before
%% looking up a nullable binding so a concurrent bind cannot become a phantom.
lock_group_tx(Conn, Gid) ->
    _ = checked(Conn, <<"LOCK TABLE public.channel IN ROW EXCLUSIVE MODE">>, []),
    case
        checked(Conn, <<"SELECT id FROM public.channel WHERE discussion_group_id=$1 FOR UPDATE">>, [
            Gid
        ])
    of
        [] ->
            ok;
        [_] ->
            _ = checked(Conn, <<"SELECT id FROM public.\"group\" WHERE id=$1 FOR UPDATE">>, [Gid]),
            lock_sequence(Conn, Gid)
    end.

lock_channel_tx(Conn, Cid) ->
    _ = checked(Conn, <<"LOCK TABLE public.channel IN ROW EXCLUSIVE MODE">>, []),
    case
        checked(Conn, <<"SELECT discussion_group_id FROM public.channel WHERE id=$1 FOR UPDATE">>, [
            Cid
        ])
    of
        [#{<<"discussion_group_id">> := Gid}] when is_integer(Gid) ->
            ok = lock_group_tx(Conn, Gid),
            Gid;
        _ ->
            null
    end.

lock_sequence(Conn, Gid) ->
    _ = checked(
        Conn,
        <<
            "INSERT INTO public.msg_store_seq(conv_key,seq) "
            "VALUES('c2g:'||$1::bigint::text,0) ON CONFLICT(conv_key) "
            "DO UPDATE SET seq=public.msg_store_seq.seq RETURNING seq"
        >>,
        [Gid]
    ),
    ok.

checked(Conn, Sql, Params) ->
    case elib_pg:query(Conn, Sql, Params) of
        {ok, Rows} -> Rows;
        {error, Reason} -> throw({abort_tx, {discussion_access_query_failed, Reason}})
    end.

%% Delivery needs current membership in addition to channel entitlement. Unbound
%% traffic retains its group rules, with live parent eligibility for workspace resources.
delivery_allowed(Gid, Uid) ->
    Predicate = predicate_sql(<<"$1">>, <<"$2">>),
    Parent = organization_resource_access_repo:group_predicate_sql(<<"$1">>, <<"$2">>),
    Sql = <<
        "SELECT (",
        Parent/binary,
        " AND (NOT EXISTS(SELECT 1 FROM public.channel WHERE discussion_group_id=$1) "
        "OR ((",
        Predicate/binary,
        ") AND EXISTS(SELECT 1 FROM public.group_member gm "
        "JOIN public.\"group\" g ON g.id=gm.group_id WHERE gm.group_id=$1 "
        "AND gm.user_id=$2 AND gm.status=1 AND g.status=1)))) AS allowed"
    >>,
    case elib_pg:one(Sql, [Gid, Uid]) of
        {ok, #{<<"allowed">> := true}} -> true;
        _ -> false
    end.

deactivate_tx(Conn, Gid, Uid) ->
    _ = checked(
        Conn,
        <<"UPDATE public.group_member SET status=0 WHERE group_id=$1 AND user_id=$2 AND status=1 RETURNING id">>,
        [Gid, Uid]
    ),
    _ = checked(
        Conn,
        <<
            "UPDATE public.\"group\" SET member_count=(SELECT count(*) FROM public.group_member "
            "WHERE group_id=$1 AND status=1) WHERE id=$1 RETURNING id"
        >>,
        [Gid]
    ),
    ok.

query_sql() ->
    %% Both parameters remain typed and bound even when the disabled predicate
    %% intentionally ignores uid. epgsql rejects surplus bind parameters.
    Predicate = predicate_sql(<<"request.gid">>, <<"request.uid">>),
    <<"SELECT (", Predicate/binary,
        ") AS allowed FROM "
        "(SELECT $1::bigint AS gid, $2::bigint AS uid) request">>.

filter_uids(Gid, Uids) ->
    Predicate = predicate_sql(<<"$1">>, <<"mu.uid">>),
    Sql = <<"SELECT mu.uid FROM unnest($2::bigint[]) AS mu(uid) WHERE ", Predicate/binary>>,
    case elib_pg:query(Sql, [Gid, Uids]) of
        {ok, Rows} -> {ok, [Uid || #{<<"uid">> := Uid} <- Rows]};
        {error, _} = Error -> Error
    end.
