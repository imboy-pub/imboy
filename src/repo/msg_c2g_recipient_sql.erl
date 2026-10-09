-module(msg_c2g_recipient_sql).
-moduledoc "Recipient snapshot SQL constructed with current discussion policy on every call.".
-export([recipients/0, action/0]).

-spec recipients() -> binary().
recipients() ->
    CallerAccess = micro_community_access_repo:predicate_sql(<<"grp.id">>, <<"caller.user_id">>),
    RecipientAccess = micro_community_access_repo:predicate_sql(
        <<"grp.id">>, <<"recipient.user_id">>
    ),
    <<"SELECT recipient.user_id FROM public.\"group\" grp ",
        "JOIN public.group_member caller ON caller.group_id = grp.id ",
        "AND caller.user_id = $2 AND caller.status = 1 AND caller.role >= $3 ",
        "JOIN public.group_member recipient ON recipient.group_id = grp.id ",
        "AND recipient.status = 1 ", "WHERE grp.id = $1 AND grp.status = 1 AND ",
        CallerAccess/binary, " AND ", RecipientAccess/binary, " LIMIT $4">>.

-spec action() -> binary().
action() ->
    CallerAccess = micro_community_access_repo:predicate_sql(<<"grp.id">>, <<"caller.user_id">>),
    RecipientAccess = micro_community_access_repo:predicate_sql(
        <<"grp.id">>, <<"recipient.user_id">>
    ),
    <<"SELECT recipient.user_id FROM public.\"group\" grp",
        " JOIN public.group_member caller ON caller.group_id = grp.id",
        " AND caller.user_id = $2 AND caller.status = 1 AND caller.role >= $3",
        " JOIN public.group_member_generation caller_gen",
        " ON caller_gen.group_id = caller.group_id AND caller_gen.user_id = caller.user_id",
        " AND caller_gen.end_seq IS NULL", " JOIN public.msg_c2g_recipient_snapshot target",
        " ON target.to_gid = grp.id AND target.from_id = caller.user_id", " AND target.msg_id = $4",
        " AND target.conv_seq >= caller_gen.start_seq",
        " CROSS JOIN LATERAL unnest(target.recipient_uids) original_recipient(user_id)",
        " JOIN public.group_member recipient ON recipient.group_id = grp.id",
        " AND recipient.user_id = original_recipient.user_id AND recipient.status = 1",
        " JOIN public.group_member_generation recipient_gen",
        " ON recipient_gen.group_id = recipient.group_id",
        " AND recipient_gen.user_id = recipient.user_id AND recipient_gen.end_seq IS NULL",
        " AND target.conv_seq >= recipient_gen.start_seq",
        " WHERE grp.id = $1 AND grp.status = 1 AND ", CallerAccess/binary, " AND ",
        RecipientAccess/binary, " LIMIT $5">>.
