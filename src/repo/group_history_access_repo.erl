-module(group_history_access_repo).
-moduledoc "Current group history generation authorized with fresh discussion entitlement.".
-export([authorize/2]).

-spec authorize(integer(), integer()) -> {ok, map()} | {error, denied}.
authorize(Uid, Gid) ->
    Entitlement = micro_community_access_repo:predicate_sql(<<"grp.id">>, <<"$2">>),
    Sql =
        <<"SELECT gmg.generation_no, gmg.start_seq, ",
            "COALESCE(mss.seq, gmg.start_seq - 1) AS end_seq ",
            "FROM public.group_member_generation gmg ",
            "JOIN public.group_member gm ON gm.group_id = gmg.group_id ",
            " AND gm.user_id = gmg.user_id AND gm.status = 1 ",
            "JOIN public.\"group\" grp ON grp.id = gmg.group_id AND grp.status = 1 ",
            "LEFT JOIN public.msg_store_seq mss ON mss.conv_key = 'c2g:' || gmg.group_id::text ",
            " AND mss.seq >= gmg.start_seq ",
            "WHERE gmg.group_id = $1 AND gmg.user_id = $2 AND gmg.end_seq IS NULL AND ",
            Entitlement/binary>>,
    case elib_pg:query(Sql, [Gid, Uid]) of
        {ok, [
            #{
                <<"generation_no">> := GenNo,
                <<"start_seq">> := StartSeq,
                <<"end_seq">> := EndSeq
            }
        ]} ->
            {ok, #{generation_no => GenNo, start_seq => StartSeq, end_seq => EndSeq}};
        {ok, _} ->
            %% 0 行或多行（多行被部分唯一索引排除，防御性 deny）→ fail-closed
            {error, denied};
        {error, _Reason} ->
            {error, denied}
    end.
