-module(msg_forward_generation_pg_tests).
-include_lib("eunit/include/eunit.hrl").

forward_generation_test_() ->
    case os:getenv("C08_FORWARD_PG_DB") of
        false -> [];
        _ -> {timeout, 30, fun forward_generation/0}
    end.

forward_generation() ->
    Db = os:getenv("C08_FORWARD_PG_DB"),
    true = lists:prefix("c08_clean_", Db),
    {ok, _} = application:ensure_all_started(epgsql),
    {ok, Conn} = epgsql:connect(#{
        host => "127.0.0.1",
        port => 49320,
        username => os:getenv("C08_FORWARD_PG_USER"),
        password => os:getenv("C08_FORWARD_PG_PASSWORD"),
        database => Db
    }),
    try
        {ok, _, [{<<"c08-clean-candidate-owned">>}]} = epgsql:squery(
            Conn,
            "SELECT shobj_description(oid,'pg_database') FROM pg_database WHERE datname=current_database()"
        ),
        {ok, [], []} = epgsql:squery(Conn, "BEGIN"),
        {ok, [], []} = epgsql:squery(Conn, "SET LOCAL session_replication_role=replica"),
        run_fixture(Conn)
    after
        epgsql:squery(Conn, "ROLLBACK"),
        epgsql:close(Conn)
    end.

run_fixture(C) ->
    Base = 9000000000000 + binary:decode_unsigned(crypto:strong_rand_bytes(5)),
    Gid = Base + 1,
    Uid = Base + 2,
    Msg = <<"c08-generation-", (integer_to_binary(Base))/binary>>,
    {ok, _} = elib_pg:query(
        C,
        <<"INSERT INTO public.\"group\"(id,owner_uid,creator_uid,scope) VALUES($1,$2,$2,'personal')">>,
        [Gid, Uid]
    ),
    {ok, _} = elib_pg:query(
        C,
        <<"INSERT INTO public.group_member(id,group_id,user_id,role,is_join,status) VALUES($1,$2,$3,1,true,1)">>,
        [Base + 3, Gid, Uid]
    ),
    {ok, _} = elib_pg:query(
        C,
        <<"INSERT INTO public.group_member_generation(group_id,user_id,start_seq) VALUES($1,$2,5)">>,
        [Gid, Uid]
    ),
    {ok, _} = elib_pg:query(
        C,
        <<"INSERT INTO public.msg_c2g(id,from_id,to_id,msg_id,msg_type,payload) VALUES($1,$2,$3,$4,'text','{}'::jsonb)">>,
        [Base + 5, Uid, Gid, Msg]
    ),
    {ok, _} = elib_pg:query(
        C,
        <<"INSERT INTO public.msg_c2g_recipient_snapshot(msg_id,from_id,to_gid,conv_seq,recipient_uids,created_at) VALUES($1,$2,$3,8,ARRAY[$2::bigint],now())">>,
        [Msg, Uid, Gid]
    ),
    ?assert(msg_forward_repo:source_group_visible_tx(C, Msg, Gid, Uid)),
    ?assertEqual({error, forbidden}, forward_attachment_source_repo:c2g_tx(C, Msg, Gid, Uid)),
    {ok, _} = elib_pg:query(
        C,
        <<"INSERT INTO public.c2g_forward_source_fact(msg_id,group_id,sender_id,conv_seq) VALUES($1,$2,$3,8)">>,
        [Msg, Gid, Uid]
    ),
    ?assertEqual(ok, forward_attachment_source_repo:c2g_tx(C, Msg, Gid, Uid)),
    lock_excluded(<<"c2g_forward_revoke:", (integer_to_binary(Gid))/binary, ":", Msg/binary>>),
    {ok, _} = elib_pg:query(
        C,
        <<"INSERT INTO public.c2g_revoke_fact(group_id,original_msg_id,action_msg_id,actor_uid,accepted_seq) VALUES($1,$2,$3,$4,9)">>,
        [Gid, Msg, <<"c08-test-revoke">>, Uid]
    ),
    ?assertNot(msg_forward_repo:source_group_visible_tx(C, Msg, Gid, Uid)),
    ?assertEqual({error, forbidden}, forward_attachment_source_repo:c2g_tx(C, Msg, Gid, Uid)),
    {ok, _} = elib_pg:query(
        C, <<"DELETE FROM public.c2g_revoke_fact WHERE group_id=$1 AND original_msg_id=$2">>, [
            Gid, Msg
        ]
    ),
    {ok, _} = elib_pg:query(
        C,
        <<"UPDATE public.msg_c2g SET expire_at=now()-interval '1 second' WHERE msg_id=$1">>,
        [Msg]
    ),
    ?assertNot(msg_forward_repo:source_group_visible_tx(C, Msg, Gid, Uid)),
    {ok, _} = elib_pg:query(
        C,
        <<"UPDATE public.msg_c2g SET expire_at=NULL WHERE msg_id=$1">>,
        [Msg]
    ),
    {ok, _} = elib_pg:query(
        C,
        <<"UPDATE public.group_member_generation SET start_seq=9 WHERE group_id=$1 AND user_id=$2">>,
        [Gid, Uid]
    ),
    ?assertNot(msg_forward_repo:source_group_visible_tx(C, Msg, Gid, Uid)),
    {ok, _} = elib_pg:query(
        C,
        <<"UPDATE public.group_member_generation SET start_seq=1 WHERE group_id=$1 AND user_id=$2">>,
        [Gid, Uid]
    ),
    {ok, _} = elib_pg:query(
        C,
        <<"UPDATE public.msg_c2g_recipient_snapshot SET recipient_uids=ARRAY[$2::bigint] WHERE msg_id=$1">>,
        [Msg, Base + 4]
    ),
    ?assertNot(msg_forward_repo:source_group_visible_tx(C, Msg, Gid, Uid)),
    c2c_fixture(C, Base, Uid).

c2c_fixture(C, Base, Uid) ->
    Peer = Base + 6,
    Msg = <<"c08-c2c-", (integer_to_binary(Base))/binary>>,
    {ok, _} = elib_pg:query(
        C,
        <<"INSERT INTO public.msg_c2c(id,from_id,to_id,msg_id,msg_type,payload,conv_seq) VALUES($1,$2,$3,$4,'text','{}',1)">>,
        [Base + 7, Uid, Peer, Msg]
    ),
    ?assertEqual({error, forbidden}, forward_attachment_source_repo:c2c_tx(C, Msg, Uid)),
    {ok, _} = elib_pg:query(
        C,
        <<"INSERT INTO public.c2c_forward_source_fact(msg_id,sender_id,recipient_id,conv_seq) VALUES($1,$2,$3,1)">>,
        [Msg, Uid, Peer]
    ),
    ?assertEqual(ok, forward_attachment_source_repo:c2c_tx(C, Msg, Uid)),
    lock_excluded(<<"c2c_forward_revoke:", Msg/binary>>),
    ?assertEqual({error, forbidden}, forward_attachment_source_repo:c2c_tx(C, Msg, Base + 8)),
    {ok, _} = elib_pg:query(
        C,
        <<"INSERT INTO public.c2c_action_receipt(request_id,original_msg_id,actor_uid,peer_uid,action) VALUES($1,$2,$3,$4,'revoke')">>,
        [<<"c08-test-receipt">>, Msg, Uid, Peer]
    ),
    ?assertEqual({error, forbidden}, forward_attachment_source_repo:c2c_tx(C, Msg, Uid)).

lock_excluded(Key) ->
    {ok, Other} = epgsql:connect(#{
        host => "127.0.0.1",
        port => 49320,
        username => os:getenv("C08_FORWARD_PG_USER"),
        password => os:getenv("C08_FORWARD_PG_PASSWORD"),
        database => os:getenv("C08_FORWARD_PG_DB")
    }),
    try
        {ok, [#{<<"acquired">> := false}]} = elib_pg:query(
            Other,
            <<"SELECT pg_try_advisory_xact_lock(hashtextextended($1,0)) AS acquired">>,
            [Key]
        )
    after
        epgsql:close(Other)
    end.
