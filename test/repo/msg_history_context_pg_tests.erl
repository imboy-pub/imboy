-module(msg_history_context_pg_tests).
-include_lib("eunit/include/eunit.hrl").

history_context_pg_test_() ->
    {timeout, 180,
        {setup, fun setup/0, fun cleanup/1, fun(_) ->
            {inorder, [
                {Name, {timeout, 15, fun() -> with_fixture(Check) end}}
             || {Name, Check} <- [
                    {"sparse_sequences_and_exact_target", fun sparse_context/1},
                    {"fixed_total_cap", fun fixed_cap/1},
                    {"target_only", fun target_only/1},
                    {"missing_target", fun missing_target/1},
                    {"cross_conversation_target", fun foreign_target/1},
                    {"expired_target", fun expired_target/1},
                    {"expired_neighbors_do_not_extend_scan", fun expired_neighbors/1},
                    {"current_generation_floor", fun generation_floor/1},
                    {"closed_membership", fun closed_membership/1},
                    {"missing_generation", fun missing_generation/1},
                    {"stale_scope_after_rejoin", fun stale_scope/1},
                    {"inactive_group", fun inactive_group/1},
                    {"workspace_entitlement_revoked", fun workspace_revoked/1},
                    {"device_envelope_filter", fun device_filter/1},
                    {"read_does_not_advance_watermarks", fun read_only/1},
                    {"concurrent_rejoin_cannot_reuse_old_generation", fun concurrent_rejoin/1}
                ]
            ]}
        end}}.

setup() ->
    {ok, Conn} = eunit_runner:eunit_setup_with_db(),
    SavedFlag = application:get_env(imboy, msg_archive_enabled),
    try
        ?assertEqual(
            {ok, inttest_marker_db:migration_head(), false},
            erlang_migrate:version(#{conn => Conn, dir => "priv/migrations"})
        ),
        application:set_env(imboy, msg_archive_enabled, true),
        #{conn => Conn, archive_flag => SavedFlag}
    catch
        FailureClass:FailureReason:Stack ->
            eunit_runner:eunit_cleanup_db(Conn),
            erlang:raise(FailureClass, FailureReason, Stack)
    end.

cleanup(#{conn := Conn, archive_flag := SavedFlag}) ->
    case SavedFlag of
        undefined -> application:unset_env(imboy, msg_archive_enabled);
        {ok, Value} -> application:set_env(imboy, msg_archive_enabled, Value)
    end,
    eunit_runner:eunit_cleanup_db(Conn).

with_fixture(Check) ->
    Fixture = #{
        uid => elib_tsid:generate(user),
        peer => elib_tsid:generate(user),
        gid => elib_tsid:generate(group),
        workspace => elib_tsid:generate(workspace)
    },
    try
        seed_scope(Fixture),
        Check(Fixture)
    after
        cleanup_scope(Fixture)
    end.

sparse_context(Fixture) ->
    TargetSeq = 1000000000000,
    seed_many(Fixture, <<"c2c">>, [1, 2, 3, TargetSeq, TargetSeq + 1, TargetSeq + 2, TargetSeq + 3]),
    {ok, Payload} = context(Fixture, <<"c2c">>, message_id(Fixture, <<"c2c">>, TargetSeq), 5),
    ?assertEqual([2, 3, TargetSeq, TargetSeq + 1, TargetSeq + 2], sequences(Payload)),
    ?assertEqual(
        message_id(Fixture, <<"c2c">>, TargetSeq), maps:get(<<"context_msg_id">>, Payload)
    ),
    ?assertNot(maps:is_key(<<"next_seq">>, Payload)),
    ?assertNot(maps:is_key(<<"safe_head">>, Payload)).

fixed_cap(Fixture) ->
    seed_many(Fixture, <<"c2c">>, lists:seq(1, 103)),
    {ok, Payload} = context(Fixture, <<"c2c">>, message_id(Fixture, <<"c2c">>, 52), 1000),
    ?assertEqual(lists:seq(3, 102), sequences(Payload)).

target_only(Fixture) ->
    seed_many(Fixture, <<"c2c">>, [6, 7, 8]),
    {ok, Payload} = context(Fixture, <<"c2c">>, message_id(Fixture, <<"c2c">>, 7), 1),
    ?assertEqual([7], sequences(Payload)).

missing_target(Fixture) ->
    seed_many(Fixture, <<"c2c">>, [6, 8]),
    unavailable(context(Fixture, <<"c2c">>, message_id(Fixture, <<"c2c">>, 7), 50)).

foreign_target(Fixture) ->
    seed_many(Fixture, <<"c2c">>, [6, 8]),
    seed_many(Fixture, <<"c2g">>, [7]),
    unavailable(context(Fixture, <<"c2c">>, message_id(Fixture, <<"c2g">>, 7), 50)).

expired_target(Fixture) ->
    seed(Fixture, <<"c2c">>, 7, -1),
    seed(Fixture, <<"c2c">>, 8, null),
    unavailable(context(Fixture, <<"c2c">>, message_id(Fixture, <<"c2c">>, 7), 50)).

expired_neighbors(Fixture) ->
    seed(Fixture, <<"c2c">>, 1, null),
    seed(Fixture, <<"c2c">>, 2, -1),
    seed(Fixture, <<"c2c">>, 3, null),
    seed(Fixture, <<"c2c">>, 4, 0),
    seed(Fixture, <<"c2c">>, 5, null),
    {ok, Payload} = context(Fixture, <<"c2c">>, message_id(Fixture, <<"c2c">>, 3), 3),
    ?assertEqual([3], sequences(Payload)).

generation_floor(Fixture) ->
    seed_many(Fixture, <<"c2g">>, lists:seq(1, 8)),
    unavailable(context(Fixture, <<"c2g">>, message_id(Fixture, <<"c2g">>, 4), 9)),
    {ok, Payload} = context(Fixture, <<"c2g">>, message_id(Fixture, <<"c2g">>, 5), 9),
    ?assertEqual([5, 6, 7, 8], sequences(Payload)).

closed_membership(Fixture) ->
    seed_many(Fixture, <<"c2g">>, [7]),
    execute(
        <<"UPDATE public.group_member_generation SET end_seq=8 WHERE group_id=$1">>,
        [maps:get(gid, Fixture)]
    ),
    execute(
        <<"UPDATE public.group_member SET status=2 WHERE group_id=$1 AND user_id=$2">>,
        [maps:get(gid, Fixture), maps:get(uid, Fixture)]
    ),
    ?assertMatch(
        {error, _, 403}, context(Fixture, <<"c2g">>, message_id(Fixture, <<"c2g">>, 7), 50)
    ).

missing_generation(Fixture) ->
    seed_many(Fixture, <<"c2g">>, [7]),
    execute(<<"DELETE FROM public.group_member_generation WHERE group_id=$1">>, [
        maps:get(gid, Fixture)
    ]),
    ?assertMatch(
        {error, _, 403}, context(Fixture, <<"c2g">>, message_id(Fixture, <<"c2g">>, 7), 50)
    ).

stale_scope(Fixture) ->
    seed_many(Fixture, <<"c2g">>, lists:seq(1, 8)),
    {ok, _} = elib_pg:with_tx(fun(Conn) ->
        rejoin_tx(Conn, Fixture),
        {ok, rejoined}
    end),
    seed_many(Fixture, <<"c2g">>, lists:seq(9, 12)),
    ?assertEqual({ok, []}, repo_context(Fixture, 7, 5)),
    unavailable(context(Fixture, <<"c2g">>, message_id(Fixture, <<"c2g">>, 7), 50)),
    {ok, Rows} = repo_context(Fixture, 9, 5),
    ?assertEqual([9, 10, 11], [maps:get(<<"conv_seq">>, Row) || Row <- Rows]).

inactive_group(Fixture) ->
    seed_many(Fixture, <<"c2g">>, [7]),
    execute(<<"UPDATE public.\"group\" SET status=0 WHERE id=$1">>, [maps:get(gid, Fixture)]),
    ?assertMatch(
        {error, _, 403}, context(Fixture, <<"c2g">>, message_id(Fixture, <<"c2g">>, 7), 50)
    ).

workspace_revoked(Fixture) ->
    seed_many(Fixture, <<"c2g">>, [7]),
    Workspace = maps:get(workspace, Fixture),
    Uid = maps:get(uid, Fixture),
    execute(
        <<"INSERT INTO public.workspace(id,name,owner_id) VALUES($1,'w11-synthetic',$2)">>,
        [Workspace, Uid]
    ),
    execute(
        <<
            "INSERT INTO public.workspace_member(workspace_id,user_id,role,status) "
            "VALUES($1,$2,'owner','active')"
        >>,
        [Workspace, Uid]
    ),
    execute(
        <<"UPDATE public.\"group\" SET scope='workspace',workspace_id=$2 WHERE id=$1">>,
        [maps:get(gid, Fixture), Workspace]
    ),
    Target = message_id(Fixture, <<"c2g">>, 7),
    ?assertMatch({ok, _}, context(Fixture, <<"c2g">>, Target, 50)),
    execute(
        <<"UPDATE public.group_member SET status=0 WHERE group_id=$1 AND user_id=$2">>,
        [maps:get(gid, Fixture), Uid]
    ),
    execute(
        <<"UPDATE public.workspace_member SET status='removed' WHERE workspace_id=$1 AND user_id=$2">>,
        [Workspace, Uid]
    ),
    ?assertEqual({error, permission_denied}, repo_context(Fixture, 7, 5)),
    ?assertMatch({error, _, 403}, context(Fixture, <<"c2g">>, Target, 50)).

device_filter(Fixture) ->
    seed_many(Fixture, <<"c2c">>, [6, 7, 8]),
    Target = message_id(Fixture, <<"c2c">>, 7),
    Meta = jsone:encode(#{
        <<"fan_out">> => <<"per_device">>,
        <<"devices">> => #{<<"other-device">> => #{<<"ciphertext">> => <<"synthetic-only">>}}
    }),
    execute(<<"UPDATE public.msg_store SET e2ee=$2::jsonb WHERE msg_id=$1">>, [Target, Meta]),
    unavailable(
        messaging_logic:history_context(
            maps:get(uid, Fixture),
            <<"c2c">>,
            integer_to_binary(maps:get(peer, Fixture)),
            Target,
            50,
            <<"device">>
        )
    ),
    ?assertMatch(
        {ok, _},
        messaging_logic:history_context(
            maps:get(uid, Fixture),
            <<"c2c">>,
            integer_to_binary(maps:get(peer, Fixture)),
            Target,
            50,
            <<"other-device">>
        )
    ).

read_only(Fixture) ->
    seed_many(Fixture, <<"c2g">>, [7]),
    execute(
        <<"UPDATE public.msg_store_seq SET seq=100 WHERE conv_key=$1">>,
        [key(Fixture, <<"c2g">>)]
    ),
    Before = read_facts(Fixture),
    ?assertMatch({ok, _}, context(Fixture, <<"c2g">>, message_id(Fixture, <<"c2g">>, 7), 50)),
    ?assertEqual(Before, read_facts(Fixture)).

concurrent_rejoin(Fixture) ->
    seed_many(Fixture, <<"c2g">>, [7, 8]),
    Parent = self(),
    {ok, {Reader, Monitor}} = elib_pg:with_tx(fun(Conn) ->
        {ok, [#{<<"pid">> := WriterPid}]} = elib_pg:query(
            Conn, <<"SELECT pg_backend_pid() AS pid">>, []
        ),
        {ok, [_]} = elib_pg:query(
            Conn,
            <<"SELECT id FROM public.group_member_generation WHERE group_id=$1 FOR UPDATE">>,
            [maps:get(gid, Fixture)]
        ),
        {Child, Ref} = spawn_monitor(fun() ->
            Parent ! {context_reader_result, self(), repo_context(Fixture, 7, 5)}
        end),
        try
            wait_for_blocker(WriterPid, erlang:monotonic_time(millisecond) + 3000),
            rejoin_tx(Conn, Fixture),
            {ok, {Child, Ref}}
        catch
            FailureClass:FailureReason:Stack ->
                exit(Child, kill),
                erlang:demonitor(Ref, [flush]),
                erlang:raise(FailureClass, FailureReason, Stack)
        end
    end),
    try
        receive
            {context_reader_result, Reader, Result} ->
                ?assert(Result =:= {error, permission_denied} orelse Result =:= {ok, []});
            {'DOWN', Monitor, process, Reader, Reason} ->
                error({context_reader_failed, Reason})
        after 5000 -> error(context_reader_timeout)
        end
    after
        exit(Reader, kill),
        erlang:demonitor(Monitor, [flush])
    end.

wait_for_blocker(WriterPid, Deadline) ->
    {ok, [#{<<"blocked">> := Blocked}]} = elib_pg:query(
        <<"SELECT EXISTS(SELECT 1 FROM pg_stat_activity WHERE $1=ANY(pg_blocking_pids(pid))) AS blocked">>,
        [WriterPid]
    ),
    case Blocked of
        true ->
            ok;
        false ->
            case erlang:monotonic_time(millisecond) < Deadline of
                true ->
                    timer:sleep(20),
                    wait_for_blocker(WriterPid, Deadline);
                false ->
                    error(context_reader_did_not_wait_for_generation_lock)
            end
    end.

rejoin_tx(Conn, Fixture) ->
    {ok, _} = elib_pg:execute(
        Conn,
        <<
            "UPDATE public.group_member_generation SET end_seq=8,close_reason='leave' "
            "WHERE group_id=$1 AND user_id=$2 AND end_seq IS NULL"
        >>,
        [maps:get(gid, Fixture), maps:get(uid, Fixture)]
    ),
    {ok, _} = elib_pg:execute(
        Conn,
        <<
            "INSERT INTO public.group_member_generation(group_id,user_id,generation_no,start_seq) "
            "VALUES($1,$2,2,9)"
        >>,
        [maps:get(gid, Fixture), maps:get(uid, Fixture)]
    ),
    ok.

seed_scope(Fixture) ->
    Uid = maps:get(uid, Fixture),
    Gid = maps:get(gid, Fixture),
    lists:foreach(
        fun(UserId) ->
            Account = <<"w11_", (integer_to_binary(UserId))/binary>>,
            execute(
                <<
                    "INSERT INTO public.\"user\"(id,account,nickname,password,reg_ip,reg_cosv) "
                    "VALUES($1,$2,'w11-synthetic','w11-synthetic','127.0.0.1','w11-synthetic')"
                >>,
                [UserId, Account]
            )
        end,
        [Uid, maps:get(peer, Fixture)]
    ),
    execute(
        <<
            "INSERT INTO public.\"group\"(id,owner_uid,creator_uid,member_max,member_count,"
            "introduction,avatar,title,chat_aes_key,status,created_at,e2ee_mode,scope) "
            "VALUES($1,$2,$2,500,1,'','','w11-synthetic','synthetic-only',1,now(),0,'personal')"
        >>,
        [Gid, Uid]
    ),
    execute(
        <<
            "INSERT INTO public.group_member(id,group_id,user_id,role,is_join,join_mode,status,created_at,updated_at) "
            "VALUES($1,$2,$3,1,true,'invite',1,now(),now())"
        >>,
        [elib_tsid:generate(group_member), Gid, Uid]
    ),
    execute(
        <<
            "INSERT INTO public.group_member_generation(group_id,user_id,generation_no,start_seq) "
            "VALUES($1,$2,1,5)"
        >>,
        [Gid, Uid]
    ),
    execute(
        <<"INSERT INTO public.msg_store_seq(conv_key,seq) VALUES($1,4)">>,
        [key(Fixture, <<"c2g">>)]
    ).

seed_many(Fixture, Type, Sequences) ->
    lists:foreach(fun(Seq) -> seed(Fixture, Type, Seq, null) end, Sequences).

seed(Fixture, Type, Seq, ExpiresIn) ->
    {ToId, GroupId} =
        case Type of
            <<"c2c">> -> {maps:get(uid, Fixture), null};
            <<"c2g">> -> {null, maps:get(gid, Fixture)}
        end,
    execute(
        <<
            "INSERT INTO public.msg_store(id,chat_type,conv_key,conv_seq,msg_id,msg_type,from_id,to_id,"
            "group_id,payload,created_at,server_ts,accepted_at,expire_at) "
            "VALUES($1,$2,$3,$4,$5,'e2ee',$6,$7,$8,'{\"ciphertext\":\"synthetic-only\"}',now(),now(),now(),"
            "CASE WHEN $9::integer IS NULL THEN NULL ELSE statement_timestamp()+$9*interval '1 second' END)"
        >>,
        [
            elib_tsid:generate(msg_store),
            Type,
            key(Fixture, Type),
            Seq,
            message_id(Fixture, Type, Seq),
            maps:get(peer, Fixture),
            ToId,
            GroupId,
            ExpiresIn
        ]
    ),
    case Type of
        <<"c2c">> ->
            ok;
        <<"c2g">> ->
            execute(
                <<"UPDATE public.msg_store_seq SET seq=GREATEST(seq,$2) WHERE conv_key=$1">>,
                [key(Fixture, Type), Seq]
            )
    end.

context(Fixture, Type, Target, Limit) ->
    Peer =
        case Type of
            <<"c2c">> -> maps:get(peer, Fixture);
            <<"c2g">> -> maps:get(gid, Fixture)
        end,
    messaging_logic:history_context(
        maps:get(uid, Fixture),
        Type,
        integer_to_binary(Peer),
        Target,
        Limit,
        <<>>
    ).

repo_context(Fixture, TargetSeq, StaleMinSeq) ->
    msg_archive_repo:history_context(
        key(Fixture, <<"c2g">>),
        message_id(Fixture, <<"c2g">>, TargetSeq),
        5,
        <<"c2g">>,
        maps:get(uid, Fixture),
        maps:get(gid, Fixture),
        StaleMinSeq
    ).

key(Fixture, <<"c2c">>) ->
    msg_archive_repo:conv_key(<<"c2c">>, maps:get(uid, Fixture), maps:get(peer, Fixture));
key(Fixture, <<"c2g">>) ->
    msg_archive_repo:conv_key(<<"c2g">>, 0, maps:get(gid, Fixture)).

message_id(Fixture, Type, Seq) ->
    <<"w11_", (integer_to_binary(maps:get(gid, Fixture)))/binary, "_", Type/binary, "_",
        (integer_to_binary(Seq))/binary>>.

sequences(#{<<"messages">> := Messages}) ->
    [maps:get(<<"conv_seq">>, Message) || Message <- Messages].
unavailable(Result) -> ?assertEqual({error, <<"history_context_unavailable">>, 404}, Result).

read_facts(Fixture) ->
    Gid = maps:get(gid, Fixture),
    [
        begin
            {ok, Rows} = elib_pg:query(Sql, Params),
            Rows
        end
     || {Sql, Params} <- [
            {<<"SELECT seq FROM public.msg_store_seq WHERE conv_key=$1">>, [
                key(Fixture, <<"c2g">>)
            ]},
            {<<"SELECT * FROM public.group_member_generation WHERE group_id=$1 ORDER BY generation_no">>,
                [Gid]},
            {
                <<
                    "SELECT * FROM public.workspace_group_read_cursor WHERE generation_id IN "
                    "(SELECT id FROM public.group_member_generation WHERE group_id=$1)"
                >>,
                [Gid]
            },
            {<<"SELECT * FROM public.msg_delivery WHERE to_uid=$1">>, [maps:get(uid, Fixture)]}
        ]
    ].

execute(Sql, Params) ->
    {ok, _} = elib_pg:execute(Sql, Params),
    ok.

cleanup_scope(Fixture) ->
    execute(
        <<"DELETE FROM public.msg_store WHERE conv_key=ANY($1::text[])">>,
        [[key(Fixture, <<"c2c">>), key(Fixture, <<"c2g">>)]]
    ),
    execute(
        <<"DELETE FROM public.msg_store_seq WHERE conv_key=ANY($1::text[])">>,
        [[key(Fixture, <<"c2c">>), key(Fixture, <<"c2g">>)]]
    ),
    execute(<<"DELETE FROM public.group_member_generation WHERE group_id=$1">>, [
        maps:get(gid, Fixture)
    ]),
    execute(<<"DELETE FROM public.group_member WHERE group_id=$1">>, [maps:get(gid, Fixture)]),
    execute(<<"DELETE FROM public.\"group\" WHERE id=$1">>, [maps:get(gid, Fixture)]),
    execute(<<"DELETE FROM public.workspace WHERE id=$1">>, [maps:get(workspace, Fixture)]),
    execute(
        <<"DELETE FROM public.\"user\" WHERE id=ANY($1::bigint[])">>,
        [[maps:get(uid, Fixture), maps:get(peer, Fixture)]]
    ).
