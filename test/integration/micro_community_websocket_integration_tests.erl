-module(micro_community_websocket_integration_tests).
-export([run/0]).
-import(
    micro_community_owned_http_fixture,
    [assert/2, rows/3, exec/3, request/5, capture/1, report_outcome/2, committed/2]
).
-define(BASE, 9007199254744000).
-define(OWNER, (?BASE + 1)).
-define(SUB, (?BASE + 3)).
-define(GROUP, (?BASE + 10)).
-define(CHANNEL, (?BASE + 20)).
-define(OFFLINE_ID, <<"mcws-owned-offline-20261008">>).
-define(TIMER_ID, <<"mcws-owned-timer-20261008">>).

%% Explicit opt-in; finite local transport evidence, not the full acceptance set.
run() -> micro_community_owned_http_fixture:run_transport(fun session/1).

session(Ctx = #{conn := Conn}) ->
    assert(ets:info(ack_retry_cache_ets) =:= undefined, ws_retry_table_preexisting),
    assert(not lists:keymember(syn, 1, application:which_applications()), ws_syn_preexisting),
    transport_preflight(Conn),
    prepare_members(Ctx),
    {ok, _} = application:ensure_all_started(gun),
    ok = imboy_syn:init(),
    ok = ack_retry_cache:init_table(),
    {ok, Worker} = user_server:start_link(),
    Outcome = capture(fun() -> connections(Ctx) end),
    Cleanup = capture(fun() ->
        ok = gen_server:stop(Worker),
        cleanup_timer(),
        cleanup_offline(Conn),
        _ = application:stop(syn),
        _ = application:stop(gun),
        ok
    end),
    report_outcome(Outcome, Cleanup).

transport_preflight(Conn) ->
    Users = [?OWNER, ?SUB],
    assert(
        rows(
            Conn,
            <<
                "SELECT count(*) FROM public.user_friend WHERE "
                "from_user_id=ANY($1::bigint[]) OR to_user_id=ANY($1::bigint[])"
            >>,
            [Users]
        ) =:= [{0}],
        ws_friend_collision
    ),
    assert(
        rows(Conn, <<"SELECT count(*) FROM public.push_token WHERE user_id=ANY($1::bigint[])">>, [
            Users
        ]) =:= [{0}],
        ws_push_collision
    ),
    assert(
        rows(
            Conn,
            <<
                "SELECT user_id,device_id FROM public.user_device "
                "WHERE user_id=ANY($1::bigint[]) ORDER BY user_id"
            >>,
            [Users]
        ) =:=
            [{?OWNER, did(?OWNER)}, {?SUB, did(?SUB)}],
        ws_foreign_device
    ),
    assert(
        rows(
            Conn,
            <<"SELECT count(*) FROM public.msg_c2g WHERE msg_id=$1 OR id=$2">>,
            [?OFFLINE_ID, ?BASE + 400]
        ) =:= [{0}],
        ws_message_collision
    ),
    assert(
        rows(
            Conn,
            <<"SELECT count(*) FROM public.msg_c2g_timeline WHERE msg_id=$1">>,
            [?OFFLINE_ID]
        ) =:= [{0}],
        ws_timeline_collision
    ).

prepare_members(Ctx = #{conn := Conn}) ->
    expect_ok(Ctx, ?OWNER, put, discussion(), #{<<"discussion_group_id">> => ?GROUP}),
    expect_ok(Ctx, ?SUB, post, <<(discussion())/binary, "/join">>, #{}),
    assert(
        rows(
            Conn,
            <<
                "SELECT user_id FROM public.group_member WHERE group_id=$1 "
                "AND status=1 ORDER BY user_id"
            >>,
            [?GROUP]
        ) =:= [{?OWNER}, {?SUB}],
        ws_foreign_member
    ).

connections(Ctx) ->
    Owner = open(Ctx, ?OWNER),
    try
        Sub = open(Ctx, ?SUB),
        try
            wait_registered(?OWNER, 100),
            wait_registered(?SUB, 100),
            transport_cases(Ctx, Owner, Sub)
        after
            gun:close(element(1, Sub))
        end
    after
        gun:close(element(1, Owner))
    end.

open(#{port := Port, credentials := Credentials}, Uid) ->
    {ok, Pid} = gun:open("127.0.0.1", Port, #{protocols => [http], retry => 0}),
    try
        {ok, http} = gun:await_up(Pid, 5000),
        Headers = [{list_to_binary(K), list_to_binary(V)} || {K, V} <- maps:get(Uid, Credentials)],
        Ref = gun:ws_upgrade(
            Pid,
            "/api/v1/ws",
            Headers,
            #{protocols => [{<<"imboy-json">>, gun_ws_h}]}
        ),
        receive
            {gun_upgrade, Pid, Ref, [<<"websocket">>], _} -> {Pid, Ref};
            {gun_response, Pid, Ref, _, _, _} -> assert(false, ws_auth_upgrade_rejected)
        after 5000 -> assert(false, ws_upgrade_timeout)
        end
    catch
        _:Reason ->
            gun:close(Pid),
            throw(Reason)
    end.

wait_registered(Uid, Attempts) when Attempts > 0 ->
    case imboy_syn:list_by_uid(Uid) of
        [{Pid, {_, DID}}] when is_pid(Pid) -> assert(DID =:= did(Uid), ws_registration_did);
        _ ->
            timer:sleep(20),
            wait_registered(Uid, Attempts - 1)
    end;
wait_registered(_, 0) ->
    assert(false, ws_registration_timeout).

transport_cases(Ctx = #{conn := Conn}, Owner, Sub) ->
    BeforeId = <<"mcws-owned-before-20261008">>,
    Before = envelope(BeforeId),
    publish_both(Before),
    receive_target(Owner, BeforeId, 3000),
    receive_target(Sub, BeforeId, 3000),
    ok = message_ds:send_next(?OWNER, BeforeId, Before, [0], [did(?OWNER)], true),
    ok = message_ds:send_next(?SUB, BeforeId, Before, [0], [did(?SUB)], true),
    receive_target(Owner, BeforeId, 3000),
    receive_target(Sub, BeforeId, 3000),
    prepare_offline(Conn),
    Snapshot = offline_snapshot(Conn),
    offline_expect(Ctx, 1),
    Timer = envelope(?TIMER_ID),
    ok = message_ds:send_next(?SUB, ?TIMER_ID, Timer, [2000, 1000], [did(?SUB)], true),
    {ok, Ref} = ack_retry_cache:get(timer_key()),
    Remaining = erlang:read_timer(Ref),
    assert(
        is_reference(Ref) andalso is_integer(Remaining) andalso Remaining > 0, ws_timer_not_pending
    ),
    revoke(Conn),
    CommitAt = erlang:monotonic_time(millisecond),
    AtCommit = erlang:read_timer(Ref),
    assert(is_integer(AtCommit) andalso AtCommit > 0, ws_timer_expired_before_revoke),
    io:format(
        "MC_WS_TIMER pending_ref=~p remaining_ms=~p revoke_commit_mono_ms=~p artificial_delay_ms=2000~n",
        [Ref, Remaining, CommitAt]
    ),
    after_revoke(Ctx, Owner, Sub, Timer, Snapshot, Ref),
    io:format("MC_WS_CASE_GROUPS_COMPLETED count=3 default_c2g_delay_ms=0~n"),
    ok.

publish_both(Msg) ->
    {ok, 1} = imboy_syn:publish(?OWNER, Msg),
    {ok, 1} = imboy_syn:publish(?SUB, Msg),
    ok.

revoke(Conn) ->
    committed(Conn, fun(Tx) ->
        exec(
            Tx,
            <<
                "UPDATE public.channel_subscription SET status=0 WHERE "
                "channel_id=$1 AND user_id=$2 AND status=1"
            >>,
            [?CHANNEL, ?SUB]
        ),
        assert(
            rows(
                Tx,
                <<
                    "SELECT status FROM public.channel_subscription WHERE "
                    "channel_id=$1 AND user_id=$2"
                >>,
                [?CHANNEL, ?SUB]
            ) =:= [{0}],
            ws_revoke_not_committed
        ),
        ok
    end).

after_revoke(Ctx = #{conn := Conn}, Owner, Sub, Timer, Snapshot, Ref) ->
    PublishId = <<"mcws-owned-after-publish-20261008">>,
    NextId = <<"mcws-owned-after-next-20261008">>,
    publish_both(envelope(PublishId)),
    receive_target(Owner, PublishId, 3000),
    ok = message_ds:send_next(?OWNER, NextId, envelope(NextId), [0], [did(?OWNER)], true),
    ok = message_ds:send_next(?SUB, NextId, envelope(NextId), [0], [did(?SUB)], true),
    receive_target(Owner, NextId, 3000),
    ok = message_ds:send_next(?OWNER, ?TIMER_ID, Timer, [0], [did(?OWNER)], true),
    receive_target(Owner, ?TIMER_ID, 3000),
    no_targets(Sub, [PublishId, NextId, ?TIMER_ID], 3600),
    assert(erlang:read_timer(Ref) =:= false, ws_timer_not_fired),
    assert(ack_retry_cache:get(timer_key()) =:= undefined, ws_timer_successor_or_residue),
    ping(Sub),
    io:format("MC_WS_CASE id=w01 transport_revoke=ok live_ping=ok~n"),
    io:format("MC_WS_CASE id=w02 real_pending_timeout=ok ref_removed=ok successor_absent=ok~n"),
    offline_expect(Ctx, 0),
    assert(offline_snapshot(Conn) =:= Snapshot, ws_offline_rows_mutated),
    io:format("MC_WS_CASE id=w03 offline_before_total=1 offline_after_total=0 rows_preserved=ok~n"),
    ok.

receive_target(Socket, Id, Timeout) ->
    Deadline = erlang:monotonic_time(millisecond) + Timeout,
    wait_frame(Socket, fun(Frame) -> target(Frame, Id) end, Deadline).

wait_frame({Pid, Ref} = Socket, Predicate, Deadline) ->
    Remaining = erlang:max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {gun_ws, Pid, Ref, Frame} ->
            case Predicate(Frame) of
                true -> ok;
                false -> wait_frame(Socket, Predicate, Deadline)
            end;
        {gun_down, Pid, _, _, _} ->
            assert(false, ws_connection_dropped)
    after Remaining -> assert(false, ws_expected_frame_timeout)
    end.

no_targets({Pid, Ref} = Socket, Ids, Timeout) ->
    no_targets_until(Socket, Ids, erlang:monotonic_time(millisecond) + Timeout, Pid, Ref).

no_targets_until(Socket, Ids, Deadline, Pid, Ref) ->
    Remaining = erlang:max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {gun_ws, Pid, Ref, Frame} ->
            assert(
                not lists:any(fun(Id) -> target(Frame, Id) end, Ids), ws_revoked_target_delivered
            ),
            no_targets_until(Socket, Ids, Deadline, Pid, Ref);
        {gun_down, Pid, _, _, _} ->
            assert(false, ws_connection_dropped)
    after Remaining -> ok
    end.

target({text, Json}, Id) ->
    try jsone:decode(Json, [{object_format, map}]) of
        #{<<"id">> := Id} -> true;
        _ -> false
    catch
        _:_ -> false
    end;
target(_, _) ->
    false.

ping({Pid, Ref} = Socket) ->
    gun:ws_send(Pid, Ref, {text, <<"ping">>}),
    wait_frame(
        Socket,
        fun(Frame) -> Frame =:= {text, <<"pong">>} end,
        erlang:monotonic_time(millisecond) + 3000
    ).

prepare_offline(Conn) ->
    committed(Conn, fun(Tx) ->
        exec(
            Tx,
            <<
                "INSERT INTO public.msg_c2g(id,from_id,to_id,msg_id,msg_type,payload,e2ee,sender_did) "
                "VALUES($1,$2,$3,$4,'text',$5::jsonb,NULL,$6)"
            >>,
            [
                ?BASE + 400,
                ?OWNER,
                ?GROUP,
                ?OFFLINE_ID,
                jsone:encode(#{<<"text">> => <<"synthetic">>}),
                did(?OWNER)
            ]
        ),
        exec(
            Tx,
            <<
                "INSERT INTO public.msg_c2g_timeline(msg_id,to_uid,to_gid,conv_seq,created_at) "
                "SELECT $1,$2,$3,g.start_seq,m.created_at FROM public.group_member_generation g "
                "JOIN public.msg_c2g m ON m.msg_id=$1 WHERE g.group_id=$3 AND g.user_id=$2 AND g.end_seq IS NULL"
            >>,
            [?OFFLINE_ID, ?SUB, ?GROUP]
        ),
        assert(
            rows(Tx, <<"SELECT count(*) FROM public.msg_c2g_timeline WHERE msg_id=$1">>, [
                ?OFFLINE_ID
            ]) =:= [{1}],
            ws_offline_fixture_invalid
        ),
        ok
    end).

offline_expect(Ctx, Total) ->
    Path = <<"/api/v1/msg/offline?did=", (did(?SUB))/binary>>,
    Result = expect_ok(Ctx, ?SUB, get, Path, none),
    C2G = maps:get(<<"c2g">>, maps:get(<<"payload">>, Result)),
    Items = maps:get(<<"list">>, C2G),
    Found = lists:any(
        fun(Item) -> maps:get(<<"msg_id">>, Item, undefined) =:= ?OFFLINE_ID end, Items
    ),
    assert(
        maps:get(<<"total">>, C2G) =:= Total andalso Found =:= (Total =:= 1),
        ws_offline_filter_failed
    ).

offline_snapshot(Conn) ->
    [
        rows(
            Conn,
            <<"SELECT to_jsonb(t)::text FROM ", Table/binary, " t WHERE ", Where/binary,
                " ORDER BY to_jsonb(t)::text">>,
            Params
        )
     || {Table, Where, Params} <-
            [
                {<<"public.msg_c2g">>, <<"msg_id=$1">>, [?OFFLINE_ID]},
                {<<"public.msg_c2g_timeline">>, <<"msg_id=$1">>, [?OFFLINE_ID]},
                {<<"public.group_member">>, <<"group_id=$1 AND user_id=$2">>, [?GROUP, ?SUB]},
                {<<"public.group_member_generation">>, <<"group_id=$1 AND user_id=$2">>, [
                    ?GROUP, ?SUB
                ]}
            ]
    ].

cleanup_offline(Conn) ->
    committed(Conn, fun(Tx) ->
        exec(
            Tx,
            <<"DELETE FROM public.msg_c2g_timeline WHERE msg_id=$1 AND to_gid=$2 AND to_uid=$3">>,
            [?OFFLINE_ID, ?GROUP, ?SUB]
        ),
        exec(Tx, <<"DELETE FROM public.msg_c2g WHERE msg_id=$1 AND id=$2 AND to_id=$3">>, [
            ?OFFLINE_ID, ?BASE + 400, ?GROUP
        ]),
        ok
    end),
    assert(
        rows(Conn, <<"SELECT count(*) FROM public.msg_c2g WHERE msg_id=$1">>, [?OFFLINE_ID]) =:=
            [{0}],
        ws_cleanup_residue
    ).

cleanup_timer() ->
    case ack_retry_cache:get(timer_key()) of
        {ok, Ref} when is_reference(Ref) ->
            erlang:cancel_timer(Ref),
            ack_retry_cache:delete_if_value(timer_key(), Ref);
        _ ->
            ok
    end,
    case persistent_term:get(ack_retry_cache_cleanup_timer, undefined) of
        undefined ->
            ok;
        Ref2 ->
            timer:cancel(Ref2),
            persistent_term:erase(ack_retry_cache_cleanup_timer)
    end,
    ok.

expect_ok(Ctx, Uid, Method, Path, Body) ->
    {Status, Result} = request(Ctx, Uid, Method, Path, Body),
    assert(
        Status =:= 200 andalso maps:get(<<"code">>, Result, invalid) =:= 0, ws_http_request_rejected
    ),
    Result.

envelope(Id) ->
    jsone:encode(
        message_ds:assemble_msg(
            <<"C2G">>,
            integer_to_binary(?OWNER),
            integer_to_binary(?GROUP),
            #{<<"text">> => <<"synthetic">>},
            Id,
            <<"text">>,
            <<>>,
            null
        )
    ).
did(Uid) -> <<"mc-http-", (integer_to_binary(Uid))/binary>>.
discussion() -> <<"/api/v1/channel/", (integer_to_binary(?CHANNEL))/binary, "/discussion">>.
timer_key() -> {?SUB, did(?SUB), ?TIMER_ID}.
