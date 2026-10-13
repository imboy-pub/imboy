-module(micro_community_android_negative_probe).
-behaviour(gen_server).
-export([start_link/2, session/2, final/1, stop/1, stop/2, cleanup_owned/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-import(micro_community_owned_http_fixture, [assert/2, rows/3, exec/3, committed/2]).
-define(BASE, 9007199254744000).
-define(OWNER, (?BASE + 1)).
-define(SUB, (?BASE + 3)).
-define(CID, (?BASE + 20)).
-define(GID, (?BASE + 10)).

start_link(Ctx, Config) -> gen_server:start_link(?MODULE, {Ctx, Config}, []).
session(Pid, Payload) -> gen_server:call(Pid, {session, Payload}, 90000).
final(Pid) -> gen_server:call(Pid, final, 90000).
stop(Pid) ->
    assert(
        micro_community_android_negative_lifecycle:stop_actor(Pid) =:= ok,
        negative_probe_forced_stop
    ).
stop(Pid, Ctx) ->
    case micro_community_android_negative_lifecycle:stop_owned(Pid, Ctx) of
        ok -> ok;
        {error, Code} -> assert(false, Code)
    end.

cleanup_owned(Ctx) ->
    cleanup(#{ctx => Ctx, ids => ids(maps:get(negative_mode, Ctx)), owner_socket => undefined}).

init({Ctx, Config}) ->
    Mode = maps:get(negative_mode, Ctx),
    Ids = ids(Mode),
    preflight(maps:get(conn, Ctx), Ids),
    {ok, #{
        ctx => Ctx,
        config => Config,
        mode => Mode,
        stage => <<"boot">>,
        sequence => 0,
        ids => Ids,
        deadline => erlang:monotonic_time(millisecond) + 600000,
        completed => [],
        content_snapshot => content_snapshot(maps:get(conn, Ctx)),
        owner_socket => undefined,
        session => undefined,
        failed => false
    }}.

ids(Mode) ->
    Prefix = <<"mc22-", (atom_to_binary(Mode))/binary, "-">>,
    #{
        before_id => <<Prefix/binary, "before">>,
        after_id => <<Prefix/binary, "after">>,
        offline_id => <<Prefix/binary, "offline">>,
        rejected_id => <<Prefix/binary, "reject">>,
        rejected_marker => <<Prefix/binary, "reject">>
    }.

preflight(Conn, Ids) ->
    Id = maps:get(offline_id, Ids),
    assert(
        rows(
            Conn,
            <<"SELECT count(*) FROM public.msg_c2g WHERE msg_id=$1 OR id=$2">>,
            [Id, ?BASE + 450]
        ) =:= [{0}],
        negative_canary_collision
    ),
    assert(
        rows(Conn, <<"SELECT count(*) FROM public.msg_c2g_timeline WHERE msg_id=$1">>, [Id]) =:=
            [{0}],
        negative_timeline_collision
    ),
    lists:foreach(
        fun(Id0) -> canary_collision(Conn, Id0) end,
        [maps:get(before_id, Ids), maps:get(after_id, Ids), Id]
    ),
    ledger_scope_absent(Conn),
    rejected_absent(Conn, maps:get(rejected_marker, Ids)).

canary_collision(Conn, Id) ->
    lists:foreach(
        fun(Table) ->
            assert(
                rows(Conn, <<"SELECT count(*) FROM public.", Table/binary, " WHERE msg_id=$1">>, [
                    Id
                ]) =:= [{0}],
                negative_canary_collision
            )
        end,
        [
            <<"msg_c2g">>,
            <<"msg_store_staging">>,
            <<"msg_c2g_timeline">>,
            <<"msg_c2g_recipient_snapshot">>,
            <<"msg_c2g_request_ledger">>
        ]
    ).

handle_call({session, Payload}, _, #{session := undefined} = State) ->
    {reply, ok, State#{session => Payload}};
handle_call(snapshot, _, State) ->
    {reply, {ok, public(State)}, State};
handle_call(cleanup, _, State) ->
    Result = guarded(fun() -> cleanup(State) end),
    {reply,
        case Result of
            {ok, _} -> ok;
            _ -> {error, cleanup_failed}
        end, State};
handle_call(final, _, State) ->
    Result = guarded(fun() ->
        assert(maps:get(stage, State) =:= <<"verified">>, negative_not_verified),
        final_sql(State)
    end),
    {reply,
        case Result of
            {ok, _} -> ok;
            _ -> {error, negative_final_failed}
        end, State};
handle_call({phase, Action, Id}, _, State) ->
    Valid =
        (Action =:= <<"chat_denied">> orelse Action =:= <<"queued_denied">>) andalso
            is_binary(Id) andalso byte_size(Id) > 0 andalso byte_size(Id) =< 50 andalso
            re:run(Id, <<"^[A-Za-z0-9_-]+$">>, [{capture, none}]) =:= match,
    case
        {Valid, lists:member({phase, Action}, maps:get(completed, State)),
            maps:get(rejected_actual, State, undefined)}
    of
        {true, false, _} -> transition(phase, Action, State#{rejected_actual => Id});
        {true, true, Id} -> {reply, {ok, public(State)}, State};
        _ -> {reply, {error, 400}, State}
    end;
handle_call({phase, <<"chat_denied">>}, _, #{mode := live} = State) ->
    transition(phase, <<"chat_denied">>, State);
handle_call({phase, <<"queued_denied">>}, _, State) ->
    {reply, {error, 400}, State};
handle_call({Role, Action}, _, State) when Role =:= phase; Role =:= control ->
    transition(Role, Action, State);
handle_call(_, _, State) ->
    {reply, {error, 400}, State}.

transition(Role, Action, State) ->
    Completed = maps:get(completed, State),
    case lists:member({Role, Action}, Completed) of
        true -> {reply, {ok, public(State)}, State};
        false -> advance(Role, Action, State)
    end.

advance(Role, Action, State) ->
    Stage = maps:get(stage, State),
    Mode = maps:get(mode, State),
    case {maps:get(failed, State), next(Mode, Stage, Role, Action)} of
        {false, Next} when is_binary(Next) ->
            case
                guarded(fun() ->
                    assert(
                        erlang:monotonic_time(millisecond) < maps:get(deadline, State),
                        negative_timeout
                    ),
                    perform(Action, State)
                end)
            of
                {ok, Changed} ->
                    S = Changed#{
                        stage => Next,
                        sequence => maps:get(sequence, State) + 1,
                        completed => [{Role, Action} | maps:get(completed, State)]
                    },
                    io:format("MC22_PHASE mode=~p stage=~s sequence=~p~n", [
                        Mode, Next, maps:get(sequence, S)
                    ]),
                    {reply, {ok, public(S)}, S};
                {error, Code} ->
                    io:format("MC22_PROBE_FAILED code=~p~n", [Code]),
                    {reply, {error, 500}, State#{failed => true}}
            end;
        _ ->
            {reply, {error, 409}, State}
    end.

next(_, <<"boot">>, phase, <<"joined">>) -> <<"joined">>;
next(_, <<"joined">>, phase, <<"warm">>) -> <<"warm">>;
next(_, <<"warm">>, control, <<"emit_before">>) -> <<"before_emitted">>;
next(_, <<"before_emitted">>, phase, <<"before_received">>) -> <<"before_received">>;
next(live, <<"before_received">>, control, <<"revoke">>) -> <<"revoked">>;
next(live, <<"revoked">>, control, <<"emit_after">>) -> <<"after_emitted">>;
next(live, <<"after_emitted">>, phase, <<"chat_denied">>) -> <<"chat_denied">>;
next(live, <<"chat_denied">>, phase, <<"old_intent_denied">>) -> <<"old_intent_denied">>;
next(live, <<"old_intent_denied">>, phase, <<"cache_denied">>) -> <<"cache_denied">>;
next(live, <<"cache_denied">>, control, <<"finish">>) -> <<"verified">>;
next(reconnect, <<"before_received">>, control, <<"link_drop">>) -> <<"link_drop_requested">>;
next(reconnect, <<"link_drop_requested">>, phase, <<"disconnected">>) -> <<"disconnected">>;
next(reconnect, <<"disconnected">>, control, <<"prepare_before_offline">>) -> <<"offline_before">>;
next(reconnect, <<"offline_before">>, control, <<"revoke">>) -> <<"revoked">>;
next(reconnect, <<"revoked">>, control, <<"prepare_offline">>) -> <<"offline_prepared">>;
next(reconnect, <<"offline_prepared">>, phase, <<"queue_ready">>) -> <<"offline_queued">>;
next(reconnect, <<"offline_queued">>, control, <<"link_restore">>) -> <<"link_restored">>;
next(reconnect, <<"link_restored">>, phase, <<"reconnected">>) -> <<"reconnected">>;
next(reconnect, <<"reconnected">>, phase, <<"old_intent_denied">>) -> <<"old_intent_denied">>;
next(reconnect, <<"old_intent_denied">>, phase, <<"offline_denied">>) -> <<"offline_denied">>;
next(reconnect, <<"offline_denied">>, phase, <<"queued_denied">>) -> <<"queued_denied">>;
next(reconnect, <<"queued_denied">>, control, <<"finish">>) -> <<"verified">>;
next(_, _, _, _) -> invalid.

perform(<<"joined">>, State) ->
    joined_sql(State),
    State;
perform(<<"warm">>, State) ->
    assert(maps:get(session, State) =/= undefined, negative_session_missing),
    State;
perform(<<"emit_before">>, State) ->
    S = ensure_owner(State),
    emit(S, maps:get(before_id, maps:get(ids, S))),
    S;
perform(<<"prepare_before_offline">>, State) ->
    prepare_offline(State),
    offline_total(State, 1),
    State#{offline_snapshot => source_snapshot(State)};
perform(<<"disconnected">>, State) ->
    State#{sub_pids_before_restore => [Pid || {Pid, _} <- imboy_syn:list_by_uid(?SUB)]};
perform(<<"reconnected">>, State) ->
    registered_new(State, 100),
    emit(State, maps:get(after_id, maps:get(ids, State))),
    State;
perform(<<"revoke">>, State) ->
    {200, #{<<"code">> := 0}} = http(
        State, post, <<"/api/v1/channel/", (integer_to_binary(?CID))/binary, "/unsubscribe">>, #{}
    ),
    revoked_sql(State),
    [{At}] = rows(conn(State), <<"SELECT clock_timestamp()::text">>, []),
    io:format("MC22_REVOKE_COMMIT_OBSERVED mono_ms=~p db_observed_at=~s~n", [
        erlang:monotonic_time(millisecond), At
    ]),
    State;
perform(<<"link_drop_requested">>, State) ->
    %% 断链物证：当前 adb 的 reverse 移除不杀既有 TCP（r6/r7 实证：app
    %% 协议级 ping 对黑洞连接写不失败、无 pong 超时，永远等不到
    %% disconnected）。物理等效断开由服务端执行：关闭该用户全部 WS 会话
    %% 进程（生产语义：服务端踢除死链），TCP 关闭 → app onClosed →
    %% disconnected。快照由 disconnected 分支重查询（此时为空，
    %% registered_new 的"新会话"对拍天然成立）。
    Pids = [Pid || {Pid, _} <- imboy_syn:list_by_uid(?SUB)],
    lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids),
    io:format("MC22_LINK_DROP_KILLED pids=~p~n", [length(Pids)]),
    State;
perform(Action, State) ->
    perform_later(Action, State).

perform_later(<<"emit_after">>, State) ->
    emit(State, maps:get(after_id, maps:get(ids, State))),
    State;
perform_later(<<"prepare_offline">>, State) ->
    assert(
        source_snapshot(State) =:= maps:get(offline_snapshot, State),
        negative_offline_source_changed
    ),
    emit(State, maps:get(after_id, maps:get(ids, State))),
    State;
perform_later(<<"offline_denied">>, State) ->
    offline_total(State, 0),
    State;
perform_later(<<"finish">>, State) ->
    final_sql(State),
    State;
perform_later(Action, State) when
    Action =:= <<"chat_denied">>;
    Action =:= <<"old_intent_denied">>;
    Action =:= <<"cache_denied">>;
    Action =:= <<"queued_denied">>
->
    denied_http(State),
    rejected_absent(conn(State), maps:get(rejected_marker, maps:get(ids, State))),
    case Action of
        <<"chat_denied">> -> actual_absent(State);
        <<"queued_denied">> -> actual_absent(State);
        _ -> ok
    end,
    State;
perform_later(_, State) ->
    State.

public(State) ->
    (maps:get(ids, State))#{
        mode => maps:get(mode, State),
        stage => maps:get(stage, State),
        sequence => maps:get(sequence, State),
        deadline_remaining_ms => erlang:max(
            0, maps:get(deadline, State) - erlang:monotonic_time(millisecond)
        )
    }.
conn(State) -> maps:get(conn, maps:get(ctx, State)).

guarded(Fun) ->
    try
        {ok, Fun()}
    catch
        throw:{mc_http_failed, Code} when is_atom(Code) -> {error, Code};
        _:_ -> {error, negative_fixed_failure}
    end.

joined_sql(State) ->
    Conn = conn(State),
    assert(
        rows(
            Conn,
            <<"SELECT status FROM public.channel_subscription WHERE channel_id=$1 AND user_id=$2">>,
            [?CID, ?SUB]
        ) =:= [{1}],
        negative_not_subscribed
    ),
    assert(
        rows(Conn, <<"SELECT status FROM public.group_member WHERE group_id=$1 AND user_id=$2">>, [
            ?GID, ?SUB
        ]) =:= [{1}],
        negative_not_joined
    ),
    assert(
        rows(
            Conn,
            <<"SELECT generation_no,end_seq FROM public.group_member_generation WHERE group_id=$1 AND user_id=$2">>,
            [?GID, ?SUB]
        ) =:= [{1, null}],
        negative_wrong_generation
    ).

revoked_sql(State) ->
    Conn = conn(State),
    assert(
        rows(
            Conn,
            <<"SELECT status FROM public.channel_subscription WHERE channel_id=$1 AND user_id=$2">>,
            [?CID, ?SUB]
        ) =:= [{0}],
        negative_subscription_active
    ),
    assert(
        rows(Conn, <<"SELECT status FROM public.group_member WHERE group_id=$1 AND user_id=$2">>, [
            ?GID, ?SUB
        ]) =:= [{0}],
        negative_member_active
    ),
    assert(
        rows(
            Conn,
            <<"SELECT generation_no,end_seq IS NOT NULL,close_reason FROM public.group_member_generation WHERE group_id=$1 AND user_id=$2">>,
            [?GID, ?SUB]
        ) =:= [{1, true, <<"channel_entitlement_revoked">>}],
        negative_generation_open
    ).

final_sql(State) ->
    revoked_sql(State),
    denied_http(State),
    rejected_absent(conn(State), maps:get(rejected_marker, maps:get(ids, State))),
    assert(
        rows(
            conn(State),
            <<"SELECT discussion_group_id,visibility,access_type FROM public.channel WHERE id=$1">>,
            [?CID]
        ) =:= [{?GID, 0, 0}],
        negative_binding_changed
    ),
    assert(
        rows(
            conn(State),
            <<"SELECT status FROM public.group_member WHERE group_id=$1 AND user_id=$2">>,
            [?GID, ?OWNER]
        ) =:= [{1}],
        negative_owner_inactive
    ),
    assert(
        content_snapshot(conn(State)) =:= maps:get(content_snapshot, State),
        negative_content_changed
    ),
    assert(
        rows(
            conn(State),
            <<"SELECT member_count=(SELECT count(*) FROM public.group_member WHERE group_id=$1 AND status=1) FROM public.\"group\" WHERE id=$1">>,
            [?GID]
        ) =:= [{true}],
        negative_member_count_mismatch
    ),
    ledger_scope_absent(conn(State)),
    actual_absent(State),
    case maps:get(mode, State) of
        reconnect ->
            offline_total(State, 0),
            assert(
                source_snapshot(State) =:= maps:get(offline_snapshot, State),
                negative_offline_source_changed
            );
        live ->
            ok
    end,
    io:format("MC22_FINAL_SQL mode=~p revoked=1 rejected_source_rows=0~n", [maps:get(mode, State)]),
    ok.

rejected_absent(Conn, Marker) ->
    assert(
        rows(
            Conn,
            <<"SELECT count(*) FROM public.msg_c2g WHERE from_id=$1 AND to_id=$2 AND strpos(payload::text,$3)>0">>,
            [?SUB, ?GID, Marker]
        ) =:= [{0}],
        negative_rejected_main_present
    ),
    assert(
        rows(
            Conn,
            <<"SELECT count(*) FROM public.msg_store_staging WHERE from_id::text=$1 AND to_id::text=$2 AND strpos(payload::text,$3)>0">>,
            [integer_to_binary(?SUB), integer_to_binary(?GID), Marker]
        ) =:= [{0}],
        negative_rejected_staging_present
    ).

content_snapshot(Conn) ->
    rows(
        Conn,
        <<"SELECT content,payload::text,is_essence,is_pinned FROM public.channel_message WHERE id=$1">>,
        [?BASE + 30]
    ).

actual_absent(State) ->
    actual_absent_id(State, maps:get(rejected_actual, State, undefined)).

actual_absent_id(#{mode := live}, undefined) ->
    ok;
actual_absent_id(State, Id) ->
    assert(is_binary(Id), negative_actual_id_missing),
    Conn = conn(State),
    ledger_id_absent(Conn, Id),
    assert(
        rows(
            Conn,
            <<"SELECT count(*) FROM public.msg_c2g WHERE msg_id=$1 AND from_id=$2 AND to_id=$3">>,
            [Id, ?SUB, ?GID]
        ) =:= [{0}],
        negative_actual_main_present
    ),
    assert(
        rows(
            Conn,
            <<"SELECT count(*) FROM public.msg_store_staging WHERE msg_id=$1 AND from_id=$2 AND to_id=$3">>,
            [Id, ?SUB, ?GID]
        ) =:= [{0}],
        negative_actual_staging_present
    ),
    assert(
        rows(
            Conn,
            <<"SELECT count(*) FROM public.msg_c2g_timeline WHERE msg_id=$1 AND to_gid=$2">>,
            [Id, ?GID]
        ) =:= [{0}],
        negative_actual_timeline_present
    ),
    assert(
        rows(
            Conn,
            <<"SELECT count(*) FROM public.msg_c2g_recipient_snapshot WHERE msg_id=$1 AND from_id=$2 AND to_gid=$3">>,
            [Id, ?SUB, ?GID]
        ) =:= [{0}],
        negative_actual_recipient_present
    ).

ledger_id_absent(Conn, Id) ->
    assert(
        rows(
            Conn,
            <<"SELECT count(*) FROM public.msg_c2g_request_ledger WHERE msg_id=$1 AND from_id=$2 AND to_gid=$3">>,
            [Id, ?SUB, ?GID]
        ) =:= [{0}],
        negative_actual_ledger_present
    ).

ledger_scope_absent(Conn) ->
    assert(
        rows(
            Conn,
            <<"SELECT count(*) FROM public.msg_c2g_request_ledger WHERE from_id=$1 AND to_gid=$2">>,
            [?SUB, ?GID]
        ) =:= [{0}],
        negative_ledger_scope_present
    ).

http(State, Method, Path, Body) ->
    Session = maps:get(session, State),
    Config = maps:get(config, State),
    Did = maps:get(did, Session),
    Key = app_version_ds:sign_key(<<"android">>, <<"1">>, maps:get(package, Config)),
    Sign = elib_hasher:hmac_sha256(
        <<Did/binary, "|1|android|", (maps:get(package, Config))/binary>>, Key
    ),
    Headers = [
        {"authorization", binary_to_list(<<"Bearer ", (maps:get(token, Session))/binary>>)},
        {"did", binary_to_list(Did)},
        {"cos", "android"},
        {"vsn", "1"},
        {"sk", "1"},
        {"pkg", binary_to_list(maps:get(package, Config))},
        {"method", "sha256"},
        {"sign", binary_to_list(Sign)}
    ],
    Url =
        "http://127.0.0.1:" ++ integer_to_list(maps:get(port, maps:get(ctx, State))) ++
            binary_to_list(Path),
    Req =
        case Method of
            get -> {Url, Headers};
            post -> {Url, Headers, "application/json", jsone:encode(Body)}
        end,
    case
        httpc:request(Method, Req, [{timeout, 15000}, {autoredirect, false}], [
            {body_format, binary}
        ])
    of
        {ok, {{_, Status, _}, _, Json}} -> {Status, jsone:decode(Json, [{object_format, map}])};
        _ -> assert(false, negative_http_unavailable)
    end.

denied_http(State) ->
    Before = membership_snapshot(State),
    Gid = integer_to_binary(?GID),
    Cid = integer_to_binary(?BASE + 20),
    Did = maps:get(did, maps:get(session, State)),
    denied_request(State, get, <<"/api/v1/group/detail?gid=", Gid/binary>>, none, 403),
    denied_request(State, post, <<"/api/v1/channel/", Cid/binary, "/discussion/join">>, #{}, 403),
    denied_request(
        State,
        get,
        <<"/api/v1/msg/history?chat_type=c2g&peer_id=", Gid/binary, "&did=", Did/binary>>,
        none,
        403
    ),
    denied_request(State, get, <<"/api/v1/group_member/page?gid=", Gid/binary>>, none, 1),
    assert(membership_snapshot(State) =:= Before, negative_denial_changed_membership).

denied_request(State, Method, Path, Body, Expected) ->
    {Status, Result} = http(State, Method, Path, Body),
    Code = maps:get(<<"code">>, Result, invalid),
    assert(Status =:= 200 andalso Code =:= Expected, negative_access_not_denied).

membership_snapshot(State) ->
    [
        rows(
            conn(State),
            <<"SELECT to_jsonb(t)::text FROM public.", Table/binary,
                " t WHERE group_id=$1 AND user_id=$2 ORDER BY to_jsonb(t)::text">>,
            [?GID, ?SUB]
        )
     || Table <- [<<"group_member">>, <<"group_member_generation">>]
    ].

offline_total(State, Expected) ->
    {200, #{<<"code">> := 0, <<"payload">> := Payload}} = http(
        State,
        get,
        <<"/api/v1/msg/offline?did=", (maps:get(did, maps:get(session, State)))/binary>>,
        none
    ),
    C2G = maps:get(<<"c2g">>, Payload),
    Items = maps:get(<<"list">>, C2G),
    Id = maps:get(offline_id, maps:get(ids, State)),
    Found = lists:any(fun(I) -> maps:get(<<"msg_id">>, I, undefined) =:= Id end, Items),
    assert(
        maps:get(<<"total">>, C2G) =:= Expected andalso Found =:= (Expected =:= 1),
        negative_offline_filter_failed
    ).

prepare_offline(State) ->
    Id = maps:get(offline_id, maps:get(ids, State)),
    Did = <<"mc-http-", (integer_to_binary(?OWNER))/binary>>,
    committed(conn(State), fun(Tx) ->
        exec(
            Tx,
            <<"INSERT INTO public.msg_c2g(id,from_id,to_id,msg_id,msg_type,payload,e2ee,sender_did) VALUES($1,$2,$3,$4,'text',$5::jsonb,NULL,$6)">>,
            [?BASE + 450, ?OWNER, ?GID, Id, jsone:encode(#{<<"text">> => Id}), Did]
        ),
        exec(
            Tx,
            <<"INSERT INTO public.msg_c2g_timeline(msg_id,to_uid,to_gid,conv_seq,created_at) SELECT $1,$2,$3,g.start_seq,m.created_at FROM public.group_member_generation g JOIN public.msg_c2g m ON m.msg_id=$1 WHERE g.group_id=$3 AND g.user_id=$2 AND g.end_seq IS NULL">>,
            [Id, ?SUB, ?GID]
        ),
        assert(
            rows(Tx, <<"SELECT count(*) FROM public.msg_c2g_timeline WHERE msg_id=$1">>, [Id]) =:=
                [{1}],
            negative_offline_fixture_failed
        ),
        ok
    end).

source_snapshot(State) ->
    Id = maps:get(offline_id, maps:get(ids, State)),
    [
        rows(
            conn(State),
            <<"SELECT id,from_id,to_id,msg_id,payload::text,created_at::text FROM public.msg_c2g WHERE msg_id=$1">>,
            [Id]
        ),
        rows(
            conn(State),
            <<"SELECT msg_id,to_uid,to_gid,conv_seq,created_at::text FROM public.msg_c2g_timeline WHERE msg_id=$1">>,
            [Id]
        )
    ].

ensure_owner(#{owner_socket := undefined} = State) ->
    {ok, _} = application:ensure_all_started(gun),
    {ok, Pid} = gun:open("127.0.0.1", maps:get(port, maps:get(ctx, State)), #{
        protocols => [http], retry => 0
    }),
    put(negative_owner_pid, Pid),
    {ok, http} = gun:await_up(Pid, 5000),
    Headers = [
        {list_to_binary(K), list_to_binary(V)}
     || {K, V} <- maps:get(?OWNER, maps:get(credentials, maps:get(ctx, State)))
    ],
    Ref = gun:ws_upgrade(Pid, "/api/v1/ws", Headers, #{protocols => [{<<"imboy-json">>, gun_ws_h}]}),
    receive
        {gun_upgrade, Pid, Ref, [<<"websocket">>], _} -> ok
    after 5000 -> assert(false, negative_owner_upgrade_failed)
    end,
    registered(?OWNER, 100),
    State#{owner_socket => {Pid, Ref}};
ensure_owner(State) ->
    State.

registered(Uid, N) when N > 0 ->
    case imboy_syn:list_by_uid(Uid) of
        [_] ->
            ok;
        _ ->
            timer:sleep(20),
            registered(Uid, N - 1)
    end;
registered(_, 0) ->
    assert(false, negative_owner_registration_failed).

registered_new(State, N) when N > 0 ->
    Old = maps:get(sub_pids_before_restore, State, []),
    Did = maps:get(did, maps:get(session, State)),
    New = [
        Pid
     || {Pid, {_, D}} <- imboy_syn:list_by_uid(?SUB), D =:= Did, not lists:member(Pid, Old)
    ],
    case New of
        [] ->
            timer:sleep(20),
            registered_new(State, N - 1);
        _ ->
            ok
    end;
registered_new(_, 0) ->
    assert(false, negative_new_server_session_missing).

emit(State, Id) ->
    Msg = jsone:encode(
        message_ds:assemble_msg(
            <<"C2G">>,
            integer_to_binary(?OWNER),
            integer_to_binary(?GID),
            #{<<"text">> => Id},
            Id,
            <<"text">>,
            <<>>,
            null
        )
    ),
    {ok, 1} = imboy_syn:publish(?OWNER, Msg),
    _ = imboy_syn:publish(?SUB, Msg),
    owner_receive(State, Id, erlang:monotonic_time(millisecond) + 5000),
    ok = message_ds:send_next(?OWNER, Id, Msg, [0], [], true),
    ok = message_ds:send_next(?SUB, Id, Msg, [0], [], true),
    owner_receive(State, Id, erlang:monotonic_time(millisecond) + 5000),
    io:format("MC22_OWNER_POSITIVE id=~s delivery_paths=2~n", [Id]),
    ok.

owner_receive(State, Id, Deadline) ->
    {Pid, Ref} = maps:get(owner_socket, State),
    Remaining = erlang:max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {gun_ws, Pid, Ref, {text, Json}} ->
            case jsone:decode(Json, [{object_format, map}]) of
                #{<<"id">> := Id} -> ok;
                _ -> owner_receive(State, Id, Deadline)
            end
    after Remaining -> assert(false, negative_owner_positive_missing)
    end.

cleanup(State) ->
    case erase(negative_owner_pid) of
        Pid0 when is_pid(Pid0) -> gun:close(Pid0);
        _ -> ok
    end,
    case maps:get(owner_socket, State) of
        {Pid, _} -> gun:close(Pid);
        _ -> ok
    end,
    cleanup_rejected(State),
    cleanup_canaries(State),
    Id = maps:get(offline_id, maps:get(ids, State)),
    committed(conn(State), fun(Tx) ->
        exec(
            Tx,
            <<"DELETE FROM public.msg_c2g_timeline WHERE msg_id=$1 AND to_uid=$2 AND to_gid=$3">>,
            [Id, ?SUB, ?GID]
        ),
        exec(Tx, <<"DELETE FROM public.msg_c2g WHERE msg_id=$1 AND id=$2 AND to_id=$3">>, [
            Id, ?BASE + 450, ?GID
        ]),
        ok
    end),
    assert(
        rows(conn(State), <<"SELECT count(*) FROM public.msg_c2g WHERE msg_id=$1">>, [Id]) =:=
            [{0}],
        negative_cleanup_residue
    ),
    ok.

cleanup_rejected(State) ->
    Conn = conn(State),
    Marker = maps:get(rejected_marker, maps:get(ids, State)),
    Found = rows(
        Conn,
        <<"SELECT msg_id FROM public.msg_c2g WHERE from_id=$1 AND to_id=$2 AND strpos(payload::text,$3)>0 UNION SELECT msg_id FROM public.msg_store_staging WHERE from_id=$1 AND to_id=$2 AND strpos(payload::text,$3)>0 UNION SELECT msg_id FROM public.msg_c2g_request_ledger WHERE from_id=$1 AND to_gid=$2">>,
        [?SUB, ?GID, Marker]
    ),
    Actual =
        case maps:get(rejected_actual, State, undefined) of
            undefined -> [];
            Id -> [Id]
        end,
    Ids = lists:usort(Actual ++ [Id || {Id} <- Found]),
    committed(Conn, fun(Tx) ->
        lists:foreach(fun(Id) -> cleanup_rejected_id(Tx, Id) end, Ids),
        ok
    end),
    rejected_absent(Conn, Marker),
    ledger_scope_absent(Conn),
    lists:foreach(fun(Id) -> actual_absent(State#{rejected_actual => Id}) end, Ids).

cleanup_rejected_id(Tx, Id) ->
    cleanup_source_id(Tx, Id, ?SUB).

cleanup_source_id(Tx, Id, From) ->
    exec(
        Tx,
        <<"DELETE FROM public.msg_c2g_request_ledger WHERE msg_id=$1 AND from_id=$2 AND to_gid=$3">>,
        [Id, From, ?GID]
    ),
    exec(
        Tx,
        <<"DELETE FROM public.msg_c2g_timeline WHERE msg_id=$1 AND to_gid=$2">>,
        [Id, ?GID]
    ),
    exec(
        Tx,
        <<"DELETE FROM public.msg_c2g_recipient_snapshot WHERE msg_id=$1 AND from_id=$2 AND to_gid=$3">>,
        [Id, From, ?GID]
    ),
    exec(
        Tx,
        <<"DELETE FROM public.msg_store_staging WHERE msg_id=$1 AND from_id=$2 AND to_id=$3">>,
        [Id, From, ?GID]
    ),
    exec(
        Tx,
        <<"DELETE FROM public.msg_c2g WHERE msg_id=$1 AND from_id=$2 AND to_id=$3">>,
        [Id, From, ?GID]
    ).

cleanup_canaries(State) ->
    Ids = maps:get(ids, State),
    CanaryIds = [maps:get(K, Ids) || K <- [before_id, after_id, offline_id]],
    committed(conn(State), fun(Tx) ->
        lists:foreach(fun(Id) -> cleanup_source_id(Tx, Id, ?OWNER) end, CanaryIds),
        ok
    end),
    lists:foreach(fun(Id) -> canary_collision(conn(State), Id) end, CanaryIds).

handle_cast(_, State) -> {noreply, State}.
handle_info(_, State) -> {noreply, State}.
terminate(_, _) -> ok.
code_change(_, State, _) -> {ok, State}.
