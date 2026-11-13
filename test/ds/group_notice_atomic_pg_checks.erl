%% Run only against a disposable database in a fresh VM, with no imboy application.
%% Core ownership/member/audit/staging/formal SQL and the existing worker stay real.
-module(group_notice_atomic_pg_checks).
-export([run/1]).
-include_lib("eunit/include/eunit.hrl").

run(ConnOpts0) ->
    ConnOpts = ConnOpts0#{codecs => [{epgsql_codec_rfc3339_bin, []}]},
    assert_no_app(),
    undefined = whereis(msg_store_worker),
    {ok, _} = application:ensure_all_started(pooler),
    ok = application:set_env(imboy, sql_driver, pgsql),
    ok = application:set_env(imboy, msg_archive_enabled, false),
    ok = application:set_env(imboy, msg_store_worker_tick_ms, 0),
    ok = elib_tsid:init(#{dc_id => 0, node_id => 1}),
    {ok, Verify} = epgsql:connect(ConnOpts),
    {ok, _} = pooler:new_pool(#{
        name => pgsql,
        init_count => 2,
        max_count => 2,
        queue_max => 10,
        start_mfa => {epgsql, connect, [ConnOpts]}
    }),
    try
        create_schema(Verify),
        mock_external_boundaries(Verify),
        leave_staging_failure_rolls_back(Verify),
        [transfer_staging_failure_rolls_back(Verify, Mode) || Mode <- modes()],
        [transfer_member_disappears(Verify, Mode, Uid) || Mode <- modes(), Uid <- [2, 4]],
        [transfer_commits_consistent_notice(Verify, Mode) || Mode <- modes()],
        actor_crash_and_rejoin(Verify),
        drain_to_formal_and_replay(Verify),
        default_tick_recovers_lost_kick(Verify),
        assert_no_app(),
        io:format("GROUP_NOTICE_ATOMIC_PG_CHECKS=PASS~n"),
        ok
    after
        stop_worker(),
        [unload(M) || M <- boundary_modules()],
        pooler:rm_pool(pgsql),
        epgsql:close(Verify)
    end.

modes() -> [standard, keep_admin, drop_admin].

boundary_modules() ->
    [
        group_ds,
        imboy_syn,
        micro_community_access_repo,
        micro_community_access_ds,
        micro_community_binding_repo,
        workspace_guard
    ].

mock_external_boundaries(Verify) ->
    [ok = meck:new(M, [passthrough, no_link]) || M <- boundary_modules()],
    %% Cache invalidations and online presence are external to this SQL contract.
    meck:expect(group_ds, leave, fun(_, _) -> ok end),
    meck:expect(group_ds, join, fun(_, _) -> ok end),
    meck:expect(imboy_syn, list_by_uid, fun(_) -> [] end),
    %% Isolate workspace/discussion ACLs only. The group lock SQL remains real.
    meck:expect(micro_community_access_repo, predicate_sql, fun(_, _) -> <<"TRUE">> end),
    meck:expect(micro_community_access_ds, eligible_tx, fun(_, _, _) -> true end),
    meck:expect(micro_community_access_ds, delivery_allowed, fun(_, _) -> true end),
    meck:expect(micro_community_binding_repo, guard_group_tx, fun(_, _) -> ok end),
    meck:expect(workspace_guard, ensure_writable_tx, fun(_, {group, 10}) ->
        %% Controlled interleaving: preflight has returned active, then another
        %% connection deactivates the member before the real role UPDATE.
        case erase({?MODULE, deactivate}) of
            undefined ->
                ok;
            Uid ->
                sql(Verify, "UPDATE public.group_member SET status=0 WHERE user_id=$1", [Uid]),
                ok
        end
    end).

leave_staging_failure_rolls_back(C) ->
    seed(C),
    reject_action(C, "group_member_leave"),
    staging_probe_install(),
    try
        %% Existing leave API is fire-and-forget; the database is the rollback oracle.
        ok = group_member_logic:leave(4, 10, 4),
        assert_initial_state(C),
        staging_probe_assert_check_violation(),
        ?assertEqual(
            1,
            scalar(
                C,
                "SELECT count(*) FROM group_member_generation WHERE user_id=4 AND end_seq IS NULL"
            )
        )
    after
        allow_actions(C),
        staging_probe_unload()
    end,
    io:format("GROUP_NOTICE_PG_LEAVE_STAGING_ROLLBACK=PASS~n").

transfer_staging_failure_rolls_back(C, Mode) ->
    seed(C),
    reject_action(C, "group_transfer"),
    staging_probe_install(),
    try
        ?assertMatch({error, _}, transfer(Mode)),
        assert_initial_state(C),
        staging_probe_assert_check_violation()
    after
        allow_actions(C),
        staging_probe_unload()
    end,
    io:format("GROUP_NOTICE_PG_TRANSFER_STAGING_ROLLBACK(~p)=PASS~n", [Mode]).

%% Evidence contract for the negative scenarios: the abort must come from the
%% staging INSERT hitting the reject CHECK constraint (SQLSTATE 23514), never
%% from an earlier failure (undef, connection error, ...). Passthrough-probe
%% the real stage_s2c_tx result and prove the SQLSTATE reached the tx.
staging_probe_install() ->
    ok = meck:new(msg_store_repo, [passthrough, no_link]),
    meck:expect(
        msg_store_repo,
        stage_s2c_tx,
        fun(Conn, MsgId, Action, Payload, From, To, At) ->
            Result = meck:passthrough([Conn, MsgId, Action, Payload, From, To, At]),
            put({?MODULE, stage_s2c_tx}, Result),
            Result
        end
    ).

staging_probe_assert_check_violation() ->
    case erase({?MODULE, stage_s2c_tx}) of
        undefined ->
            error(stage_insert_never_reached);
        {error, Reason} ->
            true = contains_bin(Reason, <<"23514">>),
            io:format("STAGING_CHECK_VIOLATION SQLSTATE=23514 constraint=reject_notice~n");
        Other ->
            error({unexpected_stage_result, Other})
    end.

staging_probe_unload() ->
    case lists:member(msg_store_repo, meck:mocked()) of
        true -> meck:unload(msg_store_repo);
        false -> ok
    end.

contains_bin(T, Code) when is_tuple(T) ->
    contains_bin(tuple_to_list(T), Code);
contains_bin(L, Code) when is_list(L) ->
    lists:any(fun(X) -> contains_bin(X, Code) end, L);
contains_bin(B, Code) when is_binary(B) ->
    B =:= Code;
contains_bin(_, _Code) ->
    false.

transfer_member_disappears(C, Mode, Uid) ->
    seed(C),
    put({?MODULE, deactivate}, Uid),
    ?assertMatch({error, _}, transfer(Mode)),
    ?assertEqual(2, scalar(C, "SELECT owner_uid FROM public.\"group\" WHERE id=10")),
    ?assertEqual(
        [{2, 4}, {4, 1}, {6, 1}],
        rows(C, "SELECT user_id,role FROM group_member ORDER BY user_id", [])
    ),
    ?assertEqual(0, scalar(C, "SELECT count(*) FROM group_log")),
    ?assertEqual(0, scalar(C, "SELECT count(*) FROM msg_store_staging")),
    ?assertEqual(0, scalar(C, "SELECT status FROM group_member WHERE user_id=$1", [Uid])),
    io:format("GROUP_NOTICE_PG_TRANSFER_STALE_MEMBER(~p,~p)=PASS~n", [Mode, Uid]).

transfer_commits_consistent_notice(C, Mode) ->
    seed(C),
    ok = transfer(Mode),
    ?assertEqual(4, scalar(C, "SELECT owner_uid FROM public.\"group\" WHERE id=10")),
    OldRole =
        case Mode of
            keep_admin -> 5;
            _ -> 1
        end,
    ?assertEqual(
        [{2, OldRole}, {4, 4}, {6, 1}],
        rows(C, "SELECT user_id,role FROM group_member ORDER BY user_id", [])
    ),
    ?assertEqual(1, scalar(C, "SELECT count(*) FROM group_log WHERE type=9")),
    LogId = scalar(C, "SELECT id FROM group_log"),
    Effect = <<"group_transfer|10|", (integer_to_binary(LogId))/binary>>,
    Expected = lists:sort([{U, msg_s2c_ds:deterministic_msg_id(Effect, U)} || U <- [2, 4, 6]]),
    ?assertEqual(
        Expected, rows(C, "SELECT to_id,msg_id::text FROM msg_store_staging ORDER BY to_id", [])
    ),
    ?assertEqual(
        3,
        scalar(
            C,
            "SELECT count(*) FROM msg_store_staging WHERE action='group_transfer' AND payload->>'to_owner_uid'='4'"
        )
    ),
    io:format("GROUP_NOTICE_PG_TRANSFER_ATOMIC(~p)=PASS~n", [Mode]).

actor_crash_and_rejoin(C) ->
    seed(C),
    Parent = self(),
    meck:expect(imboy_syn, list_by_uid, fun(_) ->
        Parent ! {after_commit_online, self()},
        receive
            finish -> []
        end
    end),
    {Actor, Ref} = spawn_monitor(fun() -> group_member_logic:leave(4, 10, 4) end),
    try
        receive
            {after_commit_online, Actor} -> ok
        after 5000 -> error(post_commit_actor_not_reached)
        end,
        %% These reads occur before terminating the actor and use another connection.
        ?assertEqual(0, scalar(C, "SELECT count(*) FROM group_member WHERE user_id=4")),
        ?assertEqual(1, scalar(C, "SELECT count(*) FROM group_log")),
        ?assertEqual(3, scalar(C, "SELECT count(*) FROM msg_store_staging")),
        OldIds = rows(C, "SELECT to_id,msg_id::text FROM msg_store_staging ORDER BY to_id", []),
        OldEffect = <<"group_member_leave|10|1004">>,
        ?assertEqual(
            [{U, msg_s2c_ds:deterministic_msg_id(OldEffect, U)} || U <- [2, 4, 6]], OldIds
        ),
        ?assertEqual(
            3,
            scalar(
                C,
                "SELECT count(*) FROM msg_store_staging WHERE action='group_member_leave' AND payload->>'gid'='10' AND payload->>'leave_uid'='4'"
            )
        ),
        exit(Actor, kill),
        receive
            {'DOWN', Ref, process, Actor, killed} -> ok
        after 1000 -> error(actor_did_not_exit)
        end,
        meck:expect(imboy_syn, list_by_uid, fun(_) -> [] end),
        ok = group_member_logic:leave(4, 10, 4),
        ?assertEqual(
            OldIds, rows(C, "SELECT to_id,msg_id::text FROM msg_store_staging ORDER BY to_id", [])
        ),
        ?assertEqual(1, scalar(C, "SELECT count(*) FROM group_log")),
        ok = group_member_ds:add_member(10, 4),
        NewMemberId = scalar(C, "SELECT id FROM group_member WHERE user_id=4"),
        ?assertNotEqual(1004, NewMemberId),
        ok = group_member_logic:leave(4, 10, 4),
        ?assertEqual(6, scalar(C, "SELECT count(*) FROM msg_store_staging")),
        ?assertEqual(6, scalar(C, "SELECT count(DISTINCT msg_id) FROM msg_store_staging")),
        ?assertEqual(2, scalar(C, "SELECT count(*) FROM group_log")),
        Effect = <<"group_member_leave|10|", (integer_to_binary(NewMemberId))/binary>>,
        NewIds = [msg_s2c_ds:deterministic_msg_id(Effect, U) || U <- [2, 4, 6]],
        ActualIds = [Mid || {Mid} <- rows(C, "SELECT msg_id::text FROM msg_store_staging", [])],
        ?assert(lists:all(fun(Mid) -> lists:member(Mid, ActualIds) end, NewIds)),
        ?assertEqual([], [Mid || {_, Mid} <- OldIds, lists:member(Mid, NewIds)]),
        ?assertEqual(
            2,
            scalar(
                C,
                "SELECT count(*) FROM group_member_generation WHERE user_id=4 AND end_seq IS NOT NULL"
            )
        ),
        io:format("GROUP_NOTICE_PG_ACTOR_CRASH_REPEAT_REJOIN=PASS~n")
    after
        exit(Actor, kill),
        meck:expect(imboy_syn, list_by_uid, fun(_) -> [] end)
    end.

drain_to_formal_and_replay(C) ->
    {ok, Worker} = msg_store_worker:start_link(),
    unlink(Worker),
    try
        gen_statem:cast(Worker, kick),
        await(fun() ->
            scalar(
                C,
                "SELECT count(*) FROM msg_store_staging WHERE processed_at IS NOT NULL AND error_msg IS NULL"
            ) =:= 6
        end),
        ?assertEqual(6, scalar(C, "SELECT count(*) FROM msg_s2c")),
        Offline = msg_s2c_ds:read_msg_for_device(4, <<"new-offline-device">>, 100, undefined),
        ?assertEqual(2, length(Offline)),
        ?assert(
            lists:all(fun(M) -> maps:get(<<"action">>, M) =:= <<"group_member_leave">> end, Offline)
        ),
        Before = rows(C, "SELECT id,msg_id::text FROM msg_s2c ORDER BY id", []),
        %% Model worker death after formal INSERT but before marking staging complete.
        sql(C, "UPDATE msg_store_staging SET processed_at=NULL,available_at=now()", []),
        gen_statem:cast(Worker, kick),
        await(fun() ->
            scalar(
                C,
                "SELECT count(*) FROM msg_store_staging WHERE processed_at IS NOT NULL AND error_msg IS NULL"
            ) =:= 6
        end),
        ?assertEqual(Before, rows(C, "SELECT id,msg_id::text FROM msg_s2c ORDER BY id", [])),
        ?assertEqual(
            2, length(msg_s2c_ds:read_msg_for_device(4, <<"new-offline-device">>, 100, undefined))
        ),
        io:format("GROUP_NOTICE_PG_WORKER_OFFLINE_IDEMPOTENT=PASS~n")
    after
        gen_statem:stop(Worker)
    end.

%% Production recovery path (plan 4.6 item 1): the periodic tick must not be
%% proven only with tick=0 fixtures. Commit with no worker registered (the
%% after-commit kick is silently dropped by gen_server:cast to an unregistered
%% name), then start the worker under the DEFAULT tick configuration and never
%% send a kick: only the 1s periodic tick may drain the staging rows.
default_tick_recovers_lost_kick(C) ->
    application:unset_env(imboy, msg_store_worker_tick_ms),
    seed(C),
    undefined = whereis(msg_store_worker),
    ok = group_member_logic:leave(4, 10, 4),
    ?assertEqual(3, scalar(C, "SELECT count(*) FROM msg_store_staging")),
    %% The lost kick left rows staged; without the periodic tick they would
    %% stay forever (tick=0 mode has no other wakeup source).
    ?assertEqual(
        0,
        scalar(
            C,
            "SELECT count(*) FROM msg_store_staging WHERE processed_at IS NOT NULL AND error_msg IS NULL"
        )
    ),
    {ok, Worker} = msg_store_worker:start_link(),
    unlink(Worker),
    try
        %% No kick is ever sent: recovery must come from the default-period tick.
        await(
            fun() ->
                scalar(
                    C,
                    "SELECT count(*) FROM msg_store_staging WHERE processed_at IS NOT NULL AND error_msg IS NULL"
                ) =:= 3
            end,
            1000
        ),
        ?assertEqual(3, scalar(C, "SELECT count(*) FROM msg_s2c")),
        Offline = msg_s2c_ds:read_msg_for_device(4, <<"tick-offline-device">>, 100, undefined),
        ?assertEqual(1, length(Offline)),
        ?assert(
            lists:all(fun(M) -> maps:get(<<"action">>, M) =:= <<"group_member_leave">> end, Offline)
        ),
        io:format("GROUP_NOTICE_PG_DEFAULT_TICK_LOST_KICK_RECOVERY=PASS~n")
    after
        gen_statem:stop(Worker),
        ok = application:set_env(imboy, msg_store_worker_tick_ms, 0)
    end.

transfer(standard) -> group_logic:transfer(2, 10, 4);
transfer(keep_admin) -> group_logic:transfer(2, 10, 4, true);
transfer(drop_admin) -> group_logic:transfer(2, 10, 4, false).

assert_initial_state(C) ->
    ?assertEqual(2, scalar(C, "SELECT owner_uid FROM public.\"group\" WHERE id=10")),
    ?assertEqual(3, scalar(C, "SELECT member_count FROM public.\"group\" WHERE id=10")),
    ?assertEqual(
        [{2, 4}, {4, 1}, {6, 1}],
        rows(C, "SELECT user_id,role FROM group_member ORDER BY user_id", [])
    ),
    ?assertEqual(0, scalar(C, "SELECT count(*) FROM group_log")),
    ?assertEqual(0, scalar(C, "SELECT count(*) FROM msg_store_staging")).

reject_action(C, Action) ->
    %% Only compile-time fixture strings reach this DDL, never request input.
    sql(
        C,
        [
            "ALTER TABLE msg_store_staging ADD CONSTRAINT reject_notice CHECK (action <> '",
            Action,
            "')"
        ],
        []
    ).

allow_actions(C) -> sql(C, "ALTER TABLE msg_store_staging DROP CONSTRAINT reject_notice", []).

seed(C) ->
    sql(
        C,
        "TRUNCATE public.\"group\",group_member,group_log,group_member_generation,msg_store_seq,msg_store_staging,msg_s2c,msg_delivery,user_device RESTART IDENTITY",
        []
    ),
    sql(C, "INSERT INTO public.\"group\"(id,owner_uid,member_count) VALUES(10,2,3)", []),
    [
        begin
            Role =
                case U of
                    2 -> 4;
                    _ -> 1
                end,
            sql(
                C,
                "INSERT INTO group_member(id,group_id,user_id,role,is_join) VALUES($1,10,$2,$3,true)",
                [1000 + U, U, Role]
            ),
            sql(
                C,
                "INSERT INTO group_member_generation(group_id,user_id,start_seq) VALUES(10,$1,1)",
                [U]
            )
        end
     || U <- [2, 4, 6]
    ],
    ok.

create_schema(C) ->
    Names = [
        <<"group">>,
        <<"group_member">>,
        <<"group_log">>,
        <<"group_member_generation">>,
        <<"msg_store_seq">>,
        <<"msg_store_staging">>,
        <<"msg_s2c">>,
        <<"msg_delivery">>,
        <<"user_device">>,
        <<"channel">>
    ],
    ?assertEqual(
        0,
        scalar(
            C,
            "SELECT count(*) FROM pg_tables WHERE schemaname='public' AND tablename=ANY($1::text[])",
            [Names]
        )
    ),
    %% Minimal column-compatible fixture slices from foundation, 8, 19, 101, 165, 229.
    %% Timescale compression/retention is outside this transaction/delivery check.
    Ddl = [
        "CREATE TABLE public.\"group\"(id bigint PRIMARY KEY,owner_uid bigint NOT NULL,member_count integer NOT NULL,scope text DEFAULT 'personal',workspace_id bigint,status smallint DEFAULT 1,created_at timestamptz DEFAULT now(),updated_at timestamptz DEFAULT now())",
        "CREATE TABLE group_member(id bigint PRIMARY KEY,group_id bigint NOT NULL,user_id bigint NOT NULL,role smallint DEFAULT 1,is_join boolean DEFAULT false,join_mode varchar(120) DEFAULT '',status smallint DEFAULT 1,created_at timestamptz DEFAULT now(),updated_at timestamptz DEFAULT now(),UNIQUE(group_id,user_id))",
        "CREATE TABLE group_log(id bigint PRIMARY KEY,type integer NOT NULL,option_uid bigint NOT NULL,group_id bigint NOT NULL,body text NOT NULL,remark text DEFAULT '',created_at timestamptz NOT NULL DEFAULT now())",
        "CREATE TABLE channel(id bigint PRIMARY KEY,discussion_group_id bigint)",
        "CREATE TABLE msg_store_seq(conv_key varchar(64) PRIMARY KEY,seq bigint NOT NULL DEFAULT 0)",
        "CREATE TABLE group_member_generation(id bigint GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,group_id bigint NOT NULL,user_id bigint NOT NULL,generation_no integer DEFAULT 1,start_seq bigint NOT NULL,end_seq bigint,close_reason text,updated_at timestamptz DEFAULT now(),UNIQUE(group_id,user_id,generation_no),CHECK(end_seq IS NULL OR end_seq>=start_seq-1))",
        "CREATE UNIQUE INDEX uq_gmg_one_open_generation ON group_member_generation(group_id,user_id) WHERE end_seq IS NULL",
        "CREATE TABLE msg_s2c(id bigint NOT NULL,from_id bigint NOT NULL,to_id bigint NOT NULL,msg_id varchar(40) NOT NULL,action varchar(40) NOT NULL,msg_type varchar(40) DEFAULT '' NOT NULL,e2ee jsonb,payload jsonb NOT NULL,server_ts timestamptz DEFAULT now(),created_at timestamptz NOT NULL DEFAULT now(),PRIMARY KEY(id,created_at))",
        "CREATE TABLE msg_delivery(msg_kind varchar(8) NOT NULL,msg_id varchar(40) NOT NULL,to_uid bigint NOT NULL,to_did varchar(40) NOT NULL,created_at timestamptz DEFAULT now(),PRIMARY KEY(msg_kind,msg_id,to_uid,to_did))",
        "CREATE TABLE user_device(user_id bigint NOT NULL,device_id varchar(40) NOT NULL,status smallint DEFAULT 1,last_active_at timestamptz DEFAULT now())"
    ],
    [sql(C, D, []) || D <- Ddl],
    {ok, StagingDdl} = file:read_file(
        "priv/migrations/00000165_msg_staging_before_tsid_bootstrap.up.sql"
    ),
    Results = epgsql:squery(C, StagingDdl),
    ?assert(is_list(Results)),
    ?assertEqual([], [Error || {error, _} = Error <- Results]),
    sql(
        C,
        "ALTER TABLE msg_store_staging ADD COLUMN accepted_at timestamptz,ADD COLUMN expire_at timestamptz",
        []
    ),
    ok.

sql(C, Sql, Params) ->
    case epgsql:equery(C, iolist_to_binary(Sql), Params) of
        {ok, _} -> ok;
        {ok, [], []} -> ok;
        Other -> error({fixture_sql_failed, Sql, Other})
    end.

rows(C, Sql, Params) ->
    case epgsql:equery(C, Sql, Params) of
        {ok, _, Rows} -> Rows;
        Other -> error({fixture_query_failed, Sql, Other})
    end.

scalar(C, Sql) -> scalar(C, Sql, []).
scalar(C, Sql, Params) ->
    [{Value}] = rows(C, Sql, Params),
    Value.

await(Fun) -> await(Fun, 500).
await(_, 0) ->
    error(worker_did_not_complete);
await(Fun, N) ->
    case Fun() of
        true ->
            ok;
        false ->
            timer:sleep(20),
            await(Fun, N - 1)
    end.

assert_no_app() -> ?assertNot(lists:keymember(imboy, 1, application:which_applications())).

stop_worker() ->
    case whereis(msg_store_worker) of
        undefined -> ok;
        Pid -> gen_statem:stop(Pid)
    end.

unload(M) ->
    case lists:member(M, meck:mocked()) of
        true -> meck:unload(M);
        false -> ok
    end.
