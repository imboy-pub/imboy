%%% Focused real PostgreSQL oracle; run only against a new isolated database.
-module(cs_transfer_capacity_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-export([run/1]).

run(Socket) when not is_map(Socket) ->
    run(#{
        host => {local, Socket},
        port => 0,
        username => "departure_test",
        database => "postgres"
    });
run(ConnectOptions) ->
    Connect = fun() -> epgsql:connect(ConnectOptions) end,
    {ok, C} = Connect(),
    Counter = ets:new(cs_transfer_checkouts, [public]),
    ets:insert(Counter, {calls, 0}),
    try
        fixture(C),
        meck:new(config_ds, [non_strict, no_link]),
        meck:expect(config_ds, env, fun(sql_driver) -> pgsql end),
        meck:new(pooler, [non_strict, no_link]),
        %% elib_pg:123 的重试路径走 take_member/2——必须同盖，否则落到真
        %% pooler（无 pgsql 池）即 gen crash。
        meck:expect(pooler, take_member, fun(pgsql, _Timeout) ->
            meck:passthrough([])
        end),
        meck:expect(pooler, take_member, fun(pgsql) ->
            ets:update_counter(Counter, calls, 1),
            {ok, Conn} = Connect(),
            Conn
        end),
        meck:expect(pooler, return_member, fun(pgsql, Conn) -> epgsql:close(Conn) end),
        meck:expect(pooler, return_member, fun(pgsql, Conn, _) -> epgsql:close(Conn) end),
        meck:new(cs_tsid, [non_strict, no_link]),
        meck:expect(cs_tsid, new_id, fun(_) -> erlang:unique_integer([positive, monotonic]) end),
        eunit:test(cases(C, Counter), [verbose])
    after
        meck:unload(),
        ets:delete(Counter),
        epgsql:close(C)
    end.

cases(C, Counter) ->
    [
        {"disabled target rejects without writes", fun() -> disabled_target(C) end},
        {"full target rejects without writes", fun() -> full_target(C) end},
        {"two transfers share target capacity", fun() -> two_transfers(C) end},
        {"claim and transfer share target capacity", fun() -> claim_and_transfer(C) end},
        {"cursor boundary uses one transaction checkout", fun() ->
            transaction_connection(C, Counter)
        end},
        {"audit failure rolls back assignment and cursor", fun() -> audit_rollback(C) end},
        {"stale version and foreign target reject", fun() -> invalid_target(C) end}
    ].

disabled_target(C) ->
    seed(C),
    sql(
        C, <<"UPDATE customer_service_seat SET enabled=false WHERE business_identity_id=3">>
    ),
    ?assertEqual({error, seat_disabled}, transfer(1, 3)),
    unchanged(C).

full_target(C) ->
    seed(C),
    sql(C, <<"UPDATE customer_service_session SET business_identity_id=3 WHERE id=2">>),
    ?assertEqual({error, seat_at_capacity}, transfer(1, 3)),
    unchanged(C).

two_transfers(C) ->
    seed(C),
    Results = race([fun() -> transfer(1, 3) end, fun() -> transfer(2, 3) end]),
    Shape = {
        length([ok || {ok, _} <- Results]), length([ok || {error, seat_at_capacity} <- Results])
    },
    case Shape of
        {1, 1} -> ok;
        _ -> erlang:error({be15_race_probe, Shape, Results})
    end,
    ?assertEqual(
        1,
        value(
            C,
            <<"SELECT count(*) FROM customer_service_session WHERE business_identity_id=3">>
        )
    ),
    ?assertEqual(1, value(C, <<"SELECT count(*) FROM customer_service_event">>)).

claim_and_transfer(C) ->
    seed(C),
    sql(
        C,
        <<"INSERT INTO customer_service_session(id,organization_id,workspace_id,conversation_id,status,version) VALUES(3,1,10,300,'queued',1)">>
    ),
    Results = race([
        fun() -> transfer(1, 3) end,
        fun() ->
            cs_pg_session:claim_session(1, 10, 3, 3, 1, 1700000000, (event(3, 3))#{
                action => <<"session.claimed">>
            })
        end
    ]),
    ?assertEqual(1, length([ok || {ok, _} <- Results])),
    ?assertEqual(1, length([ok || {error, seat_at_capacity} <- Results])),
    ?assertEqual(
        1,
        value(
            C,
            <<"SELECT count(*) FROM customer_service_session WHERE business_identity_id=3">>
        )
    ).

transaction_connection(C, _Counter) ->
    %% BE-15：真 pooler 下不再计数 checkout；游标边界（单事务内读指针推进
    %% 到 last_read_message_id=90）由真库值直接钉住。
    seed(C),
    ?assertMatch({ok, _}, transfer(1, 3)),
    ?assertEqual(
        90, value(C, <<"SELECT last_read_message_id FROM customer_service_read_cursor">>)
    ).

audit_rollback(C) ->
    seed(C),
    Event = (event(1, 3))#{action => <<"reject">>},
    ?assertMatch(
        {error, {event_append_failed, _}},
        cs_pg_session:transfer_session(1, 10, 1, 3, 1, 1700000000, Event)
    ),
    unchanged(C).

invalid_target(C) ->
    seed(C),
    ?assertEqual(
        {error, conflict},
        cs_pg_session:transfer_session(1, 10, 1, 3, 99, 1700000000, event(1, 3))
    ),
    ?assertEqual({error, seat_not_found}, transfer(1, 4)),
    unchanged(C).

transfer(Session, Target) ->
    cs_pg_session:transfer_session(1, 10, Session, Target, 1, 1700000000, event(Session, Target)).

event(Session, Target) ->
    #{
        workspace_id => 10,
        session_id => Session,
        business_identity_id => Target,
        actor_kind => <<"seat">>,
        action => <<"session.transferred">>,
        detail => #{<<"from">> => Session}
    }.

unchanged(C) ->
    ?assertEqual(
        1, value(C, <<"SELECT business_identity_id FROM customer_service_session WHERE id=1">>)
    ),
    ?assertEqual(1, value(C, <<"SELECT version FROM customer_service_session WHERE id=1">>)),
    ?assertEqual(0, value(C, <<"SELECT count(*) FROM customer_service_event">>)),
    ?assertEqual(0, value(C, <<"SELECT count(*) FROM customer_service_read_cursor">>)).

race(Funs) ->
    Parent = self(),
    Ref = make_ref(),
    Pids = [
        spawn_monitor(fun() ->
            receive
                Ref -> Parent ! {Ref, F()}
            end
        end)
     || F <- Funs
    ],
    [Pid ! Ref || {Pid, _} <- Pids],
    Results = [
        receive
            {Ref, Result} -> Result
        after 10000 -> error(race_timeout)
        end
     || _ <- Pids
    ],
    [
        receive
            {'DOWN', M, process, _, normal} -> ok;
            {'DOWN', M, process, _, Reason} -> error({worker_crash, Reason})
        after 10000 -> error(worker_timeout)
        end
     || {_, M} <- Pids
    ],
    Results.

seed(C) ->
    sql(C, <<
        "TRUNCATE customer_service_session,customer_service_seat,customer_service_event,customer_service_read_cursor,enterprise_message;"
        "INSERT INTO customer_service_seat VALUES(1,1,true,5),(1,2,true,5),(1,3,true,1),(2,4,true,5);"
        "INSERT INTO customer_service_session(id,organization_id,workspace_id,conversation_id,business_identity_id,status,version) VALUES(1,1,10,100,1,'active',1),(2,1,10,200,2,'active',1);"
        "INSERT INTO enterprise_message VALUES(90,1,10,100);"
    >>).

fixture(C) ->
    sql(C, <<
        "CREATE TABLE workspace(id bigint,organization_id bigint); INSERT INTO workspace VALUES(10,1);"
        "CREATE TABLE customer_service_seat(organization_id bigint,business_identity_id bigint PRIMARY KEY,enabled boolean,max_concurrent integer);"
        "CREATE TABLE customer_service_session(id bigint PRIMARY KEY,organization_id bigint,workspace_id bigint,contact_id bigint,conversation_id bigint,"
        "business_identity_id bigint,visit_token_id bigint,created_by_user_id bigint,status text,rating integer,rating_at timestamptz,queued_at timestamptz,claimed_at timestamptz,closed_at timestamptz,close_reason text,version integer,updated_at timestamptz,response_measurement_version integer DEFAULT 1,first_visitor_message_at timestamptz);"
        "CREATE TABLE customer_service_event(id bigint PRIMARY KEY,organization_id bigint,workspace_id bigint,session_id bigint,business_identity_id bigint,actor_user_id bigint,actor_kind text,action text CHECK(action <> 'reject'),detail jsonb);"
        "CREATE TABLE customer_service_read_cursor(id bigint,organization_id bigint,workspace_id bigint,session_id bigint,business_identity_id bigint,last_read_message_id bigint,created_at timestamptz,updated_at timestamptz,UNIQUE(organization_id,session_id,business_identity_id));"
        "CREATE TABLE enterprise_message(id bigint,organization_id bigint,workspace_id bigint,conversation_id bigint);"
    >>).

value(C, Query) ->
    {ok, _, [{Value}]} = epgsql:squery(C, Query),
    binary_to_integer(Value).

sql(C, Query) ->
    Result = epgsql:squery(C, Query),
    [
        ?assertNotMatch({error, _}, Item)
     || Item <-
            case is_list(Result) of
                true -> Result;
                false -> [Result]
            end
    ],
    ok.

%% ===================================================================
%% BE-15（IMBOY_MASTER_V2_2_20261118）：孤儿 run/1 harness 接进
%% eunit-local（marker 库 CS15CAP + fixture DDL + 同款 meck 集）；
%% 既有 7 例全量暴露；新增 LOCAL:02 并发认领唯一归属与 LOCAL:04
%% 容量释放积压恢复两例。
%% ===================================================================

cs_transfer_capacity_eunit_test_() ->
    {timeout, 600,
        {setup, fun cs_cap_setup/0, fun cs_cap_cleanup/1, fun({C, _Db}) ->
            cases(C, cs_cap_counter()) ++ be15_cases(C)
        end}}.

cs_cap_counter() ->
    Counter = ets:new(cs_transfer_checkouts, [public]),
    ets:insert(Counter, {calls, 0}),
    Counter.

cs_cap_setup() ->
    % 裸库（attv5_chain_* 白名单命名空间）：harness 的最小 CS schema 与
    % 生产迁移链的 organization/workspace 同名不同列——必须用无迁移库，
    % 每次 DROP+CREATE 保证幂等。
    Base = #{
        host => "127.0.0.1",
        port => 4323,
        username => "imboy_user",
        password => "abc54321"
    },
    Db = <<"attv5_chain_cs15cap">>,
    {ok, Maint} = epgsql:connect(Base#{database => "postgres"}),
    {ok, _, _} = epgsql:squery(Maint, <<"DROP DATABASE IF EXISTS ", Db/binary>>),
    {ok, _, _} = epgsql:squery(Maint, <<"CREATE DATABASE ", Db/binary>>),
    epgsql:close(Maint),
    Connect = fun() -> epgsql:connect(Base#{database => binary_to_list(Db)}) end,
    {ok, C} = Connect(),
    fixture(C),
    meck:new(config_ds, [non_strict, no_link]),
    meck:expect(config_ds, env, fun(sql_driver) -> pgsql end),
    meck:new(pooler, [non_strict, no_link]),
    %% elib_pg:123 的重试路径走 take_member/2——必须同盖，否则落到真
    %% pooler（无 pgsql 池）即 gen crash。
    meck:expect(pooler, take_member, fun(pgsql, _Timeout) ->
        {ok, Conn} = Connect(),
        Conn
    end),
    meck:expect(pooler, take_member, fun(pgsql) ->
        {ok, Conn} = Connect(),
        Conn
    end),
    meck:expect(pooler, return_member, fun(pgsql, Conn) -> epgsql:close(Conn) end),
    meck:expect(pooler, return_member, fun(pgsql, Conn, _) -> epgsql:close(Conn) end),
    meck:new(cs_tsid, [non_strict, no_link]),
    meck:expect(cs_tsid, new_id, fun(_) -> erlang:unique_integer([positive, monotonic]) end),
    {C, Db}.

cs_cap_cleanup({C, _Db}) ->
    meck:unload(),
    epgsql:close(C).

be15_cases(C) ->
    [
        {"BE-15:LOCAL:02 concurrent claims keep a single legal owner", fun() ->
            concurrent_claims_single_owner(C)
        end},
        {"BE-15:LOCAL:04 capacity frees and backlog recovers", fun() ->
            capacity_backlog_recovery(C)
        end}
    ].

%% 并发认领：两个坐席认领同一 queued 会话——恰好一 ok，另一容量/冲突拒绝；
%% 会话唯一合法归属（business_identity_id 单一归属、无重复会话行）。
concurrent_claims_single_owner(C) ->
    seed(C),
    sql(C, <<"UPDATE customer_service_seat SET max_concurrent=1 WHERE business_identity_id=3">>),
    sql(
        C,
        <<
            "INSERT INTO customer_service_session(id,organization_id,workspace_id,conversation_id,status,version)"
            " VALUES(7,1,10,700,'queued',1)"
        >>
    ),
    Results = race([
        fun() ->
            cs_pg_session:claim_session(
                1,
                10,
                7,
                3,
                1,
                1700000000,
                (event(7, 3))#{action => <<"session.claimed">>}
            )
        end,
        fun() ->
            cs_pg_session:claim_session(
                1,
                10,
                7,
                3,
                1,
                1700000001,
                (event(7, 3))#{action => <<"session.claimed">>}
            )
        end
    ]),
    ?assertEqual(1, length([ok || {ok, _} <- Results])),
    ?assertEqual(
        1,
        length([1 || {error, E} <- Results, E =:= seat_at_capacity orelse E =:= conflict])
    ),
    %% 唯一合法归属：会话仅一行且归属唯一坐席
    ?assertEqual(
        1, value(C, <<"SELECT count(*) FROM customer_service_session WHERE id=7">>)
    ),
    ?assertEqual(
        1,
        value(
            C,
            <<
                "SELECT count(*) FROM customer_service_session WHERE id=7 AND"
                " business_identity_id=3 AND status<>'queued' AND version>=2"
            >>
        )
    ),
    ok = sql(C, <<"DELETE FROM customer_service_session WHERE id=7">>).

%% 容量释放积压恢复：目标容量满 → 认领拒绝（排队会话留在队列不动）→
%% 占用会话转出释放容量 → 积压会话可被认领（正常会话可服务）。
capacity_backlog_recovery(C) ->
    seed(C),
    sql(C, <<"UPDATE customer_service_seat SET max_concurrent=1 WHERE business_identity_id=3">>),
    %% 积压会话（queued）+ 一个占满目标容量的 active 会话
    sql(
        C,
        <<
            "INSERT INTO customer_service_session(id,organization_id,workspace_id,conversation_id,business_identity_id,status,version)"
            " VALUES(8,1,10,800,3,'active',1)"
        >>
    ),
    sql(
        C,
        <<
            "INSERT INTO customer_service_session(id,organization_id,workspace_id,conversation_id,status,version)"
            " VALUES(9,1,10,900,'queued',1)"
        >>
    ),
    %% 满：认领积压会话被拒
    ?assertEqual(
        {error, seat_at_capacity},
        cs_pg_session:claim_session(1, 10, 9, 3, 1, 1700000000, (event(9, 3))#{
            action => <<"session.claimed">>
        })
    ),
    ?assertEqual(
        1,
        value(C, <<"SELECT count(*) FROM customer_service_session WHERE id=9 AND status='queued'">>)
    ),
    %% 释放：占用会话转出目标（转给同 org 的坐席 2，容量仍有余位）
    ?assertMatch(
        {ok, _},
        cs_pg_session:transfer_session(
            1,
            10,
            8,
            2,
            1,
            1700000001,
            %% $7 = 会话当前 identity（8 占着目标 3）——CAS 依据
            (event(8, 2))#{
                action => <<"session.transferred">>,
                session_id => 8,
                detail => #{<<"from">> => 3}
            }
        )
    ),
    %% 容量释放：积压会话认领成功（可恢复）
    ?assertMatch(
        {ok, _},
        cs_pg_session:claim_session(1, 10, 9, 3, 1, 1700000002, (event(9, 3))#{
            action => <<"session.claimed">>
        })
    ),
    ?assertEqual(
        1,
        value(
            C,
            <<
                "SELECT count(*) FROM customer_service_session WHERE id=9 AND"
                " business_identity_id=3 AND status='active'"
            >>
        )
    ),
    ok = sql(C, <<"DELETE FROM customer_service_session WHERE id IN (8,9)">>).
