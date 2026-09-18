%% @doc D6：agent 域 DeletionPreflightFacts provider 的真库契约测试。
%%
%% == 运行前置 ==
%%
%% 隔离一次性 PG（非共享库、非生产），库内已应用全链迁移（bot 表来自
%% 00000070）。连接参数只经进程环境变量注入：
%%
%%   AG31_PG_HOST / AG31_PG_PORT / AG31_PG_DB / AG31_PG_USER / AG31_PG_PASSWORD(可空)
%%
%% == 铁律 ==
%%
%% 环境变量缺失或连不上库 → 显式 FAIL（erlang:error），禁止静默 skip。
%% 夹具自建自清（固定高位 id 区段 992000+，与 990000+/991000+ 区段不相交，
%% 不 TRUNCATE 任何共享表）；清理走 session_replication_role=replica 旁路，
%% 按子表（bot）→父表（"user"）顺序显式删除。
-module(agent_preflight_facts_pg_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MOD, agent_preflight_facts_pg).

%% 夹具固定 id 区段（992000+）
-define(ID_SUBJECT, 992010).
-define(ID_BOT_ACTIVE1, 992011).
-define(ID_BOT_ACTIVE2, 992012).
-define(ID_BOT_DISABLED, 992013).
-define(ID_BOT_DELETED, 992014).
-define(ID_OTHER_OWNER, 992020).
-define(ID_OTHER_BOT, 992021).
-define(ID_CLEAN_SUBJECT, 992030).

-define(SEGMENT_USERS, [
    ?ID_SUBJECT,
    ?ID_BOT_ACTIVE1,
    ?ID_BOT_ACTIVE2,
    ?ID_BOT_DISABLED,
    ?ID_BOT_DELETED,
    ?ID_OTHER_OWNER,
    ?ID_OTHER_BOT,
    ?ID_CLEAN_SUBJECT
]).

%% ===================================================================
%% 套件组织
%% ===================================================================

agent_preflight_facts_pg_test_() ->
    {setup, fun setup_required/0, fun teardown/1, fun(Conn) ->
        {inorder, [
            {"a: two active owned bots -> frozen shape + two AGENT_OWNER_ACTIVE blockers", fun() ->
                t_active_ownership_shape(Conn)
            end},
            {"b: disabled(-0)/deleted(-1) owned bots excluded from blockers", fun() ->
                t_status_filtering(Conn)
            end},
            {"c: disable resolution path removes blocker without row delete", fun() ->
                t_disable_resolution(Conn)
            end},
            {"d: another owner's bots isolated; clean subject zero blockers", fun() ->
                t_owner_isolation_and_empty(Conn)
            end},
            {"e: bad subject -> unavailable (real-db guard)", fun() ->
                t_bad_subject(Conn)
            end},
            {"z: fixture fully cleaned", fun() -> t_cleanup_verified(Conn) end}
        ]}
    end}.

%% 环境变量缺失 → 显式 FAIL（任务铁律：禁止静默 skip/pass）。
%% 双通道：裸 epgsql 连接（夹具造数/清理）+ pooler `pgsql` 池（被测
%% provider 经 elib_pg:query 走池——与 ORG cs_preflight 套件同形态）。
setup_required() ->
    Missing = [
        K
     || K <- ["AG31_PG_HOST", "AG31_PG_PORT", "AG31_PG_DB", "AG31_PG_USER"],
        os:getenv(K) =:= false
    ],
    case Missing of
        [] -> ok;
        _ -> erlang:error({ag31_pg_missing_env, Missing})
    end,
    ConnOpts = #{
        host => os:getenv("AG31_PG_HOST"),
        port => list_to_integer(os:getenv("AG31_PG_PORT")),
        username => os:getenv("AG31_PG_USER"),
        password => os:getenv("AG31_PG_PASSWORD", ""),
        database => os:getenv("AG31_PG_DB")
    },
    {ok, Conn} =
        case epgsql:connect(ConnOpts) of
            {ok, C} -> {ok, C};
            {error, Reason} -> erlang:error({ag31_pg_connect_failed, Reason})
        end,
    ok = ensure_pool(ConnOpts),
    Conn.

ensure_pool(ConnOpts) ->
    _ = application:load(imboy),
    {ok, _} = application:ensure_all_started(pooler),
    PgConf = #{
        name => pgsql,
        max_count => 5,
        init_count => 2,
        start_mfa => {epgsql, connect, [ConnOpts#{ssl => false, timeout => 4000}]}
    },
    _ = pooler:new_pool(PgConf),
    ok.

teardown(Conn) ->
    cleanup_fixture(Conn),
    try epgsql:close(Conn) of
        _ -> ok
    catch
        _:_ -> ok
    end,
    try pooler:rm_pool(pgsql) of
        _ -> ok
    catch
        _:_ -> ok
    end,
    ok.

%% ===================================================================
%% 夹具
%% ===================================================================

fixture(Conn) ->
    cleanup_fixture(Conn),
    lists:foreach(
        fun(Id) -> insert_user(Conn, Id) end,
        ?SEGMENT_USERS
    ),
    insert_bot(Conn, ?ID_BOT_ACTIVE1, ?ID_SUBJECT, 1),
    insert_bot(Conn, ?ID_BOT_ACTIVE2, ?ID_SUBJECT, 1),
    insert_bot(Conn, ?ID_BOT_DISABLED, ?ID_SUBJECT, 0),
    insert_bot(Conn, ?ID_BOT_DELETED, ?ID_SUBJECT, -1),
    insert_bot(Conn, ?ID_OTHER_BOT, ?ID_OTHER_OWNER, 1),
    ok.

insert_user(Conn, Id) ->
    {ok, 1} = epgsql:equery(
        Conn,
        <<
            "INSERT INTO \"user\" (id, password, account, reg_ip, reg_cosv)"
            " VALUES ($1, 'fixture', $2, '127.0.0.1', 'fixture')"
        >>,
        [Id, <<"d6fixture-", (integer_to_binary(Id))/binary>>]
    ),
    ok.

insert_bot(Conn, BotUserId, OwnerUid, Status) ->
    {ok, 1} = epgsql:equery(
        Conn,
        <<
            "INSERT INTO bot (user_id, name, username, owner_uid, status)"
            " VALUES ($1, 'd6-fixture-bot', $2, $3, $4)"
        >>,
        [BotUserId, <<"d6bot-", (integer_to_binary(BotUserId))/binary>>, OwnerUid, Status]
    ),
    ok.

%% replica 旁路 + 子表→父表序（bot 引用 "user"；append-only 触发器不适用
%% 于本两表，旁路为一致性保险而非必需）。
cleanup_fixture(Conn) ->
    {ok, _, _} = epgsql:squery(Conn, "SET session_replication_role = replica"),
    {ok, _} = epgsql:equery(
        Conn, "DELETE FROM bot WHERE user_id = ANY($1)", [?SEGMENT_USERS]
    ),
    {ok, _} = epgsql:equery(
        Conn, "DELETE FROM \"user\" WHERE id = ANY($1)", [?SEGMENT_USERS]
    ),
    {ok, _, _} = epgsql:squery(Conn, "SET session_replication_role = DEFAULT"),
    ok.

%% ===================================================================
%% 用例
%% ===================================================================

t_active_ownership_shape(Conn) ->
    fixture(Conn),
    {ok, Fact} = ?MOD:facts_agent(?ID_SUBJECT),
    %% §1.6 冻结五键
    ?assertEqual(?ID_SUBJECT, maps:get(subject_user_id, Fact)),
    ?assertEqual(agent, maps:get(domain, Fact)),
    ?assert(is_integer(maps:get(observed_at, Fact))),
    ?assertEqual(1, maps:get(fact_version, Fact)),
    %% 两个 active owned bot -> 两个冻结四字段 blocker（opaque、org=null）
    ?assertEqual(
        [
            #{
                code => <<"AGENT_OWNER_ACTIVE">>,
                resource_type => <<"bot">>,
                resource_id => integer_to_binary(?ID_BOT_ACTIVE1),
                organization_id => null
            },
            #{
                code => <<"AGENT_OWNER_ACTIVE">>,
                resource_type => <<"bot">>,
                resource_id => integer_to_binary(?ID_BOT_ACTIVE2),
                organization_id => null
            }
        ],
        maps:get(blockers, Fact)
    ).

t_status_filtering(Conn) ->
    fixture(Conn),
    {ok, Fact} = ?MOD:facts_agent(?ID_SUBJECT),
    Ids = [maps:get(resource_id, B) || B <- maps:get(blockers, Fact)],
    %% status=0（disabled）与 status=-1（deleted）的归属 bot 不产 blocker
    ?assertEqual(false, lists:member(integer_to_binary(?ID_BOT_DISABLED), Ids)),
    ?assertEqual(false, lists:member(integer_to_binary(?ID_BOT_DELETED), Ids)),
    ?assertEqual(2, length(Ids)).

t_disable_resolution(Conn) ->
    fixture(Conn),
    {ok, 1} = epgsql:equery(
        Conn,
        <<
            "UPDATE bot SET status = 0, updated_at = CURRENT_TIMESTAMP"
            " WHERE user_id = $1"
        >>,
        [?ID_BOT_ACTIVE1]
    ),
    {ok, Fact} = ?MOD:facts_agent(?ID_SUBJECT),
    %% 停用（不删行）即消除 blocker——offboarding-free 的最小消除路径
    ?assertEqual(
        [integer_to_binary(?ID_BOT_ACTIVE2)],
        [maps:get(resource_id, B) || B <- maps:get(blockers, Fact)]
    ).

t_owner_isolation_and_empty(Conn) ->
    fixture(Conn),
    %% 另一所有者的 active bot 只归属其所有者
    {ok, OtherFact} = ?MOD:facts_agent(?ID_OTHER_OWNER),
    ?assertEqual(
        [integer_to_binary(?ID_OTHER_BOT)],
        [maps:get(resource_id, B) || B <- maps:get(blockers, OtherFact)]
    ),
    %% 无归属 bot 的 subject → 零 blocker（实时读取，非缓存 allow）
    {ok, CleanFact} = ?MOD:facts_agent(?ID_CLEAN_SUBJECT),
    ?assertEqual([], maps:get(blockers, CleanFact)).

t_bad_subject(_Conn) ->
    ?assertEqual({error, unavailable}, ?MOD:facts_agent(0)),
    ?assertEqual({error, unavailable}, ?MOD:facts_agent(-1)),
    ?assertEqual({error, unavailable}, ?MOD:facts_agent(<<"x">>)).

t_cleanup_verified(Conn) ->
    cleanup_fixture(Conn),
    {ok, _, [{BotCount}]} = epgsql:equery(
        Conn, "SELECT count(*) FROM bot WHERE user_id = ANY($1)", [?SEGMENT_USERS]
    ),
    {ok, _, [{UserCount}]} = epgsql:equery(
        Conn, "SELECT count(*) FROM \"user\" WHERE id = ANY($1)", [?SEGMENT_USERS]
    ),
    ?assertEqual(0, BotCount),
    ?assertEqual(0, UserCount).
