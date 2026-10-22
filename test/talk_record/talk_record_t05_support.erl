%%% @doc T05 talk_record eunit 支撑（test-only，不进 release）。
%%%
%%% 职责（intbe02_http_support 精简版，无 HTTP listener——T05 覆盖
%%% Logic+DS+Handler action 三层，真实 HTTP 全链路留给 T22）：
%%%   1. pooler `pgsql` 池指向隔离库 127.0.0.1:55433/talk_record_t05
%%%      （env T05_PG_* 可覆写）；连接带 rfc3339 binary codec；
%%%   2. imboy app env 注入：sql_driver=pgsql、游标签名 key、幂等 TTL；
%%%   3. elib_tsid:init + meck elib_tsid:generate/1——talk_record 命名空间
%%%      尚未登记进 elib_tsid_inventory（共享文件，T05 无权改；已在报告
%%%      blocker 列出）。meck 用 VM 级唯一单调计数保证 bigint 唯一性。
%%%   4. 夹具 seed（合成 7xxxxx 段 ID）与清库辅助。
%%%
%%% 绝不触碰共享 DB/进程：池指向 55433，且不启动 imboy app 本体。
-module(talk_record_t05_support).

-export([
    setup_all/0,
    teardown_all/1,
    sql/2,
    one/2,
    seed_fixtures/1,
    truncate_talk/1,
    ids/0
]).

%% ---- 合成夹具 ID（7xxxxx 段，避开 intbe02 的 995 段）----

%% ORG_A owner / 主角
-define(U1, 700001).
%% ORG_A 普通成员，viewer grant
-define(U2, 700002).
%% ORG_A 普通成员，editor grant
-define(U3, 700003).
%% ORG_B owner（跨企业用）
-define(U4, 700004).
%% 陌生人（无任何组织关系）
-define(U7, 700007).
%% 多 org 成员（creation-contexts 翻页）
-define(U6, 700006).
%% 分页企业独立所有者，不扩张 U1 的组织关系
-define(U_PAGE_OWNER, 700008).
-define(ORG_A, 710001).
-define(ORG_B, 710002).
%% 生成 21 个 org 的基址（711001..711021）
-define(ORG_BASE_PAGE, 711000).
%% ORG_A，U1 是 ws member
-define(WS_A1, 720001).
%% ORG_A，无 ws 成员（仅 org owner/admin 可用）
-define(WS_A2, 720002).
%% ORG_B
-define(WS_B1, 720011).

-define(CURSOR_KEY, <<"t05_cursor_signing_key_0123456789abcdef">>).

%%%===================================================================
%%% Setup / Teardown
%%%===================================================================

-spec setup_all() -> map().
setup_all() ->
    case application:load(imboy) of
        ok -> ok;
        {error, {already_loaded, imboy}} -> ok
    end,
    Prev = #{
        sql_driver => application:get_env(imboy, sql_driver),
        cursor_key => application:get_env(imboy, talk_record_cursor_signing_key),
        idem_ttl => application:get_env(imboy, talk_record_idempotency_ttl_seconds)
    },
    ok = application:set_env(imboy, sql_driver, pgsql),
    ok = application:set_env(imboy, talk_record_cursor_signing_key, ?CURSOR_KEY),
    {ok, _} = application:ensure_all_started(epgsql),
    {ok, _} = application:ensure_all_started(pooler),
    ConnOpts = conn_opts(),
    catch pooler:rm_pool(pgsql),
    {ok, _} =
        pooler:new_pool(#{
            name => pgsql,
            max_count => 4,
            init_count => 1,
            start_mfa => {epgsql, connect, [ConnOpts]}
        }),
    try
        elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3})
    catch
        _:_ -> ok
    end,
    ok = tsid_meck_on(),
    {ok, Conn} = epgsql:connect(ConnOpts),
    #{conn => Conn, prev_env => Prev}.

-spec teardown_all(map()) -> ok.
teardown_all(#{conn := Conn, prev_env := Prev}) ->
    tsid_meck_off(),
    catch epgsql:close(Conn),
    catch pooler:rm_pool(pgsql),
    restore_env(sql_driver, maps:get(sql_driver, Prev)),
    restore_env(talk_record_cursor_signing_key, maps:get(cursor_key, Prev)),
    restore_env(talk_record_idempotency_ttl_seconds, maps:get(idem_ttl, Prev)),
    ok.

-spec restore_env(atom(), undefined | {ok, term()}) -> ok.
restore_env(_Key, undefined) ->
    application:unset_env(imboy, _Key);
restore_env(Key, {ok, Value}) ->
    application:set_env(imboy, Key, Value).

%%%===================================================================
%%% SQL / fixtures
%%%===================================================================

-spec sql(pid() | map(), binary() | iolist()) -> ok.
sql(Conn, Sql) ->
    case elib_pg:query(Conn, iolist_to_binary(Sql), []) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({t05_sql_error, Reason, Sql})
    end.

-spec one(pid() | map(), binary()) -> map().
one(Conn, Sql) ->
    case elib_pg:query(Conn, iolist_to_binary(Sql), []) of
        {ok, [Row | _]} -> Row;
        {ok, []} -> #{};
        {error, Reason} -> erlang:error({t05_sql_error, Reason, Sql})
    end.

-spec ids() -> map().
ids() ->
    #{
        u1 => ?U1,
        u2 => ?U2,
        u3 => ?U3,
        u4 => ?U4,
        u6 => ?U6,
        u7 => ?U7,
        org_a => ?ORG_A,
        org_b => ?ORG_B,
        org_base => ?ORG_BASE_PAGE,
        ws_a1 => ?WS_A1,
        ws_a2 => ?WS_A2,
        ws_b1 => ?WS_B1
    }.

%% @doc 基础夹具：用户/组织/工作区/成员关系（幂等：先清后插）。
-spec seed_fixtures(pid() | map()) -> ok.
seed_fixtures(Conn) ->
    truncate_talk(Conn),
    lists:foreach(
        fun(Uid) ->
            talk_record_parent_fixture:user(Conn, Uid, #{})
        end,
        [?U1, ?U2, ?U3, ?U4, ?U6, ?U7, ?U_PAGE_OWNER]
    ),
    seed_org(Conn, ?ORG_A, <<"t05-org-a">>, ?U1),
    seed_org(Conn, ?ORG_B, <<"t05-org-b">>, ?U4),
    %% U1 owner；U2/U3 普通 active 成员；U4 无成员关系
    seed_org_member(Conn, ?ORG_A, ?U1, <<"owner">>),
    seed_org_member(Conn, ?ORG_A, ?U2, <<"member">>),
    seed_org_member(Conn, ?ORG_A, ?U3, <<"member">>),
    seed_org_member(Conn, ?ORG_B, ?U4, <<"owner">>),
    seed_workspace(Conn, ?WS_A1, ?ORG_A, <<"t05-ws-a1">>),
    seed_workspace(Conn, ?WS_A2, ?ORG_A, <<"t05-ws-a2">>),
    seed_workspace(Conn, ?WS_B1, ?ORG_B, <<"t05-ws-b1">>),
    %% U1 是 WS_A1 成员；U2/U3 无 ws 成员（依赖 org 成员 + 记录归属组合测试）
    seed_ws_member(Conn, ?WS_A1, ?U1),
    %% U6 加入 21 个 org（711001..711021）——creation-contexts 翻页
    lists:foreach(
        fun(N) ->
            OrgId = ?ORG_BASE_PAGE + N,
            seed_org(Conn, OrgId, <<"t05-org-p-", (integer_to_binary(N))/binary>>, ?U_PAGE_OWNER),
            seed_org_member(Conn, OrgId, ?U6, <<"member">>)
        end,
        lists:seq(1, 21)
    ),
    ok.

-spec seed_org(pid() | map(), integer(), binary(), integer()) -> ok.
seed_org(Conn, OrgId, Name, Owner) ->
    sql(Conn, [
        "INSERT INTO organization(id,name,status,owner_id) VALUES (",
        integer_to_binary(OrgId),
        ", '",
        Name,
        "', 'active', ",
        integer_to_binary(Owner),
        ")"
    ]).

-spec seed_org_member(pid() | map(), integer(), integer(), binary()) -> ok.
seed_org_member(Conn, OrgId, Uid, Role) ->
    sql(Conn, [
        "INSERT INTO organization_member(organization_id,user_id,role,status) VALUES (",
        integer_to_binary(OrgId),
        ", ",
        integer_to_binary(Uid),
        ", '",
        Role,
        "', 'active') ON CONFLICT (organization_id,user_id) DO UPDATE SET role=EXCLUDED.role,status=EXCLUDED.status"
    ]).

-spec seed_workspace(pid() | map(), integer(), integer(), binary()) -> ok.
seed_workspace(Conn, WsId, OrgId, Name) ->
    {ok, [#{<<"owner_id">> := Owner}]} = elib_pg:query(
        Conn,
        <<"SELECT owner_id FROM organization WHERE id=$1">>,
        [OrgId]
    ),
    talk_record_parent_fixture:workspace(Conn, WsId, OrgId, Name, Owner).

-spec seed_ws_member(pid() | map(), integer(), integer()) -> ok.
seed_ws_member(Conn, WsId, Uid) ->
    sql(Conn, [
        "INSERT INTO workspace_member(workspace_id,user_id,role,status) VALUES (",
        integer_to_binary(WsId),
        ", ",
        integer_to_binary(Uid),
        ", 'member', 'active') ON CONFLICT (workspace_id,user_id) DO UPDATE SET role=EXCLUDED.role,status=EXCLUDED.status"
    ]).

%% @doc 清空 talk 域数据表（保留父表桩与成员关系由 seed_fixtures 重建）。
-spec truncate_talk(pid() | map()) -> ok.
truncate_talk(Conn) ->
    %% equery 不接受多语句；逐条执行（elib_pg:query 单语句约束）
    Extra =
        case
            elib_pg:query(Conn, <<"SELECT to_regclass('talk_record_processor_run') AS name">>, [])
        of
            {ok, [#{<<"name">> := null}]} -> "";
            {ok, [_]} -> "talk_record_processor_run, ";
            E -> error({processor_run_fixture_lookup, E})
        end,
    %% 迁移 221 新增 talk_record_cleanup_retry（FK → talk_record ON DELETE RESTRICT）；
    %% 不并入同一 TRUNCATE 则 fresh 221 库上报 0A000（cannot truncate referenced table）。
    %% 该表的行级 UPDATE/DELETE 不可变触发器不拦 TRUNCATE，合成库清空合法。
    CleanupRetry =
        case
            elib_pg:query(Conn, <<"SELECT to_regclass('talk_record_cleanup_retry') AS name">>, [])
        of
            {ok, [#{<<"name">> := null}]} -> "";
            {ok, [_]} -> "talk_record_cleanup_retry, ";
            E2 -> error({cleanup_retry_fixture_lookup, E2})
        end,
    sql(Conn, [
        "TRUNCATE ",
        Extra,
        CleanupRetry,
        empty_governance_fixture(Conn),
        "talk_record_idempotency, talk_record_consent_event, talk_record_audit, ",
        "talk_record_export, talk_record_job, talk_record_summary, talk_record_segment, ",
        "talk_record_transcript, talk_record_speaker, talk_record_mark, talk_record_audio_part, ",
        "talk_record_capture_event, talk_record_capture, talk_record_grant, ",
        "talk_record_participant, talk_record_attachment_ref, talk_record_assembly, ",
        "talk_record"
    ]),
    %% enterprise_retention_policy（迁移 117）随 workspace 夹具自动生成且 FK RESTRICT 到
    %% workspace；其行级触发器禁止 DELETE 但不拦 TRUNCATE。不清则后续父表清理报 23001。
    case elib_pg:query(Conn, <<"SELECT to_regclass('enterprise_retention_policy') AS name">>, []) of
        {ok, [#{<<"name">> := null}]} -> ok;
        {ok, [_]} -> sql(Conn, "TRUNCATE enterprise_retention_policy");
        E3 -> error({retention_policy_fixture_lookup, E3})
    end,
    %% 全套共享库中其他套件残留（enterprise_message、enterprise_asset_delete_queue 等）
    %% 经 RESTRICT FK 阻塞父表级联删除；父表逐一 DELETE 无法枚举全部残留引用方。
    %% TRUNCATE ... CASCADE 按 FK 图自动纳入全部引用表；行级不可变触发器不拦 TRUNCATE。
    %% enterprise_attachment_retention 治理守卫已在前置 TRUNCATE 生效（非空即拒绝），
    %% 此处 CASCADE 不会绕过该守卫静默清除治理事实。
    case elib_pg:query(Conn, <<"SELECT to_regclass('enterprise_conversation') AS name">>, []) of
        {ok, [#{<<"name">> := null}]} ->
            sql(Conn, "DELETE FROM workspace_member"),
            sql(Conn, "DELETE FROM workspace"),
            sql(Conn, "DELETE FROM organization"),
            sql(Conn, "DELETE FROM organization_member"),
            sql(Conn, "DELETE FROM \"user\"");
        {ok, [_]} ->
            sql(
                Conn,
                "TRUNCATE enterprise_conversation, workspace, organization, "
                "organization_member, workspace_member, \"user\" CASCADE"
            );
        E4 ->
            error({enterprise_conversation_fixture_lookup, E4})
    end.

%%%===================================================================
%%% epgsql 连接 / tsid meck
%%%===================================================================

-spec conn_opts() -> map().
conn_opts() ->
    #{
        host => os:getenv("T05_PG_HOST", "127.0.0.1"),
        port => list_to_integer(os:getenv("T05_PG_PORT", "55433")),
        username => list_to_binary(os:getenv("T05_PG_USER", "t05admin")),
        password => os:getenv("T05_PG_PASSWORD", ""),
        database => list_to_binary(os:getenv("T05_PG_DB", "talk_record_t05")),
        ssl => false,
        timeout => 10000,
        codecs => [{epgsql_codec_rfc3339_bin, []}]
    }.

%% A new FK requires naming the empty EAR table in initial fixture TRUNCATE.
%% Never erase immutable governance facts to make repeated tests pass.
empty_governance_fixture(C) ->
    case
        elib_pg:query(
            C,
            <<"SELECT EXISTS(SELECT 1 FROM pg_constraint WHERE conname='fk_ear_talk_record') AS enabled">>,
            []
        )
    of
        {ok, [#{<<"enabled">> := false}]} ->
            "";
        {ok, [_]} ->
            case
                elib_pg:query(C, <<"SELECT count(*) n FROM enterprise_attachment_retention">>, [])
            of
                {ok, [#{<<"n">> := 0}]} -> "enterprise_attachment_retention, ";
                _ -> error(governance_fixture_requires_fresh_database)
            end;
        E ->
            error({governance_fixture_lookup, E})
    end.

%% @private talk_record 命名空间未登记（见模块头），用 VM 唯一单调计数
%% 替代 TSID 槽位；bigint 范围内（9e12 起步，<< 2^63）。
-spec tsid_meck_on() -> ok.
tsid_meck_on() ->
    case lists:member(elib_tsid, meck:mocked()) of
        true ->
            ok;
        false ->
            ok = meck:new(elib_tsid, [passthrough, unstick, no_link]),
            ok = meck:expect(elib_tsid, generate, fun(_) ->
                9000000000000 + erlang:unique_integer([positive, monotonic])
            end),
            ok
    end.

-spec tsid_meck_off() -> ok.
tsid_meck_off() ->
    catch meck:unload(elib_tsid),
    ok.
