%%% @doc CS-01 触库套件的合成租户夹具（test-only；镜像 `eb_pg_test_fixture` 的
%%% 随机 TSID scope 模式）。
%%%
%%% 隔离策略：每次 `new_scope/1` 都生成全新随机 TSID 的 Org/Workspace/User，
%%% 套件间互不可见；不做 TRUNCATE、不写共享库、不删别人的行。合成数据只含
%%% 随机 TSID 与 `cs01-` 前缀合成文本，无真实联系方式/客户资料/生产资源。
%%%
%%% 清场：customer_service 表先删（事件 → 会话 → token/key → seat），再清
%%% enterprise 侧被引用行；清场是 best-effort，隔离靠唯一 TSID。
-module(cs_pg_test_fixture).

-export([
    id/0,
    new_scope/0,
    new_scope/1,
    cleanup/1,
    exec/2,
    tx/1,
    scalar/2,
    scalar/3,
    count/2,
    table/1,
    key_ref/0
]).

-define(ACCOUNT_PREFIX, <<"cs01-account-">>).
-define(NAME_PREFIX, <<"cs01-name-">>).

%% @doc 生成一个新的合成 TSID。
-spec id() -> integer().
id() ->
    try
        elib_tsid:generate(default)
    catch
        _:_ ->
            1000000000000000 + erlang:unique_integer([positive, monotonic])
    end.

%% @doc 合成主密钥引用（32 字节随机 + 声明版本）。测试专用，非生产密钥。
-spec key_ref() -> map().
key_ref() ->
    #{key => crypto:strong_rand_bytes(32), key_version => 1}.

-spec new_scope() -> map().
new_scope() ->
    new_scope(#{}).

%% @doc 建立一套完整合成租户：owner/actor/peer 三个 user、Org + Workspace、
%% org member（actor）、sales + customer_service 两个 identity、sales 的 active
%% assignment、contact、带 synthetic consent 的 conversation、retention policy
%% （enterprise_message/1095，消息全栈路径需要），以及 service identity 的 seat
%% （enabled，max_concurrent 可经 Opts 覆盖）。
%%
%% Opts：
%%   max_concurrent => pos_integer()（seat 上限，默认 1）
-spec new_scope(map()) -> map().
new_scope(Opts) ->
    Owner = id(),
    Actor = id(),
    Peer = id(),
    Org = id(),
    Workspace = id(),
    OtherOrg = id(),
    OtherWorkspace = id(),
    Sales = id(),
    Service = id(),
    Assignment = id(),
    Contact = id(),
    Conversation = id(),
    Policy = id(),

    ok = exec(
        <<
            "INSERT INTO \"user\"(id,password,account,reg_ip,reg_cosv) VALUES"
            " ($1,'x',$2,'127.0.0.1','x'),($3,'x',$4,'127.0.0.1','x'),($5,'x',$6,'127.0.0.1','x')"
        >>,
        [Owner, account(Owner), Actor, account(Actor), Peer, account(Peer)]
    ),
    ok = exec(<<"INSERT INTO organization(id,name,owner_id,status) VALUES ($1,$2,$3,'active')">>, [
        Org, name(<<"cs01-org-">>, Org), Owner
    ]),
    ok = exec(
        <<
            "INSERT INTO workspace(id,name,owner_id,status,type,organization_id)"
            " VALUES ($1,$2,$3,'active','project',$4)"
        >>,
        [Workspace, name(<<"cs01-ws-">>, Workspace), Owner, Org]
    ),
    %% 跨租户负例：另一个 Org + Workspace（同 owner）
    ok = exec(<<"INSERT INTO organization(id,name,owner_id,status) VALUES ($1,$2,$3,'active')">>, [
        OtherOrg, name(<<"cs01-org-">>, OtherOrg), Owner
    ]),
    ok = exec(
        <<
            "INSERT INTO workspace(id,name,owner_id,status,type,organization_id)"
            " VALUES ($1,$2,$3,'active','project',$4)"
        >>,
        [OtherWorkspace, name(<<"cs01-ws-">>, OtherWorkspace), Owner, OtherOrg]
    ),
    ok = exec(
        <<
            "INSERT INTO organization_member(organization_id,user_id,role,status)"
            " VALUES ($1,$2,'member','active')"
        >>,
        [Org, Actor]
    ),
    ok = exec(
        <<
            "INSERT INTO organization_business_identity"
            " (id,organization_id,function_key,display_name,status,version,created_by_user_id)"
            " VALUES ($1,$2,'sales',$3,'active',1,$4),($5,$2,'customer_service',$6,'active',1,$4)"
        >>,
        [
            Sales,
            Org,
            name(<<"cs01-sales-">>, Sales),
            Owner,
            Service,
            name(<<"cs01-service-">>, Service)
        ]
    ),
    ok = exec(
        <<
            "INSERT INTO organization_business_identity_assignment"
            " (id,organization_id,business_identity_id,function_key,user_id,status,assigned_by,version)"
            " VALUES ($1,$2,$3,'sales',$4,'active',$5,1)"
        >>,
        [Assignment, Org, Sales, Actor, Owner]
    ),
    ok = exec(
        <<
            "INSERT INTO enterprise_contact"
            " (id,organization_id,imboy_user_id,status,display_name,"
            "  created_by_business_identity_id,version)"
            " VALUES ($1,$2,NULL,'active',$3,$4,1)"
        >>,
        [Contact, Org, name(<<"cs01-contact-">>, Contact), Service]
    ),
    ok = exec(
        <<
            "INSERT INTO enterprise_conversation"
            " (id,organization_id,workspace_id,contact_id,business_identity_id,status,version,"
            "  notice_version,consent_at,consent_subject,consent_evidence_kind)"
            " VALUES ($1,$2,$3,$4,$5,'active',1,'cs01-notice-v1',CURRENT_TIMESTAMP,$6,'synthetic')"
        >>,
        [Conversation, Org, Workspace, Contact, Service, name(<<"cs01-consent-">>, Conversation)]
    ),
    ok = exec(
        <<
            "INSERT INTO enterprise_retention_policy"
            " (id,organization_id,workspace_id,data_class,version,retention_days,"
            "  trigger_event,created_by_user_id)"
            " VALUES ($1,$2,$3,'enterprise_message',1,1095,'message.accept',$4)"
        >>,
        [Policy, Org, Workspace, Owner]
    ),
    Seat = id(),
    MaxConcurrent = maps:get(max_concurrent, Opts, 1),
    ok = exec(
        <<
            "INSERT INTO customer_service_seat"
            " (organization_id, business_identity_id, function_key, enabled, max_concurrent,"
            "  created_by_user_id)"
            " VALUES ($1, $2, 'customer_service', true, $3, $4)"
        >>,
        [Org, Service, MaxConcurrent, Owner]
    ),
    #{
        owner_user_id => Owner,
        actor_user_id => Actor,
        peer_user_id => Peer,
        org_id => Org,
        workspace_id => Workspace,
        other_org_id => OtherOrg,
        other_workspace_id => OtherWorkspace,
        sales_identity_id => Sales,
        service_identity_id => Service,
        assignment_id => Assignment,
        contact_id => Contact,
        conversation_id => Conversation,
        policy_id => Policy,
        max_concurrent => MaxConcurrent
    }.

%% ===================================================================
%% 清场（best-effort）
%% ===================================================================

-spec cleanup(map()) -> ok.
cleanup(Scope) ->
    Org = maps:get(org_id, Scope, undefined),
    Workspace = maps:get(workspace_id, Scope, undefined),
    Actor = maps:get(actor_user_id, Scope, undefined),
    case {is_integer(Org), is_integer(Workspace)} of
        {true, true} ->
            %% R2：customer_service_event 是 append-only（触发器禁 DELETE），
            %% 普通清理永远 23514，且 event → session → seat 的 RESTRICT 链会把
            %% 后续清理全部挡死（run5/R2 实证：每跑一轮套件永久累积残留）。
            %% 处置：customer_service 五表在一个事务内「临时禁用触发器 → 删本
            %% scope → 复原触发器」（与 eb_offboarding_concurrency_tests 的 a07
            %% 同款手法，需要表 owner 权限——scratch 库由 imboy_user 持有）。
            ok = purge_cs_tables(Org),
            lists:foreach(fun(Sql) -> _ = quiet(Sql) end, cleanup_sql(Org, Workspace, Actor)),
            ok;
        _ ->
            ok
    end.

purge_cs_tables(Org) ->
    _ = elib_pg:with_tx(
        fun(Conn) ->
            _ = elib_pg:execute(
                Conn,
                <<
                    "ALTER TABLE customer_service_event"
                    " DISABLE TRIGGER trg_customer_service_event_append_only"
                >>,
                []
            ),
            lists:foreach(
                fun(Table) ->
                    _ = elib_pg:execute(
                        Conn,
                        <<"DELETE FROM ", Table/binary, " WHERE organization_id = $1">>,
                        [Org]
                    )
                end,
                [
                    <<"customer_service_event">>,
                    %% CS-BE-04：游标行 RESTRICT 引用 session/seat——先删。
                    <<"customer_service_read_cursor">>,
                    <<"customer_service_session">>,
                    <<"customer_service_visit_token">>,
                    <<"customer_service_shop_key">>,
                    <<"customer_service_seat">>
                ]
            ),
            _ = elib_pg:execute(
                Conn,
                <<
                    "ALTER TABLE customer_service_event"
                    " ENABLE TRIGGER trg_customer_service_event_append_only"
                >>,
                []
            ),
            ok
        end,
        [{reraise, false}]
    ),
    ok.

cleanup_sql(Org, Workspace, Actor) ->
    [
        {<<"DELETE FROM enterprise_message_delivery WHERE organization_id = $1 AND workspace_id = $2">>,
            [
                Org, Workspace
            ]},
        {<<"DELETE FROM enterprise_message WHERE organization_id = $1 AND workspace_id = $2">>, [
            Org, Workspace
        ]},
        {<<"DELETE FROM enterprise_conversation WHERE organization_id = $1 AND workspace_id = $2">>,
            [
                Org, Workspace
            ]},
        {<<"DELETE FROM enterprise_contact WHERE organization_id = $1">>, [Org]},
        {
            <<
                "UPDATE organization_business_identity_assignment SET status='ended', ended_at=assigned_at"
                " WHERE organization_id=$1 AND status='active'"
            >>,
            [Org]
        },
        {<<"DELETE FROM organization_member WHERE organization_id=$1 AND user_id=$2">>, [
            Org, Actor
        ]},
        {<<"DELETE FROM organization_business_identity WHERE organization_id=$1">>, [Org]}
    ].

%% 每个语句独立事务 + purge GUC：一条语句失败（如 RESTRICT FK 引用）只影响
%% 它自己，不会让后续清场语句整批停摆。
quiet({Sql, Params}) ->
    _ = elib_pg:with_tx(
        fun(Conn) ->
            _ = epgsql:squery(Conn, <<"SET LOCAL imboy.enterprise_purge = 'on'">>),
            _ = elib_pg:execute(Conn, Sql, Params),
            ok
        end,
        [{reraise, false}]
    ),
    ok.

%% ===================================================================
%% 只读探针 / 通用执行
%% ===================================================================

%% @doc 表名白名单（避免在测试里拼接任意表名）。
-spec table(atom()) -> binary() | {error, unknown_table}.
table(sessions) -> <<"customer_service_session">>;
table(seats) -> <<"customer_service_seat">>;
table(shop_keys) -> <<"customer_service_shop_key">>;
table(visit_tokens) -> <<"customer_service_visit_token">>;
table(events) -> <<"customer_service_event">>;
table(messages) -> <<"enterprise_message">>;
table(_Other) -> {error, unknown_table}.

%% @doc 统计白名单表在 Org 范围内的行数。
-spec count(integer(), atom()) -> integer().
count(Org, Table) ->
    case
        scalar(
            <<"SELECT count(*) AS n FROM ", (table(Table))/binary, " WHERE organization_id=$1">>, [
                Org
            ]
        )
    of
        N when is_integer(N) -> N;
        _ -> -1
    end.

-spec scalar(iodata(), list()) -> term().
scalar(Sql, Params) ->
    scalar(Sql, Params, undefined).

-spec scalar(iodata(), list(), term()) -> term().
scalar(Sql, Params, Default) ->
    case elib_pg:query(Sql, Params) of
        {ok, [Row | _]} ->
            case maps:values(Row) of
                [Value | _] -> Value;
                [] -> Default
            end;
        _ ->
            Default
    end.

-spec exec(iodata(), list()) -> ok | {error, term()}.
exec(Sql, Params) ->
    case elib_pg:execute(Sql, Params) of
        {ok, _Count} -> ok;
        {ok, _Count, _Rows} -> ok;
        {error, Reason} -> {error, Reason}
    end.

-spec tx(fun((term()) -> term())) -> term().
tx(Fun) ->
    elib_pg:with_tx(Fun, [{reraise, false}]).

%% ===================================================================
%% 合成命名
%% ===================================================================

account(Id) ->
    <<?ACCOUNT_PREFIX/binary, (integer_to_binary(Id))/binary>>.

name(Prefix, Id) ->
    <<Prefix/binary, (integer_to_binary(Id))/binary>>.
