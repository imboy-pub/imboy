%%% @doc CS-01 触库套件的合成租户夹具（test-only；镜像 `eb_pg_test_fixture` 的
%%% 带 VM 盐的合成主键 scope 模式）。
%%%
%%% 隔离策略：每次 `new_scope/1` 都生成合成主键的 Org/Workspace/User，
%%% 须使用隔离 marker 数据库；不做 TRUNCATE、不写共享库、不删别人的行。合成数据只含
%%% 合成主键与 `cs01-` 前缀合成文本，无真实联系方式/客户资料/生产资源。
%%%
%%% 清场：customer_service 表先删（事件 → 会话 → token/key → seat），再清
%%% enterprise 侧被引用行；清场失败显式冒泡，ID 复用 eb_pg_test_fixture 的有限盐方案，非永久唯一保证。
-module(cs_pg_test_fixture).

-export([
    id/0,
    new_scope/0,
    new_scope/1,
    cleanup/1,
    cleanup_scope/1,
    cleanup_plan/1,
    checked_cleanup_result/2,
    exec/2,
    tx/1,
    scalar/2,
    scalar/3,
    count/2,
    table/1,
    key_ref/0,
    install_keyring/0,
    restore_keyring/1
]).

-define(ACCOUNT_PREFIX, <<"cs01-account-">>).
-define(NAME_PREFIX, <<"cs01-name-">>).

%% @doc 生成合成 fixture 主键，不依赖生产实体 TSID 游标。
-spec id() -> integer().
id() ->
    eb_pg_test_fixture:id().

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
%% 清场：完整合成 scope 身份核验后，在同一事务内精确删除。
%% ===================================================================

-spec cleanup(map()) -> ok.
cleanup(Scope) ->
    ok = cleanup_scope(Scope),
    ok = elib_pg:with_tx(
        fun(Conn) ->
            ok = verify_cleanup_scope(Conn, Scope),
            lists:foreach(
                fun({Sql, Params}) ->
                    ok = checked_cleanup_result(Sql, elib_pg:execute(Conn, Sql, Params))
                end,
                cleanup_plan(Scope)
            ),
            ok
        end,
        [{reraise, true}]
    ),
    ok.

cleanup_scope(Scope) when is_map(Scope) ->
    Keys = [
        owner_user_id,
        actor_user_id,
        peer_user_id,
        org_id,
        workspace_id,
        other_org_id,
        other_workspace_id
    ],
    Values = [maps:get(Key, Scope, undefined) || Key <- Keys],
    case
        lists:all(fun(V) -> is_integer(V) andalso V > 0 end, Values) andalso
            length(lists:usort(Values)) =:= length(Keys)
    of
        true -> ok;
        false -> error({invalid_cs_cleanup_scope, Keys})
    end;
cleanup_scope(_) ->
    error(invalid_cs_cleanup_scope).

verify_cleanup_scope(Conn, S) ->
    Owner = maps:get(owner_user_id, S),
    lists:foreach(
        fun(Key) ->
            Id = maps:get(Key, S),
            {ok, _, [{Id}]} = epgsql:equery(
                Conn,
                <<"SELECT id FROM \"user\" WHERE id=$1 AND account=$2 FOR UPDATE">>,
                [Id, account(Id)]
            )
        end,
        [owner_user_id, actor_user_id, peer_user_id]
    ),
    lists:foreach(
        fun({OrgKey, WsKey}) ->
            Org = maps:get(OrgKey, S),
            Ws = maps:get(WsKey, S),
            {ok, _, [{Org}]} = epgsql:equery(
                Conn,
                <<"SELECT id FROM organization WHERE id=$1 AND name=$2 AND owner_id=$3 FOR UPDATE">>,
                [Org, name(<<"cs01-org-">>, Org), Owner]
            ),
            {ok, _, [{Ws}]} = epgsql:equery(
                Conn,
                <<"SELECT id FROM workspace WHERE id=$1 AND name=$2 AND organization_id=$3 AND owner_id=$4 FOR UPDATE">>,
                [Ws, name(<<"cs01-ws-">>, Ws), Org, Owner]
            )
        end,
        [{org_id, workspace_id}, {other_org_id, other_workspace_id}]
    ),
    ok.

%% Only named, existing synthetic-fixture guard triggers are disabled. PostgreSQL
%% transactional DDL restores them on any SQL error; successful cleanup re-enables
%% them before COMMIT. Neither FK triggers nor global replication modes are altered.
cleanup_plan(S) ->
    ok = cleanup_scope(S),
    Orgs = [maps:get(org_id, S), maps:get(other_org_id, S)],
    Triggers = [
        {<<"customer_service_event">>, <<"trg_customer_service_event_append_only">>},
        {<<"enterprise_audit_event">>, <<"trg_enterprise_audit_event_append_only">>},
        {<<"enterprise_message">>, <<"trg_enterprise_message_purge_guard">>},
        {<<"enterprise_retention_hold">>, <<"trg_enterprise_retention_hold_append_only">>},
        {<<"enterprise_retention_policy">>, <<"trg_enterprise_retention_policy_immutable">>}
    ],
    Guards = [
        {
            <<"DO $$ BEGIN IF NOT EXISTS (SELECT 1 FROM pg_trigger WHERE tgrelid='", Table/binary,
                "'::regclass AND tgname='", Trigger/binary,
                "' AND tgenabled='O') THEN RAISE EXCEPTION 'fixture trigger state mismatch'; END IF; END $$">>,
            []
        }
     || {Table, Trigger} <- Triggers
    ],
    Guards ++ trigger_sql(Triggers, <<"DISABLE">>) ++
        [{<<"SET LOCAL imboy.ai_trace_purge = 'on'">>, []}] ++
        [
            {<<"DELETE FROM ", Table/binary, " WHERE organization_id IN ($1,$2)">>, Orgs}
         || Table <- cleanup_tables()
        ] ++
        parent_cleanup_sql(S) ++ trigger_sql(Triggers, <<"ENABLE">>).

trigger_sql(Triggers, Action) ->
    [
        {<<"ALTER TABLE ", Table/binary, " ", Action/binary, " TRIGGER ", Trigger/binary>>, []}
     || {Table, Trigger} <- Triggers
    ].

cleanup_tables() ->
    [
        <<"customer_service_ai_budget_attempt">>,
        <<"customer_service_ai_consume_seen">>,
        <<"customer_service_ai_consume_cursor">>,
        <<"customer_service_ai_delivery">>,
        <<"customer_service_ai_job">>,
        <<"customer_service_ai_turn">>,
        <<"customer_service_ai_session">>,
        <<"customer_service_ai_budget">>,
        <<"customer_service_event">>,
        <<"customer_service_visitor_message_read">>,
        <<"customer_service_visitor_read_cursor">>,
        <<"customer_service_read_cursor">>,
        <<"customer_service_session">>,
        <<"customer_service_visit_token">>,
        <<"customer_service_widget_nonce">>,
        <<"customer_service_widget_identity_key">>,
        <<"customer_service_widget_installation">>,
        <<"customer_service_shop_key">>,
        <<"customer_service_seat_presence">>,
        <<"customer_service_seat_console">>,
        <<"customer_service_seat_limit">>,
        <<"customer_service_seat">>,
        <<"customer_service_ai_agent">>,
        <<"customer_service_ai_canary">>,
        <<"customer_service_faq">>,
        <<"enterprise_retention_hold">>,
        <<"enterprise_message_delivery">>,
        <<"enterprise_message">>,
        <<"enterprise_conversation">>,
        <<"enterprise_contact_identity">>,
        <<"enterprise_contact_assignment">>,
        <<"enterprise_note">>,
        <<"enterprise_contact">>,
        <<"enterprise_audit_event">>,
        <<"organization_business_identity_assignment">>,
        <<"organization_business_identity">>,
        <<"organization_member">>,
        <<"enterprise_retention_policy">>
    ].

parent_cleanup_sql(S) ->
    Owner = maps:get(owner_user_id, S),
    lists:append([
        [
            {<<"DELETE FROM workspace WHERE id=$1 AND organization_id=$2 AND owner_id=$3">>, [
                maps:get(WsKey, S), maps:get(OrgKey, S), Owner
            ]},
            {<<"DELETE FROM organization WHERE id=$1 AND owner_id=$2">>, [
                maps:get(OrgKey, S), Owner
            ]}
        ]
     || {OrgKey, WsKey} <- [{org_id, workspace_id}, {other_org_id, other_workspace_id}]
    ]) ++
        [
            {<<"DELETE FROM \"user\" WHERE id=$1 AND account=$2">>, [Id, account(Id)]}
         || Id <- [Owner, maps:get(actor_user_id, S), maps:get(peer_user_id, S)]
        ].

checked_cleanup_result(_Sql, {ok, _Count}) -> ok;
checked_cleanup_result(_Sql, {ok, _Count, _Rows}) -> ok;
checked_cleanup_result(Sql, Other) -> error({cs_fixture_cleanup_sql_failed, Sql, Other}).

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

%% 合成投递沿真实服务端密钥装配路径执行，结束后恢复调用前配置。
install_keyring() ->
    Old = application:get_env(imboy, eb_enterprise_keyring),
    Hex = binary:encode_hex(crypto:strong_rand_bytes(32), lowercase),
    ok = application:set_env(
        imboy,
        eb_enterprise_keyring,
        #{active_version => 1, keys => #{1 => Hex}}
    ),
    Old.

restore_keyring({ok, Value}) -> application:set_env(imboy, eb_enterprise_keyring, Value);
restore_keyring(undefined) -> application:unset_env(imboy, eb_enterprise_keyring).
