%%% @doc EB-03 触库套件的合成租户夹具（test-only，无任何真实账号/数据）。
%%%
%%% 隔离策略（作业书 §1.2）：每次 `new_scope/1` 都生成**全新随机 TSID** 的
%%% Org/Workspace/User，因此不同套件、不同轮次的合成行天然互不可见；
%%% **不做 TRUNCATE、不写共享 imboy_v1**，也从不删别人的行。
%%%
%%% 合成数据只含：随机 TSID、`eb03-...` 前缀的合成 account/display_name、
%%% 合成 consent 主体与 reason code。不含真实联系方式、客户资料或生产资源。
%%%
%%% 清场是 best-effort：`enterprise_retention_hold` / `enterprise_audit_event` /
%%% `enterprise_retention_policy` 在 schema 上是 append-only 或不可变事实，
%%% 其行按设计不可 DELETE，因此清场后仍会保留少量合成行（连同被 RESTRICT FK
%%% 引用的消息行）。这是 EB-01 冻结语义，不是本夹具的缺陷；隔离靠唯一 TSID。
-module(eb_pg_test_fixture).

-export([
    id/0,
    new_scope/0,
    new_scope/1,
    cleanup/1,
    ensure_purge_role/0,
    count/3,
    scalar/2,
    scalar/3,
    exec/2,
    tx/1,
    canary/0,
    canary_absent/1,
    resource_aad/4,
    key_ref/0,
    key_ref/1,
    keyring_ref/1,
    store/0,
    table/1
]).

-define(MASTER_KEY_SOURCE, <<"eb03-synthetic-master-key-v1">>).
-define(CANARY_PREFIX, <<"EB03-CANARY-PLAINTEXT-">>).

%% ===================================================================
%% 合成 ID / 夹具常量
%% ===================================================================

%% @doc 生成一个新的合成 TSID（时间有序、跨进程唯一）。
-spec id() -> integer().
id() ->
    try
        elib_tsid:generate(default)
    catch
        _:_ ->
            %% 纯套件（未启动 imboy app）下的兜底：仍在 TSID 量级之下且分布唯一。
            1000000000000000 + erlang:unique_integer([positive, monotonic])
    end.

%% @doc 可归因的合成明文金丝雀（ASCII，便于在日志/DB 中做子串断言）。
-spec canary() -> binary().
canary() ->
    <<?CANARY_PREFIX/binary, (integer_to_binary(id()))/binary>>.

%% @doc 断言给定二进制中不含金丝雀明文。
-spec canary_absent(binary() | undefined | null) -> boolean().
canary_absent(Bin) when is_binary(Bin) ->
    binary:match(Bin, ?CANARY_PREFIX) =:= nomatch;
canary_absent(_Otherwise) ->
    true.

%% @doc 通用资源作用域（AAD 绑定 OrgId/WorkspaceId/资源），供客户资料等非消息资源使用。
%% 消息路径用 EB-02 冻结的 4 字段 aad()（见 eb_crypto_port）。
-spec resource_aad(binary(), integer(), integer(), integer()) -> map().
resource_aad(ResourceType, OrgId, WorkspaceId, ResourceId) ->
    #{
        organization_id => OrgId,
        workspace_id => WorkspaceId,
        resource_type => ResourceType,
        resource_id => ResourceId
    }.

%% @doc 合成主密钥引用（32 字节随机密钥 + 声明版本）。测试专用，非生产密钥。
-spec key_ref() -> map().
key_ref() ->
    key_ref(1).

-spec key_ref(pos_integer()) -> map().
key_ref(Version) ->
    #{key => crypto:strong_rand_bytes(32), key_version => Version}.

%% @doc CSB-02S：生产 store facade（DB 型用例直读原始行做保真性自证用）。
-spec store() -> module().
store() ->
    eb_infra_ports:store().

%% ===================================================================
%% 合成租户
%% ===================================================================

-spec new_scope() -> map().
new_scope() ->
    new_scope(#{}).

%% @doc 建立一套完整合成租户：同 Org 的 Workspace + 另一个 Org/Workspace（跨租户负例用）、
%% 3 个合成 user、sales/customer_service 两个 identity 与一条 active assignment、
%% 一个 contact、一个带合成 consent 的 conversation，以及一条 retention policy。
%%
%% Opts：
%%   with_policy  => boolean()（默认 true）不建 policy，用于「缺策略 fail-closed」
%%   with_consent => boolean()（默认 true）不写 consent 字段，用于 consent 门禁负例
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
        Org, name(<<"eb03-org-">>, Org), Owner
    ]),
    ok = exec(
        <<
            "INSERT INTO workspace(id,name,owner_id,status,type,organization_id)"
            " VALUES ($1,$2,$3,'active','project',$4)"
        >>,
        [Workspace, name(<<"eb03-ws-">>, Workspace), Owner, Org]
    ),
    %% 跨租户负例：同 owner、另一个 Organization 及其 Workspace。
    ok = exec(<<"INSERT INTO organization(id,name,owner_id,status) VALUES ($1,$2,$3,'active')">>, [
        OtherOrg, name(<<"eb03-org-">>, OtherOrg), Owner
    ]),
    ok = exec(
        <<
            "INSERT INTO workspace(id,name,owner_id,status,type,organization_id)"
            " VALUES ($1,$2,$3,'active','project',$4)"
        >>,
        [OtherWorkspace, name(<<"eb03-ws-">>, OtherWorkspace), Owner, OtherOrg]
    ),
    %% organization_member：owner 行由 113 的同步触发器建立，这里只补经办人所在成员行。
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
            name(<<"eb03-sales-">>, Sales),
            Owner,
            Service,
            name(<<"eb03-service-">>, Service)
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
        [Contact, Org, name(<<"eb03-contact-">>, Contact), Sales]
    ),
    ok = insert_conversation(
        Conversation, Org, Workspace, Contact, Sales, maps:get(with_consent, Opts, true)
    ),
    ok =
        case maps:get(with_policy, Opts, true) of
            true ->
                exec(
                    <<
                        "INSERT INTO enterprise_retention_policy"
                        " (id,organization_id,workspace_id,data_class,version,retention_days,"
                        "  trigger_event,created_by_user_id)"
                        " VALUES ($1,$2,$3,'enterprise_message',1,1095,'message.accept',$4)"
                    >>,
                    [Policy, Org, Workspace, Owner]
                );
            false ->
                ok
        end,
    ScopeKey = crypto:strong_rand_bytes(32),
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
        %% scope 级消息密钥（eb06 治根）：同 scope 写入统一用这一把、读取经
        %% keyring_ref/1 显式回传同一把 —— 写读闭环不再依赖
        %% IMBOY_EB_ENTERPRISE_KEYRING_FILE 是否导出（历史行为：keyring 缺席
        %% 时读面走密文投影、断言退化为纯结构检查 = 假绿；keyring 在场时随机
        %% 单 key 密文必 authentication_failed = 单跑 3 FAIL）。
        scope_key => ScopeKey
    }.

%% @doc scope 级 keyring 形态 key_ref：写入（seal）与读取（list 显式 key_ref）
%% 都用它，同 scope 内任意多行互相可解；keys map 形态走 resolve_keyring，
%% 与生产 keyring 的解析路径同构。
-spec keyring_ref(map()) -> map().
keyring_ref(Scope) ->
    Key = maps:get(scope_key, Scope),
    #{key => Key, key_version => 1, keys => #{1 => Key}}.

insert_conversation(Conversation, Org, Workspace, Contact, Sales, WithConsent) ->
    case WithConsent of
        true ->
            exec(
                <<
                    "INSERT INTO enterprise_conversation"
                    " (id,organization_id,workspace_id,contact_id,business_identity_id,status,version,"
                    "  notice_version,consent_at,consent_subject,consent_evidence_kind)"
                    " VALUES ($1,$2,$3,$4,$5,'active',1,'eb03-notice-v1',CURRENT_TIMESTAMP,$6,'synthetic')"
                >>,
                [
                    Conversation,
                    Org,
                    Workspace,
                    Contact,
                    Sales,
                    name(<<"eb03-consent-">>, Conversation)
                ]
            );
        false ->
            exec(
                <<
                    "INSERT INTO enterprise_conversation"
                    " (id,organization_id,workspace_id,contact_id,business_identity_id,status,version)"
                    " VALUES ($1,$2,$3,$4,$5,'active',1)"
                >>,
                [Conversation, Org, Workspace, Contact, Sales]
            )
    end.

%% ===================================================================
%% 清场（best-effort）
%% ===================================================================

%% @doc 尽力移除本次 scope 的合成行。append-only / 不可变事实（hold / policy /
%% audit）按 schema 不可 DELETE，其行与受其 RESTRICT FK 保护的消息行会保留；
%% 由于 ID 全局唯一，残留不影响其它 scope。
-spec cleanup(map()) -> ok.
cleanup(Scope) ->
    Org = maps:get(org_id, Scope, undefined),
    Workspace = maps:get(workspace_id, Scope, undefined),
    Actor = maps:get(actor_user_id, Scope, undefined),
    case {is_integer(Org), is_integer(Workspace)} of
        {true, true} ->
            lists:foreach(fun(Sql) -> _ = quiet_tx(Sql) end, cleanup_sql(Org, Workspace, Actor)),
            ok;
        _ ->
            ok
    end.

%% 每个语句独立事务 + purge GUC：一条语句失败（如 append-only 事实的 RESTRICT 引用）
%% 只影响它自己，不会让后续清场语句整批停摆。
cleanup_sql(Org, Workspace, Actor) ->
    [
        {<<"DELETE FROM enterprise_asset WHERE organization_id=$1 AND workspace_id=$2">>, [
            Org, Workspace
        ]},
        {<<"DELETE FROM enterprise_message_delivery WHERE organization_id=$1 AND workspace_id=$2">>,
            [
                Org, Workspace
            ]},
        {<<"DELETE FROM enterprise_note WHERE organization_id=$1">>, [Org]},
        {<<"DELETE FROM enterprise_contact_assignment WHERE organization_id=$1">>, [Org]},
        {<<"DELETE FROM enterprise_message WHERE organization_id=$1 AND workspace_id=$2">>, [
            Org, Workspace
        ]},
        {<<"DELETE FROM enterprise_conversation WHERE organization_id=$1 AND workspace_id=$2">>, [
            Org, Workspace
        ]},
        {<<"DELETE FROM enterprise_contact_identity WHERE organization_id=$1">>, [Org]},
        {<<"DELETE FROM enterprise_contact WHERE organization_id=$1">>, [Org]},
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

quiet_tx({Sql, Params}) ->
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
%% purge worker 角色（cluster 级对象，迁移刻意不 CREATE ROLE）
%% ===================================================================

%% @doc 确保 cluster 角色 `imboy_enterprise_purge_worker` 存在（幂等）。
%%
%% EB-01 的迁移明确注释「本迁移不 CREATE ROLE，由部署/DBA 预置」；scratch 库里
%% 由本夹具按同样口径补齐，使 bounded purge 的正向路径可被验证。仅创建名字与
%% 一个 NOLOGIN 角色，不授予任何表权限。
-spec ensure_purge_role() -> ok | {error, term()}.
ensure_purge_role() ->
    case
        elib_pg:query(
            <<"SELECT (to_regrole('imboy_enterprise_purge_worker') IS NULL) AS missing">>, []
        )
    of
        {ok, [#{<<"missing">> := true}]} ->
            case elib_pg:execute(<<"CREATE ROLE imboy_enterprise_purge_worker NOLOGIN">>, []) of
                {ok, _} -> ok;
                {error, Reason} -> {error, {create_purge_role_failed, Reason}}
            end;
        {ok, _} ->
            ok;
        {error, Reason} ->
            {error, {regrole_probe_failed, Reason}}
    end.

%% ===================================================================
%% 只读探针 / 通用执行
%% ===================================================================

%% @doc 表名白名单（避免在测试里拼接任意表名）。
-spec table(atom()) -> binary() | {error, unknown_table}.
table(messages) -> <<"enterprise_message">>;
table(deliveries) -> <<"enterprise_message_delivery">>;
table(assets) -> <<"enterprise_asset">>;
table(audits) -> <<"enterprise_audit_event">>;
table(holds) -> <<"enterprise_retention_hold">>;
table(policies) -> <<"enterprise_retention_policy">>;
table(conversations) -> <<"enterprise_conversation">>;
table(contacts) -> <<"enterprise_contact">>;
table(contact_identities) -> <<"enterprise_contact_identity">>;
table(identities) -> <<"organization_business_identity">>;
table(assignments) -> <<"organization_business_identity_assignment">>;
table(_Other) -> {error, unknown_table}.

%% @doc 统计某张白名单表在 (Org, Workspace) 范围内的行数（无 workspace 列的表用 Org）。
-spec count(integer(), integer(), atom()) -> integer().
count(Org, Workspace, Table) ->
    Sql =
        case has_workspace_column(Table) of
            true ->
                <<"SELECT count(*) AS n FROM ", (table(Table))/binary,
                    " WHERE organization_id=$1 AND workspace_id=$2">>;
            false ->
                <<"SELECT count(*) AS n FROM ", (table(Table))/binary, " WHERE organization_id=$1">>
        end,
    Params =
        case has_workspace_column(Table) of
            true -> [Org, Workspace];
            false -> [Org]
        end,
    case scalar(Sql, Params) of
        N when is_integer(N) -> N;
        _ -> -1
    end.

%% 这些表按 EB-01 的 schema 没有 workspace_id 列（Org 级），其余企业表均显式带 Workspace。
has_workspace_column(Table) ->
    not lists:member(Table, [
        contacts, contact_identities, identities, assignments, audits
    ]).

%% @doc 取单值（无参数）。
-spec scalar(iodata(), list()) -> term().
scalar(Sql, Params) ->
    scalar(Sql, Params, undefined).

%% @doc 取单值（默认值兜底）。
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
    <<"eb03-account-", (integer_to_binary(Id))/binary>>.

name(Prefix, Id) ->
    <<Prefix/binary, (integer_to_binary(Id))/binary>>.
