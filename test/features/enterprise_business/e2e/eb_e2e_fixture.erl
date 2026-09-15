%%% @doc EB-11 E2E 的**合成夹具**（test-only）：两个 Org、各自默认 Workspace、A/B/X/Y 用户与真 JWT。
%%%
%%% 依据：`agents/a5/EB-11/ORDER.md` §1（两 Org 及各自默认 Workspace、A/B、旧 JWT）、
%%% plan §2.1 #1..#8、EB-11-A01/A02/A05。
%%%
%%% 隔离策略（与 EB-03 的 `eb_pg_test_fixture` 同规）：
%%%   * 每次 `seed/0` 生成**全新随机 TSID** 的 Org / Workspace / user；所有名字带
%%%     `eb11-<run_token>-` 前缀，使证据可归因；
%%%   * **不 TRUNCATE、不删别人的行**、不触共享 `imboy_v1`；清场只针对本 run 的合成行；
%%%   * 只种入「企业侧之外」的基础数据（user / organization / workspace / member）——
%%%     企业侧资源一律经**真实企业路径**创建（HTTP 或 facade），不在夹具里伪造。
%%%
%%% 如实声明：`organization` 行由 113 的同步触发器自动建立 owner 成员行（role=owner），
%%% 因此 owner 的治理角色来自**真实 role 值**，不是夹具编造的。
%%%
%%% 清场是 best-effort：`enterprise_audit_event` / `enterprise_retention_policy` /
%%% `enterprise_retention_hold` 在 schema 上是 append-only 或不可变事实（DELETE 一律
%%% 23514，无 GUC 旁路），其行与被其 RESTRICT FK 钉住的 organization/workspace 行
%%% **按设计不可删除**。该残留由 A05 的原始输出逐条登记，不隐藏、不假称 0。
-module(eb_e2e_fixture).

-export([seed/0, tokens/1, bootstrap_identity/4, cleanup/1, personal_tables_snapshot/1]).

%% ===================================================================
%% 合成租户
%% ===================================================================

%% @doc 种入两 Org 的基础数据，返回作用域 map。
%%
%%   Org1（默认 Workspace = Ws1，另有一个同 Org 的 Ws1b 用于跨 Workspace 负例）
%%     Owner1 = Org owner（治理角色来自真实 role）
%%     A      = Org1 成员（被绑定业务身份、随后被 suspend / 交接）
%%     B      = Org1 成员（承接人）
%%   Org2（默认 Workspace = Ws2）
%%     Owner2 = Org owner
%%     X      = Org2 成员（并发交接场景的 leaver）
%%     Y      = Org2 成员（并发交接场景的 successor）
-spec seed() -> map().
seed() ->
    Owner1 = eb_e2e_lib:id(),
    Ws1 = eb_e2e_lib:id(),
    Ws1b = eb_e2e_lib:id(),
    A = eb_e2e_lib:id(),
    B = eb_e2e_lib:id(),
    Owner2 = eb_e2e_lib:id(),
    Ws2 = eb_e2e_lib:id(),
    X = eb_e2e_lib:id(),
    Y = eb_e2e_lib:id(),
    Org1 = eb_e2e_lib:id(),
    Org2 = eb_e2e_lib:id(),
    ok = insert_users([Owner1, A, B, Owner2, X, Y]),
    ok = insert_org(Org1, Owner1),
    ok = insert_org(Org2, Owner2),
    ok = insert_workspace(Ws1, Org1, Owner1),
    ok = insert_workspace(Ws1b, Org1, Owner1),
    ok = insert_workspace(Ws2, Org2, Owner2),
    ok = insert_member(Org1, A),
    ok = insert_member(Org1, B),
    ok = insert_member(Org2, X),
    ok = insert_member(Org2, Y),
    #{
        org1 => Org1,
        ws1 => Ws1,
        ws1b => Ws1b,
        owner1 => Owner1,
        a_user => A,
        b_user => B,
        org2 => Org2,
        ws2 => Ws2,
        owner2 => Owner2,
        x_user => X,
        y_user => Y,
        tokens => tokens([{owner1, Owner1}, {a, A}, {b, B}, {owner2, Owner2}, {x, X}, {y, Y}])
    }.

%% @doc 真 JWT（`token_ds:encrypt_token/1`，HS256 + 配置密钥；`did` 为空 ⇒ legacy 形状）。
%%
%% 口径：token 由**生产同一签发函数**产出、由**生产中同一中间件**（`auth_ds:verify_token`）
%% 校验；`did` 为空意味着不触发「设备吊销即失效」分支（该分支属设备域，不在企业范围）。
-spec tokens([{atom(), integer()}]) -> #{atom() => binary()}.
tokens(Pairs) ->
    maps:from_list([
        {Name, token_ds:encrypt_token(UserId)}
     || {Name, UserId} <- Pairs
    ]).

insert_users(Ids) ->
    lists:foreach(
        fun(Id) ->
            ok = eb_e2e_lib:exec(
                <<
                    "INSERT INTO \"user\"(id,password,account,reg_ip,reg_cosv)"
                    " VALUES ($1,'x',$2,'127.0.0.1','x')"
                >>,
                [Id, account(Id)]
            )
        end,
        Ids
    ),
    ok.

insert_org(OrgId, OwnerId) ->
    ok = eb_e2e_lib:exec(
        <<"INSERT INTO organization(id,name,owner_id,status) VALUES ($1,$2,$3,'active')">>,
        [OrgId, name(<<"org-">>, OrgId), OwnerId]
    ),
    ok.

insert_workspace(WsId, OrgId, OwnerId) ->
    ok = eb_e2e_lib:exec(
        <<
            "INSERT INTO workspace(id,name,owner_id,status,type,organization_id)"
            " VALUES ($1,$2,$3,'active','project',$4)"
        >>,
        [WsId, name(<<"ws-">>, WsId), OwnerId, OrgId]
    ),
    ok.

insert_member(OrgId, UserId) ->
    ok = eb_e2e_lib:exec(
        <<
            "INSERT INTO organization_member(organization_id,user_id,role,status)"
            " VALUES ($1,$2,'member','active')"
        >>,
        [OrgId, UserId]
    ),
    ok.

account(Id) ->
    <<"eb11-", (eb_e2e_lib:run_token())/binary, "-", (integer_to_binary(Id))/binary>>.

%% ===================================================================
%% 引导：第一个业务身份 + 经办关系（**测试侧**，见 EB-11-FND-1）
%% ===================================================================

%% @doc 直接落库建立「Org1 的第一个 sales 身份 + 绑定 A」。
%%
%% 为什么必须由夹具做（如实登记为 findings[EB-11-FND-1]）：动作表把
%% `POST /business-identities` 登记为 `enterprise_member` + `required_function=sales` +
%% `org.manage`，而 `eb_auth_app:enterprise_member/3` 要求请求者**已有**一条 active
%% 经办关系 ⇒ 空 Org 下第一个身份在生产装配里**无法经 HTTP 创建**（自举死锁）。
%% 与 EB-03/06/08 的既有夹具同规：夹具只负责打破自举，企业侧资源仍由真实路径创建。
%% 本函数的 SQL 与 `eb_pg_test_fixture:new_scope/1` 的身份/经办插入逐字同形。
-spec bootstrap_identity(integer(), integer(), integer(), binary()) -> {integer(), integer()}.
bootstrap_identity(Org, Assignee, Owner, FunctionKey) ->
    IdentityId = eb_e2e_lib:id(),
    AssignmentId = eb_e2e_lib:id(),
    ok = eb_e2e_lib:exec(
        <<
            "INSERT INTO organization_business_identity"
            " (id,organization_id,function_key,display_name,status,version,created_by_user_id)"
            " VALUES ($1,$2,$3,$4,'active',1,$5)"
        >>,
        [IdentityId, Org, FunctionKey, name(<<"bootstrap-identity-">>, IdentityId), Owner]
    ),
    ok = eb_e2e_lib:exec(
        <<
            "INSERT INTO organization_business_identity_assignment"
            " (id,organization_id,business_identity_id,function_key,user_id,status,assigned_by,version)"
            " VALUES ($1,$2,$3,$4,$5,'active',$6,1)"
        >>,
        [AssignmentId, Org, IdentityId, FunctionKey, Assignee, Owner]
    ),
    {IdentityId, AssignmentId}.

name(Prefix, Id) ->
    <<"eb11-", Prefix/binary, (eb_e2e_lib:run_token())/binary, "-",
        (integer_to_binary(Id))/binary>>.

%% ===================================================================
%% 清场（best-effort，逐语句记账）
%% ===================================================================

%% @doc 尽力移除本 run 的合成行；返回**原始结果**列表（A05 的证据输入）。
%%
%% 顺序遵守 RESTRICT FK：子表 → 父表。每条语句独立事务 + purge GUC，一条失败不影响
%% 其余语句（append-only 事实与受其 RESTRICT FK 钉住的行会如实失败并列在结果里）。
-spec cleanup(map()) -> [{binary(), ok | {error, term()}}].
cleanup(Scope) ->
    Org1 = maps:get(org1, Scope),
    Ws1 = maps:get(ws1, Scope),
    Ws1b = maps:get(ws1b, Scope),
    Org2 = maps:get(org2, Scope),
    Ws2 = maps:get(ws2, Scope),
    Statements = [
        {"del_asset",
            <<"DELETE FROM enterprise_asset WHERE organization_id=$1 AND workspace_id=$2">>, [
                Org1, Ws1
            ]},
        {"del_asset_b",
            <<"DELETE FROM enterprise_asset WHERE organization_id=$1 AND workspace_id=$2">>, [
                Org1, Ws1b
            ]},
        {"del_asset_o2",
            <<"DELETE FROM enterprise_asset WHERE organization_id=$1 AND workspace_id=$2">>, [
                Org2, Ws2
            ]},
        {"del_delivery",
            <<"DELETE FROM enterprise_message_delivery WHERE organization_id=$1 AND workspace_id=$2">>,
            [Org1, Ws1]},
        {"del_delivery_o2",
            <<"DELETE FROM enterprise_message_delivery WHERE organization_id=$1 AND workspace_id=$2">>,
            [Org2, Ws2]},
        {"del_note", <<"DELETE FROM enterprise_note WHERE organization_id=$1">>, [Org1]},
        {"del_contact_assignment",
            <<"DELETE FROM enterprise_contact_assignment WHERE organization_id=$1">>, [Org1]},
        {"del_message",
            <<"DELETE FROM enterprise_message WHERE organization_id=$1 AND workspace_id=$2">>, [
                Org1, Ws1
            ]},
        {"del_message_o2",
            <<"DELETE FROM enterprise_message WHERE organization_id=$1 AND workspace_id=$2">>, [
                Org2, Ws2
            ]},
        {"del_conversation",
            <<"DELETE FROM enterprise_conversation WHERE organization_id=$1 AND workspace_id=$2">>,
            [Org1, Ws1]},
        {"del_offboarding_item",
            <<"DELETE FROM enterprise_offboarding_item WHERE organization_id=$1">>, [Org1]},
        {"del_offboarding_item_o2",
            <<"DELETE FROM enterprise_offboarding_item WHERE organization_id=$1">>, [Org2]},
        {"del_offboarding_case",
            <<"DELETE FROM enterprise_offboarding_case WHERE organization_id=$1">>, [Org1]},
        {"del_offboarding_case_o2",
            <<"DELETE FROM enterprise_offboarding_case WHERE organization_id=$1">>, [Org2]},
        {"del_contact_identity",
            <<"DELETE FROM enterprise_contact_identity WHERE organization_id=$1">>, [Org1]},
        {"del_contact", <<"DELETE FROM enterprise_contact WHERE organization_id=$1">>, [Org1]},
        {"end_assignments",
            <<
                "UPDATE organization_business_identity_assignment SET status='ended', ended_at=assigned_at"
                " WHERE organization_id=$1 AND status='active'"
            >>,
            [Org1]},
        {"end_assignments_o2",
            <<
                "UPDATE organization_business_identity_assignment SET status='ended', ended_at=assigned_at"
                " WHERE organization_id=$1 AND status='active'"
            >>,
            [Org2]},
        {"del_members_non_owner",
            <<"DELETE FROM organization_member WHERE organization_id=$1 AND role <> 'owner'">>, [
                Org1
            ]},
        {"del_members_non_owner_o2",
            <<"DELETE FROM organization_member WHERE organization_id=$1 AND role <> 'owner'">>, [
                Org2
            ]},
        {"del_assignments",
            <<"DELETE FROM organization_business_identity_assignment WHERE organization_id=$1">>, [
                Org1
            ]},
        {"del_assignments_o2",
            <<"DELETE FROM organization_business_identity_assignment WHERE organization_id=$1">>, [
                Org2
            ]},
        {"del_identities",
            <<"DELETE FROM organization_business_identity WHERE organization_id=$1">>, [Org1]},
        {"del_identities_o2",
            <<"DELETE FROM organization_business_identity WHERE organization_id=$1">>, [Org2]},
        {"del_workspaces", <<"DELETE FROM workspace WHERE organization_id=$1">>, [Org1]},
        {"del_workspaces_o2", <<"DELETE FROM workspace WHERE organization_id=$1">>, [Org2]},
        {"del_orgs", <<"DELETE FROM organization WHERE id=$1">>, [Org1]},
        {"del_orgs_o2", <<"DELETE FROM organization WHERE id=$1">>, [Org2]}
    ],
    [{Label, quiet_tx(Sql, Params)} || {Label, Sql, Params} <- Statements].

quiet_tx(Sql, Params) ->
    Result = elib_pg:with_tx(
        fun(Conn) ->
            _ = epgsql:squery(Conn, <<"SET LOCAL imboy.enterprise_purge = 'on'">>),
            case elib_pg:execute(Conn, Sql, Params) of
                {ok, _Count} -> ok;
                {ok, _Count, _Rows} -> ok;
                {error, Reason} -> {error, Reason}
            end
        end,
        [{reraise, false}]
    ),
    case Result of
        ok -> ok;
        {rollback, Reason} -> {error, Reason};
        {error, _} = Err -> Err;
        _Other -> ok
    end.

%% ===================================================================
%% 个人表快照（A02：企业动作不得产生个人行）
%% ===================================================================

%% @doc 对若干**个人域**表，统计「行的文本形态里出现任一合成 user id」的行数。
%%
%% 只读、幂等；表不存在（`to_regclass` 为空）时返回 `absent`。
-spec personal_tables_snapshot([integer()]) -> [{atom(), non_neg_integer() | absent}].
personal_tables_snapshot(UserIds) ->
    Tables = [friend, conversation, msg_c2c, attachment, user_collect, user_device],
    [{Table, personal_hits(Table, UserIds)} || Table <- Tables].

personal_hits(Table, UserIds) ->
    Name = atom_to_binary(Table, utf8),
    case
        eb_e2e_lib:scalar(
            <<"SELECT to_regclass($1) IS NOT NULL AS present">>,
            [<<"public.", Name/binary>>],
            false
        )
    of
        true ->
            %% id 是 18~19 位数字，用正则择一匹配整行文本（无 PII、无注入面：纯数字 + 固定操作符）。
            Regex = iolist_to_binary([
                "(",
                lists:join(<<"|">>, [integer_to_binary(Id) || Id <- UserIds]),
                ")"
            ]),
            Sql = iolist_to_binary([
                "SELECT count(*) AS n FROM ", Name, " t WHERE t::text ~ $1"
            ]),
            case eb_e2e_lib:scalar(Sql, [Regex], -1) of
                N when is_integer(N) -> N;
                _Other -> -1
            end;
        false ->
            absent
    end.
