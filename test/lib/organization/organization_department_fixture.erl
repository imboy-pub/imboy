%%% @doc ORG-04 Department 触库套件的合成租户夹具（test-only）。
%%%
%%% 隔离策略（对齐 eb_pg_test_fixture 口径）：每次 new_scope 生成全新随机 TSID
%%% 的 Org/User/Department，跨套件、跨轮次天然互不可见；不做 TRUNCATE、
%%% 不删别人的行；清场 best-effort、按 FK 安全顺序逐条静默执行。
%%% 合成数据只含随机 TSID 与 `org04-` 前缀合成名，无任何真实账号/联系方式。
-module(organization_department_fixture).

-export([
    id/0,
    new_scope/0,
    new_scope/1,
    cleanup/1,
    exec/2,
    scalar/2,
    scalar/3,
    one_scalar/2
]).

%% ===================================================================
%% 合成 ID
%% ===================================================================

-spec id() -> integer().
id() ->
    try
        elib_tsid:generate()
    catch
        _:_ ->
            1000000000000000 + erlang:unique_integer([positive, monotonic])
    end.

%% ===================================================================
%% 合成租户
%% ===================================================================

%% @doc 缺省 scope：OrgA(owner+memberA+memberB+removedMember) + OrgB(跨 Org 负例，
%% owner+memberX) + OrgA 的一个 Workspace（权限不受扰断言用）。
-spec new_scope() -> map().
new_scope() ->
    new_scope(#{}).

-spec new_scope(map()) -> map().
new_scope(_Opts) ->
    Owner = id(),
    MemberA = id(),
    MemberB = id(),
    Outsider = id(),
    RemovedMember = id(),
    OtherOwner = id(),
    OtherMember = id(),
    Org = id(),
    OtherOrg = id(),
    Workspace = id(),

    ok = exec(
        <<
            "INSERT INTO \"user\"(id,password,account,reg_ip,reg_cosv) VALUES"
            " ($1,'x',$2,'127.0.0.1','x'),($3,'x',$4,'127.0.0.1','x'),"
            " ($5,'x',$6,'127.0.0.1','x'),($7,'x',$8,'127.0.0.1','x'),"
            " ($9,'x',$10,'127.0.0.1','x'),($11,'x',$12,'127.0.0.1','x'),"
            " ($13,'x',$14,'127.0.0.1','x')"
        >>,
        [
            Owner,
            account(Owner),
            MemberA,
            account(MemberA),
            MemberB,
            account(MemberB),
            Outsider,
            account(Outsider),
            RemovedMember,
            account(RemovedMember),
            OtherOwner,
            account(OtherOwner),
            OtherMember,
            account(OtherMember)
        ]
    ),
    ok = exec(
        <<"INSERT INTO organization(id,name,owner_id,status) VALUES ($1,$2,$3,'active')">>,
        [Org, name(<<"org04-org-">>, Org), Owner]
    ),
    ok = exec(
        <<"INSERT INTO organization(id,name,owner_id,status) VALUES ($1,$2,$3,'active')">>,
        [OtherOrg, name(<<"org04-org-">>, OtherOrg), OtherOwner]
    ),
    %% OrgB 的成员（跨 Org 负例主角）
    ok = exec(
        <<
            "INSERT INTO organization_member(organization_id,user_id,role,status)"
            " VALUES ($1,$2,'member','active')"
        >>,
        [OtherOrg, OtherMember]
    ),
    %% OrgA：普通成员、removed 成员（owner 行由 113 触发器自动建立）
    ok = exec(
        <<
            "INSERT INTO organization_member(organization_id,user_id,role,status)"
            " VALUES ($1,$2,'member','active'),($1,$3,'member','active'),"
            " ($1,$4,'member','removed')"
        >>,
        [Org, MemberA, MemberB, RemovedMember]
    ),
    ok = exec(
        <<
            "INSERT INTO workspace(id,name,owner_id,status,type,organization_id)"
            " VALUES ($1,$2,$3,'active','project',$4)"
        >>,
        [Workspace, name(<<"org04-ws-">>, Workspace), Owner, Org]
    ),
    ok = exec(
        <<
            "INSERT INTO workspace_member(workspace_id,user_id,role,status)"
            " VALUES ($1,$2,'owner','active')"
        >>,
        [Workspace, Owner]
    ),
    #{
        org_id => Org,
        other_org_id => OtherOrg,
        workspace_id => Workspace,
        owner_user_id => Owner,
        member_a => MemberA,
        member_b => MemberB,
        outsider => Outsider,
        removed_member => RemovedMember,
        other_owner_user_id => OtherOwner,
        other_member => OtherMember
    }.

%% ===================================================================
%% 清场（best-effort，FK 安全顺序；失败静默——隔离靠唯一 TSID）
%% ===================================================================

-spec cleanup(map()) -> ok.
cleanup(Scope) ->
    Org = maps:get(org_id, Scope, undefined),
    OtherOrg = maps:get(other_org_id, Scope, undefined),
    Workspace = maps:get(workspace_id, Scope, undefined),
    Users = [
        maps:get(K, Scope)
     || K <- [
            owner_user_id,
            member_a,
            member_b,
            outsider,
            removed_member,
            other_owner_user_id,
            other_member
        ],
        maps:is_key(K, Scope)
    ],
    Stmts = lists:flatten([
        [
            {<<"DELETE FROM organization_department_member WHERE organization_id=$1">>, [Org]},
            {<<"DELETE FROM organization_department WHERE organization_id=$1">>, [Org]},
            {<<"DELETE FROM workspace_member WHERE workspace_id=$1">>, [Workspace]},
            {<<"DELETE FROM workspace WHERE id=$1">>, [Workspace]},
            {<<"DELETE FROM organization WHERE id=$1">>, [Org]},
            {<<"DELETE FROM organization WHERE id=$1">>, [OtherOrg]}
        ] ++
            [{<<"DELETE FROM \"user\" WHERE id=$1">>, [U]} || U <- Users]
    ]),
    lists:foreach(
        fun({Sql, Params}) ->
            _ = elib_pg:execute(Sql, Params),
            ok
        end,
        Stmts
    ),
    ok.

%% ===================================================================
%% 通用执行
%% ===================================================================

-spec exec(iodata(), list()) -> ok | {error, term()}.
exec(Sql, Params) ->
    case elib_pg:execute(Sql, Params) of
        {ok, _} -> ok;
        {ok, _, _} -> ok;
        {error, Reason} -> {error, Reason}
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

%% 单行单列显式命名版（避免 maps:values 顺序依赖）
one_scalar(Sql, Params) ->
    case elib_pg:query(Sql, Params) of
        {ok, [Row | _]} -> hd(maps:values(Row));
        {ok, []} -> undefined;
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% 合成命名
%% ===================================================================

account(Id) ->
    <<"org04-account-", (integer_to_binary(Id))/binary>>.

name(Prefix, Id) ->
    <<Prefix/binary, (integer_to_binary(Id))/binary>>.
