%%% @doc Human Organization Directory 触库套件的合成租户夹具（test-only）。
%%%
%%% 隔离策略（对齐 organization_department_fixture 口径）：new_scope 生成
%%% 全新随机 TSID 的 Org/User/Department，跨套件、跨轮次天然互不可见；
%%% 不做 TRUNCATE、不删别人的行；清场 best-effort、按 FK 安全顺序静默执行。
%%% 合成数据只含随机 TSID 与 `orgdir-` 前缀合成名 + 随机 token，无任何
%%% 真实账号/联系方式。
%%%
%%% 场景矩阵（§14.2 行为合同的负例/正例原料）：
%%%   * 主 Org：active；root_a(active 根，名含 token) / root_b(archived 根)
%%%     / child_a1+child_a2(root_a 的 active 子部门) / search_dept(根级，
%%%     名含 token)。
%%%   * 成员：owner(根成员) / m_root(无部门，email+mobile 含 token——
%%%     U-02「不搜邮箱/手机号」负例原料) / m_arch_only(空昵称+只挂
%%%     archived 部门→根成员；display_name 回落 account 原料)
%%%     / m_child(挂 child_a1) / m_dual(挂 child_a1+child_a2)
%%%     / m_removed(先挂 child_a2 再 removed)
%%%     / m_suspended(先挂 child_a2 再 suspended)
%%%     / m_search(nickname 含 token)。
%%%   * 次 Org：active，含 token 昵称成员 + token 名部门（跨 Org 隔离原料）。
%%%   * 归档 Org：archived（403 organization_disabled 原料）。
-module(organization_directory_fixture).

-export([
    id/0,
    token/0,
    new_scope/0,
    cleanup/1,
    exec/2,
    scalar/2,
    scalar/3
]).

%% ===================================================================
%% 合成 ID / token
%% ===================================================================

-spec id() -> integer().
id() ->
    try
        elib_tsid:generate()
    catch
        _:_ ->
            2000000000000000 + erlang:unique_integer([positive, monotonic])
    end.

%% 检索断言专用随机 token（只命中本 scope 的行）。
-spec token() -> binary().
token() ->
    <<"orgdir", (integer_to_binary(id() rem 100000000))/binary>>.

%% ===================================================================
%% 合成租户
%% ===================================================================

-spec new_scope() -> map().
new_scope() ->
    Tok = token(),
    Owner = id(),
    MRoot = id(),
    MArchOnly = id(),
    MChild = id(),
    MDual = id(),
    MRemoved = id(),
    MSuspended = id(),
    MSearch = id(),
    OtherOwner = id(),
    OtherMember = id(),
    Org = id(),
    ArchivedOrg = id(),
    OtherOrg = id(),
    RootA = id(),
    RootB = id(),
    ChildA1 = id(),
    ChildA2 = id(),
    SearchDept = id(),
    OtherOrgDept = id(),

    %% —— 用户（逐行插入，避免多行 VALUES 参数错位）——
    %% 命名约定：仅 m_search(nickname) / m_dual(account) / other_member
    %% （nickname，次 Org）含 token，其余昵称/账号不含——search 断言据此
    %% 精确区分 命中来源 与 排除项。
    %% owner：显式 avatar（投影断言原料）。
    insert_user(
        Owner,
        acc(Owner),
        <<"orgdir-owner">>,
        <<"https://avatar.example.com/owner.png">>,
        null,
        null
    ),
    %% m_root：昵称/账号均不含 token，email/mobile 含 token
    %% （U-02：search 查 token 不得命中该用户）。
    insert_user(
        MRoot,
        acc(MRoot),
        <<"orgdir-root">>,
        <<>>,
        <<Tok/binary, "@mail.invalid">>,
        <<"+86", Tok/binary>>
    ),
    %% m_arch_only：空昵称（display_name 回落 account 原料）。
    insert_user(MArchOnly, acc(MArchOnly), <<>>, <<>>, null, null),
    insert_user(MChild, acc(MChild), <<"orgdir-child">>, <<>>, null, null),
    %% m_dual：账号含 token（account 命中原料）。
    insert_user(MDual, <<Tok/binary, "-acc">>, <<"orgdir-dual">>, <<>>, null, null),
    insert_user(MRemoved, acc(MRemoved), <<"orgdir-removed">>, <<>>, null, null),
    insert_user(MSuspended, acc(MSuspended), <<"orgdir-suspended">>, <<>>, null, null),
    %% m_search：nickname 含 token（nickname 命中原料），email/mobile 不掺。
    insert_user(
        MSearch,
        acc(MSearch),
        <<"张三-"/utf8, Tok/binary>>,
        <<"https://avatar.example.com/search.png">>,
        null,
        null
    ),
    insert_user(OtherOwner, acc(OtherOwner), <<"orgdir-otherowner">>, <<>>, null, null),
    %% 次 Org 成员：nickname 含 token（跨 Org 泄漏负例原料）。
    insert_user(OtherMember, acc(OtherMember), <<"李四-"/utf8, Tok/binary>>, <<>>, null, null),

    %% —— 三个 Org：主(active) / 归档 / 跨 Org 隔离对照(active) ——
    ok = exec(
        <<"INSERT INTO organization(id,name,owner_id,status) VALUES ($1,$2,$3,'active')">>,
        [Org, <<"orgdir-org-", Tok/binary>>, Owner]
    ),
    ok = exec(
        <<"INSERT INTO organization(id,name,owner_id,status) VALUES ($1,$2,$3,'archived')">>,
        [ArchivedOrg, <<"orgdir-archived-", Tok/binary>>, Owner]
    ),
    ok = exec(
        <<"INSERT INTO organization(id,name,owner_id,status) VALUES ($1,$2,$3,'active')">>,
        [OtherOrg, <<"orgdir-other-", Tok/binary>>, OtherOwner]
    ),

    %% —— 成员：主 Org 全员（owner 行由 113 触发器自动建立，勿重复插入）
    %%    + 次 Org 一名成员 ——
    Members = [
        {Org, MRoot, member},
        {Org, MArchOnly, member},
        {Org, MChild, member},
        {Org, MDual, member},
        {Org, MRemoved, member},
        {Org, MSuspended, member},
        {Org, MSearch, member},
        {OtherOrg, OtherMember, member}
    ],
    lists:foreach(
        fun({O, U, Role}) ->
            ok = exec(
                <<
                    "INSERT INTO organization_member(organization_id,user_id,role,status)"
                    " VALUES ($1,$2,$3,'active')"
                >>,
                [O, U, atom_to_binary(Role, utf8)]
            )
        end,
        Members
    ),

    %% —— 部门：root_a(active 根，名含 token) / root_b(archived 根) /
    %%    child_a1+child_a2(root_a 子) / search_dept(根，名含 token) /
    %%    root_c+root_d(active 根，名不含 token——分页走页原料) /
    %%    other_org_dept(次 Org，名含 token) ——
    RootC = id(),
    RootD = id(),
    insert_dept(RootA, Org, null, <<"研发-"/utf8, Tok/binary>>, active, Owner),
    insert_dept(RootB, Org, null, <<"已归档-"/utf8, Tok/binary>>, archived, Owner),
    insert_dept(ChildA1, Org, RootA, <<"后端组-"/utf8, Tok/binary>>, active, Owner),
    insert_dept(ChildA2, Org, RootA, <<"前端组-"/utf8, Tok/binary>>, active, Owner),
    insert_dept(SearchDept, Org, null, <<"搜索部门-"/utf8, Tok/binary>>, active, Owner),
    insert_dept(RootC, Org, null, <<"平台部"/utf8>>, active, Owner),
    insert_dept(RootD, Org, null, <<"测试部"/utf8>>, active, Owner),
    insert_dept(OtherOrgDept, OtherOrg, null, <<"跨租户-"/utf8, Tok/binary>>, active, OtherOwner),

    %% —— 部门成员（建时成员必须 active——先挂再改离场态）——
    DeptMembers = [
        {Org, RootB, MArchOnly},
        {Org, ChildA1, MChild},
        {Org, ChildA1, MDual},
        {Org, ChildA2, MDual},
        {Org, ChildA2, MRemoved},
        {Org, ChildA2, MSuspended}
    ],
    lists:foreach(
        fun({O, D, U}) ->
            ok = exec(
                <<
                    "INSERT INTO organization_department_member"
                    " (organization_id,department_id,user_id,is_admin,added_by_user_id)"
                    " VALUES ($1,$2,$3,false,$4)"
                >>,
                [O, D, U, Owner]
            )
        end,
        DeptMembers
    ),

    %% —— 撤权（离场不级联删部门行；member_count/列表必须按 active 排除）——
    ok = exec(
        <<"UPDATE organization_member SET status='removed' WHERE organization_id=$1 AND user_id=$2">>,
        [Org, MRemoved]
    ),
    ok = exec(
        <<"UPDATE organization_member SET status='suspended' WHERE organization_id=$1 AND user_id=$2">>,
        [Org, MSuspended]
    ),

    #{
        token => Tok,
        org_id => Org,
        archived_org_id => ArchivedOrg,
        other_org_id => OtherOrg,
        owner_user_id => Owner,
        m_root => MRoot,
        m_arch_only => MArchOnly,
        m_child => MChild,
        m_dual => MDual,
        m_removed => MRemoved,
        m_suspended => MSuspended,
        m_search => MSearch,
        other_member => OtherMember,
        root_a => RootA,
        root_b => RootB,
        root_c => RootC,
        root_d => RootD,
        child_a1 => ChildA1,
        child_a2 => ChildA2,
        search_dept => SearchDept,
        other_org_dept => OtherOrgDept
    }.

insert_user(Id, Account, Nickname, Avatar, Email, Mobile) ->
    ok = exec(
        <<
            "INSERT INTO \"user\"(id,password,account,nickname,avatar,email,mobile,reg_ip,reg_cosv)"
            " VALUES ($1,'x',$2,$3,$4,$5,$6,'127.0.0.1','x')"
        >>,
        [Id, Account, Nickname, Avatar, Email, Mobile]
    ).

insert_dept(Id, OrgId, ParentId, Name, Status, CreatorId) ->
    ok = exec(
        <<
            "INSERT INTO organization_department"
            " (id,organization_id,parent_id,name,status,created_by_user_id)"
            " VALUES ($1,$2,$3,$4,$5,$6)"
        >>,
        [Id, OrgId, ParentId, Name, atom_to_binary(Status, utf8), CreatorId]
    ).

%% ===================================================================
%% 清场（best-effort，FK 安全顺序；失败静默——隔离靠唯一 TSID）
%% ===================================================================

-spec cleanup(map()) -> ok.
cleanup(Scope) ->
    Org = maps:get(org_id, Scope, undefined),
    ArchivedOrg = maps:get(archived_org_id, Scope, undefined),
    OtherOrg = maps:get(other_org_id, Scope, undefined),
    Users = [
        maps:get(K, Scope)
     || K <- [
            owner_user_id,
            m_root,
            m_arch_only,
            m_child,
            m_dual,
            m_removed,
            m_suspended,
            m_search,
            other_member
        ],
        maps:is_key(K, Scope)
    ],
    Orgs = [O || O <- [Org, OtherOrg], O =/= undefined],
    Stmts = lists:flatten(
        lists:map(
            fun(O) ->
                [
                    {<<"DELETE FROM organization_department_member WHERE organization_id=$1">>, [O]},
                    {<<"DELETE FROM organization_department WHERE organization_id=$1">>, [O]},
                    {<<"DELETE FROM organization_member WHERE organization_id=$1">>, [O]},
                    {<<"DELETE FROM organization WHERE id=$1">>, [O]}
                ]
            end,
            Orgs
        ) ++
            [
                {<<"DELETE FROM organization WHERE id=$1">>, [ArchivedOrg]}
             || ArchivedOrg =/= undefined
            ] ++
            [{<<"DELETE FROM \"user\" WHERE id=$1">>, [U]} || U <- Users]
    ),
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

%% ===================================================================
%% 合成命名
%% ===================================================================

acc(Id) ->
    <<"orgdir-acc-", (integer_to_binary(Id))/binary>>.
