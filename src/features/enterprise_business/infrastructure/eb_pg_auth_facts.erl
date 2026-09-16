%%% @doc EB-03R C5/D7：`eb_auth_port` 的**唯一装配实现**（逐请求只读事实加载）。
%%%
%%% 依据：R0 的 D7——`eb_auth_port` 此前**未注册**（不在 `eb_ports:all/0`、不在
%%% `eb_infra_ports:implementations/0`，也没有任何实现）。EB-04 交付了只读契约与
%%% 调用方（`eb_auth_app:authorize_via_port/3` 接受注入的端口模块），但**缺装配**。
%%%
%%% 本模块把契约 ↔ 装配对齐，并且只做三件事：
%%%   * **逐请求**读取成员关系、有效经办关系与权限（不缓存、不跨请求复用）；
%%%   * 事实**不含授权结论**：`status = suspended` 照样把事实返回（permissions 为空），
%%%     「能不能做某事」由 `eb_auth_app` 判定；
%%%   * **零写**：本模块只有 `SELECT`（`sql_statements/0` 可机械核对）。
%%%
%%% 失败一律 fail-closed：无成员关系 / 请求缺租户键 / 未知请求类别 → `{error, _}`，
%%% **绝不**返回「默认放行」的空事实。
-module(eb_pg_auth_facts).

-behaviour(eb_auth_port).

-export([load_request_facts/1, sql_statements/0]).

-define(SQL_MEMBER, <<
    "SELECT m.organization_id, m.user_id, m.role, m.status"
    "  FROM organization_member m"
    " WHERE m.organization_id = $1 AND m.user_id = $2"
>>).

-define(SQL_MEMBER_ASSIGNMENTS, <<
    "SELECT a.business_identity_id, a.user_id, a.organization_id, a.function_key,"
    " a.status, a.version"
    "  FROM organization_business_identity_assignment a"
    " WHERE a.organization_id = $1 AND a.user_id = $2 AND a.status = 'active'"
    " ORDER BY a.business_identity_id"
>>).

%% @doc 冻结语句（**只读**：只有 SELECT）。
-spec sql_statements() -> [binary()].
sql_statements() ->
    [?SQL_MEMBER, ?SQL_MEMBER_ASSIGNMENTS].

%% @doc 逐请求加载成员类授权事实。
%%
%% `Request` 必含 `organization_id` 与 `user_id`（V1 只支持成员类 principal；
%% 访客 / 店铺类与平台类在 EB-04 的契约里另有形状，本实现**显式**拒绝而非猜测）。
%%
%% 返回 `{ok, #{organization_id, member, assignments, permissions}}` 或 `{error, _}`。
%% `member` 含 `user_id` / `role` / `status`（含 `suspended`——事实照报，权限为空）；
%% `permissions` 是**基于角色的静态权限集**，是事实的一部分，不是最终裁决。
-spec load_request_facts(map()) -> {ok, map()} | {error, term()}.
load_request_facts(Request) when is_map(Request) ->
    case tenant_keys(Request) of
        {error, _} = Err ->
            Err;
        {ok, OrgId, UserId} ->
            case member_row(OrgId, UserId) of
                {ok, Member} ->
                    case assignments(OrgId, UserId) of
                        {ok, Assignments} ->
                            %% F1/F2（RULING-2026-09-15 §五/§六）：
                            %%  * member 事实投影 governance_roles（唯一事实源
                            %%    organization_member.role：owner/admin → 自身，
                            %%    其余 → 空），保留原始 role；
                            %%  * permissions = 治理能力 ∪ 经办业务能力，且仅在
                            %%    member status=active 时授予 —— suspended/removed
                            %%    即使历史 role 为 owner/admin 也拿不到任何权限
                            %%    （active-member 门之外的又一重 fail-closed）。
                            FunctionKeys = [
                                maps:get(function_key, A)
                             || A <- Assignments,
                                maps:get(status, A) =:= active
                            ],
                            {ok, #{
                                organization_id => OrgId,
                                member => Member#{
                                    governance_roles => governance_roles(maps:get(role, Member))
                                },
                                assignments => Assignments,
                                permissions => permissions_for(
                                    maps:get(status, Member),
                                    maps:get(role, Member),
                                    FunctionKeys
                                )
                            }};
                        {error, _} = Err ->
                            Err
                    end;
                {error, _} = Err ->
                    Err
            end
    end;
load_request_facts(_Request) ->
    {error, invalid_request}.

tenant_keys(Request) ->
    OrgId = maps:get(organization_id, Request, undefined),
    UserId = maps:get(user_id, Request, undefined),
    case {is_integer(OrgId), is_integer(UserId)} of
        {true, true} -> {ok, OrgId, UserId};
        _ -> {error, {missing_tenant_keys, Request}}
    end.

member_row(OrgId, UserId) ->
    case elib_pg:query(?SQL_MEMBER, [OrgId, UserId]) of
        {ok, [Row | _]} ->
            {ok, #{
                user_id => maps:get(<<"user_id">>, Row),
                role => atomize(maps:get(<<"role">>, Row)),
                status => atomize(maps:get(<<"status">>, Row))
            }};
        {ok, []} ->
            {error, no_member};
        {error, Reason} ->
            {error, eb_pg_store_sql:normalize_error(Reason)}
    end.

assignments(OrgId, UserId) ->
    case elib_pg:query(?SQL_MEMBER_ASSIGNMENTS, [OrgId, UserId]) of
        {ok, Rows} ->
            {ok, [
                #{
                    business_identity_id => maps:get(<<"business_identity_id">>, Row),
                    user_id => maps:get(<<"user_id">>, Row),
                    organization_id => maps:get(<<"organization_id">>, Row),
                    function_key => maps:get(<<"function_key">>, Row),
                    status => atomize(maps:get(<<"status">>, Row)),
                    version => maps:get(<<"version">>, Row)
                }
             || Row <- Rows
            ]};
        {error, Reason} ->
            {error, eb_pg_store_sql:normalize_error(Reason)}
    end.

%% ===================================================================
%% F1/F2（RULING-2026-09-15 §五/§六）：三分离的事实层投影
%% ===================================================================

%% @doc 治理角色投影：**唯一事实源**是 organization_member.role。
%% active owner -> [owner]，active admin -> [admin]，member/未知 -> []。
%% suspended/removed 的拦截由 active-member 门 + permissions_for 的 status
%% 守卫双重保证（本函数只投影 role，不看 status——status 在 permissions_for
%% 与 eb_auth_app:active_member/1 处强制）。
governance_roles(owner) -> [<<"owner">>];
governance_roles(admin) -> [<<"admin">>];
governance_roles(_Other) -> [].

%% @doc 权限投影 = 治理能力 ∪ 经办业务能力，仅 active member 授予。
%%
%% 治理能力（创建/列举/绑定业务身份、suspend、offboarding、retention/hold）
%% 由 active owner/admin 获得；业务读写（contact/note/conversation/message/
%% asset）**只能**经匹配的 active assignment（sales/customer_service 同清单，
%% §五 V1 冻结集）获得 —— owner/admin 不因治理角色自动获得业务读写。
permissions_for(active, Role, FunctionKeys) ->
    governance_permissions(Role) ++ function_permissions(FunctionKeys);
permissions_for(_NotActive, _Role, _FunctionKeys) ->
    [].

%% 治理能力：owner/admin 并列（§五第 1 条）；member 走 _Other 空集。
governance_permissions(owner) -> governance_permission_set();
governance_permissions(admin) -> governance_permission_set();
governance_permissions(_Other) -> [].

governance_permission_set() ->
    [
        <<"org.manage">>,
        <<"member.manage">>,
        <<"member.suspend">>,
        <<"retention.manage">>,
        <<"offboarding.manage">>
    ].

%% V1 经办身份（sales | customer_service）的业务能力清单 —— §五第 3 条逐字
%% 冻结：contact.read/write、note.write、conversation.read/write、
%% message.write、asset.read/write。未知 function_key 不给任何权限。
function_permissions(FunctionKeys) ->
    lists:usort(lists:append([function_permissions_for(K) || K <- FunctionKeys])).

function_permissions_for(<<"sales">>) -> business_permission_set();
function_permissions_for(<<"customer_service">>) -> business_permission_set();
function_permissions_for(_Unknown) -> [].

business_permission_set() ->
    [
        <<"contact.read">>,
        <<"contact.write">>,
        <<"note.write">>,
        <<"conversation.read">>,
        <<"conversation.write">>,
        <<"message.write">>,
        <<"asset.read">>,
        <<"asset.write">>
    ].

atomize(<<"owner">>) -> owner;
atomize(<<"admin">>) -> admin;
atomize(<<"member">>) -> member;
atomize(<<"active">>) -> active;
atomize(<<"suspended">>) -> suspended;
atomize(<<"removed">>) -> removed;
atomize(Other) -> Other.
