%%% @doc 客服 HTTP 动作表（CS-02，plan v4.1 §5.2/§5.3 的**冻结契约**，纯数据 + 纯函数）。
%%%
%%% 依据：plan v4.1 §5.2（客服 API：shop key、visit token、session、claim、transfer、
%%% close、rating）、§5.3（平台 Admin API）、EB-D10（五类 principal）、CS-02-A01/A02。
%%% 客户端契约基准（A0 裁决，与 imboyapp A4 纵切对齐）：`POST /api/v1/cs/sessions/queue`、
%%% `POST /api/v1/cs/sessions/{id}/claim|transfer|close|rating`、
%%% `GET /api/v1/enterprise/conversations/{convId}/messages?after_id=`、
%%% offboarding 降级 = HTTP 409 + envelope `offboarding_required`。
%%%
%%% ## 与 `eb_enterprise_actions` 同构的两层形状
%%%
%%% cowboy 只按 path 匹配，故一行 = 一条**路径动作**，其下按 HTTP 方法分派**用例**：
%%%
%%%     action（路径级，进 route metadata，是 principal 的唯一来源）
%%%       └── cases: [{方法, facade 函数, 参数表, 路径参数表}]
%%%
%%% ## 五类 principal（EB-D10，本表是唯一的 HTTP 侧声明点）
%%%
%%%   * `enterprise_owner_admin` —— 租户治理（seat / shop key / visit token 管理）；
%%%   * `cs_seat`               —— 坐席（JWT + customer_service 职能 assignment）；
%%%   * `cs_visit`              —— 访客（visit token，digest + 未吊销 + 未过期）；
%%%   * `cs_shop_key`           —— 门店接入（shop key，digest + 未吊销）；
%%%   * `platform_admin`        —— 平台运营面（`customer_service:read|write`）。
%%%
%%% 五类身份的**凭证类别互斥**（`cs_auth` 按本表声明的 principal 只取该类凭证，
%%% 其余一概不采信）——这是 CS-02-A01「无混淆」的表级保证。
%%%
%%% ## Org 归属
%%%
%%%   * `/api/v1/cs/organizations/:org_id/*`（治理动作）与平台面：OrgId 来自 path；
%%%   * 其余租户动作（A0 冻结路径，path 无 org 段）：OrgId 是客户端**申报**的必填
%%%     `organization_id` 参数，由 `cs_auth` 用凭证/事实**证明**（跨 Org 的
%%%     secret/token/member 在该 Org 的同语句查找必失败 ⇒ 401/403，不是信任）。
%%%
%%% **本模块不做**：不解析请求、不读库、不判权限、不拼响应。
-module(cs_actions).

-export([
    tenant/1,
    platform/1,
    tenant_actions/0,
    platform_actions/0,
    find/2,
    case_for/2,
    param_keys/1,
    feature/0,
    surface_required/0,
    org_source/1
]).

-export_type([owner/0, ptype/0, param/0, kase/0, entry/0, org_source/0]).

-type owner() :: tenant | platform.
%% `tsid` = 64-bit TSID：传输层是 JSON/path **字符串**，投影成 integer 交给 application
%% （出站再编回 string，见 cs_http:encode_entity/1）。
-type ptype() :: tsid | int | binary.
-type param() :: {atom(), ptype(), required | optional}.
-type kase() :: #{
    method := binary(),
    facade := atom(),
    params := [param()],
    path_params := [{atom(), atom()}]
}.
-type entry() :: #{
    owner := owner(),
    cases := [kase()],
    auth := map(),
    %% 服务端派生键：客户端**提供即 400**（操作人/时钟/坐席与访客身份一律不可自报）。
    client_forbidden := [atom()],
    org_source := org_source()
}.
%% OrgId 的来源：path 绑定 / 请求参数（客户端申报 + 授权证明）。
-type org_source() :: path | param.

-define(FEATURE, customer_service).

%% @doc route metadata 里自报的 feature 键（契约一致性）。
feature() ->
    ?FEATURE.

%% @doc 面级 route 元数据的注入键（router 装配点统一注入；缺一即 fail-closed）。
surface_required() ->
    [surface, feature, auth_facts].

%% @doc 该动作的 OrgId 来源（`cs_http` 据此解析，契约测试据此核对）。
org_source(Entry) ->
    maps:get(org_source, Entry, param).

%% ===================================================================
%% 查表
%% ===================================================================

tenant(Action) ->
    find(tenant, Action).

platform(Action) ->
    find(platform, Action).

find(Owner, Action) ->
    case lists:keyfind(Action, 1, table(Owner)) of
        {Action, Entry} -> {ok, Entry#{action => Action}};
        false -> {error, {unknown_action, Action}}
    end.

tenant_actions() ->
    [A || {A, _} <- table(tenant)].

platform_actions() ->
    [A || {A, _} <- table(platform)].

%% @doc 路径级动作 + HTTP 方法 → 用例。未登记方法返回 `{error, method_not_allowed}`。
case_for(Entry, Method) ->
    case [C || C <- maps:get(cases, Entry), maps:get(method, C) =:= Method] of
        [Case | _] -> {ok, Case};
        [] -> {error, method_not_allowed}
    end.

%% @doc 该路径动作的全部参数键（正文/查询键白名单，用于 binary→atom 规范化）。
param_keys(Entry) ->
    lists:usort([
        K
     || Case <- maps:get(cases, Entry),
        {K, _Type, _Req} <- maps:get(params, Case)
    ]).

%% ===================================================================
%% 授权需求（五类 principal 的唯一 HTTP 声明点）
%% ===================================================================

%% 租户治理（seat / shop key / visit token 管理）：owner/admin，不因业务身份自动获得。
governance_auth() ->
    #{
        auth_context => enterprise_owner_admin,
        required_governance => [<<"owner">>, <<"admin">>]
    }.

%% 坐席：customer_service 职能 assignment + 独立动作权限（职能不替代权限）。
seat_auth(Permission) ->
    #{
        auth_context => cs_seat,
        required_function => <<"customer_service">>,
        required_permission => Permission
    }.

%% 访客：visit token（digest + 未吊销 + 未过期 + 绑定本 Org/contact）。
visit_auth() ->
    #{auth_context => cs_visit}.

%% 门店接入：shop key（digest + 未吊销）。
shop_key_auth() ->
    #{auth_context => cs_shop_key}.

%% 平台面权限（§5.3：read 只读 / write 纠错）。
platform_auth(Permission) ->
    #{auth_context => platform_admin, required_permission => Permission}.

%% 服务端派生键的公共集：操作人与时钟永远不来自客户端。
server_common() ->
    [actor_user_id, at].

%% ===================================================================
%% 租户面（§5.2 + A0 客户端契约基准）
%% ===================================================================

table(tenant) ->
    [
        %% 门店开会话（A0 冻结路径）：shop key 主体；contact/conversation 由门店
        %% 集成方给出，session 只挂同一个 enterprise conversation（§5.2）。
        {session_queue,
            entry(
                [
                    {<<"POST">>, open_session,
                        [{contact_id, tsid, required}, {conversation_id, tsid, required}], []}
                ],
                shop_key_auth(),
                server_common() ++ [created_by_user_id],
                param
            )},
        %% 访客视角：只列**自己的**会话（contact 取自 token 作用域，不可自报）。
        {visitor_sessions,
            entry(
                [{<<"GET">>, list_contact_sessions, [], []}],
                visit_auth(),
                server_common() ++ [contact_id],
                param
            )},
        %% 访客入站消息：client_msg_id/key_ref 是 application 的无默认
        %% maps:get 键（CS-01 审查观察项）——handler 必须前置结构化校验，
        %% 缺失返回 422 而不是 500（cs_handler_tests 逐条覆盖）。
        {session_messages,
            entry(
                [
                    {<<"POST">>, append_session_message,
                        [
                            {body, binary, required},
                            {client_msg_id, binary, required},
                            {key_ref, binary, required}
                        ],
                        [{id, session_id}]}
                ],
                visit_auth(),
                server_common() ++ [contact_id, business_identity_id],
                param
            )},
        %% 访客评分（1..5，仅 closed，不可重复）。
        {session_rating,
            entry(
                [
                    {<<"POST">>, rate, [{rating, int, required}, {expected_version, int, required}],
                        [{id, session_id}]}
                ],
                visit_auth(),
                server_common() ++ [contact_id, business_identity_id],
                param
            )},
        %% 坐席接单：business_identity_id 服务端派生（坐席只能以**本人**身份接单）。
        {session_claim,
            entry(
                [{<<"POST">>, claim, [{expected_version, int, required}], [{id, session_id}]}],
                seat_auth(<<"conversation.write">>),
                server_common() ++ [business_identity_id],
                param
            )},
        {session_transfer,
            entry(
                [
                    {<<"POST">>, transfer,
                        [{to_identity_id, tsid, required}, {expected_version, int, required}], [
                            {id, session_id}
                        ]}
                ],
                seat_auth(<<"conversation.write">>),
                server_common(),
                param
            )},
        {session_close,
            entry(
                [
                    {<<"POST">>, close,
                        [{expected_version, int, required}, {reason, binary, optional}], [
                            {id, session_id}
                        ]}
                ],
                seat_auth(<<"conversation.write">>),
                server_common(),
                param
            )},
        %% A0 客户端契约基准：客服端的企业消息列表（游标 after_id，TSID string）。
        %% 唯一经 `enterprise_business_facade:list_messages` 复用企业真源的读路径，
        %% 不复制任何消息逻辑。
        {conversation_messages,
            entry(
                [
                    {<<"GET">>, list_messages, [{after_id, tsid, optional}, {limit, int, optional}],
                        [{conversation_id, conversation_id}]}
                ],
                seat_auth(<<"conversation.read">>),
                server_common() ++ [business_identity_id],
                param
            )},
        %% —— 以下为租户治理面（owner/admin）：seat / shop key / visit token ——
        {seats,
            entry(
                [
                    {<<"GET">>, list_dispatchable_seats, [], []},
                    {<<"POST">>, create_seat,
                        [
                            {business_identity_id, tsid, required},
                            {max_concurrent, int, optional}
                        ],
                        []}
                ],
                governance_auth(),
                server_common() ++ [created_by_user_id],
                path
            )},
        {seat_suspend,
            entry(
                [
                    {<<"POST">>, suspend_seat, [{reason, binary, optional}], [
                        {id, business_identity_id}
                    ]}
                ],
                governance_auth(),
                server_common(),
                path
            )},
        {seat_resume,
            entry(
                [{<<"POST">>, resume_seat, [], [{id, business_identity_id}]}],
                governance_auth(),
                server_common(),
                path
            )},
        {shop_key_create,
            entry(
                [
                    {<<"POST">>, create_shop_key,
                        [{secret, binary, required}, {display_hint, binary, optional}], []}
                ],
                governance_auth(),
                server_common() ++ [created_by_user_id],
                path
            )},
        {shop_key_revoke,
            entry(
                [{<<"POST">>, revoke_shop_key, [], [{id, id}]}],
                governance_auth(),
                server_common(),
                path
            )},
        {visit_token_issue,
            entry(
                [
                    {<<"POST">>, issue_visit_token,
                        [
                            {contact_id, tsid, required},
                            {secret, binary, required},
                            {expires_at, int, required}
                        ],
                        []}
                ],
                governance_auth(),
                server_common() ++ [created_by_user_id, created_by_business_identity_id],
                path
            )},
        {visit_token_revoke,
            entry(
                [{<<"POST">>, revoke_visit_token, [], [{id, id}]}],
                governance_auth(),
                server_common(),
                path
            )}
    ];
%% ===================================================================
%% 平台运营面（§5.3）：每条路径显式带 :org_id + workspace_id 必填；
%% 与租户面共用同一 application（CS-02-A02：不复制业务逻辑）。
%% ===================================================================
table(platform) ->
    [
        {p_seats,
            platform_entry(
                [{<<"GET">>, list_dispatchable_seats, [], []}],
                platform_auth(<<"customer_service:read">>)
            )},
        {p_seat_suspend,
            platform_entry(
                [
                    {<<"POST">>, suspend_seat, [{reason, binary, optional}], [
                        {id, business_identity_id}
                    ]}
                ],
                platform_auth(<<"customer_service:write">>)
            )},
        {p_seat_resume,
            platform_entry(
                [{<<"POST">>, resume_seat, [], [{id, business_identity_id}]}],
                platform_auth(<<"customer_service:write">>)
            )},
        {p_session,
            platform_entry(
                [{<<"GET">>, fetch_session, [], [{id, session_id}]}],
                platform_auth(<<"customer_service:read">>)
            )},
        {p_session_transfer,
            platform_entry(
                [
                    {<<"POST">>, transfer,
                        [{to_identity_id, tsid, required}, {expected_version, int, required}], [
                            {id, session_id}
                        ]}
                ],
                platform_auth(<<"customer_service:write">>)
            )},
        {p_session_close,
            platform_entry(
                [{<<"POST">>, close, [{expected_version, int, required}], [{id, session_id}]}],
                platform_auth(<<"customer_service:write">>)
            )}
    ].

%% 租户面路径动作构造（action 由 find/2 用表键补齐）。
entry(Cases, Auth, ClientForbidden, OrgSource) ->
    #{
        owner => tenant,
        cases => [kase(C) || C <- Cases],
        auth => Auth,
        client_forbidden => ClientForbidden,
        org_source => OrgSource
    }.

kase({Method, Facade, Params, PathParams}) ->
    #{
        method => Method,
        facade => Facade,
        params => Params,
        path_params => PathParams
    }.

%% 平台面路径动作构造：org 恒来自 path；服务端派生键 = 公共集。
platform_entry(Cases, Auth) ->
    #{
        owner => platform,
        cases => [kase(C) || C <- Cases],
        auth => Auth,
        client_forbidden => server_common(),
        org_source => path
    }.
