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
    widget/1,
    tenant_actions/0,
    platform_actions/0,
    widget_actions/0,
    find/2,
    case_for/2,
    param_keys/1,
    feature/0,
    surface_required/0,
    org_source/1
]).

-export_type([owner/0, ptype/0, param/0, kase/0, entry/0, org_source/0]).

-type owner() :: tenant | platform | widget.
%% `tsid` = 64-bit TSID：传输层是 JSON/path **字符串**，投影成 integer 交给 application
%% （出站再编回 string，见 cs_http:encode_entity/1）。`map` = 嵌套 JSON 对象
%% （widget 断言 `assertion`，形状判定由 application 的 claims 全查承担）。
-type ptype() :: tsid | int | binary | list | map.
-type param() :: {atom(), ptype(), required | optional}.
-type kase() :: #{
    method := binary(),
    facade := atom(),
    params := [param()],
    path_params := [{atom(), atom()}],
    %% CSB-02R：workspace 门放宽（缺省 required；optional = 缺失不 422，
    %% application 自行决定作用域——坐席 org-wide 列表）。
    workspace => optional
}.
-type entry() :: #{
    owner := owner(),
    cases := [kase()],
    auth := map(),
    %% 服务端派生键：客户端**提供即 400**（操作人/时钟/坐席与访客身份一律不可自报）。
    client_forbidden := [atom()],
    org_source := org_source(),
    %% CSB-02R：同路径 method+auth_context 分流（route metadata 的 auth_context
    %% 冻结为 POST/默认主体；此表按 HTTP 方法覆盖认证声明——同一 cowboy 路径
    %% 的 GET 坐席语义）。cs_route_contract_tests 审计其方法/主体合法性。
    case_auth => #{binary() => map()}
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

widget(Action) ->
    find(widget, Action).

find(Owner, Action) ->
    case lists:keyfind(Action, 1, table(Owner)) of
        {Action, Entry} -> {ok, Entry#{action => Action}};
        false -> {error, {unknown_action, Action}}
    end.

tenant_actions() ->
    [A || {A, _} <- table(tenant)].

platform_actions() ->
    [A || {A, _} <- table(platform)].

widget_actions() ->
    [A || {A, _} <- table(widget)].

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

%% CSB-03：widget 接入面（principal 与访客同类：visit token 头
%% `x-cs-visit-token` 携带的 bootstrap 令牌）。与既有 visit token 的差别只在
%% 存储行的绑定列（installation + contact）：令牌校验（digest 命中
%% (Org, installation) + 未吊销 + 未过期）在 application 用例内逐请求裁决
%% （`cs_widget_support:verify_bootstrap_token/2`）。`widget_bootstrap` 是
%% 签发点本身，令牌**可选**（重放心跳）；其余 widget 动作令牌必填
%% （缺头即 401，由 cs_widget_handler 执行）。
widget_auth() ->
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

%% CSB-03：widget 面的服务端派生/注入键全集。除公共时钟外：contact 与
%% workspace 由令牌行与默认 Workspace 事实服务端解析；`origin` 只来自
%% Origin 头（handler 归一化后注入）；`secret` 只来自专用头；其余键是
%% application 的 Ctx 注入面（HMAC 材料 / 事实 fun / 端口 / 摘要 fun /
%% 断言验证器 / TTL）——浏览器可写即等于把服务端事实交给客户端，
%% 一律「提供即 400」。
widget_server_derived() ->
    server_common() ++
        [
            contact_id,
            business_identity_id,
            created_by_user_id,
            workspace_id,
            origin,
            secret,
            subject_key,
            default_workspace,
            assertion_verifier,
            intake_business_identity_id,
            store,
            id,
            digest,
            new_secret,
            bootstrap_token_ttl,
            eb_store,
            eb_audit,
            eb_id,
            eb_clock,
            eb_crypto
        ].

%% ===================================================================
%% 租户面（§5.2 + A0 客户端契约基准）
%% ===================================================================

table(tenant) ->
    [
        %% 门店开会话（A0 冻结路径）：shop key 主体；contact/conversation 由门店
        %% 集成方给出，session 只挂同一个 enterprise conversation（§5.2）。
        {session_queue,
            with_case_auth(
                entry(
                    [
                        {<<"POST">>, open_session,
                            [{contact_id, tsid, required}, {conversation_id, tsid, required}], []},
                        %% CSB-02R：坐席队列视图（GET）——与 POST 门店开会话同路径
                        %% 按 method+auth_context 分流；workspace 可选（org-wide，
                        %% 显式给出则收窄）。坐席作用域由 cs_auth 在进用例前裁决。
                        {<<"GET">>, seat_session_queue,
                            [{after_id, binary, optional}, {limit, binary, optional}], [], #{
                                workspace => optional
                            }}
                    ],
                    shop_key_auth(),
                    server_common() ++ [created_by_user_id],
                    param
                ),
                #{<<"GET">> => seat_auth(<<"conversation.read">>)}
            )},
        %% 访客视角：只列**自己的**会话（contact 取自 token 作用域，不可自报）。
        {visitor_sessions,
            entry(
                [{<<"GET">>, list_contact_sessions, [], []}],
                visit_auth(),
                server_common() ++ [contact_id],
                param
            )},
        %% 访客入站消息：client_msg_id 是 application 的无默认 maps:get 键——
        %% handler 必须前置结构化校验，缺失返回 422 而不是 500。
        %% F6（RULING-2026-09-15 §七）：主密钥材料不经 HTTP/JSON 面——`key_ref`
        %% 参数已删除；客户端显式提交即 422（cs_http 的密钥材料键守卫，
        %% FND-5 body_cipher 同款先例）。密钥由服务端经 `imboy.eb_enterprise_keyring`
        %% 装配（eb_message_app → eb_env_keyring）。
        {session_messages,
            entry(
                [
                    {<<"POST">>, append_session_message,
                        [
                            {body, binary, required},
                            {client_msg_id, binary, required}
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
        %% CSB-03：坐席会话详情（GET /api/v1/cs/sessions/:id）——坐席侧单会话读，
        %% 与平台面 p_session 共用同一 fetch_session 用例（不复制逻辑）。
        {session_detail,
            entry(
                [{<<"GET">>, seat_session_detail, [], [{id, session_id}]}],
                seat_auth(<<"conversation.read">>),
                server_common() ++ [business_identity_id],
                param
            )},
        %% CSB-02R：坐席 active/closed 两视图（GET /api/v1/cs/seats/sessions）。
        %% 独立路径的理由：GET /api/v1/cs/sessions 已冻结为访客面（cs_visit，
        %% route metadata 是 principal 的唯一分流依据，同方法双主体必须换路径）；
        %% `seats/sessions` 与既有 cs_seat/seats 命名族一致。queued 视图冻结在
        %% /sessions/queue（GET，case_auth 分流），此处 status 显式必填且仅
        %% 接受 active|closed（application 复核）。
        {seat_session_list,
            entry(
                [
                    {<<"GET">>, seat_session_list,
                        [
                            {status, binary, required},
                            {after_id, binary, optional},
                            {limit, binary, optional}
                        ],
                        [], #{workspace => optional}}
                ],
                seat_auth(<<"conversation.read">>),
                server_common() ++ [business_identity_id],
                param
            )},
        %% —— 以下为租户治理面（owner/admin）：seat / shop key / visit token ——
        %% C4（contracts-w2）：seats 列表 GET 支持 after_id/limit 键集分页
        %%（binary 形态透传给 application 校验——非法取值 422，而非 400）。
        {seats,
            entry(
                [
                    {<<"GET">>, list_dispatchable_seats,
                        [{after_id, binary, optional}, {limit, binary, optional}], []},
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
        %% C2（contracts-w2）：shop key 治理面。cowboy 只按 path 匹配——列表 GET
        %% 与创建 POST 同路径动作（seats 同款先例），动作键按冻结契约命名。
        {shop_key_list,
            entry(
                [
                    {<<"GET">>, list_shop_keys,
                        [{after_id, binary, optional}, {limit, binary, optional}], []},
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
        %% C3（contracts-w2）：visit token 治理面（列表 GET + 签发 POST 同路径动作）。
        {visit_token_list,
            entry(
                [
                    {<<"GET">>, list_visit_tokens,
                        [{after_id, binary, optional}, {limit, binary, optional}], []},
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
%% widget 接入面（CSB-03，plan §12.4 的 HTTP 落地）：浏览器访客，凭证是
%% bootstrap 令牌专用头（查询串携带凭证即 400，见 cs_http）；令牌校验在
%% application 用例内逐请求裁决。bootstrap 无令牌可验（它就是签发点，
%% 令牌可选 = 重放心跳）；会话生命周期用例复用 `cs_widget_session_app`。
%% Org 恒为申报参数（path 无 org 段），由令牌 digest 的同语句命中证明。
%% ===================================================================
table(widget) ->
    [
        {widget_bootstrap,
            widget_entry(
                [
                    {<<"POST">>, widget_bootstrap,
                        [
                            {public_widget_id, binary, required},
                            {subject_id, binary, required}
                        ],
                        []}
                ],
                widget_auth(),
                widget_server_derived(),
                param
            )},
        {widget_identity_exchange,
            widget_entry(
                [
                    {<<"POST">>, widget_identity_exchange,
                        [
                            {installation_id, tsid, required},
                            {assertion, map, required}
                        ],
                        []}
                ],
                widget_auth(),
                widget_server_derived(),
                param
            )},
        %% 会话建立（POST）与访客会话列表（GET）同路径动作（cowboy 只按 path
        %% 匹配——seats/shop-keys 同款先例）。
        {widget_sessions,
            widget_entry(
                [
                    {<<"POST">>, widget_create_session, [{installation_id, tsid, required}], []},
                    {<<"GET">>, widget_list_sessions, [{installation_id, tsid, required}], []}
                ],
                widget_auth(),
                widget_server_derived(),
                param
            )},
        {widget_session_messages,
            widget_entry(
                [
                    {<<"GET">>, widget_history_after,
                        [
                            {installation_id, tsid, required},
                            {after_id, tsid, optional},
                            {limit, int, optional}
                        ],
                        [{id, session_id}]},
                    {<<"POST">>, widget_visitor_message,
                        [
                            {installation_id, tsid, required},
                            {body, binary, required},
                            {client_msg_id, binary, required}
                        ],
                        [{id, session_id}]}
                ],
                widget_auth(),
                widget_server_derived(),
                param
            )},
        %% SSE 事件流（GET）：流式响应由 cs_widget_handler 专用分支承担；
        %% 动作表声明的是补偿读语义（Last-Event-ID / after_id → 历史 after 游标），
        %% facade 与普通历史同源（widget_history_after）。
        {widget_session_events,
            widget_entry(
                [
                    {<<"GET">>, widget_history_after,
                        [
                            {installation_id, tsid, required},
                            {after_id, tsid, optional},
                            {limit, int, optional}
                        ],
                        [{id, session_id}]}
                ],
                widget_auth(),
                widget_server_derived(),
                param
            )},
        {widget_asset_upload,
            widget_entry(
                [
                    {<<"POST">>, widget_asset_upload,
                        [
                            {installation_id, tsid, required},
                            {mime, binary, required},
                            {size_bytes, int, required},
                            %% CSB-02S D6 补全：企业面 request_presign 的
                            %% object_hash（64 位小写 hex SHA-256）为必填，
                            %% PUT 后由服务端复核——此前 widget 面漏收该参数，
                            %% 桥接层送 undefined 进校验恒 500。
                            {object_hash, binary, required}
                        ],
                        [{id, session_id}]}
                ],
                widget_auth(),
                widget_server_derived(),
                param
            )},
        {widget_asset_confirm,
            widget_entry(
                [
                    {<<"POST">>, widget_asset_confirm,
                        [
                            {installation_id, tsid, required},
                            {upload_ref, binary, required}
                        ],
                        [{id, session_id}]}
                ],
                widget_auth(),
                widget_server_derived(),
                param
            )},
        {widget_session_rating,
            widget_entry(
                [
                    {<<"POST">>, widget_rate,
                        [
                            {installation_id, tsid, required},
                            {rating, int, required},
                            {expected_version, int, required}
                        ],
                        [{id, session_id}]}
                ],
                widget_auth(),
                widget_server_derived(),
                param
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
                [
                    {<<"GET">>, list_dispatchable_seats,
                        [{after_id, binary, optional}, {limit, binary, optional}], []}
                ],
                platform_auth(<<"customer_service:read">>)
            )},
        %% C1（contracts-w2）：平台 session 列表（只读）。workspace_id 是 handler
        %% 强制的 face 级必填（Derived 注入）；status 白名单 / after_id / limit 由
        %% application 校验（非法取值 422 原子）。
        {p_session_list,
            platform_entry(
                [
                    {<<"GET">>, list_sessions,
                        [
                            {status, binary, optional},
                            {after_id, binary, optional},
                            {limit, binary, optional}
                        ],
                        []}
                ],
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
            )},
        {p_widget_installations,
            with_case_auth(
                platform_param_entry(
                    [
                        {<<"GET">>, list_widget_installations,
                            [{after_id, binary, optional}, {limit, binary, optional}], []},
                        {<<"POST">>, create_widget_installation,
                            [
                                {display_name, binary, required},
                                {allowed_origins, list, required},
                                {branding, map, required},
                                {consent_version, binary, required}
                            ],
                            [], #{clock_unit => second}}
                    ],
                    platform_auth(<<"customer_service:read">>),
                    [id, store, new_public_widget_id]
                ),
                #{<<"POST">> => platform_auth(<<"customer_service:write">>)}
            )},
        {p_widget_installation_revoke,
            platform_param_entry(
                [
                    {<<"POST">>, revoke_widget_installation, [], [{id, id}], #{
                        clock_unit => second
                    }}
                ],
                platform_auth(<<"customer_service:write">>),
                [store]
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
    };
kase({Method, Facade, Params, PathParams, Opts}) when is_map(Opts) ->
    maps:merge(kase({Method, Facade, Params, PathParams}), Opts).

%% CSB-02R：entry 级 case_auth 覆盖（同路径 method+auth_context 分流）。
with_case_auth(Entry, CaseAuth) when is_map(CaseAuth) ->
    Entry#{case_auth => CaseAuth}.

%% widget 面路径动作构造（owner=widget；其余形状与租户面一致）。
widget_entry(Cases, Auth, ClientForbidden, OrgSource) ->
    (entry(Cases, Auth, ClientForbidden, OrgSource))#{owner => widget}.

%% 平台面路径动作构造：org 默认来自 path；服务端派生键 = 公共集。
platform_entry(Cases, Auth) ->
    #{
        owner => platform,
        cases => [kase(C) || C <- Cases],
        auth => Auth,
        client_forbidden => server_common(),
        org_source => path
    }.

platform_param_entry(Cases, Auth, ExtraForbidden) ->
    (platform_entry(Cases, Auth))#{
        client_forbidden => server_common() ++ ExtraForbidden,
        org_source => param
    }.
