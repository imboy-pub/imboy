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
%%%   * `/api/v1/cs/organizations/:org_id/*`（T-2 裁定后的坐席/治理动作）与
%%%     平台面：OrgId 来自 path；
%%%   * `self`（BE-S01a）：主体自身作用域（坐席上下文清单），无 Org 键——
%%%     handler 走 self 认证分支，聚合时逐 Org 复核；
%%%   * 其余访客动作（A0 冻结路径，path 无 org 段）：OrgId 是客户端**申报**的
%%%     必填 `organization_id` 参数，由 `cs_auth` 用凭证/事实**证明**（跨 Org 的
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
%% OrgId 的来源：path 绑定 / 请求参数（客户端申报 + 授权证明）/ self（主体
%% 自身作用域——跨 Org 聚合用例，如坐席上下文清单；授权只验凭证类别，
%% 每个 Org 的成员/坐席事实由 application 聚合时逐 Org 复核）/ derived
%% （CSD-BE-01R/01S，hosted-widget-contract S3：浏览器零申报面——bootstrap
%% 由 public_widget_id 全局反查、持 token 动作面由 (installation_id, secret)
%% 的 digest 全局命中行**权威派生**，handler 传 0 占位）/ param_optional
%% （平台运营面专属：`organization_id` 是**可选**过滤参数，缺失 = 跨企业
%% 全局列举，handler 传 0 占位；给出则收窄到该企业）。
-type org_source() :: path | param | self | derived | param_optional.

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

%% BE-S01a：主体自身作用域（跨 Org 聚合用例，如坐席上下文清单）。授权只验
%% 凭证类别（IMBoy JWT——handler 的 self 分支），不声明 org 级
%% function/permission：各 Org 的 member/assignment/seat 事实由 application
%% 聚合时逐 Org 复核（cs_seat_app:seat_contexts）。route metadata 仍标
%% cs_seat（五类 principal 内，凭证类别互斥语义不变）。
self_auth() ->
    #{auth_context => cs_seat}.

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
%% Origin 头（handler 归一化后注入）；`request_host` 只来自 Host 头 +
%% X-Forwarded-Proto（CSD-BE-01S：同源放行判定的输入，handler 派生注入）；
%% `secret` 只来自专用头；其余键是 application 的 Ctx 注入面（HMAC 材料 /
%% 事实 fun / 端口 / 摘要 fun / 断言验证器 / TTL）——浏览器可写即等于把
%% 服务端事实交给客户端，一律「提供即 400」。
widget_server_derived() ->
    server_common() ++
        [
            contact_id,
            business_identity_id,
            created_by_user_id,
            workspace_id,
            origin,
            request_host,
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
        %% —— BE-S01a：坐席上下文清单（T-2 裁定后的坐席面入口；Org 数未知，
        %% org 不在路径也不在参数——作用域是「当前用户本人」，各 Org 的
        %% member/assignment/seat 事实由 application 聚合时逐 Org 复核）——
        {seat_contexts,
            entry(
                [
                    {<<"GET">>, seat_contexts, [], [], #{
                        %% 跨 Org 聚合：作用域是「当前用户本人」，无 workspace 键。
                        workspace => optional
                    }}
                ],
                self_auth(),
                server_common(),
                self
            )},
        %% 门店开会话（T-2 后 org 显式在路径）：shop key 主体；contact/conversation
        %% 由门店集成方给出，session 只挂同一个 enterprise conversation（§5.2）。
        %% path org 由 cs_auth 用 shop key digest 同语句证明（跨 Org 必失败）。
        {session_queue,
            with_case_auth(
                entry(
                    [
                        {<<"POST">>, open_session,
                            [{contact_id, tsid, required}, {conversation_id, tsid, required}], []},
                        %% CSB-02R：坐席队列视图（GET）——与 POST 门店开会话同路径
                        %% 按 method+auth_context 分流；workspace 可选（org-wide，
                        %% 显式给出则收窄）。坐席作用域由 cs_auth 在进用例前裁决。
                        %% CS-BE-02（队列摘要）：clock_unit => second（DF-6 同族）
                        %% ——本读面的 `at` 供 application 计算 waiting_seconds =
                        %% at − queued_at（epoch 秒）；毫秒量纲会把等待时长放大
                        %% 1000 倍。写路径不消费本 GET 的 `at`，量纲切换零外溢。
                        {<<"GET">>, seat_session_queue,
                            [{after_id, binary, optional}, {limit, binary, optional}], [], #{
                                workspace => optional,
                                clock_unit => second
                            }}
                    ],
                    shop_key_auth(),
                    server_common() ++ [created_by_user_id],
                    path
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
                    %% clock_unit => second（DF-6）：评分写路径的 `at` 进 store 的
                    %% `to_timestamp`（epoch 秒）；毫秒量纲会把 rating_at 污染成
                    %% 约 5.8 万年后（与 claim/close 同族，DF-4 同款机制）。
                    {<<"POST">>, rate, [{rating, int, required}, {expected_version, int, required}],
                        [{id, session_id}], #{clock_unit => second}}
                ],
                visit_auth(),
                server_common() ++ [contact_id, business_identity_id],
                param
            )},
        %% 坐席接单：business_identity_id 服务端派生（坐席只能以**本人**身份接单）。
        %% T-2 裁定：路径显式 org_id，cs_auth 逐字校验 path org == 坐席 active
        %% member org；session.org_id 由 store 同语句裁决（跨 Org not_found）。
        {session_claim,
            entry(
                [
                    %% clock_unit => second（DF-6）：claimed_at/updated_at 的
                    %% `to_timestamp` 以秒为量纲，毫秒输入即时间戳写污染。
                    {<<"POST">>, claim, [{expected_version, int, required}], [{id, session_id}], #{
                        clock_unit => second
                    }}
                ],
                seat_auth(<<"conversation.write">>),
                server_common() ++ [business_identity_id],
                path
            )},
        {session_transfer,
            entry(
                [
                    %% clock_unit => second（DF-6R，DF-6 同族收尾）：转接写路径的
                    %% `at` 进 SQL_TRANSFER_UPDATE 的 updated_at = to_timestamp
                    %% （epoch 秒）；毫秒量纲会把 updated_at 污染成约 5.8 万年后
                    %% （与 claim/close/rate 同源）。
                    {<<"POST">>, transfer,
                        [{to_identity_id, tsid, required}, {expected_version, int, required}],
                        [
                            {id, session_id}
                        ],
                        #{clock_unit => second}}
                ],
                seat_auth(<<"conversation.write">>),
                server_common(),
                path
            )},
        {session_close,
            entry(
                [
                    %% clock_unit => second（DF-6）：同 session_claim——closed_at/
                    %% updated_at 的 `to_timestamp` 以秒为量纲（实证残留行
                    %% closed_at=58691-02-01）。
                    {<<"POST">>, close,
                        [{expected_version, int, required}, {reason, binary, optional}],
                        [
                            {id, session_id}
                        ],
                        #{clock_unit => second}}
                ],
                seat_auth(<<"conversation.write">>),
                server_common(),
                path
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
        %% CSB-03：坐席会话详情（GET；坐席 JWT + conversation.read；T-2 后路径
        %% 显式 org_id）——坐席侧单会话读，与平台面 p_session 共用同一
        %% fetch_session 用例（不复制逻辑）。
        {session_detail,
            entry(
                [{<<"GET">>, seat_session_detail, [], [{id, session_id}]}],
                seat_auth(<<"conversation.read">>),
                server_common() ++ [business_identity_id],
                path
            )},
        %% CS-BE-03（CS-DEC-01 冻结）：坐席客户上下文只读投影——白名单**仅限**
        %% 掩码名 / 来源 / first/last seen / 同 Org 历史会话 / 授权备注事实；
        %% 电话/邮箱/原始外部身份/密文/凭证/object key 永不进本响应。
        %% after_id/limit 作用于历史会话页（binary 透传，application 校验——
        %% seat_session_list 同款）。
        {session_customer_context,
            entry(
                [
                    {<<"GET">>, session_customer_context,
                        [{after_id, binary, optional}, {limit, binary, optional}], [
                            {id, session_id}
                        ]}
                ],
                seat_auth(<<"conversation.read">>),
                server_common() ++ [business_identity_id],
                path
            )},
        %% CS-BE-04（CS-DEC-02）：会话已读游标——同路径双方法（session_queue
        %% 同款分流）：GET = 读状态（游标 + 未读数，conversation.read）；
        %% POST = ACK（单调前进、不可回退，重复/乱序幂等；conversation.write）。
        %% `last_read_message_id` 是路径外唯一参数；`at` 是服务端派生时钟
        %% （clock_unit => second，写路径 to_timestamp 量纲，客户端不可报时）。
        {session_read_cursor,
            with_case_auth(
                entry(
                    [
                        {<<"GET">>, session_read_state, [], [{id, session_id}]},
                        {<<"POST">>, session_read_ack, [{last_read_message_id, tsid, required}],
                            [{id, session_id}], #{
                                clock_unit => second
                            }}
                    ],
                    seat_auth(<<"conversation.read">>),
                    server_common() ++ [business_identity_id],
                    path
                ),
                #{<<"POST">> => seat_auth(<<"conversation.write">>)}
            )},
        %% CS-BE-06（CS-DEC-03）：席位 entitlement 治理——PUT 配置/清除 limit
        %%（seat_limit 正整数；缺省=清除→unlimited）；GET 额度视图
        %%（seat_limit n|unlimited + used 现算计数）。治理权限
        %% enterprise_owner_admin（owner/admin，不因业务身份自动获得）。
        %% CS-BE-07 对账修正：去掉冗余的 with_case_auth（PUT 覆盖与默认
        %% auth 逐字相同——case_auth 只允许「收窄」，同值重复声明被
        %% cs_route_contract_tests 的 case_auth_same_as_default 审计拒绝）。
        {seat_limit_governance,
            entry(
                [
                    {<<"PUT">>, seat_limit_set, [{seat_limit, integer, optional}], []},
                    {<<"GET">>, seat_limit_view, [], []}
                ],
                governance_auth(),
                server_common(),
                path
            )},
        %% CS-BE-07（按需统计）：治理面只读——GET 统计视图（date 缺省 =
        %% 服务端时钟 UTC 当日；tz_offset 缺省 0，界 ±840 分钟；显式窗口
        %% epoch 秒回显，不依赖 DB 时区）。workspace 可选（缺省 org-wide）。
        %% 指标：new_sessions / first_response{count,avg_seconds} /
        %% closed_sessions / rating{count,avg} / current{queued,active}。
        %% DEFECT-2（CS-INT-03 发现）：此前漏 clock_unit => second，at 以毫秒
        %% 注入——统计窗口换算错 1000 倍（缺省日推导到远未来，窗口 epoch 为
        %% 毫秒量纲）。与 read-cursor/queue/heartbeat 的 DF-6 同族；两面同步修。
        {session_stats,
            entry(
                [
                    {<<"GET">>, session_stats,
                        [
                            {date, binary, optional},
                            {tz_offset, int, optional}
                        ],
                        [], #{
                            workspace => optional,
                            clock_unit => second
                        }}
                ],
                governance_auth(),
                server_common(),
                path
            )},
        %% CS-BE-05（CS-DEC-02）：presence 心跳 lease——POST /seats/me/heartbeat
        %% 刷新心跳 lease 并返回派生运行态；PUT /seats/me/presence 设置/清除
        %% 手动 away（manual_status 缺省 = clear）。`at` 是服务端派生时钟
        %% （clock_unit => second，客户端不可报时）；enabled 门在 application
        %% （suspend 立即 seat_disabled）。心跳不是会话写——用
        %% conversation.read 权限（只写自身 presence 行，不碰会话事实）。
        {seat_presence_heartbeat,
            entry(
                [
                    {<<"POST">>, seat_heartbeat, [], [], #{clock_unit => second}}
                ],
                seat_auth(<<"conversation.read">>),
                server_common() ++ [business_identity_id],
                path
            )},
        {seat_presence_status,
            with_case_auth(
                entry(
                    [
                        {<<"PUT">>, seat_manual_status, [{manual_status, binary, optional}], [], #{
                            clock_unit => second
                        }},
                        {<<"GET">>, seat_presence, [], []}
                    ],
                    seat_auth(<<"conversation.read">>),
                    server_common() ++ [business_identity_id],
                    path
                ),
                #{<<"PUT">> => seat_auth(<<"conversation.write">>)}
            )},
        %% Org 级运行态列表（工作台/管理面；与自动派单同一派生真源）。
        %% DEFECT（CS-INT-02 发现）：此前漏声明 clock_unit => second，at 以
        %% 毫秒缺省注入，presence TTL（last_heartbeat_at 是 epoch 秒）判定
        %% 恒 offline——真实集成门实证：心跳当秒 list 仍 offline。与
        %% seat_presence_heartbeat / seat_presence_status 同口径补 second。
        {seat_presence_list,
            entry(
                [
                    {<<"GET">>, seat_presence_list, [], [], #{
                        clock_unit => second
                    }}
                ],
                seat_auth(<<"conversation.read">>),
                server_common() ++ [business_identity_id],
                path
            )},
        %% CSB-02R：坐席 active/closed 两视图（T-2 后
        %% GET /api/v1/cs/organizations/:org_id/seats/sessions）。
        %% 独立路径的理由：GET /api/v1/cs/sessions 已冻结为访客面（cs_visit，
        %% route metadata 是 principal 的唯一分流依据，同方法双主体必须换路径）。
        %% queued 视图冻结在 sessions/queue（GET，case_auth 分流），此处 status
        %% 显式必填且仅接受 active|closed（application 复核）。
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
                path
            )},
        %% —— BE-S01a（api-surface-freeze）：转接目标最小投影——同 Org 其他
        %% 可用坐席（identity id / 显示名 / 可用状态），无 owner/admin 权限要求，
        %% 不复用治理 identity 列表。排除调用者本人（application 以认证派生的
        %% business_identity_id 为准，客户端不可申报）。
        {transfer_targets,
            entry(
                [
                    {<<"GET">>, transfer_targets,
                        [{after_id, binary, optional}, {limit, binary, optional}], [], #{
                            %% 转接目标是 Org 级最小投影（不按 workspace 收窄）。
                            workspace => optional
                        }}
                ],
                seat_auth(<<"conversation.read">>),
                server_common() ++ [business_identity_id],
                path
            )},
        %% —— BE-S01a：坐席 SSE 事件流（T-2 裁定路由族成员；sse-event-contract
        %% 的流式实现在 BE-S01b）。占位动作：handler 照常解析 workspace_id（查询
        %% 串必填）并走完整坐席认证，facade 返回 not_implemented → HTTP 501
        %% （客户端可探测能力，不误判路由缺失 404）。
        {seat_events,
            entry(
                [
                    {<<"GET">>, seat_events,
                        [
                            {after_id, tsid, optional}
                        ],
                        []}
                ],
                seat_auth(<<"conversation.read">>),
                server_common() ++ [business_identity_id],
                path
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
                %% clock_unit => second（DF-4）：吊销写路径的 `at` 进 store 的
                %% `to_timestamp`（epoch 秒）；毫秒量纲会把 revoked_at 污染成
                %% 约 5.8 万年后 → 吊销判定永不命中（fail-open）。
                [{<<"POST">>, revoke_shop_key, [], [{id, id}], #{clock_unit => second}}],
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
                %% clock_unit => second（DF-4）：同 shop_key_revoke——revoked_at
                %% 的 to_timestamp 以秒为量纲，毫秒输入即吊销 fail-open。
                [{<<"POST">>, revoke_visit_token, [], [{id, id}], #{clock_unit => second}}],
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
%% Org 来源（CSD-BE-01R/01S，hosted-widget-contract S3 v1.1）：**全部
%% 浏览器动作面零 org 申报**——bootstrap 由 public_widget_id 全局反查的
%% 命中行权威派生；持 token 动作面（sessions/messages/events/rating/assets/
%% identity_exchange）由 (installation_id, secret) 的 digest **全局命中行**
%% 派生（token 行本就绑定 (org, installation)，digest 命中无枚举面），
%% `organization_id` 在全部面上客户端提供即 400。仅两个例外保留 param：
%% 旧 frame（S4 兼容窗口，路径带 organization_id 查询参数原样保留）与
%% widget_asset_put（无 token 面——presign 下发的裸 PUT URL 携带服务端
%% 签发的 organization_id，同值回传语义，非浏览器申报）。
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
                %% organization_id 是 bootstrap 面的服务端派生键（S3 零申报面）。
                widget_server_derived() ++ [organization_id],
                derived
            )},
        {widget_identity_exchange,
            widget_token_entry(
                [
                    {<<"POST">>, widget_identity_exchange,
                        [
                            {installation_id, tsid, required},
                            {assertion, map, required}
                        ],
                        []}
                ]
            )},
        %% BE-W01（router wiring manifest W-1）：动态 frame HTML。handler 自行
        %% 解析参数（不经 cs_actions 的 dispatch——零凭证导航面）；此处登记
        %% 只为动作表/路由表/契约测试三方一致。
        {widget_frame_html,
            widget_entry(
                [
                    {<<"GET">>, widget_frame_html, [{installation_id, tsid, required}], []}
                ],
                widget_auth(),
                widget_server_derived(),
                param
            )},
        %% CSD-BE-01（hosted-widget-contract S3/S4）：/w/:public_widget_id 动态
        %% frame HTML（iframe src 新落点）。handler 自行解析路径绑定（不经
        %% cs_actions 的 dispatch——零凭证导航面）；此处登记只为动作表/路由表/
        %% 契约测试三方一致。租户归属是命中行的派生输出（public_widget_id
        %% 全局反查），浏览器零 org/workspace 申报面（CSD-BE-01S：org_source
        %% 如实登记为 derived）。
        {widget_public_frame_html,
            widget_token_entry(
                [
                    {<<"GET">>, widget_public_frame_html, [{public_widget_id, binary, required}],
                        []}
                ]
            )},
        %% 会话建立（POST）与访客会话列表（GET）同路径动作（cowboy 只按 path
        %% 匹配——seats/shop-keys 同款先例）。
        {widget_sessions,
            widget_token_entry(
                [
                    {<<"POST">>, widget_create_session, [{installation_id, tsid, required}], []},
                    {<<"GET">>, widget_list_sessions, [{installation_id, tsid, required}], []}
                ]
            )},
        {widget_session_messages,
            widget_token_entry(
                [
                    {<<"GET">>, widget_history_after,
                        [
                            {installation_id, tsid, required},
                            {after_id, tsid, optional},
                            {limit, int, optional}
                        ],
                        [{id, session_id}]},
                    %% BE-PATCH-01（attachment-state-machine append_message）：
                    %% `asset_ids` = TSID string 数组（list 传输形态沿用
                    %% allowed_origins 先例；TSID 投影在应用桥接层做——空正文 +
                    %% asset_ids 是合法附件消息，body 由此改 optional，纯文本
                    %% 消息仍要求非空 body 由应用层裁决）。幂等 = 同一
                    %% client_msg_id + asset_ids 重放返回同一 message（企业面
                    %% canonical 事务冻结语义，widget 只桥接）。
                    {<<"POST">>, widget_visitor_message,
                        [
                            {installation_id, tsid, required},
                            {body, binary, optional},
                            {client_msg_id, binary, required},
                            {asset_ids, list, optional}
                        ],
                        [{id, session_id}]}
                ]
            )},
        %% SSE 事件流（GET）：流式响应由 cs_widget_handler 专用分支承担；
        %% 动作表声明的是补偿读语义（Last-Event-ID / after_id → 历史 after 游标），
        %% facade 与普通历史同源（widget_history_after）。
        {widget_session_events,
            widget_token_entry(
                [
                    {<<"GET">>, widget_history_after,
                        [
                            {installation_id, tsid, required},
                            {after_id, tsid, optional},
                            {limit, int, optional}
                        ],
                        [{id, session_id}]}
                ]
            )},
        {widget_asset_upload,
            widget_token_entry(
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
                            {object_hash, binary, required},
                            %% CS-BE-01：展示文件名（可选；随凭证进 PUT 登记
                            %% 与历史 assets[].file_name 投影）。
                            {file_name, binary, optional}
                        ],
                        [{id, session_id}]}
                ]
            )},
        {widget_asset_confirm,
            widget_token_entry(
                [
                    {<<"POST">>, widget_asset_confirm,
                        [
                            {installation_id, tsid, required},
                            {upload_ref, binary, required}
                        ],
                        [{id, session_id}]}
                ]
            )},
        %% BE-PATCH-01：字节上传代理（POST .../assets/upload）。upload_ref 是
        %% 唯一凭证（FE 裸 PUT 合同：无凭证头/Cookie，URL 查询串携带申报键），
        %% **不要求** visit token 头；payload=请求体字节，不经 JSON 参数表——
        %% 线格式分支在 cs_widget_handler（asset_content 同款先例），handler 取
        %% 原始体注入 payload 键。鉴权链：installation active → 会话事实（服务
        %% 端派生 contact）→ upload_ref open（过期/篡改/同上传人）→ contact
        %% 会话归属门（全部复用 eb_asset_app:put_object 既有实现）。
        {widget_asset_put,
            widget_entry(
                [
                    {<<"POST">>, widget_asset_put,
                        [
                            {installation_id, tsid, required},
                            {upload_ref, binary, required}
                        ],
                        [{id, session_id}]},
                    %% P1-E2E-01 实证缺陷修复：presign 回显 upload.method=PUT
                    %% （with_upload_url），浏览器按合同发裸 PUT，而动作表只登记
                    %% POST → PUT 一律 405，FE-W01 附件链必炸。PUT 与 POST 同参
                    %% 同用例，方法门放行两形态（鉴权/绑定门不变）。
                    {<<"PUT">>, widget_asset_put,
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
        %% BE-S01b（api-surface-freeze widget_apis）：访客附件内容代理（GET）。
        %% 响应是对象字节本体（mime 定 content-type），不走 cs_http:respond 的
        %% JSON 面——线格式分支在 cs_widget_handler；此处动作表登记的是解析/
        %% 认证/参数投影契约。asset_id 是路径绑定（服务端解析会话外作用域）。
        {widget_asset_content,
            widget_token_entry(
                [
                    {<<"GET">>, widget_asset_content, [{installation_id, tsid, required}], [
                        {id, session_id}, {asset, asset_id}
                    ]}
                ]
            )},
        {widget_session_rating,
            widget_token_entry(
                [
                    {<<"POST">>, widget_rate,
                        [
                            {installation_id, tsid, required},
                            {rating, int, required},
                            {expected_version, int, required}
                        ],
                        [{id, session_id}]}
                ]
            )}
    ];
%% ===================================================================
%% 平台运营面（§5.3）：每条路径显式带 :org_id + workspace_id 必填；
%% 与租户面共用同一 application（CS-02-A02：不复制业务逻辑）。
%% ===================================================================
table(platform) ->
    [
        %% 平台运营面坐席分页（跨企业）：`/api/adm/customer-service/seats`，
        %% organization_id 是可选过滤（org_source=param_optional，缺失 = 全局，
        %% cs_http 传 OrgId=0 占位）；workspace 可选（坐席是 Org 级事实，
        %% workspace_id 仅 suspend/resume 审计事件需要，行投影带默认 Workspace）。
        %% 与 p_seats 的差别：不按 enabled 过滤（运营面要能定位并恢复已停用坐席），
        %% 投影带 organization_name / display_name。
        {p_platform_seats, (platform_entry(
            [
                {<<"GET">>, list_platform_seats,
                    [{after_id, binary, optional}, {limit, binary, optional}], [], #{
                        workspace => optional
                    }}
            ],
            platform_auth(<<"customer_service:read">>)
        ))#{
            org_source => param_optional
        }},
        {p_seats,
            platform_entry(
                [
                    {<<"GET">>, list_dispatchable_seats,
                        [{after_id, binary, optional}, {limit, binary, optional}], []}
                ],
                platform_auth(<<"customer_service:read">>)
            )},
        %% BE-S01b（api-surface-freeze admin_provisioning）：平台面事务化开通/
        %% 修复坐席（identity + assignment + enabled seat 单事务 + 不可抵赖审计；
        %% 幂等）。workspace_id 仍是 handler 强制的 face 级必填（Derived 注入）；
        %% adm_user_id 是认证派生键（进审计 detail，客户端提供即 400）。
        {p_seat_provision,
            platform_entry(
                [
                    {<<"POST">>, provision_seat,
                        [
                            {user_id, tsid, required},
                            {display_name, binary, required},
                            {max_concurrent, int, optional}
                        ],
                        []}
                ],
                platform_auth(<<"customer_service:write">>)
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
        %% CS-ADM-02（CS-GOV-03B）：平台运营面按需统计——只读 GET 统计视图。
        %% 复用 CS-BE-07 租户面同一 session_stats facade 与窗口语义
        %% （date 缺省=UTC 当日 + tz_offset 缺省 0，界 ±840 分钟；显式窗口
        %% epoch 秒回显）；workspace 可选（缺省 org-wide，与租户面一致）。
        %% 零预聚合零缓存——Admin UI 只投影服务端事实，不客户端重算。
        {p_session_stats,
            platform_entry(
                [
                    {<<"GET">>, session_stats,
                        [
                            {date, binary, optional},
                            {tz_offset, int, optional}
                        ],
                        [], #{
                            workspace => optional,
                            clock_unit => second
                        }}
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

%% CSD-BE-01S（hosted-widget-contract S3 v1.1）：持 token 动作面的统一构造
%% ——org_source=derived（Org 由 token digest 全局命中行服务端派生），
%% `organization_id` 与其余服务端派生键一样客户端提供即 400
%% `server_derived_key_rejected`（浏览器零申报面）。
widget_token_entry(Cases) ->
    widget_entry(
        Cases, widget_auth(), widget_server_derived() ++ [organization_id], derived
    ).

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
