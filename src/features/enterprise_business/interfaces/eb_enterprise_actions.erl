%%% @doc 企业业务 HTTP 动作表（EB-09 §5 的**冻结契约**，纯数据 + 纯函数）。
%%%
%%% 依据：plan v4.1 §5.1（租户 Enterprise API 最小动作集）、§5.3（平台 Admin API）、
%%% §5.4（必测负例）、EB-09-A01/A03/A04/A05/A06。
%%%
%%% ## 为什么「路径动作 + 方法用例」两层
%%%
%%% cowboy **只按 path 匹配**（同路径登记两条会互相遮蔽，且 `contract_gate` 直接判
%%% 重复路由为致命错误）。而 §5.1 有 4 条路径同时承载读与写（`business-identities`
%%% 的 POST/GET、`contacts` 的 GET/POST、`contacts/:id` 的 GET/PATCH、
%%% `conversations/:id/messages` 的 GET/POST）。因此本表的一行是**一条路径动作**，
%%% 其下按 HTTP 方法分派到**用例**：
%%%
%%%     action（路径级，进 route metadata，是 principal 的唯一来源）
%%%       └── cases: [{方法, facade 函数, 参数表, 路径参数表}]
%%%
%%% 授权需求挂在**路径级**（route metadata 同样是路径级，两者必须逐字一致）；
%%% 同一路径的多个方法共享一个 principal 类别与一个权限门——这是 V1 的显式取舍，
%%% 已登记在 findings（EB-09-F1：装配事实的权限粒度比 §5.4 词汇表粗）。
%%%
%%% ## 表表达的三条硬红线（可机械核对）
%%%
%%%   * `delivery_only => true`（企业 message ACK）：**只**调 delivery 用例，响应
%%%     不得含删除/归档语义（A06）；
%%%   * `proxy_content => true`（asset content）：经 facade 取流并流式返回，响应**不**
%%%     含 object key / storage endpoint / presigned（A05）；
%%%   * 每个平台动作的路径都带 `:org_id`，且 `workspace_id` 为必填参数——不存在
%%%     「不带 Org 的全局列举」（A04）。
%%%
%%% **本模块不做**：不解析请求、不读库、不判权限、不拼响应。
-module(eb_enterprise_actions).

-export([
    tenant/1,
    platform/1,
    tenant_actions/0,
    platform_actions/0,
    find/2,
    case_for/2,
    param_keys/1,
    feature/0,
    tenant_prefix/0,
    platform_prefix/0,
    surface_required/0
]).

-export_type([owner/0, ptype/0, param/0, kase/0, entry/0]).

-type owner() :: tenant | platform.
%% `tsid` = 64-bit TSID：**传输层是 JSON/path 字符串**，投影成 integer 交给 application
%% （A02：出站再编回 string）。
-type ptype() :: tsid | int | binary.
-type param() :: {atom(), ptype(), required | optional}.
-type kase() :: #{
    method := binary(),
    facade := atom(),
    params := [param()],
    path_params := [{atom(), atom()}]
}.
-type entry() :: #{
    action := atom(),
    owner := owner(),
    cases := [kase()],
    auth := map(),
    delivery_only := boolean(),
    proxy_content := boolean()
}.

%% 企业租户面前缀（`eb_auth_principal` 的租户面同口径）。
-define(TENANT_PREFIX, <<"/api/v1/enterprise">>).
%% 平台运营面前缀（imboyadmin 只走 /api/adm）。
-define(PLATFORM_PREFIX, <<"/api/adm/enterprise-business">>).

%% @doc route metadata 里自报的 feature 键（A01 的 `feature` 一致性）。
-spec feature() -> atom().
feature() ->
    enterprise_business.

-spec tenant_prefix() -> binary().
tenant_prefix() ->
    ?TENANT_PREFIX.

-spec platform_prefix() -> binary().
platform_prefix() ->
    ?PLATFORM_PREFIX.

%% @doc 面级 route 元数据的**注入**键（由 router 的装配点统一注入；缺一即 500
%% fail-closed，不降级为「不要求企业授权」）。
-spec surface_required() -> [atom()].
surface_required() ->
    [surface, feature, auth_facts].

%% ===================================================================
%% 查表
%% ===================================================================

-spec tenant(atom()) -> {ok, entry()} | {error, {unknown_action, atom()}}.
tenant(Action) ->
    find(tenant, Action).

-spec platform(atom()) -> {ok, entry()} | {error, {unknown_action, atom()}}.
platform(Action) ->
    find(platform, Action).

-spec find(owner(), atom()) -> {ok, entry()} | {error, {unknown_action, atom()}}.
find(Owner, Action) ->
    case lists:keyfind(Action, 1, table(Owner)) of
        {Action, Entry} -> {ok, Entry#{action => Action}};
        false -> {error, {unknown_action, Action}}
    end.

-spec tenant_actions() -> [atom()].
tenant_actions() ->
    [A || {A, _} <- table(tenant)].

-spec platform_actions() -> [atom()].
platform_actions() ->
    [A || {A, _} <- table(platform)].

%% @doc 路径级动作 + HTTP 方法 → 用例。未登记的方法返回 `{error, method_not_allowed}`
%% （**不**静默当 POST，也不落到默认用例）。
-spec case_for(entry(), binary()) -> {ok, kase()} | {error, method_not_allowed}.
case_for(Entry, Method) ->
    case [C || C <- maps:get(cases, Entry), maps:get(method, C) =:= Method] of
        [Case | _] -> {ok, Case};
        [] -> {error, method_not_allowed}
    end.

%% @doc 该路径动作的全部参数键（正文键名白名单，用于 JSX binary→atom 规范化）。
-spec param_keys(entry()) -> [atom()].
param_keys(Entry) ->
    lists:usort([
        K
     || Case <- maps:get(cases, Entry),
        {K, _Type, _Req} <- maps:get(params, Case)
    ]).

%% ===================================================================
%% 租户面（§5.1 逐条；权限取自路径的主语义，共享路径取更严的一侧）
%% ===================================================================

table(tenant) ->
    [
        %% 业务身份（POST 创建 / GET 列举）：身份管理是治理动作，两侧同为治理门。
        %% FND-1（RULING-2026-09-15 §五）：member_auth(org.manage) 会把「建身份」
        %% 挂在「已有 sales assignment」上（owner 也过不了 assignment 门，空 Org
        %% 自举死锁）—— 改为 governance_auth()，与路由侧同一定义。
        {business_identities,
            entry(
                tenant,
                [
                    {<<"POST">>, create_identity,
                        [
                            {function_key, binary, required},
                            {display_name, binary, required}
                        ],
                        []},
                    {<<"GET">>, list_identities,
                        [{after_id, tsid, optional}, {limit, int, optional}], []}
                ],
                governance_auth(),
                false,
                false
            )},
        {assign_identity,
            entry(
                tenant,
                [{<<"POST">>, bind_assignment, [{user_id, tsid, required}], [{id, identity_id}]}],
                governance_auth(),
                false,
                false
            )},
        %% 企业客户（GET 列举 / POST 建档）：共享路径取更严的写侧语义不可满足，
        %% 故 V1 以 contact.read 为门（见 findings EB-09-F1）。
        {contacts,
            entry(
                tenant,
                [
                    {<<"GET">>, list_contacts, [{after_id, tsid, optional}, {limit, int, optional}],
                        []},
                    {<<"POST">>, create_contact,
                        [
                            {channel, binary, required},
                            {subject, binary, required},
                            {display_name, binary, optional},
                            {imboy_user_id, tsid, optional},
                            {subject_mask, binary, optional}
                        ],
                        []}
                ],
                member_auth(<<"contact.read">>),
                false,
                false
            )},
        {contact_detail,
            entry(
                tenant,
                [
                    {<<"GET">>, get_contact, [], [{id, contact_id}]},
                    {<<"PATCH">>, update_contact,
                        [
                            {display_name, binary, optional},
                            {profile_plaintext, binary, optional},
                            {profile_cipher, binary, optional},
                            {profile_key_version, int, optional}
                        ],
                        [{id, contact_id}]}
                ],
                member_auth(<<"contact.read">>),
                false,
                false
            )},
        {append_note,
            entry(
                tenant,
                [
                    {<<"POST">>, append_note,
                        %% FND-5：只收业务明文（服务端加密）；body_cipher/body_key_version
                        %% 已从 HTTP 面删除，客户端提交即 422（unknown_param/forbidden）。
                        [
                            {body_plaintext, binary, required}
                        ],
                        [{id, contact_id}]}
                ],
                member_auth(<<"note.write">>),
                false,
                false
            )},
        {open_conversation,
            entry(
                tenant,
                [
                    {<<"POST">>, open_conversation,
                        [{contact_id, tsid, required}, {business_identity_id, tsid, required}], []}
                ],
                member_auth(<<"conversation.write">>),
                false,
                false
            )},
        {conversation_messages,
            entry(
                tenant,
                [
                    {<<"GET">>, list_messages, [{after_id, tsid, optional}, {limit, int, optional}],
                        [{id, conversation_id}]},
                    {<<"POST">>, append_message,
                        [
                            {client_msg_id, binary, required},
                            {sender_type, binary, required},
                            {body, binary, required},
                            {contact_id, tsid, optional},
                            {identity_id, tsid, optional}
                        ],
                        [{id, conversation_id}]}
                ],
                member_auth(<<"conversation.read">>),
                false,
                false
            )},
        %% A06：企业 message ACK **只**写 delivery（delivery_only = true）。
        {ack_delivery,
            entry(
                tenant,
                [
                    {<<"POST">>, ack_delivery,
                        [{recipient_ref, binary, required}, {device_id, binary, optional}], [
                            {id, conversation_id}, {message_id, message_id}
                        ]}
                ],
                member_auth(<<"message.write">>),
                true,
                false
            )},
        {presign,
            entry(
                tenant,
                [
                    {<<"POST">>, request_presign,
                        [
                            {conversation_id, tsid, required},
                            {mime, binary, required},
                            {size_bytes, int, required},
                            {object_hash, binary, required},
                            {message_id, tsid, optional},
                            {business_identity_id, tsid, optional},
                            {retain_until, int, optional}
                        ],
                        []}
                ],
                member_auth(<<"asset.write">>),
                false,
                false
            )},
        {confirm_asset,
            entry(
                tenant,
                [{<<"POST">>, confirm_asset, [{upload_ref, binary, required}], []}],
                member_auth(<<"asset.write">>),
                false,
                false
            )},
        %% A05：content 经 facade 取流（proxy_content = true），响应不含存储侧引用。
        {asset_content,
            entry(
                tenant,
                [{<<"GET">>, content_stream, [], [{id, asset_id}]}],
                member_auth(<<"asset.read">>),
                false,
                true
            )},
        {suspend_member,
            entry(
                tenant,
                [
                    {<<"POST">>, suspend_member, [{reason, binary, required}], [
                        {uid, member_user_id}
                    ]}
                ],
                governance_auth(),
                false,
                false
            )},
        {offboarding_open,
            entry(
                tenant,
                [
                    {<<"POST">>, open_offboarding,
                        [
                            {leaver_user_id, tsid, required},
                            {successor_user_id, tsid, required},
                            {reason, binary, optional}
                        ],
                        []}
                ],
                governance_auth(),
                false,
                false
            )},
        {offboarding_execute,
            entry(
                tenant,
                [
                    {<<"POST">>, execute_offboarding, [{expected_version, int, required}], [
                        {id, case_id}
                    ]}
                ],
                governance_auth(),
                false,
                false
            )},
        {offboarding_verify,
            entry(
                tenant,
                [{<<"POST">>, verify_offboarding, [], [{id, case_id}]}],
                governance_auth(),
                false,
                false
            )},
        {offboarding_finalize,
            entry(
                tenant,
                [{<<"POST">>, finalize_offboarding, [], [{id, case_id}]}],
                governance_auth(),
                false,
                false
            )}
    ];
table(platform) ->
    [
        %% §5.3：跨 Org 检索 / 只读详情 / 受 *:write 控制的纠错。
        %% **每条**平台路径都显式带 :org_id，且 workspace_id 为必填参数；表里不存在
        %% 「不带 Org 的全局列举」动作（A04）。
        {p_identities,
            platform_entry(
                p_identities,
                [
                    {<<"GET">>, list_identities,
                        [{after_id, tsid, optional}, {limit, int, optional}], []}
                ],
                read_auth(),
                false
            )},
        {p_contacts,
            platform_entry(
                p_contacts,
                [
                    {<<"GET">>, list_contacts, [{after_id, tsid, optional}, {limit, int, optional}],
                        []}
                ],
                read_auth(),
                false
            )},
        {p_contact_detail,
            platform_entry(
                p_contact_detail,
                [{<<"GET">>, get_contact, [], [{id, contact_id}]}],
                read_auth(),
                false
            )},
        {p_conversation_messages,
            platform_entry(
                p_conversation_messages,
                [
                    {<<"GET">>, list_messages, [{after_id, tsid, optional}, {limit, int, optional}],
                        [{id, conversation_id}]}
                ],
                read_auth(),
                false
            )},
        {p_message_detail,
            platform_entry(
                p_message_detail,
                [{<<"GET">>, fetch_message, [], [{message_id, message_id}]}],
                read_auth(),
                false
            )},
        %% 平台取流同样经 facade（proxy_content = true）。asset ACL 的判据是「请求者在本
        %% Org 的 active 经办身份 == 会话当前经办身份」，平台管理员不是 Org 成员，
        %% 故必须显式给出**被代理的组织责任人** actor_user_id，由 application 照常裁决。
        {p_asset_content,
            platform_entry(
                p_asset_content,
                [{<<"GET">>, content_stream, [{actor_user_id, tsid, required}], [{id, asset_id}]}],
                read_auth(),
                true
            )},
        %% 纠错写动作：平台侧没有 Org 成员身份，Core 要求 `actor_user_id` 是**本 Org 的**
        %% Owner/Admin（`organization_member_logic:suspend/3` 的 actor 契约），故把它列为
        %% 必填参数并在审计里原样留痕；它同时是平台写入的显式租户归属证明（A04）。
        {p_suspend_member,
            platform_entry(
                p_suspend_member,
                [
                    {<<"POST">>, suspend_member,
                        [{reason, binary, required}, {actor_user_id, tsid, required}], [
                            {uid, member_user_id}
                        ]}
                ],
                write_auth(),
                false
            )},
        {p_offboarding_execute,
            platform_entry(
                p_offboarding_execute,
                [
                    {<<"POST">>, execute_offboarding,
                        [{expected_version, int, required}, {actor_user_id, tsid, required}], [
                            {id, case_id}
                        ]}
                ],
                write_auth(),
                false
            )},
        {p_offboarding_verify,
            platform_entry(
                p_offboarding_verify,
                [
                    {<<"POST">>, verify_offboarding, [{actor_user_id, tsid, required}], [
                        {id, case_id}
                    ]}
                ],
                write_auth(),
                false
            )},
        {p_offboarding_finalize,
            platform_entry(
                p_offboarding_finalize,
                [
                    {<<"POST">>, finalize_offboarding, [{actor_user_id, tsid, required}], [
                        {id, case_id}
                    ]}
                ],
                write_auth(),
                false
            )}
    ].

%% 租户面路径动作构造：`action` 由 `find/2` 用表键补齐（表键就是路径动作名）。
entry(Owner, Cases, Auth, DeliveryOnly, ProxyContent) ->
    #{
        owner => Owner,
        cases => [kase(C) || C <- Cases],
        auth => Auth,
        delivery_only => DeliveryOnly,
        proxy_content => ProxyContent
    }.

kase({Method, Facade, Params, PathParams}) ->
    #{
        method => Method,
        facade => Facade,
        params => Params,
        path_params => PathParams
    }.

platform_entry(Action, Cases, Auth, ProxyContent) ->
    #{
        action => Action,
        owner => platform,
        cases => [kase(C) || C <- Cases],
        auth => Auth,
        delivery_only => false,
        proxy_content => ProxyContent
    }.

%% 企业成员类授权需求：职能类型 + 独立动作权限（function_key 不替代权限）。
member_auth(Permission) ->
    #{
        auth_context => enterprise_member,
        required_function => <<"sales">>,
        required_permission => Permission
    }.

%% 治理类授权需求：Owner/Admin 治理角色（**不**因持有业务身份自动获得）。
governance_auth() ->
    #{
        auth_context => enterprise_owner_admin,
        required_governance => [<<"owner">>, <<"admin">>]
    }.

read_auth() ->
    #{auth_context => platform_admin, required_permission => <<"enterprise_business:read">>}.

write_auth() ->
    #{auth_context => platform_admin, required_permission => <<"enterprise_business:write">>}.
