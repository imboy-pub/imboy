%%% @doc CS-02 的契约与形状套件（纯静态 + 纯函数，零 DB、零 HTTP socket）。
%%%
%%% 覆盖：
%%%   * **A01**：`imboy_router:get_routes/0` 里每一条客服路由的
%%%     method/path/action/auth_context/feature 与冻结动作表（`cs_actions`）逐字
%%%     一致；auth_context ∈ 五类 principal（`cs_auth:principals/0`）；
%%%     `imboy_feature:route_feature/3` 把两张面都归到 customer_service（运行时门
%%%     与编译期裁剪双保险的接线证明）。
%%%   * **A02**：租户面与平台面的**每个**用例动作都打到同一 facade 函数
%%%     （`cs_facade_call` 的调用点表 + facade 真实导出核对）——双管理面共用
%%%     application，无第二套实现。
%%%   * **A03（静态侧）**：裁剪接线三件套在位——router 的 ifdef helper、
%%%     生成器 FEATURE_BACKEND_MODULES 覆盖 customer_service 目录**逐项相等**、
%%%     route_feature 归属。
%%%   * **A04（静态侧）**：坐席动作的 metadata 声明了 required_function=customer_service
%%%     （cs_auth 的 seat 门按此判定，suspended seat actor 即时拒绝）。
%%%   * credential 面一致性：动作表的 principal（cs_visit/cs_shop_key）与
%%%     `cs_http:is_credential_surface_path/1` 双向核对（中间件免签/免 JWT 面
%%%     与 handler 认证语义不漂移）。
%%%   * Web 坐席面一致性：cs_seat 主体路径与
%%%     `cs_http:is_web_seat_surface_path/1` 双向核对（浏览器无设备签名密钥，
%%%     免签不免 JWT——2026-09-23 生产 902 修复的契约锚）。
%%%   * Org 来源一致性：路径带 `:org_id` 的动作 org_source=path，A0 冻结路径
%%%     org_source=param。
%%%   * 静态判据：handler/http/auth/facade_call 源码零 DB、零 apply/list_to_atom、
%%%     零 crypto、零个人附件 URL 能力。
%%%   * 所有静态断言带**非真空**证明：把输入换成「该断言要拦住的改动」副本，
%%%     同一审计必须报出违规。
-module(cs_route_contract_tests).

-include_lib("eunit/include/eunit.hrl").

-define(S, cs_test_support).
-define(TENANT_HANDLER, "src/features/customer_service/interfaces/cs_tenant_handler.erl").
-define(WIDGET_HANDLER, "src/features/customer_service/interfaces/cs_widget_handler.erl").
-define(PLATFORM_HANDLER, "src/features/customer_service/interfaces/cs_platform_handler.erl").
-define(HTTP_MODULE, "src/features/customer_service/interfaces/cs_http.erl").
-define(AUTH_MODULE, "src/features/customer_service/interfaces/cs_auth.erl").
-define(FACADE_CALL, "src/features/customer_service/interfaces/cs_facade_call.erl").
-define(SEAT_CONSOLE_HANDLER,
    "src/features/customer_service/interfaces/cs_seat_console_handler.erl"
).

%% ===================================================================
%% 冻结的路由清单（path → 动作/方法/五类 principal）；与 router 字面登记互为审计面
%% ===================================================================

tenant_literal_routes() ->
    O = <<"/api/v1/cs/organizations/:org_id">>,
    [
        %% BE-S01a（T-2 裁定）：坐席上下文清单——主体自身作用域（无 Org 键）。
        {<<"/api/v1/cs/me/seat-contexts">>, seat_contexts, [<<"GET">>], cs_seat},
        %% CSB-02R：同路径 method+auth_context 分流——POST=门店（route metadata
        %% 冻结主体），GET=坐席队列（case_auth 覆盖，审计见 entry_violations）。
        %% T-2 后 org 显式在路径（旧 /api/v1/cs/sessions/queue 已删）。
        {<<O/binary, "/sessions/queue">>, session_queue, [<<"GET">>, <<"POST">>], cs_shop_key},
        {<<"/api/v1/cs/sessions">>, visitor_sessions, [<<"GET">>], cs_visit},
        {<<"/api/v1/cs/sessions/:id/messages">>, session_messages, [<<"POST">>], cs_visit},
        {<<"/api/v1/cs/sessions/:id/rating">>, session_rating, [<<"POST">>], cs_visit},
        {<<O/binary, "/sessions/:id/claim">>, session_claim, [<<"POST">>], cs_seat},
        {<<O/binary, "/sessions/:id/transfer">>, session_transfer, [<<"POST">>], cs_seat},
        {<<O/binary, "/sessions/:id/close">>, session_close, [<<"POST">>], cs_seat},
        {<<"/api/v1/enterprise/conversations/:conversation_id/messages">>, conversation_messages,
            [<<"GET">>], cs_seat},
        %% CSB-03：坐席会话详情（GET；坐席 JWT + conversation.read；T-2 后 org
        %% 显式在路径）。
        {<<O/binary, "/sessions/:id">>, session_detail, [<<"GET">>], cs_seat},
        %% CS-BE-03（CS-DEC-01）：坐席客户上下文只读投影（白名单逐字冻结：
        %% 掩码名/来源/first/last seen/同 Org 历史会话/授权备注事实）。
        {<<O/binary, "/sessions/:id/context">>, session_customer_context, [<<"GET">>], cs_seat},
        %% CS-BE-04（CS-DEC-02）：会话已读游标——同路径双方法（GET 读状态 /
        %% POST ACK；route metadata 冻结主体 cs_seat，POST 权限由 case_auth
        %% 收窄到 conversation.write）。
        {
            <<O/binary, "/sessions/:id/read-cursor">>,
            session_read_cursor,
            [<<"GET">>, <<"POST">>],
            cs_seat
        },
        %% CSB-02R：坐席工作台 active/closed 两视图（T-2 后 org 显式在路径——
        %% 旧 /api/v1/cs/seats/sessions 已删）。
        {<<O/binary, "/seats/sessions">>, seat_session_list, [<<"GET">>], cs_seat},
        %% BE-S01a：转接目标最小投影（api-surface-freeze）。
        {<<O/binary, "/transfer-targets">>, transfer_targets, [<<"GET">>], cs_seat},
        %% BE-S01a：坐席 SSE 占位（流式实现在 BE-S01b；先注册 501）。
        {<<O/binary, "/seats/me/events">>, seat_events, [<<"GET">>], cs_seat},
        %% CS-BE-05（CS-DEC-02）：presence 心跳（POST）/ 手动状态与自身运行态
        %% 视图（PUT/GET）/ Org 级运行态列表（GET）；PUT 由 case_auth 收窄到
        %% conversation.write。
        {<<O/binary, "/seats/me/heartbeat">>, seat_presence_heartbeat, [<<"POST">>], cs_seat},
        {<<O/binary, "/seats/me/presence">>, seat_presence_status, [<<"PUT">>, <<"GET">>], cs_seat},
        {<<O/binary, "/seats/presence">>, seat_presence_list, [<<"GET">>], cs_seat},
        {<<O/binary, "/seats">>, seats, [<<"GET">>, <<"POST">>], enterprise_owner_admin},
        {<<O/binary, "/seats/:id/suspend">>, seat_suspend, [<<"POST">>], enterprise_owner_admin},
        {<<O/binary, "/seats/:id/resume">>, seat_resume, [<<"POST">>], enterprise_owner_admin},
        %% C2/C3（contracts-w2）：治理面列表 GET 与既有 POST 同路径动作（cowboy
        %% 只按 path 匹配，一行 = 一条路径动作，按方法分派用例——seats 同款先例）。
        {
            <<O/binary, "/shop-keys">>,
            shop_key_list,
            [<<"GET">>, <<"POST">>],
            enterprise_owner_admin
        },
        {
            <<O/binary, "/shop-keys/:id/revoke">>,
            shop_key_revoke,
            [<<"POST">>],
            enterprise_owner_admin
        },
        {
            <<O/binary, "/visit-tokens">>,
            visit_token_list,
            [<<"GET">>, <<"POST">>],
            enterprise_owner_admin
        },
        {
            <<O/binary, "/visit-tokens/:id/revoke">>,
            visit_token_revoke,
            [<<"POST">>],
            enterprise_owner_admin
        },
        %% CS-BE-06（CS-DEC-03）：席位 entitlement 治理——PUT 配置/清除
        %% seat_limit + GET 额度视图（本清单此前漏登记：路由/动作表已进而
        %% 冻结清单未同步，a01 双向对账红——CS-BE-07 补齐对账面）。
        {
            <<O/binary, "/seat-limit">>,
            seat_limit_governance,
            [<<"PUT">>, <<"GET">>],
            enterprise_owner_admin
        },
        %% CS-BE-07（按需统计）：治理面只读统计视图——GET date/tz_offset
        %% 显式窗口（零预聚合零缓存）。
        {<<O/binary, "/stats/sessions">>, session_stats, [<<"GET">>], enterprise_owner_admin}
    ].

platform_literal_routes() ->
    P = <<"/api/adm/customer-service/organizations/:org_id">>,
    [
        %% 平台运营面坐席分页（跨企业）：organization_id 可选过滤（缺失 =
        %% 全局，org_source=param_optional）；workspace 可选；含已停用坐席。
        {<<"/api/adm/customer-service/seats">>, p_platform_seats, [<<"GET">>], platform_admin},
        {<<P/binary, "/seats">>, p_seats, [<<"GET">>], platform_admin},
        %% BE-S01b（api-surface-freeze admin_provisioning）：事务化开通/修复坐席。
        {<<P/binary, "/provisioning">>, p_seat_provision, [<<"POST">>], platform_admin},
        %% C1（contracts-w2）：平台 session 列表（只读）。
        {<<P/binary, "/sessions">>, p_session_list, [<<"GET">>], platform_admin},
        %% CS-ADM-02（CS-GOV-03B）：平台运营面按需统计（只读；session_stats
        %% facade 与租户面 CS-BE-07 同一实现；workspace 可选）。
        {<<P/binary, "/stats/sessions">>, p_session_stats, [<<"GET">>], platform_admin},
        {<<P/binary, "/sessions/:id">>, p_session, [<<"GET">>], platform_admin},
        {<<P/binary, "/seats/:id/suspend">>, p_seat_suspend, [<<"POST">>], platform_admin},
        {<<P/binary, "/seats/:id/resume">>, p_seat_resume, [<<"POST">>], platform_admin},
        {<<P/binary, "/sessions/:id/transfer">>, p_session_transfer, [<<"POST">>], platform_admin},
        {<<P/binary, "/sessions/:id/close">>, p_session_close, [<<"POST">>], platform_admin},
        {<<"/api/adm/customer-service/widget-installations">>, p_widget_installations,
            [<<"GET">>, <<"POST">>], platform_admin},
        {<<"/api/adm/customer-service/widget-installations/:id">>, p_widget_installation_update,
            [<<"PUT">>], platform_admin},
        {<<"/api/adm/customer-service/widget-installations/:id/revoke">>,
            p_widget_installation_revoke, [<<"POST">>], platform_admin},
        %% seat-console-embed SC-BE：workspace 坐席工作台嵌入配置 CRUD
        %% （widget-installations 同款：列表 GET 与创建 POST 同路径动作）。
        {<<"/api/adm/customer-service/seat-consoles">>, p_seat_consoles, [<<"GET">>, <<"POST">>],
            platform_admin},
        {<<"/api/adm/customer-service/seat-consoles/:id">>, p_seat_console_update, [<<"PUT">>],
            platform_admin},
        {<<"/api/adm/customer-service/seat-consoles/:id/revoke">>, p_seat_console_revoke,
            [<<"POST">>], platform_admin}
    ].

%% CSB-03：widget 接入面（浏览器访客；principal 与访客同类——visit token 头）。
widget_literal_routes() ->
    W = <<"/api/v1/cs/widget">>,
    [
        {<<W/binary, "/bootstrap">>, widget_bootstrap, [<<"POST">>], cs_visit},
        {<<W/binary, "/identity/exchange">>, widget_identity_exchange, [<<"POST">>], cs_visit},
        {<<W/binary, "/sessions">>, widget_sessions, [<<"GET">>, <<"POST">>], cs_visit},
        {
            <<W/binary, "/sessions/:id/messages">>,
            widget_session_messages,
            [<<"GET">>, <<"POST">>],
            cs_visit
        },
        {<<W/binary, "/sessions/:id/events">>, widget_session_events, [<<"GET">>], cs_visit},
        {<<W/binary, "/sessions/:id/assets/presign">>, widget_asset_upload, [<<"POST">>], cs_visit},
        {
            <<W/binary, "/sessions/:id/assets/confirm">>,
            widget_asset_confirm,
            [<<"POST">>],
            cs_visit
        },
        %% BE-PATCH-01：访客附件字节上传代理（upload_ref 唯一凭证——FE 裸 PUT
        %% 合同；payload=请求体字节，线格式分支在 cs_widget_handler）。
        %% P1-E2E-01 实证：presign 回显 upload.method=PUT，动作表放行 POST+PUT
        %% 双形态（同参同用例），否则浏览器按合同发 PUT 一律 405。
        {
            <<W/binary, "/sessions/:id/assets/upload">>,
            widget_asset_put,
            [<<"POST">>, <<"PUT">>],
            cs_visit
        },
        %% BE-S01b（api-surface-freeze widget_apis）：访客附件内容代理（对象字节
        %% 本体响应；线格式分支在 cs_widget_handler）。
        {
            <<W/binary, "/sessions/:id/assets/:asset/content">>,
            widget_asset_content,
            [<<"GET">>],
            cs_visit
        },
        {<<W/binary, "/sessions/:id/rating">>, widget_session_rating, [<<"POST">>], cs_visit},
        %% BE-W01（router wiring manifest W-1）：动态 frame HTML（iframe src 落点，
        %% 零凭证面——principal 声明 cs_visit，嵌入策略由 frame-ancestors CSP 裁决）。
        {<<W/binary, "/frame/:installation_id">>, widget_frame_html, [<<"GET">>], cs_visit},
        %% CSD-BE-01（hosted-widget-contract S2/S4）：/w/:public_widget_id 动态
        %% frame HTML（iframe src 新落点；零凭证导航面——public_widget_id 全局
        %% 反查派生租户，浏览器零 org/workspace 申报面；兼容窗口内旧 frame 原样保留）。
        {<<"/w/:public_widget_id">>, widget_public_frame_html, [<<"GET">>], cs_visit},
        %% seat-console-embed SC-BE：/seat/:public_seat_console_id 坐席工作台
        %% 嵌入面（零凭证导航落点；租户由全局反查派生，浏览器零 org 申报面；
        %% XFO 豁免 / 免签直通经 imboy_route_shape:is_cs_seat_console_frame_path/1
        %% 单一真源登记）。
        {<<"/seat/:public_seat_console_id">>, seat_console_frame_html, [<<"GET">>], cs_visit}
    ].

%% ===================================================================
%% A01：method/path/action/auth_context/feature 一致 + 五类无混淆
%% ===================================================================

a01_route_metadata_matches_frozen_action_table_test() ->
    ?assertEqual([], violations(?S:cs_routes(all))).

violations(Routes) ->
    Known = lists:append([
        [
            {tenant, Path, Action, Methods, Principal}
         || {Path, Action, Methods, Principal} <- tenant_literal_routes()
        ],
        [
            {widget, Path, Action, Methods, Principal}
         || {Path, Action, Methods, Principal} <- widget_literal_routes()
        ],
        [
            {platform, Path, Action, Methods, Principal}
         || {Path, Action, Methods, Principal} <- platform_literal_routes()
        ]
    ]),
    Registered = [{Path, maps:get(action, Opts, undefined)} || {Path, _H, Opts} <- Routes],
    Missing = [
        {route_not_registered, Path, Action}
     || {_Surface, Path, Action, _Methods, _Principal} <- Known,
        not lists:member({Path, Action}, Registered)
    ],
    Missing ++ audit(Routes, Known, []).

audit([], _Known, Acc) ->
    lists:reverse(Acc);
audit([{Path, Handler, Opts} | Rest], Known, Acc) ->
    Action = maps:get(action, Opts, undefined),
    Surface = maps:get(surface, Opts, undefined),
    EntryResult = find_entry(Surface, Action),
    Violations = lists:flatten(route_violations(Path, Handler, Opts, Known, EntryResult)),
    audit(Rest, Known, lists:reverse(Violations) ++ Acc).

find_entry(_Surface, undefined) ->
    {error, {unknown_action, undefined}};
find_entry(tenant, Action) ->
    cs_actions:tenant(Action);
find_entry(widget, Action) ->
    cs_actions:widget(Action);
find_entry(platform, Action) ->
    cs_actions:platform(Action);
find_entry(_Surface, Action) ->
    {error, {unknown_surface, Action}}.

route_violations(Path, Handler, Opts, Known, EntryResult) ->
    Action = maps:get(action, Opts, undefined),
    Surface = maps:get(surface, Opts, undefined),
    ExpectedHandler =
        case Surface of
            platform -> cs_platform_handler;
            %% BE-W01：widget 面有多个 handler（frame HTML 是零凭证导航端点；
            %% cs_seat_console_handler 为 seat-console-embed SC-BE 的 /seat/ 面）。
            widget when Handler =:= cs_widget_frame_handler -> cs_widget_frame_handler;
            widget when Handler =:= cs_seat_console_handler -> cs_seat_console_handler;
            widget -> cs_widget_handler;
            _ -> cs_tenant_handler
        end,
    Base = [{path_not_frozen, Path} || not lists:keymember(Path, 2, Known)],
    [
        Base,
        [{wrong_handler, Path, Handler} || Handler =/= ExpectedHandler],
        [
            {feature_mismatch, Path, maps:get(feature, Opts, undefined)}
         || maps:get(feature, Opts, undefined) =/= cs_actions:feature()
        ],
        [
            {missing_surface_key, Path, K}
         || K <- cs_actions:surface_required(), not maps:is_key(K, Opts)
        ],
        [{surface_not_atom, Path, Surface} || not is_atom(Surface)],
        %% A01：principal 必须属于五类（无第六类、无拼错）。
        [
            {principal_not_in_five, Path, maps:get(auth_context, Opts, undefined)}
         || not lists:member(
                maps:get(auth_context, Opts, undefined), cs_auth:principals()
            )
        ],
        entry_violations(Path, Action, Opts, EntryResult)
    ].

entry_violations(_Path, _Action, _Opts, {error, Reason}) ->
    [{action_not_in_table, Reason}];
entry_violations(Path, Action, Opts, {ok, Entry}) ->
    Auth = maps:get(auth, Entry),
    [
        [
            {auth_context_mismatch, Path, maps:get(auth_context, Opts, undefined)}
         || maps:get(auth_context, Opts, undefined) =/= maps:get(auth_context, Auth)
        ],
        [
            {auth_key_mismatch, Path, K, maps:get(K, Opts, undefined), V}
         || {K, V} <- maps:to_list(Auth),
            maps:get(K, Opts, undefined) =/= V
        ],
        [
            {method_not_declared, Path, Action, M}
         || M <- route_methods(Opts), not lists:member(M, entry_methods(Entry))
        ]
    ] ++
        [
            {undeclared_action_for_path, Path, Action}
         || not lists:member({Path, Action}, path_actions())
        ] ++
        case_auth_violations(Path, Action, Entry).

%% CSB-02R：case_auth（同路径 method+auth_context 分流）的机械审计——
%% 覆盖方法必须已在该动作表登记、principal 必属五类、且完整 auth 配置
%% **不得**与 entry 默认值相同（完全相同才是无谓漂移面；同主体可按 method
%% 收窄为不同 permission）。
case_auth_violations(Path, Action, Entry) ->
    CaseAuth = maps:get(case_auth, Entry, #{}),
    Methods = entry_methods(Entry),
    DefaultAuth = maps:get(auth, Entry),
    lists:append([
        [
            {case_auth_method_not_declared, Path, Action, M}
         || M <- maps:keys(CaseAuth), not lists:member(M, Methods)
        ],
        [
            {case_auth_principal_invalid, Path, Action, M}
         || M <- maps:keys(CaseAuth),
            not lists:member(
                maps:get(auth_context, maps:get(M, CaseAuth), undefined),
                cs_auth:principals()
            )
        ],
        [
            {case_auth_same_as_default, Path, Action, M}
         || M <- maps:keys(CaseAuth),
            maps:get(M, CaseAuth) =:= DefaultAuth
        ]
    ]).

a01_widget_installation_permissions_split_by_method_test() ->
    {ok, Entry} = cs_actions:platform(p_widget_installations),
    DefaultAuth = maps:get(auth, Entry),
    PostAuth = maps:get(<<"POST">>, maps:get(case_auth, Entry)),
    ?assertEqual(platform_admin, maps:get(auth_context, DefaultAuth)),
    ?assertEqual(platform_admin, maps:get(auth_context, PostAuth)),
    ?assertEqual(<<"customer_service:read">>, maps:get(required_permission, DefaultAuth)),
    ?assertEqual(<<"customer_service:write">>, maps:get(required_permission, PostAuth)).

entry_methods(Entry) ->
    [maps:get(method, Case) || Case <- maps:get(cases, Entry)].

path_actions() ->
    sets:to_list(
        sets:from_list(
            [{Path, Action} || {_S, Path, Action, _M, _P} <- all_literal_routes()]
        )
    ).

all_literal_routes() ->
    lists:append([
        [{tenant, P, A, M, Pr} || {P, A, M, Pr} <- tenant_literal_routes()],
        [{widget, P, A, M, Pr} || {P, A, M, Pr} <- widget_literal_routes()],
        [{platform, P, A, M, Pr} || {P, A, M, Pr} <- platform_literal_routes()]
    ]).

route_methods(Opts) ->
    case {maps:get(surface, Opts, undefined), maps:get(action, Opts, undefined)} of
        {Surface, Action} when
            Surface =:= tenant; Surface =:= widget; Surface =:= platform
        ->
            case lists:keyfind(Action, 2, literal_for(Surface)) of
                {_Path, Action, Methods, _Pr} -> Methods;
                false -> []
            end;
        _ ->
            []
    end.

literal_for(tenant) ->
    [{P, A, M, Pr} || {P, A, M, Pr} <- tenant_literal_routes()];
literal_for(widget) ->
    [{P, A, M, Pr} || {P, A, M, Pr} <- widget_literal_routes()];
literal_for(platform) ->
    [{P, A, M, Pr} || {P, A, M, Pr} <- platform_literal_routes()].

%% A01 非真空：auth_context 改错 / feature 拿掉 / 装配键拿掉 / 少登记一条，
%% 审计必须逐条报红。
a01_audit_is_not_vacuous_test() ->
    Real = ?S:cs_routes(all),
    %% 38 = 租户 20（T-2 org 作用域化：queue/claim/transfer/close/detail/
    %% seats-sessions 迁径 + seat-contexts/transfer-targets/seats-me-events 新增；
    %% 访客三路与 A0 enterprise messages 路保持）+ widget 9（BE-W01 frame）+
    %% 平台 9；C2/C3 治理列表与既有 POST 同路径（动作名按 contracts-w2 冻结为
    %% shop_key_list/visit_token_list）。
    ?assert(length(Real) >= 38),
    MutatedAuth = lists:map(
        fun({Path, H, Opts}) ->
            case maps:get(action, Opts) of
                p_seats -> {Path, H, Opts#{auth_context => cs_visit}};
                _ -> {Path, H, Opts}
            end
        end,
        Real
    ),
    ?assertNotEqual([], violations(MutatedAuth)),
    MutatedFeature = [{Path, H, maps:remove(feature, Opts)} || {Path, H, Opts} <- Real],
    ?assertNotEqual([], violations(MutatedFeature)),
    MutatedFacts = [{Path, H, maps:remove(auth_facts, Opts)} || {Path, H, Opts} <- Real],
    ?assertNotEqual([], violations(MutatedFacts)),
    ?assertNotEqual([], violations(lists:droplast(Real))).

%% A01：动作表 ↔ 冻结清单双向覆盖（动作恰好一条路径、方法集合一致）。
a01_action_table_covers_frozen_paths_test() ->
    TenantActions = sets:from_list(cs_actions:tenant_actions()),
    PlatformActions = sets:from_list(cs_actions:platform_actions()),
    WidgetActions = sets:from_list(cs_actions:widget_actions()),
    FrozenTenant = sets:from_list([A || {_P, A, _M, _Pr} <- tenant_literal_routes()]),
    FrozenPlatform = sets:from_list([A || {_P, A, _M, _Pr} <- platform_literal_routes()]),
    FrozenWidget = sets:from_list([A || {_P, A, _M, _Pr} <- widget_literal_routes()]),
    ?assertEqual(sets:to_list(FrozenTenant), sets:to_list(TenantActions)),
    ?assertEqual(sets:to_list(FrozenPlatform), sets:to_list(PlatformActions)),
    ?assertEqual(sets:to_list(FrozenWidget), sets:to_list(WidgetActions)),
    lists:foreach(
        fun({Path, Action, Methods, Principal}) ->
            {ok, Entry} = cs_actions:tenant(Action),
            Auth = maps:get(auth, Entry),
            ?assertEqual(
                {Path, lists:sort(Methods), Principal},
                {Path, lists:sort(entry_methods(Entry)), maps:get(auth_context, Auth)}
            )
        end,
        tenant_literal_routes()
    ),
    lists:foreach(
        fun({Path, Action, Methods, Principal}) ->
            {ok, Entry} = cs_actions:platform(Action),
            Auth = maps:get(auth, Entry),
            ?assertEqual(
                {Path, lists:sort(Methods), Principal},
                {Path, lists:sort(entry_methods(Entry)), maps:get(auth_context, Auth)}
            )
        end,
        platform_literal_routes()
    ),
    lists:foreach(
        fun({Path, Action, Methods, Principal}) ->
            {ok, Entry} = cs_actions:widget(Action),
            Auth = maps:get(auth, Entry),
            ?assertEqual(
                {Path, lists:sort(Methods), Principal},
                {Path, lists:sort(entry_methods(Entry)), maps:get(auth_context, Auth)}
            )
        end,
        widget_literal_routes()
    ).

%% A01：五类 principal 各自的凭证类别互不相同（机制上的「无混淆」）。
a01_credential_classes_are_distinct_test() ->
    Classes = [{P, cs_auth:credential_class(P)} || P <- cs_auth:principals()],
    ?assertEqual(5, length(Classes)),
    %% enterprise_owner_admin 与 cs_seat 同为 imboy_jwt（成员类凭证，由事实装配
    %% 区分职能），其余三类各持专属凭证类别——共 4 类。
    ?assertEqual(4, length(lists:usort([C || {_P, C} <- Classes]))),
    ?assertEqual(imboy_jwt, cs_auth:credential_class(enterprise_owner_admin)),
    ?assertEqual(imboy_jwt, cs_auth:credential_class(cs_seat)),
    ?assertEqual(adm_session, cs_auth:credential_class(platform_admin)),
    ?assertEqual(visit_token, cs_auth:credential_class(cs_visit)),
    ?assertEqual(shop_key, cs_auth:credential_class(cs_shop_key)).

%% A01：每条客服路由的运行时 feature 门都归到 customer_service。
a01_runtime_feature_gate_wired_test() ->
    lists:foreach(
        fun({_Path, Handler, _Opts}) ->
            Surface =
                case Handler of
                    cs_platform_handler -> admin;
                    _ -> api
                end,
            ?assertEqual(customer_service, imboy_feature:route_feature(Surface, Handler, x))
        end,
        ?S:cs_routes(all)
    ).

%% ===================================================================
%% A04（静态侧）：坐席动作的 metadata 声明 + A02：双管理面共用同一 facade 用例
%% ===================================================================

a04_seat_actions_declare_customer_service_function_test() ->
    lists:foreach(
        fun(Action) ->
            {ok, Entry} = cs_actions:tenant(Action),
            Auth = maps:get(auth, Entry),
            ?assertEqual(cs_seat, maps:get(auth_context, Auth)),
            case cs_actions:org_source(Entry) of
                %% self 面（seat_contexts）不声明 org 级 function——聚合本身
                %% 就是枚举对象（application 逐 Org 复核）。
                self ->
                    ok;
                _ ->
                    ?assertEqual(<<"customer_service">>, maps:get(required_function, Auth)),
                    ?assert(is_binary(maps:get(required_permission, Auth)))
            end
        end,
        [
            session_claim,
            session_transfer,
            session_close,
            conversation_messages,
            %% BE-S01a：org 作用域坐席新面（T-2 后全部走 seat 门）。
            session_detail,
            seat_session_list,
            transfer_targets,
            seat_events,
            seat_contexts
        ]
    ).

a02_both_surfaces_share_facade_use_cases_test() ->
    %% 平台面的写动作（suspend/resume/transfer/close）与租户治理/坐席动作
    %% 解析到**同一个** facade 函数名——两个 handler 只有一张调用点表。
    SameActions = [
        {seat_suspend, suspend_seat},
        {seat_resume, resume_seat},
        {session_transfer, transfer},
        {session_close, close}
    ],
    lists:foreach(
        fun({TenantAction, FacadeFn}) ->
            {ok, TenantEntry} = cs_actions:tenant(TenantAction),
            ?assert(
                lists:member(
                    FacadeFn,
                    [maps:get(facade, C) || C <- maps:get(cases, TenantEntry)]
                )
            )
        end,
        SameActions
    ),
    lists:foreach(
        fun({PlatformAction, FacadeFn}) ->
            {ok, PlatformEntry} = cs_actions:platform(PlatformAction),
            ?assert(
                lists:member(
                    FacadeFn,
                    [maps:get(facade, C) || C <- maps:get(cases, PlatformEntry)]
                )
            )
        end,
        [
            {p_seat_suspend, suspend_seat},
            {p_seat_resume, resume_seat},
            {p_session_transfer, transfer},
            {p_session_close, close}
        ]
    ),
    %% 调用点表覆盖动作表：每个路径动作的**每个方法用例**的 facade 函数都在
    %% 调用点表里（无「路由有、调用点无」的用例）。
    Declared = cs_facade_call:actions(),
    lists:foreach(
        fun({Owner, Action}) ->
            {ok, Entry} =
                case Owner of
                    tenant -> cs_actions:tenant(Action);
                    widget -> cs_actions:widget(Action);
                    platform -> cs_actions:platform(Action)
                end,
            lists:foreach(
                fun(Case) ->
                    ?assert(lists:member(maps:get(facade, Case), Declared))
                end,
                maps:get(cases, Entry)
            )
        end,
        [
            {Owner, Action}
         || Owner <- [tenant, widget, platform],
            Action <-
                case Owner of
                    tenant -> cs_actions:tenant_actions();
                    widget -> cs_actions:widget_actions();
                    platform -> cs_actions:platform_actions()
                end
        ]
    ),
    %% facade 函数真实存在（customer_service_facade + enterprise_business_facade）。
    CsExports = customer_service_facade:module_info(exports),
    EbExports = enterprise_business_facade:module_info(exports),
    lists:foreach(
        fun(Fn) ->
            case Fn of
                list_messages ->
                    ?assert(lists:member({list_messages, 2}, EbExports));
                _ ->
                    ?assert(lists:member({Fn, 2}, CsExports))
            end
        end,
        Declared
    ).

%% ===================================================================
%% Web 坐席面一致性（中间件免签（JWT 门不变）↔ principal 声明）
%% ===================================================================

%% 2026-09-23 生产 902 修复的契约锚：凡坐席主体（cs_seat，route metadata 或
%% queue 的 GET case_auth）消费的 tenant 路径必须在
%% cs_http:is_web_seat_surface_path/1 豁免面内（浏览器无 APP 设备签名密钥）；
%% 访客/门店/治理面**不**豁免（照常签名 + JWT / 专用头）。
web_seat_surface_matches_seat_principal_declaration_test() ->
    lists:foreach(
        fun({Path, _Action, _Methods, Principal}) ->
            %% queue 同路径双主体：POST=门店（credential 面）、GET=坐席
            %% （case_auth）——形状层面统一豁免，语义差异由 handler 裁决。
            ExpectWebSeat =
                Principal =:= cs_seat orelse
                    Path =:= <<"/api/v1/cs/organizations/:org_id/sessions/queue">> orelse
                    %% CS-BE-06：seat-limit 经浏览器管理台调用（owner/admin 无
                    %% 设备签名密钥）——cs_http 的免签面已逐字登记，此处对账
                    %% 补认（其余治理面照常签名）。
                    Path =:= <<"/api/v1/cs/organizations/:org_id/seat-limit">>,
            ?assertEqual(
                {Path, ExpectWebSeat},
                {Path, cs_http:is_web_seat_surface_path(Path)}
            )
        end,
        tenant_literal_routes()
    ),
    %% enterprise 面：坐席工作台复用的两条消息路径（浏览器与 APP 共用合同）。
    ?assert(cs_http:is_web_seat_surface_path(<<"/api/v1/enterprise/conversations/123/messages">>)),
    ?assert(
        cs_http:is_web_seat_surface_path(
            <<"/api/v1/enterprise/organizations/123/conversations/456/messages">>
        )
    ),
    lists:foreach(fun(Path) -> ?assert(cs_http:is_web_seat_surface_path(Path)) end, [
        <<"/api/v1/enterprise/organizations/123/assets/presign">>,
        <<"/api/v1/enterprise/organizations/123/assets/confirm">>,
        <<"/api/v1/enterprise/organizations/123/assets/456/content">>
    ]),
    %% 负例真空证明：访客面 / widget 面 / 治理面 / ACK / 未知路径不得命中
    %% （把任一正例改成这些形状时本审计必须报红）。
    lists:foreach(
        fun(Negative) ->
            ?assertNot(cs_http:is_web_seat_surface_path(Negative))
        end,
        [
            <<"/api/v1/cs/sessions">>,
            <<"/api/v1/cs/sessions/123/messages">>,
            <<"/api/v1/cs/sessions/123/rating">>,
            <<"/api/v1/cs/widget/bootstrap">>,
            <<"/api/v1/cs/widget/sessions/123/messages">>,
            <<"/api/v1/cs/organizations/123/shop-keys">>,
            <<"/api/v1/cs/organizations/123/shop-keys/456/revoke">>,
            <<"/api/v1/cs/organizations/123/visit-tokens">>,
            <<"/api/v1/cs/organizations/123/seats">>,
            <<"/api/v1/cs/organizations/123/seats/456/suspend">>,
            <<"/api/v1/cs/organizations/123/sessions/123/assets/789/content">>,
            <<"/api/v1/enterprise/organizations/123/conversations/456/messages/789/ack">>,
            <<"/api/v1/enterprise/organizations/123/assets/456/content/extra">>,
            <<"/api/v1/enterprise/organizations/123/assets/presign/extra">>,
            <<"/api/v1/enterprise/organizations/123/assets/456/delete">>,
            <<"/api/v1/user/show">>,
            <<"/api/v1/passport/qr_login/create">>,
            <<"/w/wgt_pub_x">>
        ]
    ).

%% ===================================================================
%% credential 面一致性（中间件免签/免 JWT 面 ↔ handler 认证语义）
%% ===================================================================

credential_surface_matches_principal_declaration_test() ->
    lists:foreach(
        fun({_Path, Action, _Methods, Principal}) ->
            {ok, Entry} = cs_actions:tenant(Action),
            PathBin = path_of(tenant, Action),
            ExpectCredential = lists:member(Principal, [cs_visit, cs_shop_key]),
            ?assertEqual(
                {PathBin, ExpectCredential},
                {PathBin, cs_http:is_credential_surface_path(PathBin)}
            ),
            %% org 来源：param 面的路径不能带 :org_id 绑定；path 面必须带；
            %% self 面（BE-S01a 坐席上下文清单）路径不带 :org_id。
            case cs_actions:org_source(Entry) of
                path -> ?assert(is_map_key(org_id, path_bindings(PathBin)));
                self -> ?assertNot(is_map_key(org_id, path_bindings(PathBin)));
                param -> ?assertNot(is_map_key(org_id, path_bindings(PathBin)))
            end
        end,
        tenant_literal_routes()
    ),
    %% CSB-03：widget 面（全部令牌/引导动作，凭证在专用头——bootstrap 是签发
    %% 点本身，令牌可选，同样免签名 + 免 JWT 直通）。
    lists:foreach(
        fun({_Path, Action, _Methods, Principal}) ->
            {ok, Entry} = cs_actions:widget(Action),
            PathBin = path_of(widget, Action),
            ?assert(lists:member(Principal, [cs_visit, cs_shop_key])),
            ?assert(cs_http:is_credential_surface_path(PathBin)),
            %% CSD-BE-01R：widget 面 org 来源只有 param（申报+证明）与
            %% derived（bootstrap 的 public_id 全局反查零申报面）；两者都
            %% 不允许路径出现 :org_id 绑定。
            ?assert(lists:member(cs_actions:org_source(Entry), [param, derived])),
            ?assertNot(is_map_key(org_id, path_bindings(PathBin)))
        end,
        widget_literal_routes()
    ),
    lists:foreach(
        fun({Path, Action, _Methods, _Principal}) ->
            {ok, Entry} = cs_actions:platform(Action),
            case cs_actions:org_source(Entry) of
                path -> ?assert(is_map_key(org_id, path_bindings(Path)));
                param -> ?assertNot(is_map_key(org_id, path_bindings(Path)));
                %% 平台全局面（p_platform_seats）：org 是可选过滤，路径同样
                %% 不带 :org_id 绑定。
                param_optional -> ?assertNot(is_map_key(org_id, path_bindings(Path)))
            end
        end,
        platform_literal_routes()
    ).

path_of(Surface, Action) ->
    {Pattern, _Opts} = ?S:route_opt(Surface, Action),
    case Pattern of
        Bin when is_binary(Bin) -> Bin;
        List when is_list(List) -> unicode:characters_to_binary(List)
    end.

path_bindings(Path) ->
    Segments = [S || S <- binary:split(Path, <<"/">>, [global]), S =/= <<>>],
    maps:from_list([
        {binary_to_atom(binary:part(S, 1, byte_size(S) - 1), utf8), true}
     || S <- Segments, byte_size(S) > 1, binary:first(S) =:= $:
    ]).

%% ===================================================================
%% A03（静态侧）：裁剪接线三件套
%% ===================================================================

a03_generator_mapping_covers_feature_directory_test() ->
    %% 与 test/scripts/test_generate_product_features.py 的 enterprise 同款判据：
    %% 生成器映射 == 目录逐项（否则漏排的模块会被静默编译）。
    Modules = cs_source_modules("src/features/customer_service"),
    ?assertNotEqual([], Modules),
    ?assertEqual(lists:sort(Modules), lists:sort(generator_customer_service_modules())).

a03_router_helpers_are_compile_time_trimmed_test() ->
    RouterSrc = read_source("src/imboy_router.erl"),
    ?assert(string:find(RouterSrc, "-ifdef(IMBOY_FEATURE_CUSTOMER_SERVICE).") =/= nomatch),
    ?assert(string:find(RouterSrc, "customer_service_tenant_routes()") =/= nomatch),
    ?assert(string:find(RouterSrc, "customer_service_platform_routes()") =/= nomatch),
    ?assert(string:find(RouterSrc, "customer_service_wire(") =/= nomatch).

a03_middleware_credential_surface_is_ifdef_guarded_test() ->
    Src = read_source("src/api/auth_middleware_api_v1.erl"),
    ?assert(string:find(Src, "-ifdef(IMBOY_FEATURE_CUSTOMER_SERVICE).") =/= nomatch),
    ?assert(string:find(Src, "is_cs_credential_path") =/= nomatch),
    %% Web 坐席面（902 修复）：中间件接线 + ifdef 保护同款在位。
    ?assert(string:find(Src, "is_web_seat_path") =/= nomatch).

%% ===================================================================
%% 静态判据：接口层零 DB / 零动态派发 / 零 crypto
%% ===================================================================

interface_sources_have_no_db_or_dynamic_dispatch_test() ->
    Files = [
        ?TENANT_HANDLER,
        ?WIDGET_HANDLER,
        ?SEAT_CONSOLE_HANDLER,
        ?PLATFORM_HANDLER,
        ?HTTP_MODULE,
        ?AUTH_MODULE,
        ?FACADE_CALL,
        "src/features/customer_service/interfaces/cs_actions.erl"
    ],
    lists:foreach(
        fun(File) ->
            Src = strip_comments(read_source(File)),
            Forbidden = [
                <<"elib_pg:">>,
                <<"cs_pg_">>,
                <<"eb_pg_">>,
                <<"_repo:">>,
                <<"_ds:">>,
                <<"apply(">>,
                <<"list_to_atom">>,
                <<"binary_to_atom">>,
                <<"crypto:">>
            ],
            lists:foreach(
                fun(Needle) ->
                    ?assertEqual(
                        {File, Needle, nomatch},
                        {File, Needle, string:find(Src, Needle)}
                    )
                end,
                Forbidden
            ),
            ?assertEqual([], asset_operation_references(read_source(File)), {asset_call, File})
        end,
        Files
    ).

asset_operation_references_test() ->
    ?assertEqual(
        [],
        asset_operation_references(
            <<"action() -> <<\"presign\">>. % store:presign().\n">>
        )
    ),
    ?assertEqual(
        [presign, view_url, presign_put],
        asset_operation_references(
            <<"f() -> store:presign (x), fun store:view_url/1, store:presign_put(x).">>
        )
    ).

asset_operation_references(Src) ->
    {ok, Tokens, _} = erl_scan:string(unicode:characters_to_list(Src)),
    asset_operation_tokens(Tokens).

asset_operation_tokens([{atom, _, Name}, Next | Rest]) ->
    Text = atom_to_list(Name),
    Forbidden = lists:prefix("presign", Text) orelse lists:prefix("view_url", Text),
    Reference = element(1, Next) =:= '(' orelse element(1, Next) =:= '/',
    case Forbidden andalso Reference of
        true -> [Name | asset_operation_tokens([Next | Rest])];
        false -> asset_operation_tokens([Next | Rest])
    end;
asset_operation_tokens([_ | Rest]) ->
    asset_operation_tokens(Rest);
asset_operation_tokens([]) ->
    [].

%% ===================================================================
%% 出站编码（A02 TSID string）
%% ===================================================================

tsid_outbound_is_string_test() ->
    View = #{
        id => 123456789012345,
        session_id => 42,
        contact_id => 43,
        status => queued,
        client_msg_id => <<"cmid">>,
        device_id => <<"dev-1">>,
        nested => [#{business_identity_id => 7}]
    },
    Out = cs_http:encode_entity(View),
    ?assertEqual(<<"123456789012345">>, maps:get(id, Out)),
    ?assertEqual(<<"42">>, maps:get(session_id, Out)),
    ?assertEqual(<<"43">>, maps:get(contact_id, Out)),
    ?assertEqual(queued, maps:get(status, Out)),
    ?assertEqual(<<"cmid">>, maps:get(client_msg_id, Out)),
    ?assertEqual(<<"dev-1">>, maps:get(device_id, Out)),
    ?assertEqual([#{business_identity_id => <<"7">>}], maps:get(nested, Out)).

%% 错误映射：400/401/403/404/405/409/422 每类至少一条显式登记。
error_status_mapping_is_explicit_test() ->
    %% CSD-BE-01R（hosted-widget-contract S3 冻结码）：客户端申报服务端派生键
    %% = 400 `server_derived_key_rejected`（原 forbidden_client_key 对齐改名，
    %% tag 不回显命中键名——派生键集合不给客户端枚举面）。
    ?assertEqual(400, cs_http:status({server_derived_key_rejected, actor_user_id})),
    ?assertEqual(
        <<"server_derived_key_rejected">>, cs_http:tag({server_derived_key_rejected, secret})
    ),
    ?assertEqual(401, cs_http:status(credential_missing)),
    ?assertEqual(401, cs_http:status({principal_mismatch, cs_visit, imboy_jwt})),
    ?assertEqual(403, cs_http:status(seat_disabled)),
    ?assertEqual(403, cs_http:status({permission_missing, <<"x">>})),
    ?assertEqual(404, cs_http:status({not_found, any})),
    ?assertEqual(405, cs_http:status(method_not_allowed)),
    ?assertEqual(409, cs_http:status({stale_version, 1})),
    %% F6（RULING-2026-09-15 §七）：key_ref 不再是参数——提交即结构化 422；
    %% 密钥装配是服务端职责，env 缺失是服务端配置问题 ⇒ 500（不伪装成 4xx）。
    ?assertEqual(422, cs_http:status({unexpected_argument, key_ref})),
    ?assertEqual(500, cs_http:status(missing_key)),
    ?assertEqual(500, cs_http:status({seal_failed, missing_key})),
    ?assertEqual(500, cs_http:status({audit_append_failed, x})),
    ?assertEqual(500, cs_http:status(some_unmapped_reason)),
    %% F-LAY-01：seat 绑定身份的两类错误此前 500，显式登记。
    ?assertEqual(404, cs_http:status({identity_not_found, 1})),
    ?assertEqual(422, cs_http:status({identity_not_customer_service, 1, <<"sales">>})),
    %% F-LAY-03：CS 域真原子（死 EB 条目 visit_token_revoked 等已删）。
    ?assertEqual(401, cs_http:status(contact_mismatch)),
    %% F-SEC-05：路由缺权限声明 = 配置错误，fail-closed。
    ?assertEqual(403, cs_http:status({missing_required_permission, platform_admin})),
    %% A0 客户端契约：offboarding 降级 = 409 + envelope offboarding_required。
    ?assertEqual(409, cs_http:status({assignee_change_requires_offboarding, 1})),
    ?assertEqual(
        <<"offboarding_required">>, cs_http:tag({assignee_change_requires_offboarding, 1})
    ),
    %% CSB-03：widget 接入面错误分类（400/401/403/409/422/500 显式登记）。
    ?assertEqual(400, cs_http:status({invalid_origin, <<"x">>})),
    ?assertEqual(400, cs_http:status(credential_in_query_string)),
    ?assertEqual(401, cs_http:status({invalid_claim, iss})),
    ?assertEqual(401, cs_http:status(assertion_expired)),
    ?assertEqual(401, cs_http:status(subject_mismatch)),
    ?assertEqual(401, cs_http:status(identity_key_expired)),
    ?assertEqual(403, cs_http:status(origin_not_allowed)),
    ?assertEqual(403, cs_http:status(installation_revoked)),
    ?assertEqual(403, cs_http:status(identity_key_revoked)),
    ?assertEqual(409, cs_http:status({session_already_open, 1})),
    ?assertEqual(409, cs_http:status(replay)),
    ?assertEqual(422, cs_http:status(identity_key_not_configured)),
    %% 服务端注入事实缺失/默认 Workspace 解析失败是配置问题 ⇒ 500（不伪装 4xx）。
    ?assertEqual(500, cs_http:status({missing_injection, default_workspace})),
    ?assertEqual(500, cs_http:status(default_workspace_unresolved)),
    %% DF-2：坏 upload_ref（confirm 的唯一凭证）是客户端凭证错误——未显式
    %% 登记时落 server_side 兜底恒 500，语义错位。篡改/跨租户/垃圾 ref =
    %% 客户端凭证 400；过期 ref 与 EB 面显式登记同口径（F-LAY-02：客户端
    %% 可重试的冲突语义 409）。
    ?assertEqual(400, cs_http:status(invalid_upload_ref)),
    ?assertEqual(409, cs_http:status(expired_upload_ref)),
    %% CSD-BE-01（hosted-widget-contract S3/S4）：public_widget_id 反查面——
    %% missing/disabled/revoked 三态统一 404 `installation_unavailable`（响应
    %% 标签不区分三态，无存在性枚举）；路径绑定形状非法为 400。
    ?assertEqual(404, cs_http:status(installation_unavailable)),
    ?assertEqual(<<"installation_unavailable">>, cs_http:tag(installation_unavailable)),
    ?assertEqual(400, cs_http:status(invalid_public_widget_id)).

%% CSD-BE-01R/01S（hosted-widget-contract S3 v1.1）：bootstrap 的 public_id
%% 反查语义**锁死**——动作收 public_widget_id（required），org 来源是 derived
%% （浏览器零申报面：public_widget_id 全局反查命中行权威派生，OrgId 占位 0）；
%% `organization_id` 与其余服务端派生键（workspace/origin/secret/contact 等）
%% 客户端提供即 400 `server_derived_key_rejected`。
widget_bootstrap_public_id_and_server_derived_locked_test() ->
    {ok, Entry} = cs_actions:widget(widget_bootstrap),
    [Case] = maps:get(cases, Entry),
    Params = maps:get(params, Case),
    ?assert(lists:member({public_widget_id, binary, required}, Params)),
    ?assertNot(lists:member({organization_id, tsid, required}, Params)),
    Forbidden = maps:get(client_forbidden, Entry),
    lists:foreach(
        fun(Key) -> ?assert(lists:member(Key, Forbidden)) end,
        [workspace_id, origin, request_host, secret, contact_id, subject_key, organization_id]
    ),
    ?assertEqual(derived, cs_actions:org_source(Entry)),
    %% /w/ 面动作同表同纪律：public_widget_id 是唯一公开输入。
    {ok, PubEntry} = cs_actions:widget(widget_public_frame_html),
    [PubCase] = maps:get(cases, PubEntry),
    ?assert(
        lists:member({public_widget_id, binary, required}, maps:get(params, PubCase))
    ).

%% CSD-BE-01S（hosted-widget-contract S3 v1.1，GAP-3 修复锁死）：**全部持
%% token widget 动作面**零 org 申报——org_source=derived（Org 由 (installation_id,
%% secret) 的 digest 全局命中行服务端派生），organization_id 客户端提供即
%% 400 `server_derived_key_rejected`。两个例外是同一豁免面：旧 frame（S4
%% 兼容窗口，query organization_id 原样保留）与 widget_asset_put（无 token
%% 的裸 PUT 代理——presign 下发 URL 携带服务端签发的 organization_id，同值
%% 回传，非浏览器申报）。
widget_token_surfaces_org_derived_locked_test() ->
    DerivedSurfaces = [
        widget_identity_exchange,
        widget_sessions,
        widget_session_messages,
        widget_session_events,
        widget_asset_upload,
        widget_asset_confirm,
        widget_asset_content,
        widget_session_rating,
        widget_public_frame_html
    ],
    lists:foreach(
        fun(Action) ->
            {ok, Entry} = cs_actions:widget(Action),
            ?assertEqual({Action, derived}, {Action, cs_actions:org_source(Entry)}),
            Forbidden = maps:get(client_forbidden, Entry),
            ?assert(lists:member(organization_id, Forbidden), {Action, organization_id}),
            %% request_host 是 CSD-BE-01S 的服务端注入键（同源判定输入）。
            ?assert(lists:member(request_host, Forbidden), {Action, request_host}),
            lists:foreach(
                fun(Case) ->
                    ?assertNot(
                        lists:member({organization_id, tsid, required}, maps:get(params, Case))
                    )
                end,
                maps:get(cases, Entry)
            )
        end,
        DerivedSurfaces
    ),
    %% 豁免面保持 param（兼容窗口 / 无 token 裸 PUT），路径无 :org_id 绑定。
    lists:foreach(
        fun(Action) ->
            {ok, Entry} = cs_actions:widget(Action),
            ?assertEqual({Action, param}, {Action, cs_actions:org_source(Entry)})
        end,
        [widget_frame_html, widget_asset_put]
    ).

%% R2-F2（hosted-widget-contract S3 查询串面）：派生键申报面 = 正文与查询串
%% 双查——org 申报进 query 同样 400 `server_derived_key_rejected`，与 F6 密钥
%% 键守卫同口径；豁免面（旧 frame 兼容窗口）query organization_id 原样保留。
widget_query_string_declared_org_rejected_test() ->
    DerivedSurfaces = [
        widget_identity_exchange,
        widget_sessions,
        widget_session_messages,
        widget_session_events,
        widget_asset_upload,
        widget_asset_confirm,
        widget_asset_content,
        widget_session_rating,
        widget_public_frame_html
    ],
    lists:foreach(
        fun(Action) ->
            {ok, Entry} = cs_actions:widget(Action),
            Forbidden = maps:get(client_forbidden, Entry) ++ [workspace_organization_id],
            ?assertMatch(
                {error, {server_derived_key_rejected, organization_id}},
                cs_http:check_forbidden(#{}, [{<<"organization_id">>, <<"7001001">>}], Forbidden)
            ),
            ?assertMatch(
                {error, {server_derived_key_rejected, request_host}},
                cs_http:check_forbidden(
                    #{}, [{<<"request_host">>, <<"https://evil.example">>}], Forbidden
                )
            )
        end,
        DerivedSurfaces
    ),
    %% 兼容窗口豁免：旧 frame 的 query organization_id 不在禁键集，放行由
    %% org_source=param 走 value() 收集（行为零修改）。
    {ok, FrameEntry} = cs_actions:widget(widget_frame_html),
    FrameForbidden = maps:get(client_forbidden, FrameEntry) ++ [workspace_organization_id],
    ?assertEqual(
        ok,
        cs_http:check_forbidden(#{}, [{<<"organization_id">>, <<"7001001">>}], FrameForbidden)
    ).

%% CSD-BE-01（hosted-widget-contract S6）：/w/* 形状登记进共享谓词
%% （XFO 豁免 / CORS 面 / 免签直通三处消费的单一真源）；旧 frame 形状原样
%% 保留，相似路径不放宽。
csd_be01_public_frame_shape_single_source_test() ->
    ?assert(imboy_route_shape:is_cs_widget_frame_path(<<"/w/wgt_pub_x">>)),
    ?assert(imboy_route_shape:is_cs_widget_frame_path(<<"/api/v1/cs/widget/frame/810001">>)),
    ?assertNot(imboy_route_shape:is_cs_widget_frame_path(<<"/w">>)),
    ?assertNot(imboy_route_shape:is_cs_widget_frame_path(<<"/w/a/b">>)),
    ?assertNot(imboy_route_shape:is_cs_widget_frame_path(<<"/www/wgt_pub_x">>)),
    %% /w/ 免签直通面（与旧 frame 同一判定入口）。
    ?assert(cs_http:is_credential_surface_path(<<"/w/wgt_pub_x">>)).

%% seat-console-embed SC-BE：/seat/* 形状登记进共享谓词族（XFO 豁免 / CORS
%% 面 / 免签直通三处消费的单一真源）；恰两段、首段字面 seat；相似路径
%% 不放宽；与 /w/ 两谓词互斥（无吞并、无重叠加宽）。
seat_console_frame_shape_single_source_test() ->
    ?assert(imboy_route_shape:is_cs_seat_console_frame_path(<<"/seat/sc_pub_x">>)),
    ?assert(imboy_route_shape:is_cs_seat_console_frame_path(<<"/seat/1234567890">>)),
    ?assertNot(imboy_route_shape:is_cs_seat_console_frame_path(<<"/seat">>)),
    ?assertNot(imboy_route_shape:is_cs_seat_console_frame_path(<<"/seat/a/b">>)),
    ?assertNot(imboy_route_shape:is_cs_seat_console_frame_path(<<"/seats/sc_pub_x">>)),
    ?assertNot(imboy_route_shape:is_cs_seat_console_frame_path(<<"/search/sc_pub_x">>)),
    ?assertNot(imboy_route_shape:is_cs_seat_console_frame_path(<<"/seatfw/sc_pub_x">>)),
    %% A08：XFO 豁免**精确** /seat/*——widget 谓词不吞 /seat/，seat 谓词不吞
    %% /w/ 与既有 widget frame 形状（两面互斥）。
    ?assertNot(imboy_route_shape:is_cs_widget_frame_path(<<"/seat/sc_pub_x">>)),
    ?assertNot(imboy_route_shape:is_cs_seat_console_frame_path(<<"/w/wgt_pub_x">>)),
    ?assertNot(
        imboy_route_shape:is_cs_seat_console_frame_path(<<"/api/v1/cs/widget/frame/810001">>)
    ),
    %% 免签直通面（cs_http 消费点与 /w/ 同一口径登记）。
    ?assert(cs_http:is_credential_surface_path(<<"/seat/sc_pub_x">>)),
    %% 平台 CRUD 四路由在册（路由表 ↔ 冻结清单双向对账由 a01 承担；此处锁
    %% 存在性与方法形状逐字）。
    {Pat, Opts} = ?S:route_opt(platform, p_seat_consoles),
    ?assertEqual(<<"/api/adm/customer-service/seat-consoles">>, Pat),
    ?assertEqual(p_seat_consoles, maps:get(action, Opts)),
    {PatU, _} = ?S:route_opt(platform, p_seat_console_update),
    ?assertEqual(<<"/api/adm/customer-service/seat-consoles/:id">>, PatU),
    {PatR, _} = ?S:route_opt(platform, p_seat_console_revoke),
    ?assertEqual(<<"/api/adm/customer-service/seat-consoles/:id/revoke">>, PatR),
    {PatS, _} = ?S:route_opt(widget, seat_console_frame_html),
    ?assertEqual(<<"/seat/:public_seat_console_id">>, PatS).

%% CSB-03：widget 面凭证传输纪律——专用头合法、查询串即 400；Origin 归一化
%% 复用 domain cs_widget（接口层只做归一与形状门，allowlist 匹配在 application）。
widget_transport_discipline_test() ->
    %% 凭证面：widget 全部路径免签名 + 免 JWT 直通（中间件口径）。
    lists:foreach(
        fun({Path, _A, _M, _P}) ->
            ?assert(cs_http:is_credential_surface_path(Path))
        end,
        widget_literal_routes()
    ),
    %% Origin 归一化：同源 sibling 折叠（缺省端口/大小写）、形状非法 fail-closed。
    ?assertEqual(
        {ok, <<"https://shop.example.com">>},
        cs_widget:normalize_origin(<<"HTTPS://Shop.Example.COM">>)
    ),
    ?assertEqual(
        {ok, <<"https://shop.example.com">>},
        cs_widget:normalize_origin(<<"https://shop.example.com:443">>)
    ),
    ?assertMatch(
        {error, {invalid_origin, _}}, cs_widget:normalize_origin(<<"https://x.example.com/path">>)
    ),
    ?assertMatch({error, {invalid_origin, _}}, cs_widget:normalize_origin(<<"not-an-origin">>)),
    %% allowlist 精确匹配（无子域通融；空 allowlist 全拒）。
    ?assertEqual(
        ok,
        cs_widget:origin_allowed(<<"https://shop.example.com">>, [<<"https://shop.example.com">>])
    ),
    ?assertEqual(
        {error, origin_not_allowed},
        cs_widget:origin_allowed(<<"https://evil.example.com">>, [<<"https://shop.example.com">>])
    ),
    ?assertEqual(
        {error, origin_not_allowed}, cs_widget:origin_allowed(<<"https://shop.example.com">>, [])
    ).

%% ===================================================================
%% 工具
%% ===================================================================

cs_source_modules(Dir) ->
    Files = filelib:wildcard(filename:join(Dir, "**/*.erl")),
    lists:usort([module_of(F) || F <- Files]).

module_of(File) ->
    Src = read_source(File),
    case re:run(Src, "^-module\\(([a-z][a-z0-9_]*)\\)\\.", [{capture, [1], list}, multiline]) of
        {match, [Mod]} -> list_to_atom(Mod);
        _ -> erlang:error({no_module_attribute, File})
    end.

%% 生成器 FEATURE_BACKEND_MODULES 里 customer_service 段的模块名（按字面提取；
%% 断言独立于被断言对象——清单直接来自生成器源码，不来自目录枚举的反面）。
generator_customer_service_modules() ->
    Src = read_source("scripts/generate_product_features.py"),
    case string:find(Src, "\"customer_service\": (") of
        nomatch ->
            erlang:error(customer_service_mapping_missing);
        Start ->
            Section = string:slice(Start, 0, 4096),
            %% 取本 dict 条目（到下一个 feature 键为止），首项是键名本身。
            Block = hd(binary:split(Section, <<"\"moment\"">>)),
            {match, Rows} = re:run(
                Block,
                "\"([a-z][a-z0-9_]*)\"",
                [global, {capture, [1], list}]
            ),
            [list_to_atom(N) || [N] <- Rows, N =/= "customer_service"]
    end.

read_source(Path) ->
    {ok, Bin} = file:read_file(Path),
    unicode:characters_to_binary(Bin).

strip_comments(Src) ->
    NoBlock = re:replace(Src, "%%%.*", "", [global, multiline]),
    re:replace(NoBlock, "%[^%].*", "", [global, multiline]).
