%%% @doc 客服动作 → facade 的**唯一**调用点表（CS-02）。
%%%
%%% 依据：`docs/architecture/feature-slice-rules.md` 铁律 3（facade = 公开 API）、
%%% 铁律 5（跨单元只准引用 facade）、plan §5.2（客服消息/客户/附件只经
%%% `enterprise_business_facade` 复用企业基础，不建副本）。
%%%
%%% 与 `eb_enterprise_facade_call` 同构的机械核对性质：
%%%
%%%   * handler **只**经本表进 facade，绝不 `elib_pg`/`cs_pg_*`/`eb_pg_*`/
%%%     `*_repo`/`*_ds`（cs_route_contract_tests 的静态判据直接扫源码）；
%%%   * 没有 `apply/3`、没有 `list_to_atom`——动作表未登记的动作在此没有分支，
%%%     调用即显式失败（`{error, {unknown_action, A}}`）；
%%%   * 跨单元引用**只有两个 facade 模块**：`customer_service_facade`（本 feature）
%%%     与 `enterprise_business_facade`（企业真源，铁律 5 允许的 facade 引用）。
%%%
%%% **本模块不做**：不加工参数、不解释结果（形状由 cs_http，语义由 application）。
-module(cs_facade_call).

-export([call/3, actions/0]).

%% @doc 执行一次用例调用。`FacadeAction` 来自冻结的动作表（`cs_actions`）。
-spec call(atom(), integer(), map()) -> {ok, term()} | {error, term()}.
call(open_session, OrgId, Params) ->
    customer_service_facade:open_session(OrgId, Params);
call(list_contact_sessions, OrgId, Params) ->
    customer_service_facade:list_contact_sessions(OrgId, Params);
call(append_session_message, OrgId, Params) ->
    customer_service_facade:append_session_message(OrgId, Params);
call(rate, OrgId, Params) ->
    customer_service_facade:rate(OrgId, Params);
call(claim, OrgId, Params) ->
    customer_service_facade:claim(OrgId, Params);
call(transfer, OrgId, Params) ->
    customer_service_facade:transfer(OrgId, Params);
call(close, OrgId, Params) ->
    customer_service_facade:close(OrgId, Params);
%% A0 客户端契约基准：客服端企业消息列表——企业真源的唯一读路径（跨 facade 复用）。
call(list_messages, OrgId, Params) ->
    enterprise_business_facade:list_messages(OrgId, Params);
call(fetch_session, OrgId, Params) ->
    customer_service_facade:fetch_session(OrgId, Params);
%% C1（contracts-w2）：平台 session 列表（只读，租户/平台共用同一用例）。
call(list_sessions, OrgId, Params) ->
    customer_service_facade:list_sessions(OrgId, Params);
call(create_seat, OrgId, Params) ->
    customer_service_facade:create_seat(OrgId, Params);
%% BE-S01b：admin provisioning（api-surface-freeze admin_provisioning）。
call(provision_seat, OrgId, Params) ->
    customer_service_facade:provision_seat(OrgId, Params);
call(list_dispatchable_seats, OrgId, Params) ->
    customer_service_facade:list_dispatchable_seats(OrgId, Params);
%% 平台运营面坐席分页（跨企业可选 Org 过滤；OrgId=0 = 全局）。
call(list_platform_seats, OrgId, Params) ->
    customer_service_facade:list_platform_seats(OrgId, Params);
call(suspend_seat, OrgId, Params) ->
    customer_service_facade:suspend_seat(OrgId, Params);
call(resume_seat, OrgId, Params) ->
    customer_service_facade:resume_seat(OrgId, Params);
%% C2（contracts-w2）：shop key 治理列表（与创建同路径动作的两个用例之一）。
call(list_shop_keys, OrgId, Params) ->
    customer_service_facade:list_shop_keys(OrgId, Params);
call(create_shop_key, OrgId, Params) ->
    customer_service_facade:create_shop_key(OrgId, Params);
call(revoke_shop_key, OrgId, Params) ->
    customer_service_facade:revoke_shop_key(OrgId, Params);
%% C3（contracts-w2）：visit token 治理列表。
call(list_visit_tokens, OrgId, Params) ->
    customer_service_facade:list_visit_tokens(OrgId, Params);
call(issue_visit_token, OrgId, Params) ->
    customer_service_facade:issue_visit_token(OrgId, Params);
call(revoke_visit_token, OrgId, Params) ->
    customer_service_facade:revoke_visit_token(OrgId, Params);
call(list_widget_installations, OrgId, Params) ->
    customer_service_facade:list_widget_installations(OrgId, Params);
call(create_widget_installation, OrgId, Params) ->
    customer_service_facade:create_widget_installation(OrgId, Params);
call(revoke_widget_installation, OrgId, Params) ->
    customer_service_facade:revoke_widget_installation(OrgId, Params);
%% CSB-02：Widget 与 Seat 补缺用例（HTTP 动作行归 CSB-03；这里只登记
%% facade 调用点，保证「有 facade 函数必有调用点」的机械核对闭合）。
call(widget_bootstrap, OrgId, Params) ->
    customer_service_facade:widget_bootstrap(OrgId, Params);
%% BE-W01 A05：动态 frame HTML 的公开 installation 投影（零凭证面）。
call(widget_frame_html, OrgId, Params) ->
    customer_service_facade:widget_frame_html(OrgId, Params);
%% CSD-BE-01（hosted-widget-contract S3/S4）：public_widget_id 全局反查的
%% frame HTML 投影（/w/ 面）。OrgId=0 是同构占位（租户由命中行派生，
%% seat_contexts 的 self 面先例）。
call(widget_public_frame_html, _OrgId, Params) ->
    customer_service_facade:widget_public_frame_html(0, Params);
call(widget_identity_exchange, OrgId, Params) ->
    customer_service_facade:widget_identity_exchange(OrgId, Params);
call(widget_create_session, OrgId, Params) ->
    customer_service_facade:widget_create_session(OrgId, Params);
call(widget_list_sessions, OrgId, Params) ->
    customer_service_facade:widget_list_sessions(OrgId, Params);
call(widget_history_after, OrgId, Params) ->
    customer_service_facade:widget_history_after(OrgId, Params);
call(widget_visitor_message, OrgId, Params) ->
    customer_service_facade:widget_visitor_message(OrgId, Params);
call(widget_rate, OrgId, Params) ->
    customer_service_facade:widget_rate(OrgId, Params);
call(widget_asset_upload, OrgId, Params) ->
    customer_service_facade:widget_asset_upload(OrgId, Params);
call(widget_asset_confirm, OrgId, Params) ->
    customer_service_facade:widget_asset_confirm(OrgId, Params);
%% BE-PATCH-01：访客附件字节上传代理（payload=请求体字节，upload_ref 鉴权）。
call(widget_asset_put, OrgId, Params) ->
    customer_service_facade:widget_asset_put(OrgId, Params);
%% BE-S01b：访客附件内容代理（api-surface-freeze widget_apis）。
call(widget_asset_content, OrgId, Params) ->
    customer_service_facade:widget_asset_content(OrgId, Params);
call(seat_session_detail, OrgId, Params) ->
    customer_service_facade:seat_session_detail(OrgId, Params);
%% CS-BE-03（CS-DEC-01）：客户上下文只读投影。
call(session_customer_context, OrgId, Params) ->
    customer_service_facade:session_customer_context(OrgId, Params);
%% CSB-02R：坐席工作台（队列 GET + active/closed 列表）。
call(seat_session_queue, OrgId, Params) ->
    customer_service_facade:seat_session_queue(OrgId, Params);
call(seat_session_list, OrgId, Params) ->
    customer_service_facade:seat_session_list(OrgId, Params);
%% BE-S01a：坐席上下文清单 / 转接目标 / SSE 占位（api-surface-freeze）。
call(seat_contexts, OrgId, Params) ->
    customer_service_facade:seat_contexts(OrgId, Params);
call(transfer_targets, OrgId, Params) ->
    customer_service_facade:transfer_targets(OrgId, Params);
call(seat_events, OrgId, Params) ->
    customer_service_facade:seat_events(OrgId, Params);
call(Action, _OrgId, _Params) ->
    {error, {unknown_action, Action}}.

%% @doc 本表登记的全部用例动作（供契约测试核对「动作表 ⊆ 调用点」）。
-spec actions() -> [atom()].
actions() ->
    [
        open_session,
        list_contact_sessions,
        append_session_message,
        rate,
        claim,
        transfer,
        close,
        list_messages,
        fetch_session,
        list_sessions,
        create_seat,
        provision_seat,
        list_dispatchable_seats,
        list_platform_seats,
        suspend_seat,
        resume_seat,
        list_shop_keys,
        create_shop_key,
        revoke_shop_key,
        list_visit_tokens,
        issue_visit_token,
        revoke_visit_token,
        list_widget_installations,
        create_widget_installation,
        revoke_widget_installation,
        %% CSB-02：Widget 与 Seat 补缺用例
        widget_bootstrap,
        widget_frame_html,
        widget_public_frame_html,
        widget_identity_exchange,
        widget_create_session,
        widget_list_sessions,
        widget_history_after,
        widget_visitor_message,
        widget_rate,
        widget_asset_upload,
        widget_asset_confirm,
        widget_asset_put,
        widget_asset_content,
        seat_session_detail,
        %% CS-BE-03：客户上下文只读投影
        session_customer_context,
        %% CSB-02R：坐席工作台
        seat_session_queue,
        seat_session_list,
        %% BE-S01a：坐席上下文清单 / 转接目标 / SSE 占位
        seat_contexts,
        transfer_targets,
        seat_events
    ].
