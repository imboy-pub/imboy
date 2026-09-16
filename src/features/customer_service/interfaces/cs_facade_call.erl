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
call(create_seat, OrgId, Params) ->
    customer_service_facade:create_seat(OrgId, Params);
call(list_dispatchable_seats, OrgId, Params) ->
    customer_service_facade:list_dispatchable_seats(OrgId, Params);
call(suspend_seat, OrgId, Params) ->
    customer_service_facade:suspend_seat(OrgId, Params);
call(resume_seat, OrgId, Params) ->
    customer_service_facade:resume_seat(OrgId, Params);
call(create_shop_key, OrgId, Params) ->
    customer_service_facade:create_shop_key(OrgId, Params);
call(revoke_shop_key, OrgId, Params) ->
    customer_service_facade:revoke_shop_key(OrgId, Params);
call(issue_visit_token, OrgId, Params) ->
    customer_service_facade:issue_visit_token(OrgId, Params);
call(revoke_visit_token, OrgId, Params) ->
    customer_service_facade:revoke_visit_token(OrgId, Params);
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
        create_seat,
        list_dispatchable_seats,
        suspend_seat,
        resume_seat,
        create_shop_key,
        revoke_shop_key,
        issue_visit_token,
        revoke_visit_token
    ].
