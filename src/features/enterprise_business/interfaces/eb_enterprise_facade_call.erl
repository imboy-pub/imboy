%%% @doc 动作 → facade 的**唯一**调用点表（EB-09）。
%%%
%%% 依据：`docs/architecture/feature-slice-rules.md` 铁律 3（facade = 公开 API，只能进
%%% application）、铁律 5（单元内跨层调用不得越界）。
%%%
%%% 为什么单列一个模块：handler 必须是**薄**的，而「动作 → 用例」是一张**编译期
%%% 固定**的映射。把映射写成一串字面调用点（每条 `enterprise_business_facade:F/2`），
%%% 使下面两件事成为可机械核对的事实：
%%%
%%%   * handler **只**调 facade，绝不 `elib_pg`/`eb_pg_*`/`*_repo`/`*_ds`
%%%     （A05 的静态判据直接扫这三个模块的源码）；
%%%   * 不存在「按请求字符串拼函数名」的动态调用路径（没有 `apply/3`、没有
%%%     `list_to_atom`），动作表里未登记的动作在此**没有**分支，调用即 `undef` 式
%%%     的显式失败（`{error, {unknown_action, A}}`），不会误落到默认用例。
%%%
%%% 本模块不做任何参数加工、不做任何结果解释：由 `eb_enterprise_http` 负责形状，
%%% 由 application 负责语义。
-module(eb_enterprise_facade_call).

-export([call/3, actions/0]).

%% @doc 执行一次用例调用。`FacadeAction` 来自冻结的动作表（`eb_enterprise_actions`）。
-spec call(atom(), integer(), map()) -> {ok, term()} | {error, term()}.
call(create_identity, OrgId, Params) ->
    enterprise_business_facade:create_identity(OrgId, Params);
call(list_identities, OrgId, Params) ->
    enterprise_business_facade:list_identities(OrgId, Params);
call(bind_assignment, OrgId, Params) ->
    enterprise_business_facade:bind_assignment(OrgId, Params);
call(list_contacts, OrgId, Params) ->
    enterprise_business_facade:list_contacts(OrgId, Params);
call(create_contact, OrgId, Params) ->
    enterprise_business_facade:create_contact(OrgId, Params);
call(get_contact, OrgId, Params) ->
    enterprise_business_facade:get_contact(OrgId, Params);
call(update_contact, OrgId, Params) ->
    enterprise_business_facade:update_contact(OrgId, Params);
call(append_note, OrgId, Params) ->
    enterprise_business_facade:append_note(OrgId, Params);
call(open_conversation, OrgId, Params) ->
    enterprise_business_facade:open_conversation(OrgId, Params);
call(list_messages, OrgId, Params) ->
    enterprise_business_facade:list_messages(OrgId, Params);
call(append_message, OrgId, Params) ->
    enterprise_business_facade:append_message(OrgId, Params);
call(ack_delivery, OrgId, Params) ->
    enterprise_business_facade:ack_delivery(OrgId, Params);
call(request_presign, OrgId, Params) ->
    enterprise_business_facade:request_presign(OrgId, Params);
%% CS-BE-01B：presign 路径的 PUT 用例（坐席字节上传；payload=原始体字节，
%% 由 handler 线格式层注入）。用例内复核凭证 TTL/篡改/同上传人/经办 ACL。
call(put_object, OrgId, Params) ->
    enterprise_business_facade:put_object(OrgId, Params);
call(confirm_asset, OrgId, Params) ->
    enterprise_business_facade:confirm_asset(OrgId, Params);
call(content_stream, OrgId, Params) ->
    enterprise_business_facade:content_stream(OrgId, Params);
call(suspend_member, OrgId, Params) ->
    enterprise_business_facade:suspend_member(OrgId, Params);
call(open_offboarding, OrgId, Params) ->
    enterprise_business_facade:open_offboarding(OrgId, Params);
call(execute_offboarding, OrgId, Params) ->
    enterprise_business_facade:execute_offboarding(OrgId, Params);
call(verify_offboarding, OrgId, Params) ->
    enterprise_business_facade:verify_offboarding(OrgId, Params);
call(finalize_offboarding, OrgId, Params) ->
    enterprise_business_facade:finalize_offboarding(OrgId, Params);
call(list_offboarding, OrgId, Params) ->
    enterprise_business_facade:list_offboarding(OrgId, Params);
call(offboarding_detail, OrgId, Params) ->
    enterprise_business_facade:offboarding_detail(OrgId, Params);
call(fetch_message, OrgId, Params) ->
    enterprise_business_facade:fetch_message(OrgId, Params);
call(Action, _OrgId, _Params) ->
    {error, {unknown_action, Action}}.

%% @doc 本表登记的全部用例动作（供契约测试核对「动作表 ⊆ 调用点」）。
-spec actions() -> [atom()].
actions() ->
    [
        create_identity,
        list_identities,
        bind_assignment,
        list_contacts,
        create_contact,
        get_contact,
        update_contact,
        append_note,
        open_conversation,
        list_messages,
        append_message,
        ack_delivery,
        request_presign,
        put_object,
        confirm_asset,
        content_stream,
        suspend_member,
        open_offboarding,
        execute_offboarding,
        verify_offboarding,
        finalize_offboarding,
        list_offboarding,
        offboarding_detail,
        fetch_message
    ].
