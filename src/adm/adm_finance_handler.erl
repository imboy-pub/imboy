-module(adm_finance_handler).

%%%
% adm_finance 控制器模块 / Admin finance operational API
%
% 运营财务端点 /adm/finance/*：供 React 管理后台（管理员鉴权）做跨用户运营查询。
%   - 钱包列表 / 某用户钱包流水
%   - 充值订单列表 / 支付流水列表（对账）
%   - billing 套餐 CRUD / 订阅列表 / 账单列表
%
% 职责边界：仅做参数解析 + 鉴权 + 调 logic + 响应封装，不写业务/SQL。
%   wallet/recharge/payment 运营查询 -> finance_adm_logic
%   billing 套餐/订阅/账单         -> billing_logic
%
% 金额单位「分」(bigint) 透传不改，前端负责分→元显示。
% 统一响应信封 {code,msg,sv_ts,payload}（经 elib_response）。
%
% 权限：沿用 adm 管理员 Cookie 鉴权（adm_auth_middleware 注入 adm_user_id），
%   并用 adm_acl 细粒度权限守卫：
%     finance:read  — 查询类（GET）
%     finance:write — 变更类（套餐创建/更新 POST）
%   这两个权限新增于 adm_index_handler:role_acl/1（super_admin/ops_admin）。
%%%

-behavior(cowboy_rest).

-export([init/2]).

%% fail/3 是查询失败的运行时兜底响应。底层分页查询的 dialyzer 成功类型多为 ok-only，
%% 导致 dialyzer 判定 fail/3 的 {error,_} 调用分支不可达（no local return）。
%% 该 error 分支为防御性运行时保留（底层 SQL 实际可能返回 {error,_}），故抑制此告警。
-dialyzer({nowarn_function, [fail/3]}).

-include("error_code.hrl").
-include("log.hrl").

-define(PERM_READ, <<"finance:read">>).
-define(PERM_WRITE, <<"finance:write">>).

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0, false),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 = dispatch(Action, Method, Req0, State),
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec dispatch(atom(), binary(), cowboy_req:req(), map()) -> cowboy_req:req().
dispatch(wallets, Method, Req0, State) -> wallets(Method, Req0, State);
dispatch(wallet_transactions, Method, Req0, State) -> wallet_transactions(Method, Req0, State);
dispatch(recharge_orders, Method, Req0, State) -> recharge_orders(Method, Req0, State);
dispatch(payment_transactions, Method, Req0, State) -> payment_transactions(Method, Req0, State);
dispatch(billing_plans, Method, Req0, State) -> billing_plans(Method, Req0, State);
dispatch(billing_plan_create, Method, Req0, State) -> billing_plan_create(Method, Req0, State);
dispatch(billing_plan_update, Method, Req0, State) -> billing_plan_update(Method, Req0, State);
dispatch(billing_subscriptions, Method, Req0, State) -> billing_subscriptions(Method, Req0, State);
dispatch(billing_invoices, Method, Req0, State) -> billing_invoices(Method, Req0, State);
dispatch(_, _Method, Req0, _State) -> Req0.

%% -------------------------------------------------------------------
%% 钱包列表 GET /adm/finance/wallets
%% 筛选 user_id / status
%% -------------------------------------------------------------------
-spec wallets(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
wallets(<<"GET">>, Req0, State) ->
    with_read_perm(State, Req0, fun() ->
        {Page, Size} = elib_param:page(Req0),
        Qs = cowboy_req:parse_qs(Req0),
        Filter = collect_filters(Qs, [
            {<<"user_id">>, user_id, int},
            {<<"status">>, status, int}
        ]),
        respond_list(Req0, finance_adm_logic:list_wallets(Filter, Page, Size))
    end);
wallets(_, Req0, _State) ->
    Req0.

%% -------------------------------------------------------------------
%% 某用户钱包流水 GET /adm/finance/wallet/:user_id/transactions
%% -------------------------------------------------------------------
-spec wallet_transactions(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
wallet_transactions(<<"GET">>, Req0, State) ->
    with_read_perm(State, Req0, fun() ->
        case parse_uid_binding(Req0) of
            {error, Msg} ->
                elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
            {ok, Uid} ->
                {Page, Size} = elib_param:page(Req0),
                respond_list(Req0, finance_adm_logic:list_wallet_transactions(Uid, Page, Size))
        end
    end);
wallet_transactions(_, Req0, _State) ->
    Req0.

%% -------------------------------------------------------------------
%% 充值订单列表 GET /adm/finance/recharge-orders
%% 筛选 status[0-4] / payment_method / user_id / order_no
%% -------------------------------------------------------------------
-spec recharge_orders(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
recharge_orders(<<"GET">>, Req0, State) ->
    with_read_perm(State, Req0, fun() ->
        {Page, Size} = elib_param:page(Req0),
        Qs = cowboy_req:parse_qs(Req0),
        Filter = collect_filters(Qs, [
            {<<"status">>, status, int},
            {<<"payment_method">>, payment_method, binary},
            {<<"user_id">>, user_id, int},
            {<<"order_no">>, order_no, binary}
        ]),
        respond_list(Req0, finance_adm_logic:list_recharge_orders(Filter, Page, Size))
    end);
recharge_orders(_, Req0, _State) ->
    Req0.

%% -------------------------------------------------------------------
%% 支付流水列表（对账）GET /adm/finance/payment-transactions
%% 筛选 gateway / biz_type[1-3] / status[0-4] / user_id
%% -------------------------------------------------------------------
-spec payment_transactions(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
payment_transactions(<<"GET">>, Req0, State) ->
    with_read_perm(State, Req0, fun() ->
        {Page, Size} = elib_param:page(Req0),
        Qs = cowboy_req:parse_qs(Req0),
        Filter = collect_filters(Qs, [
            {<<"gateway">>, gateway, binary},
            {<<"biz_type">>, biz_type, int},
            {<<"status">>, status, int},
            {<<"user_id">>, user_id, int},
            {<<"trade_no">>, trade_no, binary}
        ]),
        respond_list(Req0, finance_adm_logic:list_payment_transactions(Filter, Page, Size))
    end);
payment_transactions(_, Req0, _State) ->
    Req0.

%% -------------------------------------------------------------------
%% billing 套餐列表 GET /adm/finance/billing/plans
%% 筛选 status[0-1]
%% -------------------------------------------------------------------
-spec billing_plans(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
billing_plans(<<"GET">>, Req0, State) ->
    with_read_perm(State, Req0, fun() ->
        {Page, Size} = elib_param:page(Req0),
        Qs = cowboy_req:parse_qs(Req0),
        Filter = collect_filters(Qs, [
            {<<"status">>, status, int},
            {<<"billing_period">>, billing_period, binary}
        ]),
        case billing_logic:list_plans_page(Filter, Page, Size) of
            {ok, Payload} ->
                elib_response:success(Req0, normalize_billing_payload(Payload, [<<"id">>]));
            {error, Reason} ->
                fail(Req0, <<"套餐列表查询失败"/utf8>>, Reason)
        end
    end);
billing_plans(_, Req0, _State) ->
    Req0.

%% -------------------------------------------------------------------
%% billing 套餐创建 POST /adm/finance/billing/plan
%% -------------------------------------------------------------------
-spec billing_plan_create(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
billing_plan_create(<<"POST">>, Req0, State) ->
    with_write_perm(State, Req0, fun() ->
        PostVals = elib_param:post(Req0),
        case billing_logic:create_plan(PostVals) of
            {ok, PlanId} ->
                elib_response:success(
                    Req0,
                    #{<<"id">> => elib_id:tsid_to_bin(PlanId)},
                    <<"套餐已创建"/utf8>>
                );
            {error, Msg} ->
                elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST)
        end
    end);
billing_plan_create(_, Req0, _State) ->
    Req0.

%% -------------------------------------------------------------------
%% billing 套餐更新 POST /adm/finance/billing/plan/update
%% body 须含 id；其余字段为待更新列
%% -------------------------------------------------------------------
-spec billing_plan_update(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
billing_plan_update(<<"POST">>, Req0, State) ->
    with_write_perm(State, Req0, fun() ->
        PostVals = elib_param:post(Req0),
        case parse_required_id(PostVals, <<"id">>) of
            {error, Msg} ->
                elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
            {ok, PlanId} ->
                ChangeMap = maps:remove(<<"id">>, PostVals),
                case map_size(ChangeMap) > 0 of
                    false ->
                        elib_response:error(
                            Req0, <<"至少提供一个可更新字段"/utf8>>, ?ERR_BAD_REQUEST
                        );
                    true ->
                        case billing_logic:update_plan(PlanId, ChangeMap) of
                            {ok, _} ->
                                elib_response:success(Req0, #{}, <<"套餐已更新"/utf8>>);
                            {error, Msg} ->
                                elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST)
                        end
                end
        end
    end);
billing_plan_update(_, Req0, _State) ->
    Req0.

%% -------------------------------------------------------------------
%% billing 订阅列表 GET /adm/finance/billing/subscriptions
%% 筛选 status[0-3] / plan_id / tenant_id
%% -------------------------------------------------------------------
-spec billing_subscriptions(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
billing_subscriptions(<<"GET">>, Req0, State) ->
    with_read_perm(State, Req0, fun() ->
        {Page, Size} = elib_param:page(Req0),
        Qs = cowboy_req:parse_qs(Req0),
        Filter = collect_filters(Qs, [
            {<<"status">>, status, int},
            {<<"plan_id">>, plan_id, int},
            {<<"tenant_id">>, tenant_id, int}
        ]),
        case billing_logic:list_subscriptions_page(Filter, Page, Size) of
            {ok, Payload} ->
                elib_response:success(
                    Req0, normalize_billing_payload(Payload, [<<"id">>, <<"plan_id">>])
                );
            {error, Reason} ->
                fail(Req0, <<"订阅列表查询失败"/utf8>>, Reason)
        end
    end);
billing_subscriptions(_, Req0, _State) ->
    Req0.

%% -------------------------------------------------------------------
%% billing 账单列表 GET /adm/finance/billing/invoices
%% 筛选 status[0-2] / subscription_id
%% -------------------------------------------------------------------
-spec billing_invoices(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
billing_invoices(<<"GET">>, Req0, State) ->
    with_read_perm(State, Req0, fun() ->
        {Page, Size} = elib_param:page(Req0),
        Qs = cowboy_req:parse_qs(Req0),
        Filter = collect_filters(Qs, [
            {<<"status">>, status, int},
            {<<"subscription_id">>, subscription_id, int},
            {<<"invoice_no">>, invoice_no, binary}
        ]),
        case billing_logic:list_invoices_page(Filter, Page, Size) of
            {ok, Payload} ->
                elib_response:success(
                    Req0, normalize_billing_payload(Payload, [<<"id">>, <<"subscription_id">>])
                );
            {error, Reason} ->
                fail(Req0, <<"账单列表查询失败"/utf8>>, Reason)
        end
    end);
billing_invoices(_, Req0, _State) ->
    Req0.

%% ===================================================================
%% 鉴权守卫
%% ===================================================================

%% @doc 读权限守卫：通过后执行 Fun（返回 cowboy_req），否则返回 403。
-spec with_read_perm(map(), cowboy_req:req(), fun(() -> cowboy_req:req())) -> cowboy_req:req().
with_read_perm(State, Req0, Fun) ->
    guard(State, ?PERM_READ, Req0, Fun).

%% @doc 写权限守卫。
-spec with_write_perm(map(), cowboy_req:req(), fun(() -> cowboy_req:req())) -> cowboy_req:req().
with_write_perm(State, Req0, Fun) ->
    guard(State, ?PERM_WRITE, Req0, Fun).

-spec guard(map(), binary(), cowboy_req:req(), fun(() -> cowboy_req:req())) -> cowboy_req:req().
guard(State, Permission, Req0, Fun) ->
    case adm_acl:ensure_permission(State, Permission, Req0) of
        ok ->
            Fun();
        {error, Req1} ->
            Req1
    end.

%% ===================================================================
%% 参数解析与响应
%% ===================================================================

%% @doc 从 query-string proplist 中按规格提取筛选项，组装 Filter map。
%% Spec: [{QsKey :: binary(), MapKey :: atom(), int | binary}]。
%% 缺省/空值的键不进 Filter（由 logic 层 build_where 再次过白名单）。
-spec collect_filters([{binary(), binary()}], [{binary(), atom(), int | binary}]) -> map().
collect_filters(Qs, Spec) ->
    lists:foldl(
        fun({QsKey, MapKey, Type}, Acc) ->
            case proplists:get_value(QsKey, Qs) of
                undefined -> Acc;
                <<>> -> Acc;
                Raw -> put_filter(Acc, MapKey, Type, Raw)
            end
        end,
        #{},
        Spec
    ).

-spec put_filter(map(), atom(), int | binary, binary()) -> map().
put_filter(Acc, MapKey, int, Raw) ->
    case parse_int(Raw) of
        {ok, Int} -> Acc#{MapKey => Int};
        error -> Acc
    end;
put_filter(Acc, MapKey, binary, Raw) ->
    Acc#{MapKey => Raw}.

-spec parse_int(binary()) -> {ok, integer()} | error.
parse_int(Bin) ->
    case string:to_integer(binary_to_list(Bin)) of
        {Int, []} when is_integer(Int) -> {ok, Int};
        _ -> error
    end.

%% @doc 解析路由 binding 中的 user_id（须为正整数）
-spec parse_uid_binding(cowboy_req:req()) -> {ok, integer()} | {error, binary()}.
parse_uid_binding(Req0) ->
    case cowboy_req:binding(user_id, Req0) of
        undefined ->
            {error, <<"用户ID不能为空"/utf8>>};
        UidBin ->
            case parse_int(UidBin) of
                {ok, Uid} when Uid > 0 -> {ok, Uid};
                _ -> {error, <<"用户ID格式错误"/utf8>>}
            end
    end.

%% @doc 从 body 解析必填的整数 id 字段
-spec parse_required_id(map(), binary()) -> {ok, integer()} | {error, binary()}.
parse_required_id(PostVals, Key) ->
    case maps:get(Key, PostVals, undefined) of
        undefined ->
            {error, <<"ID不能为空"/utf8>>};
        Val ->
            case to_positive_int(Val) of
                {ok, Id} -> {ok, Id};
                error -> {error, <<"ID格式错误"/utf8>>}
            end
    end.

-spec to_positive_int(term()) -> {ok, integer()} | error.
to_positive_int(Val) when is_integer(Val), Val > 0 -> {ok, Val};
to_positive_int(Val) when is_binary(Val) ->
    case parse_int(Val) of
        {ok, Id} when Id > 0 -> {ok, Id};
        _ -> error
    end;
to_positive_int(_) ->
    error.

%% @doc 统一封装 logic 返回的 {ok, Payload} | {error, Reason}（payload 已含 list/total/page/size）
%% 注：不加 -spec，与本仓其他 adm handler 的响应辅助函数一致；底层查询成功类型多为
%% ok-only，error 分支为运行时兜底，加严格 spec 会触发 dialyzer 死分支告警。
respond_list(Req0, {ok, Payload}) ->
    elib_response:success(Req0, Payload);
respond_list(Req0, {error, Reason}) ->
    fail(Req0, <<"查询失败"/utf8>>, Reason).

%% @doc 规范化 billing 分页 payload：list 中各条目指定 TSID 键转字符串。
-spec normalize_billing_payload(map(), [binary()]) -> map().
normalize_billing_payload(Payload, TsidKeys) ->
    List = maps:get(list, Payload, []),
    List2 = [elib_id:tsid_keys_to_bin(Item, TsidKeys) || Item <- List],
    Payload#{list => List2}.

%% @doc 查询失败统一响应（500 + 信封）。Reason 仅记日志，不外泄。
%% 注：不加 -spec，与本仓其他 adm handler 响应辅助函数一致；底层查询成功类型多为
%% ok-only，本函数仅在运行时 {error,_} 兜底分支触达。
fail(Req0, Msg, Reason) ->
    ?DEBUG_LOG("adm_finance 查询失败: ~p", [Reason]),
    elib_response:error(Req0, Msg, ?ERR_INTERNAL_SERVER_ERROR).
