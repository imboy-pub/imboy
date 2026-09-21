-module(enterprise_group_handler).

%%%
% EPGZ-08 W4 INT-04/05/06 Workspace 企业群端点壳（handler 壳需求见 A3
% EPGZ-03 冻结合同「给 A0 W4 的 handler 壳需求清单」，本模块严格照此编排）。
%
% 路由（A0 W4 经 Router lease 登记）：
%   POST   /api/internal/v1/groups                       -> #{action => create}
%   PUT    /api/internal/v1/groups/:group_id/members     -> #{action => add_members}
%   DELETE /api/internal/v1/groups/:group_id/members     -> #{action => remove_members}
% —— 必须经 enterprise_internal_middleware（A2 认证链：credential →
%    active/expiry → application active → organization active → scope
%    groups:write → rate internal_write fail-closed）。
%    ctx 注入 handler_opts.enterprise_internal（atom 键 map）。
%
% 幂等（A2 模式，INV-7）：Idempotency-Key 已由 decide 强制存在；单事务
% begin_tx -> logic -> complete_tx；业务 {error} 经 throw({rollback,...})
% 整体回滚（幂等行一并消失，同 key 重试可过）。
% 资源：create -> group（resource_id=group_id）；add/remove -> group_members。
%
% path 绑定：group_id 由 cowboy_router 注入 bindings，A2 middleware 合并进
% handler_opts；非法/缺失绑定 → 0（logic 侧 invalid_request fail-closed）。
%%%

-behavior(cowboy_rest).

-export([init/2]).

-include("log.hrl").

-define(GROUP_CREATE_PATH, <<"/api/internal/v1/groups">>).
-define(GROUP_MEMBERS_TEMPLATE, <<"/api/internal/v1/groups/{group_id}/members">>).

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case Action of
            create -> create(Method, Req0, State);
            members -> members(Method, Req0, State);
            _ -> Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec create(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
create(<<"POST">>, Req0, State) ->
    Ctx = maps:get(enterprise_internal, State, #{}),
    Params = elib_param:post(Req0),
    IdemKey = idempotency_key(Req0),
    Digest = enterprise_internal_idempotency:request_digest(
        <<"POST">>, ?GROUP_CREATE_PATH, Params
    ),
    with_idempotency(
        Req0,
        Ctx,
        <<"group">>,
        IdemKey,
        Digest,
        fun(Conn) ->
            enterprise_group_logic:create_group_tx(Conn, Ctx, params_to_input(Params))
        end
    );
create(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc INT-05/06 共用一个 cowboy 路由（同一 path，方法区分）：PUT=添加、
%% DELETE=移除。A2 decide 已按 method+path 在冻结表里判到 INT-05 / INT-06
%% （scope 同为 groups:write），本壳只做方法分派与 405。
-spec members(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
members(<<"PUT">>, Req0, State) ->
    members_op(add, <<"PUT">>, Req0, State);
members(<<"DELETE">>, Req0, State) ->
    members_op(remove, <<"DELETE">>, Req0, State);
members(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec members_op(add | remove, binary(), cowboy_req:req(), map()) -> cowboy_req:req().
members_op(Op, Method, Req0, State) ->
    Ctx = maps:get(enterprise_internal, State, #{}),
    GroupId = maps:get(group_id, State, 0),
    Params = elib_param:post(Req0),
    IdemKey = idempotency_key(Req0),
    Digest = enterprise_internal_idempotency:request_digest(
        Method, members_path(GroupId), Params
    ),
    with_idempotency(
        Req0,
        Ctx,
        <<"group_members">>,
        IdemKey,
        Digest,
        fun(Conn) ->
            ExternalIds = maps:get(<<"external_user_ids">>, Params, undefined),
            case Op of
                add ->
                    enterprise_group_logic:add_members_tx(Conn, Ctx, GroupId, ExternalIds);
                remove ->
                    enterprise_group_logic:remove_members_tx(Conn, Ctx, GroupId, ExternalIds)
            end
        end
    ).

%% @doc 幂等 digest 用**具体路径**（非模板）：同 key 换 group_id 必须是
%% digest_conflict 409，不得被当成首次请求的静默重放（参见 message handler
%% group_path/1 同款口径）。
members_path(GroupId) when is_integer(GroupId) ->
    <<"/api/internal/v1/groups/", (integer_to_binary(GroupId))/binary, "/members">>;
members_path(_) ->
    ?GROUP_MEMBERS_TEMPLATE.

%% @doc A2 幂等模式（单事务）。返回值契约见 enterprise_internal_idempotency。
-spec with_idempotency(
    cowboy_req:req(), map(), binary(), binary() | undefined, binary(), fun()
) ->
    cowboy_req:req().
with_idempotency(Req0, Ctx, ResourceType, IdemKey, Digest, LogicFun) ->
    TxResult =
        elib_pg:with_tx(fun(Conn) ->
            case
                enterprise_internal_idempotency:begin_tx(Conn, Ctx, ResourceType, IdemKey, Digest)
            of
                {ok, inserted} ->
                    case LogicFun(Conn) of
                        {ok, Result} ->
                            _ = enterprise_internal_idempotency:complete_tx(
                                Conn,
                                Ctx,
                                ResourceType,
                                IdemKey,
                                resource_id_of(Result),
                                200
                            ),
                            {tx_ok, Result};
                        {error, {Code, _Detail}} ->
                            throw({rollback, {business_error, Code}})
                    end;
                Other ->
                    Other
            end
        end),
    case TxResult of
        {tx_ok, Result} ->
            reply_json(Req0, 200, Result);
        {rollback, {business_error, Code}} ->
            enterprise_internal_error:reply(Req0, Code);
        {rollback, Reason} ->
            ?ERROR_LOG("enterprise_group_handler rollback (~s): ~p~n", [ResourceType, Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>);
        {ok, replay, #{resource_id := RId, response_code := Code}} ->
            replay_json(Req0, ResourceType, RId, Code);
        {ok, pending} ->
            enterprise_internal_error:reply(Req0, <<"idempotency_conflict">>);
        {error, digest_conflict} ->
            enterprise_internal_error:reply(Req0, enterprise_internal_idempotency:conflict_code());
        {error, Reason} ->
            ?ERROR_LOG("enterprise_group_handler idempotency error: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>)
    end.

%% @doc 重放应答：幂等表只持久化 {resource_id, response_code}，响应体按
%% resource_type 以 resource_id 重建（A2 冻结形态；不新增响应体列）。
replay_json(Req0, <<"group">>, RId, Code) ->
    reply_json(Req0, ok_code(Code), #{<<"replayed">> => true, <<"group_id">> => RId});
replay_json(Req0, ResourceType, RId, Code) ->
    reply_json(Req0, ok_code(Code), #{
        <<"replayed">> => true, <<"resource_type">> => ResourceType, <<"resource_id">> => RId
    }).

resource_id_of(Result) when is_map(Result) ->
    case maps:find(<<"group_id">>, Result) of
        {ok, Id} when is_integer(Id) -> Id;
        _ -> null
    end;
resource_id_of(_) ->
    null.

ok_code(Code) when is_integer(Code), Code >= 200, Code < 300 -> Code;
ok_code(_) -> 200.

-spec reply_json(cowboy_req:req(), non_neg_integer(), map()) -> cowboy_req:req().
reply_json(Req0, Status, Map) ->
    Body = jsone:encode(Map),
    cowboy_req:reply(Status, #{<<"content-type">> => <<"application/json">>}, Body, Req0).

idempotency_key(Req0) ->
    case cowboy_req:header(<<"idempotency-key">>, Req0) of
        K when is_binary(K), K =/= <<>> -> K;
        _ -> undefined
    end.

%% @doc 只把**冻结合同内的已知键**转 atom 后交给 logic；未知键一律丢弃。
%% 不用 blanket binary_to_atom（未受信 body 的任意键会持续增长 atom 表）。
params_to_input(Params) when is_map(Params) ->
    maps:fold(
        fun(K, V, Acc) ->
            case known_key(K) of
                {ok, Atom} -> Acc#{Atom => V};
                error -> Acc
            end
        end,
        #{},
        Params
    );
params_to_input(_) ->
    #{}.

known_key(<<"workspace_id">>) -> {ok, workspace_id};
known_key(<<"title">>) -> {ok, title};
known_key(<<"introduction">>) -> {ok, introduction};
known_key(<<"members">>) -> {ok, members};
known_key(<<"owner_external_user_id">>) -> {ok, owner_external_user_id};
known_key(_) -> error.
