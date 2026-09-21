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
            group -> group(Method, Req0, State);
            member_roles -> member_roles(Method, Req0, State);
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
            Input = params_to_input(Params),
            %% INT-04 的边界资源是请求体里的 workspace_id（manifest: workspace scoped）
            with_boundary(
                Conn, Ctx, <<"INT-04">>, maps:get(workspace_id, Input, undefined), fun() ->
                    enterprise_group_logic:create_group_tx(Conn, Ctx, Input)
                end
            )
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
    RouteId = route_id_of(Op),
    with_idempotency(
        Req0,
        Ctx,
        <<"group_members">>,
        IdemKey,
        Digest,
        fun(Conn) ->
            ExternalIds = maps:get(<<"external_user_ids">>, Params, undefined),
            with_group_boundary(Conn, Ctx, RouteId, GroupId, fun() ->
                case Op of
                    add ->
                        enterprise_group_logic:add_members_tx(Conn, Ctx, GroupId, ExternalIds);
                    remove ->
                        enterprise_group_logic:remove_members_tx(Conn, Ctx, GroupId, ExternalIds)
                end
            end)
        end
    ).

%% @doc INT-18（GET 详情）/ INT-19（PATCH 更新）/ INT-21（DELETE 归档）：同一
%% path 的群生命周期端点（FULL-02 新增，待 A0 接线）。
%%   GET    /api/internal/v1/groups/{group_id}  -> #{action => group}
%%   PATCH  /api/internal/v1/groups/{group_id}  -> #{action => group}
%%   DELETE /api/internal/v1/groups/{group_id}  -> #{action => group}
-spec group(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
group(<<"GET">>, Req0, State) ->
    %% 只读（manifest idempotency: not_required）——无幂等键、无写入。
    read_group(read_detail, Req0, State);
group(<<"PATCH">>, Req0, State) ->
    write_group(update, <<"INT-19">>, <<"PATCH">>, Req0, State);
group(<<"DELETE">>, Req0, State) ->
    write_group(archive, <<"INT-21">>, <<"DELETE">>, Req0, State);
group(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc INT-20（FULL-02 新增，待 A0 接线）：成员角色管理。
%% 路由：PUT /api/internal/v1/groups/{group_id}/members/roles -> #{action => member_roles}
-spec member_roles(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
member_roles(<<"PUT">>, Req0, State) ->
    write_group(set_roles, <<"INT-20">>, <<"PUT">>, Req0, State);
member_roles(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc 详情（单事务只读；无幂等键）。
-spec read_group(read_detail, cowboy_req:req(), map()) -> cowboy_req:req().
read_group(read_detail, Req0, State) ->
    Ctx = maps:get(enterprise_internal, State, #{}),
    GroupId = maps:get(group_id, State, 0),
    Result =
        elib_pg:with_tx(fun(Conn) ->
            with_group_boundary(Conn, Ctx, <<"INT-18">>, GroupId, fun() ->
                enterprise_group_logic:group_detail_tx(Conn, Ctx, GroupId)
            end)
        end),
    case Result of
        {ok, Detail} ->
            reply_json(Req0, 200, Detail);
        {error, {Code, _Detail}} ->
            enterprise_internal_error:reply(Req0, Code);
        {rollback, Reason} ->
            ?ERROR_LOG("enterprise_group_handler detail rollback: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>);
        {error, Reason} ->
            ?ERROR_LOG("enterprise_group_handler detail error: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>)
    end.

%% @doc 写路径（更新/归档/角色）：走 A2 幂等模式（单事务）。
-spec write_group(update | archive | set_roles, binary(), binary(), cowboy_req:req(), map()) ->
    cowboy_req:req().
write_group(Op, RouteId, Method, Req0, State) ->
    Ctx = maps:get(enterprise_internal, State, #{}),
    GroupId = maps:get(group_id, State, 0),
    Params = elib_param:post(Req0),
    IdemKey = idempotency_key(Req0),
    ResourceType = resource_type_of(Op),
    Digest = enterprise_internal_idempotency:request_digest(
        Method, group_path(Op, GroupId), Params
    ),
    with_idempotency(Req0, Ctx, ResourceType, IdemKey, Digest, fun(Conn) ->
        with_group_boundary(Conn, Ctx, RouteId, GroupId, fun() ->
            do_write_group(Conn, Ctx, Op, GroupId, Params)
        end)
    end).

-spec do_write_group(any(), map(), update | archive | set_roles, term(), map()) ->
    {ok, map()} | {error, {binary(), term()}}.
do_write_group(Conn, Ctx, update, GroupId, Params) ->
    enterprise_group_logic:update_group_tx(Conn, Ctx, GroupId, params_to_input(Params));
do_write_group(Conn, Ctx, archive, GroupId, _Params) ->
    enterprise_group_logic:archive_group_tx(Conn, Ctx, GroupId);
do_write_group(Conn, Ctx, set_roles, GroupId, Params) ->
    Roles = maps:get(<<"roles">>, Params, undefined),
    enterprise_group_logic:set_member_roles_tx(Conn, Ctx, GroupId, json_roles(Roles)).

%% @doc roles 入参（JSON 数组 [{external_user_id, role}]）→ logic 需要的 atom 键
%% 列表；形态不合法交给 logic 归一 invalid_request（本函数只做结构转换，
%% 不判业务合法性）。
-spec json_roles(term()) -> term().
json_roles(Roles) when is_list(Roles) ->
    [json_role(R) || R <- Roles];
json_roles(Other) ->
    Other.

-spec json_role(term()) -> term().
json_role(#{<<"external_user_id">> := Ext, <<"role">> := Role}) ->
    #{external_user_id => Ext, role => Role};
json_role(Other) ->
    Other.

-spec resource_type_of(update | archive | set_roles) -> binary().
resource_type_of(update) -> <<"group">>;
resource_type_of(archive) -> <<"group_archive">>;
resource_type_of(set_roles) -> <<"group_member_roles">>.

-spec route_id_of(add | remove) -> binary().
route_id_of(add) -> <<"INT-05">>;
route_id_of(remove) -> <<"INT-06">>.

%% @doc 群类路由的边界接线：先按 Org 边界定位群（跨 Org/个人群/已归档一律
%% resource_not_found，不泄露存在性），再用其 workspace_id 做 Grant workspace
%% 边界判定（同一事务，逐请求读）。
-spec with_group_boundary(any(), map(), binary(), term(), fun()) ->
    {ok, map()} | {error, {binary(), term()}}.
with_group_boundary(Conn, Ctx, RouteId, GroupId, Fun) ->
    case enterprise_group_logic:boundary_workspace_tx(Conn, Ctx, GroupId) of
        {ok, WsId} ->
            with_boundary(Conn, Ctx, RouteId, WsId, Fun);
        {error, _} = Err ->
            Err
    end.

%% @doc FULL-01/FULL-02 授权边界接线（唯一接线点
%% enterprise_internal_boundary:enforce/4）：未受管应用 no-op；受管应用要求
%% 同一生效 Grant 同时覆盖 scope 与 workspace，撤权/降权下一请求即失败。
-spec with_boundary(any(), map(), binary(), undefined | integer(), fun()) ->
    {ok, map()} | {error, {binary(), term()}}.
with_boundary(Conn, Ctx, RouteId, WorkspaceId, Fun) ->
    case enterprise_internal_boundary:enforce(Conn, Ctx, RouteId, WorkspaceId) of
        ok ->
            Fun();
        {error, Code} ->
            {error, {enterprise_internal_boundary:error_code(Code), grant_boundary}}
    end.

%% @doc 幂等 digest 用**具体路径**（非模板）：同 key 换 group_id 必须是
%% digest_conflict 409，不得被当成首次请求的静默重放（参见 message handler
%% group_path/1 同款口径）。
members_path(GroupId) when is_integer(GroupId) ->
    <<"/api/internal/v1/groups/", (integer_to_binary(GroupId))/binary, "/members">>;
members_path(_) ->
    ?GROUP_MEMBERS_TEMPLATE.

-spec group_path(update | archive | set_roles, term()) -> binary().
group_path(set_roles, GroupId) when is_integer(GroupId) ->
    <<"/api/internal/v1/groups/", (integer_to_binary(GroupId))/binary, "/members/roles">>;
group_path(set_roles, _GroupId) ->
    <<"/api/internal/v1/groups/{group_id}/members/roles">>;
group_path(_Op, GroupId) when is_integer(GroupId) ->
    <<"/api/internal/v1/groups/", (integer_to_binary(GroupId))/binary>>;
group_path(_Op, _GroupId) ->
    <<"/api/internal/v1/groups/{group_id}">>.

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
