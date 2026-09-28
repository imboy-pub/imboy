%%% @doc Human Organization Directory 集成 handler（计划 §14.2，4 只读端点）。
%%%
%%% 只做协议面接线：method 门（GET only）、path binding / query 参数提取、
%%% 调用 organization_directory_app、把结果映射为 HTTP 信封。业务裁决
%%% （授权门 / 游标绑定 / 分页语义）全部在 application 层冻结。
%%%
%%% 信封口径（冻结，A5 Flutter 按 RESULT.json fixture 对齐）：
%%%   * 成功：HTTP 200 + imboy 既有 envelope
%%%     {code:0, msg:"success", sv_ts, payload:{list, cursor, has_more}}
%%%     （列表 payload 一律 list 键——与 /organizations/* v2 面同口径）。
%%%   * 失败：真实 HTTP 状态 + {"error":{"code":"<stable>","message":"<generic>"}}
%%%     （stable 码/状态映射复用 enterprise_internal_error 的冻结表；
%%%     message 静态生成，不回显请求细节）。
%%%
%%% 路由注册：本模块不注册路由；imboy_router.erl 的 4 条挂载由
%%% SHARED_PATH_PROPOSAL.json 交 A0 集成（Router 为共享路径）。
-module(organization_directory_handler).

-behavior(cowboy_rest).

-export([init/2, handle_action/3]).

-include("log.hrl").

-define(APP, organization_directory_app).
-define(ERR, enterprise_internal_error).

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    {ok, handle_action(Action, Req0, State), State}.

-spec handle_action(atom(), cowboy_req:req(), map()) -> cowboy_req:req().
%% —— 目录浏览：某父节点的直接 active 子部门（parent_id 缺省=根级）——
handle_action(directory_departments, Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"GET">> -> departments(Req0, State);
        _ -> method_not_allowed(Req0)
    end;
%% —— 本级成员（department_id 缺省=未挂 active 部门的根成员）——
handle_action(directory_members, Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"GET">> -> members(Req0, State);
        _ -> method_not_allowed(Req0)
    end;
%% —— 我的部门捷径（无部门=空数组）——
handle_action(directory_me, Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"GET">> -> me(Req0, State);
        _ -> method_not_allowed(Req0)
    end;
%% —— 联合检索（部门名/昵称/账号；U-02：不搜手机号/邮箱/职位）——
handle_action(directory_search, Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"GET">> -> search(Req0, State);
        _ -> method_not_allowed(Req0)
    end.

%% ===================================================================
%% 4 个 action 的参数提取（二进制键透传，形状校验在 app 层）
%% ===================================================================

departments(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    with_org_id(Req0, fun(OrgId) ->
        Params =
            #{
                parent_id => elib_param:get(<<"parent_id">>, Req0, undefined),
                cursor => elib_param:get(<<"cursor">>, Req0, undefined),
                limit => elib_param:get(<<"limit">>, Req0, undefined)
            },
        respond(Req0, ?APP:list_departments(OrgId, Uid, Params))
    end).

members(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    with_org_id(Req0, fun(OrgId) ->
        Params =
            #{
                department_id => elib_param:get(<<"department_id">>, Req0, undefined),
                cursor => elib_param:get(<<"cursor">>, Req0, undefined),
                limit => elib_param:get(<<"limit">>, Req0, undefined)
            },
        respond(Req0, ?APP:list_members(OrgId, Uid, Params))
    end).

me(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    with_org_id(Req0, fun(OrgId) ->
        respond(Req0, ?APP:my_departments(OrgId, Uid))
    end).

search(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    with_org_id(Req0, fun(OrgId) ->
        Params =
            #{
                q => elib_param:get(<<"q">>, Req0, undefined),
                cursor => elib_param:get(<<"cursor">>, Req0, undefined),
                limit => elib_param:get(<<"limit">>, Req0, undefined)
            },
        respond(Req0, ?APP:search(OrgId, Uid, Params))
    end).

%% ===================================================================
%% 信封映射
%% ===================================================================

%% app 层（?APP 四用例）契约返回 {ok, Payload} | {error, Code :: binary()}，
%% 两形态已穷尽（Code 均为 stable 二进制码）。
respond(Req0, {ok, Payload}) ->
    elib_response:success(Req0, Payload);
respond(Req0, {error, Code}) when is_binary(Code) ->
    reply_error(Req0, Code).

%% stable 码 → 真实 HTTP 状态 + {"error":{"code","message"}} 信封。
%% 状态/文案映射复用 ?ERR 冻结表（同一 13 码集合，不新造码）。
reply_error(Req0, Code) ->
    Status = ?ERR:http_status(Code),
    case Code of
        <<"insufficient_scope">> ->
            ?WARN_LOG([organization_directory_forbidden, #{path => cowboy_req:path(Req0)}]);
        <<"organization_disabled">> ->
            ?WARN_LOG([organization_directory_org_disabled, #{path => cowboy_req:path(Req0)}]);
        <<"security_gate_closed">> ->
            ?ERROR_LOG([organization_directory_gate_closed, #{path => cowboy_req:path(Req0)}]);
        _Other ->
            ok
    end,
    cowboy_req:reply(
        Status,
        #{<<"content-type">> => <<"application/json; charset=utf-8">>},
        ?ERR:error_body(Code),
        Req0
    ).

%% ===================================================================
%% 协议面辅助
%% ===================================================================

with_org_id(Req0, Fun) ->
    case positive_binding(organization_id, Req0) of
        {ok, OrgId} ->
            Fun(OrgId);
        error ->
            reply_error(Req0, <<"invalid_request">>)
    end.

positive_binding(Name, Req0) ->
    case elib_cnv:safe_to_integer(cowboy_req:binding(Name, Req0)) of
        Id when is_integer(Id), Id > 0 ->
            {ok, Id};
        _ ->
            error
    end.

method_not_allowed(Req0) ->
    cowboy_req:reply(
        405,
        #{
            <<"allow">> => <<"GET">>,
            <<"content-type">> => <<"text/plain; charset=utf-8">>
        },
        <<"Method Not Allowed">>,
        Req0
    ).
