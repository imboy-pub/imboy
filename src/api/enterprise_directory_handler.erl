-module(enterprise_directory_handler).

-behavior(cowboy_rest).

-export([init/2]).

-include("log.hrl").

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
            mappings -> mappings(Method, Req0, State);
            users -> users(Method, Req0, State);
            sync -> sync(Method, Req0, State);
            organizations -> source_page(organizations, Method, Req0, State);
            departments -> source_page(departments, Method, Req0, State);
            persons -> source_page(persons, Method, Req0, State);
            _ -> Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal
%% ===================================================================

source_page(Kind, <<"GET">>, Req, State) ->
    Result =
        case source_query(Req) of
            {ok, Query} ->
                enterprise_directory_source_read_logic:page(
                    maps:get(enterprise_internal, State, #{}), Kind, Query
                );
            Error ->
                Error
        end,
    case Result of
        {ok, Page} ->
            cowboy_req:reply(
                200,
                #{
                    <<"content-type">> => <<"application/json">>,
                    <<"cache-control">> => <<"no-store">>
                },
                jsone:encode(Page),
                Req
            );
        {error, Code} when is_atom(Code) ->
            enterprise_internal_error:reply(Req, atom_to_binary(Code, utf8));
        _ ->
            enterprise_internal_error:reply(Req, <<"internal_error">>)
    end;
source_page(_, _, Req, _) ->
    cowboy_req:reply(405, #{<<"allow">> => <<"GET">>}, <<>>, Req).

source_query(Req) ->
    try cowboy_req:parse_qs(Req) of
        Pairs ->
            Query = maps:from_list(Pairs),
            case map_size(Query) =:= length(Pairs) of
                true -> {ok, Query};
                false -> {error, invalid_request}
            end
    catch
        exit:{request_error, qs, _} -> {error, invalid_request}
    end.

sync(<<"POST">>, Req0, State) ->
    Ctx = maps:get(enterprise_internal, State, #{}),
    Key = cowboy_req:header(<<"idempotency-key">>, Req0, <<>>),
    case enterprise_directory_sync_logic:sync(Ctx, Key, enterprise_internal_request:params(Req0)) of
        {ok, Kind, #{response_code := Code, response_body := Body}} ->
            Headers = #{<<"content-type">> => <<"application/json">>},
            ReplyHeaders =
                case Kind of
                    replay -> maps:from_list([enterprise_internal_idempotency:replay_header()]);
                    applied -> #{}
                end,
            cowboy_req:reply(Code, maps:merge(Headers, ReplyHeaders), Body, Req0);
        {error, Reason} ->
            enterprise_internal_error:reply(Req0, sync_error(Reason));
        _ ->
            enterprise_internal_error:reply(Req0, <<"internal_error">>)
    end;
sync(_, Req0, _) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

sync_error(invalid_request) -> <<"invalid_request">>;
sync_error(invalid_idempotency_key) -> <<"invalid_request">>;
sync_error(organization_boundary_violation) -> <<"organization_boundary_violation">>;
sync_error(invalid_source_context) -> <<"invalid_credential">>;
sync_error(source_not_enabled) -> <<"organization_boundary_violation">>;
sync_error(directory_source_not_configured) -> <<"organization_boundary_violation">>;
sync_error(directory_bootstrap_not_authorized) -> <<"organization_boundary_violation">>;
sync_error(organization_not_active) -> <<"organization_boundary_violation">>;
sync_error(security_gate_closed) -> <<"security_gate_closed">>;
sync_error(local_membership_conflict) -> <<"resource_conflict">>;
sync_error({membership_conflict, _}) -> <<"resource_conflict">>;
sync_error(confirmed_human_owner_required) -> <<"resource_conflict">>;
sync_error(initial_owner_conflict) -> <<"resource_conflict">>;
sync_error(not_found) -> <<"resource_not_found">>;
sync_error(digest_conflict) -> <<"idempotency_conflict">>;
sync_error(idempotency_pending) -> <<"idempotency_conflict">>;
sync_error(stale_source_version) -> <<"version_conflict">>;
sync_error(source_version_conflict) -> <<"version_conflict">>;
sync_error(identity_conflict) -> <<"resource_conflict">>;
sync_error(cycle) -> <<"invalid_request">>;
sync_error(invalid_source_membership_status) -> <<"invalid_request">>;
sync_error(department_archived) -> <<"resource_conflict">>;
sync_error(department_not_empty) -> <<"resource_conflict">>;
sync_error(name_conflict) -> <<"resource_conflict">>;
sync_error(conflict) -> <<"resource_conflict">>;
sync_error(local_department_admin_conflict) -> <<"resource_conflict">>;
sync_error(department_not_found) -> <<"resource_not_found">>;
sync_error(parent_not_found) -> <<"resource_not_found">>;
sync_error(active_source_memberships) -> <<"resource_conflict">>;
sync_error(directory_only_application_required) -> <<"organization_boundary_violation">>;
sync_error({400, _}) -> <<"invalid_request">>;
sync_error({403, _}) -> <<"organization_boundary_violation">>;
sync_error({409, _}) -> <<"resource_conflict">>;
sync_error(_) -> <<"internal_error">>.

%% @doc INT-16 映射目录（等价于 enterprise_identity_handler 的 directory action；
%% 两条路由指向同一逻辑，Router 侧任选其一登记即可）。
-spec mappings(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
mappings(<<"POST">>, Req0, State) ->
    Ctx = maps:get(enterprise_internal, State, #{}),
    Params = enterprise_internal_request:params(Req0),
    run(Req0, Ctx, <<"INT-16">>, fun(Conn) ->
        enterprise_directory_logic:page_mappings_tx(Conn, Ctx, page_opts(Params))
    end);
mappings(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc INT-17 成员目录（最小字段 + 可选 workspace 过滤）。
-spec users(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
users(<<"POST">>, Req0, State) ->
    Ctx = maps:get(enterprise_internal, State, #{}),
    Params = enterprise_internal_request:params(Req0),
    run(Req0, Ctx, <<"INT-17">>, fun(Conn) ->
        enterprise_directory_logic:page_users_tx(Conn, Ctx, page_opts(Params))
    end);
users(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec run(cowboy_req:req(), map(), binary(), fun((any()) -> term())) -> cowboy_req:req().
run(Req0, Ctx, RouteId, Fun) ->
    Opts = page_opts(enterprise_internal_request:params(Req0)),
    Result =
        elib_pg:with_tx(fun(Conn) ->
            case
                enterprise_internal_boundary:enforce(
                    Conn, Ctx, RouteId, maps:get(workspace_id, Opts, undefined)
                )
            of
                ok ->
                    Fun(Conn);
                {error, Code} ->
                    {error, {enterprise_internal_boundary:error_code(Code), resource_attribution}}
            end
        end),
    case Result of
        {ok, Page} ->
            reply_json(Req0, 200, Page);
        {error, {Code, _Detail}} ->
            enterprise_internal_error:reply(Req0, Code);
        {rollback, Reason} ->
            ?ERROR_LOG("enterprise_directory_handler rollback: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>);
        {error, Reason} ->
            ?ERROR_LOG("enterprise_directory_handler error: ~p~n", [Reason]),
            enterprise_internal_error:reply(Req0, <<"internal_error">>)
    end.

%% @doc 分页入参（只取契约内已知键；未知键丢弃）。
-spec page_opts(map()) -> map().
page_opts(Params) when is_map(Params) ->
    Opts0 = #{
        cursor => maps:get(<<"cursor">>, Params, undefined),
        page_size => maps:get(<<"page_size">>, Params, undefined)
    },
    case maps:get(<<"workspace_id">>, Params, undefined) of
        undefined -> Opts0;
        WsId -> Opts0#{workspace_id => WsId}
    end.

-spec reply_json(cowboy_req:req(), non_neg_integer(), map()) -> cowboy_req:req().
reply_json(Req0, Status, Map) ->
    Body = jsone:encode(Map),
    cowboy_req:reply(Status, #{<<"content-type">> => <<"application/json">>}, Body, Req0).
