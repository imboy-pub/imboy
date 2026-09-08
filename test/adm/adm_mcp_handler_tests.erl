-module(adm_mcp_handler_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc adm_mcp_handler 治理端点回归（MCP-01）。
%%% 验收：权限门（adm_acl:ensure_permission）与治理 logic 透传/响应映射。
%%% cowboy_req/elib_response 经 meck non_strict mock；治理 logic mock，无需 DB。
%%%===================================================================

adm_mcp_handler_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun t_list_passes_through/1,
        fun t_approve_calls_logic/1,
        fun t_create_calls_logic/1,
        fun t_set_grant_calls_logic/1,
        fun t_perm_denied_short_circuits/1
    ]}.

setup() ->
    meck:new(mcp_governance_logic, [no_link]),
    meck:new(adm_acl, [no_link]),
    meck:new(elib_param, [no_link]),
    meck:new(elib_response, [no_link, non_strict]),
    meck:new(cowboy_req, [no_link, non_strict]),
    meck:expect(adm_acl, ensure_permission, fun(_State, _Perm, _Req) -> ok end),
    meck:expect(elib_param, page, fun(_Req) -> {1, 20} end),
    meck:expect(elib_param, binary, fun(_Key, _Req, Default) -> {ok, Default} end),
    meck:expect(elib_param, post, fun(_Req) -> #{<<"client_id">> => 900} end),
    meck:expect(cowboy_req, method, fun(_) -> <<"GET">> end),
    meck:expect(cowboy_req, header, fun(_, _) -> undefined end),
    meck:expect(cowboy_req, peer, fun(_) -> {{127, 0, 0, 1}, 50000} end),
    meck:expect(
        elib_response,
        success,
        fun(Req, _Body) ->
            put(reply, success),
            Req
        end
    ),
    meck:expect(
        elib_response,
        success,
        fun(Req, _Body, _Msg) ->
            put(reply, success),
            Req
        end
    ),
    meck:expect(
        elib_response,
        error,
        fun(Req, Msg) ->
            put(reply, {error, Msg}),
            Req
        end
    ),
    meck:expect(
        elib_response,
        error,
        fun(Req, Msg, _Code) ->
            put(reply, {error, Msg}),
            Req
        end
    ),
    meck:expect(
        elib_response,
        error_with_status,
        fun(Req, _Code, Msg) ->
            put(reply, {error, Msg}),
            Req
        end
    ),
    ok.

cleanup(_) ->
    erase(reply),
    meck:unload().

t_list_passes_through(_) ->
    fun() ->
        meck:expect(mcp_governance_logic, list_clients, fun(1, 20, <<>>, <<>>) ->
            {ok, #{<<"total">> => 0}}
        end),
        {ok, _Req, _S} = adm_mcp_handler:init(req(), #{action => list}),
        ?assertEqual(success, get(reply)),
        ?assert(meck:called(mcp_governance_logic, list_clients, '_'))
    end.

t_approve_calls_logic(_) ->
    fun() ->
        meck:expect(cowboy_req, method, fun(_) -> <<"POST">> end),
        meck:expect(mcp_governance_logic, approve, fun(900, _AdmUid, <<"127.0.0.1">>) ->
            {ok, #{<<"client_id">> => 900, <<"status">> => <<"approved">>}}
        end),
        {ok, _Req, _S} = adm_mcp_handler:init(req(), #{action => approve, adm_uid => 1}),
        ?assertEqual(success, get(reply)),
        ?assert(meck:called(mcp_governance_logic, approve, '_'))
    end.

t_create_calls_logic(_) ->
    fun() ->
        meck:expect(cowboy_req, method, fun(_) -> <<"POST">> end),
        meck:expect(elib_param, post, fun(_Req) ->
            #{
                <<"owner_uid">> => <<"7001">>,
                <<"name">> => <<"ext01-client">>,
                <<"description">> => <<"smoke">>
            }
        end),
        %% create action 直接调 repo；mock repo 层验证参数规整（owner_uid 转整数）
        meck:new(mcp_client_repo, [no_link, non_strict]),
        meck:expect(mcp_client_repo, create_client, fun(7001, <<"ext01-client">>, <<"smoke">>) ->
            {ok, #{<<"client_id">> => 900, <<"secret">> => <<"s">>}}
        end),
        {ok, _Req, _S} = adm_mcp_handler:init(req(), #{action => create}),
        ?assertEqual(success, get(reply)),
        ?assert(meck:called(mcp_client_repo, create_client, '_'))
    end.

t_set_grant_calls_logic(_) ->
    fun() ->
        meck:expect(cowboy_req, method, fun(_) -> <<"POST">> end),
        meck:expect(elib_param, post, fun(_Req) ->
            #{
                <<"client_id">> => 900,
                <<"tool">> => <<"create_agent_task">>,
                <<"enabled">> => true
            }
        end),
        meck:expect(
            mcp_governance_logic,
            set_grant,
            fun(900, <<"create_agent_task">>, true) ->
                {ok, #{
                    <<"client_id">> => 900,
                    <<"tool">> => <<"create_agent_task">>,
                    <<"enabled">> => true
                }}
            end
        ),
        {ok, _Req, _S} = adm_mcp_handler:init(req(), #{action => set_grant}),
        ?assertEqual(success, get(reply)),
        ?assert(meck:called(mcp_governance_logic, set_grant, '_'))
    end.

t_perm_denied_short_circuits(_) ->
    fun() ->
        %% 权限门拒绝：直接短路返回，绝不触达治理 logic
        meck:expect(
            adm_acl,
            ensure_permission,
            fun(_State, _Perm, _Req) -> {error, denied} end
        ),
        meck:expect(mcp_governance_logic, list_clients, fun(_, _, _, _) ->
            meck:error(unexpected_call)
        end),
        {ok, R, _S} = adm_mcp_handler:init(req(), #{action => list}),
        ?assertEqual(denied, R),
        ?assertNot(meck:called(mcp_governance_logic, list_clients, '_'))
    end.

%% ===================================================================
req() ->
    %% with_perm 拒绝分支直接把该占位透传回来
    denied.
