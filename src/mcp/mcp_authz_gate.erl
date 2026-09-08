-module(mcp_authz_gate).

%%%
% MCP 授权闸门 / MCP authorization gate（imboy 集成，非 vendored barrel 通用件）
%
% barrel_mcp_registry:run_tool_worker 在执行 tool 前经 function_exported 守卫可选调用
% 本模块 check/2。返回 ok（放行）| {deny, Content}（拒绝，Content 为 MCP text 内容，
% 上层编成 isError:true 的 tool_error）。
%
% client 身份取 Ctx.auth_info（mcp_handler 从 JWT 注入的 caller uid）。业务判定委托
% mcp_governance_logic:authorize/2（enforce 默认关闭，见该模块）。
%%%

-export([check/2]).

-spec check(binary(), map()) -> ok | {deny, [map()]}.
check(ToolName, Ctx) ->
    %% MCP-01：auth_info 为 mcp_handler 注入的 Principal map
    %% （mcp_governance_logic:authenticate_secret/1 产物，含 owner_uid/client_id/
    %% client_key）；tools 不接受参数自报身份。
    case maps:get(auth_info, Ctx, undefined) of
        #{owner_uid := OwnerUid, client_id := ClientId, client_key := ClientKey} ->
            case mcp_governance_logic:check_rate(ClientKey) of
                allow ->
                    case
                        mcp_governance_logic:authorize_client(
                            ClientId,
                            OwnerUid,
                            ToolName
                        )
                    of
                        allow ->
                            ok;
                        {deny, Reason} ->
                            {deny, [#{<<"type">> => <<"text">>, <<"text">> => Reason}]}
                    end;
                {deny, rate_limited} ->
                    {deny, [
                        #{
                            <<"type">> => <<"text">>,
                            <<"text">> => <<"请求过于频繁，请稍后再试"/utf8>>
                        }
                    ]}
            end;
        _ ->
            {deny, [#{<<"type">> => <<"text">>, <<"text">> => <<"未认证"/utf8>>}]}
    end.
