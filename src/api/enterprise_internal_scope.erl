-module(enterprise_internal_scope).

%%%
% enterprise_internal_scope 是 internal API 固定 scope 枚举与授权判定
% （EPGZ-02，plan-gz §4.2 / manifest INV-4；V2.1 原 14 值，坐席接口追加 2 值）。
%
% 冻结规则：
%   * scope 全集恰为 16 个固定值，无 wildcard（"*" 不是合法授予）；
%   * authorize/2 只做**逐字精确成员**判定——
%       - required 不在全集（且非 "*"") → {error, invalid_scope}
%         （调用方（handler）要求了不存在的 scope，属程序错误，fail-closed）；
%       - required = "*" → {error, insufficient_scope}（通配不存在，任何
%         授予集都不满足，包括 granted 里的 "*" 字面量）；
%       - granted 含 "*" 不代表任何授权（无隐含包含）；
%   * messages:send_as_human / friend_requests:create / webhooks:manage
%     必须显式授予，不被 messages:send 隐式包含（负例测试钉死）；
%   * V2.1 新增 4 个只读 scope（groups:read / workspaces:read /
%     projects:read / channels:read，INT-18/24..31）：read 不隐含 write、
%     write 不隐含 read（§7「无 read implies write」双向成立）。
%%%

-export([all/0, authorize/2]).

-define(SCOPES, [
    <<"application:read">>,
    <<"identities:read">>,
    <<"identities:write">>,
    <<"groups:read">>,
    <<"groups:write">>,
    <<"workspaces:read">>,
    <<"projects:read">>,
    <<"channels:read">>,
    <<"files:write">>,
    <<"messages:send">>,
    <<"messages:send_as_human">>,
    <<"friend_requests:create">>,
    <<"webhooks:manage">>,
    <<"sso:exchange">>,
    <<"customer_service:read">>,
    <<"customer_service:write">>
]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 固定 scope 全集（固定 16 个；原枚举顺序保留，坐席读写追加）。
-spec all() -> [binary(), ...].
all() ->
    ?SCOPES.

%% @doc 精确成员判定：Required 必须是固定 scope 且逐字出现在 Granted 中。
%% 无 wildcard、无前缀隐含、无任何隐式包含。
-spec authorize(binary(), [binary()]) -> ok | {error, insufficient_scope | invalid_scope}.
authorize(<<"*">>, _Granted) ->
    %% 通配符不是合法 required：任何授予集都不满足（含 granted 里的 "*"）
    {error, insufficient_scope};
authorize(Required, Granted) when is_binary(Required), is_list(Granted) ->
    case lists:member(Required, ?SCOPES) of
        false ->
            {error, invalid_scope};
        true ->
            case lists:member(Required, [G || G <- Granted, is_binary(G)]) of
                true -> ok;
                false -> {error, insufficient_scope}
            end
    end.
