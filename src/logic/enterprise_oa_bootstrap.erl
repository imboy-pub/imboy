-module(enterprise_oa_bootstrap).

-moduledoc "默认 OA 应用部署引导（M0，ADR 2026-10-12 定位修订 §4.4/§5）。".
%%%
% enterprise_oa_bootstrap 是 OA 免登「开箱即用」的部署引导（M0）：
% 部署环境配置 IMBOY_OA_REDIRECT_URI 后，启动期自动登记实例级默认 OA 应用
% （application_key 实例内全局唯一；organization_id 仅为业务归属）+ 生效
% Grant（sso:exchange，按生效 scope 判定；幂等键带时间后缀以支持撤销后重授）。
% 幂等：重复启动零副作用，只补缺口。
%
% 边界（与 scripts/seed_default_oa_application.escript 的分工）：
%   - 本模块只做应用登记（app + allowlist + grant）——getAuthCode（App 侧
%     签发）由此开箱即用；
%   - credential 不在此签发：明文凭证不得进日志（EPGZ-05 §0 redaction
%     红线），OA 侧 exchange 凭证统一走 escript 运维通道（ stdout 打印
%     一次，由运维转交 OA 团队；脚本幂等，会对已登记应用补发凭证）。
%
% 失败语义：任何异常自吞 + error 日志，绝不阻断节点启动（OA 免登不可用
% 由工作台入口静默隐藏兜底，比启动失败安全得多）。
%
% 环境变量（imboy_env_overrides 同风格的 OS env 直读；部署 .env 注入）：
%   IMBOY_OA_REDIRECT_URI    必填才触发；未配置 = 本模块零动作
%   IMBOY_OA_APPLICATION_KEY 可选，默认 <<"default-oa">>
%   IMBOY_OA_NAME            可选，默认 <<"办公">>
%   IMBOY_OA_ORG_ID          可选；缺省取实例首个 active 组织作业务归属
%%%

-export([maybe_seed/0]).

-include("log.hrl").

-define(DEFAULT_KEY, <<"default-oa">>).
-define(DEFAULT_NAME, <<"办公"/utf8>>).

%% @doc 启动期引导入口（imboy_app:start/2 在 sup 启动后调用）。
%% IMBOY_OA_REDIRECT_URI 未配置时零动作；异常自吞。
-spec maybe_seed() -> ok.
maybe_seed() ->
    case os:getenv("IMBOY_OA_REDIRECT_URI") of
        false ->
            ok;
        "" ->
            ok;
        RedirectStr ->
            try
                RedirectUri = unicode:characters_to_binary(RedirectStr),
                Key = env_bin("IMBOY_OA_APPLICATION_KEY", ?DEFAULT_KEY),
                Name = env_bin("IMBOY_OA_NAME", ?DEFAULT_NAME),
                {ok, #{org_id := OrgId, application_id := AppId}} =
                    elib_pg:with_tx(fun(Conn) ->
                        seed_tx(Conn, Key, Name, RedirectUri)
                    end),
                ?INFO_LOG([
                    enterprise_oa_bootstrap_seeded,
                    #{org_id => OrgId, application_id => AppId, application_key => Key}
                ]),
                ok
            catch
                Class:Reason:Stack ->
                    %% 引导失败不阻断启动；工作台入口静默隐藏兜底
                    ?ERROR_LOG([
                        enterprise_oa_bootstrap_failed,
                        #{class => Class, reason => Reason, stack => Stack}
                    ]),
                    ok
            end
    end.

%%%===================================================================
%%% Internal
%%%===================================================================

seed_tx(Conn, Key, Name, RedirectUri) ->
    OrgId = resolve_org(Conn),
    AppId = ensure_application(Conn, OrgId, Key, Name, RedirectUri),
    ensure_grant(Conn, OrgId, AppId),
    #{org_id => OrgId, application_id => AppId}.

%% 业务归属 org：显式 IMBOY_OA_ORG_ID > 实例首个 active 组织。
-spec resolve_org(any()) -> integer().
resolve_org(Conn) ->
    case env_int("IMBOY_OA_ORG_ID") of
        OrgId when is_integer(OrgId), OrgId > 0 ->
            OrgId;
        _ ->
            Sql = <<"SELECT id FROM organization WHERE status = 'active' ORDER BY id LIMIT 1">>,
            case elib_pg:query(Conn, Sql, []) of
                {ok, [#{<<"id">> := OrgId} | _]} ->
                    OrgId;
                {ok, []} ->
                    erlang:error(no_active_organization);
                {error, Reason} ->
                    erlang:error({org_lookup, Reason})
            end
    end.

-spec ensure_application(any(), integer(), binary(), binary(), binary()) -> integer().
ensure_application(Conn, OrgId, Key, Name, RedirectUri) ->
    %% key 实例内全局唯一（迁移 00000171 uq_ea_key_global），按 key 直取
    case enterprise_application_repo:find_by_application_key_tx(Conn, Key) of
        {ok, #{<<"id">> := AppId, <<"allowed_redirect_uris">> := Allow}} ->
            case lists:member(RedirectUri, Allow) of
                true ->
                    AppId;
                false ->
                    ok = enterprise_application_repo:update_redirect_uris_tx(
                        Conn, OrgId, AppId, Allow ++ [RedirectUri]
                    ),
                    AppId
            end;
        {error, not_found} ->
            {ok, App} = enterprise_application_repo:create_tx(
                Conn, OrgId, Key, Name, {null, [<<"sso:exchange">>]}, [RedirectUri]
            ),
            maps:get(<<"id">>, App);
        {error, Reason} ->
            erlang:error({application_lookup, Reason})
    end.

%% Grant 幂等按**生效 scope**判定（V2.1 §5.2 语义）：sso:exchange 已生效即
%% 跳过；生效集缺失（从未授予/被撤销/过期）才创建——幂等键只能防同刻重复，
%% 不能把 revoked 行误当有效授权（否则撤销后引导永远跳过，exchange 403）。
-spec ensure_grant(any(), integer(), integer()) -> ok.
ensure_grant(Conn, OrgId, AppId) ->
    case enterprise_application_grant_repo:effective_scopes_tx(Conn, OrgId, AppId) of
        {ok, Scopes} ->
            case lists:member(<<"sso:exchange">>, Scopes) of
                true ->
                    ok;
                false ->
                    IdemKey = grant_idem_key(),
                    {ok, _} = enterprise_application_grant_repo:create_tx(
                        Conn, OrgId, AppId, #{
                            scopes => [<<"sso:exchange">>],
                            workspace_scope_kind => none,
                            expires_at => <<"2099-12-31T23:59:59Z">>,
                            idempotency_key => IdemKey
                        }
                    ),
                    ok
            end;
        {error, Reason} ->
            erlang:error({grant_lookup, Reason})
    end.

%% 幂等键带时间后缀：同 (org, app, key) 唯一约束下，撤销后重授需要新键。
-spec grant_idem_key() -> binary().
grant_idem_key() ->
    <<"bootstrap-default-oa-grant-", (integer_to_binary(os:system_time(millisecond)))/binary>>.

%%%===================================================================
%%% Env helpers
%%%===================================================================

-spec env_bin(string(), binary()) -> binary().
env_bin(Name, Default) ->
    case os:getenv(Name) of
        false -> Default;
        "" -> Default;
        V -> unicode:characters_to_binary(V)
    end.

-spec env_int(string()) -> undefined | integer().
env_int(Name) ->
    case os:getenv(Name) of
        false ->
            undefined;
        V ->
            try
                list_to_integer(V)
            catch
                _:_ -> undefined
            end
    end.
