-module(enterprise_oa_bootstrap).

-moduledoc "默认 OA 应用部署引导（M0，ADR 2026-10-12 定位修订 §4.4/§5）。".
%%%
% enterprise_oa_bootstrap 是 OA 免登「开箱即用」的部署引导（M0）：
% 配置 oa_redirect_uri（sys.config 主通道，见下方配置段）后，启动期自动登记
% 实例级默认 OA 应用（application_key 实例内全局唯一；organization_id 仅为
% 业务归属）+ 生效 Grant（sso:exchange，按生效 scope 判定；幂等键带时间
% 后缀以支持撤销后重授）。幂等：重复启动零副作用，只补缺口。
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
% 配置通道（两层，app env 优先）：
%   1. sys.config（config/sys.pro.config 的 {imboy, [...]}，权威配置面）：
%      {oa_redirect_uri,    "https://oa.example.com/sso/imboy/callback"}  必填才触发
%      {oa_application_key, <<"default-oa">>}                            可选
%      {oa_name,            <<"办公">>}                                   可选
%   2. OS env 兜底（部署 .env 注入的旧通道，兼容保留）：
%      IMBOY_OA_REDIRECT_URI / IMBOY_OA_APPLICATION_KEY / IMBOY_OA_NAME
%%%

-export([maybe_seed/0]).

-include("log.hrl").

-define(DEFAULT_KEY, <<"default-oa">>).
-define(DEFAULT_NAME, <<"办公"/utf8>>).

%% @doc 启动期引导入口（imboy_app:start/2 在 sup 启动后调用）。
%% oa_redirect_uri（sys.config 或 OS env）未配置时零动作；异常自吞。
-spec maybe_seed() -> ok.
maybe_seed() ->
    case oa_redirect_uri() of
        undefined ->
            ok;
        RedirectUri ->
            try
                Key = cfg_bin(oa_application_key, ?DEFAULT_KEY),
                Name = cfg_bin(oa_name, ?DEFAULT_NAME),
                %% with_tx 契约：成功直接返回事务 fun 的结果（不包 {ok,_}）；
                %% 回滚 {rollback,_} / 连接错误 {error,_} 是正常返回形状
                case
                    elib_pg:with_tx(fun(Conn) ->
                        seed_tx(Conn, Key, Name, RedirectUri)
                    end)
                of
                    #{org_id := _OrgId, application_id := _AppId} = Seeded ->
                        ?INFO_LOG([
                            enterprise_oa_bootstrap_seeded,
                            Seeded#{application_key => Key}
                        ]),
                        ok;
                    {rollback, Reason} ->
                        ?ERROR_LOG([
                            enterprise_oa_bootstrap_failed,
                            #{reason => {rollback, Reason}}
                        ]),
                        ok;
                    {error, Reason} ->
                        ?ERROR_LOG([
                            enterprise_oa_bootstrap_failed,
                            #{reason => Reason}
                        ]),
                        ok
                end
            catch
                %% 与 try 块内 case 分支的 Reason 区分命名，避免 unsafe variable
                Class:FailReason:Stack ->
                    %% 引导失败不阻断启动；工作台入口静默隐藏兜底
                    ?ERROR_LOG([
                        enterprise_oa_bootstrap_failed,
                        #{class => Class, reason => FailReason, stack => Stack}
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

%% redirect_uri 未配置（两通道都无/空）⇒ undefined ⇒ 零动作。
-spec oa_redirect_uri() -> binary() | undefined.
oa_redirect_uri() ->
    case cfg_bin(oa_redirect_uri, <<>>) of
        <<>> -> undefined;
        Bin -> Bin
    end.

%% 业务归属 org：OA 免登与 org 无关（授权边界=部署实例），organization_id
%% 仅作 DB 业务归属列——取实例首个 active 组织，不提供配置项。
-spec resolve_org(any()) -> integer().
resolve_org(Conn) ->
    Sql = <<"SELECT id FROM organization WHERE status = 'active' ORDER BY id LIMIT 1">>,
    case elib_pg:query(Conn, Sql, []) of
        {ok, [#{<<"id">> := OrgId} | _]} ->
            OrgId;
        {ok, []} ->
            erlang:error(no_active_organization);
        {error, Reason} ->
            erlang:error({org_lookup, Reason})
    end.

%%%===================================================================
%%% Config helpers（sys.config 主通道 + OS env 兜底）
%%%===================================================================

%% app env 值统一转 binary（config 里写 string/binary 均可；atom 容错）。
%% 未配置（undefined/<<>>）时回落 OS env，最终落 Default。
-spec cfg_bin(atom(), binary()) -> binary().
cfg_bin(ConfigKey, Default) ->
    case config_ds:env(ConfigKey, undefined) of
        undefined -> env_bin(env_name(ConfigKey), Default);
        <<>> -> env_bin(env_name(ConfigKey), Default);
        Value -> to_binary(Value)
    end.

%% config 键 oa_redirect_uri -> OS env IMBOY_OA_REDIRECT_URI（命名对齐）。
env_name(oa_redirect_uri) -> "IMBOY_OA_REDIRECT_URI";
env_name(oa_application_key) -> "IMBOY_OA_APPLICATION_KEY";
env_name(oa_name) -> "IMBOY_OA_NAME".

-spec to_binary(term()) -> binary().
to_binary(Value) when is_binary(Value) -> Value;
to_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
to_binary(Value) when is_list(Value) -> unicode:characters_to_binary(Value).

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
