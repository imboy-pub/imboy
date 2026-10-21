#!/usr/bin/env escript
%%! -noshell
%% ============================================================
%% M0（ADR 2026-10-12 定位修订）：部署默认 OA 应用 seed（幂等）
%% ------------------------------------------------------------
%% docs/architecture/2026-10-12-organization-positioning-amendment.md §4.4/§5-M0
%%
%% 目标：标准部署发布完成后 OA 免登开箱即用——getAuthCode 不因「未登记」
%% 报 app_not_configured。登记动作从运维手工 SQL 变为本脚本：
%%   1. 创建实例级默认 OA 应用（application_key 实例内全局唯一，
%%      organization_id 仅为业务归属）；
%%   2. redirect_uri 由部署配置注入 allowlist（未配置则跳过 seed，
%%      OA 入口静默隐藏——不报错）；
%%   3. 签发 Application Credential（HMAC-SHA256 签名凭证，仅在新签发时
%%      打印明文一次，由运维转交 OA 团队）。
%%
%% 幂等：按 application_key 定位应用，已存在则只补齐 allowlist/credential
%% 缺口；重复部署零副作用。
%%
%% 用法（imboy 仓根）：
%%   PG_HOST=127.0.0.1 PG_PORT=4323 PG_USER=imboy_user PG_PASSWORD=... \
%%   PG_DATABASE=imboy scripts/seed_default_oa_application.escript
%%
%% redirect_uri 来源优先级：env IMBOY_OA_REDIRECT_URI > 仓内
%% config/sys.pro.config 的 {imboy, [{oa_redirect_uri, "..."}]}（与节点
%% bootstrap 同源配置）。可选 env：IMBOY_OA_APPLICATION_KEY（默认
%% default-oa）/ IMBOY_OA_NAME（默认 办公）
%% ============================================================

-mode(compile).

-define(DEFAULT_KEY, <<"default-oa">>).
-define(DEFAULT_NAME, <<"办公"/utf8>>).

main(_Args) ->
    RepoDir = os:getenv("IMBOY_DIR", "."),
    code:add_pathsa([
        filename:join([RepoDir, "ebin"]),
        filename:join([RepoDir, "deps/epgsql/ebin"]),
        filename:join([RepoDir, "deps/jsone/ebin"])
    ]),
    {ok, _} = application:ensure_all_started(epgsql),
    try
        elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3})
    catch
        _:_ -> ok
    end,
    case redirect_uri(RepoDir) of
        undefined ->
            io:format("SKIP: oa_redirect_uri 未配置（env IMBOY_OA_REDIRECT_URI 与 "
                "config/sys.pro.config 均无）——不登记默认 OA 应用，"
                "OA 入口将静默隐藏（配置后重跑本脚本即可开通）。~n"),
            halt(0);
        RedirectUri ->
            seed(RedirectUri)
    end.

%% redirect_uri：env IMBOY_OA_REDIRECT_URI 优先；否则从
%% {RepoDir}/config/sys.pro.config 读 {imboy, [{oa_redirect_uri, ...}]}
%% （file:consult 解析标准 config 项，与节点 bootstrap 同源）。
redirect_uri(RepoDir) ->
    case env_bin("IMBOY_OA_REDIRECT_URI") of
        undefined -> from_config(filename:join(RepoDir, "config/sys.pro.config"));
        RedirectUri -> RedirectUri
    end.

from_config(Path) ->
    case file:consult(Path) of
        {ok, Terms} ->
            Value = lists:foldl(fun
                ({imboy, Props}, Acc) when is_list(Props) ->
                    case lists:keyfind(oa_redirect_uri, 1, Props) of
                        {oa_redirect_uri, V} when Acc =:= undefined -> to_binary(V);
                        _ -> Acc
                    end;
                (_, Acc) -> Acc
            end, undefined, Terms),
            Value;
        {error, Reason} ->
            io:format("提示：config/sys.pro.config 读取失败（~0p），仅 env 通道可用。~n",
                [Reason]),
            undefined
    end.

to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_binary(V) when is_list(V) -> unicode:characters_to_binary(V).

seed(RedirectUri) ->
    ApplicationKey = env_bin("IMBOY_OA_APPLICATION_KEY", ?DEFAULT_KEY),
    Name = env_bin("IMBOY_OA_NAME", ?DEFAULT_NAME),
    Conn = connect(),
    try
        ok = exec(Conn, <<"BEGIN">>),
        try
            Result = seed_tx(Conn, ApplicationKey, Name, RedirectUri),
            ok = exec(Conn, <<"COMMIT">>),
            report(Result)
        catch
            throw:{seed_error, Reason} ->
                _ = exec(Conn, <<"ROLLBACK">>),
                io:format("FAIL: ~0p~n", [Reason]),
                halt(1)
        end
    after
        epgsql:close(Conn)
    end.

%% seed 主体（调用方持有事务）：定位/创建应用 -> 补齐 allowlist/credential。
seed_tx(Conn, ApplicationKey, Name, RedirectUri) ->
    OrgId = resolve_org(Conn),
    AppId = ensure_application(Conn, OrgId, ApplicationKey, Name, RedirectUri),
    ensure_credential(Conn, OrgId, AppId),
    #{org_id => OrgId, application_id => AppId, application_key => ApplicationKey}.

%% 业务归属 org：OA 免登与 org 无关（授权边界=部署实例），organization_id
%% 仅作 DB 业务归属列——取实例首个 active 组织，不提供配置项。无组织则
%% 无法落 FK——FAIL 提示（部署应先完成组织初始化）。
resolve_org(Conn) ->
    Sql = <<"SELECT id FROM organization WHERE status = 'active' ORDER BY id LIMIT 1">>,
    case elib_pg:query(Conn, Sql, []) of
        {ok, [#{<<"id">> := OrgId} | _]} ->
            OrgId;
        {ok, []} ->
            throw({seed_error, no_active_organization});
        {error, R} ->
            throw({seed_error, {org_lookup, R}})
    end.

ensure_application(Conn, OrgId, ApplicationKey, Name, RedirectUri) ->
    %% 与 enterprise_oa_bootstrap 同口径：按 key 实例级直取（全局唯一），
    %% 不限定 org——key 已登记于其他 org 时同样复用该应用。
    case enterprise_application_repo:find_by_application_key_tx(Conn, ApplicationKey) of
        {ok, App} ->
            adopt_application(Conn, OrgId, App, RedirectUri);
        {error, not_found} ->
            case enterprise_application_repo:create_tx(Conn, OrgId, ApplicationKey, Name, null, [RedirectUri]) of
                {ok, App} ->
                    AppId = maps:get(<<"id">>, App),
                    io:format("APP: 创建 ~ts (org=~p, key=~s)~n", [Name, OrgId, ApplicationKey]),
                    AppId;
                %% 并发/他 org 先种撞 uq_ea_key_global：按 key 重取复用
                {error, key_conflict} ->
                    case enterprise_application_repo:find_by_application_key_tx(Conn, ApplicationKey) of
                        {ok, App2} ->
                            adopt_application(Conn, OrgId, App2, RedirectUri);
                        {error, R} ->
                            throw({seed_error, {application_lookup_after_conflict, R}})
                    end;
                {error, R} ->
                    throw({seed_error, {application_create, R}})
            end;
        {error, R} ->
            throw({seed_error, {application_lookup, R}})
    end.

adopt_application(Conn, OrgId, App, RedirectUri) ->
    AppId = maps:get(<<"id">>, App),
    Allow = maps:get(<<"allowed_redirect_uris">>, App),
    case lists:member(RedirectUri, Allow) of
        true ->
            ok;
        false ->
            ok = enterprise_application_repo:update_redirect_uris_tx(
                Conn, OrgId, AppId, Allow ++ [RedirectUri]
            ),
            io:format("APP: allowlist 追加 ~s~n", [RedirectUri])
    end,
    AppId.

%% credential 幂等：已有未过期 active credential 则复用（明文不可再取，
%% 提示运维用既有凭证）；无则新签发并打印明文（仅此一次）。
ensure_credential(Conn, OrgId, AppId) ->
    Sql = <<
        "SELECT id FROM enterprise_application_credential"
        " WHERE organization_id = $1 AND application_id = $2 AND status = 'active'"
        " AND (expires_at IS NULL OR expires_at > clock_timestamp()) LIMIT 1"
    >>,
    case elib_pg:query(Conn, Sql, [OrgId, AppId]) of
        {ok, [_ | _]} ->
            io:format("CREDENTIAL: 已有生效凭证（明文不再显示；如需新凭证请先轮换）~n"),
            ok;
        {ok, []} ->
            {ok, #{credential := Credential}} =
                enterprise_internal_ops:issue_credential_tx(Conn, OrgId, AppId, undefined),
            io:format("CREDENTIAL: 新签发（明文仅此一次，请立即转交 OA 团队）：~n  ~s~n", [
                Credential
            ]),
            ok;
        {error, R} ->
            throw({seed_error, {credential_lookup, R}})
    end.

report(#{org_id := OrgId, application_id := AppId, application_key := Key}) ->
    io:format("OK: 默认 OA 应用就绪（org=~p, app=~p, key=~s）——"
        "getAuthCode 已可用；OA 侧 exchange 使用上述 credential。~n", [OrgId, AppId, Key]).

%% ---- 基础设施 ----

connect() ->
    %% epgsql 连接参数需 string（gen_tcp host 为 binary 时 badarg）；
    %% 业务值（key/name/redirect）另用 env_bin。
    Opts = #{
        host => env_str("PG_HOST", "127.0.0.1"),
        port => env_int("PG_PORT", 4323),
        username => env_str("PG_USER", "imboy_user"),
        password => env_str("PG_PASSWORD", ""),
        database => env_str("PG_DATABASE", "imboy"),
        timeout => 10000,
        codecs => [{epgsql_codec_rfc3339_bin, []}]
    },
    case epgsql:connect(Opts) of
        {ok, C} -> C;
        {error, R} -> erlang:error({connect_failed, R})
    end.

exec(Conn, Sql) ->
    case epgsql:squery(Conn, Sql) of
        {ok, _, _} -> ok;
        {ok, _} -> ok;
        {error, R} -> erlang:error({sql_error, R, Sql})
    end.

env_bin(Name) ->
    case os:getenv(Name) of
        false -> undefined;
        "" -> undefined;
        V -> unicode:characters_to_binary(V)
    end.

env_bin(Name, Default) ->
    case env_bin(Name) of
        undefined -> Default;
        V -> V
    end.

env_str(Name, Default) ->
    case os:getenv(Name) of
        false -> Default;
        "" -> Default;
        V -> V
    end.

env_int(Name, Default) ->
    case os:getenv(Name) of
        false -> Default;
        V ->
            try
                list_to_integer(V)
            catch
                _:_ -> Default
            end
    end.
