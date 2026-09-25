%%% @doc INT-BE-05（INT-API-02D）：Application credential / Grant 生命周期
%%% 全链时序闭环套件（intbe02_http_support 同款 harness——真 Cowboy + 真
%%% internal credential 认证链 + disposable marker PG）。
%%%
%%% 验收对照（计划 v1.1 INT-BE-05）：
%%%   * 创建 Application → 签发 credential（secret 只展示一次、DB 只存
%%%     digest）→ 授予精确 scope/workspace → Internal 调用成功；
%%%   * rotate 后旧 secret 立即 401、新 secret 成功；revoke Grant /
%%%     credential 后立即拒绝；
%%%   * zero Grant、expired Grant、disabled Application 全部 fail-closed；
%%%   * 全链审计完整（credential_issued/rotated/revoked、grant_issued/
%%%     grant_revoked 落 enterprise_audit_event）。
%%%
%%% 管理侧动作经 `enterprise_admin_governance_logic`（append_audit 同事务
%%% 真源，池化路径落 marker 库）；internal 侧经真 HTTP（31-op 冻结面内
%%% INT-01）。套件不触碰共享库。
-module(enterprise_app_credential_lifecycle_http_pg_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SUP, intbe02_http_support).

-define(ORG, 995101).
-define(KEY_L, <<"intbe05-oa-lifecycle">>).
-define(KEY_M, <<"intbe05-oa-grantrev">>).
-define(KEY_Z, <<"intbe05-oa-zerogrant">>).
-define(SCOPES, [<<"application:read">>, <<"workspaces:read">>, <<"groups:read">>]).
-define(ACTOR, #{adm_user_id => 995001, account => <<"intbe05-admin">>}).
%% intbe02 种子矩阵的 principal（995 段；套件动态 app 复用同 principal）。
-define(PRIN, 995014).

app_credential_lifecycle_http_pg_test_() ->
    {setup, fun ?SUP:setup_all/0, fun ?SUP:teardown_all/1, fun cases/1}.

cases(State) ->
    [
        {timeout, 600, fun() -> full_lifecycle_rotate_revoke(State) end},
        {timeout, 300, fun() -> grant_revoke_immediately_denies(State) end},
        {timeout, 300, fun() -> secret_shown_once_digest_only(State) end},
        {timeout, 300, fun() -> zero_grant_disabled_expired_fail_closed(State) end},
        {timeout, 300, fun() -> audit_trail_complete(State) end}
    ].

%% ===================================================================
%% 全链：create → credential → grant → internal 200 → rotate（旧 401 /
%% 新 200）→ revoke credential（新 401）
%% ===================================================================

full_lifecycle_rotate_revoke(State) ->
    {ok, App} = elib_pg:with_conn(fun(C) ->
        enterprise_application_repo:create_tx(
            C, ?ORG, ?KEY_L, <<"intbe05 lifecycle oa"/utf8>>, {?PRIN, ?SCOPES}
        )
    end),
    AppId = grant_int(maps:get(<<"id">>, App)),
    {ok, #{<<"secret">> := Full1, <<"credential">> := Meta1}} =
        enterprise_admin_governance_logic:issue_credential(?ORG, AppId, undefined, ?ACTOR),
    CredId = grant_int(maps:get(<<"id">>, Meta1)),
    ok = issue_grant_ok(AppId, <<"intbe05-g-l">>),
    %% internal 调用成功（INT-01 真 HTTP）。
    ?assertEqual(200, internal_status(State, Full1)),
    %% rotate：旧 secret 立即 401、新 secret 200（rotate 撤旧签新——
    %% 旧 credential 行状态机转 revoked）。
    {ok, #{<<"secret">> := Full2, <<"credential">> := Meta2}} =
        enterprise_admin_governance_logic:rotate_credential(?ORG, AppId, CredId, ?ACTOR),
    NewCredId = grant_int(maps:get(<<"id">>, Meta2)),
    ?assertNotEqual(Full1, Full2),
    ?assertEqual(401, internal_status(State, Full1), "rotate 后旧 secret 必须立即失效"),
    ?assertEqual(200, internal_status(State, Full2)),
    ?assertEqual(
        <<"revoked">>,
        one(
            <<
                "SELECT status FROM enterprise_application_credential"
                " WHERE organization_id = $1 AND id = $2"
            >>,
            [?ORG, CredId]
        )
    ),
    %% revoke rotate 后的新 credential：新 secret 立即 401（行转 revoked）。
    ok = enterprise_admin_governance_logic:revoke_credential(?ORG, AppId, NewCredId, ?ACTOR),
    ?assertEqual(401, internal_status(State, Full2), "revoke 后新 secret 必须立即失效"),
    ?assertEqual(
        <<"revoked">>,
        one(
            <<
                "SELECT status FROM enterprise_application_credential"
                " WHERE organization_id = $1 AND id = $2"
            >>,
            [?ORG, NewCredId]
        )
    ).

%% ===================================================================
%% revoke Grant 后立即拒绝（credential 本身仍 active）
%% ===================================================================

grant_revoke_immediately_denies(State) ->
    {ok, App} = elib_pg:with_conn(fun(C) ->
        enterprise_application_repo:create_tx(
            C, ?ORG, ?KEY_M, <<"intbe05 grantrev oa"/utf8>>, {?PRIN, ?SCOPES}
        )
    end),
    AppId = grant_int(maps:get(<<"id">>, App)),
    {ok, #{<<"secret">> := Full}} =
        enterprise_admin_governance_logic:issue_credential(?ORG, AppId, undefined, ?ACTOR),
    {ok, GrantView} = issue_grant_view(AppId, <<"intbe05-g-m">>),
    %% grant view 是 JSON 投影（id/version 为 binary 整数字符串）——
    %% 管理面 CAS 入口吃 integer。
    GrantId = grant_int(maps:get(<<"id">>, GrantView)),
    Version = grant_int(maps:get(<<"version">>, GrantView)),
    ?assertEqual(200, internal_status(State, Full)),
    %% revoke grant（patch revoke=true）→ 立即 403（zero grant fail-closed）。
    ok =
        enterprise_admin_governance_logic:patch_grant(
            ?ORG, AppId, GrantId, Version, #{revoke => true}, ?ACTOR
        ),
    ?assertEqual(403, internal_status(State, Full), "revoke grant 后必须立即拒绝").

%% ===================================================================
%% secret 只展示一次、DB 只存 digest
%% ===================================================================

secret_shown_once_digest_only(State) ->
    {ok, App} = elib_pg:with_conn(fun(C) ->
        enterprise_application_repo:create_tx(
            C, ?ORG, <<"intbe05-oa-secret">>, <<"intbe05 secret oa"/utf8>>, {?PRIN, ?SCOPES}
        )
    end),
    AppId = grant_int(maps:get(<<"id">>, App)),
    {ok, #{<<"secret">> := Full, <<"credential">> := MetaS}} =
        enterprise_admin_governance_logic:issue_credential(?ORG, AppId, undefined, ?ACTOR),
    CredId = grant_int(maps:get(<<"id">>, MetaS)),
    %% 展示形态 = prefix.secret 一次性完整凭证（secret 服务端生成式）。
    ?assertMatch(<<"ib_int_", _/binary>>, Full),
    [Prefix, SecretPart] = binary:split(Full, <<".">>),
    ?assertEqual(Prefix, maps:get(<<"credential_prefix">>, MetaS)),
    ?assert(byte_size(SecretPart) >= 32),
    %% DB 行只有 digest（64 hex），无明文列且 digest ≠ 明文。
    Columns = one(
        <<
            "SELECT string_agg(column_name, ',') FROM information_schema.columns"
            " WHERE table_name = 'enterprise_application_credential'"
        >>,
        []
    ),
    ?assert(is_binary(Columns)),
    ?assert(
        binary:match(Columns, <<"secret">>) =:= nomatch orelse
            binary:match(Columns, <<"secret_digest">>) =/= nomatch,
        {credential_columns, Columns}
    ),
    Digest = one(
        <<
            "SELECT secret_digest FROM enterprise_application_credential"
            " WHERE organization_id = $1 AND id = $2"
        >>,
        [?ORG, CredId]
    ),
    ?assertEqual(64, byte_size(Digest)),
    ?assertNotEqual(SecretPart, Digest),
    %% digest = secret 段的 SHA-256 hex（存储口径复核）。
    Expected = binary:encode_hex(crypto:hash(sha256, SecretPart), lowercase),
    ?assertEqual(Expected, Digest),
    %% 读面（admin list）永不回显 secret/digest。
    Creds = enterprise_admin_governance_logic:list_credentials(?ORG, AppId),
    ?assertMatch({ok, _}, Creds),
    {ok, CredRows} = Creds,
    lists:foreach(
        fun(Row) ->
            ?assertNot(is_map_key(<<"secret">>, Row)),
            ?assertNot(is_map_key(<<"secret_digest">>, Row))
        end,
        CredRows
    ).

%% ===================================================================
%% zero Grant / disabled Application / expired Grant 全部 fail-closed
%% ===================================================================

zero_grant_disabled_expired_fail_closed(State) ->
    %% zero grant：credential 有效但无任何 Grant → 403。
    {ok, AppZ} = elib_pg:with_conn(fun(C) ->
        enterprise_application_repo:create_tx(
            C, ?ORG, ?KEY_Z, <<"intbe05 zerogrant oa"/utf8>>, {?PRIN, ?SCOPES}
        )
    end),
    AppZId = grant_int(maps:get(<<"id">>, AppZ)),
    {ok, #{<<"secret">> := FullZ}} =
        enterprise_admin_governance_logic:issue_credential(?ORG, AppZId, undefined, ?ACTOR),
    ?assertEqual(403, internal_status(State, FullZ)),
    %% disabled Application：credential 有效也 403。
    {ok, #{version := VerZ}} = app_version(AppZId),
    ok = enterprise_admin_governance_logic:set_status(?ORG, AppZId, VerZ, <<"disabled">>, ?ACTOR),
    {ok, #{<<"secret">> := FullD}} =
        enterprise_admin_governance_logic:issue_credential(?ORG, AppZId, undefined, ?ACTOR),
    ?assertEqual(403, internal_status(State, FullD)),
    %% expired Grant：valid window 已过 → 403。
    {ok, AppE} = elib_pg:with_conn(fun(C) ->
        enterprise_application_repo:create_tx(
            C, ?ORG, <<"intbe05-oa-expired">>, <<"intbe05 expired oa"/utf8>>, {?PRIN, ?SCOPES}
        )
    end),
    AppEId = grant_int(maps:get(<<"id">>, AppE)),
    {ok, #{<<"secret">> := FullE}} =
        enterprise_admin_governance_logic:issue_credential(?ORG, AppEId, undefined, ?ACTOR),
    ok = issue_grant_expired(AppEId, <<"intbe05-g-e">>),
    ?assertEqual(403, internal_status(State, FullE), "expired grant 必须 fail-closed").

%% ===================================================================
%% 全链审计完整（五类动作各有落行）
%% ===================================================================

audit_trail_complete(State) ->
    Counts = one(
        <<
            "SELECT string_agg(action || '=' || n::text, ',') FROM ("
            "  SELECT action, count(*) AS n FROM enterprise_audit_event"
            "  WHERE organization_id = $1 AND resource_type = 'enterprise_application'"
            "    AND action IN ('credential_issued','credential_rotated',"
            "                   'credential_revoked','grant_issued','grant_revoked')"
            "  GROUP BY action) t"
        >>,
        [?ORG]
    ),
    ?assert(is_binary(Counts), {audit_counts, Counts}),
    ?assert(binary:match(Counts, <<"credential_issued=">>) =/= nomatch),
    ?assert(binary:match(Counts, <<"credential_rotated=">>) =/= nomatch),
    ?assert(binary:match(Counts, <<"credential_revoked=">>) =/= nomatch),
    ?assert(binary:match(Counts, <<"grant_issued=">>) =/= nomatch),
    ?assert(binary:match(Counts, <<"grant_revoked=">>) =/= nomatch).

%% ===================================================================
%% helpers
%% ===================================================================

issue_grant_ok(AppId, IdemKey) ->
    {ok, _} = issue_grant_view(AppId, IdemKey),
    ok.

issue_grant_view(AppId, IdemKey) ->
    enterprise_admin_governance_logic:issue_grant(
        ?ORG,
        AppId,
        #{
            scopes => ?SCOPES,
            workspace_scope_kind => none,
            workspace_ids => [],
            idempotency_key => IdemKey,
            valid_to => <<"2099-01-01T00:00:00Z">>
        },
        1,
        ?ACTOR
    ).

issue_grant_expired(AppId, IdemKey) ->
    %% 管理面拒签过去窗口（invalid_grant_value，另一条 fail-closed）；
    %% 自然过期路径：先签有效 Grant，再把 expires_at 改到过去。
    {ok, View} = issue_grant_view(AppId, IdemKey),
    GrantId = grant_int(maps:get(<<"id">>, View)),
    ok = elib_pg:with_conn(fun(C) ->
        {ok, _} = elib_pg:execute(
            C,
            <<
                "UPDATE enterprise_application_grant"
                "   SET valid_from = '2019-01-01T00:00:00Z',"
                "       expires_at = '2020-01-01T00:00:00Z'"
                " WHERE organization_id = $1 AND application_id = $2 AND id = $3"
            >>,
            [?ORG, AppId, GrantId]
        ),
        ok
    end),
    ok.

app_version(AppId) ->
    Row =
        one_map(
            <<
                "SELECT id, version FROM enterprise_application"
                " WHERE organization_id = $1 AND id = $2"
            >>,
            [?ORG, AppId]
        ),
    {ok, #{version => maps:get(<<"version">>, Row)}}.

%% JSON 投影整数（binary | integer 兼容）。
grant_int(V) when is_integer(V) -> V;
grant_int(V) when is_binary(V) -> binary_to_integer(V).

%% INT-01 GET /application 的裸状态码（认证失败 401 / 授权不足 403 / 成功 200）。
internal_status(State, FullCredential) ->
    R = ?SUP:http(
        maps:get(port, State),
        <<"GET">>,
        <<"/api/internal/v1/application">>,
        <<>>,
        ?SUP:auth(FullCredential)
    ),
    maps:get(status, R).

one(Sql, Params) ->
    case elib_pg:query(Sql, Params) of
        {ok, [Row | _]} ->
            case maps:values(Row) of
                [Value | _] -> Value;
                [] -> undefined
            end;
        {ok, []} ->
            undefined;
        {error, Reason} ->
            erlang:error({intbe05_sql_failed, Sql, Reason})
    end.

one_map(Sql, Params) ->
    case elib_pg:query(Sql, Params) of
        {ok, [Row | _]} -> Row;
        {error, Reason} -> erlang:error({intbe05_sql_failed, Sql, Reason})
    end.
