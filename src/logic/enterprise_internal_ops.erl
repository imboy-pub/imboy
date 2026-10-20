-module(enterprise_internal_ops).

-export([
    create_application_tx/4,
    create_application/3,
    set_application_status_tx/4,
    set_application_status/3,
    set_application_status_cas/4,
    set_application_status_cas_tx/5,
    list_credentials/2,
    list_credentials/3,
    issue_credential_tx/5,
    issue_credential_tx/4,
    issue_credential/3,
    rotate_credential_tx/3,
    rotate_credential_tx/4,
    rotate_credential/2,
    revoke_credential_tx/3,
    revoke_credential/2,
    application_status_tx/3,
    application_status/2
]).

-include("log.hrl").

%%%===================================================================
%%% Application 治理
%%%===================================================================

%% @doc 事务内创建 application。
create_application_tx(Conn, OrgId, ApplicationKey, Name) ->
    enterprise_application_repo:create_tx(Conn, OrgId, ApplicationKey, Name).

create_application(OrgId, ApplicationKey, Name) ->
    pool(fun(Conn) -> create_application_tx(Conn, OrgId, ApplicationKey, Name) end).

%% @doc 事务内启停 application（active|disabled）。
-spec set_application_status_tx(any(), integer(), integer(), binary()) ->
    ok | {error, not_found | term()}.
set_application_status_tx(Conn, OrgId, AppId, Status) ->
    enterprise_application_repo:update_status_tx(Conn, OrgId, AppId, Status).

%% @doc 池化启停 application。
-spec set_application_status(integer(), integer(), binary()) -> ok | {error, term()}.
set_application_status(OrgId, AppId, Status) ->
    pool(fun(Conn) -> set_application_status_tx(Conn, OrgId, AppId, Status) end).

-spec set_application_status_cas(integer(), integer(), pos_integer(), binary()) ->
    ok | {error, invalid_status | not_found | version_conflict | term()}.
set_application_status_cas(OrgId, AppId, ExpectedVersion, Status) ->
    pool(fun(Conn) ->
        set_application_status_cas_tx(Conn, OrgId, AppId, ExpectedVersion, Status)
    end).

set_application_status_cas_tx(Conn, OrgId, AppId, ExpectedVersion, Status) ->
    case validate_lifecycle(Status) of
        ok ->
            enterprise_application_repo:update_status_cas_tx(
                Conn, OrgId, AppId, ExpectedVersion, Status
            );
        {error, _} = Err ->
            Err
    end.

%%%===================================================================
%%% Credential 生命周期
%%%===================================================================

%% @doc 事务内签发凭证（secret 由本模块生成，明文只在返回值出现一次）。
-spec issue_credential_tx(any(), integer() | null, integer(), undefined | binary()) ->
    {ok, #{credential_id := integer(), credential := binary(), expires_at := undefined | binary()}}
    | {error, term()}.
issue_credential_tx(Conn, OrgId, AppId, ExpiresAt) ->
    issue_credential_tx(Conn, OrgId, AppId, generate_secret(), ExpiresAt).

%% @doc 事务内签发凭证（显式 secret——备份恢复等运维场景）。
%% 返回 #{credential_id, credential(明文 ib_int_<id>.<secret>), expires_at}。
-spec issue_credential_tx(any(), integer() | null, integer(), binary(), undefined | binary()) ->
    {ok, #{credential_id := integer(), credential := binary(), expires_at := undefined | binary()}}
    | {error, term()}.
issue_credential_tx(Conn, OrgId, AppId, Secret, ExpiresAt) when
    is_binary(Secret), Secret =/= <<>>
->
    PreId = enterprise_application_credential_repo:next_id(),
    Prefix = <<"ib_int_", (integer_to_binary(PreId))/binary>>,
    case
        enterprise_application_credential_repo:create_tx(
            Conn, OrgId, AppId, Prefix, Secret, ExpiresAt
        )
    of
        {ok, Row} ->
            %% 行 id 由 repo 生成（与 prefix 中的定位 id 独立）；
            %% credential_id 以行 id 为准（撤销/轮换按行 id 定位）。
            CredId = maps:get(<<"id">>, Row),
            {ok, #{
                credential_id => CredId,
                credential => <<Prefix/binary, ".", Secret/binary>>,
                expires_at => ExpiresAt
            }};
        {error, Reason} ->
            {error, Reason}
    end;
issue_credential_tx(_Conn, _OrgId, _AppId, _Secret, _ExpiresAt) ->
    {error, invalid_secret}.

%% @doc 池化签发凭证（生成式）。
-spec issue_credential(integer() | null, integer(), undefined | binary()) ->
    {ok, map()} | {error, term()}.
issue_credential(OrgId, AppId, ExpiresAt) ->
    pool(fun(Conn) -> issue_credential_tx(Conn, OrgId, AppId, ExpiresAt) end).

%% @doc 事务内轮换凭证（同事务：签发新凭证 + 撤销旧凭证；
%% 新凭证创建失败时旧凭证保持有效——轮换原子向安全侧倾斜）。
-spec rotate_credential_tx(any(), integer() | null, integer()) ->
    {ok, map()} | {error, term()}.
rotate_credential_tx(Conn, OrgId, OldCredId) ->
    rotate_credential_tx(Conn, OrgId, OldCredId, undefined).

%% @doc 事务内轮换凭证（可指定新凭证过期时间）。
-spec rotate_credential_tx(any(), integer() | null, integer(), undefined | binary()) ->
    {ok, map()} | {error, term()}.
rotate_credential_tx(Conn, OrgId, OldCredId, ExpiresAt) ->
    case rotation_application(Conn, OrgId, OldCredId) of
        {ok, {AppId, CredOrg}} ->
            case issue_credential_tx(Conn, OrgId, AppId, ExpiresAt) of
                {ok, _} = Ok ->
                    case
                        enterprise_application_credential_repo:revoke_tx(Conn, CredOrg, OldCredId)
                    of
                        ok -> Ok;
                        {error, Reason} -> throw({rollback, Reason})
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 池化轮换凭证。
-spec rotate_credential(integer() | null, integer()) -> {ok, map()} | {error, term()}.
rotate_credential(OrgId, OldCredId) ->
    pool(fun(Conn) -> rotate_credential_tx(Conn, OrgId, OldCredId) end).

%% @doc 事务内撤销凭证（幂等语义见 repo：非 active → {error, not_active}）。
-spec revoke_credential_tx(any(), integer() | null, integer()) ->
    ok | {error, not_found | not_active | term()}.
revoke_credential_tx(Conn, OrgId, CredId) ->
    case rotation_application(Conn, OrgId, CredId) of
        {ok, {_AppId, CredOrg}} ->
            enterprise_application_credential_repo:revoke_tx(Conn, CredOrg, CredId);
        {error, _} = Err ->
            Err
    end.

%% @doc 池化撤销凭证。
-spec revoke_credential(integer() | null, integer()) -> ok | {error, term()}.
revoke_credential(OrgId, CredId) ->
    pool(fun(Conn) -> revoke_credential_tx(Conn, OrgId, CredId) end).

%%%===================================================================
%%% 状态查询（redacted：不含 digest / 明文）
%%%===================================================================

%% @doc 事务内查询 application 状态与凭证列表（仅元数据；
%% 不含 secret_digest / 明文 credential —— redaction 红线）。
-spec application_status_tx(any(), integer() | null, integer()) -> {ok, map()} | {error, term()}.
application_status_tx(Conn, OrgId, AppId) ->
    case enterprise_application_repo:find_tx(Conn, OrgId, AppId) of
        {ok, App} ->
            {ok, #{
                application => #{
                    id => maps:get(<<"id">>, App),
                    application_key => maps:get(<<"application_key">>, App),
                    name => maps:get(<<"name">>, App),
                    status => maps:get(<<"status">>, App)
                },
                credentials => list_credentials(Conn, OrgId, AppId)
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 池化查询 application 状态。
-spec application_status(integer() | null, integer()) -> {ok, map()} | {error, term()}.
application_status(OrgId, AppId) ->
    pool(fun(Conn) -> application_status_tx(Conn, OrgId, AppId) end).

%%%===================================================================
%%% Internal
%%%===================================================================

-spec pool(fun()) -> term().
pool(Fun) ->
    case elib_pg:with_tx(Fun) of
        {rollback, Reason} -> {error, Reason};
        Result -> Result
    end.

%% 生命周期四值（迁移 00000143 放宽 ck_ea_status；与 imboyadmin
%% contracts.ts:APPLICATION_STATUSES 同序）。archived 是终态——合法迁移由上层裁决。
-spec validate_lifecycle(binary()) -> ok | {error, invalid_status}.
validate_lifecycle(S) when
    S =:= <<"draft">>; S =:= <<"active">>; S =:= <<"disabled">>; S =:= <<"archived">>
->
    ok;
validate_lifecycle(_) ->
    {error, invalid_status}.

%% 32 字节 CSPRNG → base64url（无 padding，43 字符，熵 ≥ 256 bit）。
-spec generate_secret() -> binary().
generate_secret() ->
    Raw = crypto:strong_rand_bytes(32),
    B64 = base64:encode(Raw),
    NoPad = binary:part(B64, 0, byte_size(B64) - 2),
    <<<<(urlsafe(C))/binary>> || <<C>> <= NoPad>>.

urlsafe($+) -> <<"-">>;
urlsafe($/) -> <<"_">>;
urlsafe(C) -> <<C>>.

%% Application attribution gates access, then the credential's own attribution
%% selects its row. A null credential attribution never bypasses the App lock.
-spec rotation_application(any(), integer() | null, integer()) ->
    {ok, {integer(), integer() | null}} | {error, term()}.
rotation_application(_Conn, OrgId, _CredId) when not is_integer(OrgId), OrgId =/= null ->
    {error, {invalid_organization_id, OrgId}};
rotation_application(Conn, OrgId, CredId) ->
    case enterprise_application_credential_repo:find_by_id_tx(Conn, CredId) of
        {ok, Row} ->
            AppId = maps:get(<<"application_id">>, Row),
            CredOrg = maps:get(<<"organization_id">>, Row),
            case enterprise_application_repo:lock_tx(Conn, OrgId, AppId) of
                {ok, _} ->
                    case enterprise_application_credential_repo:lock_tx(Conn, CredOrg, CredId) of
                        {ok, #{<<"status">> := <<"active">>, <<"application_id">> := AppId}} ->
                            {ok, {AppId, CredOrg}};
                        {ok, _} ->
                            {error, not_active};
                        {error, _} = Err ->
                            Err
                    end;
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

%% @doc 池化列出 credential 元数据；不含 secret_digest 或明文。
-spec list_credentials(integer() | null, integer()) -> [map()].
list_credentials(OrgId, AppId) ->
    pool(fun(Conn) -> list_credentials(Conn, OrgId, AppId) end).

-spec list_credentials(any(), integer() | null, integer()) -> [map()].
list_credentials(Conn, OrgId, AppId) when
    (is_integer(OrgId) orelse OrgId =:= null), is_integer(AppId)
->
    Sql =
        <<"SELECT c.id, c.credential_prefix, c.status, c.created_at, c.expires_at, c.last_used_at, c.revoked_at",
            " FROM enterprise_application_credential c JOIN enterprise_application a ON a.id=c.application_id",
            " WHERE a.organization_id IS NOT DISTINCT FROM $1 AND a.id = $2 ORDER BY c.id">>,
    case elib_pg:query(Conn, Sql, [OrgId, AppId]) of
        {ok, Rows} when is_list(Rows) ->
            Rows;
        _ ->
            []
    end;
list_credentials(_Conn, _OrgId, _AppId) ->
    [].
