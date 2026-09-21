-module(enterprise_internal_ops).

%%%
% enterprise_internal_ops 是企业集成 Application/Credential 的最小运维面
% （EPGZ-02，plan-gz §1.11：不做 Admin 治理 UI，只提供受权限保护的
% module API / shell 命令形态）。
%
% 能力：创建 application、下发/轮换/撤销 credential、查状态、scopes 与
% 启停治理。调用方是运维（imboy_ctl/erl shell，W4 接线后可挂 /api/adm/*），
% **不是** Application Credential 面。
%
% credential 明文纪律：secret 由本模块生成（32 字节 CSPRNG base64url）
% 或由运维显式提供，只在 issue/rotate 的**返回值出现一次**；库内只存
% SHA-256 digest（enterprise_application_credential_repo）。
%
% _tx 变体供测试（直连连接）与运行时复用；无后缀变体经 elib_pg:with_tx
% 池化执行。scope 只接受 enterprise_internal_scope:all() 固定枚举成员。
%%%

-export([
    create_application_tx/5,
    create_application/4,
    update_scopes_tx/4,
    update_scopes/3,
    set_application_status_tx/4,
    set_application_status/3,
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

%% @doc 事务内创建 application（scopes 必须全为固定枚举成员）。
-spec create_application_tx(any(), integer(), binary(), binary(), [binary()]) ->
    {ok, map()} | {error, invalid_scope | key_conflict | term()}.
create_application_tx(Conn, OrgId, ApplicationKey, Name, Scopes) ->
    case validate_scopes(Scopes) of
        ok ->
            enterprise_application_repo:create_tx(
                Conn, OrgId, ApplicationKey, Name, {null, Scopes}
            );
        {error, _} = Err ->
            Err
    end.

%% @doc 池化创建 application。
-spec create_application(integer(), binary(), binary(), [binary()]) ->
    {ok, map()} | {error, term()}.
create_application(OrgId, ApplicationKey, Name, Scopes) ->
    pool(fun(Conn) -> create_application_tx(Conn, OrgId, ApplicationKey, Name, Scopes) end).

%% @doc 事务内整体替换 allowed_scopes（固定枚举成员校验）。
-spec update_scopes_tx(any(), integer(), integer(), [binary()]) ->
    ok | {error, invalid_scope | not_found | term()}.
update_scopes_tx(Conn, OrgId, AppId, Scopes) ->
    case validate_scopes(Scopes) of
        ok -> enterprise_application_repo:update_scopes_tx(Conn, OrgId, AppId, Scopes);
        {error, _} = Err -> Err
    end.

%% @doc 池化替换 scopes。
-spec update_scopes(integer(), integer(), [binary()]) -> ok | {error, term()}.
update_scopes(OrgId, AppId, Scopes) ->
    pool(fun(Conn) -> update_scopes_tx(Conn, OrgId, AppId, Scopes) end).

%% @doc 事务内启停 application（active|disabled）。
-spec set_application_status_tx(any(), integer(), integer(), binary()) ->
    ok | {error, not_found | term()}.
set_application_status_tx(Conn, OrgId, AppId, Status) ->
    enterprise_application_repo:update_status_tx(Conn, OrgId, AppId, Status).

%% @doc 池化启停 application。
-spec set_application_status(integer(), integer(), binary()) -> ok | {error, term()}.
set_application_status(OrgId, AppId, Status) ->
    pool(fun(Conn) -> set_application_status_tx(Conn, OrgId, AppId, Status) end).

%%%===================================================================
%%% Credential 生命周期
%%%===================================================================

%% @doc 事务内签发凭证（secret 由本模块生成，明文只在返回值出现一次）。
-spec issue_credential_tx(any(), integer(), integer(), undefined | binary()) ->
    {ok, #{credential_id := integer(), credential := binary(), expires_at := undefined | binary()}}
    | {error, term()}.
issue_credential_tx(Conn, OrgId, AppId, ExpiresAt) ->
    issue_credential_tx(Conn, OrgId, AppId, generate_secret(), ExpiresAt).

%% @doc 事务内签发凭证（显式 secret——备份恢复等运维场景）。
%% 返回 #{credential_id, credential(明文 ib_int_<id>.<secret>), expires_at}。
-spec issue_credential_tx(any(), integer(), integer(), binary(), undefined | binary()) ->
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
-spec issue_credential(integer(), integer(), undefined | binary()) ->
    {ok, map()} | {error, term()}.
issue_credential(OrgId, AppId, ExpiresAt) ->
    pool(fun(Conn) -> issue_credential_tx(Conn, OrgId, AppId, ExpiresAt) end).

%% @doc 事务内轮换凭证（同事务：签发新凭证 + 撤销旧凭证；
%% 新凭证创建失败时旧凭证保持有效——轮换原子向安全侧倾斜）。
-spec rotate_credential_tx(any(), integer(), integer()) ->
    {ok, map()} | {error, term()}.
rotate_credential_tx(Conn, OrgId, OldCredId) ->
    rotate_credential_tx(Conn, OrgId, OldCredId, undefined).

%% @doc 事务内轮换凭证（可指定新凭证过期时间）。
-spec rotate_credential_tx(any(), integer(), integer(), undefined | binary()) ->
    {ok, map()} | {error, term()}.
rotate_credential_tx(Conn, OrgId, OldCredId, ExpiresAt) ->
    case credential_application_id(Conn, OrgId, OldCredId) of
        {ok, AppId} ->
            case issue_credential_tx(Conn, OrgId, AppId, ExpiresAt) of
                {ok, _} = Ok ->
                    case enterprise_application_credential_repo:revoke_tx(Conn, OrgId, OldCredId) of
                        ok -> Ok;
                        {error, not_active} -> Ok;
                        {error, Reason} -> {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 池化轮换凭证。
-spec rotate_credential(integer(), integer()) -> {ok, map()} | {error, term()}.
rotate_credential(OrgId, OldCredId) ->
    pool(fun(Conn) -> rotate_credential_tx(Conn, OrgId, OldCredId) end).

%% @doc 事务内撤销凭证（幂等语义见 repo：非 active → {error, not_active}）。
-spec revoke_credential_tx(any(), integer(), integer()) ->
    ok | {error, not_found | not_active | term()}.
revoke_credential_tx(Conn, OrgId, CredId) ->
    enterprise_application_credential_repo:revoke_tx(Conn, OrgId, CredId).

%% @doc 池化撤销凭证。
-spec revoke_credential(integer(), integer()) -> ok | {error, term()}.
revoke_credential(OrgId, CredId) ->
    pool(fun(Conn) -> revoke_credential_tx(Conn, OrgId, CredId) end).

%%%===================================================================
%%% 状态查询（redacted：不含 digest / 明文）
%%%===================================================================

%% @doc 事务内查询 application 状态与凭证列表（仅元数据；
%% 不含 secret_digest / 明文 credential —— redaction 红线）。
-spec application_status_tx(any(), integer(), integer()) -> {ok, map()} | {error, term()}.
application_status_tx(Conn, OrgId, AppId) ->
    case enterprise_application_repo:find_tx(Conn, OrgId, AppId) of
        {ok, App} ->
            Scopes =
                case jsone:decode(maps:get(<<"allowed_scopes">>, App, <<"[]">>)) of
                    L when is_list(L) -> [S || S <- L, is_binary(S)];
                    _ -> []
                end,
            {ok, #{
                application => #{
                    id => maps:get(<<"id">>, App),
                    application_key => maps:get(<<"application_key">>, App),
                    name => maps:get(<<"name">>, App),
                    status => maps:get(<<"status">>, App)
                },
                granted_scopes => Scopes,
                credentials => list_credentials(Conn, OrgId, AppId)
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 池化查询 application 状态。
-spec application_status(integer(), integer()) -> {ok, map()} | {error, term()}.
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

-spec validate_scopes([binary()]) -> ok | {error, invalid_scope}.
validate_scopes(Scopes) when is_list(Scopes) ->
    Fixed = enterprise_internal_scope:all(),
    case [S || S <- Scopes, not lists:member(S, Fixed)] of
        [] -> ok;
        _Bad -> {error, invalid_scope}
    end;
validate_scopes(_) ->
    {error, invalid_scope}.

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

-spec credential_application_id(any(), integer(), integer()) ->
    {ok, integer()} | {error, not_found | term()}.
credential_application_id(Conn, OrgId, CredId) ->
    Sql =
        <<"SELECT application_id FROM enterprise_application_credential",
            " WHERE organization_id = $1 AND id = $2 LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [OrgId, CredId]) of
        {ok, [#{<<"application_id">> := AppId} | _]} ->
            {ok, AppId};
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

-spec list_credentials(any(), integer(), integer()) -> [map()].
list_credentials(Conn, OrgId, AppId) ->
    Sql =
        <<"SELECT id, credential_prefix, status, expires_at, last_used_at, revoked_at",
            " FROM enterprise_application_credential",
            " WHERE organization_id = $1 AND application_id = $2 ORDER BY id">>,
    case elib_pg:query(Conn, Sql, [OrgId, AppId]) of
        {ok, Rows} when is_list(Rows) ->
            Rows;
        _ ->
            []
    end.
