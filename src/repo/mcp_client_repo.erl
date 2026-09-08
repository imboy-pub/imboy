-module(mcp_client_repo).

%%%
% MCP 客户端登记/审批仓库 / MCP client registry & approval repository
% 表 mcp_client（见 priv/migrations/00000028_mcp_governance）：
%   client_id TSID 主键，owner_uid 唯一（一个用户一条 client 记录）。
% 所有 SQL 经 elib_pg 参数化。
%%%

-export([tablename/0]).
-export([find_by_owner/1]).
-export([find/1]).
-export([ensure/2]).
-export([set_status/4]).
-export([page/4]).
%% MCP-01：独立身份与凭证
-export([create_client/2, create_client/3]).
-export([find_by_client_key/1, find_by_digest/1]).
-export([touch_last_used/1]).
-export([set_disabled/2]).
-export([list_by_owner/1]).
%% digest 唯一真源：mcp_governance_logic:authenticate_secret/1 的 HTTP 认证路径
%% 依赖本函数（EXT-01 实测曾因未导出在运行时 undef），导出以免逻辑层另行实现摘要。
-export([digest_hex/1]).

-include("log.hrl").

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"mcp_client">>).

%% @doc 按 owner_uid 查 client（唯一）
-spec find_by_owner(integer()) -> {ok, map()} | {error, notfound | term()}.
find_by_owner(OwnerUid) ->
    Tb = tablename(),
    Sql =
        <<
            "SELECT client_id, owner_uid, name, description, status, reason,"
            " created_at, approved_at FROM ",
            Tb/binary,
            " WHERE owner_uid = $1"
        >>,
    case elib_pg:query(Sql, [OwnerUid]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, notfound};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 按 client_id 查 client
-spec find(integer()) -> {ok, map()} | {error, notfound | term()}.
find(ClientId) ->
    Tb = tablename(),
    Sql =
        <<
            "SELECT client_id, owner_uid, name, description, status, reason,"
            " created_at, approved_at FROM ",
            Tb/binary,
            " WHERE client_id = $1"
        >>,
    case elib_pg:query(Sql, [ClientId]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, notfound};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 惰性登记：owner_uid 无记录则插入 pending，返回 client_id。
%% 并发下靠 uniq_mcp_client_owner + ON CONFLICT DO NOTHING 幂等。
-spec ensure(integer(), binary()) -> {ok, integer()} | {error, term()}.
ensure(OwnerUid, Name) ->
    case find_by_owner(OwnerUid) of
        {ok, #{<<"client_id">> := ClientId}} ->
            {ok, ClientId};
        {error, notfound} ->
            ClientId = elib_tsid:generate(mcp_client),
            Tb = tablename(),
            Sql =
                <<"INSERT INTO ", Tb/binary,
                    " (client_id, owner_uid, name, status, created_at, updated_at)"
                    " VALUES ($1,$2,$3,'pending',NOW(),NOW())"
                    " ON CONFLICT (owner_uid) DO NOTHING">>,
            case elib_pg:execute(Sql, [ClientId, OwnerUid, Name]) of
                {ok, _Count} ->
                    %% 若刚好被并发抢先插入，冲突跳过→重查取权威 client_id
                    case find_by_owner(OwnerUid) of
                        {ok, #{<<"client_id">> := Cid}} -> {ok, Cid};
                        Other -> Other
                    end;
                {error, Reason} ->
                    ?ERROR_LOG("mcp_client_repo:ensure owner=~p error ~p~n", [OwnerUid, Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 改状态（approved 记 approved_at；reject/revoke 记 reason）
-spec set_status(integer(), binary(), binary(), integer()) ->
    {ok, non_neg_integer()} | {error, term()}.
set_status(ClientId, <<"approved">>, _Reason, _ActorUid) ->
    Tb = tablename(),
    elib_pg:update(
        Tb,
        #{status => <<"approved">>, approved_at => elib_dt:now(), updated_at => elib_dt:now()},
        <<"client_id = $1">>,
        [ClientId]
    );
set_status(ClientId, Status, Reason, _ActorUid) ->
    Tb = tablename(),
    elib_pg:update(
        Tb,
        #{status => Status, reason => Reason, updated_at => elib_dt:now()},
        <<"client_id = $1">>,
        [ClientId]
    ).

%% @doc 分页列出 client（可按 status 过滤；keyword 模糊 MVP 从简，仅 status 等值过滤）
-spec page(pos_integer(), pos_integer(), binary(), binary()) ->
    {ok, map()} | {error, term()}.
page(Page, Size, Status, _Keyword) ->
    Tb = tablename(),
    Column =
        <<
            "client_id, owner_uid, name, description, status, reason,"
            " created_at, approved_at"
        >>,
    Where = build_where(Status),
    elib_pg:page_with_total(Tb, Column, Where, <<"created_at DESC">>, Page, Size).

%% ponytail: keyword 模糊匹配后续真需要再下推 SQL，当前仅 status 等值过滤。
build_where(<<>>) -> #{};
build_where(Status) -> #{status => Status}.

%% ===================================================================
%% MCP-01：独立身份与凭证（PDT-01 mcp_client 契约 §2）
%% ===================================================================

%% @doc 创建新 client（同 owner 可多 client，无唯一约束）。
%% 生成 client_key（稳定标识）与随机 secret（≥32 字节）；
%% 库中只存 SHA-256 摘要与明文前缀，明文 secret 仅经本函数返回一次。
%% Opts：name/description/expires_at（timestamptz 可 terms elib_dt 形态）。
-spec create_client(integer(), binary(), binary() | map()) ->
    {ok, map()} | {error, term()}.
create_client(OwnerUid, Name, Description) when is_integer(OwnerUid), OwnerUid > 0 ->
    create_client(OwnerUid, #{name => Name, description => Description}).
create_client(OwnerUid, Opts) when is_integer(OwnerUid), is_map(Opts) ->
    %% client_id 列为 bigint：保持整数（epgsql int8 编码不接受二进制）
    ClientId = elib_tsid:generate(),
    ClientKey = iolist_to_binary(
        [<<"mck-">>, binary:encode_hex(crypto:strong_rand_bytes(8), lowercase)]
    ),
    Secret = binary:encode_hex(crypto:strong_rand_bytes(32), lowercase),
    Digest = digest_hex(Secret),
    Prefix = binary:part(Secret, 0, 8),
    Tb = tablename(),
    ExpiresAt = maps:get(expires_at, Opts, undefined),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (client_id, owner_uid, name, description, status, client_key,"
            " credential_digest, credential_prefix, expires_at, created_at, updated_at)"
            " VALUES ($1,$2,$3,$4,'pending',$5,$6,$7,$8,NOW(),NOW())">>,
    Params = [
        ClientId,
        OwnerUid,
        maps:get(name, Opts, <<>>),
        maps:get(description, Opts, <<>>),
        ClientKey,
        Digest,
        Prefix,
        ExpiresAt
    ],
    case elib_pg:execute(Sql, Params) of
        {ok, _} ->
            {ok, #{
                <<"client_id">> => ClientId,
                <<"client_key">> => ClientKey,
                <<"secret">> => Secret,
                <<"credential_prefix">> => Prefix,
                <<"status">> => <<"pending">>
            }};
        {error, Reason} ->
            ?ERROR_LOG(
                "mcp_client_repo:create_client owner=~p error ~p~n",
                [OwnerUid, Reason]
            ),
            {error, Reason}
    end;
create_client(_OwnerUid, _Other) ->
    {error, invalid_owner}.

%% @doc 按 client_key 精确查 client。
-spec find_by_client_key(binary()) -> {ok, map()} | {error, notfound | term()}.
find_by_client_key(ClientKey) ->
    find_by_col(<<"client_key">>, ClientKey).

%% @doc 按凭证摘要精确查 client（高熵 token：SHA-256 索引查找，防时序侧信道）。
-spec find_by_digest(binary()) -> {ok, map()} | {error, notfound | term()}.
find_by_digest(DigestHex) ->
    find_by_col(<<"credential_digest">>, DigestHex).

find_by_col(Col, Val) ->
    Tb = tablename(),
    Sql = iolist_to_binary(
        [
            <<"SELECT client_id, owner_uid, name, description, status, reason, ">>,
            <<"client_key, credential_digest, credential_prefix, expires_at, ">>,
            <<"last_used_at, disabled, created_at, approved_at FROM ">>,
            Tb,
            <<" WHERE ">>,
            Col,
            <<" = $1 LIMIT 1">>
        ]
    ),
    case elib_pg:query(Sql, [Val]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, notfound};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 更新最近使用时间（best-effort）。
-spec touch_last_used(integer()) -> ok.
touch_last_used(ClientId) ->
    Tb = tablename(),
    _ = elib_pg:execute(
        <<"UPDATE ", Tb/binary, " SET last_used_at = NOW() WHERE client_id = $1">>,
        [ClientId]
    ),
    ok.

%% @doc 禁用/启用（与 revoke 不同：disabled 是管理面瞬时开关，同样 fail-closed）。
-spec set_disabled(integer(), boolean()) -> {ok, non_neg_integer()} | {error, term()}.
set_disabled(ClientId, Disabled) ->
    Tb = tablename(),
    elib_pg:update(
        Tb,
        #{disabled => Disabled, updated_at => elib_dt:now()},
        <<"client_id = $1">>,
        [ClientId]
    ).

%% @doc 某 owner 的全部 client（同 owner 可多 client）。
-spec list_by_owner(integer()) -> {ok, [map()]} | {error, term()}.
list_by_owner(OwnerUid) ->
    Tb = tablename(),
    Sql =
        <<
            "SELECT client_id, owner_uid, name, description, status, reason,"
            " client_key, credential_prefix, expires_at, last_used_at, disabled,"
            " created_at, approved_at FROM ",
            Tb/binary,
            " WHERE owner_uid = $1 ORDER BY created_at DESC"
        >>,
    elib_pg:query(Sql, [OwnerUid]).

%% @doc SHA-256 摘要（小写 hex）。摘要不含任何盐：token 本身高熵，防彩虹表无意义，
%% 固定摘要才能用索引精确查找（认证路径 O(1)）。
digest_hex(Token) when is_binary(Token) ->
    binary:encode_hex(crypto:hash(sha256, Token), lowercase);
digest_hex(Token) ->
    digest_hex(ec_cnv:to_binary(Token)).
