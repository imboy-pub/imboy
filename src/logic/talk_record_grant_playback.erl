-module(talk_record_grant_playback).
-moduledoc "面聊记录受控回放会话（T17）：playback 票据签发/续签/逐段重验。".
%%%
% playback 域业务逻辑（§4 POST /:id/playback + §11.3 撤权边界 + §10.3
% 「回听：先记录 ACL 再附件授权；逐次续票/Range或分段请求重验撤销版本」）。
%
% 票据模型（capability token 模式，参考 attach_gateway 的
% imboy_jwt:issue_upload_capability 先例——专用 audience + 绝对 exp +
% 平台 jwt_key（≥32 字节，缺失拒绝签发/验签，不回退弱密钥））：
%   * claims：aud/sub=<<"talk_record_playback">>、rid=记录 ID、uid=签发
%     对象、acl=签发时 acl_revision（撤销版本绑定进票据 claims）、
%     pos=position_ms、iat/exp（短时，默认 300s，config
%     {imboy, talk_record_playback_ttl_seconds} 覆写）；
%   * URL 为受控网关路径（票据在 query，不暴露可长期直读的 S3/Garage
%     地址；talk_record_playback_handler 按 JWT 主体、路径与 ticket 对
%     每个请求及每个 64KiB 响应块重验）。
%
% 校验点（导出供网关用）：
%   * verify_ticket/1：验签 + 时间 claims + audience → 服务端重验
%     记录 ACL（visible 谓词：owner/活跃 grant + 企业归属有效 + 未删除）
%     + 当前 acl_revision == 票据 acl claim（撤销/再授权/失效使
%     acl_revision 推进 → 票据立即失效，§11.3 撤权阻止 Range/分段访问）。
%     这是逐段/Range 重验的单一校验点——网关每个分段请求都调用；
%   * open/3（POST /:id/playback）：每次开票/续签都重新走 visible 谓词
%     （撤权后开票/续签 → 404，无权限/不存在统一 404，§11.3）。
%
% 到期语义：票据过期即不可用（expired）；续签 = 同端点重开（T16
% renewTicket 复用 POST /:id/playback），续签时重验 ACL 与 acl_revision。
%%%

-export([open/3, verify_ticket/1, verify_ticket/3, playback_url/2, ttl_seconds/0]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

-define(DEFAULT_TTL_SECONDS, 300).
-define(PLAYBACK_AUDIENCE, <<"talk_record_playback">>).

%% ===================================================================
%% POST /:id/playback（开票/续签）
%% ===================================================================

%% @doc 开票（或续签：同端点重开，重验 ACL）。PositionMs 可选（0..记录
%% 时长；缺省 0）。返回 {ok, #{url, ticket, expiry, expires_at,
%% position_ms, record_id, acl_revision}} | {error, Reason}。
%% T16/T08 消费字段：url（T16 sessionId 同源）+ expiry（unix 秒）/
%% expires_at（RFC3339）。
-spec open(integer(), integer(), integer() | undefined) -> {ok, map()} | {error, term()}.
open(Uid, RecordId, PositionMs) ->
    case talk_record_content_common:visible_or_404(Uid, RecordId) of
        {ok, Row} ->
            open_visible(Uid, RecordId, Row, PositionMs);
        {error, Reason} ->
            {error, Reason}
    end.

-spec open_visible(integer(), integer(), map(), integer() | undefined) ->
    {ok, map()} | {error, term()}.
open_visible(Uid, RecordId, #{<<"media_status">> := <<"available">>} = Row, PositionMs) ->
    case normalize_position(PositionMs, Row) of
        {ok, Pos} ->
            issue_ticket(Uid, RecordId, Row, Pos);
        {error, Reason} ->
            {error, Reason}
    end;
open_visible(_Uid, _RecordId, #{<<"media_status">> := _}, _PositionMs) ->
    %% 服务端无可回放音频（local_only/uploading/verifying/assembling/
    %% retryable_failed）——如实拒绝，不签发不可用票据
    {error, playback_media_unavailable}.

-spec normalize_position(integer() | undefined, map()) ->
    {ok, integer()} | {error, invalid_position_ms}.
normalize_position(undefined, _Row) ->
    {ok, 0};
normalize_position(Pos, Row) when is_integer(Pos), Pos >= 0 ->
    Duration = maps:get(<<"duration_ms">>, Row, 0),
    case Duration of
        D when D >= Pos -> {ok, Pos};
        _ -> {error, invalid_position_ms}
    end;
normalize_position(_, _) ->
    {error, invalid_position_ms}.

-spec issue_ticket(integer(), integer(), map(), integer()) -> {ok, map()} | {error, term()}.
issue_ticket(Uid, RecordId, Row, Pos) ->
    case playback_signing_key() of
        {ok, Secret} ->
            AclRevision = maps:get(<<"acl_revision">>, Row, 1),
            Now = os:system_time(second),
            Claims = #{
                <<"aud">> => ?PLAYBACK_AUDIENCE,
                <<"sub">> => ?PLAYBACK_AUDIENCE,
                <<"rid">> => RecordId,
                <<"uid">> => Uid,
                <<"acl">> => AclRevision,
                <<"pos">> => Pos,
                <<"iat">> => Now,
                <<"exp">> => Now + ttl_seconds()
            },
            Ticket = imboy_jwt:sign(Claims, Secret),
            ExpiresAt = elib_dt:to_rfc3339(Now + ttl_seconds()),
            {ok, #{
                <<"record_id">> => RecordId,
                <<"url">> => playback_url(RecordId, Ticket),
                <<"ticket">> => Ticket,
                <<"expiry">> => Now + ttl_seconds(),
                <<"expires_at">> => ExpiresAt,
                <<"position_ms">> => Pos,
                <<"acl_revision">> => AclRevision
            }};
        {error, _} = Error ->
            Error
    end.

%% @doc 受控回放 URL（票据在 query；流式网关逐段/Range 按 ticket 重验，
%% 不暴露长期直读地址）。
-spec playback_url(integer(), binary()) -> binary().
playback_url(RecordId, Ticket) ->
    <<"/api/v1/talk-records/", (integer_to_binary(RecordId))/binary, "/playback/stream?ticket=",
        Ticket/binary>>.

%% ===================================================================
%% 票据校验点（导出供网关用：逐段/Range/续签前重验）
%% ===================================================================

%% @doc 票据全量重验（网关逐段/Range 请求调用）：
%% 验签（HS256 strict + exp/iat）+ audience → 记录 ACL 重验（visible）→
%% 撤销版本比对（当前 acl_revision == 票据 acl claim）。
%% 撤权/再授权使 acl_revision 推进 → {error, acl_stale}；撤权使 visible
%% 失败 → {error, not_found}；过期 → {error, expired}。
-spec verify_ticket(binary()) -> {ok, map()} | {error, atom() | term()}.
verify_ticket(Ticket) -> verify_bound_ticket(Ticket, any).

%% The authenticated actor and route identity must match before any artifact IO.
verify_ticket(Ticket, Uid, Rid) -> verify_bound_ticket(Ticket, {Uid, Rid}).

verify_bound_ticket(Ticket, Expected) when is_binary(Ticket) ->
    case playback_signing_key() of
        {ok, Secret} ->
            case imboy_jwt:verify(Ticket, Secret) of
                {ok, #{<<"aud">> := ?PLAYBACK_AUDIENCE, <<"sub">> := ?PLAYBACK_AUDIENCE} = Claims} ->
                    recheck_expected(Claims, Expected);
                {ok, _} ->
                    {error, playback_ticket_invalid};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, _} = Error ->
            Error
    end;
verify_bound_ticket(_, _) ->
    {error, playback_ticket_invalid}.

recheck_expected(Claims, any) -> recheck_claims(Claims);
recheck_expected(#{<<"uid">> := U, <<"rid">> := R} = Claims, {U, R}) -> recheck_claims(Claims);
recheck_expected(_, _) -> {error, playback_ticket_invalid}.

%% @private 服务端重验：ACL（visible 谓词）+ 撤销版本（acl_revision）。
-spec recheck_claims(map()) -> {ok, map()} | {error, atom() | term()}.
recheck_claims(#{<<"rid">> := RecordId, <<"uid">> := Uid, <<"acl">> := TicketAcl} = Claims) when
    is_integer(RecordId), is_integer(Uid), is_integer(TicketAcl)
->
    case talk_record_repo:find_visible(Uid, RecordId) of
        {ok, #{<<"acl_revision">> := TicketAcl, <<"media_status">> := <<"available">>}} ->
            {ok, Claims};
        {ok, #{<<"acl_revision">> := _Other}} ->
            {error, acl_stale};
        {error, Reason} ->
            {error, Reason}
    end;
recheck_claims(_) ->
    {error, playback_ticket_invalid}.

%% ===================================================================
%% 配置
%% ===================================================================

%% @doc 票据寿命（秒；默认 300）。撤权由服务端逐块重验立即阻止后续读取；
%% 已送达播放器的缓冲无法收回，客户端应在账号变化时立即停止。
-spec ttl_seconds() -> pos_integer().
ttl_seconds() ->
    case config_ds:env(imboy, talk_record_playback_ttl_seconds, ?DEFAULT_TTL_SECONDS) of
        N when is_integer(N), N >= 30, N =< 3600 -> N;
        _ -> ?DEFAULT_TTL_SECONDS
    end.

%% @private 平台 jwt_key（≥32 字节；缺失/无效拒绝签发与验签——与
%% imboy_jwt upload_signing_key 同策略，不回退 jwt_secret 或空密钥）。
-spec playback_signing_key() -> {ok, binary()} | {error, playback_signing_key_unavailable}.
playback_signing_key() ->
    case application:get_env(imboy, jwt_key) of
        {ok, Secret} when is_binary(Secret), byte_size(Secret) >= 32 ->
            {ok, Secret};
        _ ->
            {error, playback_signing_key_unavailable}
    end.
