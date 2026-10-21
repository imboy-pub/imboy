-module(talk_record_export_download).
-moduledoc "面聊导出下载票据（T19）：capability JWT 签发/验签——同 T17 playback 票据模式。".
%%%
% 导出下载受控会话（§4 GET /:id/exports/:export_id/download + §10.3
% 「回听/导出：先记录 ACL 再附件授权；历史通用 view_url 不能成为绕过
% 记录 ACL 的旁路」）。
%
% 票据模型（capability token，与 T17 talk_record_grant_playback 同构——
% 参考 attach_gateway 的 imboy_jwt:issue_upload_capability 先例）：
%   * claims：aud/sub=<<"talk_record_export_download">>、rid=记录 ID、
%     eid=导出行 ID、uid=请求者（下载流验票时须与当前 JWT 身份一致，
%     防拿他人票据）、**acl=签发时 acl_revision**（撤销版本绑定进票据
%     claims——撤权/再授权/移交使 acl_revision 推进 → 票据立即失效，
%     §11.3 撤权阻止分段访问）、exp（短时，默认 300s，config
%     {imboy, talk_record_export_download_ttl_seconds} 覆写）、iat；
%   * URL 形状（冻结，T08/T21 exportDownloadUrl 消费 payload.url）：
%     `/api/v1/talk-records/:id/exports/:export_id/download?ticket=<jwt>`
%     ——同端点带 ticket 参数即内容流；不暴露可长期直读的存储地址；
%   * 下载端点每次开票都重新走 export_access 四要素重验（记录 ACL +
%     requester 绑定 + 未过期 + 未删除——§4"每次下载重验"）。
%
% 校验点（导出供流式/网关用）：verify_ticket/1——验签 + 时间 + audience
% → 记录 ACL 重验（visible 谓词，票据 uid 为主体）→ 撤销版本比对
% （当前 acl_revision == 票据 acl claim）→ 导出行复核（复合匹配 +
% state=ready + 未过期）。撤权/软删 → {error, not_found}；再授权/移交
% （acl_revision 推进）→ {error, acl_stale}；票据/导出过期 →
% {error, export_expired}。这是导出内容流的单一校验点。
%
% **导出副本边界（§11.3）**：导出生成的是独立副本——生成后无法通过
% 删除记录或撤销权限收回接收者已保存的字节；票据机制只保证后续下载
% 被拒，无任何回收逻辑。
%%%

-export([issue/5, verify_ticket/1, verify_ticket/4, stream_url/3, ttl_seconds/0]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

-define(DEFAULT_TTL_SECONDS, 300).
-define(DOWNLOAD_AUDIENCE, <<"talk_record_export_download">>).

%% ===================================================================
%% 开票（下载端点重验通过后调用）
%% ===================================================================

%% @doc 签发短时下载票据。AclRevision 为**签发时的记录 acl_revision**
%% （撤销版本绑定进票据 claims——撤权/再授权/移交使 acl_revision 推进 →
%% 票据立即失效，与 T17 playback 票据同构）。ExpiresAt 为导出行到期
%% （票据 exp 取 min(票据 TTL, 导出剩余寿命)——导出到期即断，不签超出
%% 导出寿命的票）。返回 {ok, #{url, ticket, expiry, expires_at,
%% export_id}} | {error, Reason}。
-spec issue(
    integer(),
    integer(),
    integer(),
    binary() | integer() | null,
    integer()
) -> {ok, map()} | {error, term()}.
issue(RecordId, ExportId, RequesterId, ExportExpiresAt, AclRevision) ->
    case download_signing_key() of
        {ok, Secret} ->
            Now = os:system_time(second),
            TicketTtl = ttl_seconds(),
            ExportTtl = export_remaining_seconds(ExportExpiresAt, Now),
            Ttl = min(TicketTtl, ExportTtl),
            case Ttl > 0 of
                true ->
                    Claims = #{
                        <<"aud">> => ?DOWNLOAD_AUDIENCE,
                        <<"sub">> => ?DOWNLOAD_AUDIENCE,
                        <<"rid">> => RecordId,
                        <<"eid">> => ExportId,
                        <<"uid">> => RequesterId,
                        <<"acl">> => AclRevision,
                        <<"iat">> => Now,
                        <<"exp">> => Now + Ttl
                    },
                    Ticket = imboy_jwt:sign(Claims, Secret),
                    {ok, #{
                        <<"export_id">> => ExportId,
                        <<"url">> => stream_url(RecordId, ExportId, Ticket),
                        <<"ticket">> => Ticket,
                        <<"expiry">> => Now + Ttl,
                        <<"expires_at">> => elib_dt:to_rfc3339(Now + Ttl)
                    }};
                false ->
                    {error, export_expired}
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc 受控内容流 URL（票据在 query；同端点 ticket 参数即内容流）。
-spec stream_url(integer(), integer(), binary()) -> binary().
stream_url(RecordId, ExportId, Ticket) ->
    <<"/api/v1/talk-records/", (integer_to_binary(RecordId))/binary, "/exports/",
        (integer_to_binary(ExportId))/binary, "/download?ticket=", Ticket/binary>>.

%% ===================================================================
%% 票据校验点（内容流/网关调用）
%% ===================================================================

%% @doc 票据全量重验：验签（HS256 strict + exp/iat）+ audience → 记录 ACL
%% 重验（visible 谓词，以票据 uid claim 为主体——撤权/软删/企业退出 →
%% {error, not_found}）→ 撤销版本比对（当前 acl_revision == 票据 acl
%% claim；撤权/再授权/移交使 acl_revision 推进 → {error, acl_stale}）
%% → 导出行复核（record_id+export_id 复合匹配、state=ready、未过期）。
%% 与 T17 talk_record_grant_playback:verify_ticket 同构（ACL+撤销版本
%% 双重验），这是导出内容流的单一校验点。
%% 成功返回 {ok, ExportRow}（含 content，供内容流输出）。
-spec verify_ticket(binary()) -> {ok, map()} | {error, atom() | term()}.
verify_ticket(Ticket) -> verify_bound_ticket(Ticket, any).
verify_ticket(Ticket, Uid, Rid, Eid) -> verify_bound_ticket(Ticket, {Uid, Rid, Eid}).

verify_bound_ticket(Ticket, Expected) when is_binary(Ticket) ->
    case download_signing_key() of
        {ok, Secret} ->
            case imboy_jwt:verify(Ticket, Secret) of
                {ok, #{<<"aud">> := ?DOWNLOAD_AUDIENCE, <<"sub">> := ?DOWNLOAD_AUDIENCE} = Claims} ->
                    recheck_expected(Claims, Expected);
                {ok, _} ->
                    {error, export_ticket_invalid};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, _} = Error ->
            Error
    end;
verify_bound_ticket(_, _) ->
    {error, export_ticket_invalid}.

%% Path and authenticated actor are checked before any artifact read.
recheck_expected(Claims, any) ->
    recheck_claims(Claims);
recheck_expected(#{<<"uid">> := U, <<"rid">> := R, <<"eid">> := E} = Claims, {U, R, E}) ->
    recheck_claims(Claims);
recheck_expected(_, _) ->
    {error, export_ticket_invalid}.

%% @private 记录 ACL 重验 + 撤销版本比对（P1 修复：对齐 T17 recheck_claims
%% 模式——票据必须携带签发时 acl_revision，验票时动态证实仍有效）。
%% 无 acl claim 的票据（旧形态/伪造）→ export_ticket_invalid。
-spec recheck_claims(map()) -> {ok, map()} | {error, atom() | term()}.
recheck_claims(#{<<"rid">> := RecordId, <<"uid">> := Uid, <<"acl">> := TicketAcl} = Claims) when
    is_integer(RecordId), is_integer(Uid), is_integer(TicketAcl)
->
    case
        elib_pg:with_tx(
            fun(C) -> talk_record_export_logic:authorize_tx(C, Uid, RecordId) end,
            [{reraise, false}]
        )
    of
        {ok, #{<<"acl_revision">> := TicketAcl}} ->
            recheck_export(Claims);
        {ok, #{<<"acl_revision">> := _Other}} ->
            {error, acl_stale};
        {error, Reason} ->
            {error, Reason}
    end;
recheck_claims(_) ->
    {error, export_ticket_invalid}.

%% @private 导出行复核（票据 rid/eid 复合匹配；state/到期裁决）。
-spec recheck_export(map()) -> {ok, map()} | {error, atom() | term()}.
recheck_export(#{<<"rid">> := RecordId, <<"eid">> := ExportId} = Claims) when
    is_integer(RecordId), is_integer(ExportId)
->
    Uid = maps:get(<<"uid">>, Claims),
    case
        elib_pg:with_conn(fun(Conn) -> talk_record_export_repo:find_tx(Conn, RecordId, ExportId) end)
    of
        {ok, #{<<"state">> := <<"ready">>, <<"requester_id">> := Uid} = Row} ->
            case export_row_expired(Row) of
                true ->
                    _ = elib_pg:with_conn(
                        fun(Conn) -> talk_record_export_repo:expire_tx(Conn, ExportId) end
                    ),
                    {error, export_expired};
                false ->
                    case talk_record_export_artifact:read(Row) of
                        {ok, Content} ->
                            {ok, Row#{<<"claims">> => Claims, <<"content">> => Content}};
                        Error ->
                            Error
                    end;
                _ ->
                    {error, export_expired}
            end;
        {ok, #{<<"state">> := <<"ready">>}} ->
            {error, export_ticket_invalid};
        {ok, #{<<"state">> := <<"queued">>} = _Row} ->
            {error, export_not_ready};
        {ok, #{<<"state">> := <<"failed">>}} ->
            {error, export_failed};
        {ok, _} ->
            {error, export_expired};
        {error, Reason} ->
            {error, Reason}
    end;
recheck_export(_) ->
    {error, export_ticket_invalid}.

%% ===================================================================
%% 配置
%% ===================================================================

%% @doc 下载票据寿命（秒；默认 300——短时受控，导出寿命内多次下载各自
%% 重新开票重验，§4"每次下载重验"）。
-spec ttl_seconds() -> pos_integer().
ttl_seconds() ->
    case config_ds:env(imboy, talk_record_export_download_ttl_seconds, ?DEFAULT_TTL_SECONDS) of
        N when is_integer(N), N >= 30, N =< 3600 -> N;
        _ -> ?DEFAULT_TTL_SECONDS
    end.

%% @private 平台 jwt_key（≥32 字节；与 T17 playback_signing_key 同策略）。
-spec download_signing_key() -> {ok, binary()} | {error, export_signing_key_unavailable}.
download_signing_key() ->
    case application:get_env(imboy, jwt_key) of
        {ok, Secret} when is_binary(Secret), byte_size(Secret) >= 32 ->
            {ok, Secret};
        _ ->
            {error, export_signing_key_unavailable}
    end.

%% @private 导出行到期判定（expires_at timestamptz：epgsql 回
%% binary/integer 双形态兼容）。
-spec export_row_expired(map()) -> boolean() | undefined.
export_row_expired(#{<<"expires_at">> := null}) ->
    undefined;
export_row_expired(#{<<"expires_at">> := undefined}) ->
    undefined;
export_row_expired(#{<<"expires_at">> := ExpiresAt}) when is_binary(ExpiresAt) ->
    case elib_dt:rfc3339_to(ExpiresAt, second) of
        Sec when is_integer(Sec) -> Sec =< os:system_time(second);
        _ -> undefined
    end;
export_row_expired(#{<<"expires_at">> := Sec}) when is_integer(Sec) ->
    Sec =< os:system_time(second);
export_row_expired(_) ->
    undefined.

%% @private 导出剩余寿命（秒；解析失败/缺失 → 票据 TTL 全额——到期
%% 裁决在 verify 的行复核兜底）。
-spec export_remaining_seconds(binary() | integer() | null, integer()) -> pos_integer().
export_remaining_seconds(null, _Now) ->
    0;
export_remaining_seconds(undefined, _Now) ->
    0;
export_remaining_seconds(ExpiresAt, Now) when is_binary(ExpiresAt) ->
    case elib_dt:rfc3339_to(ExpiresAt, second) of
        Sec when is_integer(Sec), Sec > Now -> Sec - Now;
        Sec when is_integer(Sec) -> 0;
        _ -> 0
    end;
export_remaining_seconds(Sec, Now) when is_integer(Sec) ->
    case Sec > Now of
        true -> Sec - Now;
        false -> 0
    end.
